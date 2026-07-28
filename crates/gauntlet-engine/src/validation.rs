//! Response validation: status code plus dot-path field assertions against the
//! JSON body. See the crate docs for the compile-once rule on regex patterns.

use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

use regex::Regex;
use serde_json::Value;

use gauntlet_core::{BenchmarkConfig, FieldAssertion, ValidationError, ValidationSpec};

/// Compiled `matches` patterns, keyed by pattern source. Warmed by
/// [`check_patterns`]; the measurement loop only ever takes a read lock.
fn regex_cache() -> &'static RwLock<HashMap<String, Regex>> {
    static CACHE: OnceLock<RwLock<HashMap<String, Regex>>> = OnceLock::new();
    CACHE.get_or_init(|| RwLock::new(HashMap::new()))
}

/// Fetch a compiled regex for `pattern`, compiling and caching it on first use.
fn compiled(pattern: &str) -> Result<Regex, String> {
    if let Ok(cache) = regex_cache().read() {
        if let Some(re) = cache.get(pattern) {
            return Ok(re.clone());
        }
    }

    let re = Regex::new(pattern).map_err(|e| format!("invalid regex: {e}"))?;
    if let Ok(mut cache) = regex_cache().write() {
        cache.insert(pattern.to_owned(), re.clone());
    }
    Ok(re)
}

/// Compile every `matches` pattern in the config, reporting a bad one as a
/// config error. See the crate docs.
pub fn check_patterns(config: &BenchmarkConfig) -> Result<(), Vec<String>> {
    let mut errors = Vec::new();
    for payload in &config.payloads {
        let Some(fields) = payload.validate.as_ref().and_then(|v| v.fields.as_ref()) else {
            continue;
        };
        for (path, assertion) in fields {
            if let FieldAssertion::Matches(pattern) = assertion {
                if let Err(e) = compiled(pattern) {
                    errors.push(format!(
                        "payloads.{}.validate.fields.{path}: {e}",
                        payload.name
                    ));
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Check one response against its spec, returning every failed assertion.
pub fn validate_response(spec: &ValidationSpec, status: u16, body: &[u8]) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    if let Some(expected) = spec.status {
        if status != expected {
            errors.push(ValidationError {
                field: "status".to_owned(),
                message: format!("expected status {expected}, got {status}"),
            });
        }
    }

    if let Some(fields) = &spec.fields {
        match serde_json::from_slice::<Value>(body) {
            Ok(json) => {
                for (path, assertion) in fields {
                    if let Err(message) = check_field(&json, path, assertion) {
                        errors.push(ValidationError {
                            field: path.clone(),
                            message,
                        });
                    }
                }
            }
            Err(e) => errors.push(ValidationError {
                field: "$".to_owned(),
                message: format!("response body is not valid JSON: {e}"),
            }),
        }
    }

    errors
}

/// Resolve a dot-path against a JSON root. Leading `$` / `$.` is stripped; numeric
/// segments index arrays, others index objects.
fn lookup<'a>(root: &'a Value, path: &str) -> Option<&'a Value> {
    let trimmed = path
        .strip_prefix("$.")
        .or_else(|| path.strip_prefix('$'))
        .unwrap_or(path);
    let mut cur = root;
    for seg in trimmed.split('.') {
        if seg.is_empty() {
            continue;
        }
        cur = match cur {
            Value::Object(map) => map.get(seg)?,
            Value::Array(arr) => arr.get(seg.parse::<usize>().ok()?)?,
            _ => return None,
        };
    }
    Some(cur)
}

fn type_name(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

/// Apply one assertion at `path`. `Present`/`Null`/`NotNull` define their own
/// missing-field semantics; every other assertion fails on a missing path.
fn check_field(root: &Value, path: &str, assertion: &FieldAssertion) -> Result<(), String> {
    let found = lookup(root, path);

    match assertion {
        FieldAssertion::Present => match found {
            Some(_) => Ok(()),
            None => Err("field not present".to_owned()),
        },
        FieldAssertion::Null => match found {
            Some(Value::Null) => Ok(()),
            Some(v) => Err(format!("expected null, got {}", type_name(v))),
            None => Err("field not present".to_owned()),
        },
        FieldAssertion::NotNull => match found {
            Some(Value::Null) => Err("expected non-null, got null".to_owned()),
            Some(_) => Ok(()),
            None => Err("field not present".to_owned()),
        },
        _ => {
            let value = found.ok_or_else(|| "field not present".to_owned())?;
            check_present(value, assertion)
        }
    }
}

/// Assertions that require the field to exist (its presence is handled above).
fn check_present(value: &Value, assertion: &FieldAssertion) -> Result<(), String> {
    match assertion {
        FieldAssertion::Eq(expected) => {
            if value == expected {
                Ok(())
            } else {
                Err(format!("expected {expected}, got {value}"))
            }
        }
        FieldAssertion::Type(expected) => {
            let actual = type_name(value);
            if actual == expected {
                Ok(())
            } else {
                Err(format!("expected type {expected}, got {actual}"))
            }
        }
        FieldAssertion::Matches(pattern) => {
            let re = compiled(pattern)?;
            match value.as_str() {
                Some(s) if re.is_match(s) => Ok(()),
                Some(s) => Err(format!("{s:?} does not match /{pattern}/")),
                None => Err(format!(
                    "expected string to match, got {}",
                    type_name(value)
                )),
            }
        }
        FieldAssertion::Range { min, max } => {
            let n = value
                .as_f64()
                .ok_or_else(|| format!("expected number, got {}", type_name(value)))?;
            if min.is_some_and(|m| n < m) || max.is_some_and(|m| n > m) {
                Err(format!("{n} out of range [{min:?}, {max:?}]"))
            } else {
                Ok(())
            }
        }
        FieldAssertion::ArrayLength(expected) => match value.as_array() {
            Some(arr) if arr.len() == *expected => Ok(()),
            Some(arr) => Err(format!(
                "expected array length {expected}, got {}",
                arr.len()
            )),
            None => Err(format!("expected array, got {}", type_name(value))),
        },
        FieldAssertion::ArrayContains(needle) => match value.as_array() {
            Some(arr) if arr.contains(needle) => Ok(()),
            Some(_) => Err(format!("array does not contain {needle}")),
            None => Err(format!("expected array, got {}", type_name(value))),
        },
        // Handled in `check_field`.
        FieldAssertion::Present | FieldAssertion::Null | FieldAssertion::NotNull => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn config_with_pattern(pattern: &str) -> BenchmarkConfig {
        serde_json::from_str(&format!(
            r#"{{"targets":[{{"name":"t","url":"http://host"}}],
                "settings":{{"iterations":1,"concurrency":1}},
                "payloads":[{{"name":"p","method":"GET","path":"/",
                    "validate":{{"fields":{{"$.id":{{"matches":"{pattern}"}}}}}}}}]}}"#
        ))
        .expect("config parses")
    }

    #[test]
    fn a_malformed_pattern_is_a_config_error_naming_the_payload_and_field() {
        // Before this check a typo'd pattern was only found per response, where
        // it read as the *service* failing every assertion in the run.
        let errors = check_patterns(&config_with_pattern("[unclosed"))
            .expect_err("an unclosed class is not a regex");

        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].contains("payloads.p.validate.fields.$.id"),
            "the error must locate the pattern: {}",
            errors[0]
        );
        assert!(errors[0].contains("invalid regex"), "{}", errors[0]);
    }

    #[test]
    fn a_valid_pattern_passes_and_is_cached_for_the_run() {
        let config = config_with_pattern("^[a-f0-9]+$");
        assert!(check_patterns(&config).is_ok());
        assert!(
            regex_cache()
                .read()
                .expect("cache lock")
                .contains_key("^[a-f0-9]+$"),
            "check_patterns warms the cache so the measurement loop never compiles"
        );
    }

    #[test]
    fn a_config_with_no_patterns_has_nothing_to_check() {
        let config: BenchmarkConfig = serde_json::from_str(
            r#"{"targets":[{"name":"t","url":"http://host"}],
               "settings":{"iterations":1,"concurrency":1},
               "payloads":[{"name":"p","method":"GET","path":"/"}]}"#,
        )
        .expect("config parses");
        assert!(check_patterns(&config).is_ok());
    }

    fn spec(fields: &[(&str, FieldAssertion)]) -> ValidationSpec {
        ValidationSpec {
            status: None,
            fields: Some(
                fields
                    .iter()
                    .map(|(k, v)| (k.to_string(), v.clone()))
                    .collect::<BTreeMap<_, _>>(),
            ),
        }
    }

    fn errs(spec: &ValidationSpec, body: &str) -> Vec<ValidationError> {
        validate_response(spec, 200, body.as_bytes())
    }

    #[test]
    fn status_mismatch_is_reported() {
        let s = ValidationSpec {
            status: Some(201),
            fields: None,
        };
        let e = validate_response(&s, 200, b"");
        assert_eq!(e.len(), 1);
        assert_eq!(e[0].field, "status");
    }

    #[test]
    fn present_and_paths_resolve() {
        let s = spec(&[
            ("$.user.id", FieldAssertion::Present),
            ("$.user.tags.0", FieldAssertion::Eq(serde_json::json!("a"))),
        ]);
        assert!(errs(&s, r#"{"user":{"id":1,"tags":["a","b"]}}"#).is_empty());
    }

    #[test]
    fn each_assertion_detects_its_failure() {
        let cases: &[(&str, FieldAssertion, &str)] = &[
            ("$.x", FieldAssertion::Present, r#"{"y":1}"#),
            ("$.x", FieldAssertion::NotNull, r#"{"x":null}"#),
            (
                "$.x",
                FieldAssertion::Eq(serde_json::json!(2)),
                r#"{"x":1}"#,
            ),
            ("$.x", FieldAssertion::Type("string".into()), r#"{"x":1}"#),
            (
                "$.x",
                FieldAssertion::Matches("^a".into()),
                r#"{"x":"zzz"}"#,
            ),
            (
                "$.x",
                FieldAssertion::Range {
                    min: Some(0.0),
                    max: Some(10.0),
                },
                r#"{"x":99}"#,
            ),
            ("$.x", FieldAssertion::ArrayLength(2), r#"{"x":[1]}"#),
            (
                "$.x",
                FieldAssertion::ArrayContains(serde_json::json!(9)),
                r#"{"x":[1,2]}"#,
            ),
        ];
        for (path, assertion, body) in cases {
            let s = spec(&[(path, assertion.clone())]);
            assert_eq!(errs(&s, body).len(), 1, "{assertion:?} on {body}");
        }
    }

    #[test]
    fn invalid_json_body_fails_field_checks() {
        let s = spec(&[("$.x", FieldAssertion::Present)]);
        let e = errs(&s, "not json");
        assert_eq!(e.len(), 1);
        assert_eq!(e[0].field, "$");
    }
}
