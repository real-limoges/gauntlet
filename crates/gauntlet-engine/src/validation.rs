//! Response validation: status code + dot-path field assertions against the JSON
//! body. Produces `gauntlet_core::ValidationError`s; the endpoint loop aggregates
//! them into a `ValidationSummary` (capped at `MAX_VALIDATION_ERRORS`).

use regex::Regex;
use serde_json::Value;

use gauntlet_core::{FieldAssertion, ValidationError, ValidationSpec};

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

/// Apply one assertion at `path`. `Ok(())` passes; `Err(msg)` is the failure
/// reason. `Present`/`Null`/`NotNull` define their own missing-field semantics;
/// every other assertion treats a missing path as a failure.
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
            let re = Regex::new(pattern).map_err(|e| format!("invalid regex: {e}"))?;
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
