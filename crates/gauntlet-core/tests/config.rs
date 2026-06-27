//! Config-loading correctness: every committed `examples/*.json` deserializes
//! and validates, the derived schema generates, `LoadMode`/`FieldAssertion`
//! round-trip, and `${VAR}` interpolation behaves.

use std::collections::HashMap;
use std::path::PathBuf;

use gauntlet_core::config::env::interpolate_env;
use gauntlet_core::types::config::LoadMode;
use gauntlet_core::types::response::FieldAssertion;
use gauntlet_core::{config_schema_string, BenchmarkConfig};

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples")
}

fn example_files() -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = std::fs::read_dir(examples_dir())
        .expect("examples/ must exist")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|x| x == "json"))
        .collect();
    files.sort();
    files
}

#[test]
fn every_example_deserializes_and_validates() {
    let files = example_files();
    assert!(
        files.len() >= 8,
        "expected the committed example set, got {files:?}"
    );
    for path in files {
        let text = std::fs::read_to_string(&path).unwrap();
        let cfg: BenchmarkConfig = serde_json::from_str(&text)
            .unwrap_or_else(|e| panic!("{} failed to deserialize: {e}", path.display()));
        cfg.validate()
            .unwrap_or_else(|e| panic!("{} failed validation: {e}", path.display()));
    }
}

#[test]
fn derived_schema_matches_committed_file() {
    // The committed schema is regenerated from the types; it must stay in sync.
    let committed = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../schema/config-schema.json"),
    )
    .unwrap();
    assert_eq!(
        config_schema_string().trim(),
        committed.trim(),
        "schema/config-schema.json is stale — regenerate with \
         `cargo run -p gauntlet-core --example print_schema > schema/config-schema.json`"
    );
}

// ---- LoadMode round-trips all five variants -------------------------------

fn load_mode(json: &str) -> LoadMode {
    serde_json::from_str(json).unwrap()
}

#[test]
fn load_mode_parses_all_variants() {
    assert_eq!(
        load_mode(r#"{"mode":"unthrottled"}"#),
        LoadMode::Unthrottled
    );
    assert_eq!(
        load_mode(r#"{"mode":"constant_rpm","target_rpm":120}"#),
        LoadMode::ConstantRpm { target_rpm: 120.0 }
    );
    assert_eq!(
        load_mode(r#"{"mode":"poisson_rpm","target_rpm":50}"#),
        LoadMode::PoissonRpm { target_rpm: 50.0 }
    );
    assert_eq!(
        load_mode(r#"{"mode":"ramp_up","start_rpm":10,"end_rpm":100,"duration_secs":60}"#),
        LoadMode::RampUp {
            start_rpm: 10.0,
            end_rpm: 100.0,
            duration_secs: 60.0,
        }
    );
    assert_eq!(
        load_mode(r#"{"mode":"step_load","steps":[{"rpm":10,"duration_secs":5}]}"#),
        LoadMode::StepLoad {
            steps: vec![gauntlet_core::LoadStep {
                rpm: 10.0,
                duration_secs: 5.0,
            }],
        }
    );
}

#[test]
fn load_mode_default_is_unthrottled() {
    assert_eq!(LoadMode::default(), LoadMode::Unthrottled);
}

// ---- FieldAssertion native (de)serialization ------------------------------

#[test]
fn field_assertion_unit_variants_are_bare_strings() {
    assert_eq!(
        serde_json::from_str::<FieldAssertion>(r#""present""#).unwrap(),
        FieldAssertion::Present
    );
    assert_eq!(
        serde_json::from_str::<FieldAssertion>(r#""not_null""#).unwrap(),
        FieldAssertion::NotNull
    );
    assert_eq!(
        serde_json::to_string(&FieldAssertion::Present).unwrap(),
        r#""present""#
    );
}

#[test]
fn field_assertion_data_variants_are_single_key_objects() {
    let eq: FieldAssertion = serde_json::from_str(r#"{"eq":42}"#).unwrap();
    assert_eq!(eq, FieldAssertion::Eq(serde_json::json!(42)));

    let range: FieldAssertion = serde_json::from_str(r#"{"range":{"min":1.0,"max":9.0}}"#).unwrap();
    assert_eq!(
        range,
        FieldAssertion::Range {
            min: Some(1.0),
            max: Some(9.0)
        }
    );

    let len: FieldAssertion = serde_json::from_str(r#"{"array_length":3}"#).unwrap();
    assert_eq!(len, FieldAssertion::ArrayLength(3));
}

// ---- ${VAR} interpolation --------------------------------------------------

fn env_of(pairs: &[(&str, &str)]) -> HashMap<String, String> {
    pairs
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[test]
fn interpolation_substitutes_and_errors_on_undefined() {
    let env = env_of(&[("HOST", "example.com"), ("PORT", "8080")]);
    assert_eq!(
        interpolate_env(&env, "http://${HOST}:${PORT}/api").unwrap(),
        "http://example.com:8080/api"
    );
    assert_eq!(
        interpolate_env(&env, "no vars here").unwrap(),
        "no vars here"
    );
    // The error carries the bare variable name; the crate `Error` owns the prose.
    assert_eq!(interpolate_env(&env, "${MISSING}").unwrap_err(), "MISSING");
}

#[test]
fn interpolation_unclosed_brace_is_literal() {
    let env = env_of(&[("HOST", "example.com")]);
    assert_eq!(
        interpolate_env(&env, "prefix ${HOST and more").unwrap(),
        "prefix ${HOST and more"
    );
}
