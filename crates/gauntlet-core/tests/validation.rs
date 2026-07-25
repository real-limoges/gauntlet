//! Two layers of rejection:
//!   1. the type system rejects bad methods, zero counts, and unknown keys at
//!      *parse* time (a config with those states can't be constructed);
//!   2. `validate` *accumulates* the remaining semantic rules and reports them
//!      all at once.

use gauntlet_core::{build_endpoints, BenchmarkConfig, HttpMethod, PayloadSpec};

fn parses(json: &str) -> bool {
    serde_json::from_str::<BenchmarkConfig>(json).is_ok()
}

/// Parse (must succeed), then validate and return the accumulated messages.
fn validation_errors(json: &str) -> Vec<String> {
    let cfg: BenchmarkConfig = serde_json::from_str(json).expect("must deserialize");
    cfg.validate().expect_err("expected validation to fail").0
}

fn base(targets: &str, settings: &str, payloads: &str) -> String {
    format!(r#"{{"targets":{targets},"settings":{settings},"payloads":{payloads}}}"#)
}

const OK_TARGET: &str = r#"[{"name":"a","url":"http://x"}]"#;
const OK_PAYLOAD: &str = r#"[{"name":"p","method":"GET","path":"/"}]"#;

fn settings(extra: &str) -> String {
    format!(r#"{{"iterations":10,"concurrency":2{extra}}}"#)
}

#[test]
fn minimal_config_is_accepted() {
    let cfg: BenchmarkConfig =
        serde_json::from_str(&base(OK_TARGET, &settings(""), OK_PAYLOAD)).unwrap();
    assert!(cfg.validate().is_ok());
}

// ---- parse-time rejections (the type system) ------------------------------

#[test]
fn type_system_rejects_bad_states_at_parse_time() {
    // zero iterations — NonZeroU32 can't hold it.
    assert!(!parses(&base(
        OK_TARGET,
        r#"{"iterations":0,"concurrency":2}"#,
        OK_PAYLOAD
    )));
    // unknown HTTP method — HttpMethod enum has no such variant.
    assert!(!parses(&base(
        OK_TARGET,
        &settings(""),
        r#"[{"name":"p","method":"FETCH","path":"/"}]"#
    )));
    // unknown field — deny_unknown_fields catches the typo.
    assert!(!parses(&base(
        OK_TARGET,
        r#"{"iterations":10,"concurrency":2,"iteratoins":5}"#,
        OK_PAYLOAD
    )));
}

// ---- accumulating semantic validation -------------------------------------

#[test]
fn rejects_empty_collections() {
    assert_eq!(
        validation_errors(&base("[]", &settings(""), OK_PAYLOAD)),
        vec!["must define at least one target"]
    );
    assert_eq!(
        validation_errors(&base(OK_TARGET, &settings(""), "[]")),
        vec!["must define at least one payload"]
    );
}

/// Names are identities: a target names its baseline file, and a payload is
/// matched by name when endpoint results are put back into config order. Two
/// entries sharing a name make one of them unreachable.
#[test]
fn rejects_duplicate_target_and_payload_names() {
    let dup_targets = r#"[{"name":"a","url":"http://x"},{"name":"a","url":"http://y"}]"#;
    assert_eq!(
        validation_errors(&base(dup_targets, &settings(""), OK_PAYLOAD)),
        vec![r#"targets: duplicate name "a""#]
    );

    let dup_payloads = r#"[{"name":"p","method":"GET","path":"/a"},
                           {"name":"p","method":"GET","path":"/b"}]"#;
    assert_eq!(
        validation_errors(&base(OK_TARGET, &settings(""), dup_payloads)),
        vec![r#"payloads: duplicate name "p""#]
    );

    // Reported once per offending name, however many times it repeats.
    let triple = r#"[{"name":"a","url":"http://x"},
                     {"name":"a","url":"http://y"},
                     {"name":"a","url":"http://z"}]"#;
    assert_eq!(
        validation_errors(&base(triple, &settings(""), OK_PAYLOAD)).len(),
        1
    );
}

#[test]
fn rejects_small_backoff_multiplier() {
    let json = base(
        OK_TARGET,
        &settings(r#","retry":{"backoff_multiplier":0.5}"#),
        OK_PAYLOAD,
    );
    assert_eq!(
        validation_errors(&json),
        vec!["settings.retry.backoff_multiplier must be at least 1.0"]
    );
}

#[test]
fn rejects_bad_load_modes() {
    let cases = [
        (
            r#"{"mode":"poisson_rpm","target_rpm":0}"#,
            vec!["settings.load_mode.target_rpm must be greater than 0"],
        ),
        (
            r#"{"mode":"constant_rpm","target_rpm":0}"#,
            vec!["settings.load_mode.target_rpm must be greater than 0"],
        ),
        (
            r#"{"mode":"ramp_up","start_rpm":0,"end_rpm":0,"duration_secs":5}"#,
            vec![
                "settings.load_mode.start_rpm must be greater than 0",
                "settings.load_mode.end_rpm must be greater than 0",
            ],
        ),
        (
            r#"{"mode":"step_load","steps":[]}"#,
            vec!["settings.load_mode.steps must not be empty"],
        ),
        (
            r#"{"mode":"step_load","steps":[{"rpm":0,"duration_secs":0}]}"#,
            vec![
                "settings.load_mode.steps[0].rpm must be greater than 0",
                "settings.load_mode.steps[0].duration_secs must be greater than 0",
            ],
        ),
    ];
    for (mode, want) in cases {
        let json = base(
            OK_TARGET,
            &settings(&format!(r#","load_mode":{mode}"#)),
            OK_PAYLOAD,
        );
        assert_eq!(validation_errors(&json), want, "mode {mode}");
    }
}

#[test]
fn rejects_lifecycle_problems() {
    let cases = [
        (
            r#"{"setup":{"cmd":"  "}}"#,
            "targets[0].lifecycle.setup.cmd must not be empty",
        ),
        (
            r#"{"teardown":{"cmd":"   "}}"#,
            "targets[0].lifecycle.teardown.cmd must not be empty",
        ),
        (
            r#"{"health_check":{"url":""}}"#,
            "targets[0].lifecycle.health_check.url must not be empty",
        ),
    ];
    for (lifecycle, want) in cases {
        let targets = format!(r#"[{{"name":"a","url":"http://x","lifecycle":{lifecycle}}}]"#);
        let json = base(&targets, &settings(""), OK_PAYLOAD);
        assert_eq!(
            validation_errors(&json),
            vec![want.to_owned()],
            "lifecycle {lifecycle}"
        );
    }
}

#[test]
fn validation_accumulates_every_problem() {
    // Empty targets + bad backoff + bad load mode → three messages, one pass.
    let json = base(
        "[]",
        &settings(
            r#","retry":{"backoff_multiplier":0.5},"load_mode":{"mode":"constant_rpm","target_rpm":-1}"#,
        ),
        OK_PAYLOAD,
    );
    let errs = validation_errors(&json);
    assert_eq!(errs.len(), 3, "got {errs:?}");
    assert!(errs.iter().any(|e| e.contains("at least one target")));
    assert!(errs.iter().any(|e| e.contains("backoff_multiplier")));
    assert!(errs.iter().any(|e| e.contains("target_rpm")));
}

// ---- build_endpoints -------------------------------------------------------

#[test]
fn build_endpoints_adds_default_content_type_and_joins_url() {
    let payloads: Vec<PayloadSpec> =
        serde_json::from_str(r#"[{"name":"p","method":"POST","path":"/v1/items","body":{"a":1}}]"#)
            .unwrap();
    let eps = build_endpoints("http://host", &payloads);
    assert_eq!(eps.len(), 1);
    assert_eq!(eps[0].url, "http://host/v1/items");
    assert_eq!(eps[0].method, HttpMethod::Post);
    assert_eq!(
        eps[0].headers,
        vec![("Content-Type".to_owned(), "application/json".to_owned())]
    );
}

#[test]
fn build_endpoints_respects_custom_content_type() {
    let payloads: Vec<PayloadSpec> = serde_json::from_str(
        r#"[{"name":"p","method":"GET","path":"/","headers":{"Content-Type":"text/plain","X-Trace":"1"}}]"#,
    )
    .unwrap();
    let eps = build_endpoints("http://host", &payloads);
    assert_eq!(
        eps[0].headers,
        vec![
            ("Content-Type".to_owned(), "text/plain".to_owned()),
            ("X-Trace".to_owned(), "1".to_owned()),
        ]
    );
}
