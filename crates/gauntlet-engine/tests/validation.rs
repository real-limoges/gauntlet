//! Response validation observed through a full run: the endpoint's
//! `ValidationSummary` reflects status + field assertions, capped at 50 failing
//! responses.

mod support;

use gauntlet_engine::run_benchmark;
use support::{parse_config, start};

/// All assertions hold → zero failures.
#[tokio::test]
async fn passing_validation_reports_no_failures() {
    let mock = start(200, serde_json::json!({"id": 1, "name": "ok"}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":5,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping",
              "validate":{{"status":200,
                "fields":{{"$.id":"present","$.name":{{"eq":"ok"}}}}}}}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let v = &run.targets[0].endpoints[0].validation;
    assert_eq!(v.total, 5);
    assert_eq!(v.failed, 0);
    assert!(v.errors.is_empty());
}

/// Status + field mismatches are both reported per response.
#[tokio::test]
async fn failing_validation_reports_errors() {
    let mock = start(418, serde_json::json!({"id": 1, "name": "wrong"}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":3,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping",
              "validate":{{"status":200,
                "fields":{{"$.name":{{"eq":"ok"}}}}}}}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let v = &run.targets[0].endpoints[0].validation;
    assert_eq!(v.total, 3);
    assert_eq!(v.failed, 3, "every response fails status + field");
    // Two failed assertions (status, $.name) per failing response.
    assert!(v.errors.iter().any(|e| e.field == "status"));
    assert!(v.errors.iter().any(|e| e.field == "$.name"));
}

/// Errors are collected from at most the first 50 failing responses.
#[tokio::test]
async fn validation_errors_are_capped_at_fifty_responses() {
    let mock = start(200, serde_json::json!({"v": 0}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":60,"concurrency":4,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping",
              "validate":{{"fields":{{"$.v":{{"eq":1}}}}}}}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let v = &run.targets[0].endpoints[0].validation;
    assert_eq!(v.total, 60);
    assert_eq!(v.failed, 60, "all 60 fail the eq check");
    // One assertion per response ⇒ at most 50 collected errors.
    assert_eq!(v.errors.len(), 50);
}
