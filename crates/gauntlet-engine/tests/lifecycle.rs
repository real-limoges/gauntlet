//! Lifecycle hooks: setup runs and a health check gates the benchmark; a failing
//! setup aborts the run before any request is sent.

mod support;

use gauntlet_engine::{run_benchmark, EngineError};
use support::{parse_config, start};

/// Setup succeeds, the health check (pointed at the live mock) passes, the
/// benchmark runs, and teardown executes.
#[tokio::test]
async fn setup_health_check_and_teardown_run() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{base}",
              "lifecycle":{{
                "setup":{{"cmd":"true"}},
                "teardown":{{"cmd":"true"}},
                "health_check":{{"url":"{base}/health","interval_ms":50,"timeout_secs":5}}
              }}}}],
            "settings":{{"iterations":3,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        base = mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 3);
}

/// A failing setup hook aborts before any benchmark request is sent.
#[tokio::test]
async fn failing_setup_aborts_the_run() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{base}",
              "lifecycle":{{"setup":{{"cmd":"exit 1"}}}}}}],
            "settings":{{"iterations":3,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        base = mock.base
    ));

    let err = run_benchmark(&cfg, None).await.unwrap_err();
    assert!(matches!(err, EngineError::Lifecycle { .. }), "got {err:?}");
    assert_eq!(mock.request_count(), 0, "no requests after failed setup");
}
