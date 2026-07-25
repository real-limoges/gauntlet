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

/// A target naming a branch that cannot be checked out fails the run.
///
/// The field used to be parsed and then ignored, which is the worst outcome
/// available: the run would benchmark whatever was already checked out, once per
/// target, and report a confident comparison of a build against itself.
///
/// Only the failure path is exercised — `switch_branch` runs `git switch` in the
/// process working directory, so a passing case would move this very checkout.
#[tokio::test]
async fn a_branch_that_cannot_be_checked_out_fails_the_run() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{base}",
              "branch":"gauntlet-no-such-branch-8f21c"}}],
            "settings":{{"iterations":3,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        base = mock.base
    ));

    let err = run_benchmark(&cfg, None).await.unwrap_err();
    assert!(matches!(err, EngineError::Lifecycle { .. }), "got {err:?}");
    assert!(
        err.to_string().contains("gauntlet-no-such-branch-8f21c"),
        "the message should name the branch: {err}"
    );
    assert_eq!(mock.request_count(), 0, "no requests after a failed switch");
}

/// An empty branch string is a no-op rather than an error, matching the Haskell.
#[tokio::test]
async fn an_empty_branch_is_a_no_op() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{base}","branch":""}}],
            "settings":{{"iterations":2,"concurrency":1,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        base = mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 2);
}
