//! Load-mode pacing observed end-to-end through the loop: constant-RPM spacing
//! and duration-bounded runs.

mod support;

use std::time::Instant;

use gauntlet_engine::run_benchmark;
use support::{parse_config, start};

/// ConstantRpm paces dispatch: N requests take roughly `60·(N−1)/rpm` seconds.
/// 6000 rpm → 10ms spacing; 6 requests ⇒ ~50ms over 5 gaps.
#[tokio::test]
async fn constant_rpm_paces_dispatch() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":6,"concurrency":6,"warmup":{{"iterations":0}},
                         "load_mode":{{"mode":"constant_rpm","target_rpm":6000}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let t = Instant::now();
    let run = run_benchmark(&cfg, None).await.unwrap();
    let elapsed = t.elapsed().as_secs_f64();

    assert_eq!(run.targets[0].endpoints[0].responses.len(), 6);
    assert!(
        elapsed >= 0.040,
        "expected ≥40ms of pacing, got {elapsed:.3}s"
    );
}

/// A duration-based mode (StepLoad) runs for ~its total duration and produces a
/// non-empty, plausibly-sized sample.
#[tokio::test]
async fn step_load_runs_for_its_duration() {
    let mock = start(200, serde_json::json!({}), 0).await;
    // One 0.6s step at 600 rpm (10/s) ⇒ ~6 requests, ~0.6s wall clock.
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":1,"concurrency":2,"warmup":{{"iterations":0}},
                         "load_mode":{{"mode":"step_load",
                            "steps":[{{"rpm":600,"duration_secs":0.6}}]}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let t = Instant::now();
    let run = run_benchmark(&cfg, None).await.unwrap();
    let elapsed = t.elapsed().as_secs_f64();
    let n = run.targets[0].endpoints[0].responses.len();

    assert!(elapsed >= 0.55, "ran {elapsed:.3}s, expected ~0.6s");
    assert!(elapsed < 1.5, "ran too long: {elapsed:.3}s");
    assert!((3..=12).contains(&n), "got {n} requests, expected ~6");
}
