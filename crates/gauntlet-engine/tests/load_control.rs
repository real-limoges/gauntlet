//! Load-mode pacing observed end-to-end through the loop: constant-RPM spacing
//! and duration-bounded runs.

mod support;

use std::time::Instant;

use gauntlet_engine::run_benchmark;
use support::{parse_config, start};

/// `target_rpm` is the rate the *target* receives, so a target's endpoints share
/// one limiter rather than each pacing itself.
///
/// A limiter per endpoint made the real aggregate `target_rpm × payloads`: the
/// two payloads below would have run in parallel at 600 rpm each, finishing in
/// ~200ms at 1200 rpm against a target configured for 600.
#[tokio::test]
async fn all_of_a_targets_endpoints_share_one_rate_limit() {
    let mock = start(200, serde_json::json!({}), 0).await;
    // 600 rpm → 100ms spacing. 2 payloads × 3 iterations = 6 requests, so five
    // 100ms gaps ⇒ ~500ms shared, against ~200ms if each payload paced itself.
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":3,"concurrency":4,"warmup":{{"iterations":0}},
                         "load_mode":{{"mode":"constant_rpm","target_rpm":600}}}},
            "payloads":[{{"name":"a","method":"GET","path":"/a"}},
                        {{"name":"b","method":"GET","path":"/b"}}]}}"#,
        mock.base
    ));

    let t = Instant::now();
    let run = run_benchmark(&cfg, None).await.unwrap();
    let elapsed = t.elapsed().as_secs_f64();

    let total: usize = run.targets[0]
        .endpoints
        .iter()
        .map(|e| e.outcomes.len())
        .sum();
    assert_eq!(
        total, 6,
        "every payload still fires its full iteration count"
    );
    assert!(
        elapsed >= 0.40,
        "6 requests at an aggregate 600 rpm need ~0.5s; ran in {elapsed:.3}s, \
         which is the per-endpoint rate"
    );
}

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

    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 6);
    assert!(
        elapsed >= 0.040,
        "expected ≥40ms of pacing, got {elapsed:.3}s"
    );
}

/// A duration-based mode (StepLoad) runs for ~its total duration and produces a
/// non-empty, plausibly-sized sample.
///
/// The window closes at the last slot that fits *before* the deadline, so the
/// run ends up to one pacing interval short of `duration_secs` — here the final
/// slot is at 0.5s of a 0.6s step. Overshooting is the failure that matters:
/// workers claim slots atomically up front, and sleeping to a slot past the
/// deadline instead of abandoning it stretches the run by up to
/// `concurrency × interval`.
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
    let n = run.targets[0].endpoints[0].outcomes.len();

    // ≥0.45s: the run must cover the step window bar its last pacing interval.
    assert!(elapsed >= 0.45, "ran {elapsed:.3}s, expected ~0.5s");
    // <0.9s: with concurrency 2 the old sleep-then-check would have run to the
    // 0.6s slot and beyond; the deadline must bound the run, not the slots.
    assert!(elapsed < 0.9, "ran past its duration: {elapsed:.3}s");
    assert!((3..=12).contains(&n), "got {n} requests, expected ~6");
}
