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

    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 6);
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
    let n = run.targets[0].endpoints[0].outcomes.len();

    assert!(elapsed >= 0.55, "ran {elapsed:.3}s, expected ~0.6s");
    assert!(elapsed < 1.5, "ran too long: {elapsed:.3}s");
    assert!((3..=12).contains(&n), "got {n} requests, expected ~6");
}

/// `settings.concurrency` caps in-flight requests for the **target**, not for
/// each payload. The endpoints of a target run concurrently, so a per-endpoint
/// semaphore would let a 3-payload config put 3x the configured load on the
/// service under test.
#[tokio::test]
async fn concurrency_caps_the_target_not_each_payload() {
    let mock = start(200, serde_json::json!({}), 25).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":4,"concurrency":2,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"a","method":"GET","path":"/a"}},
                        {{"name":"b","method":"GET","path":"/b"}},
                        {{"name":"c","method":"GET","path":"/c"}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();

    assert_eq!(mock.request_count(), 12, "3 payloads x 4 iterations");
    assert_eq!(run.targets[0].endpoints.len(), 3);
    assert!(
        mock.max_in_flight() <= 2,
        "concurrency is 2, but {} requests were in flight at once — \
         the semaphore is per-endpoint rather than per-target",
        mock.max_in_flight()
    );
}

/// Likewise for pacing: `target_rpm` is the rate against the target. A
/// per-endpoint limiter would run N independent streams at the full rate each.
#[tokio::test]
async fn target_rpm_paces_the_target_not_each_payload() {
    let mock = start(200, serde_json::json!({}), 0).await;
    // 6000 rpm ⇒ 10ms spacing. 3 payloads x 4 iterations = 12 requests sharing
    // one limiter ⇒ ~110ms over 11 gaps. Three separate limiters would finish
    // in ~30ms.
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":4,"concurrency":12,"warmup":{{"iterations":0}},
                         "load_mode":{{"mode":"constant_rpm","target_rpm":6000}}}},
            "payloads":[{{"name":"a","method":"GET","path":"/a"}},
                        {{"name":"b","method":"GET","path":"/b"}},
                        {{"name":"c","method":"GET","path":"/c"}}]}}"#,
        mock.base
    ));

    let t = Instant::now();
    run_benchmark(&cfg, None).await.unwrap();
    let elapsed = t.elapsed().as_secs_f64();

    assert_eq!(mock.request_count(), 12);
    assert!(
        elapsed >= 0.09,
        "12 requests at 6000 rpm should take ~110ms, took {elapsed:.3}s — \
         each payload is being paced by its own limiter"
    );
}

/// A duration-based run stops at its deadline even when the interval between
/// slots is far longer than the run itself.
///
/// The limiter hands out slots off a clock that keeps advancing, so a worker that
/// waits unconditionally sleeps out a slot the deadline has long since passed.
/// At the 6 rpm floor that is a 10s sleep per worker on a 0.5s run.
#[tokio::test]
async fn a_duration_run_stops_at_its_deadline_even_at_the_rpm_floor() {
    let mock = start(200, serde_json::json!({}), 0).await;
    // ramp_up pinned at the 6 rpm floor ⇒ 10s between slots, over a 0.5s window.
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":1,"concurrency":3,"warmup":{{"iterations":0}},
                         "load_mode":{{"mode":"ramp_up","start_rpm":6,"end_rpm":6,
                                       "duration_secs":0.5}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let t = Instant::now();
    run_benchmark(&cfg, None).await.unwrap();
    let elapsed = t.elapsed().as_secs_f64();

    assert!(
        elapsed < 3.0,
        "a 0.5s run took {elapsed:.3}s — workers are sleeping out slots past the deadline"
    );
    assert!(
        elapsed >= 0.45,
        "a 0.5s run took {elapsed:.3}s — it should occupy its full window"
    );
}
