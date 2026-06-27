//! The M1-deferred bridge: `latency_ms`/`extract_durations` drop failed
//! responses and convert ns → ms, then feed `gauntlet_stats::calculate_stats`
//! end-to-end.

use gauntlet_core::types::units::Nanoseconds;
use gauntlet_core::{extract_durations, TestingResponse};
use gauntlet_stats::calculate_stats;

const EPS: f64 = 1e-9;

fn ok(duration_ns: u64) -> TestingResponse {
    TestingResponse {
        duration: Nanoseconds(duration_ns),
        status: 200,
        error: None,
    }
}

fn failed(duration_ns: u64) -> TestingResponse {
    TestingResponse {
        duration: Nanoseconds(duration_ns),
        status: 0,
        error: Some("connection refused".to_owned()),
    }
}

#[test]
fn latency_ms_converts_ns_and_drops_failures() {
    assert!((ok(1_500_000).latency_ms().unwrap() - 1.5).abs() < EPS);
    assert_eq!(failed(1_500_000).latency_ms(), None);
}

#[test]
fn extract_durations_keeps_only_successes() {
    let responses = vec![
        ok(1_000_000),
        failed(9_000_000),
        ok(2_000_000),
        failed(8_000_000),
        ok(3_000_000),
    ];
    assert_eq!(extract_durations(&responses), vec![1.0, 2.0, 3.0]);
}

#[test]
fn extraction_feeds_calculate_stats() {
    let responses = vec![
        ok(1_000_000),
        ok(2_000_000),
        failed(0),
        ok(3_000_000),
        ok(4_000_000),
        failed(0),
        ok(5_000_000),
    ];
    let durations = extract_durations(&responses);
    let stats = calculate_stats(responses.len(), &durations);

    assert_eq!(stats.total_requests, 7);
    assert_eq!(stats.count_success, 5);
    assert_eq!(stats.count_failure, 2);
    assert!((stats.mean_ms - 3.0).abs() < EPS);
    assert!((stats.min_ms - 1.0).abs() < EPS);
    assert!((stats.max_ms - 5.0).abs() < EPS);
}
