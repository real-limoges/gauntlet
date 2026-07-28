//! Correctness anchors for the descriptive-statistics core (`calculate_stats`,
//! `expected_shortfall`, `compute_histogram`, and the `common` helpers).
//!
//! Anchored on closed forms and textbook quantiles — no Haskell oracle (the port
//! is a deliberate clean break).

use gauntlet_stats::{
    calculate_stats, compute_histogram, expected_shortfall, mean, percentile, percentile_sorted,
    std_dev, variance,
};

const EPS: f64 = 1e-9;

fn close(a: f64, b: f64) -> bool {
    (a - b).abs() < EPS
}

fn one_to_ten() -> Vec<f64> {
    (1..=10).map(|i| i as f64).collect()
}

// ---- common: mean / variance / std_dev (unbiased n−1) ------------------------

#[test]
fn mean_variance_std_dev_exact() {
    let xs = [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
    // sum 40, n 8 → mean 5; Σ(x−5)² = 32; unbiased variance = 32/7.
    assert!(close(mean(&xs), 5.0));
    assert!(close(variance(&xs), 32.0 / 7.0));
    assert!(close(std_dev(&xs), (32.0_f64 / 7.0).sqrt()));
}

#[test]
fn variance_small_samples_are_zero() {
    assert!(close(variance(&[]), 0.0));
    assert!(close(variance(&[5.0]), 0.0));
    assert!(close(std_dev(&[5.0]), 0.0));
    assert!(close(mean(&[]), 0.0));
}

// ---- common: percentile (R-7) ------------------------------------------------

#[test]
fn percentile_r7_median() {
    // idx = 0.5·9 = 4.5 → ½·(5 + 6) = 5.5
    assert!(close(percentile(0.5, &one_to_ten()), 5.5));
}

#[test]
fn percentile_r7_interpolated() {
    // p95: idx = 0.95·9 = 8.55 → 9·0.45 + 10·0.55 = 9.55
    assert!(close(percentile_sorted(0.95, &one_to_ten()), 9.55));
    // p99: idx = 0.99·9 = 8.91 → 9·0.09 + 10·0.91 = 9.91
    assert!(close(percentile_sorted(0.99, &one_to_ten()), 9.91));
}

#[test]
fn percentile_sorted_edge_cases() {
    assert!(close(percentile_sorted(0.5, &[]), 0.0));
    assert!(close(percentile_sorted(0.5, &[42.0]), 42.0));
    assert!(close(percentile_sorted(0.99, &[42.0]), 42.0));
}

// ---- expected_shortfall ------------------------------------------------------

#[test]
fn expected_shortfall_averages_top_tail() {
    // 150 values → tail_size = ceil(0.01·150) = ceil(1.5) = 2 → mean(149, 150) = 149.5.
    // (n chosen so 0.01·n sits between integers, dodging the 1.0−0.99 float artifact.)
    let xs: Vec<f64> = (1..=150).map(|i| i as f64).collect();
    assert!(close(expected_shortfall(&xs), 149.5));
}

#[test]
fn expected_shortfall_edge_cases() {
    assert!(close(expected_shortfall(&[]), 0.0));
    assert!(close(expected_shortfall(&[7.0]), 7.0));
    // n = 10 → tail_size = max(1, ceil(0.1)) = 1 → just the max.
    assert!(close(expected_shortfall(&one_to_ten()), 10.0));
}

// ---- compute_histogram -------------------------------------------------------

#[test]
fn histogram_first_bin_is_min_and_counts_sum() {
    let sorted: Vec<f64> = (1..=10).map(|i| (i * 5) as f64).collect(); // 5..=50
    let bins = compute_histogram(&sorted);
    assert!(close(bins[0].0, 5.0)); // first bin lower bound == min
    assert!((8..=20).contains(&bins.len())); // Sturges clamped to [8, 20]
    let total: usize = bins.iter().map(|(_, c)| c).sum();
    assert_eq!(total, sorted.len());
}

#[test]
fn histogram_edge_cases() {
    assert!(compute_histogram(&[]).is_empty());
    assert_eq!(compute_histogram(&[3.0]), vec![(3.0, 1)]);
}

// ---- calculate_stats ---------------------------------------------------------

#[test]
fn calculate_stats_full_vector() {
    let s = calculate_stats(10, &one_to_ten());
    assert_eq!(s.total_requests, 10);
    assert_eq!(s.count_success, 10);
    assert_eq!(s.count_failure, 0);
    assert!(close(s.mean_ms, 5.5));
    assert!(close(s.min_ms, 1.0));
    assert!(close(s.max_ms, 10.0));
    assert!(close(s.p50_ms, 5.5));
    assert!(close(s.p95_ms, 9.55));
    assert!(close(s.p99_ms, 9.91));
    // histogram counts every successful sample.
    let total: usize = s.histogram.iter().map(|(_, c)| c).sum();
    assert_eq!(total, s.count_success);
}

#[test]
fn calculate_stats_counts_failures() {
    // 10 successful samples out of 12 issued → 2 failures.
    let s = calculate_stats(12, &one_to_ten());
    assert_eq!(s.count_success, 10);
    assert_eq!(s.count_failure, 2);
}

#[test]
fn calculate_stats_no_successes() {
    let s = calculate_stats(5, &[]);
    assert_eq!(s.count_success, 0);
    assert_eq!(s.count_failure, 5);
    assert!(close(s.mean_ms, 0.0));
    assert!(close(s.es_ms, 0.0));
    assert!(s.histogram.is_empty());
}

#[test]
fn calculate_stats_single_sample() {
    let s = calculate_stats(1, &[42.0]);
    assert_eq!(s.count_success, 1);
    assert!(close(s.mean_ms, 42.0));
    assert!(close(s.std_dev_ms, 0.0));
    assert!(close(s.min_ms, 42.0));
    assert!(close(s.max_ms, 42.0));
    assert!(close(s.p50_ms, 42.0));
    assert!(close(s.p99_ms, 42.0));
    assert!(close(s.es_ms, 42.0));
    assert_eq!(s.histogram, vec![(42.0, 1)]);
}
