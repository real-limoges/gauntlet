//! Descriptive statistics, ported from `calculateStats`, `expectedShortfall`,
//! and `computeHistogram` in `Stats/Benchmark.hs`.

use crate::common::{mean, percentile_sorted, std_dev};
use crate::types::BenchmarkStats;

/// Percentile threshold for Expected Shortfall (ES = E[X | X > p99]).
const ES_PERCENTILE: f64 = 0.99;

/// Compute descriptive statistics from already-extracted successful durations
/// (in ms) plus the total number of requests issued.
///
/// `durations_ms` holds only the *successful* samples; `count_failure` is the
/// difference from `total_requests`. (The Haskell `calculateStats` folds the
/// error filtering and ns→ms conversion in via `getDuration`; that extraction
/// depends on `TestingResponse` and lands in `gauntlet-core` in M2.)
pub fn calculate_stats(total_requests: usize, durations_ms: &[f64]) -> BenchmarkStats {
    let count_success = durations_ms.len();
    let count_failure = total_requests - count_success;

    let mut sorted = durations_ms.to_vec();
    sorted.sort_by(f64::total_cmp);

    let base = BenchmarkStats {
        total_requests,
        count_success,
        count_failure,
        ..Default::default()
    };

    match sorted.len() {
        // No successful samples: every metric is zero, histogram empty.
        0 => base,
        // Single sample: every positional metric collapses to the one value;
        // stddev is 0 (the n−1 estimator is undefined for n=1).
        1 => {
            let v = sorted[0];
            BenchmarkStats {
                mean_ms: v,
                std_dev_ms: 0.0,
                min_ms: v,
                max_ms: v,
                p50_ms: v,
                p95_ms: v,
                p99_ms: v,
                es_ms: v,
                histogram: compute_histogram(&sorted),
                ..base
            }
        }
        _ => BenchmarkStats {
            mean_ms: mean(&sorted),
            std_dev_ms: std_dev(&sorted),
            min_ms: sorted[0],
            max_ms: sorted[sorted.len() - 1],
            p50_ms: percentile_sorted(0.50, &sorted),
            p95_ms: percentile_sorted(0.95, &sorted),
            p99_ms: percentile_sorted(0.99, &sorted),
            es_ms: expected_shortfall(&sorted),
            histogram: compute_histogram(&sorted),
            ..base
        },
    }
}

/// Expected Shortfall: the mean of the worst 1% of observations (E[X | X > p99]).
///
/// Input must be sorted ascending. Returns the last value for very small samples,
/// 0 for empty input.
pub fn expected_shortfall(sorted: &[f64]) -> f64 {
    let n = sorted.len();
    if n == 0 {
        return 0.0;
    }
    let tail_size = (((1.0 - ES_PERCENTILE) * n as f64).ceil() as usize).max(1);
    let tail = &sorted[n - tail_size..];
    if tail.is_empty() {
        sorted[n - 1]
    } else {
        tail.iter().sum::<f64>() / tail.len() as f64
    }
}

/// Compute a latency histogram from a sorted (ascending) slice.
///
/// Bin count follows Sturges' rule clamped to `[8, 20]`. Returns
/// `(bin_lower_bound, count)` pairs. Guards: empty → `[]`, single → `[(value, 1)]`.
pub fn compute_histogram(sorted: &[f64]) -> Vec<(f64, usize)> {
    match sorted.len() {
        0 => Vec::new(),
        1 => vec![(sorted[0], 1)],
        n => {
            let lo = sorted[0];
            let hi = sorted[n - 1];
            let range = hi - lo;
            let num_bins = ((n as f64).log2() + 1.0).ceil() as usize;
            let num_bins = num_bins.clamp(8, 20);
            let bin_width = range / num_bins as f64;

            let bin_of = |x: f64| -> usize {
                if bin_width <= 0.0 {
                    0
                } else {
                    (((x - lo) / bin_width).floor() as usize).min(num_bins - 1)
                }
            };

            let mut counts = vec![0usize; num_bins];
            for &x in sorted {
                counts[bin_of(x)] += 1;
            }
            (0..num_bins)
                .map(|i| (lo + i as f64 * bin_width, counts[i]))
                .collect()
        }
    }
}
