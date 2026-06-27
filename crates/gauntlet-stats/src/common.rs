//! Shared descriptive helpers, ported from `Stats/Common.hs`.
//!
//! `mean`/`variance`/`std_dev` use the unbiased (n−1) sample estimator, matching
//! the Haskell `varianceList`. (`calculateStats` on the Haskell side reaches for
//! the `statistics` package's `mean`/`stdDev`; with the parity oracle dropped we
//! only need correctness, and the unbiased two-pass formulas here are correct.)

/// Arithmetic mean. Empty input → 0.
pub fn mean(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        0.0
    } else {
        xs.iter().sum::<f64>() / xs.len() as f64
    }
}

/// Unbiased (n−1) sample variance. Fewer than two elements → 0.
pub fn variance(xs: &[f64]) -> f64 {
    let n = xs.len();
    if n <= 1 {
        return 0.0;
    }
    let m = mean(xs);
    let sum_sq: f64 = xs.iter().map(|x| (x - m).powi(2)).sum();
    sum_sq / (n - 1) as f64
}

/// Unbiased (n−1) sample standard deviation. Fewer than two elements → 0.
pub fn std_dev(xs: &[f64]) -> f64 {
    variance(xs).sqrt()
}

/// Percentile of an unsorted slice (sorts a copy, then [`percentile_sorted`]).
pub fn percentile(p: f64, xs: &[f64]) -> f64 {
    let mut sorted = xs.to_vec();
    sorted.sort_by(f64::total_cmp);
    percentile_sorted(p, &sorted)
}

/// Percentile of an already-sorted (ascending) slice, via **R-7** linear
/// interpolation (the NumPy/R default): `idx = p·(n−1)`, interpolate between the
/// floor and ceil indices.
///
/// Guards: empty → 0, single element → that element.
pub fn percentile_sorted(p: f64, sorted: &[f64]) -> f64 {
    match sorted.len() {
        0 => 0.0,
        1 => sorted[0],
        n => {
            let idx = p * (n - 1) as f64;
            let lower = idx.floor() as usize;
            let upper = idx.ceil() as usize;
            let frac = idx - lower as f64;
            if lower == upper {
                sorted[lower]
            } else {
                sorted[lower] * (1.0 - frac) + sorted[upper] * frac
            }
        }
    }
}
