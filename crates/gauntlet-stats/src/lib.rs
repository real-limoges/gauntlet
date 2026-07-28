//! `gauntlet-stats` — the pure statistical core: descriptive statistics,
//! Bayesian comparison, and Earth Mover's Distance.
//!
//! Pure functions, no async, no I/O. Keep it dependency-light (`libm` only): if
//! something here needs `tokio`, `serde`, or `reqwest`, it belongs in another
//! crate.
//!
//! # Correctness rests on Rust-native tests
//!
//! The port from Haskell was a clean break, with no golden-vector parity against
//! the old binary. Correctness therefore comes from closed-form anchors and
//! self-consistency properties in this crate's own tests, not from diffing two
//! implementations. Where a formula below has a specific published source, it is
//! named so the choice can be rechecked against the source rather than against
//! the Haskell.
//!
//! # The estimators, and why these ones
//!
//! - **Mean, variance, standard deviation** ([`common`]) use the unbiased (n−1)
//!   sample estimator, two-pass. Fewer than two elements yield 0 rather than
//!   NaN.
//! - **Percentiles** ([`common::percentile_sorted`]) use **R-7** linear
//!   interpolation, the NumPy and R default: `idx = p·(n−1)`, interpolating
//!   between the floor and ceil indices.
//! - **Expected shortfall** ([`descriptive::expected_shortfall`]) is the mean of
//!   the worst 1%, E[X | X > p99].
//! - **Histogram bin count** ([`descriptive::compute_histogram`]) follows
//!   Sturges' rule clamped to `[8, 20]` — enough resolution to show a bimodal
//!   latency distribution without turning a small sample into noise.
//! - **The inverse normal CDF** ([`normal::inverse_normal_cdf`]) is the rational
//!   approximation from Abramowitz & Stegun 26.2.23 (max error ≈4.5e-4), *not*
//!   an exact inverse. It is accurate well past the precision of any latency
//!   measurement, and it keeps the crate free of a heavier numerics dependency.
//!   The forward CDF is exact, via `libm::erfc`.
//! - **Earth Mover's Distance** ([`emd`]) takes the cheap path
//!   `mean(|sorted_a − sorted_b|)` when both samples are the same size, and
//!   integrates the area between the two CDFs when they are not. Empty input is
//!   0, not NaN.
//!
//! # Conventions
//!
//! Every numeric field is milliseconds, as `f64`. A positive `mean_difference`
//! in a [`BayesianComparison`] means **A is slower** — that is, B is faster.
//! `Default` is derived on the result types for ergonomic test construction.
//!
//! [`compare_bayesian`] deliberately leaves `emd` as `None`: Earth Mover's
//! Distance needs the raw sample vectors, and this function only has summary
//! statistics. The caller attaches it.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
// `float_cmp` matters most in this crate: a silent float bug in the numbers the
// tool exists to report is its most expensive failure mode.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

pub mod bayesian;
pub mod common;
pub mod descriptive;
pub mod emd;
pub mod normal;
pub mod types;

pub use bayesian::{all_pair_comparisons, compare_bayesian};
pub use common::{mean, percentile, percentile_sorted, std_dev, variance};
pub use descriptive::{calculate_stats, compute_histogram, expected_shortfall};
pub use emd::earth_movers_distance;
pub use normal::{inverse_normal_cdf, standard_normal_cdf};
pub use types::{BayesianComparison, BenchmarkStats, PercentileComparison};
