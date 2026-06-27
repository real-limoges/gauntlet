//! `gauntlet-stats` — the pure statistical core, ported from the Haskell
//! `Stats/Benchmark.hs` and `Stats/Common.hs`.
//!
//! Pure functions, no async, no I/O. This crate is the parity oracle's first
//! target (milestone **M1**): every function here is validated against the
//! Haskell implementation's output before anything depends on it.
//!
//! Design rule: keep this crate dependency-light. If something needs `tokio`
//! or `reqwest`, it belongs in `gauntlet-engine`, not here.

pub mod bayesian;
pub mod common;
pub mod descriptive;
pub mod emd;
pub mod normal;
pub mod types;

pub use bayesian::compare_bayesian;
pub use common::{mean, percentile, percentile_sorted, std_dev, variance};
pub use descriptive::{calculate_stats, compute_histogram, expected_shortfall};
pub use emd::earth_movers_distance;
pub use normal::{inverse_normal_cdf, standard_normal_cdf};
pub use types::{BayesianComparison, BenchmarkStats, PercentileComparison};
