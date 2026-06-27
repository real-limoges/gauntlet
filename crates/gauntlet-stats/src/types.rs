//! Result types mirroring the Haskell `Benchmark.Types.Stats` records.
//!
//! All numeric fields are milliseconds (`f64`), matching the Haskell side after
//! its `Milliseconds` newtype is unwrapped. `Default` is derived purely for
//! ergonomic test construction (`BenchmarkStats { mean_ms, .., ..Default::default() }`).

/// Descriptive statistics for one target, from `calculateStats`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BenchmarkStats {
    pub total_requests: usize,
    pub count_success: usize,
    pub count_failure: usize,
    pub mean_ms: f64,
    pub std_dev_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
    /// Expected shortfall: mean of the worst 1% (E[X | X > p99]).
    pub es_ms: f64,
    /// (bin lower bound, count) pairs.
    pub histogram: Vec<(f64, usize)>,
}

/// One percentile's comparison between primary (A) and candidate (B).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct PercentileComparison {
    pub pct_difference: f64,
    pub pct_credible_lower: f64,
    pub pct_credible_upper: f64,
    pub prob_pct_regression: f64,
}

/// Pairwise Bayesian comparison. Positive `mean_difference` means A is slower
/// (B is faster). `emd` is attached separately by the caller, since it needs the
/// raw duration vectors rather than these summary statistics.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BayesianComparison {
    /// P(mean_B < mean_A), population level (uses σ/√n).
    pub prob_b_faster_than_a: f64,
    /// P(X_B < X_A), individual request (uses σ).
    pub prob_single_request_faster: f64,
    /// P(σ_B < σ_A), via a log-variance approximation.
    pub prob_b_less_jittery: f64,
    pub mean_difference: f64,
    pub credible_interval_lower: f64,
    pub credible_interval_upper: f64,
    /// Cohen's d (pooled standard deviation).
    pub effect_size: f64,
    /// Mean difference as a percentage of A's mean.
    pub relative_effect: f64,
    pub p95_comparison: PercentileComparison,
    pub p99_comparison: PercentileComparison,
    /// Earth Mover's Distance, attached post-hoc from the raw samples.
    pub emd: Option<f64>,
}
