//! Bayesian comparison of two targets, ported from `compareBayesian` in
//! `Stats/Benchmark.hs`. Every edge-case guard is preserved verbatim.

use crate::normal::{standard_normal_cdf, standard_normal_pdf, Z95};
use crate::types::{BayesianComparison, BenchmarkStats, PercentileComparison};

/// Bayesian comparison of two benchmark results.
///
/// `emd` is left `None`; the caller attaches Earth Mover's Distance from the raw
/// duration vectors (see [`crate::earth_movers_distance`]), mirroring how the
/// Haskell `postAnalysis` attaches it after the fact.
pub fn compare_bayesian(a: &BenchmarkStats, b: &BenchmarkStats) -> BayesianComparison {
    let mu_a = a.mean_ms;
    let mu_b = b.mean_ms;
    let sd_a = a.std_dev_ms;
    let sd_b = b.std_dev_ms;
    let var_a = sd_a.powi(2);
    let var_b = sd_b.powi(2);
    let n_a = a.count_success as f64;
    let n_b = b.count_success as f64;
    let mu_diff = mu_a - mu_b;

    BayesianComparison {
        prob_b_faster_than_a: prob_faster(mu_diff, var_a, var_b, n_a, n_b),
        prob_single_request_faster: prob_single_faster(mu_diff, var_a, var_b),
        prob_b_less_jittery: prob_jitter(sd_a, sd_b, var_a, var_b, n_a, n_b),
        mean_difference: mu_diff,
        credible_interval_lower: ci_bound(mu_diff, var_a, var_b, n_a, n_b, -Z95),
        credible_interval_upper: ci_bound(mu_diff, var_a, var_b, n_a, n_b, Z95),
        effect_size: cohen_d(mu_diff, var_a, var_b, n_a, n_b),
        relative_effect: if mu_a > 0.0 {
            (mu_diff / mu_a) * 100.0
        } else {
            0.0
        },
        p95_comparison: compare_percentile(PercentileInput {
            quantile: 0.95,
            value_a: a.p95_ms,
            value_b: b.p95_ms,
            std_dev_a: sd_a,
            std_dev_b: sd_b,
            count_a: n_a,
            count_b: n_b,
        }),
        p99_comparison: compare_percentile(PercentileInput {
            quantile: 0.99,
            value_a: a.p99_ms,
            value_b: b.p99_ms,
            std_dev_a: sd_a,
            std_dev_b: sd_b,
            count_a: n_a,
            count_b: n_b,
        }),
        emd: None,
    }
}

/// P(mean_B < mean_A) using the population-level standard error σ/√n.
fn prob_faster(mu_diff: f64, var_a: f64, var_b: f64, n_a: f64, n_b: f64) -> f64 {
    if n_a <= 0.0 || n_b <= 0.0 {
        return 0.5;
    }
    let sigma_diff = ((var_a / n_a) + (var_b / n_b)).sqrt();
    if sigma_diff > 0.0 {
        standard_normal_cdf(mu_diff / sigma_diff)
    } else {
        0.5
    }
}

/// One credible-interval bound: muDiff + z·√(varA/nA + varB/nB).
/// Collapses to `mu_diff` when either sample is empty, matching the Haskell.
fn ci_bound(mu_diff: f64, var_a: f64, var_b: f64, n_a: f64, n_b: f64, z: f64) -> f64 {
    if n_a <= 0.0 || n_b <= 0.0 {
        mu_diff
    } else {
        mu_diff + z * ((var_a / n_a) + (var_b / n_b)).sqrt()
    }
}

/// Cohen's d using the pooled standard deviation.
fn cohen_d(mu_diff: f64, var_a: f64, var_b: f64, n_a: f64, n_b: f64) -> f64 {
    let pooled_var = if n_a + n_b > 2.0 {
        ((n_a - 1.0) * var_a + (n_b - 1.0) * var_b) / (n_a + n_b - 2.0)
    } else {
        (var_a + var_b) / 2.0
    };
    let pooled_sd = pooled_var.sqrt();
    if pooled_sd > 0.0 {
        mu_diff / pooled_sd
    } else {
        0.0
    }
}

/// P(X_B < X_A) for individual requests — uses σ, not σ/√n.
fn prob_single_faster(mu_diff: f64, var_a: f64, var_b: f64) -> f64 {
    let sigma_pred = (var_a + var_b).sqrt();
    if sigma_pred > 0.0 {
        standard_normal_cdf(mu_diff / sigma_pred)
    } else {
        0.5
    }
}

/// P(σ_B < σ_A) via log-normal approximation: log(s²) ~ N(log(σ²), 2/(n−1)).
fn prob_jitter(sd_a: f64, sd_b: f64, var_a: f64, var_b: f64, n_a: f64, n_b: f64) -> f64 {
    if sd_a <= 0.0 || sd_b <= 0.0 || n_a <= 1.0 || n_b <= 1.0 {
        return 0.5;
    }
    let se_log_var = (2.0 / (n_a - 1.0) + 2.0 / (n_b - 1.0)).sqrt();
    let z = (var_a.ln() - var_b.ln()) / se_log_var;
    standard_normal_cdf(z)
}

/// Inputs for a single percentile comparison.
struct PercentileInput {
    quantile: f64,
    value_a: f64,
    value_b: f64,
    std_dev_a: f64,
    std_dev_b: f64,
    count_a: f64,
    count_b: f64,
}

/// Compare a percentile between A and B with Maritz-Jarrett standard errors.
fn compare_percentile(pi: PercentileInput) -> PercentileComparison {
    let diff = pi.value_a - pi.value_b;
    if pi.count_a <= 0.0 || pi.count_b <= 0.0 {
        return PercentileComparison {
            pct_difference: diff,
            pct_credible_lower: diff,
            pct_credible_upper: diff,
            prob_pct_regression: 0.5,
        };
    }
    let k = percentile_se_multiplier(pi.quantile);
    let se_a = k * pi.std_dev_a / pi.count_a.sqrt();
    let se_b = k * pi.std_dev_b / pi.count_b.sqrt();
    let se_diff = (se_a.powi(2) + se_b.powi(2)).sqrt();
    let z = if se_diff > 0.0 { diff / se_diff } else { 0.0 };
    PercentileComparison {
        pct_difference: diff,
        pct_credible_lower: diff - Z95 * se_diff,
        pct_credible_upper: diff + Z95 * se_diff,
        prob_pct_regression: 1.0 - standard_normal_cdf(z),
    }
}

/// Maritz-Jarrett standard-error multiplier for a percentile.
fn percentile_se_multiplier(p: f64) -> f64 {
    let phi = standard_normal_pdf(crate::normal::inverse_normal_cdf(p));
    if phi > 0.0 {
        (p * (1.0 - p)).sqrt() / phi
    } else {
        1.0
    }
}
