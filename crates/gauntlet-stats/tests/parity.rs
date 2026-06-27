//! Parity / correctness anchors for `gauntlet-stats`.
//!
//! These anchor on values that are independent of this implementation —
//! textbook standard-normal-CDF values, the closed form of equal-size EMD, and
//! exact hand-computable arithmetic (Cohen's d, credible intervals) — plus
//! structural properties (antisymmetry, unit-interval bounds, edge guards).
//!
//! The full *binary-to-binary* golden-vector harness (run identical inputs
//! through the Haskell binary, diff to 1e-9) is milestone **M1**'s parity gate;
//! see `docs/RUST_PORT.md`. These anchors are what we lock in first.

use gauntlet_stats::{
    compare_bayesian, earth_movers_distance, standard_normal_cdf, BenchmarkStats,
};

const EPS: f64 = 1e-9;

fn close(a: f64, b: f64) -> bool {
    (a - b).abs() < EPS
}

/// Build a `BenchmarkStats` with only the fields the Bayesian path reads.
fn stats(mean: f64, sd: f64, n: usize, p95: f64, p99: f64) -> BenchmarkStats {
    BenchmarkStats {
        mean_ms: mean,
        std_dev_ms: sd,
        count_success: n,
        p95_ms: p95,
        p99_ms: p99,
        ..Default::default()
    }
}

// ---- standard normal CDF: textbook reference values --------------------------

#[test]
fn cdf_reference_values() {
    assert!(close(standard_normal_cdf(0.0), 0.5));
    assert!(close(standard_normal_cdf(1.0), 0.841_344_746_068_542_9));
    assert!(close(standard_normal_cdf(-1.0), 0.158_655_253_931_457_07));
    assert!(close(standard_normal_cdf(1.96), 0.975_002_104_851_779_5));
    assert!(close(standard_normal_cdf(2.0), 0.977_249_868_051_820_8));
}

#[test]
fn cdf_symmetry() {
    for x in [0.3_f64, 1.0, 2.5, -0.7] {
        assert!(close(standard_normal_cdf(x) + standard_normal_cdf(-x), 1.0));
    }
}

// ---- Earth Mover's Distance: closed forms ------------------------------------

#[test]
fn emd_equal_size_closed_form() {
    // sorted, equal size: mean(|1-2|, |2-4|, |3-6|) = (1+2+3)/3 = 2.0
    assert!(close(
        earth_movers_distance(&[1.0, 2.0, 3.0], &[2.0, 4.0, 6.0]),
        2.0
    ));
}

#[test]
fn emd_sorts_inputs_first() {
    // same multiset as above, shuffled — must give the same answer.
    assert!(close(
        earth_movers_distance(&[3.0, 1.0, 2.0], &[6.0, 2.0, 4.0]),
        2.0
    ));
}

#[test]
fn emd_identical_is_zero() {
    assert!(close(
        earth_movers_distance(&[5.0, 1.0, 3.0, 2.0], &[5.0, 1.0, 3.0, 2.0]),
        0.0
    ));
}

#[test]
fn emd_empty_is_zero() {
    let empty: [f64; 0] = [];
    assert!(close(earth_movers_distance(&empty, &empty), 0.0));
}

#[test]
fn emd_unequal_size_translation() {
    // B is A shifted by +10; mass moves a constant distance of 10.
    let a = [0.0, 1.0, 2.0, 3.0];
    let b = [10.0, 11.0, 12.0];
    assert!(earth_movers_distance(&a, &b) > 0.0);
    // identical-shape unequal sizes still finite & symmetric in magnitude
    assert!(close(
        earth_movers_distance(&a, &b),
        earth_movers_distance(&b, &a)
    ));
}

// ---- Bayesian comparison: exact arithmetic anchors ---------------------------

#[test]
fn bayesian_identical_inputs_are_neutral() {
    let s = stats(10.0, 2.0, 100, 14.0, 16.0);
    let c = compare_bayesian(&s, &s);
    assert!(close(c.prob_b_faster_than_a, 0.5));
    assert!(close(c.prob_single_request_faster, 0.5));
    assert!(close(c.prob_b_less_jittery, 0.5));
    assert!(close(c.mean_difference, 0.0));
    assert!(close(c.effect_size, 0.0));
    assert!(close(c.relative_effect, 0.0));
    assert!(close(c.p95_comparison.prob_pct_regression, 0.5));
}

#[test]
fn bayesian_exact_hand_computed() {
    // muA=12, muB=10, sdA=sdB=4, n=50 — every quantity below is exact.
    let a = stats(12.0, 4.0, 50, 0.0, 0.0);
    let b = stats(10.0, 4.0, 50, 0.0, 0.0);
    let c = compare_bayesian(&a, &b);

    assert!(close(c.mean_difference, 2.0));
    // pooledVar = (49·16 + 49·16) / 98 = 16; pooledSd = 4; d = 2/4 = 0.5
    assert!(close(c.effect_size, 0.5));
    assert!(close(c.relative_effect, 2.0 / 12.0 * 100.0));

    // CI = muDiff ± 1.96·√(16/50 + 16/50)
    let se = (16.0_f64 / 50.0 + 16.0 / 50.0).sqrt();
    assert!(close(c.credible_interval_lower, 2.0 - 1.96 * se));
    assert!(close(c.credible_interval_upper, 2.0 + 1.96 * se));

    // P(B faster) = Φ(muDiff / sigmaDiff); sigmaDiff equals `se` here.
    assert!(close(c.prob_b_faster_than_a, standard_normal_cdf(2.0 / se)));
    assert!(c.prob_b_faster_than_a > 0.99 && c.prob_b_faster_than_a < 1.0);
}

#[test]
fn bayesian_antisymmetry_under_swap() {
    let a = stats(11.0, 3.0, 80, 15.0, 18.0);
    let b = stats(9.0, 2.5, 120, 13.0, 16.0);
    let ab = compare_bayesian(&a, &b);
    let ba = compare_bayesian(&b, &a);

    assert!(close(
        ab.prob_b_faster_than_a + ba.prob_b_faster_than_a,
        1.0
    ));
    assert!(close(
        ab.prob_single_request_faster + ba.prob_single_request_faster,
        1.0
    ));
    assert!(close(ab.prob_b_less_jittery + ba.prob_b_less_jittery, 1.0));
    assert!(close(ab.mean_difference, -ba.mean_difference));
    assert!(close(ab.effect_size, -ba.effect_size));
}

#[test]
fn bayesian_zero_success_guards() {
    let a = stats(10.0, 2.0, 0, 0.0, 0.0); // empty sample
    let b = stats(9.0, 2.0, 100, 0.0, 0.0);
    let c = compare_bayesian(&a, &b);
    assert!(close(c.prob_b_faster_than_a, 0.5));
    // credible interval collapses to the point estimate
    assert!(close(c.credible_interval_lower, c.mean_difference));
    assert!(close(c.credible_interval_upper, c.mean_difference));
    assert!(close(c.p95_comparison.prob_pct_regression, 0.5));
}

#[test]
fn bayesian_probabilities_in_unit_interval() {
    let a = stats(10.0, 2.0, 100, 14.0, 16.0);
    let b = stats(10.5, 2.2, 100, 15.0, 17.0);
    let c = compare_bayesian(&a, &b);
    for p in [
        c.prob_b_faster_than_a,
        c.prob_single_request_faster,
        c.prob_b_less_jittery,
        c.p95_comparison.prob_pct_regression,
        c.p99_comparison.prob_pct_regression,
    ] {
        assert!((0.0..=1.0).contains(&p), "probability out of range: {p}");
    }
}
