//! Normal-distribution helpers, ported from `Stats/Benchmark.hs`.

use std::f64::consts::{PI, SQRT_2};

/// Z-score for the 95% credible interval (two-tailed).
pub const Z95: f64 = 1.96;

/// Standard normal CDF: Φ(x) = ½·erfc(−x/√2).
///
/// Uses `libm::erfc`, matching the Haskell `Numeric.SpecFunctions.erfc`.
pub fn standard_normal_cdf(x: f64) -> f64 {
    0.5 * libm::erfc(-(x / SQRT_2))
}

/// Rational approximation of the inverse normal CDF.
///
/// Abramowitz & Stegun, *Handbook of Mathematical Functions* (1964), 26.2.23.
///
/// IMPORTANT: this is an **approximation** (≈4.5e-4 max error), not the exact
/// inverse. The Haskell `inverseNormalCDF` uses exactly this formula, so parity
/// means reproducing *this*, not a higher-accuracy inverse from `statrs`.
pub fn inverse_normal_cdf(p: f64) -> f64 {
    if p <= 0.0 {
        -10.0
    } else if p >= 1.0 {
        10.0
    } else if p < 0.5 {
        -approx_inv_norm(1.0 - p)
    } else {
        approx_inv_norm(p)
    }
}

fn approx_inv_norm(q: f64) -> f64 {
    let t = (-(2.0 * (1.0 - q).ln())).sqrt();
    t - (2.515_517 + 0.802_853 * t + 0.010_328 * t * t)
        / (1.0 + 1.432_788 * t + 0.189_269 * t * t + 0.001_308 * t * t * t)
}

/// Standard normal PDF φ(z) — used by the Maritz-Jarrett percentile SE.
pub(crate) fn standard_normal_pdf(z: f64) -> f64 {
    (-(0.5 * z * z)).exp() / (2.0 * PI).sqrt()
}
