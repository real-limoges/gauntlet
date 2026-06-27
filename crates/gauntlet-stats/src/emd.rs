//! Earth Mover's Distance (1-Wasserstein), ported from `earthMoversDistance`
//! in `Stats/Benchmark.hs`.

/// Earth Mover's Distance between two sample sets.
///
/// For equal sample sizes: `mean(|sorted_a − sorted_b|)`.
/// For unequal sizes: CDF-based integration ([`compute_emd_general`]).
///
/// (The empty-input case returns 0 here, where the Haskell equal-size path would
/// divide by zero and yield NaN — a deliberate hardening, not a behavior the
/// callers can reach with real benchmark data.)
pub fn earth_movers_distance(a: &[f64], b: &[f64]) -> f64 {
    let mut sa = a.to_vec();
    let mut sb = b.to_vec();
    sa.sort_by(f64::total_cmp);
    sb.sort_by(f64::total_cmp);

    if sa.len() == sb.len() {
        if sa.is_empty() {
            return 0.0;
        }
        let sum: f64 = sa.iter().zip(&sb).map(|(x, y)| (x - y).abs()).sum();
        sum / sa.len() as f64
    } else {
        compute_emd_general(&sa, &sb)
    }
}

/// General EMD for unequal sample sizes, via CDF integration. Inputs are assumed
/// sorted ascending.
fn compute_emd_general(sorted_a: &[f64], sorted_b: &[f64]) -> f64 {
    if sorted_a.is_empty() || sorted_b.is_empty() {
        return 0.0;
    }
    let n_a = sorted_a.len() as f64;
    let n_b = sorted_b.len() as f64;

    // Merge into a single ascending event stream of (value, dCdfA, dCdfB).
    let mut events: Vec<(f64, f64, f64)> = Vec::with_capacity(sorted_a.len() + sorted_b.len());
    let (mut i, mut j) = (0usize, 0usize);
    while i < sorted_a.len() && j < sorted_b.len() {
        if sorted_a[i] <= sorted_b[j] {
            events.push((sorted_a[i], 1.0 / n_a, 0.0));
            i += 1;
        } else {
            events.push((sorted_b[j], 0.0, 1.0 / n_b));
            j += 1;
        }
    }
    while i < sorted_a.len() {
        events.push((sorted_a[i], 1.0 / n_a, 0.0));
        i += 1;
    }
    while j < sorted_b.len() {
        events.push((sorted_b[j], 0.0, 1.0 / n_b));
        j += 1;
    }

    // Walk the stream, accumulating the area between the two CDFs. At each event
    // the deltas are applied *before* measuring the width to the next event,
    // matching the Haskell `integrate`.
    let mut cdf_a = 0.0;
    let mut cdf_b = 0.0;
    let mut area = 0.0;
    for k in 0..events.len().saturating_sub(1) {
        let (x, d_a, d_b) = events[k];
        let next_x = events[k + 1].0;
        cdf_a += d_a;
        cdf_b += d_b;
        area += (next_x - x) * (cdf_a - cdf_b).abs();
    }
    area
}
