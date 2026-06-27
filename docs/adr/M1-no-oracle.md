# ADR M1 — Stats core ported; parity is a total break (no Haskell oracle)

Status: **accepted** · Milestone: M1 · See `docs/RUST_PORT.md`, supersedes the parity-as-oracle plan in ADR M0.

## Context

M1 ports the remaining pure statistics from `Stats/Benchmark.hs` and `Stats/Common.hs`
into `gauntlet-stats` (`compare_bayesian` and `earth_movers_distance` were already done in
M0). The M0 plan called for a golden-vector parity harness that diffs Rust output against
the Haskell binary to `1e-9`.

## Decision

The owner chose a **total break** from Haskell. There is **no golden oracle, no
`tools/Oracle.hs`, no fixtures generated from the Haskell binary, and no modification to the
Haskell tree.** Correctness is established with Rust-native tests only.

Consequences:

1. **No bit-parity requirement.** We target *correctness*, not bit-identical agreement with
   the `statistics` package. `mean`/`variance`/`std_dev` use the straightforward unbiased
   (n−1) two-pass formulas rather than reproducing the package's compensated summation.
2. **Tests are closed-form / textbook anchors** in `crates/gauntlet-stats/tests/`:
   - `parity.rs` (M0): textbook Φ, closed-form EMD, exact Cohen's d / credible intervals,
     swap-antisymmetry, unit-interval bounds, edge guards.
   - `descriptive.rs` (M1): R-7 percentile known values, exact mean/variance, expected-
     shortfall closed form, histogram invariants (first bin = min, Σcounts = n, bin count in
     `[8,20]`), and the `success==0` / single-element / empty guards.
3. **Faithful where it's cheap.** Every Haskell guard is reproduced exactly, and the
   hand-rolled A&S `inverse_normal_cdf` approximation is kept (not swapped for a higher-
   accuracy `statrs` inverse), so behavior matches the original intent even without a diff.

## Implementation notes

- `calculate_stats(total_requests, durations_ms: &[f64])` is pure and decoupled from
  `TestingResponse`. The ns→ms conversion and error-response filtering (`getDuration`,
  `extractDurations`) depend on that type and are deferred to **M2/`gauntlet-core`**.
- One observed floating-point subtlety, shared with the Haskell: `(1.0 − 0.99)·n` carries the
  classic `1.0 − 0.99 = 0.010000000000000009` artifact, so expected-shortfall `tail_size`
  near an integer boundary rounds up one extra. Tests pick `n` away from those boundaries; the
  behavior itself is unchanged (and would match Haskell, which does the same reduction).

## Status

Done. `gauntlet-stats` is feature-complete: 25 tests pass; `cargo fmt --check` and
`cargo clippy -D warnings` clean.
