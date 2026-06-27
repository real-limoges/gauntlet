# ADR M0 — Foundation decisions

Status: **accepted** · Milestone: M0 · See `docs/RUST_PORT.md`

Records the cross-cutting decisions that the rest of the port depends on. Each
can be revisited, but only deliberately — downstream milestones assume these.

## Decisions

1. **In-place coexistence.** The Cargo workspace lives at the repo root alongside
   the Haskell tree. The two build systems share no files (`Cargo.toml`/`crates/`
   vs. `gauntlet.cabal`/`src`,`app`,`test`) and separate artifact dirs
   (`target/` vs. `dist-newstyle/`). The Haskell binary stays the **parity
   oracle** until the M6 cutover. Shared, language-neutral assets (`examples/`,
   `schema/`, `docs/`) are consumed by both.

2. **Greenfield workspace, not built on `rlt`/`goose`.** Provisional. The 1-day
   `rlt` foundation spike is deferred to the start of **M3** (the engine), where
   its value (worker pool + TUI) actually lands — standing up the skeleton does
   not depend on it. Kill criterion for adopting `rlt` later: it must expose
   *raw per-request latency samples*, not just aggregated metrics.

3. **Workspace crate boundaries** as drawn in `RUST_PORT.md` §3:
   `core, stats, engine, report, tui, tracing, cli`. `gauntlet-stats` stays pure
   and dependency-light (currently `libm` only) and does **not** depend on
   `gauntlet-core`.

4. **Edition 2021.** Conservative MSRV choice; revisit if a 2024-only feature
   earns its keep.

5. **Tempo vs. `tracing` naming.** The `gauntlet-tracing` crate is *Grafana
   Tempo* trace querying. If/when we adopt the `tracing` logging crate for
   structured logs, the Tempo crate is renamed `gauntlet-tempo` to remove the
   collision. Until then, logging is a small hand-rolled leveled logger in
   `gauntlet-core` (port of `Log.hs`).

6. **Lint policy.** `cargo fmt --check` + `cargo clippy -D warnings` enforced in
   CI (`ci-rust.yml`), mirroring the role `fourmolu` + `-Wall` play on the
   Haskell side.

7. **`gauntlet-stats` is opt-level 2 in dev builds** (workspace profile
   override) so local measurement math is representative without a release build.

## Deferred to later milestones

- `reqwest` vs `hyper` client overhead benchmark → **M3**.
- `hdrhistogram`/`average` as display-only estimators (exact samples drive the
  statistics) → **M1** (resolved: not adopted; exact `Vec<f64>` used. See
  `M1-no-oracle.md`, which also records that the parity-as-oracle plan in this ADR
  is dropped in favor of a clean break — Rust-native correctness tests only).
- `schemars`-derived schema vs. the committed `config-schema.json` → **M2**.
- Baseline file cross-compatibility with Haskell-written baselines → **M4**.
