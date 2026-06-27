# ADR M3-engine — `gauntlet-engine`: the async measurement loop

Status: **accepted** · Milestone: M3 · See `docs/RUST_PORT.md` §M3 and ADR
`M3-A-client` (the reqwest decision). Continues the clean break: the Haskell
`Runner/*`, `Network/Exec`, `Execution/*` are the *behavioral* reference, not a
shape to transcribe.

## Context

M3 builds the layer that fires real HTTP requests and returns the
`TestingResponse` sample vectors `gauntlet_stats::calculate_stats` consumes, plus
CSV output, response validation, load-mode pacing, retry, warmup, and lifecycle
hooks. The exit criterion (per `RUST_PORT.md`) is an end-to-end localhost run whose
samples feed the stats core.

## Decisions

1. **reqwest, decided by measurement.** ADR `M3-A-client` spiked reqwest vs hyper
   against an in-process mock; reqwest's overhead is small (~8µs sequential) and
   *stable* (~5µs stddev) and indistinguishable under concurrency, so reqwest wins
   on ergonomics. `client.rs` is the single module that touches the client; hyper
   stays a dev-dependency (the spike example only).

2. **Monotonic measurement clock.** Latency is `Instant::elapsed()` spanning all
   retry attempts — an intentional improvement over the Haskell `Realtime` clock
   (monotonic is immune to wall-clock steps). A wall-clock `SystemTime` is kept
   only for the CSV `timestamp_iso`.

3. **One custom rate limiter for every mode.** Rather than `governor`
   (constant-rate only), a single atomic next-slot reservation (`tokio::Mutex<f64>`,
   the Haskell `MVar` analog) covers constant/Poisson/ramp/step, recomputing the
   interval at each claim. It **composes with the concurrency `Semaphore` without
   double-counting**: the limiter paces request *starts*, the semaphore caps
   *in-flight* requests. Time-varying modes floor at 6 RPM, matching the Haskell.

4. **Two loop shapes by load mode.** Fixed-count modes
   (`Unthrottled`/`ConstantRpm`/`PoissonRpm`) fire `total_requests` via a `JoinSet`
   gated by an acquire-before-spawn `Semaphore`. Duration-based modes
   (`RampUp`/`StepLoad`) spawn `concurrency` workers that loop until a shared
   deadline off one shared limiter.

5. **Retry is hand-rolled (not `backon`).** `with_retry` retries **only** transport
   errors (connection refused / timeout — `reqwest::Error::is_connect`/`is_timeout`)
   while retries remain; any HTTP status (incl. 5xx) is a *response*, never retried.
   `max_attempts` is the retry count (0 disables); the delay grows
   `ceil(delay × multiplier)`. Exhaustion yields an errored `TestingResponse`
   (`status: 0`), which `extract_durations` already drops from latency math. The
   generic shape makes the loop unit-testable without sockets.

6. **Validation results live in `gauntlet-core`, logic in the engine.**
   `ValidationError`/`ValidationSummary` (deferred from M2) are core *vocabulary* so
   M4 reporters can read them without depending on the engine. `validation.rs` does
   the dot-path resolution (`$`/`$.` stripped, numeric segments index arrays) + the
   `FieldAssertion` checks; the endpoint loop aggregates them, collecting errors
   from at most the first 50 failing responses.

7. **Lifecycle via `tokio::process`.** Setup/teardown run through `sh -c` (honoring
   `working_dir`/`timeout_secs`); the health check polls until 200 or timeout.
   Teardown is best-effort (a teardown failure warns, doesn't fail the run); a
   failed setup aborts before any request.

8. **CSV unchanged in shape.** The 7-column format
   (`target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso`) is
   preserved so the existing plot script / reporters stay compatible;
   `latency_ms = duration_ns / 1e6`.

## Consequences / scope boundaries

- The public surface is `run_benchmark(config, csv_path) -> BenchmarkRun`
  (`TargetResult` → `EndpointResult` with samples, `ValidationSummary`, and
  `BenchmarkStats`). Bayesian/EMD post-analysis, baselines, and the reporter
  fan-out are **M4**; the live TUI event stream is **M5** (a thin seam only here).
  Bearer-token auth uses per-payload headers for now; token injection lands with
  the CLI/secrets work in M6.
- **Known bias (from M3-A):** absolute latencies include the client's ~40µs floor.
  Fine for A/B comparison and regression deltas (the floor cancels); a caveat for
  absolute single-number claims.

## Status

Done. 24 `gauntlet-engine` tests (request counts, warmup discard, concurrency
ceiling, retry exhaustion + 500-not-retried + retry-then-succeed, constant-RPM
pacing + duration-bounded runs, validation pass/fail/50-cap, lifecycle
setup+health+teardown and setup-abort, and the end-to-end stats+CSV proof) on top
of 20 core + 25 stats — 69 total green; `cargo fmt --check` and
`cargo clippy --all-targets -D warnings` clean; Haskell tree untouched.
