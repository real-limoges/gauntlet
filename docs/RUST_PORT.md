# Porting gauntlet to Rust — a plan to make the plan

> Status: **pre-decision design doc.** This document does not commit to an implementation. It scopes the work, names the off-the-shelf components that change the build-vs-buy calculus, and breaks the migration into milestones — each of which produces its own focused plan (a "spike") before any production code is written. Read it top to bottom once, then use the milestone sections as the entry points for the real planning passes.

> **⚠️ Superseded decision (total break).** The owner has chosen a **clean break** from Haskell: there is **no golden-vector parity harness and no Haskell oracle** at any milestone, and the Haskell tree is not modified by the port. Every "Parity gate" below should be read as **"Rust-native correctness tests"** — closed-form/textbook anchors and self-consistency properties (as in `crates/gauntlet-stats/tests/`), not diffs against the Haskell binary. §7 (parity-as-oracle) is retained only as a record of the path not taken. This also softens the "silent float drift" risk in §6: we target *correctness*, not bit-identical agreement with the Haskell `statistics` package.

## 0. Why this document exists

The Haskell implementation works. The motivation to port is **iteration speed** (GHC `-O2` compile times) and **toolchain consolidation** (to Rust / Elixir / Common Lisp). Of those three, Rust is the right target for *this specific tool* for one principled reason and several practical ones:

- **Principled:** gauntlet is a *latency measurement* tool. It reports P99, expected shortfall, and "less jittery" probabilities — it sells precision. A garbage-collected runtime injects pauses into the measurement loop, adding noise to the very quantity being measured. The current code already fights this with `StrictData` and `Data.Vector.Unboxed`. Rust has no GC; that whole workaround layer disappears and the measurement floor gets quieter. Elixir (BEAM) and Common Lisp (SBCL) both keep a GC.
- **Practical:** the type modeling (ADTs, unit newtypes, exhaustive matching, the record-of-functions `Reporter`) transfers to Rust enums/traits at roughly 1:1. Elixir and CL discard most of it. And Rust's HTTP-load-testing ecosystem is mature enough that large parts of this are **buy, not build** (see §2).

This is not a rewrite-from-scratch argument. It is a *transcription with upgrades* argument: most modules have a near-mechanical Rust equivalent, and a handful of subsystems can be deleted entirely in favor of a crate.

## 1. How to use this plan

The migration is organized as **seven milestones (M0–M6)**. Each milestone section contains:

- **Goal** — what exists at the end.
- **Spike questions** — the decisions that must be resolved *before* writing the milestone's production code. The spike's output is a short ADR (architecture decision record) committed under `docs/adr/`.
- **Off-the-shelf gate** — the specific crates to evaluate, and the kill criteria for "adopt vs build."
- **Parity gate** — how we prove the Rust output matches the Haskell output before moving on.

"A plan to make plans" means: this doc gets you to the point where each milestone can be planned in isolation with full information. Do not skip the spikes — the expensive mistakes in a port like this are made by porting an abstraction faithfully when an off-the-shelf crate would have replaced it.

## 2. The off-the-shelf assessment (read this first)

This is where the real leverage is. Before mapping modules, decide how much of the tool you are even building.

### 2.1 The biggest decision: framework vs. library vs. greenfield

There are three Rust HTTP load-testing projects worth evaluating as *foundations* rather than references:

| Project | What it is | Fit for gauntlet | Verdict |
|---|---|---|---|
| **`rlt`** ([wfxr/rlt](https://github.com/wfxr/rlt)) | A *library* ("rust load testing toolkit") that gives you the concurrency harness, a built-in ratatui TUI, and live stats collection. You supply the per-iteration request logic. | High — it owns exactly the parts that are tedious to rebuild (worker pool, TUI scaffolding, progress) while leaving the request shape and custom stats to you. | **Evaluate as the M3/M5 foundation.** Kill criterion: does it expose *raw per-request latency samples*? gauntlet's Bayesian core needs the full sample vector, not pre-aggregated metrics. |
| **`goose`** ([tag1consulting/goose](https://github.com/tag1consulting/goose)) | A full Locust-style load-testing *framework* with scenarios, transactions, and its own aggregated metrics/reporting. | Medium — powerful, but opinionated around scenario scripting and aggregate metrics. Its reporting model competes with gauntlet's, and getting raw samples out for EMD/Bayesian analysis may fight the grain. | **Reference, probably not foundation.** Reassess only if M3 spike finds raw-sample extraction is clean. |
| **`oha`** ([hatoo/oha](https://github.com/hatoo/oha)) | A polished CLI HTTP load tester with a ratatui TUI. An *application*, not a library. | Reference design. Its TUI and per-request timing collection are worth studying closely; the histogram/latency-distribution presentation overlaps heavily with gauntlet's TUI widgets. | **Reference for M5 TUI.** |

**The first spike (M0) must resolve this**: greenfield workspace vs. building on `rlt`. Everything downstream depends on it. The recommendation going in is *greenfield workspace, but timebox a 1-day `rlt` spike first* — because if `rlt` gives you the worker pool + TUI for free and lets you bring your own stats, it collapses M3 and M5 substantially.

### 2.2 Subsystem-level buy-vs-build

Even greenfield, these crates replace whole modules — several of them *upgrade* the current behavior:

| Current module / concern | Off-the-shelf crate | Notes — and where it's a strict upgrade |
|---|---|---|
| `Stats/Benchmark.hs` percentiles + histogram | **`hdrhistogram`** | The canonical latency histogram (HdrHistogram). Gives P50/P95/P99/max with bounded error and tiny memory, *designed for exactly this*. **Upgrade**: replaces hand-rolled `computeHistogram` + `percentileSorted` and scales to huge sample counts without holding every sample. ⚠️ But you still keep raw samples for EMD and Bayesian SEs — see §6 risk. |
| streaming mean/variance/quantiles | **`average`** | `Mean`, `Variance`, `Quantile`, `Histogram` estimators. Useful for the live TUI rolling stats where you don't want to re-sort each frame. |
| `erfc`, normal CDF, inverse CDF | **`statrs`** (or **`libm`**) | `statrs::function::erf::erfc`, `Normal::cdf`/`inverse_cdf`. Replaces `standardNormalCDF`; you *may* keep the hand-rolled `inverseNormalCDF` (Abramowitz-Stegun) to avoid the dep — decide in M1. |
| `scripts/plot_latency.py` + `uv` subprocess + `plotReporter` | **`plotters`** (SVG backend) | **Major upgrade.** Renders charts natively to SVG/PNG in-process. **Deletes the entire Python dependency, the `uv` subprocess shell-out, and `scripts/plot_latency.py`.** The HTML reporter embeds plotters SVGs directly. |
| `Benchmark/Reporter/JUnit.hs` | **`junit-report`** | Generates JUnit XML from a typed model. Replaces hand-built XML string assembly. |
| `Benchmark/Reporter/Prometheus.hs` | **`prometheus`** / **`prometheus-client`** | Format the exposition text from typed metrics; the pushgateway push is a plain HTTP `PUT`. |
| `Benchmark/Execution/RateLimiter.hs` (`ConstantRpm`) | **`governor`** | GCRA rate limiter, the standard choice. Covers `ConstantRpm` directly. ⚠️ `Poisson`/`RampUp`/`StepLoad` still need custom interarrival logic — governor is constant-rate. |
| retry/backoff in `Network/Exec.hs` | **`backon`** (or `tokio-retry`) | Declarative retry with backoff policies. Replaces the hand-rolled retry/backoff. |
| `Benchmark/Config/Loader.hs` + `Env.hs` | **`serde`** + **`figment`** (or `config`) + **`dotenvy`** | Layered config (file < env), `.env.local` > `.env` precedence. `${VAR}` interpolation via **`subst`** or `shellexpand`. |
| embedded `schema/config-schema.json` (`file-embed`) | **`schemars`** | **Upgrade.** *Derive* the JSON schema from the config structs. The `schema` subcommand prints the derived schema; it can never drift from the types again. |
| `Benchmark/TUI` (Brick/vty) | **`ratatui`** + **`crossterm`** | Direct Brick analog. Built-in `Chart`, `Sparkline`, `BarChart`, `Gauge` widgets map onto the histogram/timeline/rolling-stats widgets. |
| HTTP client (`http-client` + `-tls`) | **`reqwest`** (rustls) *or* **`hyper`** | See §2.3 — this is a real decision, not a default. |
| `Tracing/Client.hs` (Tempo query) | `reqwest` + `serde` | Tempo's query API is plain HTTP/JSON. No special crate; possibly `opentelemetry-proto` types if you parse OTLP responses. |
| CSV latency dump (`Report/Output.hs`) | **`csv`** | Typed CSV writer. |
| `Config/CLI.hs` (`optparse-applicative`) | **`clap`** (derive) | Subcommand enum → derive. |
| `Types/Error.hs` | **`thiserror`** (lib) + **`anyhow`** (bin edge) | Error enums with derived `Display`. |
| test `MockServer.hs` (Warp) | **`wiremock`** or **`axum`** | Local mock HTTP server for engine tests. |
| `Log.hs` | **`tracing`** + `tracing-subscriber` | ⚠️ Naming collision with the Tempo `Tracing/` module — see §4 note. |

### 2.3 The one client decision that matters: `reqwest` vs `hyper`

`reqwest` is ergonomic and the default choice. But it layers conveniences (redirect handling, cookie store, body buffering) over `hyper`, and **for a measurement tool, client-side overhead is measurement error.** `hyper` (or `hyper-util`'s pooled client) gives lower, more predictable per-request overhead and exact control over connection pooling and keep-alive — at the cost of writing more plumbing.

**M3 spike must benchmark both** against a localhost mock and compare the client-side overhead floor (request issued → bytes on wire) and its variance. If reqwest's overhead is small and *stable* relative to your target latencies, take the ergonomics. If it adds jitter, drop to hyper. Do not default this.

## 3. Target workspace architecture

A Cargo **workspace**, not a single crate — this is the direct answer to the compile-time complaint. Cargo compiles crates in parallel and only recompiles the ones that change. Editing a TUI widget must not recompile the numerics.

```
gauntlet/
├── Cargo.toml                      # [workspace] members
├── crates/
│   ├── gauntlet-core/              # types, units, config, env, errors — the shared vocabulary
│   ├── gauntlet-stats/             # PURE numerics. Ideally zero deps beyond statrs/hdrhistogram.
│   ├── gauntlet-engine/            # async execution: loop, net, rate limiting, validation, warmup
│   ├── gauntlet-report/            # reporters + baselines (markdown/junit/prom/html/ci)
│   ├── gauntlet-tui/               # ratatui UI + event state machine
│   ├── gauntlet-tracing/           # Tempo client + trace analysis
│   └── gauntlet-cli/               # the binary: clap, dispatch, main
└── docs/
    ├── RUST_PORT.md                # this file
    └── adr/                        # one ADR per spike (M0-A, M1-A, ...)
```

Dependency graph (acyclic; leaf crates build first, in parallel):

```
                    gauntlet-core
              ┌──────────┼──────────┬───────────┐
        gauntlet-stats  engine    report     tracing
              └──────────┴────┬─────┴───────────┘
                         gauntlet-cli  ── also depends on gauntlet-tui
```

**Design rule:** `gauntlet-stats` must stay pure and dependency-light. It is the most-tested, most-correctness-critical, and fastest-to-iterate crate. If a function in it needs `tokio` or `reqwest`, it belongs in `engine`, not `stats`.

## 4. Module-by-module mapping

| Haskell (`src/`) | Rust home | Off-the-shelf leverage |
|---|---|---|
| `app/Main.hs` (C `exit()` hack) | `gauntlet-cli/src/main.rs` | `std::process::exit(code)` *is* the idiomatic skip-teardown exit. The FFI hack vanishes. |
| `Lib.hs` (dispatch, reporter assembly) | `gauntlet-cli/src/{lib,dispatch}.rs` | — |
| `Benchmark/Config/CLI.hs` | `gauntlet-cli/src/args.rs` | `clap` derive |
| `Benchmark/Types*.hs` (+ `Types/`) | `gauntlet-core::{types,units,config,error}` | `thiserror`, `schemars` derive on config |
| `Benchmark/Config/{Loader,Env}.hs` | `gauntlet-core::{config,env}` | `serde`, `figment`/`config`, `dotenvy`, `subst` |
| `Stats/Common.hs` | `gauntlet-stats::common` | `statrs`/`libm` |
| `Stats/Benchmark.hs` | `gauntlet-stats::{descriptive,bayesian,emd,histogram}` | `hdrhistogram`, `average`, `statrs` |
| `Runner/{Context,Loop,Benchmark,Warmup}.hs` | `gauntlet-engine::{context,run_loop,orchestrate,warmup}` | `tokio` (Semaphore, JoinSet), optionally `rlt` |
| `Benchmark/Network/{Exec,Request,Auth}.hs` | `gauntlet-engine::net::{exec,request,auth}` | `reqwest`/`hyper`, `backon` |
| `Benchmark/Execution/{RateLimiter,Validation,Environment}.hs` | `gauntlet-engine::{limiter,validate,lifecycle}` | `governor` (+ custom interarrival), `jsonpath-rust` for `$.path` validation |
| `Benchmark/Report.hs` + `Report/{Markdown,CI,Formatting,Output}.hs` | `gauntlet-report::{render,markdown,ci,format,csv}` | `csv` |
| `Benchmark/Report/Baseline.hs` | `gauntlet-report::baseline` | `serde_json` to disk |
| `Benchmark/Reporter.hs` (record-of-fns) | `gauntlet-report::reporter` (trait) | `async-trait` |
| `Benchmark/Reporter/HTML.hs` + `plotReporter` + `scripts/plot_latency.py` | `gauntlet-report::html` | **`plotters`** — deletes Python + `uv` |
| `Benchmark/Reporter/JUnit.hs` | `gauntlet-report::junit` | `junit-report` |
| `Benchmark/Reporter/Prometheus.hs` | `gauntlet-report::prometheus` | `prometheus`/`prometheus-client` |
| `Benchmark/TUI*.hs` (Brick/vty + STM `TBQueue`) | `gauntlet-tui::{app,state,widgets}` | `ratatui`, `crossterm`, `tokio::sync::mpsc` |
| `Tracing/*.hs` + `Runner/Tracing.hs` | `gauntlet-tracing::{client,report,types}` | `reqwest` + `serde` against Tempo API |
| `Log.hs` | `gauntlet-core::log` (or `tracing` crate) | see note below |

> **Naming landmine.** The `tracing` *crate* (Rust's logging/diagnostics framework) is unrelated to gauntlet's `Tracing/` *module* (Grafana Tempo distributed-trace querying). Pick one of: (a) keep `Log.hs` as a tiny hand-rolled leveled logger in `gauntlet-core::log` and reserve the name "tracing" for Tempo; or (b) use the `tracing` crate for logs and rename the Tempo crate to `gauntlet-tempo`. Decide in M0; (b) is cleaner long-term.

## 5. The migration milestones

Ordered to **front-load correctness risk and de-risk the float math first**, because a silent floating-point port bug in the stats core is the most expensive failure mode (it produces plausible, wrong probabilities). Each milestone is independently shippable and independently planned.

### M0 — Foundation & decisions (the meta-spike)
- **Goal:** an empty but compiling workspace; all cross-cutting decisions recorded as ADRs.
- **Spike questions:**
  - Greenfield vs. build on `rlt`? (§2.1) — **timeboxed 1-day spike, highest-leverage decision.**
  - `tracing`-crate-for-logs vs. reserve the name for Tempo? (§4 note)
  - Workspace crate boundaries as drawn in §3 — confirm or adjust.
  - MSRV, edition (2021/2024), lint policy (`#![deny(warnings)]`? clippy pedantic?), `rustfmt` config to mirror `fourmolu`'s role.
- **Off-the-shelf gate:** the `rlt` evaluation. Kill criterion: raw per-request sample access.
- **Exit:** `cargo build` succeeds on the skeleton; ADRs M0-A (foundation) committed.

### M1 — `gauntlet-stats` (pure core, do this first) — ✅ DONE
- **Goal:** every function in `Stats/{Common,Benchmark}.hs` ported, with Rust-native correctness tests.
- **Resolved decisions (this was implemented):**
  - Kept the hand-rolled `inverseNormalCDF` (the A&S approximation) — parity with the existing approximation, not a higher-accuracy `statrs` inverse.
  - `libm` for `erfc`; no `statrs`/`hdrhistogram` dependency. Exact `Vec<f64>`/sorted-slice percentiles drive the statistics. (`hdrhistogram`/`average` as display-only estimators are deferred to the TUI in M5, if wanted.)
  - `f64` throughout; every Haskell guard reproduced (`success == 0`, single-element, empty histogram, n−1 stddev).
  - `calculate_stats(total_requests, durations_ms: &[f64])` stays pure; the `TestingResponse` extraction (`getDuration`: ns→ms + drop failures) is deferred to M2/`gauntlet-core`.
- **Test gate (replaces the golden-vector harness):** Rust-native anchors in `crates/gauntlet-stats/tests/{parity,descriptive}.rs` — textbook Φ values, closed-form EMD/percentiles/expected-shortfall, exact hand-computed Cohen's d / credible intervals, and structural properties (swap-antisymmetry, unit-interval bounds, edge guards). No Haskell oracle.
- **Exit:** `gauntlet-stats` is feature-complete; 25 tests pass; fmt + clippy clean. See `docs/adr/M1-no-oracle.md`.

### M2 — `gauntlet-core` (types, config, env) — ✅ DONE
- **Goal:** config files load + validate; the type vocabulary exists. The Haskell
  types are the *reference*, **not** the contract — this is a Rust-native redesign,
  not a transcription (internal tool, no external consumer of the old format).
- **Resolved decisions (implemented — see `docs/adr/M2-core.md`):**
  - **snake_case JSON, no serde renaming.** Rust fields map 1:1; every Haskell
    field-name prefix (`retryMaxAttempts`, `tempoUrl`, …) is dropped. This rewrites
    `examples/*.json`. `targets` is a plain `Vec<NamedTarget>` (no union).
  - **Invalid states unrepresentable at parse time:** `NonZeroU32` for counts/delays,
    an `HttpMethod` enum, `deny_unknown_fields`, and real defaults instead of `Option`
    (`load_mode`→Unthrottled, `log_level`→Info, retry/warmup defaulted).
  - **`validate` accumulates** every problem into `ConfigErrors` (field-pathed
    messages) — only the float-range / non-empty / non-empty-string rules the types
    can't express. Not Haskell's fail-on-first.
  - **schemars-derived schema** (`schema_for!`) — never drifts; replaces the old
    custom form-descriptor `config-schema.json` (no in-repo consumer). The committed
    file is regenerated from the types and a test asserts it stays in sync.
  - `FieldAssertion` is a plain externally-tagged serde enum (deletes the hand-rolled
    impl). Crate-level `Error` + `Result<T>`; `${VAR}` interpolation hand-rolled
    (precedence `.env.local` > `.env` > process; undefined → error; unclosed → literal).
  - **No speculative vocabulary:** `Baseline`/regression → M4, runtime
    `ValidationError`/`Summary` → M3. `gauntlet-core` does **not** depend on
    `gauntlet-stats`; the `serde` feature on `gauntlet-stats` was reverted (libm-only).
- **Test gate (Rust-native):** every `examples/*.json` deserializes **and** passes
  `validate`; parse-time type rejections (zero count / bad method / unknown key);
  accumulating validation incl. a multi-error case; `LoadMode`/`FieldAssertion`
  shapes; `${VAR}` interpolation; `extract_durations` → `calculate_stats`.
- **Scope:** library-only; `gauntlet-cli` stays a stub. CLI wiring (`validate`/
  `schema` subcommands) is deferred to **M6**.
- **Exit:** `docs/adr/M2-core.md`; 20 core + 25 stats tests green; fmt + clippy clean.

### M3 — `gauntlet-engine` (the async measurement loop) — ✅ DONE
- **Goal:** real requests fire against a mock server; latency samples + validation summaries come back, CSV written.
- **Resolved decisions (implemented — see `docs/adr/M3-A-client.md` + `docs/adr/M3-engine.md`):**
  - **`reqwest` (rustls), decided by data.** The M3-A spike measured reqwest vs hyper
    overhead against an in-process mock: reqwest's overhead is small (~8µs sequential)
    and *stable* (~5µs stddev), and indistinguishable under concurrency. reqwest wins on
    ergonomics; `hyper` stays a dev-dependency (the spike example only).
  - **One custom rate limiter for every mode** (atomic next-slot reservation), not
    `governor` — covers constant/Poisson/ramp/step uniformly, floors at 6 RPM, and
    composes with the `Semaphore` without double-counting (limiter paces *starts*,
    semaphore caps *in-flight*). Two loop shapes: fixed-count (`JoinSet` + semaphore)
    and duration-based (`concurrency` workers to a shared deadline).
  - **Monotonic measurement clock** (`Instant::elapsed`, spanning retries) — the flagged
    intentional improvement over the Haskell `Realtime` clock. Wall-clock `SystemTime`
    kept only for the CSV `timestamp_iso`.
  - **Hand-rolled retry** (not `backon`): only transport errors (connect/timeout) retry,
    any HTTP status is a response; exhaustion → errored `TestingResponse` (status 0),
    dropped from latency by `extract_durations`. Loop is generic → unit-testable.
  - **Validation:** `ValidationError`/`ValidationSummary` added to `gauntlet-core`
    (vocabulary; reporters read them without an engine dep); the engine owns the dot-path
    + `FieldAssertion` checking logic, capped at the first 50 failing responses. (No
    `jsonpath-rust` dependency — the `$.a.b.0` dot-path is resolved natively.)
  - **Lifecycle hooks** via `tokio::process` (setup/teardown `sh -c`, health-check poll);
    teardown best-effort, failed setup aborts before any request.
  - **CSV** unchanged: 7 columns, `latency_ms = ns/1e6`.
- **Test gate (Rust-native, against an `axum` mock):** request counts, warmup discard,
  concurrency ceiling, retry exhaustion + 500-not-retried + retry-then-succeed,
  constant-RPM pacing + duration-bounded runs, validation pass/fail/50-cap, lifecycle
  setup+health+teardown and setup-abort, and the end-to-end stats+CSV proof. 24 engine
  tests. (Replaces the planned `wiremock`/Haskell-spec port with native equivalents.)
- **Known bias:** absolute latencies include the client's ~40µs floor — cancels for A/B
  comparison and regression deltas; a caveat for absolute single-number claims.
- **Exit:** `docs/adr/M3-A-client.md` + `docs/adr/M3-engine.md`; end-to-end localhost run
  produces a sample vector `calculate_stats` consumes. 24 engine + 20 core + 25 stats =
  69 tests green; fmt + clippy clean; Haskell tree untouched.

### M4 — `gauntlet-report` (reporters + baselines)
- **Goal:** all six output formats, baseline save/compare, regression detection and the exit-code contract.
- **Spike questions:**
  - `Reporter` as `#[async_trait]` (one backend does network I/O) — confirm; `MultiReporter` fan-out.
  - `plotters` chart parity with `plot_latency.py` outputs — which chart types, SVG embedding in HTML.
  - Baseline file format compatibility: **do Rust baselines need to read Haskell-written baseline files?** If users have saved baselines, yes — match the `serde_json` shape exactly. Decide whether to support cross-reading or require re-baselining.
- **Off-the-shelf gate:** `plotters` (HTML/charts), `junit-report` (JUnit), `prometheus` (exposition), `csv`.
- **Parity gate:** golden-file every renderer — capture Haskell markdown/JUnit/Prometheus/HTML/CI output for a fixed result set and diff. The **exit codes (0/1/2)** get explicit regression/error/success cases. Port `MarkdownSpec`, `JUnitSpec`, `PrometheusSpec`, `HTMLSpec`, `CISpec`, `ReporterSpec`, `BaselineSpec`, `OutputSpec`, `FormattingSpec`.
- **Exit:** ADR M4-A; CI-mode regression run returns exit 1.

### M5 — `gauntlet-tui`
- **Goal:** live terminal UI at parity with the Brick version.
- **Spike questions:**
  - ratatui event loop + `tokio::sync::mpsc` for `BenchmarkEvent` replacing the STM `TBQueue`; the `withTUI` async/cancel/error handoff → `tokio::select!` over the task handle + cancel signal + ctrl-c.
  - Which ratatui built-in widgets cover the rolling stats / histogram / request timeline (`Chart`, `Sparkline`, `BarChart`, `Gauge`), and what stays custom.
  - If M0 adopted `rlt`, how much of this it already provides.
- **Off-the-shelf gate:** `rlt`'s TUI vs. hand-built ratatui; study `oha`'s presentation.
- **Parity gate:** harder to golden-file. Port `TUISpec` (state-machine logic is testable headlessly); the *rendering* gets manual/visual sign-off + snapshot tests of the state reducer.
- **Exit:** ADR M5-A; headless (non-TTY) path and TTY path both verified.

### M6 — `gauntlet-tracing` + `gauntlet-cli` (wire-up)
- **Goal:** Tempo trace analysis; the binary dispatches all four subcommands; full end-to-end parity.
- **Spike questions:**
  - Tempo query API surface actually used; OTLP/JSON response parsing.
  - Final CLI dispatch matching `Lib.run`; reporter assembly from flags.
- **Parity gate:** port `TracingSpec`, `TracingClientSpec`, `TracingReportSpec`, `CLISpec`, `ContextSpec`, the `Integration`/`BenchmarkIntegrationSpec`/`BenchmarkRunnerSpec` suites. Run the **same config against both binaries** and diff every artifact and the exit code.
- **Exit:** ADR M6-A; the Rust binary is a drop-in for the documented CLI surface in `README.md`.

## 6. Risk register

| Risk | Severity | Mitigation |
|---|---|---|
| **Silent float drift** in the stats port produces plausible-but-wrong probabilities. | High | M1 golden-vector parity to `1e-9` before anything depends on it. This is why stats is M1. |
| **HdrHistogram error** leaks into the statistical analysis (not just display). | High | Hard rule: exact `Vec<f64>` samples drive Bayesian/EMD/percentile-SE math; HdrHistogram/`average` are display-only estimators. Encode this in the crate API so it can't be misused. |
| **Client overhead/jitter** (reqwest convenience layers) corrupts measurements. | Med-High | M3 reqwest-vs-hyper overhead benchmark; drop to hyper if jitter is material. |
| **Monotonic vs realtime clock** semantics differ from Haskell's `Realtime`. | Low | `Instant` (monotonic) is *more* correct for elapsed time; document as an intentional improvement, not a regression. |
| **Baseline file incompatibility** breaks existing users' saved baselines. | Med | M4 decides: cross-read Haskell baselines, or require re-baseline with a clear migration note. |
| **`governor` only does constant rate**; Poisson/ramp/step need custom code. | Low | Expected; custom interarrival in M3. governor covers only `ConstantRpm`. |
| **Schema drift** between derived (`schemars`) and committed schema. | Low | M2 diffs them; derived becomes source of truth, `schema` subcommand prints it. |
| **`rlt` doesn't expose raw samples** → wrong foundation chosen. | Med | M0 kill criterion catches this before committing to it. |
| **Two parallel implementations** drift during a long migration. | Med | Keep the Haskell binary as the parity oracle for the whole migration; freeze its feature set until M6 ships. |

## 7. Parity-as-oracle strategy — ❌ NOT TAKEN

> Retained for the record only. The owner chose a clean break (see the banner at the top): no Haskell oracle, no two-binary diffing. Each milestone validates with Rust-native correctness tests instead. The rest of this section describes the path *not* taken.

The Haskell binary is the **specification** for the duration of the port. Concretely:

1. Build a `parity/` harness (a script + fixtures) that runs *both* binaries against identical inputs and diffs outputs. Start it in M1 (stats golden vectors) and grow it each milestone.
2. Freeze Haskell feature development until M6 — a moving oracle is not an oracle.
3. Each ported `*Spec.hs` becomes a Rust test *and* (where it asserts on output) a parity fixture. The existing test suite is unusually comprehensive (40+ spec modules); lean on it — it is the best asset this port has.
4. Only at M6, when every artifact and exit code matches for the full `examples/` set, does the Haskell tree get archived.

## 8. What gets deleted (net simplification)

The port is a chance to shed accidental complexity:

- **`scripts/plot_latency.py` + the `uv` runtime dependency + the `plotReporter` subprocess shell-out** → `plotters` in-process. (Removes a whole language toolchain from the deploy.)
- **The `Main.hs` FFI `exit()` hack** → `std::process::exit`.
- **`StrictData` / manual unboxing discipline** → the default in Rust.
- **Hand-written `config-schema.json` maintenance** → derived via `schemars`.
- **Hand-rolled retry, rate limiting, JUnit XML, histogram** → `backon`, `governor`, `junit-report`, `hdrhistogram`.
- **OpenSSL system dependency** (`http-client-tls`) → `rustls` (pure Rust, no system lib).

## 9. Open questions to resolve before M0 closes

1. Greenfield vs. `rlt` foundation. *(highest leverage)*
2. Do existing saved baselines need to be readable by the Rust binary?
3. Is the `schema` subcommand's output contractually stable for anyone downstream, or free to change to the `schemars` shape?
4. Target platforms — is Windows in scope? (`crossterm`/ratatui are cross-platform; the lifecycle process hooks and `Instant` resolution are the things to check.)
5. Is the `tracing` (Tempo) subsystem actually used in practice, or could M6 be deferred / dropped to shrink scope?
6. Distribution: single static binary via `musl`? (rustls makes this clean; another argument over the OpenSSL-linked Haskell build.)

---

### Appendix A — consolidated dependency shortlist

```toml
# core
serde = { version = "1", features = ["derive"] }
serde_json = "1"
schemars = "0.8"
thiserror = "1"
figment = "0.10"          # or `config`
dotenvy = "0.15"
subst = "0.3"             # ${VAR} interpolation  (or shellexpand)

# stats
statrs = "0.17"           # or libm = "0.2"
hdrhistogram = "7"        # display-path percentiles
average = "0.15"          # streaming estimators for the TUI

# engine
tokio = { version = "1", features = ["rt-multi-thread","macros","sync","time","process"] }
reqwest = { version = "0.12", default-features = false, features = ["rustls-tls"] }  # vs. hyper — decide in M3
governor = "0.6"          # ConstantRpm
backon = "1"              # retry/backoff
rand = "0.8"
rand_distr = "0.4"        # Poisson/Exp interarrival
jsonpath-rust = "0.5"     # $.path response validation

# report
async-trait = "0.1"
plotters = "0.3"          # charts/HTML — replaces plot_latency.py
junit-report = "0.8"
prometheus = "0.13"
csv = "1"

# tui
ratatui = "0.29"
crossterm = "0.28"

# cli
clap = { version = "4", features = ["derive"] }
anyhow = "1"              # bin-edge error handling

# logging
tracing = "0.1"           # ⚠️ rename Tempo module to avoid collision
tracing-subscriber = "0.3"

# dev / test
wiremock = "0.6"          # mock server (replaces test/MockServer.hs)
```

### Appendix B — milestone dependency order

```
M0 (foundation)
 └─> M1 (stats) ──┐
 └─> M2 (core) ───┼─> M3 (engine) ─> M4 (report) ─> M6 (cli + tracing)
                  └─────────────────> M5 (tui) ────┘
```

M1 and M2 can proceed in parallel after M0. M5 (TUI) depends on M3 (it consumes the event stream) but not on M4, so it can run alongside M4.
