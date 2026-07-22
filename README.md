# gauntlet

HTTP performance benchmarking with Bayesian A/B analysis, regression detection, and CI integration.

Sends real HTTP requests, measures latency on a monotonic clock at nanosecond precision, and gives direct probability answers like "94% chance the candidate is faster." Supports 1-N targets with automatic pairwise comparison.

## Build & Run

```bash
# Build
cargo build --release

# Run a benchmark
cargo run -p gauntlet-cli --bin gauntlet -- benchmark --config config.json

# ...or use the built binary directly
./target/release/gauntlet benchmark --config config.json

# Tests
cargo test
```

The binary is `gauntlet`. Everything below writes it that way; substitute `cargo run -p gauntlet-cli --bin gauntlet --` if you have not put `target/release` on your `PATH`.

## Quick Start

```json
{
  "targets": [
    { "name": "prod", "url": "http://prod.example.com:8080" },
    { "name": "staging", "url": "http://staging.example.com:8080" }
  ],
  "settings": {
    "iterations": 1000,
    "concurrency": 10
  },
  "payloads": [
    { "name": "health", "method": "GET", "path": "/health" }
  ]
}
```

See `examples/` for more config patterns (A/B comparison, load modes, auth, lifecycle hooks). Every config key is snake_case and maps 1:1 onto the Rust config types, so `gauntlet schema` is always an exact description of what the loader accepts.

## CLI

```
benchmark --config FILE                   Run a benchmark
          --save-baseline NAME            Save this run's stats as a named baseline
          --compare-baseline NAME         Compare against a baseline; exit 1 on regression
          --baseline-dir DIR              Where baselines live (default: baselines)
          --markdown-report FILE          Write a markdown report
          --junit-report FILE             Write JUnit XML
          --html-report FILE              Write a self-contained HTML report (charts inlined)
          --prometheus-file FILE          Write Prometheus exposition text
          --prometheus-pushgateway URL    Push metrics to a Pushgateway
          --prometheus-job NAME           Job name for the push (default: gauntlet)
          --charts KINDS                  Comma-separated chart kinds to render as SVG
          --charts-dir DIR                Where chart SVGs go (default: results/charts)
          --results-dir DIR               Latency CSV + CI artifacts (default: results)
          --no-csv                        Skip the per-request latency CSV
          --no-tui                        Force the headless view on an interactive terminal

compare   FILE_A FILE_B                   Compare two saved baseline/stats JSON files, offline

validate  --config FILE                   Parse and check a config; sends no requests
          --check-endpoints               ...except one GET per target's health-check URL

schema    [--out FILE]                    Print the derived config JSON schema (stdout by default)
```

Exit codes: `0` success, `1` regression detected, `2` error. These are load-bearing for CI.

### Charts

`--charts` takes typed kinds, validated at argument-parse time — an unknown kind fails immediately with the valid list rather than blowing up after the benchmark has already been paid for:

`histogram`, `cdf`, `tail`, `timeline`, `rolling_pct` (alias `rolling`), `boxplot` (alias `box`), `throughput`, `error_rate` (alias `errors`), `status`. Hyphens and underscores are interchangeable.

Charts render in-process via `plotters` and are written as one SVG per target per kind (`<target-slug>-<kind>.svg`). The HTML report inlines its own charts (histogram + CDF) rather than linking files, so the document works when opened over `file://`.

## Configuration

Full reference in the [User Guide](docs/USER_GUIDE.md). Key fields:

| Field | Description |
|-------|-------------|
| `targets` | Array of `{name, url, branch?, lifecycle?}`. `branch` parses but is currently inert — use `lifecycle.setup` to switch branches |
| `settings.iterations` | Requests per endpoint |
| `settings.concurrency` | Concurrent in-flight request limit |
| `settings.secrets` | Path to a file holding a bearer token |
| `settings.load_mode` | `unthrottled`, `constant_rpm`, `ramp_up`, `step_load`, `poisson_rpm` |
| `settings.request_timeout_secs` | Per-request timeout (default: 30) |
| `settings.max_connections` | Connection-pool cap |
| `settings.warmup` | `{iterations: N}` (default: 1) |
| `settings.retry` | `{max_attempts, initial_delay_ms, backoff_multiplier}` |
| `settings.log_level` | `debug`, `info`, `warning`, `error` (parsed, but currently inert) |
| `settings.tempo` | Grafana Tempo tracing config |
| `payloads[].validate` | Per-response assertions: `{status: 200, fields: {"$.path": {"eq": val}}}` |

Unknown keys are rejected rather than silently ignored. Environment variables: `${VAR}` in any config value, resolved from `.env.local` > `.env` > process env.

## Statistical Output

**Per-target:** mean, stddev, min/max, P50/P95/P99, expected shortfall (mean of the worst 1%), success/total.

**Pairwise Bayesian comparison:**
- `prob_b_faster_than_a` — P(mean_B < mean_A), population level
- `prob_single_request_faster` — P(X_B < X_A), individual request
- `prob_b_less_jittery` — P(sigma_B < sigma_A)
- Mean difference with 95% credible interval
- Cohen's d effect size
- P95/P99 comparisons with Maritz-Jarrett standard errors
- Earth Mover's Distance (1-Wasserstein), computed from the raw sample vectors

## Reporters

Six output backends, all composable — every one is a `Reporter` implementation fanned out by `MultiReporter`:

- **Terminal** — always on; a run that printed nothing would look like a run that did nothing
- **Markdown** — full report for CI artifacts (`--markdown-report`)
- **JUnit XML** — test-framework integration (`--junit-report`)
- **Prometheus** — exposition text file and/or Pushgateway push
- **HTML** — one self-contained document with inline SVG charts (`--html-report`)
- **CI** — auto-detected; no flag

Plus the chart reporter (`--charts`) and the live TUI, which runs whenever stdout is an interactive terminal and no CI environment is detected. `q`, `Esc`, or `Ctrl-C` cancels; a cancelled run reports an error and writes no baseline, because an interrupted run has no trustworthy measurements.

To add an output format: implement `Reporter`, add a flag in `crates/gauntlet-cli/src/cli.rs`, and push it in `crates/gauntlet-cli/src/reporters.rs`. Nothing else in the pipeline changes.

## Baselines

```bash
gauntlet benchmark --config config.json --save-baseline v1.0
gauntlet benchmark --config config.json --compare-baseline v1.0
```

Baselines are versioned JSON in `baselines/<name>.json` (`--baseline-dir` to relocate). Regression thresholds are 10% on mean/p50/p95 and 15% on p99; any breach exits 1.

**Baselines written by the old Haskell build are not readable.** The file format is new, snake_case, and carries a `schema_version`; loading an old file reports an unsupported-format error telling you to regenerate with `--save-baseline`. Re-baselining is one run.

## CI Integration

```yaml
# GitHub Actions -- results appear in the step summary automatically
- run: |
    gauntlet benchmark \
      --config config.json \
      --compare-baseline prod \
      --markdown-report results/report.md
```

When `GITHUB_ACTIONS=true`, regression reports are appended to `$GITHUB_STEP_SUMMARY`.
When `GITLAB_CI=true`, collapsible section markers and colour are emitted.
Either way a markdown artifact is written to `--results-dir`. In CI the live TUI is disabled automatically.

## Development

Requires a stable Rust toolchain. The repo is a Cargo workspace of seven crates:

| Crate | What it owns |
|---|---|
| `gauntlet-core` | Types, units, config loading/validation, `${VAR}` env interpolation, derived JSON schema |
| `gauntlet-stats` | Pure numerics: descriptive stats, Bayesian comparison, EMD, histogram. Dependency-light (libm) on purpose |
| `gauntlet-engine` | The async measurement loop: tokio worker pool, reqwest client, rate limiting, retry, validation, warmup, lifecycle hooks, latency CSV |
| `gauntlet-report` | Reporter backends, charts, baselines, regression detection, the exit-code contract |
| `gauntlet-tracing` | Grafana Tempo client and span analysis (not the `tracing` logging crate) |
| `gauntlet-tui` | ratatui live view + the pure event-state reducer |
| `gauntlet-cli` | The `gauntlet` binary: clap surface, dispatch, engine→report adapter, reporter assembly |

The graph is acyclic — `core` at the root, `stats`/`engine`/`report`/`tracing` above it, `cli` (plus `tui`) on top — so editing a TUI widget does not recompile the numerics.

```bash
cargo build --release
cargo test
cargo clippy --all-targets
cargo fmt
```

Architecture details and per-milestone decisions in [CLAUDE.md](CLAUDE.md), [docs/RUST_PORT.md](docs/RUST_PORT.md), and the ADRs under [docs/adr/](docs/adr/).

## License

Proprietary. All rights reserved.
**Author**: Real Limoges
