# gauntlet

HTTP performance benchmarking with Bayesian A/B analysis, regression detection, and CI integration.

Sends real HTTP requests, measures latency at nanosecond precision, and gives direct probability answers like "94% chance the candidate is faster." Supports 1-N targets with automatic pairwise comparison.

## Build & Run

```bash
# Build
cabal build

# Run a benchmark
cabal run gauntlet-exe -- benchmark --config config.json

# Tests
cabal test --test-show-details=direct
```

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

See `examples/` for more config patterns (A/B comparison, load modes, auth, validation).

## CLI Options

```
benchmark --config FILE                   Run benchmark
          --output markdown               Also write markdown report
          --report-path FILE              Markdown output path (default: results/report.md)
          --save-baseline NAME            Save results as named baseline
          --compare-baseline NAME         Compare against baseline (exit 1 on regression)

compare   --baseline-a NAME --baseline-b NAME   Compare two saved baselines

validate  --config FILE                   Validate config without running

schema                                    Print JSON schema for config format
```

Exit codes: `0` success, `1` regression detected, `2` error.

## Configuration

Full reference in the [User Guide](docs/USER_GUIDE.md). Key fields:

| Field | Description |
|-------|-------------|
| `targets` | Array of `{name, url, branch?}` or object with `primary`/`candidate` URLs |
| `settings.iterations` | Requests per endpoint |
| `settings.concurrency` | Concurrent request limit |
| `settings.secrets` | Path to Bearer token file |
| `settings.loadMode` | `unthrottled`, `constantRpm`, `rampUp`, `stepLoad`, `poissonRpm` |
| `settings.requestTimeout` | Per-request timeout in seconds (default: 30) |
| `settings.warmup` | `{warmupIterations: N}` |
| `settings.retry` | `{retryMaxAttempts, retryInitialDelayMs, retryBackoffMultiplier}` |
| `settings.tempo` | Grafana Tempo tracing config |
| `payloads[].validate` | Per-response assertions: `{status: 200, fields: {"$.path": {eq: val}}}` |

Environment variables: `${VAR}` in any config value, resolved from `.env.local` > `.env` > process env.

## Statistical Output

**Per-target:** mean, stddev, P50/P95/P99, expected shortfall (mean of worst 1%).

**Pairwise Bayesian comparison:**
- `probBFasterThanA` -- P(mean_B < mean_A), population-level
- `probSingleRequestFaster` -- P(X_B < X_A), individual request
- `probBLessJittery` -- P(sigma_B < sigma_A)
- Mean difference with 95% credible intervals
- Cohen's d effect size
- Earth Mover's Distance (1-Wasserstein)

## Reporters

Six built-in output formats, combinable:

- **Terminal** -- real-time TUI with rolling stats, histogram, request timeline
- **Markdown** -- full report for CI artifacts
- **JUnit XML** -- test framework integration
- **Prometheus** -- metrics export
- **HTML** -- static report with SVG charts
- **CI** -- GitHub Actions step summary / GitLab CI collapsible sections

## CI Integration

```yaml
# GitHub Actions -- results appear in step summary automatically
- run: |
    cabal run gauntlet-exe -- benchmark \
      --config config.json \
      --compare-baseline prod \
      --output markdown --report-path results/report.md
```

When `GITHUB_ACTIONS=true`, regression reports are appended to `$GITHUB_STEP_SUMMARY`.
When `GITLAB_CI=true`, collapsible section markers are emitted.

## Development

Requires GHC 9.12+ and Cabal 3.12+. Uses GHC2024 language standard.

```bash
make build      # Build with -O2
make test       # Run tests
make format     # Format with fourmolu
make repl       # GHCi REPL
```

Architecture details in [CLAUDE.md](CLAUDE.md).

## License

Proprietary. All rights reserved.
**Author**: Real Limoges
