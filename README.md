# perf-testing

A Haskell performance testing tool with Bayesian statistical analysis and GitLab CI integration.

## Features

- **A/B Benchmarking** - Compare two targets (primary vs candidate) with statistical confidence
- **Single-target Benchmarking** - Benchmark a single endpoint
- **Bayesian Analysis** - Probability that candidate is faster, effect size (Cohen's d), credible intervals
- **Baseline Regression Detection** - Save baselines and detect regressions in CI
- **GitLab CI Integration** - Colored output, markdown reports, pipeline-aware
- **Distributed Tracing** - Optional Tempo integration for span-level analysis
- **JSON & Terminal Output** - Machine-readable or human-friendly

## Installation

```bash
# Build with optimizations
cabal build --enable-optimization=2 perf-testing-exe

# Run tests
cabal test
```

## Quick Start

### 1. Create a config file

```json
{
  "targets": {
    "primary": "https://api.example.com",
    "candidate": "https://api-staging.example.com"
  },
  "git": {
    "primary": "main",
    "candidate": "feature-branch"
  },
  "settings": {
    "iterations": 100,
    "concurrency": 4,
    "secrets": "path/to/token/file"
  },
  "payloads": [
    {
      "name": "Get Users",
      "method": "GET",
      "path": "/api/users",
      "body": null
    },
    {
      "name": "Create User",
      "method": "POST",
      "path": "/api/users",
      "body": {
        "name": "test",
        "email": "test@example.com"
      }
    }
  ]
}
```

### 2. Run a benchmark

```bash
# A/B comparison (primary vs candidate)
perf-testing-exe benchmark-multiple -c config.json

# Single target benchmark
perf-testing-exe benchmark-single -c config.json

# Verify responses match between targets
perf-testing-exe verify -c config.json
```

## Commands

### benchmark-multiple

Compares candidate against primary target with Bayesian statistical analysis.

```bash
perf-testing-exe benchmark-multiple -c config.json
```

**Output includes:**
- Mean, P50, P95, P99 latencies for both targets
- Probability that candidate is faster
- Effect size (Cohen's d)
- 95% credible interval
- KDE distribution plot saved to `results/`

### benchmark-single

Benchmarks a single target (uses `candidate` from config).

```bash
perf-testing-exe benchmark-single -c config.json

# JSON output
perf-testing-exe benchmark-single -c config.json -o json

# With baseline comparison (for CI)
perf-testing-exe benchmark-single -c config.json \
  --compare-baseline main \
  --save-baseline pr-123
```

### verify

Checks that primary and candidate return semantically identical JSON responses.

```bash
perf-testing-exe verify -c config.json
```

## Baseline Mode (CI/CD)

Save benchmark results as named baselines and compare future runs:

```bash
# Save a baseline
perf-testing-exe benchmark-single -c config.json --save-baseline v1.0

# Compare against baseline (exits 1 on regression)
perf-testing-exe benchmark-single -c config.json --compare-baseline v1.0

# Save and compare in one command
perf-testing-exe benchmark-single -c config.json \
  --save-baseline pr-123 \
  --compare-baseline main
```

**Default thresholds:**
- Mean, P50, P95: 10% regression
- P99: 15% regression

## GitLab CI Integration

The tool auto-detects GitLab CI and provides:
- Colored section markers in job logs
- Markdown report at `results/benchmark-report-*.md`
- Exit code 1 on regression

Example `.gitlab-ci.yml`:

```yaml
benchmark:
  stage: test
  script:
    - perf-testing-exe benchmark-single -c config.json
        --compare-baseline main
        --save-baseline "mr-${CI_MERGE_REQUEST_IID}"
  artifacts:
    paths:
      - results/
      - baselines/
  rules:
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"
```

## Configuration Reference

```json
{
  "targets": {
    "primary": "https://...",
    "candidate": "https://..."
  },
  "git": {
    "primary": "main",
    "candidate": "feature-branch"
  },
  "settings": {
    "iterations": 100,
    "concurrency": 4,
    "secrets": "path/to/bearer/token",
    "maxConnections": 10,
    "connIdleTimeout": 30,
    "requestTimeout": 30,
    "tempo": {
      "tempoUrl": "http://tempo:3200",
      "tempoServiceName": "my-service",
      "tempoEnabled": true,
      "tempoAuthToken": "optional-token"
    }
  },
  "payloads": [
    {
      "name": "endpoint-name",
      "method": "GET|POST|PUT|DELETE|PATCH",
      "path": "/api/endpoint",
      "body": null
    }
  ]
}
```

## Output Files

All output is written to `results/`:

| File | Description |
|------|-------------|
| `latencies-<timestamp>.csv` | Raw latency data for all requests |
| `failures-<timestamp>.log` | Failed request details |
| `kde_plot-<timestamp>.png` | Distribution comparison (A/B mode) |
| `traces-<timestamp>.json` | Raw trace data (if Tempo enabled) |
| `benchmark-report-<timestamp>.md` | Markdown report (GitLab CI) |

Baselines are stored in `baselines/`:

| File | Description |
|------|-------------|
| `baselines/<name>.json` | Saved baseline with stats and timestamp |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (no regression or baseline not specified) |
| 1 | Regression detected (metric exceeded threshold) |
| 2 | Runtime error (config invalid, endpoint unreachable) |

## Development

```bash
# Build
cabal build

# Test
cabal test

# Lint
hlint src/
```

See `ROADMAP.md` for planned features.
See `docs/TUI_IMPLEMENTATION_GUIDE.md` for TUI development guide.

## License

Proprietary
