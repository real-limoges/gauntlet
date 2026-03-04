# gauntlet

**Statistically rigorous HTTP performance benchmarking with A/B testing and distributed tracing.**

A Haskell-based performance testing tool that goes beyond simple request/second metrics, providing Bayesian statistical analysis, regression detection, and CI/CD integration for production-grade performance monitoring.

[![GHC Version](https://img.shields.io/badge/GHC-9.12%2B-blue)](https://www.haskell.org/ghc/)
[![Cabal Version](https://img.shields.io/badge/Cabal-3.12%2B-blue)](https://www.haskell.org/cabal/)
[![Language](https://img.shields.io/badge/Language-GHC2024-purple)](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2024)

---

## Features

### 🎯 Core Capabilities
- **A/B Comparison Testing** - Compare two API versions with statistical rigor
- **N-Way Comparison Testing** - Compare multiple API targets simultaneously
- **Bayesian Statistical Analysis** - Get probability distributions, not just p-values
- **Nanosecond Precision** - High-resolution timing for accurate latency measurement
- **Concurrent Execution** - Configurable concurrency with STM-based coordination
- **Connection Pooling** - Efficient HTTP connection reuse

### 📊 Statistical Analysis
- **Bayesian Comparison** - Direct probability answers ("95% chance candidate is faster")
- **Effect Size Calculation** - Cohen's d for practical significance
- **Percentile Analysis** - P50, P95, P99 with Maritz-Jarrett standard errors
- **Earth Mover's Distance** - 1-Wasserstein distance for distribution comparison
- **Credible Intervals** - 95% Bayesian credible intervals for mean differences
- **Expected Shortfall** - Mean latency of the worst 1% of requests (E[X | X > p99])
- **Anderson-Darling Test** - Tail-sensitive two-sample test (Scholz-Stephens 1987)
- **Mann-Whitney U / KS Tests** - Non-parametric distribution comparison

### 🔍 Observability
- **Real-time TUI** - Live progress tracking with Brick-based terminal UI
- **Grafana Tempo Integration** - Distributed trace collection and analysis
- **Structured Logging** - Configurable log levels (debug, info, warning, error)
- **Multiple Output Formats** - Terminal, JSON, CSV, Markdown

### 🚀 Production Ready
- **Regression Detection** - Compare against saved baselines with configurable thresholds
- **CI/CD Integration** - Native GitLab CI and GitHub Actions support with automatic step summaries
- **Markdown Reports** - Full stats, Bayesian analysis, and validation results in a single `.md` file
- **Retry Logic** - Exponential backoff for flaky networks
- **Warmup Support** - Prime caches before benchmarking
- **Custom Headers** - Full HTTP header control per endpoint

---

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [User Guide](#user-guide)
- [Usage](#usage)
- [Configuration](#configuration)
- [Statistical Methodology](#statistical-methodology)
- [Examples](#examples)
- [Development](#development)
- [Testing](#testing)
- [Architecture](#architecture)
- [CI/CD Integration](#cicd-integration)
- [Contributing](#contributing)

---

## Installation

### Prerequisites

- **GHC 9.12+** - Haskell compiler
- **Cabal 3.12+** - Haskell build tool

### macOS

```bash
# Install GHC and Cabal via GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Or via Homebrew
brew install ghc cabal-install
```

### Linux

```bash
# Via GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Or via package manager
# Ubuntu/Debian
sudo apt-get install ghc cabal-install

# Arch Linux
sudo pacman -S ghc cabal-install
```

### Build from Source

```bash
git clone https://github.com/yourusername/gauntlet.git
cd gauntlet

# Build with optimizations
cabal build -O2

# Or use Makefile
make build
```

---

## Quick Start

### 1. Create a minimal configuration

```json
{
  "targets": {
    "primary": "http://api.example.com",
    "candidate": "http://api-new.example.com"
  },
  "settings": {
    "iterations": 1000,
    "concurrency": 10
  },
  "payloads": [
    {
      "name": "health-check",
      "method": "GET",
      "path": "/health"
    }
  ]
}
```

Save as `config.json`.

### 2. Run the benchmark

```bash
cabal run gauntlet-exe -- benchmark-nway --config config.json
```

### 3. View results

```
=== Benchmark Results ===

Endpoint: health-check

PRIMARY (http://api.example.com)
  Mean: 45.2ms (±2.3ms)
  P50:  42.1ms
  P95:  78.3ms
  P99:  95.1ms

CANDIDATE (http://api-new.example.com)
  Mean: 38.7ms (±1.9ms)
  P50:  36.2ms
  P95:  65.4ms
  P99:  82.7ms

BAYESIAN COMPARISON
  Probability candidate is faster: 94.7%
  Mean difference: -6.5ms [-9.2ms, -3.8ms]
  Effect size (Cohen's d): 0.32 (small to medium)
  Earth Mover's Distance: 0.15
```

---

## User Guide

For a comprehensive walkthrough of all features, configuration options (with explanations of *why* each setting exists), output interpretation, and example configs, see the **[User Guide](docs/USER_GUIDE.md)**.

---

## Usage

### Available Commands

```bash
# Run N-way comparison benchmark (supports 2+ targets)
cabal run gauntlet-exe -- benchmark-nway --config config.json

# Run single endpoint benchmark
cabal run gauntlet-exe -- benchmark-single --config config.json

```

### Command-Line Options

```bash
# Specify configuration file
--config FILE              Path to JSON/YAML config

# Markdown report output
--output markdown          Write a markdown report in addition to terminal output
--report-path FILE         Path for the markdown report (default: results/report.md)

# Baseline comparison
--save-baseline NAME       Save results as baseline
--compare-baseline NAME    Compare against saved baseline (exit 1 on regression)
```

### Exit Codes

- `0` - Success
- `1` - Performance regression detected (when baseline comparison enabled)
- `2` - Error occurred during execution

---

## Configuration

### Minimal Configuration (A/B)

```json
{
  "targets": {
    "primary": "http://api.example.com"
  },
  "git": {
    "primary": "main",
    "candidate": "main"
  },
  "settings": {
    "iterations": 100,
    "concurrency": 10,
    "secrets": ".secrets/token.txt"
  },
  "payloads": [
    {
      "name": "endpoint-name",
      "method": "GET",
      "path": "/api/endpoint"
    }
  ]
}
```

### N-Way Configuration

```json
{
  "targets": [
    { "name": "prod", "url": "http://prod.example.com:8080" },
    { "name": "staging", "url": "http://staging.example.com:8080" },
    { "name": "dev", "url": "http://dev.example.com:8080" }
  ],
  "settings": {
    "iterations": 1000,
    "concurrency": 10,
    "secrets": ".secrets/token.txt"
  },
  "payloads": [
    {
      "name": "endpoint-name",
      "method": "GET",
      "path": "/api/endpoint"
    }
  ]
}
```

N-way targets are an array of objects with `name`, `url`, and optional `branch` fields. All pairwise Bayesian comparisons are computed automatically.

### Full Configuration

```json
{
  "targets": {
    "primary": "http://api-v1.example.com",
    "candidate": "http://api-v2.example.com"
  },
  "git": {
    "primary": "main",
    "candidate": "feature/my-branch"
  },
  "settings": {
    "iterations": 10000,
    "concurrency": 100,
    "maxConnections": 50,
    "requestTimeout": 60,
    "logLevel": "info",
    "secrets": ".secrets/token.txt",
    "healthCheckPath": "/health",
    "healthCheckTimeout": 60,

    "warmup": {
      "warmupIterations": 10
    },

    "retry": {
      "retryMaxAttempts": 3,
      "retryInitialDelayMs": 1000,
      "retryBackoffMultiplier": 2.0
    },

    "tempo": {
      "tempoUrl": "http://tempo:3200",
      "tempoServiceName": "my-service",
      "tempoEnabled": true,
      "tempoAuthToken": "optional-bearer-token"
    }
  },
  "payloads": [
    {
      "name": "create-user",
      "method": "POST",
      "path": "/api/users",
      "headers": {
        "X-API-Key": "your-key",
        "Content-Type": "application/json"
      },
      "body": {
        "name": "test-user",
        "email": "test@example.com"
      },
      "validate": {
        "status": 201,
        "fields": {
          "$.id": {"present": true},
          "$.status": {"eq": "active"}
        }
      }
    }
  ]
}
```

### Configuration Reference

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `targets.primary` | string | required | Primary endpoint base URL |
| `targets.candidate` | string | - | Candidate endpoint base URL for A/B testing |
| `git.primary` | string | required | Git branch for primary target |
| `git.candidate` | string | required | Git branch for candidate target |
| `settings.iterations` | int | required | Number of requests to execute |
| `settings.concurrency` | int | required | Concurrent request limit |
| `settings.secrets` | string | required | Path to file containing Bearer token |
| `settings.maxConnections` | int | 10 | HTTP connection pool size |
| `settings.requestTimeout` | int | 30 | Request timeout (seconds) |
| `settings.logLevel` | string | `"info"` | Log level: `debug`, `info`, `warning`, `error` |
| `settings.healthCheckPath` | string | `"/health"` | Path appended to service URL for health polling |
| `settings.healthCheckTimeout` | int | 60 | Health check poll timeout (seconds) |
| `settings.warmup.warmupIterations` | int | 1 | Warmup iterations before benchmark |
| `settings.retry.retryMaxAttempts` | int | 3 | Maximum retry attempts |
| `settings.retry.retryInitialDelayMs` | int | 1000 | Initial retry delay (ms) |
| `settings.retry.retryBackoffMultiplier` | double | 2.0 | Exponential backoff multiplier |
| `payloads[].headers` | object | - | Custom HTTP headers (key/value map) |
| `payloads[].validate.status` | int | - | Expected HTTP status code |
| `payloads[].validate.fields` | object | - | Dot-path field assertions (`present: true` or `eq: value`) |
| `settings.loadMode` | object | `unthrottled` | Load control mode (see below) |

**Load Control Modes:**

```json
// Unthrottled (default) — no rate limiting
{"mode": "unthrottled"}

// Constant RPS — steady request rate
{"mode": "constantRps", "targetRps": 100}

// Ramp Up — linearly increasing rate
{"mode": "rampUp", "startRps": 10, "endRps": 200, "durationSecs": 60}

// Step Load — discrete rate steps
{"mode": "stepLoad", "steps": [
  {"rps": 50, "durationSecs": 30},
  {"rps": 100, "durationSecs": 30},
  {"rps": 200, "durationSecs": 30}
]}
```

**Environment variable expansion:** Any config value can contain `${VAR}` references, which are expanded before JSON parsing. Variables are resolved from (highest priority first): `.env.local`, `.env`, process environment. Missing variables fail fast with a clear error.

See [`examples/`](examples/) directory for complete configuration examples.

---

## Statistical Methodology

### Bayesian A/B Comparison

Instead of traditional hypothesis testing (p-values), gauntlet uses **Bayesian inference** to provide direct probability statements:

- **"95% probability candidate is faster"** - Clear, interpretable results
- **95% Credible Intervals** - Range of plausible mean differences
- **Effect Size (Cohen's d)** - Practical significance measurement

Two distinct probability metrics are reported:

| Metric | Formula | Answers |
|--------|---------|---------|
| `probBFasterThanA` | Φ((μ_A - μ_B) / √(σ_A²/n_A + σ_B²/n_B)) | Is B's *average* faster? |
| `probSingleRequestFaster` | Φ((μ_A - μ_B) / √(σ_A² + σ_B²)) | Will a *single* request to B be faster? |

#### How It Works

1. **Conjugate Normal Model** - Assumes response times follow a normal distribution
2. **Posterior Distribution** - Computes exact posterior using conjugate priors
3. **Sampling** - Generates 100,000 samples from posterior distributions
4. **Comparison** - Directly compares samples to compute probabilities

#### Interpretation Guide

| Cohen's d | Effect Size | Interpretation |
|-----------|-------------|----------------|
| < 0.2 | Negligible | Difference is negligible |
| 0.2 - 0.5 | Small | Noticeable but small difference |
| 0.5 - 0.8 | Medium | Moderate performance improvement |
| > 0.8 | Large | Substantial performance improvement |

### Percentile Calculations

- **Method**: R-7 (Excel/NumPy default)
- **Standard Errors**: Maritz-Jarrett method for percentile uncertainty
- **Percentiles Reported**: P50 (median), P95, P99

### Distribution Comparison

**Earth Mover's Distance (1-Wasserstein)**
- Measures "cost" to transform one distribution into another
- Scale: 0 (identical) to 1 (completely different)
- Useful for detecting distribution shifts beyond mean differences

**Anderson-Darling Test (Scholz-Stephens 1987)**
- Two-sample test more sensitive to tail differences than KS
- Minimum sample size: 5 per group
- Complements Mann-Whitney U (rank-based) and KS (CDF-based) tests

**Expected Shortfall (ES)**
- `esMs` = E[X | X > p99]: mean latency of the worst 1% of requests
- Captures tail risk beyond what p99 alone conveys

---

## Examples

### Example 1: Simple Health Check

```bash
cabal run gauntlet-exe -- benchmark-nway \
  --config examples/minimal.json
```

### Example 2: A/B API Comparison with Markdown Report

```bash
cabal run gauntlet-exe -- benchmark-nway \
  --config examples/ab-comparison.json \
  --output markdown \
  --report-path results/report.md
```

### Example 3: Regression Detection

```bash
# Save baseline
cabal run gauntlet-exe -- benchmark-nway \
  --config examples/simple-benchmark.json \
  --save-baseline prod-baseline

# Compare against baseline (exits 1 if regression detected)
cabal run gauntlet-exe -- benchmark-nway \
  --config examples/simple-benchmark.json \
  --compare-baseline prod-baseline
```

### Example 4: Authenticated API with Custom Headers

```bash
# Create secrets file
mkdir -p .secrets
echo "Bearer your-token-here" > .secrets/token.txt

cabal run gauntlet-exe -- benchmark-nway \
  --config examples/api-with-auth.json
```

See [`examples/README.md`](examples/README.md) for more examples.

---

## Development

### Prerequisites

- **GHC 9.12+** with GHC2024 language standard
- **Cabal 3.12+**
- **fourmolu** (for code formatting)

### Setup

```bash
# Clone repository
git clone https://github.com/yourusername/gauntlet.git
cd gauntlet

# Install dependencies
cabal build --only-dependencies

# Build with optimizations
cabal build -O2
```

### Development Workflow

```bash
# Start REPL
cabal repl

# Build
cabal build

# Run tests
cabal test

# Format code
fourmolu --mode inplace $(find src test -name '*.hs')

# Or use Makefile
make build
make test
make format
```

### Project Structure

```
gauntlet/
├── src/
│   ├── Benchmark/          # HTTP benchmarking engine
│   │   ├── Types.hs        # Core data types (re-exports Types/ sub-modules)
│   │   ├── Types/          # Type sub-modules
│   │   │   ├── Baseline.hs # Baseline data types
│   │   │   ├── Config.hs   # Configuration types
│   │   │   ├── Error.hs    # Error types
│   │   │   ├── Internal.hs # Internal types
│   │   │   ├── Response.hs # Response types
│   │   │   ├── Stats.hs    # Statistics types
│   │   │   └── Units.hs    # Nanoseconds/Milliseconds newtypes
│   │   ├── Config.hs       # Configuration parsing
│   │   ├── Env.hs          # .env/.env.local loading, ${VAR} interpolation
│   │   ├── Environment.hs  # Git switch + docker-compose + health check
│   │   ├── Network.hs      # HTTP client facade + connection pool init
│   │   ├── Network/        # Network sub-modules (Auth, Exec, Request)
│   │   ├── CLI.hs          # Command-line interface
│   │   ├── Baseline.hs     # Baseline save/load
│   │   ├── RateLimiter.hs  # Rate limiting (unthrottled, constant RPS, ramp-up, step-load)
│   │   ├── Validation.hs   # Per-response JSON field validation
│   │   ├── TUI.hs          # Real-time terminal UI
│   │   ├── TUI/            # TUI sub-modules (State, Widgets)
│   │   ├── Report.hs       # Terminal output
│   │   ├── Report/         # Report sub-modules (Formatting, Markdown)
│   │   ├── Output.hs       # JSON/CSV serialization
│   │   └── CI.hs           # GitLab CI / GitHub Actions integration
│   ├── Runner/             # Benchmark orchestration
│   │   ├── Context.hs      # RunContext, initContext, setupOrFail
│   │   ├── Loop.hs         # Concurrent benchmark loops
│   │   ├── Nway.hs         # N-way comparison orchestration
│   │   ├── Warmup.hs       # Warmup execution
│   │   └── Tracing.hs      # Tempo trace fetching
│   ├── Stats/              # Statistical analysis
│   │   ├── Common.hs       # Percentiles, std dev
│   │   └── Benchmark.hs    # Bayesian + frequentist comparison
│   ├── Tracing/            # Grafana Tempo integration
│   │   ├── Types.hs        # Trace data structures
│   │   ├── Client.hs       # Tempo HTTP client (query construction + execution)
│   │   └── Report.hs       # Trace terminal output
│   ├── Log.hs              # Structured logging
│   ├── Runner.hs           # Top-level entry points
│   └── Lib.hs              # Main dispatcher
├── test/                   # Test suite (Tasty, 35 files)
│   ├── Spec.hs             # Test suite entry point
│   ├── StatsSpec.hs        # Statistical tests
│   ├── StatsCommonSpec.hs  # Common stats utilities
│   ├── BayesianSpec.hs     # Bayesian analysis tests
│   ├── FrequentistSpec.hs  # Frequentist tests and EMD
│   ├── ConfigSpec.hs       # Config parsing tests
│   ├── BaselineSpec.hs     # Baseline tests
│   ├── NwaySpec.hs         # N-way comparison tests
│   ├── NwayIntegrationSpec.hs  # N-way end-to-end tests
│   ├── TracingSpec.hs      # Tracing analysis tests
│   ├── TracingClientSpec.hs    # Tempo client tests
│   ├── TracingQuerySpec.hs     # TraceQL query tests
│   ├── TracingReportSpec.hs    # Trace report tests
│   ├── AuthSpec.hs         # Auth token tests
│   ├── CISpec.hs           # CI integration tests
│   ├── CLISpec.hs          # CLI parsing tests
│   ├── ContextSpec.hs      # RunContext tests
│   ├── EnvSpec.hs          # Env var expansion tests
│   ├── EnvironmentSpec.hs  # Environment setup tests
│   ├── LoadControlIntegrationSpec.hs  # Load control tests
│   ├── LogSpec.hs          # Logging tests
│   ├── MarkdownSpec.hs     # Markdown report tests
│   ├── OutputSpec.hs       # CSV/JSON output tests
│   ├── RateLimiterSpec.hs  # Rate limiter tests
│   ├── ReportSpec.hs       # Terminal report tests
│   ├── TypesJsonSpec.hs    # JSON serialization tests
│   ├── TypesSpec.hs        # Type tests
│   ├── ValidationSpec.hs   # Validation tests
│   ├── WarmupSpec.hs       # Warmup tests
│   ├── Integration.hs      # HTTP execution tests
│   ├── PropertySpec.hs     # QuickCheck properties
│   ├── TUISpec.hs          # Brick widget tests
│   ├── TestHelpers.hs      # Shared test fixtures
│   ├── TastyCompat.hs      # Test compatibility helpers
│   └── MockServer.hs       # HTTP mock utilities
├── examples/               # Example configurations
└── CLAUDE.md               # Architecture guide
```

---

## Testing

### Run All Tests

```bash
cabal test
```

### Run Specific Test Suite

```bash
# Run only unit tests
cabal test --test-options="-p 'StatsSpec'"

# Run with verbose output
cabal test --test-show-details=direct
```

### Test Coverage

- **Unit Tests**: Statistical functions, config parsing, baseline comparison
- **Integration Tests**: HTTP operations with mock server
- **Property Tests**: QuickCheck for statistical invariants
- **UI Tests**: Brick TUI widget testing

### Test Framework

- **Tasty** - Test framework with `tasty-hunit` and `tasty-quickcheck`
- **QuickCheck** - Property-based testing
- **MockServer** - HTTP mock utilities for integration tests

---

## Architecture

### Core Design Principles

1. **Type Safety** - Newtypes prevent unit conversion bugs (`Nanoseconds`, `Milliseconds`)
2. **Concurrency** - STM channels for thread-safe concurrent execution
3. **Statistical Rigor** - Bayesian inference over frequentist hypothesis testing
4. **Observability** - Structured logging and distributed tracing

### Data Flow

```
CLI Parse → Config Load → Endpoint Build
    ↓
Git Switch (candidate) → docker-compose up → Health Check
    ↓
Warmup → Concurrent Execution (STM) → Nanosecond-timed Responses
    ↓
Git Switch (primary) → docker-compose up → Health Check
    ↓
Warmup → Concurrent Execution (STM) → Nanosecond-timed Responses
    ↓
Statistical Analysis → Bayesian + Frequentist Comparison → Report
    ↓
Optional: Fetch Traces → Aggregate Spans → Trace Report
    ↓
Optional: Compare Baseline → Detect Regressions
    ↓
Terminal Output + Optional Markdown Report + Exit Code
```

### Key Modules

- **Benchmark.Network** - HTTP client with connection pooling and retries
- **Stats.Benchmark** - Bayesian comparison and percentile calculations
- **Benchmark.TUI** - Real-time Brick-based terminal UI
- **Tracing.Client** - Grafana Tempo integration
- **Benchmark.Baseline** - Regression detection

See [`CLAUDE.md`](CLAUDE.md) for detailed architecture documentation.

---

## CI/CD Integration

### GitLab CI Example

When `GITLAB_CI=true` is set, gauntlet automatically emits collapsible section markers and writes a markdown artifact report.

```yaml
performance-test:
  stage: test
  script:
    - cabal build
    - cabal run gauntlet-exe -- benchmark-nway \
        --config config.json \
        --compare-baseline prod-baseline
  artifacts:
    paths:
      - results/benchmark-report-*.md
  rules:
    - if: '$CI_PIPELINE_SOURCE == "merge_request_event"'
```

### GitHub Actions Example

When `GITHUB_ACTIONS=true` is set, gauntlet automatically appends the regression report to `$GITHUB_STEP_SUMMARY`, making results visible directly in the Actions UI.

```yaml
name: Performance Tests

on: [pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell/actions/setup@v2
        with:
          ghc-version: '9.10'
          cabal-version: '3.12'

      - name: Run benchmark
        run: |
          cabal build
          cabal run gauntlet-exe -- benchmark-nway \
            --config config.json \
            --compare-baseline prod-baseline \
            --output markdown \
            --report-path results/report.md

      - name: Upload report
        uses: actions/upload-artifact@v4
        with:
          name: benchmark-report
          path: results/report.md
```

### Markdown Reports

Use `--output markdown` to write a full report covering stats, Bayesian analysis, and validation results:

```bash
cabal run gauntlet-exe -- benchmark-nway \
  --config config.json \
  --output markdown \
  --report-path /tmp/report.md
```

The markdown report is written **in addition to** terminal output — it does not replace it.

### Exit Codes for CI

- **Exit 0** - Success, no regression
- **Exit 1** - Performance regression detected (fails CI)
- **Exit 2** - Error during execution (fails CI)

---

## Contributing

Contributions are welcome! Please follow these guidelines:

1. **Code Style** - Format with `fourmolu` before committing
2. **Tests** - Add tests for new features
3. **Documentation** - Update README and docs for user-facing changes
4. **Commit Messages** - Use conventional commit format

```bash
# Format code
fourmolu --mode inplace $(find src test -name '*.hs')

# Run tests
cabal test

# Build and verify
cabal build -O2
```

---

## License

This project is proprietary software. All rights reserved.

**Author**: Real Limoges
**Maintainer**: real.limoges@truefootage.tech

---

## Acknowledgments

Built with:
- [Haskell](https://www.haskell.org/) - Functional programming language
- [http-client](https://hackage.haskell.org/package/http-client) - HTTP client library
- [statistics](https://hackage.haskell.org/package/statistics) - Statistical analysis
- [brick](https://hackage.haskell.org/package/brick) - Terminal UI framework
- [aeson](https://hackage.haskell.org/package/aeson) - JSON parsing
- [vector](https://hackage.haskell.org/package/vector) - High-performance arrays

Statistical methodology inspired by Bayesian Data Analysis (Gelman et al.) and practical A/B testing literature.

---

## Support

- **Issues**: [GitHub Issues](https://github.com/yourusername/gauntlet/issues)
- **Examples**: See [`examples/`](examples/) directory

For questions or feature requests, please open an issue.
