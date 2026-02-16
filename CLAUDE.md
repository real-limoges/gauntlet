# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**laughing-waffle** is a Haskell-based HTTP performance benchmarking tool with statistical rigor, regression detection, and CI/CD integration. It performs A/B comparisons between endpoints using Bayesian analysis, supports distributed tracing via Grafana Tempo, and provides real-time TUI feedback.

## Build & Development Commands

### Building
```bash
make build              # Build with -O2 optimization
cabal build            # Alternative build command
```

### Running Benchmarks
```bash
make benchmark-multiple # Run multiple benchmarks (requires config.json)
make benchmark-single   # Run single benchmark (requires config.json)
make verify            # Verify test configuration

# Direct execution
cabal run laughing-waffle-exe -- benchmark-multiple --config config.json
```

### Testing
```bash
cabal test             # Run full test suite (hspec + QuickCheck)
cabal test --test-show-details=direct  # Verbose test output

# Run specific test module
cabal test --test-options="--match 'BayesianSpec'"
```

### Development
```bash
cabal repl             # Start GHCi REPL with project loaded
cabal clean            # Clean build artifacts
```

## Architecture Overview

The codebase is organized into three major functional areas:

### 1. Benchmark/ Module (HTTP Performance Testing)
Core benchmarking engine that handles HTTP requests, concurrent execution, and analysis.

**Key Components:**
- `Types.hs` - Core data types: `Endpoint`, `BenchmarkStats`, `BayesianComparison`, `TestConfig`, `PerfTestError`
- `Config.hs` - Loads YAML/JSON config, builds endpoint definitions from payloads
- `Network.hs` - HTTP client with connection pooling, nanosecond-precision timing, retries (hardcoded: 3 attempts)
- `CLI.hs` - Command-line argument parsing
- `Baseline.hs` - Saves/loads baselines, regression detection with configurable thresholds
- `TUI.hs` + `TUI/State.hs` + `TUI/Widgets.hs` - Real-time Brick-based terminal UI
- `Report.hs` - Terminal output formatting
- `Output.hs` - CSV/JSON serialization
- `Plotting.hs` - KDE distribution plots

### 2. Stats/ Module (Statistical Analysis)
Shared statistical utilities used by both benchmark and trace analysis.

**Key Components:**
- `Common.hs` - Percentile calculations (R-7 method), standard deviation, variance
- `Benchmark.hs` - Core statistical analysis:
  - `calculateStats()` - Descriptive statistics (mean, stddev, p50, p95, p99)
  - `compareBayesian()` - Bayesian A/B comparison with conjugate normal models:
    - Probability candidate is faster than primary
    - Mean difference with 95% credible intervals
    - Cohen's d effect size
    - Percentile comparison with Maritz-Jarrett standard error
  - `earthMoversDistance()` - 1-Wasserstein distance for distribution comparison

### 3. Tracing/ Module (Distributed Tracing Integration)
Optional Grafana Tempo integration for analyzing distributed traces.

**Key Components:**
- `Types.hs` - Tempo config, trace/span data structures
- `Client.hs` - Grafana Tempo HTTP client, TraceQL query execution
- `Analysis.hs` - Aggregates spans by name, computes duration statistics
- `Report.hs` - Terminal output formatting for trace analysis

### Core Entry Points
- `Lib.hs` - Main dispatcher: parses CLI, loads config, routes to Runner/VerifyRunner
- `Runner.hs` - Orchestrates A/B benchmarks: environment setup, warmup (hardcoded: 1 iteration), concurrent execution, optional trace collection
- `VerifyRunner.hs` - Response verification against expected results

## Data Flow (A/B Benchmark)

```
CLI Parse → Config Load → Endpoint Build
    ↓
Environment Setup → Network Init → Warmup (1 iteration)
    ↓
Concurrent Execution (STM channels, QSem) → Nanosecond-timed Responses
    ↓
Statistical Analysis → Bayesian Comparison → Report
    ↓
Optional: Fetch Traces → Aggregate Spans → Trace Report
    ↓
Optional: Compare Baseline → Detect Regressions
    ↓
Terminal/JSON/CSV Output + Exit Code (0=success, 1=regression, 2=error)
```

## Testing Approach

**Framework:** Hspec with QuickCheck for property-based testing

**Test Types:**
- **Unit Tests:** StatsSpec, BayesianSpec, ConfigSpec, VerifySpec
- **Integration Tests:** Integration.hs uses `MockServer.hs` to test HTTP operations end-to-end
- **Property Tests:** PropertySpec.hs verifies statistical invariants (percentile ordering, stat bounds)
- **UI Tests:** TUISpec.hs for Brick widget/state testing

**Test Helpers:**
- `TestHelpers.hs` - Builders for test fixtures (`makeResult`, `makeValidConfig`, `makeSpan`, etc.)
- `MockServer.hs` - HTTP mock utilities (`withMockServer`, `mockJson`, `mockFailThenSucceed`)

## Configuration Structure

Benchmarks are configured via JSON/YAML files. See `Benchmark.Config` for parsing logic.

**Key Config Elements:**
- `endpoint` - Target URL(s) for A/B testing
- `method` - HTTP method (GET, POST, etc.)
- `body` - Request payload
- `iterations` - Number of requests to execute
- `concurrency` - Concurrent request limit
- `baseline` - Optional baseline file path for regression detection
- `tempo` - Optional tracing configuration (URL, service name, auth token)
- `retry` - Optional retry settings (max attempts, delay, backoff multiplier)
- `warmup` - Optional warmup settings (iteration count)
- `logLevel` - Optional log verbosity: "debug", "info", "warning", "error" (default: "info")
- `headers` - Optional custom HTTP headers (Map Text Text)

## Important Architectural Decisions

### Statistical Foundation
- **Bayesian over Frequentist:** Direct probability answers ("95% chance candidate is faster") vs p-values
- **Conjugate Normal Model:** Analytical posterior computation without MCMC sampling
- **Type Safety:** Newtypes (`Nanoseconds`, `Milliseconds`) prevent unit conversion bugs

### Concurrency & Performance
- **STM Channels:** Thread-safe concurrent HTTP request execution
- **Connection Pooling:** Reuses HTTP connections via http-client
- **Nanosecond Precision:** High-resolution timing for accurate latency measurement

### Logging & Observability
- **Structured Logging:** `Log` module provides timestamped logging with levels (Debug, Info, Warning, Error)
- **Configurable Verbosity:** Set `logLevel` in config JSON to control output detail
- **Log Destinations:** Info/Debug → stdout, Warning/Error → stderr
- **Integration:** Logger created from Settings, passed through RunContext to all components

### Configurable Settings (as of 2026-02-16)
- **Warmup:** Configurable via `WarmupSettings` (default: 1 iteration)
- **Retries:** Configurable via `RetrySettings` (default: 3 attempts, 1s delay, 2x backoff)
- **Log Level:** Configurable via `logLevel` field in Settings (default: Info)
- **Headers:** Custom HTTP headers via `specHeaders` in PayloadSpec

## Roadmap Highlights

**High-Priority Features:**
1. Custom HTTP headers support (partial implementation)
2. Load control modes: constant RPS, ramp-up, step load
3. Request chaining for authenticated flows (JSONPath extraction, templating)

See `docs/ROADMAP.md` and `docs/TODO.md` for detailed implementation guides.

## Exit Codes

- `0` - Success
- `1` - Performance regression detected (when baseline comparison enabled)
- `2` - Error occurred during execution

## Language & Dependencies

- **Language Standard:** GHC2024
- **Required GHC:** 9.10+ (base >= 4.20.0.0)
- **Key Dependencies:** aeson, http-client, statistics, brick (TUI), Chart (plotting)
- **Default Extensions:** OverloadedStrings, NumericUnderscores, RecordWildCards, DeriveAnyClass, StrictData