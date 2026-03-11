# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**gauntlet** is a Haskell-based HTTP performance benchmarking tool with statistical rigor, regression detection, and CI/CD integration. It performs A/B comparisons between endpoints using Bayesian analysis, supports distributed tracing via Grafana Tempo, and provides real-time TUI feedback.

## Build & Development Commands

### Building
```bash
make build              # Build with -O2 optimization
cabal build            # Alternative build command
```

### Running Benchmarks
```bash
make benchmark-nway    # Run N-way comparison benchmark (requires config.json)
make benchmark-single   # Run single benchmark (requires config.json)
# Direct execution
cabal run gauntlet-exe -- benchmark-nway --config config.json
```

### Testing
```bash
cabal test             # Run full test suite (Tasty + QuickCheck)
cabal test --test-show-details=direct  # Verbose test output

# Run specific test module
cabal test --test-options="-p 'BayesianSpec'"
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
- `Config/Loader.hs` - Loads YAML/JSON config, builds endpoint definitions from payloads; exports `loadNwayConfig`, `validateNwayConfig`
- `Config/CLI.hs` - Command-line argument parsing
- `Config/Env.hs` - `.env` / `.env.local` loading and `${VAR}` interpolation in config JSON before decode
- `Execution/Environment.hs` - Git branch switching, docker-compose orchestration, health-check polling
- `Execution/RateLimiter.hs` - Rate limiting: unthrottled, constant RPS, ramp-up, and step-load modes
- `Execution/Validation.hs` - Per-response JSON field validation (status code + field assertions)
- `Network/Auth.hs` - Token reading and auth header injection
- `Network/Exec.hs` - Request execution with retry logic
- `Network/Request.hs` - Nanosecond-precision timed request wrapper
- `TUI.hs` + `TUI/State.hs` + `TUI/Widgets.hs` - Real-time Brick-based terminal UI
- `Report.hs` - Terminal output formatting; also exports `printNwayReport`
- `Report/Formatting.hs` - Statistical test formatting (formatMWU, formatKS, formatAD)
- `Report/Markdown.hs` - Markdown report generation for CI artifacts; also exports `markdownNwayReport`
- `Report/Baseline.hs` - Saves/loads baselines, regression detection with configurable thresholds
- `Report/CI.hs` - GitLab CI and GitHub Actions integration
- `Report/Output.hs` - CSV/JSON serialization; exports `initNwayOutputFiles`, `writeLatenciesWithTarget`
- `Reporter.hs` - `Reporter` record, `combineReporters`, `noOpReporter`
- `Reporter/Terminal.hs` - Wraps `Report.hs` functions into reporter interface
- `Reporter/Markdown.hs` - Writes markdown report to a file path; replaces old `writeMarkdownReport`
- `Reporter/CI.hs` - CI integration behind reporter interface; wraps `Report/CI.hs`
- `Types/` - Type sub-modules: `Baseline.hs`, `Config.hs`, `Error.hs`, `Internal.hs`, `Response.hs`, `Stats.hs`, `Units.hs`

### 2. Stats/ Module (Statistical Analysis)
Shared statistical utilities used by both benchmark and trace analysis.

**Key Components:**
- `Common.hs` - Percentile calculations (R-7 method), standard deviation, variance
- `Benchmark.hs` - Core statistical analysis:
  - `calculateStats()` - Descriptive statistics (mean, stddev, p50, p95, p99, esMs)
  - `compareBayesian()` - Bayesian A/B comparison with conjugate normal models:
    - `probBFasterThanA` - P(mean_B < mean_A) using σ/√n (population-level)
    - `probSingleRequestFaster` - P(X_B < X_A) = Φ((μ_A - μ_B) / √(σ_A² + σ_B²)) (individual request)
    - Mean difference with 95% credible intervals
    - Cohen's d effect size
    - Percentile comparison with Maritz-Jarrett standard error
  - `addFrequentistTests()` - Appends MWU, KS, and Anderson-Darling tests
  - `earthMoversDistance()` - 1-Wasserstein distance for distribution comparison

### 3. Tracing/ Module (Distributed Tracing Integration)
Optional Grafana Tempo integration for analyzing distributed traces.

**Key Components:**
- `Types.hs` - Tempo config, trace/span data structures (re-exports `Nanoseconds`/`Milliseconds` from `Benchmark.Types`)
- `Client.hs` - Grafana Tempo HTTP client, TraceQL query construction and execution, span aggregation
- `Report.hs` - Terminal output formatting for trace analysis

### 4. Runner/ Module (Benchmark Orchestration)
Orchestrates the full benchmark lifecycle, split into sub-modules.

**Key Components:**
- `Runner.hs` - Top-level entry points: `runNway`, `runSingle`
- `Runner/Context.hs` - `RunContext` record, `initContext`, `setupOrFail` (git switch + docker-compose)
- `Runner/Loop.hs` - Concurrent benchmark loops with STM channels
- `Runner/Warmup.hs` - Warmup request execution
- `Runner/Tracing.hs` - Fetches and prints Tempo traces for the benchmark time window
- `Runner/Nway.hs` - N-way comparison orchestration: `runNway`, `allPairComparisons`

### Core Entry Points
- `Lib.hs` - Main dispatcher: parses CLI, loads config, routes to Runner

## Data Flow (A/B Benchmark)

```
CLI Parse → Config Load → Endpoint Build
    ↓
Git Switch (candidate branch) → docker-compose up → Health Check
    ↓
Concurrent Execution (STM channels, QSem) → Nanosecond-timed Responses
    ↓
Git Switch (primary branch) → docker-compose up → Health Check
    ↓
Concurrent Execution → Nanosecond-timed Responses
    ↓
Statistical Analysis → Bayesian Comparison → Frequentist Tests → Report
    ↓
Optional: Fetch Traces → Aggregate Spans → Trace Report
    ↓
Optional: Compare Baseline → Detect Regressions
    ↓
Terminal/JSON/CSV/Markdown Output + Exit Code (0=success, 1=regression, 2=error)
```

## Testing Approach

**Framework:** Tasty with tasty-hunit and tasty-quickcheck for property-based testing

**Test Types:**
- **Unit Tests:** StatsSpec, BayesianSpec, FrequentistSpec, ConfigSpec, BaselineSpec, TracingSpec, AuthSpec, CISpec, CLISpec, ContextSpec, EnvSpec, EnvironmentSpec, LogSpec, MarkdownSpec, NwaySpec, OutputSpec, RateLimiterSpec, ReportSpec, TypesJsonSpec, TypesSpec, ValidationSpec, WarmupSpec
- **Integration Tests:** Integration.hs uses `MockServer.hs` to test HTTP operations end-to-end
- **Property Tests:** PropertySpec.hs verifies statistical invariants (percentile ordering, stat bounds)
- **UI Tests:** TUISpec.hs for Brick widget/state testing

**Test Helpers:**
- `TestHelpers.hs` - Builders for test fixtures (`makeResult`, `makeValidConfig`, `makeSpan`, `makeBaseline`, etc.)
- `MockServer.hs` - HTTP mock utilities (`withMockServer`, `mockJson`, `mockFailThenSucceed`, `mockCountedRequests`)

## Configuration Structure

Benchmarks are configured via JSON/YAML files. See `Benchmark.Config` for parsing logic.

**Key Config Elements:**
- `targets.primary` / `targets.candidate` - Target HTTP base URLs for A/B testing
- `git.primary` / `git.candidate` - Git branch names to switch before each phase
- `settings.iterations` - Number of requests to execute
- `settings.concurrency` - Concurrent request limit
- `settings.secrets` - Path to file containing the Bearer token
- `settings.logLevel` - Log verbosity: "debug", "info", "warning", "error" (default: "info")
- `settings.healthCheckPath` - Path appended to service URL for health polling (default: "/health")
- `settings.healthCheckTimeout` - Max health check poll attempts in seconds (default: 60)
- `settings.warmup` - Optional warmup settings (`warmupIterations`, default: 1)
- `settings.retry` - Optional retry settings (`retryMaxAttempts`, `retryInitialDelayMs`, `retryBackoffMultiplier`)
- `settings.tempo` - Optional tracing configuration (URL, service name, auth token)
- `payloads[].headers` - Optional custom HTTP headers (Map Text Text)
- `payloads[].validate` - Optional per-response validation (`status`, `fields` map of dot-path assertions)
- `settings.loadMode` - Load control mode: `unthrottled` (default), `constantRps`, `rampUp`, `stepLoad`
- `settings.maxConnections` - HTTP connection pool size (default: 10)
- `settings.requestTimeout` - Per-request timeout in seconds (default: 30)
- Environment variable expansion: `${VAR}` in any config value, resolved from `.env.local` > `.env` > process env

## Important Architectural Decisions

### Statistical Foundation
- **Bayesian over Frequentist:** Direct probability answers ("95% chance candidate is faster") vs p-values
- **Conjugate Normal Model:** Analytical posterior computation without MCMC sampling
- **Two probability metrics:** `probBFasterThanA` (population means, σ/√n) vs `probSingleRequestFaster` (individual requests, σ)
- **Anderson-Darling test:** Scholz-Stephens (1987) two-sample test; more tail-sensitive than KS; complement to Mann-Whitney U
- **Expected Shortfall:** `esMs` = E[X | X > p99], mean of worst 1% of requests
- **Type Safety:** Newtypes (`Nanoseconds`, `Milliseconds`) prevent unit conversion bugs

### Concurrency & Performance
- **STM Channels:** Thread-safe concurrent HTTP request execution
- **Connection Pooling:** Reuses HTTP connections via http-client
- **Nanosecond Precision:** High-resolution timing for accurate latency measurement

### Environment Setup
- **Git Integration:** `Benchmark.Environment.setupEnvironment` switches git branches before each benchmark phase
- **Docker Compose:** Runs `docker-compose` with configurable args after git switch
- **Health Checks:** Polls configurable health endpoint until service is ready (default: `/health`, 60s timeout)

### Logging & Observability
- **Structured Logging:** `Log` module provides timestamped logging with levels (Debug, Info, Warning, Error)
- **Configurable Verbosity:** Set `logLevel` in config JSON to control output detail
- **Log Destinations:** Info/Debug → stdout, Warning/Error → stderr
- **Integration:** Logger created from Settings, passed through RunContext to all components

### Configurable Settings
- **Warmup:** Configurable via `WarmupSettings` (default: 1 iteration)
- **Retries:** Configurable via `RetrySettings` (default: 3 attempts, 1s delay, 2x backoff)
- **Log Level:** Configurable via `logLevel` field in Settings (default: Info)
- **Headers:** Custom HTTP headers via `specHeaders` in PayloadSpec
- **Health Check:** `healthCheckPath` and `healthCheckTimeout` in Settings

## Exit Codes

- `0` - Success
- `1` - Performance regression detected (when baseline comparison enabled)
- `2` - Error occurred during execution

## Language & Dependencies

- **Language Standard:** GHC2024
- **Required GHC:** 9.12+ (base >= 4.20.0.0)
- **Key Dependencies:** aeson, http-client, http-client-tls, statistics, brick (TUI), vector, stm, async, process
- **Default Extensions:** OverloadedStrings, NumericUnderscores, RecordWildCards, DeriveAnyClass, StrictData