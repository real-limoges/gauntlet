# Example Configurations

This directory contains sample configuration files demonstrating various features of gauntlet.

## Quick Start

```bash
# Build once, then run the binary directly
cargo build --release

./target/release/gauntlet benchmark --config examples/simple-benchmark.json
```

Every config in this directory is parsed and validated by the test suite
(`crates/gauntlet-core/tests/examples.rs`), so these files are known-good.

## Example Files

### `minimal.json`
**Minimal configuration** - The absolute minimum required fields.
- 100 iterations, 10 concurrent requests
- Single health check endpoint

**Use case:** Quick smoke test or learning the basic structure.

### `simple-benchmark.json`
**Basic REST API testing** - Common use case for API performance testing.
- 1,000 iterations, 20 concurrent requests
- Three endpoints: GET users, GET user by ID, POST create user
- Structured logging at INFO level

**Use case:** Local development testing of a REST API.

### `ab-comparison.json`
**A/B performance comparison** - Compare two API versions.
- 5,000 iterations, 50 concurrent requests
- Warmup iterations to prime caches
- Retry settings for flaky networks
- Lifecycle health checks per target

**Use case:** Production vs. staging comparison, or v1 vs. v2 API testing.

### `api-with-auth.json`
**Custom headers and authentication** - Advanced HTTP configuration.
- Custom headers per endpoint (`X-API-Key`, `Content-Type`, etc.)
- Higher concurrency (25) and longer timeout (45s)
- More aggressive retry strategy (5 attempts)

**Use case:** Testing authenticated APIs with custom headers.

### `advanced-config.json`
**Full feature demonstration** - All features enabled.
- 10,000 iterations, 100 concurrent requests
- DEBUG logging for detailed output
- Grafana Tempo tracing integration
- Lifecycle hooks (docker-compose setup/teardown, health checks)
- Complex POST bodies with nested JSON
- Connection pooling tuning
- Load control (constant RPM)

**Use case:** Production-grade benchmarking with distributed tracing and rate limiting.

### `log-levels.json`
**Log verbosity examples** - Demonstrates different log levels.

Change `log_level` to control diagnostic verbosity on stderr:
- `"debug"` - Everything, including diagnostics that are normally suppressed
- `"info"` - Normal progress notes, e.g. where a baseline was written (default)
- `"warning"` - Only warnings and errors
- `"error"` - Only errors

This filters *diagnostics*, not the benchmark report: the statistics, comparison,
and validation output are the result of the command you ran and are always
printed to stdout.

### `comparison.json`
**Multi-target comparison** - Compare multiple API targets simultaneously.
- 3 named targets: prod, staging, dev
- All pairwise Bayesian comparisons computed automatically

**Use case:** Comparing performance across multiple environments or API versions.

### `load-modes.json`
**Step load profile** - Ramp requests up and back down in discrete steps.
- 4 steps: 600 -> 3000 -> 6000 -> 3000 RPM
- Duration-based (`iterations` ignored; total requests derived from schedule)

**Use case:** Stress testing, finding saturation points, validating autoscaling.

## Configuration Reference

### Required Fields

```json
{
  "targets": [
    { "name": "my-service", "url": "http://api.example.com" }
  ],
  "settings": {
    "iterations": 1000,
    "concurrency": 10
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

### Target Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Human-readable label for this target |
| `url` | string | yes | Base URL (payloads paths are appended) |
| `lifecycle` | object | no | Setup/teardown hooks and health check config |

To benchmark two git branches against each other, give each target a `lifecycle.setup`
that switches and rebuilds; see `advanced-config.json`. (A `branch` field used to be
accepted here; nothing ever acted on it, and configs that still set it now fail to load.)

### Optional Settings

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `secrets` | string | - | Path to a file holding a bearer token, sent as `Authorization: Bearer <token>` |
| `max_connections` | int | 10 | HTTP connection pool size |
| `request_timeout_secs` | int | 30 | Request timeout (seconds) |
| `load_mode` | object | `unthrottled` | Load control: `unthrottled`, `constant_rpm`, `ramp_up`, `step_load`, `poisson_rpm` |
| `log_level` | string | `"info"` | Diagnostic verbosity on stderr: `"debug"`, `"info"`, `"warning"`, `"error"`. Does not suppress the benchmark report itself. |
| `warmup` | object | `{"iterations": 1}` | Warmup configuration |
| `retry` | object | See below | Retry configuration |
| `tempo` | object | - | Grafana Tempo tracing |

All config keys are `snake_case`, and unknown keys are a hard error rather than
being silently ignored — a typo fails `validate` instead of being dropped.

### Retry Settings

```json
{
  "retry": {
    "max_attempts": 3,
    "initial_delay_ms": 1000,
    "backoff_multiplier": 2.0
  }
}
```

- `max_attempts` - Number of retries; `0` disables them (default: 3)
- `initial_delay_ms` - Initial backoff delay (default: 1000)
- `backoff_multiplier` - Exponential backoff multiplier, must be >= 1.0 (default: 2.0)

Only *transport* failures retry (connection refused, timeout). Any HTTP status,
including 500, is a final response and is recorded as one.

### Warmup Settings

```json
{
  "warmup": {
    "iterations": 10
  }
}
```

Warmup primes connection pools before measurement. It runs against the target's
**first** endpoint only, and its requests are discarded rather than measured.

Warmup requests prime caches and JIT compilers before the actual benchmark.

### Custom Headers

```json
{
  "payloads": [
    {
      "name": "authenticated-request",
      "method": "POST",
      "path": "/api/data",
      "headers": {
        "X-API-Key": "your-key",
        "Content-Type": "application/json",
        "Accept": "application/json"
      },
      "body": {"key": "value"}
    }
  ]
}
```

Headers are per-payload. If `Content-Type` is not specified for POST/PUT requests, it defaults to `application/json`.

### Lifecycle Hooks

```json
{
  "targets": [
    {
      "name": "my-service",
      "url": "http://localhost:8080",
      "lifecycle": {
        "setup": { "cmd": "docker-compose up -d", "timeout_secs": 120 },
        "teardown": { "cmd": "docker-compose down" },
        "health_check": { "url": "http://localhost:8080/health", "timeout_secs": 60 }
      }
    }
  ]
}
```

Setup runs before benchmarking each target; teardown runs after. Health check polls until the service is ready.

### Tempo Tracing

```json
{
  "settings": {
    "tempo": {
      "url": "http://tempo:3200",
      "service_name": "my-service",
      "enabled": true,
      "auth_token": "optional-bearer-token"
    }
  }
}
```

Trace analysis is a diagnostic: if Tempo is unreachable or has not yet ingested
the run's spans, the section is skipped with a warning and the benchmark's own
result is unaffected.

Integrates with Grafana Tempo for distributed trace analysis.

## Running Examples

```bash
GAUNTLET=./target/release/gauntlet

# Basic benchmark
$GAUNTLET benchmark --config examples/minimal.json

# Save this run as a baseline, then compare a later run against it.
# A comparison that regresses exits 1, which is what CI keys on.
$GAUNTLET benchmark --config examples/simple-benchmark.json --save-baseline my-baseline
$GAUNTLET benchmark --config examples/simple-benchmark.json --compare-baseline my-baseline

# Markdown report
$GAUNTLET benchmark \
  --config examples/simple-benchmark.json \
  --markdown-report results/report.md

# Charts + a self-contained HTML report
$GAUNTLET benchmark \
  --config examples/simple-benchmark.json \
  --charts histogram,cdf,throughput \
  --html-report results/report.html

# Validate config without sending any requests
$GAUNTLET validate --config examples/advanced-config.json
```

## Tips

1. **Start Small**: Begin with `minimal.json` and gradually add features
2. **Use Warmup**: Add 5-10 warmup iterations for consistent results
3. **Tune Concurrency**: Match your target service's capacity
4. **Enable Retries**: Use retry settings for flaky networks
5. **Log Levels**: Use `"info"` for normal runs, `"debug"` for troubleshooting
6. **Secrets Management**: Never commit `.secrets/` directory - add to `.gitignore`.
   `settings.secrets` is a path to a file holding a bearer token; it is sent as
   `Authorization: Bearer <token>` on every request that does not set its own.
7. **Live UI**: benchmarks show a live terminal view when stdout is a TTY and no CI
   is detected. Use `--no-tui` to force the plain output.

## See Also

- [docs/USER_GUIDE.md](../docs/USER_GUIDE.md) - The full configuration reference
- [README.md](../README.md) - Overview and getting started
