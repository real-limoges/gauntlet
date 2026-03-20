# Example Configurations

This directory contains sample configuration files demonstrating various features of gauntlet.

## Quick Start

```bash
# Run a simple benchmark
cabal run gauntlet-exe -- benchmark --config examples/simple-benchmark.json
```

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

Change `logLevel` to control output verbosity:
- `"debug"` - Verbose output including request/response details
- `"info"` - Normal output with progress updates (default)
- `"warning"` - Only warnings and errors
- `"error"` - Only critical errors

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
| `branch` | string | no | Git branch to switch to before benchmarking |
| `lifecycle` | object | no | Setup/teardown hooks and health check config |

### Optional Settings

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `secrets` | string | - | Path to file containing Bearer token |
| `maxConnections` | int | 10 | HTTP connection pool size |
| `requestTimeout` | int | 30 | Request timeout (seconds) |
| `loadMode` | object | `unthrottled` | Load control: `unthrottled`, `constantRpm`, `rampUp`, `stepLoad`, `poissonRpm` |
| `logLevel` | string | `"info"` | Log verbosity: `"debug"`, `"info"`, `"warning"`, `"error"` |
| `warmup` | object | `{"warmupIterations": 1}` | Warmup configuration |
| `retry` | object | See below | Retry configuration |
| `tempo` | object | - | Grafana Tempo tracing |

### Retry Settings

```json
{
  "retry": {
    "retryMaxAttempts": 3,
    "retryInitialDelayMs": 1000,
    "retryBackoffMultiplier": 2.0
  }
}
```

- `retryMaxAttempts` - Number of retry attempts (default: 3)
- `retryInitialDelayMs` - Initial delay in milliseconds (default: 1000)
- `retryBackoffMultiplier` - Exponential backoff multiplier (default: 2.0)

### Warmup Settings

```json
{
  "warmup": {
    "warmupIterations": 10
  }
}
```

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
        "setup": { "cmd": "docker-compose up -d", "timeoutSecs": 120 },
        "teardown": { "cmd": "docker-compose down" },
        "healthCheck": { "url": "http://localhost:8080/health", "timeoutSecs": 60 }
      }
    }
  ]
}
```

Setup runs before benchmarking each target; teardown runs after. Health check polls until the service is ready.

### Tempo Tracing

```json
{
  "tempo": {
    "tempoUrl": "http://tempo:3200",
    "tempoServiceName": "my-service",
    "tempoEnabled": true,
    "tempoAuthToken": "optional-bearer-token"
  }
}
```

Integrates with Grafana Tempo for distributed trace analysis.

## Running Examples

```bash
# Basic benchmark
cabal run gauntlet-exe -- benchmark --config examples/minimal.json

# With baseline comparison
cabal run gauntlet-exe -- benchmark \
  --config examples/simple-benchmark.json \
  --save-baseline my-baseline

cabal run gauntlet-exe -- benchmark \
  --config examples/simple-benchmark.json \
  --compare-baseline my-baseline

# Markdown report
cabal run gauntlet-exe -- benchmark \
  --config examples/simple-benchmark.json \
  --output markdown \
  --report-path results/report.md

# Validate config without running
cabal run gauntlet-exe -- validate --config examples/advanced-config.json
```

## Tips

1. **Start Small**: Begin with `minimal.json` and gradually add features
2. **Use Warmup**: Add 5-10 warmup iterations for consistent results
3. **Tune Concurrency**: Match your target service's capacity
4. **Enable Retries**: Use retry settings for flaky networks
5. **Log Levels**: Use `"info"` for normal runs, `"debug"` for troubleshooting
6. **Secrets Management**: Never commit `.secrets/` directory - add to `.gitignore`

## See Also

- [CLAUDE.md](../CLAUDE.md) - Architecture and development guide
