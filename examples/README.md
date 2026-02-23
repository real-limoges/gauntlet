# Example Configurations

This directory contains sample configuration files demonstrating various features of gauntlet.

## Quick Start

```bash
# Create a secrets file
mkdir -p .secrets
echo "your-bearer-token-here" > .secrets/token.txt

# Run a simple benchmark
cabal run gauntlet-exe -- benchmark-multiple --config examples/simple-benchmark.json
```

## Example Files

### `minimal.json`
**Minimal configuration** - The absolute minimum required fields.
- 100 iterations, 10 concurrent requests
- Single health check endpoint
- No optional features

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
- Search and recommendation endpoints

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
- Complex POST bodies with nested JSON
- Connection pooling tuning

**Use case:** Production-grade benchmarking with distributed tracing.

### `log-levels.json`
**Log verbosity examples** - Demonstrates different log levels.

Change `logLevel` to control output verbosity:
- `"debug"` - Verbose output including request/response details
- `"info"` - Normal output with progress updates (default)
- `"warning"` - Only warnings and errors
- `"error"` - Only critical errors

## Configuration Reference

### Required Fields

```json
{
  "targets": {
    "primary": "http://api-v1.example.com",
    "candidate": "http://api-v2.example.com"
  },
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

### Optional Settings

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `maxConnections` | int | 10 | HTTP connection pool size |
| `connIdleTimeout` | int | 30 | Connection idle timeout (seconds) |
| `requestTimeout` | int | 30 | Request timeout (seconds) |
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

### Single Benchmark
Test one endpoint at a time:

```bash
cabal run gauntlet-exe -- benchmark-multiple --config examples/minimal.json
```

### A/B Comparison
Compare primary vs. candidate:

```bash
cabal run gauntlet-exe -- benchmark-multiple --config examples/ab-comparison.json
```

### With Baseline Comparison
Save a baseline for regression detection:

```bash
# Save baseline
cabal run gauntlet-exe -- benchmark-multiple \
  --config examples/simple-benchmark.json \
  --save-baseline my-baseline

# Compare against baseline
cabal run gauntlet-exe -- benchmark-multiple \
  --config examples/simple-benchmark.json \
  --compare-baseline my-baseline
```

### Markdown Report
Write a full markdown report (stats, Bayesian analysis, validation) in addition to terminal output:

```bash
cabal run gauntlet-exe -- benchmark-multiple \
  --config examples/simple-benchmark.json \
  --output markdown \
  --report-path results/report.md
```

## Tips

1. **Start Small**: Begin with `minimal.json` and gradually add features
2. **Use Warmup**: Add 5-10 warmup iterations for consistent results
3. **Tune Concurrency**: Match your target service's capacity
4. **Enable Retries**: Use retry settings for flaky networks
5. **Log Levels**: Use `"info"` for normal runs, `"debug"` for troubleshooting
6. **Secrets Management**: Never commit `.secrets/` directory - add to `.gitignore`

## See Also

- [LOGGING.md](../docs/LOGGING.md) - Structured logging documentation
- [CLAUDE.md](../CLAUDE.md) - Architecture and development guide
- [TODO.md](../docs/TODO.md) - Roadmap and planned features
