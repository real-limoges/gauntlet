# Gauntlet User Guide

## What Gauntlet Does

Gauntlet is an HTTP performance benchmarking tool. It sends real HTTP requests to your services, measures latency at nanosecond precision, and uses Bayesian statistics to give you direct probability answers like "there is a 94% chance the candidate is faster." No more squinting at averages and hoping the difference is real.

The single `benchmark` command handles any number of targets: one target with optional baseline comparison, two targets with A/B analysis, or 2+ targets with all-pairs statistical comparison. The mode is determined automatically from the config.

## Quick Start

Create a config file (`config.json`):

```json
{
  "targets": {
    "primary": "http://api.example.com",
    "candidate": "http://api-new.example.com"
  },
  "settings": {
    "iterations": 100,
    "concurrency": 10,
    "secrets": ".secrets/token.txt"
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

Then run:

```bash
gauntlet benchmark --config config.json
```

That's it. The rest of this guide explains every config option and what the output means.

## Understanding the Config File

The config has three top-level keys: `targets`, `settings`, and `payloads`.

### Environment Variable Expansion

Any config value can contain `${VAR}` references. Variables are expanded in the raw config text before JSON parsing.

**Resolution order** (highest priority first):
1. `.env.local` — gitignored, for local overrides and secrets
2. `.env` — can be committed for non-secret defaults
3. Process environment

Missing variables fail fast with a clear error naming the undefined variable.

```json
{
  "targets": {
    "primary": "${PRIMARY_URL}",
    "candidate": "${CANDIDATE_URL}"
  },
  "settings": {
    "secrets": "${TOKEN_PATH}",
    "iterations": 1000,
    "concurrency": 10
  }
}
```

This keeps secrets and environment-specific URLs out of committed config files while allowing the same config to work across dev, staging, and CI.

### `targets` — What you're testing

The structure depends on which mode you're using.

**A/B mode** (`benchmark` with two targets):

```json
"targets": {
  "primary": "http://api-v1.example.com",
  "candidate": "http://api-v2.example.com"
}
```

The "primary" is your baseline — production, the current version, whatever you consider the reference. The "candidate" is what you're evaluating. This asymmetry matters because statistical results are framed as "probability candidate is faster than primary."

**Multi-target mode** (`benchmark` with 2+ targets):

```json
"targets": [
  { "name": "prod", "url": "http://prod.example.com:8080" },
  { "name": "staging", "url": "http://staging.example.com:8080" },
  { "name": "dev", "url": "http://dev.example.com:8080" }
]
```

An array of named targets. At least 2 required. Every pair is compared, so with 3 targets you get 3 pairwise comparisons; with 4 you get 6.

Why `name`? Reports rank targets by mean latency and show pairwise comparisons — names like "prod" and "staging" make this readable instead of showing raw URLs.

### `settings` — How the benchmark runs

#### `iterations` (required)

Number of requests per endpoint. More iterations produce tighter credible intervals on your statistical comparisons. 100 is fine for a quick sanity check; 1,000-10,000 for production decisions where you need high confidence.

#### `concurrency` (required)

Maximum simultaneous in-flight requests. This simulates realistic load. Too low and you're sending serial requests that don't reflect production traffic patterns. Too high and you're benchmarking your client machine's limits (CPU, open file descriptors), not the server's performance.

#### `secrets` (optional)

Path to a file containing a Bearer token. When set, the token is read once at startup and injected as `Authorization: Bearer <token>` on every request. Omit this field entirely for public or internal APIs that don't require authentication. When present, keep the secrets file in your `.gitignore`.

#### `requestTimeout` (default: 30s)

Per-request timeout in seconds. Prevents hung requests from stalling the entire benchmark. If your endpoints are legitimately slow (e.g., complex analytics queries), increase this.

#### `maxConnections`

HTTP connection pool size cap. Gauntlet reuses connections via HTTP keep-alive for realistic performance. Defaults are usually fine unless your concurrency is very high, in which case you may want `maxConnections` >= `concurrency`.

#### `logLevel` (default: `"info"`)

Verbosity: `"debug"`, `"info"`, `"warning"`, `"error"`. Use `"debug"` when troubleshooting config or connectivity issues — it logs individual request details. Use `"warning"` or `"error"` in CI where you only want to see problems.

#### `warmup`

```json
"warmup": {
  "warmupIterations": 10
}
```

Sends N requests before timing begins. This lets the server warm up JIT compilers, fill caches, and establish connection pools so your measurements reflect steady-state performance, not cold-start latency. Without warmup, the first few requests are often significantly slower, skewing your statistics.

#### `retry`

```json
"retry": {
  "retryMaxAttempts": 3,
  "retryInitialDelayMs": 1000,
  "retryBackoffMultiplier": 2.0
}
```

Retries transient failures with exponential backoff (1s, 2s, 4s with the defaults above). Prevents a single network hiccup from registering as a failed request. Keep attempts low — excessive retries mask real problems. If you're seeing many retries, the service likely has a genuine issue.

#### `loadMode`

Controls the rate at which requests are sent. Defaults to `unthrottled` (as fast as concurrency allows).

**Unthrottled** (default) — no rate limiting, requests sent as fast as possible:
```json
"loadMode": {"mode": "unthrottled"}
```

**Constant RPM** — steady request rate:
```json
"loadMode": {"mode": "constantRpm", "targetRpm": 100}
```

**Ramp Up** — linearly increasing rate over a duration:
```json
"loadMode": {"mode": "rampUp", "startRpm": 10, "endRpm": 200, "durationSecs": 60}
```

**Step Load** — discrete rate steps:
```json
"loadMode": {
  "mode": "stepLoad",
  "steps": [
    {"rpm": 50, "durationSecs": 30},
    {"rpm": 100, "durationSecs": 30},
    {"rpm": 200, "durationSecs": 30}
  ]
}
```

**Poisson RPM** — random inter-arrival times following a Poisson process (more realistic traffic simulation):
```json
"loadMode": {"mode": "poissonRpm", "targetRpm": 100}
```

When using `constantRpm`, `rampUp`, `stepLoad`, or `poissonRpm`, the `iterations` setting controls how many total requests are sent — the benchmark ends when either the iteration count is reached or the load duration expires, whichever comes first.

#### `healthCheckPath` / `healthCheckTimeout`

After environment setup (git switch + docker-compose, if configured), gauntlet polls `<target-url><healthCheckPath>` every second until it gets HTTP 200, up to `healthCheckTimeout` seconds. This ensures the service is fully ready before benchmarking begins, so you don't measure startup time as request latency.

Defaults: `healthCheckPath` = `"/health"`, `healthCheckTimeout` = `60` seconds.

#### `tempo`

```json
"tempo": {
  "tempoUrl": "http://tempo:3200",
  "tempoServiceName": "my-service",
  "tempoEnabled": true,
  "tempoAuthToken": "optional-tempo-bearer-token"
}
```

Optional Grafana Tempo integration. When enabled, gauntlet fetches distributed traces from the benchmark time window after benchmarking completes and aggregates span-level statistics. See [Distributed Tracing](#distributed-tracing-grafana-tempo) for details.

### `payloads` — What requests to send

Each entry defines one HTTP endpoint to benchmark. All payloads are sent to every target.

```json
"payloads": [
  {
    "name": "search-products",
    "method": "POST",
    "path": "/api/search",
    "body": {
      "query": "laptop",
      "filters": { "priceMin": 500, "priceMax": 2000 },
      "limit": 50
    },
    "headers": {
      "X-Request-Source": "benchmark"
    },
    "validate": {
      "status": 200,
      "fields": {
        "$.results": { "present": true }
      }
    }
  }
]
```

| Field | Description |
|---|---|
| `name` | Appears in reports and CSV output. Make it descriptive. |
| `method` | `GET`, `POST`, `PUT`, `DELETE`, `PATCH` |
| `path` | Appended to target URL. Can include query strings (e.g., `/api/users?limit=10`). |
| `body` | JSON request body. Used with `POST`, `PUT`, `PATCH`. |
| `headers` | Custom HTTP headers as key-value pairs. `Content-Type: application/json` is injected automatically unless you override it. |
| `validate` | Optional per-response validation (see below). |

### Validation

The `validate` block lets you assert properties of every response during the benchmark, not just at the end. This catches cases where the server returns HTTP 200 but with wrong data — benchmarking a broken endpoint is worse than useless because it gives you confidence in numbers that don't reflect real behavior.

```json
"validate": {
  "status": 201,
  "fields": {
    "$.user.id":    { "present": true },
    "$.user.email": { "eq": "john@example.com" },
    "$.score":      { "range": { "min": 0, "max": 100 } },
    "$.tags":       { "arrayLength": 3 }
  }
}
```

Field paths use dot-notation with `$.` prefix (e.g., `$.user.email` navigates into a nested `user` object). Array indices are also supported (e.g., `$.items.0.name`).

#### Assertion types

| Assertion | Example | Meaning |
|---|---|---|
| `present` | `{ "present": true }` | Field exists (any value, including null) |
| `eq` | `{ "eq": "value" }` | Exact value match (strings, numbers, booleans) |
| `null` | `{ "null": true }` | Field is explicitly `null` |
| `notNull` | `{ "notNull": true }` | Field exists and is not `null` |
| `type` | `{ "type": "string" }` | JSON type: `"string"`, `"number"`, `"boolean"`, `"array"`, `"object"`, `"null"` |
| `matches` | `{ "matches": "^[A-Z]{3}$" }` | String matches regex (POSIX ERE) |
| `range` | `{ "range": { "min": 0, "max": 100 } }` | Numeric value within bounds (both optional) |
| `arrayLength` | `{ "arrayLength": 5 }` | Array has exactly N elements |
| `arrayContains` | `{ "arrayContains": "admin" }` | Value is present in the array |

## Understanding the Output

### Per-target statistics

For each target and payload, gauntlet reports:

| Metric | What it tells you |
|---|---|
| **Mean** | Average latency. The headline number, but can be misleading if the distribution is skewed. |
| **StdDev** | How spread out the latencies are. High stddev = inconsistent performance. |
| **p50** (median) | The "typical" request. Half are faster, half are slower. More robust than mean for skewed distributions. |
| **p95** | 95th percentile. 1 in 20 requests is slower than this. |
| **p99** | 99th percentile. 1 in 100 requests is slower than this. |
| **ES** (Expected Shortfall) | The mean of the worst 1% of requests. p99 tells you "1% of requests are slower than X." ES tells you "when requests are in that worst 1%, how slow are they on average?" This catches long-tail catastrophes that p99 hides — a p99 of 50ms could have an ES of 200ms if the worst requests are extremely slow. |
| **Min / Max** | Fastest and slowest observed request. |
| **Success / Total** | Request success rate. Failed requests are excluded from latency statistics. |

### Bayesian comparison

For each pair of targets, gauntlet reports:

**"Probability Candidate is Faster (means)"** — This is the headline number. It's P(population mean of B < population mean of A), computed from the sampling distributions using a conjugate normal model. It answers: "if we ran this benchmark again with fresh samples, how confident are we that B's average would still be lower?"

**"Probability Single Request Faster"** — P(a random individual request to B is faster than a random individual request to A). This is always closer to 50% than the means comparison because individual requests have high variance.

Why report both? The first tells you about the *system* — is one version fundamentally faster? The second tells you about the *user experience* — for any given user request, what's the chance they'd actually notice a difference? A service can have a clearly faster mean (95% probability) but high variance, meaning individual users still often get slow responses (60% probability).

**Mean Difference + 95% Credible Interval** — The estimated difference in average latency with a Bayesian credible interval. Unlike a frequentist confidence interval, this directly means "there is a 95% probability the true difference lies in this range."

**Cohen's d** — Effect size. Measures the difference in means relative to pooled standard deviation. Small (<0.2), medium (0.2-0.8), large (>0.8). Useful for judging practical significance — a statistically clear difference can still be too small to matter.

**Tail analysis** — Comparison of p95 and p99 percentiles between targets, with Maritz-Jarrett standard errors. This tells you whether the tail behavior differs, not just the averages.

### Ranking table

In multi-target mode, targets are ranked by mean latency with pairwise comparisons for every pair. This gives you a quick leaderboard plus detailed statistical evidence for each ranking.

### CSV output

Raw latency data is written to `results/latencies-<timestamp>.csv` with one row per request and nanosecond precision. Use this for post-hoc analysis, custom visualizations, or feeding into other tools.

### Markdown reports

```bash
gauntlet benchmark --config config.json --output markdown --report-path results/report.md
```

Produces the same content as terminal output but in markdown, suitable for CI artifacts, PR comments, or archival.

## Baselines and Regression Detection

Baselines let you save benchmark results and compare future runs against them.

```bash
# Save current results as a named baseline
gauntlet benchmark --config config.json --save-baseline v1.0

# Compare a future run against the saved baseline
gauntlet benchmark --config config.json --compare-baseline v1.0

# Both at once: save as v1.1, compare against v1.0
gauntlet benchmark --config config.json --save-baseline v1.1 --compare-baseline v1.0
```

Baselines are stored as JSON files in `baselines/<name>.json`. When comparing, gauntlet checks mean, p50, p95, and p99 against regression thresholds:

| Metric | Default Threshold |
|---|---|
| Mean | 10% |
| p50 | 10% |
| p95 | 10% |
| p99 | 15% |

If any metric exceeds its threshold, the tool exits with code 1.

Why is the p99 threshold higher? Tail latencies are inherently noisier — small sample variations cause larger swings in p99. A 10% threshold on p99 would produce too many false positives in CI, causing alert fatigue without actionable signal.

## Git and Docker Integration

When `git` is configured, gauntlet automates the deploy-benchmark-compare workflow:

```json
{
  "git": {
    "primary": "main",
    "candidate": "feature/optimization"
  }
}
```

The sequence is:

1. Switch to the candidate branch (`git switch feature/optimization`)
2. Run `docker-compose up` (if configured)
3. Poll the health endpoint until the service is ready
4. Run the benchmark against the candidate
5. Switch to the primary branch (`git switch main`)
6. Repeat steps 2-4 for the primary
7. Compare results

This eliminates the manual process of deploying each version separately. The health check polling ensures you're not benchmarking a service that's still starting up.

In multi-target mode, an optional `branch` field per target triggers a git switch for that target's benchmark phase.

## CI/CD Integration

Gauntlet auto-detects CI environments via the `GITLAB_CI` and `GITHUB_ACTIONS` environment variables.

**GitLab CI**: Outputs collapsible CI sections with ANSI-colored regression status, making results easy to scan in job logs.

**GitHub Actions**: Produces a plain-text regression summary and writes to `$GITHUB_STEP_SUMMARY` for the job summary UI.

Exit code 1 on regression naturally fails CI pipelines. Combine with `--output markdown` to produce artifact reports that can be attached to merge/pull requests.

Example CI usage:

```yaml
# GitHub Actions
- name: Run performance benchmark
  run: gauntlet benchmark --config bench.json --compare-baseline main --output markdown --report-path results/report.md

- name: Upload report
  if: always()
  uses: actions/upload-artifact@v4
  with:
    name: benchmark-report
    path: results/report.md
```

## Distributed Tracing (Grafana Tempo)

When `tempo` is configured in settings, gauntlet fetches distributed traces from the benchmark time window after benchmarking completes. It queries Grafana Tempo using TraceQL, retrieves traces that were generated during the benchmark, and aggregates span-level statistics.

The trace report shows per-span-name statistics: count, mean duration, p50, p95, and p99. This helps you identify which internal service or operation is the bottleneck — for example, you might see that your API endpoint is slow because a downstream database query has high p99 latency.

```json
"tempo": {
  "tempoUrl": "http://tempo:3200",
  "tempoServiceName": "my-service",
  "tempoEnabled": true,
  "tempoAuthToken": "optional-tempo-bearer-token"
}
```

## Example Configs

### Minimal

The simplest possible config — two targets, 100 requests, 10 concurrent:

```json
{
  "targets": {
    "primary": "http://api.example.com",
    "candidate": "http://api-new.example.com"
  },
  "settings": {
    "iterations": 100,
    "concurrency": 10,
    "secrets": ".secrets/token.txt"
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

### Multi-target comparison

Compare three environments against each other:

```json
{
  "targets": [
    { "name": "prod", "url": "http://prod.example.com:8080" },
    { "name": "staging", "url": "http://staging.example.com:8080" },
    { "name": "dev", "url": "http://dev.example.com:8080" }
  ],
  "settings": {
    "iterations": 100,
    "concurrency": 4,
    "secrets": "secrets.txt"
  },
  "payloads": [
    {
      "name": "get-users",
      "method": "GET",
      "path": "/api/users"
    },
    {
      "name": "create-user",
      "method": "POST",
      "path": "/api/users",
      "body": { "name": "test", "email": "test@example.com" }
    }
  ]
}
```

### A/B comparison with warmup and retry

Production-grade config with warmup and retry:

```json
{
  "targets": {
    "primary": "http://api-v1.example.com",
    "candidate": "http://api-v2.example.com"
  },
  "settings": {
    "iterations": 5000,
    "concurrency": 50,
    "secrets": ".secrets/token.txt",
    "requestTimeout": 60,
    "logLevel": "info",
    "warmup": {
      "warmupIterations": 10
    },
    "retry": {
      "retryMaxAttempts": 3,
      "retryInitialDelayMs": 1000,
      "retryBackoffMultiplier": 2.0
    }
  },
  "payloads": [
    {
      "name": "search-products",
      "method": "POST",
      "path": "/api/search",
      "body": {
        "query": "laptop",
        "filters": {
          "priceMin": 500,
          "priceMax": 2000,
          "category": "electronics"
        },
        "limit": 50
      }
    },
    {
      "name": "get-recommendations",
      "method": "GET",
      "path": "/api/recommendations?userId=12345&limit=10"
    }
  ]
}
```

### Advanced config (all features)

Everything enabled — high iteration count, connection pool tuning, debug logging, distributed tracing:

```json
{
  "targets": {
    "primary": "http://primary-service.internal:8080",
    "candidate": "http://candidate-service.internal:8080"
  },
  "settings": {
    "iterations": 10000,
    "concurrency": 100,
    "secrets": ".secrets/service-token.txt",
    "maxConnections": 200,
    "requestTimeout": 120,
    "logLevel": "debug",
    "warmup": {
      "warmupIterations": 20
    },
    "retry": {
      "retryMaxAttempts": 10,
      "retryInitialDelayMs": 2000,
      "retryBackoffMultiplier": 2.5
    },
    "tempo": {
      "tempoUrl": "http://tempo:3200",
      "tempoServiceName": "my-service",
      "tempoEnabled": true,
      "tempoAuthToken": "optional-tempo-bearer-token"
    },
    "loadMode": {
      "mode": "constantRpm",
      "targetRpm": 100
    }
  },
  "payloads": [
    {
      "name": "complex-query",
      "method": "POST",
      "path": "/api/v3/analytics/query",
      "headers": {
        "X-Trace-ID": "benchmark-trace",
        "X-Priority": "high",
        "Content-Type": "application/json",
        "Accept": "application/json"
      },
      "body": {
        "timeRange": {
          "start": "2026-02-01T00:00:00Z",
          "end": "2026-02-16T23:59:59Z"
        },
        "aggregations": [
          { "field": "revenue", "function": "sum" },
          { "field": "orders", "function": "count" }
        ],
        "groupBy": ["region", "category"],
        "filters": {
          "status": "completed",
          "paymentMethod": ["credit_card", "paypal"]
        }
      }
    },
    {
      "name": "batch-operation",
      "method": "PUT",
      "path": "/api/v3/batch/update",
      "headers": {
        "X-Batch-Size": "1000",
        "X-Idempotency-Key": "benchmark-batch-001"
      },
      "body": {
        "operations": [
          { "id": 1, "action": "update", "data": { "status": "processed" } },
          { "id": 2, "action": "update", "data": { "status": "processed" } },
          { "id": 3, "action": "delete" }
        ]
      }
    }
  ]
}
```

## Exit Codes

| Code | Meaning |
|---|---|
| 0 | Success — benchmark completed, no regressions detected |
| 1 | Regression detected (baseline comparison) |
| 2 | Error — bad config, environment setup failure, connectivity issue, etc. |
