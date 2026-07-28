# Gauntlet User Guide

## What Gauntlet Does

Gauntlet is an HTTP performance benchmarking tool. It sends real HTTP requests to your services, measures latency at nanosecond precision on a monotonic clock, and uses Bayesian statistics to give you direct probability answers like "there is a 94% chance the candidate is faster." No more squinting at averages and hoping the difference is real.

The single `benchmark` command handles any number of targets: one target with optional baseline comparison, two targets with A/B analysis, or 2+ targets with all-pairs statistical comparison. The mode is determined automatically from the config.

## Quick Start

Create a config file (`config.json`):

```json
{
  "targets": [
    { "name": "primary", "url": "http://api.example.com" },
    { "name": "candidate", "url": "http://api-new.example.com" }
  ],
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

(From a source checkout: `cargo run -p gauntlet-cli --bin gauntlet -- benchmark --config config.json`, or `cargo build --release` once and use `./target/release/gauntlet`.)

That's it. The rest of this guide explains every config option and what the output means.

## Understanding the Config File

The config has three top-level keys: `targets`, `settings`, and `payloads`. All three are required.

Field names are **snake_case** and map 1:1 onto the tool's internal types — there is no renaming layer, which is why `gauntlet schema` can derive the JSON schema straight from the types and can never drift from what the loader accepts:

```bash
gauntlet schema              # print to stdout
gauntlet schema --out schema/config-schema.json
```

Unknown keys are an error, not a silent no-op. A typo like `"iteration": 100` fails at load with the offending key named, rather than quietly running with the default.

To check a config without sending any traffic:

```bash
gauntlet validate --config config.json
gauntlet validate --config config.json --check-endpoints   # also GETs each target's health URL
```

`validate` parses, expands `${VAR}` references, runs every validation rule, and prints a summary of what would run. With `--check-endpoints` it additionally issues one GET per target — the configured `lifecycle.health_check.url`, or `<url>/health` if none is set — and exits 2 if any target is unreachable.

Validation accumulates: every problem in the file is reported in one pass, not just the first.

### Environment Variable Expansion

Any config value can contain `${VAR}` references. Variables are expanded in the raw config text before JSON parsing.

**Resolution order** (highest priority first):
1. `.env.local` — gitignored, for local overrides and secrets
2. `.env` — can be committed for non-secret defaults
3. Process environment

Missing variables fail fast with a clear error naming the undefined variable. Only the bare `${VAR}` form is supported — there is no `${VAR:-default}`. An unclosed `${` is left as a literal.

```json
{
  "targets": [
    { "name": "primary", "url": "${PRIMARY_URL}" },
    { "name": "candidate", "url": "${CANDIDATE_URL}" }
  ],
  "settings": {
    "secrets": "${TOKEN_PATH}",
    "iterations": 1000,
    "concurrency": 10
  }
}
```

This keeps secrets and environment-specific URLs out of committed config files while allowing the same config to work across dev, staging, and CI.

### `targets` — What you're testing

An array of named targets. One is enough; two or more get automatic pairwise comparison.

```json
"targets": [
  { "name": "prod", "url": "http://prod.example.com:8080" },
  { "name": "staging", "url": "http://staging.example.com:8080" },
  { "name": "dev", "url": "http://dev.example.com:8080" }
]
```

Every pair is compared, so with 3 targets you get 3 pairwise comparisons; with 4 you get 6. Targets run **sequentially** — they are not benchmarked at the same time, so they do not contend for the client machine during measurement.

Why `name`? Reports rank targets by mean latency and show pairwise comparisons — names like "prod" and "staging" make this readable instead of showing raw URLs. The name is also what qualifies a baseline and what slugs a chart's filename.

Comparisons are framed as "probability B is faster than A", where A and B are two targets in config order. If you are evaluating a change, put the reference (production, the current version) first and the candidate second.

| Field | Type | Required | Description |
|---|---|---|---|
| `name` | string | yes | Label used in reports, baselines, and chart filenames |
| `url` | string | yes | Base URL; payload `path`s are appended verbatim |
| `lifecycle` | object | no | Setup/teardown hooks and health-check polling |

> **`branch` was removed.** Earlier versions accepted a `branch` field on each target. Nothing ever acted on it: the Haskell build used it only to decide whether to print a "Setting up..." message, and the Rust port never read it at all. Because the config rejects unknown fields, a config that still sets `branch` now fails to load with a clear parse error. Move the switch into `lifecycle.setup` (`"cmd": "git switch my-branch && ..."`), which actually runs, and which the `lifecycle` section below documents.

### `settings` — How the benchmark runs

#### `iterations` (required)

Number of requests per endpoint. Must be at least 1. More iterations produce tighter credible intervals on your statistical comparisons. 100 is fine for a quick sanity check; 1,000-10,000 for production decisions where you need high confidence.

Duration-based load modes (`ramp_up`, `step_load`) derive their own request count from the schedule and ignore this field.

#### `concurrency` (required)

Maximum simultaneous in-flight requests. This simulates realistic load. Too low and you're sending serial requests that don't reflect production traffic patterns. Too high and you're benchmarking your client machine's limits (CPU, open file descriptors), not the server's performance.

#### `secrets` (optional)

Path to a file containing a bearer token. When set, the file is read once at startup and the token is sent as `Authorization: Bearer <token>` on every request that does not already carry its own `Authorization` header. Surrounding whitespace is trimmed, because token files routinely end in a newline and sending that as part of the credential is a maddening bug. An empty or whitespace-only file means no auth rather than an empty credential.

Omit this field entirely for public or internal APIs that don't require authentication. When present, keep the secrets file in your `.gitignore` — the point of the indirection is that the token never lands in a committed JSON file.

#### `request_timeout_secs` (default: 30)

Per-request timeout in seconds. Prevents hung requests from stalling the entire benchmark. If your endpoints are legitimately slow (e.g., complex analytics queries), increase this.

#### `max_connections` (optional)

HTTP connection pool size cap. Gauntlet reuses connections via HTTP keep-alive for realistic performance. The default is usually fine unless your concurrency is very high, in which case you may want `max_connections` >= `concurrency`.

#### `log_level` (default: `"info"`)

Accepted values: `"debug"`, `"info"`, `"warning"`, `"error"`.

Sets the threshold for gauntlet's own diagnostics. Note the deliberate split: the level filters *diagnostic* output on stderr and never suppresses the benchmark report on stdout, so `"error"` gives you a quiet run that still prints its results in full. There is no command-line override; the config field is the only control.

#### `warmup` (default: `{"iterations": 1}`)

```json
"warmup": {
  "iterations": 10
}
```

Sends N requests against the target's **first** endpoint before timing begins, and discards them. This lets the server warm up JIT compilers, fill caches, and establish connection pools so your measurements reflect steady-state performance, not cold-start latency. Without warmup, the first few requests are often significantly slower, skewing your statistics. Set `0` to disable.

#### `retry`

```json
"retry": {
  "max_attempts": 3,
  "initial_delay_ms": 1000,
  "backoff_multiplier": 2.0
}
```

Retries **transport** failures — connection refused, DNS failure, timeout — with exponential backoff (1s, 2s, 4s with the defaults above). `max_attempts` counts retries, so `0` disables them; `backoff_multiplier` must be at least 1.0.

An HTTP status code is a *response*, not a transport failure: a 500 is recorded as a 500 and never retried. Retrying it would quietly turn a broken service into a slow-looking healthy one.

A request that exhausts its retries is recorded as a failure (status 0) and excluded from the latency statistics, but still counted in the success/total ratio and still charted by the error-rate and status charts.

Keep attempts low — excessive retries mask real problems. If you're seeing many retries, the service likely has a genuine issue.

#### `load_mode` (default: `unthrottled`)

Controls the rate at which requests are dispatched. The limiter paces request *starts*; `concurrency` independently caps how many are in flight, so the two compose without double-counting.

The rate is **per target, not per payload**. All of a target's payloads share one limiter, so `target_rpm` is the load the target actually receives however many payloads you configure. A run with three payloads at `target_rpm: 6000` sends 6,000 requests per minute in total (roughly 2,000 per payload), and takes about three times as long as the same rate with one payload. `concurrency`, by contrast, is per endpoint.

**Unthrottled** (default) — no rate limiting, requests sent as fast as concurrency allows:
```json
"load_mode": {"mode": "unthrottled"}
```

**Constant RPM** — steady request rate:
```json
"load_mode": {"mode": "constant_rpm", "target_rpm": 6000}
```

**Ramp up** — linearly increasing rate over a duration:
```json
"load_mode": {"mode": "ramp_up", "start_rpm": 60, "end_rpm": 6000, "duration_secs": 60}
```

**Step load** — discrete rate steps:
```json
"load_mode": {
  "mode": "step_load",
  "steps": [
    {"rpm": 600, "duration_secs": 30},
    {"rpm": 3000, "duration_secs": 60},
    {"rpm": 6000, "duration_secs": 60}
  ]
}
```

**Poisson RPM** — random inter-arrival times following a Poisson process, which is a more realistic model of organic traffic than a metronome:
```json
"load_mode": {"mode": "poisson_rpm", "target_rpm": 6000}
```

All rates and durations must be greater than zero. Time-varying modes floor at 6 RPM, so a ramp that starts near zero still makes progress instead of stalling.

`unthrottled`, `constant_rpm`, and `poisson_rpm` are **iteration-based**: they send `settings.iterations` requests per endpoint. `ramp_up` and `step_load` are **duration-based**: the run ends when the schedule does, and `iterations` is ignored.

#### `tempo` (optional)

```json
"tempo": {
  "url": "http://tempo:3200",
  "service_name": "my-service",
  "enabled": true,
  "auth_token": "optional-tempo-bearer-token"
}
```

Optional Grafana Tempo integration. `enabled` defaults to true when the section is present. See [Distributed Tracing](#distributed-tracing-grafana-tempo).

### `lifecycle` — Preparing a target

Per-target hooks, run around that target's benchmark phase:

```json
{
  "name": "candidate",
  "url": "http://localhost:8080",
  "lifecycle": {
    "setup": {
      "cmd": "git switch feat/candidate && docker-compose up -d --build",
      "timeout_secs": 120,
      "working_dir": "../service"
    },
    "teardown": { "cmd": "docker-compose down" },
    "health_check": {
      "url": "http://localhost:8080/health",
      "timeout_secs": 60,
      "interval_ms": 1000
    }
  }
}
```

The sequence per target is: **setup → health check → warmup → benchmark → teardown.**

- `setup` / `teardown` run through `sh -c`, so shell syntax works. `working_dir` is optional; `timeout_secs` defaults to 30. A non-zero exit or a timeout in `setup` aborts before any request is sent — benchmarking a service that failed to come up produces numbers that mean nothing. `teardown` is best-effort.
- `health_check` polls `url` until it answers 200. `timeout_secs` defaults to 30, `interval_ms` to 500. This is what stops you from measuring startup time as request latency.

This is also where the deploy-benchmark-compare workflow lives: give each target a `setup` that switches branch and brings the service up, and gauntlet will do the whole sequence for every target in turn.

### `payloads` — What requests to send

Each entry defines one HTTP endpoint to benchmark. **All payloads are sent to every target**, which is what makes the comparison apples-to-apples. Within a target, the endpoints run concurrently, each with its own concurrency semaphore; they share the target's single load limiter, so `load_mode` bounds the aggregate rate rather than each payload's.

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
        "$.results": "present"
      }
    }
  }
]
```

| Field | Description |
|---|---|
| `name` | Appears in reports and CSV output. Make it descriptive. |
| `method` | `GET`, `POST`, `PUT`, `DELETE`, `PATCH` (uppercase; anything else fails to parse) |
| `path` | Appended to the target URL verbatim. Can include query strings (e.g., `/api/users?limit=10`). |
| `body` | JSON request body. Used with `POST`, `PUT`, `PATCH`. |
| `headers` | Custom HTTP headers as key-value pairs. `Content-Type: application/json` is injected automatically unless you set your own. |
| `validate` | Optional per-response validation (see below). |

### Validation

The `validate` block lets you assert properties of every response during the benchmark, not just at the end. This catches cases where the server returns HTTP 200 but with wrong data — benchmarking a broken endpoint is worse than useless because it gives you confidence in numbers that don't reflect real behavior.

```json
"validate": {
  "status": 201,
  "fields": {
    "$.user.id":    "present",
    "$.user.email": { "eq": "john@example.com" },
    "$.score":      { "range": { "min": 0, "max": 100 } },
    "$.tags":       { "array_length": 3 }
  }
}
```

Field paths use dot-notation with a `$.` prefix (e.g., `$.user.email` navigates into a nested `user` object). Array indices are also supported (e.g., `$.items.0.name`).

#### Assertion types

Assertions that carry no data are written as bare strings; the rest are single-key objects.

| Assertion | Example | Meaning |
|---|---|---|
| `present` | `"present"` | Field exists (any value, including null) |
| `null` | `"null"` | Field is explicitly `null` |
| `not_null` | `"not_null"` | Field exists and is not `null` |
| `eq` | `{ "eq": "value" }` | Exact value match (strings, numbers, booleans) |
| `type` | `{ "type": "string" }` | JSON type: `"string"`, `"number"`, `"boolean"`, `"array"`, `"object"`, `"null"` |
| `matches` | `{ "matches": "^[A-Z]{3}$" }` | String matches a regex (Rust `regex` crate syntax) |
| `range` | `{ "range": { "min": 0, "max": 100 } }` | Numeric value within bounds (both optional) |
| `array_length` | `{ "array_length": 5 }` | Array has exactly N elements |
| `array_contains` | `{ "array_contains": "admin" }` | Value is present in the array |

Validation failures are counted per endpoint and surfaced in the reports. Retained detail is capped at 50 failing responses per endpoint (and 10 unique messages when printed), so a wholly-broken run produces a readable summary instead of megabytes of identical errors.

## Understanding the Output

### Per-target statistics

For each target, gauntlet reports:

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

Latency is measured on a monotonic clock, spanning all retry attempts of a request. Absolute numbers include the HTTP client's own overhead floor (on the order of tens of microseconds); it cancels out of A/B comparisons and regression deltas, but keep it in mind before quoting an absolute single-request figure for a very fast endpoint.

### Bayesian comparison

For each pair of targets, gauntlet reports:

**"Probability B is faster (means)"** — This is the headline number. It's P(population mean of B < population mean of A), computed from the sampling distributions using a conjugate normal model. It answers: "if we ran this benchmark again with fresh samples, how confident are we that B's average would still be lower?"

**"Probability single request faster"** — P(a random individual request to B is faster than a random individual request to A). This is always closer to 50% than the means comparison because individual requests have high variance.

Why report both? The first tells you about the *system* — is one version fundamentally faster? The second tells you about the *user experience* — for any given user request, what's the chance they'd actually notice a difference? A service can have a clearly faster mean (95% probability) but high variance, meaning individual users still often get slow responses (60% probability).

**"Probability B less jittery"** — P(sigma_B < sigma_A), via a log-variance approximation. Consistency is its own quality; a service with the same mean and half the spread is a better service.

**Mean difference + 95% credible interval** — The estimated difference in average latency with a Bayesian credible interval. Unlike a frequentist confidence interval, this directly means "there is a 95% probability the true difference lies in this range."

**Cohen's d** — Effect size. Measures the difference in means relative to pooled standard deviation. Small (<0.2), medium (0.2-0.8), large (>0.8). Useful for judging practical significance — a statistically clear difference can still be too small to matter.

**Tail analysis** — Comparison of p95 and p99 between targets, with Maritz-Jarrett standard errors. This tells you whether the tail behavior differs, not just the averages.

**Earth Mover's Distance** — The 1-Wasserstein distance between the two latency distributions: how much probability mass has to move, and how far, to turn one distribution into the other. It is the one figure computed from the raw sample vectors rather than summary statistics, which is why `gauntlet compare` (which reads saved summaries) cannot report it.

### Ranking table

With more than one target, targets are ranked by mean latency with pairwise comparisons for every pair. This gives you a quick leaderboard plus detailed statistical evidence for each ranking.

### CSV output

Raw latency data is written to `<results-dir>/latencies-<timestamp>.csv`, one row per request:

```
target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso
```

Use this for post-hoc analysis, custom visualizations, or feeding into other tools. `--results-dir DIR` relocates it; `--no-csv` skips it entirely. The directory is created if it does not exist.

Note that charts do **not** go through this file — they render from the in-memory samples. The CSV exists for you, not for the tool.

### Reports and charts

```bash
gauntlet benchmark --config config.json \
  --markdown-report results/report.md \
  --html-report results/report.html \
  --junit-report results/junit.xml \
  --prometheus-file results/metrics.prom \
  --charts cdf,tail,throughput --charts-dir results/charts
```

Every output is opt-in and they compose; the terminal output is always produced.

- **Markdown** — the same content as terminal output, suitable for CI artifacts, PR comments, or archival.
- **HTML** — one self-contained document. The stylesheet and the SVG charts (histogram and CDF) are inlined, so the file renders correctly when opened straight from a file manager over `file://` — a report that needs a web server to look right is a report nobody looks at.
- **JUnit XML** — latency metrics encoded as properties on synthetic test cases, for test-framework integrations.
- **Prometheus** — exposition text. `--prometheus-file` writes it, `--prometheus-pushgateway URL` pushes it (`--prometheus-job NAME`, default `gauntlet`), and specifying both does both.
- **Charts** — one SVG per target per kind, named `<target-slug>-<kind>.svg`.

Chart kinds, rendered natively in-process (there is no Python involved and no external tooling to install):

| Kind | Aliases | What it shows |
|---|---|---|
| `histogram` | | Binned frequency of latencies |
| `cdf` | | Empirical CDF — what fraction of requests came in under X ms |
| `tail` | | The top decile plotted percentile-against-latency, where p99-and-beyond behaviour is actually visible; p95 and p99 are marked |
| `timeline` | | Latency against request ordinal, which exposes warmup effects and drift |
| `rolling_pct` | `rolling` | Trailing-window p50/p95/p99 — separates a tail that was always there from one that appeared as load built |
| `boxplot` | `box` | Quartiles, 1.5-IQR whiskers, and outliers |
| `throughput` | | Completed requests per second over the run (failures included — a service that is fast because it is refusing everything has a throughput) |
| `error_rate` | `errors` | Share of requests that failed, over the run, on a fixed 0-100% axis |
| `status` | | Response counts grouped by status class |

Hyphens and underscores are interchangeable. An unknown kind is rejected when the arguments are parsed, with the valid list — not after the benchmark has already run.

Charts that need latency are skipped for a target where every request failed; `error_rate` and `status` still render, because those are the charts you actually want in that situation. Charts that need a time span are skipped for a run with no measurable duration.

### The live view

When stdout is an interactive terminal and no CI environment is detected, `benchmark` runs under a live TUI showing progress and rolling statistics. `--no-tui` forces the headless view, which is what CI gets automatically.

`q`, `Esc`, or `Ctrl-C` cancels. A cancelled run exits 2 and writes no baseline: an interrupted run has no trustworthy measurements, and recording it as a result would poison every later comparison.

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

Baselines are stored as JSON in `baselines/<name>.json`; `--baseline-dir DIR` relocates the directory. A run with more than one target writes one baseline per target, named `<name>--<target>`, since the targets are different systems and must not overwrite each other's history. A single-target run keeps the bare name.

When comparing, gauntlet checks mean, p50, p95, and p99 against regression thresholds:

| Metric | Default Threshold |
|---|---|
| Mean | 10% |
| p50 | 10% |
| p95 | 10% |
| p99 | 15% |

If any metric on any target exceeds its threshold, the tool exits with code 1.

Why is the p99 threshold higher? Tail latencies are inherently noisier — small sample variations cause larger swings in p99. A 10% threshold on p99 would produce too many false positives in CI, causing alert fatigue without actionable signal.

### Baseline file format

The file is versioned and self-describing:

```json
{
  "schema_version": 1,
  "name": "v1.0",
  "created_at": "2026-07-21T10:00:00Z",
  "stats": { "mean_ms": 12.5, "p50_ms": 11.9, "...": "..." }
}
```

Only the compared metrics plus context are stored; the histogram is dropped, since nothing reads it back and it dominates the file size.

**Baselines written by the previous Haskell build cannot be read.** That format was a different encoding of a different record, and carrying a compatibility shim would pin this format to that record layout forever. Loading one reports an unsupported-format error telling you to regenerate. Re-baselining costs one benchmark run:

```bash
gauntlet benchmark --config config.json --save-baseline v1.0
```

### Comparing saved results offline

```bash
gauntlet compare baselines/before.json baselines/after.json
```

Takes two file paths — a saved baseline or a bare stats snapshot, either shape — and prints the full Bayesian comparison without sending a single request. Useful for diffing two runs after the fact, or comparing results captured on different machines. Earth Mover's Distance is unavailable here: both formats store summary statistics, not the raw samples it needs.

## CI/CD Integration

Gauntlet auto-detects CI environments via the `GITLAB_CI` and `GITHUB_ACTIONS` environment variables (GitLab is checked first, since a GitLab job can shell out in ways that leave `GITHUB_ACTIONS` set). The live TUI is disabled automatically in CI, so nothing writes escape sequences into your job log.

**GitLab CI**: collapsible CI sections with ANSI-coloured regression status, making results easy to scan in job logs.

**GitHub Actions**: plain-text regression output, plus the report appended to `$GITHUB_STEP_SUMMARY` for the job summary UI.

Either way a markdown regression artifact is written into `--results-dir` (default `results/`). Exit code 1 on regression naturally fails the pipeline.

Example CI usage:

```yaml
# GitHub Actions
- name: Run performance benchmark
  run: |
    gauntlet benchmark \
      --config bench.json \
      --compare-baseline main \
      --markdown-report results/report.md

- name: Upload report
  if: always()
  uses: actions/upload-artifact@v4
  with:
    name: benchmark-report
    path: results/report.md
```

## Distributed Tracing (Grafana Tempo)

When `tempo` is configured in settings, gauntlet queries Grafana Tempo for traces from the benchmark's time window after the run completes and aggregates span-level statistics.

Spans are grouped by **(service, span name)**, not span name alone. The moment a trace crosses services, a `handle` in the gateway and a `handle` in the backend are different operations, and collapsing them gives you one row with a bimodal, meaningless distribution.

The trace report shows per-group count, error count, mean, stddev, min/max, p50, p95, and p99. This helps you identify which internal service or operation is the bottleneck — for example, your API endpoint may be slow because a downstream database query has a high p99.

```json
"tempo": {
  "url": "http://tempo:3200",
  "service_name": "my-service",
  "enabled": true,
  "auth_token": "optional-tempo-bearer-token"
}
```

Trace analysis is a **diagnostic and never fails the run**. A Tempo endpoint that is down, slow, or has not yet ingested the run's spans logs a warning and omits the section — turning a clean benchmark into a failure because a separate service was unavailable would be indefensible. A window that legitimately contains no traces still produces a report, an empty one, so "not configured" and "configured, but Tempo had nothing" stay distinguishable.

## Example Configs

The `examples/` directory holds eight working configs, all of which parse and validate:

| File | What it demonstrates |
|---|---|
| `minimal.json` | The smallest useful config: two targets, one endpoint |
| `simple-benchmark.json` | Single target, several REST endpoints |
| `comparison.json` | Three targets, all-pairs comparison |
| `ab-comparison.json` | A/B with warmup, retry, and per-target health checks |
| `api-with-auth.json` | Bearer token file plus custom per-payload headers |
| `load-modes.json` | Step-load profile (duration-based) |
| `log-levels.json` | Log verbosity |
| `advanced-config.json` | Everything: lifecycle hooks, connection tuning, Tempo, constant RPM |

Run one with:

```bash
gauntlet validate --config examples/advanced-config.json
gauntlet benchmark --config examples/minimal.json
```

### Minimal

```json
{
  "targets": [
    { "name": "primary", "url": "http://api.example.com" },
    { "name": "candidate", "url": "http://api-new.example.com" }
  ],
  "settings": {
    "iterations": 100,
    "concurrency": 10
  },
  "payloads": [
    { "name": "health-check", "method": "GET", "path": "/health" }
  ]
}
```

### Multi-target comparison

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
    { "name": "get-users", "method": "GET", "path": "/api/users" },
    {
      "name": "create-user",
      "method": "POST",
      "path": "/api/users",
      "body": { "name": "test", "email": "test@example.com" }
    }
  ]
}
```

### A/B comparison with warmup, retry, and health checks

```json
{
  "targets": [
    {
      "name": "api-v1",
      "url": "http://api-v1.example.com",
      "lifecycle": {
        "health_check": { "url": "http://api-v1.example.com/health", "timeout_secs": 30 }
      }
    },
    {
      "name": "api-v2",
      "url": "http://api-v2.example.com",
      "lifecycle": {
        "health_check": { "url": "http://api-v2.example.com/health", "timeout_secs": 30 }
      }
    }
  ],
  "settings": {
    "iterations": 5000,
    "concurrency": 50,
    "secrets": ".secrets/token.txt",
    "request_timeout_secs": 60,
    "log_level": "info",
    "warmup": { "iterations": 10 },
    "retry": {
      "max_attempts": 3,
      "initial_delay_ms": 1000,
      "backoff_multiplier": 2.0
    }
  },
  "payloads": [
    {
      "name": "search-products",
      "method": "POST",
      "path": "/api/search",
      "body": {
        "query": "laptop",
        "filters": { "priceMin": 500, "priceMax": 2000, "category": "electronics" },
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

### Advanced (lifecycle hooks, tracing, rate limiting)

```json
{
  "targets": [
    {
      "name": "primary",
      "url": "http://primary-service.internal:8080",
      "lifecycle": {
        "setup": { "cmd": "docker-compose --profile testing up -d --build", "timeout_secs": 120 },
        "teardown": { "cmd": "docker-compose down" },
        "health_check": {
          "url": "http://primary-service.internal:8080/health",
          "timeout_secs": 60,
          "interval_ms": 1000
        }
      }
    },
    {
      "name": "candidate",
      "url": "http://candidate-service.internal:8080",
      "lifecycle": {
        "setup": { "cmd": "docker-compose --profile testing up -d --build", "timeout_secs": 120 },
        "teardown": { "cmd": "docker-compose down" },
        "health_check": {
          "url": "http://candidate-service.internal:8080/health",
          "timeout_secs": 60,
          "interval_ms": 1000
        }
      }
    }
  ],
  "settings": {
    "iterations": 10000,
    "concurrency": 100,
    "secrets": ".secrets/service-token.txt",
    "max_connections": 200,
    "request_timeout_secs": 120,
    "log_level": "debug",
    "warmup": { "iterations": 20 },
    "retry": { "max_attempts": 10, "initial_delay_ms": 2000, "backoff_multiplier": 2.5 },
    "tempo": {
      "url": "http://tempo:3200",
      "service_name": "my-service",
      "enabled": true,
      "auth_token": "optional-tempo-bearer-token"
    },
    "load_mode": { "mode": "constant_rpm", "target_rpm": 6000 }
  },
  "payloads": [
    {
      "name": "complex-query",
      "method": "POST",
      "path": "/api/v3/analytics/query",
      "headers": {
        "X-Trace-ID": "benchmark-trace",
        "Accept": "application/json"
      },
      "body": {
        "timeRange": { "start": "2026-02-01T00:00:00Z", "end": "2026-02-16T23:59:59Z" },
        "groupBy": ["region", "category"]
      }
    }
  ]
}
```

The full version of this config is `examples/advanced-config.json`.

## Exit Codes

| Code | Meaning |
|---|---|
| 0 | Success — benchmark completed, no regressions detected |
| 1 | Regression detected (baseline comparison) |
| 2 | Error — bad config, setup-hook failure, unreachable endpoint under `--check-endpoints`, cancelled run, etc. |

With multiple targets, *any* target regressing fails the run.
