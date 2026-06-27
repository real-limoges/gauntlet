# ADR M3-A — HTTP client: `reqwest` over `hyper`

Status: **accepted** · Milestone: M3 (spike) · See `docs/RUST_PORT.md` §2.3, §M3.

## Context

For a measurement tool, client-side overhead — and especially its *jitter* — is
measurement error. `RUST_PORT.md` §2.3 flags the client choice as a real decision
("do not default this") and requires it be resolved with data: `reqwest` (ergonomic,
layers conveniences over hyper) vs `hyper`/`hyper-util` (lower, more predictable
per-request overhead, more plumbing).

## Method

`crates/gauntlet-engine/examples/client_overhead.rs` (rerunnable) fires requests at an
in-process `axum` mock that returns a trivial JSON 200 with ~zero server work, isolating
*client* overhead. After a 2,000-request warmup it issues 20,000 requests through each
client, both sequentially and at concurrency 32, timing issue→body-drained with a
monotonic `Instant`. It reports the per-request floor (min), median, p99, and stddev.

```
cargo run --release -p gauntlet-engine --example client_overhead
```

## Data

Two release runs on the dev machine (macOS), microseconds per request:

| scenario              | client  | floor | median | p99   | stddev |
|-----------------------|---------|------:|-------:|------:|-------:|
| sequential            | reqwest |  41.9 |   52.2 |  72.1 |    4.9 |
| sequential            | hyper   |  38.8 |   44.1 |  63.6 |    4.7 |
| concurrent (32)       | reqwest | 353.2 |  428.0 | 651.6 |  100.5 |
| concurrent (32)       | hyper   | 221.7 |  424.6 | 630.3 |   91.7 |

(Second run consistent: sequential reqwest ~54µs mean, hyper ~46µs; concurrent ~434µs both.)

## Decision

**Use `reqwest`** (rustls-tls, no default features) as the engine's HTTP client.

- **Sequential:** hyper is ~8µs / ~17% faster per request, but reqwest's overhead is
  *small and stable* — ~5µs stddev, tight p99. Relative to realistic target latencies
  (sub-millisecond to many milliseconds), an 8µs floor delta is negligible.
- **Concurrent:** the two are statistically indistinguishable (medians 428 vs 425µs,
  comparable jitter); the distribution is dominated by scheduling/contention, not client
  layering.
- The decision rule ("if reqwest's overhead is small *and* stable relative to realistic
  target latencies, take reqwest") is satisfied. reqwest buys rustls TLS, `.json()`,
  connection pooling, and redirect handling for free; hyper's marginal edge does not
  justify hand-rolling that plumbing.

## Consequences

- `gauntlet-engine` depends on `reqwest` only; `hyper`/`hyper-util`/`http-body-util`
  remain **dev-dependencies** (the spike example) and are not in the production build.
- The spike harness stays in-tree and rerunnable, so the decision can be revisited if the
  client overhead floor ever becomes material to a target's latencies.
- **Caveat recorded:** absolute latencies include this ~40µs+ client floor. That is
  acceptable for A/B *comparison* (the floor cancels) and regression deltas; it is a known
  bias for absolute single-number latency claims.
