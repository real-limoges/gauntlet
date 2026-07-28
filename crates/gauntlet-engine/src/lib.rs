//! `gauntlet-engine` — the async measurement loop: tokio worker pool, HTTP
//! execution, rate limiting, response validation, warmup, and lifecycle hooks.
//!
//! Entry point: [`run_benchmark`]. The output is `TestingResponse` sample vectors
//! that `gauntlet_stats::calculate_stats` consumes.
//!
//! # How a run is shaped
//!
//! Targets run **sequentially**, so they never contend for the machine during
//! each other's measurement. Within a target, endpoints run **concurrently**.
//!
//! Each target's run is: lifecycle setup → health check → warmup → endpoints →
//! teardown. Teardown is armed as soon as setup succeeds and runs however the
//! rest turns out; a failing *setup* returns before it is armed, because a hook
//! that brought nothing up has nothing to tear down. A failing *health check* is
//! inside the guard: service-up-but-not-ready is the common lifecycle failure,
//! and it is precisely the case where the resources setup created must still be
//! released.
//!
//! # Pacing versus concurrency
//!
//! Two independent gates, so they compose without double-counting:
//!
//! - The **rate limiter** ([`rate_limiter`]) paces request *starts*. It lives on
//!   the [`RunContext`] and is shared by every endpoint in a target, because
//!   `settings.load_mode` is one global setting: `target_rpm` is the rate the
//!   *target* receives, not the rate each payload sends. One limiter per endpoint
//!   would make the real aggregate `target_rpm × payloads`, silently invalidating
//!   any capacity-planning number drawn from a multi-payload run.
//! - The **semaphore** caps requests *in flight*, per endpoint.
//!
//! Warmup deliberately runs before the limiter and its clock exist, so priming
//! requests neither consume pacing slots nor eat into a duration-based window.
//!
//! The limiter is hand-rolled rather than `governor`, which covers constant
//! rates only. A shared `next_slot` clock advances atomically per request and the
//! caller sleeps until the slot it claimed; because the interval is recomputed at
//! each claim, the time-varying modes (ramp, step) and the stochastic one
//! (Poisson) all fall out of the one mechanism.
//!
//! Duration-based modes check the deadline *before* sleeping to a claimed slot.
//! Slots are reserved atomically up front, so sleeping first and checking after
//! would overrun by up to `concurrency × interval` — a 10-second run at 60 rpm
//! with concurrency 50 would block for 49 seconds. The cost of the correct
//! ordering is that a run ends up to one pacing interval short of its configured
//! duration, which is the right side to err on.
//!
//! # What counts as a failure
//!
//! A transport failure has no latency and is excluded from the distribution. **An
//! HTTP 500 is a response**: it has a real latency, it counts, and it is never
//! retried. Only connection refusals and timeouts are retryable.
//!
//! Latency is measured with a monotonic clock (`Instant::elapsed`) and spans all
//! retry attempts. `requested_at` keeps a wall-clock `SystemTime` purely for the
//! CSV timestamp, where a human-readable instant is the point.
//!
//! # The HTTP client
//!
//! `reqwest` over rustls, chosen by measuring it against `hyper` on a localhost
//! mock: reqwest's overhead was small (~8µs sequential), stable (~5µs stddev),
//! and indistinguishable under concurrency, so ergonomics decided it.
//! `examples/client_overhead.rs` is that measurement, kept so the choice can be
//! rechecked rather than taken on faith. [`client`] is the only module that knows
//! which client is in use.
//!
//! # Validation
//!
//! Regex patterns are compiled once, by [`validation::check_patterns`], before
//! the run starts. That serves two ends: a malformed pattern becomes a *config*
//! error rather than a per-response validation failure repeated for the length of
//! the run — a typo would otherwise report the service as having failed ten
//! thousand assertions — and the measurement loop never pays to compile.
//!
//! # Events
//!
//! The engine streams [`BenchmarkEvent`]s to an optional live UI over an
//! **unbounded** channel, and every send is fire-and-forget. This is deliberate:
//! a bounded channel would let a slow terminal apply backpressure to the request
//! loop and widen the very latencies being measured. A closed receiver just means
//! the operator quit the UI while the benchmark ran on.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

pub mod auth;
pub mod client;
pub mod context;
pub mod csv_out;
pub mod endpoint;
pub mod error;
pub mod event;
pub mod exec;
pub mod lifecycle;
pub mod rate_limiter;
pub mod validation;

use std::path::Path;
use std::sync::{Arc, Mutex};

use gauntlet_core::{build_endpoints, BenchmarkConfig, NamedTarget};
use tokio::task::JoinSet;

pub use context::RunContext;
pub use endpoint::EndpointResult;
pub use error::{EngineError, Result};
pub use event::{BenchmarkEvent, EventSink};

use csv_out::CsvSink;

/// One target's results: its name and the per-endpoint outcomes.
#[derive(Clone, Debug)]
pub struct TargetResult {
    pub name: String,
    pub endpoints: Vec<EndpointResult>,
}

/// A whole benchmark run: one entry per target, in run order.
#[derive(Clone, Debug)]
pub struct BenchmarkRun {
    pub targets: Vec<TargetResult>,
}

/// Run a full benchmark: validate, build the client, then for each target run
/// lifecycle setup → warmup → endpoints → teardown. When `csv_path` is set, every
/// response is appended to the latency CSV.
pub async fn run_benchmark(
    config: &BenchmarkConfig,
    csv_path: Option<&Path>,
) -> Result<BenchmarkRun> {
    run_benchmark_with_events(config, csv_path, None).await
}

/// As [`run_benchmark`], but also streaming progress to a live UI. The sender is
/// unbounded on purpose — see `event`.
pub async fn run_benchmark_with_events(
    config: &BenchmarkConfig,
    csv_path: Option<&Path>,
    events: EventSink,
) -> Result<BenchmarkRun> {
    config.validate()?;
    // Compiles every `matches` pattern before a single request goes out, so a
    // malformed one is a config error rather than a per-response validation
    // failure repeated for the length of the run.
    validation::check_patterns(config).map_err(EngineError::Pattern)?;

    let client = client::build_client(&config.settings)?;
    // Resolved once per run: the token is the same for every target and request.
    let token = match &config.settings.secrets {
        Some(path) => auth::read_token(path)?,
        None => None,
    };
    let csv = match csv_path {
        Some(path) => Some(Arc::new(Mutex::new(CsvSink::create(path)?))),
        None => None,
    };

    let mut targets = Vec::with_capacity(config.targets.len());
    let mut failure = None;
    for (index, target) in config.targets.iter().enumerate() {
        event::emit(
            &events,
            BenchmarkEvent::TargetStarted {
                name: target.name.clone(),
                index: index + 1,
                total_requests: expected_requests(config),
            },
        );
        match run_target(config, target, &client, token.as_deref(), &csv, &events).await {
            Ok(result) => targets.push(result),
            Err(e) => {
                event::emit(
                    &events,
                    BenchmarkEvent::Failed {
                        message: e.to_string(),
                    },
                );
                failure = Some(e);
                break;
            }
        }
    }

    // Flushed on the failure path too: `main` exits via `std::process::exit`,
    // which does not unwind, so `Drop` will not do it and a buffered row is a
    // measurement thrown away.
    if let Some(csv) = &csv {
        let flushed = csv.lock().expect("csv mutex not poisoned").flush();
        match (flushed, &failure) {
            // Don't let a flush error mask the error that ended the run.
            (Err(e), Some(_)) => gauntlet_core::log::warn(format!("flushing latency CSV: {e}")),
            (Err(e), None) => return Err(e),
            (Ok(()), _) => {}
        }
    }

    if let Some(e) = failure {
        return Err(e);
    }
    event::emit(&events, BenchmarkEvent::Finished);

    Ok(BenchmarkRun { targets })
}

/// Requests one target is expected to issue. Only a hint: a duration-based mode
/// has no such number up front, and the UI shows elapsed progress instead.
fn expected_requests(config: &BenchmarkConfig) -> usize {
    config.settings.iterations.get() as usize * config.payloads.len()
}

async fn run_target(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    token: Option<&str>,
    csv: &Option<Arc<Mutex<CsvSink>>>,
    events: &EventSink,
) -> Result<TargetResult> {
    // --- setup: outside the teardown guard, see the crate docs ---------------
    if let Some(lifecycle) = &target.lifecycle {
        if let Some(setup) = &lifecycle.setup {
            event::emit(
                &events.clone(),
                BenchmarkEvent::Status {
                    message: format!("Setting up {}...", target.name),
                },
            );
            lifecycle::run_hook(&target.name, "setup", setup).await?;
        }
    }

    // --- health check + benchmark, both under teardown ----------------------
    // The block keeps its `?` from escaping the function past the teardown below.
    let outcome = async {
        if let Some(hc) = target
            .lifecycle
            .as_ref()
            .and_then(|l| l.health_check.as_ref())
        {
            lifecycle::wait_healthy(&target.name, hc).await?;
        }
        run_target_endpoints(config, target, client, token, csv, events).await
    }
    .await;

    if let Some(lifecycle) = &target.lifecycle {
        if let Some(teardown) = &lifecycle.teardown {
            if let Err(e) = lifecycle::run_hook(&target.name, "teardown", teardown).await {
                gauntlet_core::log::warn(e.to_string());
            }
        }
    }

    outcome
}

async fn run_target_endpoints(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    token: Option<&str>,
    csv: &Option<Arc<Mutex<CsvSink>>>,
    events: &EventSink,
) -> Result<TargetResult> {
    let endpoints = build_endpoints(&target.url, &config.payloads);

    // Warm up the first endpoint, discarding results. Before the limiter exists,
    // deliberately — see the crate docs.
    if config.settings.warmup.iterations > 0 {
        if let Some(first) = endpoints.first() {
            endpoint::warmup(
                client,
                token,
                &config.settings.retry,
                first,
                config.settings.warmup.iterations,
            )
            .await;
        }
    }

    // One limiter and one clock per target, shared by every endpoint below.
    let started_at = std::time::Instant::now();
    let ctx = Arc::new(RunContext {
        client: client.clone(),
        token: token.map(str::to_string),
        settings: config.settings.clone(),
        csv: csv.clone(),
        events: events.clone(),
        limiter: rate_limiter::RateLimiter::new(config.settings.load_mode.clone(), started_at)
            .map(Arc::new),
        started_at,
        completed: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
    });

    // Feeds the live view's load-control band for as long as the endpoints run.
    let monitor = spawn_load_monitor(&ctx);

    // Run endpoints concurrently; each pairs with its payload (by build order).
    let mut set = JoinSet::new();
    let endpoint_count = endpoints.len();
    for (index, (ep, payload)) in endpoints
        .into_iter()
        .zip(config.payloads.iter())
        .enumerate()
    {
        event::emit(
            events,
            BenchmarkEvent::EndpointStarted {
                name: payload.name.clone(),
                index: index + 1,
                total: endpoint_count,
            },
        );
        let ctx = ctx.clone();
        let target_name = target.name.clone();
        let payload_name = payload.name.clone();
        set.spawn(endpoint::run_endpoint(ctx, target_name, payload_name, ep));
    }

    // Collect, then restore payload order (concurrent completion is unordered).
    let mut results: Vec<EndpointResult> = Vec::new();
    while let Some(joined) = set.join_next().await {
        let result = joined.expect("endpoint task did not panic")?;
        results.push(result);
    }
    drop(monitor);
    results.sort_by_key(|r| payload_index(config, &r.name));

    Ok(TargetResult {
        name: target.name.clone(),
        endpoints: results,
    })
}

/// Cancels its task on drop, so every exit from the endpoint run stops the
/// monitor — including the `?` on an endpoint error.
struct AbortOnDrop(tokio::task::JoinHandle<()>);

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        self.0.abort();
    }
}

/// How often the live view's load-control readout refreshes. Slow enough to cost
/// the run nothing, fast enough to look live.
const LOAD_MONITOR_TICK: std::time::Duration = std::time::Duration::from_millis(500);

/// Emit the live view's load-control events for the duration of a target's run.
///
/// The rate reported is *achieved* — completions over elapsed — because the
/// configured rate is already in the config, and a service that cannot keep up
/// is exactly what the difference between the two shows.
fn spawn_load_monitor(ctx: &Arc<RunContext>) -> AbortOnDrop {
    let ctx = ctx.clone();
    AbortOnDrop(tokio::spawn(async move {
        let started = std::time::Instant::now();
        let steps = match &ctx.settings.load_mode {
            gauntlet_core::LoadMode::StepLoad { steps } => Some(steps.clone()),
            _ => None,
        };
        let mut last_step = None;

        loop {
            tokio::time::sleep(LOAD_MONITOR_TICK).await;
            let elapsed = started.elapsed().as_secs_f64();
            if elapsed <= 0.0 {
                continue;
            }

            let done = ctx.completed.load(std::sync::atomic::Ordering::Relaxed);
            event::emit(
                &ctx.events,
                BenchmarkEvent::CurrentRpmUpdated {
                    rpm: done as f64 * 60.0 / elapsed,
                },
            );

            // Only on a change: the UI holds the last value it was sent.
            if let Some(steps) = &steps {
                if let Some((step, target_rpm)) = rate_limiter::step_at(steps, elapsed) {
                    if last_step != Some(step) {
                        last_step = Some(step);
                        event::emit(
                            &ctx.events,
                            BenchmarkEvent::LoadStepChanged { step, target_rpm },
                        );
                    }
                }
            }
        }
    }))
}

fn payload_index(config: &BenchmarkConfig, name: &str) -> usize {
    config
        .payloads
        .iter()
        .position(|p| p.name == name)
        .unwrap_or(usize::MAX)
}
