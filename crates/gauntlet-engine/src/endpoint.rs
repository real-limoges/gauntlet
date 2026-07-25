//! Per-endpoint execution. Two shapes, chosen by load mode:
//!
//! - **fixed-count** (`Unthrottled`/`ConstantRpm`/`PoissonRpm`): fire
//!   `total_requests` requests, pacing dispatch through the limiter and capping
//!   in-flight requests with a `Semaphore`.
//! - **duration-based** (`RampUp`/`StepLoad`): spawn `concurrency` workers that
//!   each loop until the deadline, sharing one limiter.
//!
//! Pacing (limiter) and concurrency (semaphore) are independent gates, so they
//! compose without double-counting.
//!
//! Both gates live in [`LoadGates`], built **once per target** and shared by
//! every endpoint that target runs. That sharing is the whole point:
//! `settings.concurrency` and `load_mode.target_rpm` describe the load placed on
//! the target, not on each payload. A per-endpoint gate would multiply both by
//! the payload count.

use std::sync::Arc;
use std::time::{Duration, Instant};

use gauntlet_core::{
    extract_durations, Endpoint, HttpMethod, TestingResponse, ValidationSummary,
    MAX_VALIDATION_ERRORS,
};
use gauntlet_stats::{calculate_stats, BenchmarkStats};
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

use crate::client::{self, PreparedEndpoint};
use crate::context::RunContext;
use crate::csv_out::CsvRow;
use crate::error::Result;
use crate::exec::{self, RequestOutcome};
use crate::rate_limiter::RateLimiter;

/// The outcome of benchmarking one endpoint: the latency samples (as
/// `TestingResponse`s), the validation summary, and the computed stats.
#[derive(Clone, Debug)]
pub struct EndpointResult {
    pub name: String,
    pub url: String,
    pub method: HttpMethod,
    /// Every request's outcome, each carrying its response *and* the wall-clock
    /// time it was issued. Reporters need the timestamp for time-series charts
    /// (throughput, error rate over time), so it is retained rather than
    /// collapsed into a bare latency vector.
    pub outcomes: Vec<RequestOutcome>,
    pub validation: ValidationSummary,
    pub stats: BenchmarkStats,
}

/// The load gates for one target: how fast requests may start (the limiter) and
/// how many may be in flight at once (the semaphore), plus the instant both are
/// anchored to.
///
/// `start` anchors the limiter's slot clock *and* the duration-mode deadline, so
/// the two cannot drift apart — the limiter schedules slots in seconds since
/// `start`, and the deadline is `start + duration`.
pub struct LoadGates {
    sem: Arc<Semaphore>,
    limiter: Option<Arc<RateLimiter>>,
    start: Instant,
}

impl LoadGates {
    /// Build the gates for a target from its settings, anchored at now.
    pub fn for_target(settings: &gauntlet_core::Settings) -> Arc<Self> {
        let start = Instant::now();
        Arc::new(LoadGates {
            sem: Arc::new(Semaphore::new(settings.concurrency.get() as usize)),
            limiter: RateLimiter::new(settings.load_mode.clone(), start).map(Arc::new),
            start,
        })
    }

    /// When this target's measurement window ends, for duration-based modes.
    fn deadline(&self, settings: &gauntlet_core::Settings) -> Instant {
        self.start + Duration::from_secs_f64(settings.load_mode.duration_secs())
    }
}

/// Warm up an endpoint: fire `n` sequential requests and discard the results, so
/// connection pools and JIT paths are primed before measurement.
pub async fn warmup(ctx: &RunContext, endpoint: &Endpoint, n: u32) -> Result<()> {
    let prepared = client::prepare(endpoint, ctx.token.as_deref())?;
    for _ in 0..n {
        let _ = exec::execute(&ctx.client, &prepared, &ctx.settings.retry).await;
    }
    Ok(())
}

/// Benchmark a single endpoint to completion, under the target's shared gates.
pub async fn run_endpoint(
    ctx: Arc<RunContext>,
    gates: Arc<LoadGates>,
    target_name: String,
    payload_name: String,
    endpoint: Endpoint,
) -> Result<EndpointResult> {
    // Resolved once, ahead of measurement, so no request pays for URL parsing,
    // header assembly, or body serialization inside its own timed window.
    let prepared = Arc::new(client::prepare(&endpoint, ctx.token.as_deref())?);

    let load_mode = ctx.settings.load_mode.clone();
    let outcomes = if load_mode.is_duration_based() {
        run_duration_based(&ctx, &gates, &prepared).await
    } else {
        run_fixed_count(&ctx, &gates, &prepared).await
    };

    if let Some(csv) = &ctx.csv {
        let mut sink = csv.lock().expect("csv mutex not poisoned");
        for o in &outcomes {
            sink.write_row(&CsvRow {
                target_name: &target_name,
                payload_id: &payload_name,
                url: &endpoint.url,
                method: endpoint.method,
                response: &o.response,
                requested_at: o.requested_at,
            })?;
        }
    }

    let validation = summarize_validation(&endpoint, &outcomes);
    let responses: Vec<TestingResponse> = outcomes.iter().map(|o| o.response.clone()).collect();
    let durations = extract_durations(&responses);
    let stats = calculate_stats(outcomes.len(), &durations);

    Ok(EndpointResult {
        name: payload_name,
        url: endpoint.url,
        method: endpoint.method,
        outcomes,
        validation,
        stats,
    })
}

/// Fixed-count modes: pace dispatch (limiter), cap in-flight (semaphore). Both
/// gates are the target's, shared with every other endpoint running alongside.
async fn run_fixed_count(
    ctx: &RunContext,
    gates: &LoadGates,
    prepared: &Arc<PreparedEndpoint>,
) -> Vec<RequestOutcome> {
    let settings = &ctx.settings;
    let total = settings.load_mode.total_requests(settings.iterations.get()) as usize;

    let mut set = JoinSet::new();
    for _ in 0..total {
        if let Some(l) = &gates.limiter {
            l.wait_for_slot().await;
        }
        let permit = gates
            .sem
            .clone()
            .acquire_owned()
            .await
            .expect("semaphore open");
        let client = ctx.client.clone();
        let prepared = prepared.clone();
        let retry = settings.retry.clone();
        let events = ctx.events.clone();
        set.spawn(async move {
            let outcome = exec::execute(&client, &prepared, &retry).await;
            drop(permit);
            // Emitted here, not in `collect`: the UI must see each request as it
            // lands, and `collect` does not run until every task is spawned.
            crate::event::emit(&events, request_event(&outcome));
            outcome
        });
    }

    collect(set, total).await
}

/// Duration-based modes: `concurrency` workers loop until the shared deadline,
/// pacing each request through the target's shared limiter.
///
/// The worker count is the *whole target's* concurrency, so with several
/// endpoints these workers contend for the same semaphore rather than each
/// endpoint getting a full allowance of its own.
async fn run_duration_based(
    ctx: &RunContext,
    gates: &LoadGates,
    prepared: &Arc<PreparedEndpoint>,
) -> Vec<RequestOutcome> {
    let settings = &ctx.settings;
    let concurrency = settings.concurrency.get() as usize;
    let deadline = gates.deadline(settings);
    let limiter = gates
        .limiter
        .clone()
        .expect("duration-based modes always have a limiter");

    let mut set = JoinSet::new();
    for _ in 0..concurrency {
        let client = ctx.client.clone();
        let prepared = prepared.clone();
        let retry = settings.retry.clone();
        let limiter = limiter.clone();
        let sem = gates.sem.clone();
        let events = ctx.events.clone();
        set.spawn(async move {
            let mut outs = Vec::new();
            loop {
                // Two gates on the deadline: this one catches a request that
                // itself ran past it, and `wait_for_slot_before` refuses to sleep
                // beyond it while waiting for the next slot.
                if Instant::now() >= deadline {
                    break;
                }
                if !limiter.wait_for_slot_before(deadline).await {
                    break;
                }
                let permit = sem.clone().acquire_owned().await.expect("semaphore open");
                let outcome = exec::execute(&client, &prepared, &retry).await;
                drop(permit);
                crate::event::emit(&events, request_event(&outcome));
                outs.push(outcome);
            }
            outs
        });
    }

    let mut outcomes = Vec::new();
    while let Some(r) = set.join_next().await {
        if let Ok(mut outs) = r {
            outcomes.append(&mut outs);
        }
    }
    outcomes
}

async fn collect(mut set: JoinSet<RequestOutcome>, hint: usize) -> Vec<RequestOutcome> {
    let mut outcomes = Vec::with_capacity(hint);
    while let Some(r) = set.join_next().await {
        if let Ok(outcome) = r {
            outcomes.push(outcome);
        }
    }
    outcomes
}

/// Classify one outcome for the live UI: a transport failure is reported as
/// such (it has no meaningful latency), anything that answered is a completion,
/// including a 500 — the status is what the UI colours on.
fn request_event(outcome: &RequestOutcome) -> crate::event::BenchmarkEvent {
    match &outcome.response.error {
        Some(message) => crate::event::BenchmarkEvent::RequestFailed {
            message: message.clone(),
        },
        None => crate::event::BenchmarkEvent::RequestCompleted {
            latency_ms: gauntlet_core::ns_to_ms(outcome.response.duration).0,
            status: outcome.response.status,
        },
    }
}

/// Aggregate per-response validation errors into the endpoint summary. Errors are
/// collected from at most the first `MAX_VALIDATION_ERRORS` failing responses.
fn summarize_validation(endpoint: &Endpoint, outcomes: &[RequestOutcome]) -> ValidationSummary {
    if endpoint.validate.is_none() {
        return ValidationSummary::default();
    }
    let mut summary = ValidationSummary {
        total: outcomes.len(),
        failed: 0,
        errors: Vec::new(),
    };
    let mut failing_collected = 0;
    for o in outcomes {
        if o.validation_errors.is_empty() {
            continue;
        }
        summary.failed += 1;
        if failing_collected < MAX_VALIDATION_ERRORS {
            summary.errors.extend(o.validation_errors.iter().cloned());
            failing_collected += 1;
        }
    }
    summary
}
