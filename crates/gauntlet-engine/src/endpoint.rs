//! Per-endpoint execution, in two shapes: fixed-count fires `total_requests` and
//! stops, duration-based runs `concurrency` workers until a deadline.

use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

use gauntlet_core::{Endpoint, HttpMethod, ValidationSummary, MAX_VALIDATION_ERRORS};
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

use crate::context::RunContext;
use crate::csv_out::CsvRow;
use crate::error::Result;
use crate::exec::{self, RequestOutcome};

/// The outcome of benchmarking one endpoint. Carries no `BenchmarkStats` on
/// purpose: the caller computes those once over a target's endpoints combined.
#[derive(Clone, Debug)]
pub struct EndpointResult {
    pub name: String,
    pub url: String,
    pub method: HttpMethod,
    /// Every request's outcome, with the wall-clock time it was issued — the
    /// time-series charts need that timestamp.
    pub outcomes: Vec<RequestOutcome>,
    pub validation: ValidationSummary,
}

/// Prime connection pools: fire `n` sequential requests, discarding the results.
/// Takes its pieces directly because it runs before a [`RunContext`] exists.
pub async fn warmup(
    client: &reqwest::Client,
    token: Option<&str>,
    retry: &gauntlet_core::RetrySettings,
    endpoint: &Endpoint,
    n: u32,
) {
    for _ in 0..n {
        let _ = exec::execute(client, endpoint, token, retry).await;
    }
}

/// Benchmark a single endpoint to completion.
pub async fn run_endpoint(
    ctx: Arc<RunContext>,
    target_name: String,
    payload_name: String,
    endpoint: Endpoint,
) -> Result<EndpointResult> {
    let load_mode = ctx.settings.load_mode.clone();
    let outcomes = if load_mode.is_duration_based() {
        run_duration_based(&ctx, &endpoint).await
    } else {
        run_fixed_count(&ctx, &endpoint).await
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

    Ok(EndpointResult {
        name: payload_name,
        url: endpoint.url,
        method: endpoint.method,
        outcomes,
        validation,
    })
}

/// Fixed-count modes: pace dispatch (limiter), cap in-flight (semaphore).
async fn run_fixed_count(ctx: &RunContext, endpoint: &Endpoint) -> Vec<RequestOutcome> {
    let settings = &ctx.settings;
    let total = settings.load_mode.total_requests(settings.iterations.get()) as usize;
    let concurrency = settings.concurrency.get() as usize;
    let sem = Arc::new(Semaphore::new(concurrency));

    let mut set = JoinSet::new();
    for _ in 0..total {
        // The target's limiter, shared with its other endpoints.
        if let Some(l) = &ctx.limiter {
            l.wait_for_slot().await;
        }
        let permit = sem.clone().acquire_owned().await.expect("semaphore open");
        let client = ctx.client.clone();
        let endpoint = endpoint.clone();
        let token = ctx.token.clone();
        let retry = settings.retry.clone();
        let events = ctx.events.clone();
        let completed = ctx.completed.clone();
        set.spawn(async move {
            let outcome = exec::execute(&client, &endpoint, token.as_deref(), &retry).await;
            drop(permit);
            // Emitted here, not in `collect`, which does not run until every
            // task has been spawned — the UI must see each request as it lands.
            completed.fetch_add(1, Ordering::Relaxed);
            crate::event::emit(&events, request_event(&outcome));
            outcome
        });
    }

    collect(set, total).await
}

/// Duration-based modes: `concurrency` workers loop until the shared deadline,
/// pacing each request through one shared limiter.
async fn run_duration_based(ctx: &RunContext, endpoint: &Endpoint) -> Vec<RequestOutcome> {
    let settings = &ctx.settings;
    let concurrency = settings.concurrency.get() as usize;
    // The target's clock, so every endpoint shares one measurement window.
    let deadline = ctx.started_at + Duration::from_secs_f64(settings.load_mode.duration_secs());
    let Some(limiter) = ctx.limiter.clone() else {
        // Unreachable: only `Unthrottled` lacks a limiter, and it is not
        // duration-based. Empty beats an `expect` in the measurement loop.
        return Vec::new();
    };

    let mut set = JoinSet::new();
    for _ in 0..concurrency {
        let client = ctx.client.clone();
        let endpoint = endpoint.clone();
        let token = ctx.token.clone();
        let retry = settings.retry.clone();
        let limiter = limiter.clone();
        let events = ctx.events.clone();
        let completed = ctx.completed.clone();
        set.spawn(async move {
            let mut outs = Vec::new();
            loop {
                // Abandons a slot that lands past the deadline instead of
                // sleeping to it, so the duration bounds the run.
                if !limiter.wait_for_slot_before(deadline).await {
                    break;
                }
                let outcome = exec::execute(&client, &endpoint, token.as_deref(), &retry).await;
                completed.fetch_add(1, Ordering::Relaxed);
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

/// Classify one outcome for the live UI. Anything that answered is a completion,
/// including a 500; only a transport failure is a failure.
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

/// Aggregate per-response validation errors, capped at `MAX_VALIDATION_ERRORS`.
/// `total` counts only requests that got a response: counting transport failures
/// would report a dead target as having passed every assertion.
fn summarize_validation(endpoint: &Endpoint, outcomes: &[RequestOutcome]) -> ValidationSummary {
    if endpoint.validate.is_none() {
        return ValidationSummary::default();
    }
    let mut summary = ValidationSummary {
        total: 0,
        failed: 0,
        errors: Vec::new(),
    };
    let mut failing_collected = 0;
    for o in outcomes {
        if o.response.error.is_some() {
            continue;
        }
        summary.total += 1;
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

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use gauntlet_core::{Nanoseconds, TestingResponse, ValidationError, ValidationSpec};

    use super::*;

    fn spec() -> Endpoint {
        Endpoint {
            method: HttpMethod::Get,
            url: "http://example.invalid/x".into(),
            body: None,
            headers: Vec::new(),
            validate: Some(ValidationSpec {
                status: Some(200),
                fields: None,
            }),
        }
    }

    fn outcome(error: Option<&str>, failures: &[&str]) -> RequestOutcome {
        RequestOutcome {
            response: TestingResponse {
                duration: Nanoseconds(1),
                status: if error.is_some() { 0 } else { 200 },
                error: error.map(str::to_owned),
            },
            requested_at: SystemTime::UNIX_EPOCH,
            validation_errors: failures
                .iter()
                .map(|m| ValidationError {
                    field: "status".into(),
                    message: (*m).to_owned(),
                })
                .collect(),
        }
    }

    #[test]
    fn transport_failures_are_not_counted_as_validation_passes() {
        // A target that is down produces outcomes that never reached validation.
        // Counting them in `total` reported "1000 checked, 1000 passed" and let a
        // dead service emit a green JUnit validation suite.
        let outcomes: Vec<_> = (0..10)
            .map(|_| outcome(Some("connection refused"), &[]))
            .collect();
        let summary = summarize_validation(&spec(), &outcomes);

        assert_eq!(summary.total, 0, "nothing was checked");
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn only_responses_that_arrived_are_counted_and_judged() {
        let outcomes = vec![
            outcome(None, &[]),
            outcome(None, &["expected status 200, got 500"]),
            outcome(Some("timed out"), &[]),
        ];
        let summary = summarize_validation(&spec(), &outcomes);

        assert_eq!(summary.total, 2, "the transport failure is not a check");
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.errors.len(), 1);
    }

    #[test]
    fn an_endpoint_without_a_spec_summarizes_to_nothing() {
        let mut endpoint = spec();
        endpoint.validate = None;
        let summary = summarize_validation(&endpoint, &[outcome(None, &["ignored"])]);

        assert_eq!(summary.total, 0);
        assert_eq!(summary.failed, 0);
    }
}
