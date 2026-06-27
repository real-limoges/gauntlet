//! One request, executed to completion: retry/backoff around [`client::send`],
//! monotonic latency measurement, and `TestingResponse` construction.
//!
//! **Clock:** latency is `Instant::elapsed()` (monotonic), spanning all retry
//! attempts. This is an intentional improvement over the Haskell, which used the
//! `Realtime` clock — monotonic is the correct choice for elapsed durations
//! (immune to wall-clock steps). `requested_at` keeps a wall-clock `SystemTime`
//! purely for the CSV timestamp.

use std::future::Future;
use std::time::{Duration, Instant, SystemTime};

use gauntlet_core::{Endpoint, Nanoseconds, RetrySettings, TestingResponse, ValidationError};

use crate::client::{self, RawResponse, TransportError};
use crate::validation;

/// The full outcome of one executed request: the stats-relevant
/// [`TestingResponse`], the wall-clock start (for CSV), and any validation
/// errors (empty unless the endpoint had a spec and the request succeeded).
#[derive(Clone, Debug)]
pub struct RequestOutcome {
    pub response: TestingResponse,
    pub requested_at: SystemTime,
    pub validation_errors: Vec<ValidationError>,
}

/// Retry an async operation per `RetrySettings`: retry only when the error is
/// retryable and retries remain. `max_attempts` is the number of *retries* (0
/// disables them); the delay starts at `initial_delay_ms` and grows by
/// `ceil(delay × backoff_multiplier)` each retry.
pub async fn with_retry<F, Fut>(
    retry: &RetrySettings,
    mut op: F,
) -> std::result::Result<RawResponse, TransportError>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = std::result::Result<RawResponse, TransportError>>,
{
    let mut delay = retry.initial_delay_ms.get() as f64;
    let mut retries_left = retry.max_attempts;
    loop {
        match op().await {
            Ok(resp) => return Ok(resp),
            Err(err) => {
                if err.retryable && retries_left > 0 {
                    retries_left -= 1;
                    tokio::time::sleep(Duration::from_millis(delay as u64)).await;
                    delay = (delay * retry.backoff_multiplier).ceil();
                    continue;
                }
                return Err(err);
            }
        }
    }
}

/// Execute one request against `endpoint`, retrying transport failures, and
/// build the outcome. Latency covers all attempts.
pub async fn execute(
    client: &reqwest::Client,
    endpoint: &Endpoint,
    token: Option<&str>,
    retry: &RetrySettings,
) -> RequestOutcome {
    let requested_at = SystemTime::now();
    let start = Instant::now();

    let result = with_retry(retry, || client::send(client, endpoint, token)).await;
    let duration = Nanoseconds(start.elapsed().as_nanos() as u64);

    match result {
        Ok(raw) => {
            let validation_errors = endpoint
                .validate
                .as_ref()
                .map(|spec| validation::validate_response(spec, raw.status, &raw.body))
                .unwrap_or_default();
            RequestOutcome {
                response: TestingResponse {
                    duration,
                    status: raw.status,
                    error: None,
                },
                requested_at,
                validation_errors,
            }
        }
        Err(err) => RequestOutcome {
            response: TestingResponse {
                duration,
                status: 0,
                error: Some(err.message),
            },
            requested_at,
            validation_errors: Vec::new(),
        },
    }
}
