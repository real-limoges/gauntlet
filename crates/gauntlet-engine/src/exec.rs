//! One request, executed to completion: retry/backoff around [`client::send`],
//! latency measurement, and `TestingResponse` construction. See the crate docs
//! for the clock choice.

use std::future::Future;
use std::time::{Duration, Instant, SystemTime};

use gauntlet_core::{Endpoint, Nanoseconds, RetrySettings, TestingResponse, ValidationError};

use crate::client::{self, RawResponse, TransportError};
use crate::validation;

/// One executed request: the response, the wall-clock start (for the CSV), and
/// any validation errors.
#[derive(Clone, Debug)]
pub struct RequestOutcome {
    pub response: TestingResponse,
    pub requested_at: SystemTime,
    pub validation_errors: Vec<ValidationError>,
}

/// Retry an async operation per `RetrySettings`. `max_attempts` counts *retries*
/// (0 disables them) and the delay grows by `backoff_multiplier` each time.
pub async fn with_retry<F, Fut>(
    retry: &RetrySettings,
    mut op: F,
) -> Result<RawResponse, TransportError>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<RawResponse, TransportError>>,
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
