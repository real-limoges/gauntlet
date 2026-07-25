//! One request, executed to completion: retry/backoff around [`client::send`],
//! monotonic latency measurement, and `TestingResponse` construction.
//!
//! **Clock:** latency is `Instant::elapsed()` (monotonic) around a *single*
//! attempt. Monotonic is an intentional improvement over the Haskell, which used
//! the `Realtime` clock. Timing per attempt rather than across the whole retry
//! loop matches the Haskell, which re-read the clock at the top of every attempt:
//! a request that failed twice and succeeded on the third try is a sample of how
//! long the server took to answer, not of how long we spent sleeping between
//! tries. Including backoff would inject multi-second "successful" samples into
//! p99 and expected shortfall. `requested_at` keeps a wall-clock `SystemTime` for
//! the CSV timestamp, stamped when the *first* attempt began.

use std::future::Future;
use std::time::{Duration, Instant, SystemTime};

use gauntlet_core::{Nanoseconds, RetrySettings, TestingResponse, ValidationError};

use crate::client::{self, PreparedEndpoint, RawResponse, TransportError};
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
///
/// Returns the outcome alongside the elapsed time of the attempt that produced
/// it — never the sum across attempts, and never including a backoff sleep.
pub async fn with_retry<F, Fut>(
    retry: &RetrySettings,
    mut op: F,
) -> (std::result::Result<RawResponse, TransportError>, Duration)
where
    F: FnMut() -> Fut,
    Fut: Future<Output = std::result::Result<RawResponse, TransportError>>,
{
    let mut delay = retry.initial_delay_ms.get() as f64;
    let mut retries_left = retry.max_attempts;
    loop {
        // Timed per attempt: only the attempt we ultimately report is measured.
        let started = Instant::now();
        let result = op().await;
        let elapsed = started.elapsed();

        match result {
            Ok(resp) => return (Ok(resp), elapsed),
            Err(err) => {
                if err.retryable && retries_left > 0 {
                    retries_left -= 1;
                    tokio::time::sleep(Duration::from_millis(delay as u64)).await;
                    delay = (delay * retry.backoff_multiplier).ceil();
                    continue;
                }
                return (Err(err), elapsed);
            }
        }
    }
}

/// Execute one prepared request, retrying transport failures, and build the
/// outcome. Latency covers the reported attempt only — see the module docs on
/// why backoff sleeps are excluded.
pub async fn execute(
    client: &reqwest::Client,
    endpoint: &PreparedEndpoint,
    retry: &RetrySettings,
) -> RequestOutcome {
    let requested_at = SystemTime::now();

    let (result, elapsed) = with_retry(retry, || client::send(client, endpoint)).await;
    let duration = Nanoseconds(elapsed.as_nanos() as u64);

    match result {
        Ok(raw) => {
            // Validation runs outside the timed window on purpose: it is our
            // bookkeeping, not the server's response time.
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
