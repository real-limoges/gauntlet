//! Request execution: counts, warmup, the concurrency ceiling, and retry policy.

mod support;

use std::num::NonZeroU32;

use gauntlet_core::{extract_durations, RetrySettings};
use gauntlet_engine::{exec, run_benchmark};

use support::{dead_url, parse_config, start};

/// `iterations` requests in, exactly that many responses out (warmup off).
#[tokio::test]
async fn fixed_iterations_produce_one_response_each() {
    let mock = start(200, serde_json::json!({"ok": true}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":10,"concurrency":2,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let ep = &run.targets[0].endpoints[0];

    assert_eq!(ep.outcomes.len(), 10);
    assert_eq!(mock.request_count(), 10);
    assert!(ep
        .outcomes
        .iter()
        .all(|o| o.response.status == 200 && o.response.error.is_none()));
}

/// Warmup requests hit the server but are not counted in the results.
#[tokio::test]
async fn warmup_requests_are_discarded() {
    let mock = start(200, serde_json::json!({}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":5,"concurrency":1,"warmup":{{"iterations":3}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 5);
    assert_eq!(mock.request_count(), 8, "3 warmup + 5 measured");
}

/// The concurrency semaphore caps simultaneous in-flight requests.
#[tokio::test]
async fn concurrency_ceiling_is_honored() {
    let mock = start(200, serde_json::json!({}), 40).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":24,"concurrency":4,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    run_benchmark(&cfg, None).await.unwrap();
    assert!(
        mock.max_in_flight() <= 4,
        "peak in-flight {} exceeded concurrency 4",
        mock.max_in_flight()
    );
    assert!(mock.max_in_flight() >= 2, "no concurrency observed");
}

/// An HTTP 500 is a *response*, not a transport failure: it is captured and
/// never retried.
#[tokio::test]
async fn http_500_is_captured_not_retried() {
    let mock = start(500, serde_json::json!({"err": "boom"}), 0).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":1,"concurrency":1,"warmup":{{"iterations":0}},
                         "retry":{{"max_attempts":3,"initial_delay_ms":1}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let ep = &run.targets[0].endpoints[0];
    assert_eq!(mock.request_count(), 1, "500 must not be retried");
    assert_eq!(ep.outcomes[0].response.status, 500);
    assert!(ep.outcomes[0].response.error.is_none());
}

/// A connection-refused target exhausts retries, yields an errored response with
/// status 0, and that response is excluded from the latency samples.
#[tokio::test]
async fn transport_failure_exhausts_retries_and_is_excluded() {
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"t","url":"{}"}}],
            "settings":{{"iterations":1,"concurrency":1,"warmup":{{"iterations":0}},
                         "request_timeout_secs":2,
                         "retry":{{"max_attempts":2,"initial_delay_ms":1}}}},
            "payloads":[{{"name":"p","method":"GET","path":"/ping"}}]}}"#,
        dead_url()
    ));

    let run = run_benchmark(&cfg, None).await.unwrap();
    let ep = &run.targets[0].endpoints[0];
    assert_eq!(ep.outcomes[0].response.status, 0);
    assert!(ep.outcomes[0].response.error.is_some());
    let responses: Vec<_> = ep.outcomes.iter().map(|o| o.response.clone()).collect();
    assert!(extract_durations(&responses).is_empty());
}

/// The retry loop retries only while errors are retryable and attempts remain,
/// then returns the first success.
#[tokio::test]
async fn with_retry_retries_then_succeeds() {
    use gauntlet_engine::client::{RawResponse, TransportError};
    use std::cell::Cell;

    let retry = RetrySettings {
        max_attempts: 5,
        initial_delay_ms: NonZeroU32::new(1).unwrap(),
        backoff_multiplier: 2.0,
    };
    let calls = Cell::new(0);
    let (result, _) = exec::with_retry(&retry, || {
        let n = calls.get() + 1;
        calls.set(n);
        async move {
            if n <= 2 {
                Err(TransportError {
                    message: "refused".into(),
                    retryable: true,
                })
            } else {
                Ok(RawResponse {
                    status: 200,
                    body: bytes::Bytes::new(),
                })
            }
        }
    })
    .await;

    assert!(result.is_ok());
    assert_eq!(calls.get(), 3, "2 failures + 1 success");
}

/// The reported latency is the successful attempt's own duration — not the sum
/// across attempts, and above all not the backoff sleeps between them. A
/// retried-then-successful request is kept as a latency sample, so counting
/// backoff here would inject multi-second samples into p99 and expected
/// shortfall from nothing worse than a transient connection refusal.
#[tokio::test(start_paused = true)]
async fn with_retry_reports_only_the_successful_attempts_duration() {
    use gauntlet_engine::client::{RawResponse, TransportError};
    use std::cell::Cell;
    use std::time::Duration;

    // 10s of backoff across two retries, dwarfing the 20ms success.
    let retry = RetrySettings {
        max_attempts: 5,
        initial_delay_ms: NonZeroU32::new(2_000).unwrap(),
        backoff_multiplier: 4.0,
    };
    let calls = Cell::new(0);
    let (result, elapsed) = exec::with_retry(&retry, || {
        let n = calls.get() + 1;
        calls.set(n);
        async move {
            if n <= 2 {
                // Failing attempts take time too, and must not be counted.
                tokio::time::sleep(Duration::from_millis(500)).await;
                Err(TransportError {
                    message: "refused".into(),
                    retryable: true,
                })
            } else {
                tokio::time::sleep(Duration::from_millis(20)).await;
                Ok(RawResponse {
                    status: 200,
                    body: bytes::Bytes::new(),
                })
            }
        }
    })
    .await;

    assert!(result.is_ok());
    assert_eq!(calls.get(), 3);
    assert!(
        elapsed < Duration::from_millis(100),
        "expected ~20ms (the successful attempt), got {elapsed:?} — \
         backoff or failed attempts are leaking into the latency sample"
    );
}

/// A non-retryable transport error is returned immediately, without retry.
#[tokio::test]
async fn with_retry_does_not_retry_non_retryable() {
    use gauntlet_engine::client::{RawResponse, TransportError};
    use std::cell::Cell;

    let retry = RetrySettings {
        max_attempts: 5,
        initial_delay_ms: NonZeroU32::new(1).unwrap(),
        backoff_multiplier: 2.0,
    };
    let calls = Cell::new(0);
    let (result, _): (std::result::Result<RawResponse, TransportError>, _) =
        exec::with_retry(&retry, || {
            calls.set(calls.get() + 1);
            async {
                Err(TransportError {
                    message: "nope".into(),
                    retryable: false,
                })
            }
        })
        .await;

    assert!(result.is_err());
    assert_eq!(calls.get(), 1);
}
