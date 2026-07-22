//! The live event stream, verified against a real run.
//!
//! The TUI is only as correct as this stream, and the failure mode is quiet:
//! a run that works headlessly but emits nothing renders an empty UI.

mod support;

use gauntlet_engine::{run_benchmark_with_events, BenchmarkEvent};

fn config(base: &str, iterations: u32) -> gauntlet_core::BenchmarkConfig {
    support::parse_config(&format!(
        r#"{{
          "targets": [{{ "name": "api", "url": "{base}" }}],
          "settings": {{ "iterations": {iterations}, "concurrency": 2, "warmup": {{ "iterations": 0 }} }},
          "payloads": [{{ "name": "p", "method": "GET", "path": "/x" }}]
        }}"#
    ))
}

async fn collect_events(base: &str, iterations: u32) -> Vec<BenchmarkEvent> {
    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    run_benchmark_with_events(&config(base, iterations), None, Some(tx))
        .await
        .expect("benchmark runs");

    let mut events = Vec::new();
    while let Ok(event) = rx.try_recv() {
        events.push(event);
    }
    events
}

#[tokio::test]
async fn a_run_emits_one_completion_per_request_and_finishes() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;
    let events = collect_events(&mock.base, 5).await;

    let completions = events
        .iter()
        .filter(|e| matches!(e, BenchmarkEvent::RequestCompleted { .. }))
        .count();
    assert_eq!(completions, 5, "one event per request");

    assert!(matches!(
        events.first(),
        Some(BenchmarkEvent::TargetStarted { .. })
    ));
    assert_eq!(events.last(), Some(&BenchmarkEvent::Finished));
}

#[tokio::test]
async fn a_server_error_is_a_completion_carrying_its_status_not_a_failure() {
    let mock = support::start(500, serde_json::json!({"bad": true}), 0).await;
    let events = collect_events(&mock.base, 3).await;

    let statuses: Vec<u16> = events
        .iter()
        .filter_map(|e| match e {
            BenchmarkEvent::RequestCompleted { status, .. } => Some(*status),
            _ => None,
        })
        .collect();

    assert_eq!(statuses, [500, 500, 500], "a 500 answered, so it completed");
    assert!(
        !events
            .iter()
            .any(|e| matches!(e, BenchmarkEvent::RequestFailed { .. })),
        "RequestFailed is for transport failures only"
    );
}

#[tokio::test]
async fn an_unreachable_target_emits_transport_failures() {
    let events = collect_events(&support::dead_url(), 2).await;

    assert!(events
        .iter()
        .any(|e| matches!(e, BenchmarkEvent::RequestFailed { .. })));
    assert_eq!(events.last(), Some(&BenchmarkEvent::Finished));
}

/// The point of the live stream is that it is *live*. Events used to be emitted
/// from the collection loop, which only runs once every task has been spawned —
/// so the UI saw nothing during the run and the whole run at once at the end,
/// which made its elapsed time ~0 and its throughput nonsense.
#[tokio::test]
async fn events_arrive_while_the_run_is_still_going() {
    // 8 requests, one at a time, 60ms each: the run takes ~half a second, so an
    // event that arrives promptly is unambiguously mid-run.
    let mock = support::start(200, serde_json::json!({"ok": true}), 60).await;
    let cfg = support::parse_config(&format!(
        r#"{{
          "targets": [{{ "name": "api", "url": "{}" }}],
          "settings": {{ "iterations": 8, "concurrency": 1, "warmup": {{ "iterations": 0 }} }},
          "payloads": [{{ "name": "p", "method": "GET", "path": "/x" }}]
        }}"#,
        mock.base
    ));

    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    let started = std::time::Instant::now();
    let run = tokio::spawn(async move { run_benchmark_with_events(&cfg, None, Some(tx)).await });

    // Wait for the first *request* event, ignoring the lifecycle ones.
    let mut first_request_at = None;
    while let Some(event) = rx.recv().await {
        if matches!(event, BenchmarkEvent::RequestCompleted { .. }) {
            first_request_at = Some(started.elapsed());
            break;
        }
    }

    let first = first_request_at.expect("a request event arrived");
    let total = {
        run.await.expect("task joins").expect("benchmark runs");
        started.elapsed()
    };

    assert!(
        first < total / 2,
        "first event at {first:?} should land well before the run ended at {total:?}"
    );
}

#[tokio::test]
async fn a_headless_run_needs_no_receiver_and_does_not_stall() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;

    // The receiver is dropped immediately: every send fails, and the run must
    // neither block nor error.
    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
    drop(rx);

    let run = run_benchmark_with_events(&config(&mock.base, 4), None, Some(tx))
        .await
        .expect("a run outliving its UI still succeeds");
    assert_eq!(run.targets[0].endpoints[0].outcomes.len(), 4);
}
