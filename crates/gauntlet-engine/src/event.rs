//! The live event stream the TUI renders.
//!
//! The engine emits these as it works; nothing in the measurement path reads
//! them back. The channel is **unbounded and every send is fire-and-forget**
//! (ADR M5-tui §1): a bounded queue would let a slow terminal apply
//! backpressure to the request loop and widen the very latencies being
//! measured, and a closed receiver just means the operator quit the UI while
//! the benchmark ran on.

use tokio::sync::mpsc::UnboundedSender;

/// Something worth showing the operator as it happens.
#[derive(Clone, Debug, PartialEq)]
pub enum BenchmarkEvent {
    /// A target's measurement began: name, 1-based index, expected requests.
    TargetStarted {
        name: String,
        index: usize,
        total_requests: usize,
    },
    /// An endpoint's measurement began: name, 1-based index, endpoint count.
    EndpointStarted {
        name: String,
        index: usize,
        total: usize,
    },
    /// A request came back. `status` is 0 when there was no response at all.
    RequestCompleted { latency_ms: f64, status: u16 },
    /// A request failed at the transport level.
    RequestFailed { message: String },
    /// The measured rate, recomputed as the run proceeds.
    CurrentRpmUpdated { rpm: f64 },
    /// A stepped load mode advanced: 1-based step and its target rate.
    LoadStepChanged { step: usize, target_rpm: f64 },
    /// Free-text progress ("Setting up staging...").
    Status { message: String },
    /// Measurement finished normally.
    Finished,
    /// Measurement stopped early.
    Failed { message: String },
}

/// The engine's end of the event channel. `None` in headless runs, which is why
/// every emit site goes through [`emit`] rather than touching a sender.
pub type EventSink = Option<UnboundedSender<BenchmarkEvent>>;

/// Send an event if anyone is listening, ignoring a closed channel.
///
/// Errors are deliberately dropped: a UI that has gone away must never fail a
/// benchmark that is otherwise fine.
pub fn emit(sink: &EventSink, event: BenchmarkEvent) {
    if let Some(tx) = sink {
        let _ = tx.send(event);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn events_reach_a_live_receiver() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let sink: EventSink = Some(tx);

        emit(
            &sink,
            BenchmarkEvent::RequestCompleted {
                latency_ms: 12.5,
                status: 200,
            },
        );

        assert_eq!(
            rx.recv().await,
            Some(BenchmarkEvent::RequestCompleted {
                latency_ms: 12.5,
                status: 200
            })
        );
    }

    #[tokio::test]
    async fn emitting_into_a_closed_channel_is_silent() {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        drop(rx);
        let sink: EventSink = Some(tx);

        // The operator quitting the UI must not disturb the run.
        emit(&sink, BenchmarkEvent::Finished);
    }

    #[test]
    fn a_headless_run_has_no_sink_and_emits_nothing() {
        emit(&None, BenchmarkEvent::Finished);
    }
}
