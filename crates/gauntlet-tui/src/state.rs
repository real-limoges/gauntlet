//! The UI's state and its reducer.
//!
//! Pure data and a pure-ish transition function (it takes `now` rather than
//! reading the clock), exactly as the Haskell `updateState` was. All the logic
//! worth testing — rolling windows, counters, percentile recomputation — lives
//! here and is tested with no terminal involved. Rendering only ever reads a
//! `&State`.

use std::collections::VecDeque;
use std::time::Instant;

use gauntlet_engine::BenchmarkEvent;

/// Recent latencies kept for the rolling statistics.
pub const ROLLING_WINDOW: usize = 100;
/// Success/failure marks kept for the timeline strip.
pub const TIMELINE_CAPACITY: usize = 120;
/// Recent error messages shown to the operator.
pub const ERROR_CAPACITY: usize = 5;

/// Descriptive statistics over the rolling window.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RollingStats {
    pub mean_ms: f64,
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
}

/// Everything the UI draws.
#[derive(Clone, Debug)]
pub struct State {
    pub target: String,
    pub endpoint: String,
    pub endpoint_index: usize,
    pub total_endpoints: usize,

    pub completed: usize,
    pub total_requests: usize,
    pub success_count: usize,
    pub error_count: usize,

    /// Set on the first request, so elapsed time excludes setup hooks.
    pub started_at: Option<Instant>,
    pub elapsed_s: f64,

    pub recent_latencies: VecDeque<f64>,
    pub recent_errors: VecDeque<String>,
    /// Per-request success marks, oldest first.
    pub timeline: VecDeque<bool>,
    pub rolling: Option<RollingStats>,

    pub finished: bool,
    pub error: Option<String>,
    pub status: String,

    pub current_rpm: Option<f64>,
    pub target_rpm: Option<f64>,
    pub current_step: Option<usize>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            target: String::new(),
            endpoint: String::new(),
            endpoint_index: 0,
            total_endpoints: 0,
            completed: 0,
            total_requests: 0,
            success_count: 0,
            error_count: 0,
            started_at: None,
            elapsed_s: 0.0,
            recent_latencies: VecDeque::with_capacity(ROLLING_WINDOW),
            recent_errors: VecDeque::with_capacity(ERROR_CAPACITY),
            timeline: VecDeque::with_capacity(TIMELINE_CAPACITY),
            rolling: None,
            finished: false,
            error: None,
            status: String::new(),
            current_rpm: None,
            target_rpm: None,
            current_step: None,
        }
    }
}

impl State {
    /// Fraction of expected requests completed, clamped to `0.0..=1.0`.
    ///
    /// Duration-based load modes have no meaningful total up front, so this
    /// reports 0 rather than a ratio against a number that means nothing.
    pub fn progress(&self) -> f64 {
        if self.total_requests == 0 {
            0.0
        } else {
            (self.completed as f64 / self.total_requests as f64).clamp(0.0, 1.0)
        }
    }

    /// Observed requests per second since the first request.
    pub fn throughput(&self) -> f64 {
        if self.elapsed_s > 0.0 {
            self.completed as f64 / self.elapsed_s
        } else {
            0.0
        }
    }

    /// Share of completed requests that failed, as a fraction.
    pub fn error_rate(&self) -> f64 {
        if self.completed == 0 {
            0.0
        } else {
            self.error_count as f64 / self.completed as f64
        }
    }

    /// Apply one event.
    ///
    /// `now` is passed in rather than read here so the reducer stays
    /// deterministic under test.
    pub fn reduce(&mut self, now: Instant, event: BenchmarkEvent) {
        match event {
            BenchmarkEvent::TargetStarted {
                name,
                total_requests,
                ..
            } => {
                // A new target restarts progress: its counters and rolling
                // window describe that target, not the run so far.
                let status = std::mem::take(&mut self.status);
                *self = State {
                    target: name,
                    total_requests,
                    status,
                    ..Default::default()
                };
            }

            BenchmarkEvent::EndpointStarted { name, index, total } => {
                self.endpoint = name;
                self.endpoint_index = index;
                self.total_endpoints = total;
            }

            BenchmarkEvent::RequestCompleted { latency_ms, status } => {
                let ok = (200..400).contains(&status);
                self.record(now, ok);
                push_bounded(&mut self.recent_latencies, latency_ms, ROLLING_WINDOW);
                self.rolling = Some(rolling_stats(&self.recent_latencies));
            }

            BenchmarkEvent::RequestFailed { message } => {
                self.record(now, false);
                push_bounded(&mut self.recent_errors, message, ERROR_CAPACITY);
            }

            BenchmarkEvent::CurrentRpmUpdated { rpm } => self.current_rpm = Some(rpm),

            BenchmarkEvent::LoadStepChanged { step, target_rpm } => {
                self.current_step = Some(step);
                self.target_rpm = Some(target_rpm);
            }

            BenchmarkEvent::Status { message } => self.status = message,

            BenchmarkEvent::Finished => self.finished = true,

            BenchmarkEvent::Failed { message } => {
                self.finished = true;
                self.error = Some(message);
            }
        }
    }

    /// Counters and clock shared by both request outcomes.
    fn record(&mut self, now: Instant, ok: bool) {
        self.completed += 1;
        if ok {
            self.success_count += 1;
        } else {
            self.error_count += 1;
        }
        push_bounded(&mut self.timeline, ok, TIMELINE_CAPACITY);

        let started = *self.started_at.get_or_insert(now);
        self.elapsed_s = now.saturating_duration_since(started).as_secs_f64();
    }
}

/// Push, dropping the oldest entry once at capacity.
fn push_bounded<T>(queue: &mut VecDeque<T>, item: T, capacity: usize) {
    if queue.len() >= capacity {
        queue.pop_front();
    }
    queue.push_back(item);
}

/// Descriptive statistics over the rolling window.
///
/// Sorts a copy each time: the window is 100 items, so this is cheaper than
/// maintaining an incremental structure and impossible to get subtly wrong.
fn rolling_stats(latencies: &VecDeque<f64>) -> RollingStats {
    if latencies.is_empty() {
        return RollingStats::default();
    }

    let mut sorted: Vec<f64> = latencies.iter().copied().collect();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

    RollingStats {
        mean_ms: sorted.iter().sum::<f64>() / sorted.len() as f64,
        p50_ms: percentile(&sorted, 0.50),
        p95_ms: percentile(&sorted, 0.95),
        p99_ms: percentile(&sorted, 0.99),
        min_ms: sorted[0],
        max_ms: sorted[sorted.len() - 1],
    }
}

/// Nearest-rank percentile over an already-sorted slice.
fn percentile(sorted: &[f64], p: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let index = ((p * sorted.len() as f64).ceil() as usize).saturating_sub(1);
    sorted[index.min(sorted.len() - 1)]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn completed(latency_ms: f64, status: u16) -> BenchmarkEvent {
        BenchmarkEvent::RequestCompleted { latency_ms, status }
    }

    fn apply(state: &mut State, events: Vec<BenchmarkEvent>) {
        let now = Instant::now();
        for event in events {
            state.reduce(now, event);
        }
    }

    #[test]
    fn a_fresh_state_shows_no_progress_and_no_throughput() {
        let state = State::default();
        assert_eq!(state.progress(), 0.0);
        assert_eq!(state.throughput(), 0.0);
        assert_eq!(state.error_rate(), 0.0);
        assert!(state.rolling.is_none());
    }

    #[test]
    fn completions_are_split_into_successes_and_errors_by_status() {
        let mut state = State::default();
        apply(
            &mut state,
            vec![
                completed(10.0, 200),
                completed(11.0, 301),
                completed(12.0, 404),
                completed(13.0, 500),
            ],
        );

        assert_eq!(state.completed, 4);
        assert_eq!(state.success_count, 2, "2xx and 3xx are successes");
        assert_eq!(state.error_count, 2, "4xx and 5xx are not");
        assert_eq!(state.error_rate(), 0.5);
    }

    #[test]
    fn a_transport_failure_counts_as_an_error_and_is_shown() {
        let mut state = State::default();
        apply(
            &mut state,
            vec![BenchmarkEvent::RequestFailed {
                message: "connection refused".into(),
            }],
        );

        assert_eq!(state.completed, 1);
        assert_eq!(state.error_count, 1);
        assert_eq!(state.recent_errors.back().unwrap(), "connection refused");
        assert!(
            state.rolling.is_none(),
            "a failure has no latency to fold into the rolling window"
        );
    }

    #[test]
    fn the_rolling_window_keeps_only_the_most_recent_latencies() {
        let mut state = State::default();
        let events: Vec<_> = (0..ROLLING_WINDOW + 50)
            .map(|i| completed(i as f64, 200))
            .collect();
        apply(&mut state, events);

        assert_eq!(state.recent_latencies.len(), ROLLING_WINDOW);
        assert_eq!(
            *state.recent_latencies.front().unwrap(),
            50.0,
            "the oldest 50 were dropped"
        );
        assert_eq!(state.completed, ROLLING_WINDOW + 50, "but all were counted");
    }

    #[test]
    fn rolling_statistics_track_the_window() {
        let mut state = State::default();
        apply(
            &mut state,
            (1..=100).map(|i| completed(i as f64, 200)).collect(),
        );

        let rolling = state.rolling.expect("stats exist once a request lands");
        assert_eq!(rolling.min_ms, 1.0);
        assert_eq!(rolling.max_ms, 100.0);
        assert_eq!(rolling.mean_ms, 50.5);
        assert_eq!(rolling.p50_ms, 50.0);
        assert_eq!(rolling.p95_ms, 95.0);
        assert_eq!(rolling.p99_ms, 99.0);
    }

    #[test]
    fn the_timeline_is_bounded_and_ordered_oldest_first() {
        let mut state = State::default();
        apply(&mut state, vec![completed(1.0, 500), completed(1.0, 200)]);

        assert_eq!(
            state.timeline.iter().copied().collect::<Vec<_>>(),
            [false, true]
        );

        apply(
            &mut state,
            (0..TIMELINE_CAPACITY)
                .map(|_| completed(1.0, 200))
                .collect(),
        );
        assert_eq!(state.timeline.len(), TIMELINE_CAPACITY);
        assert!(
            state.timeline.iter().all(|ok| *ok),
            "the early failure aged out"
        );
    }

    #[test]
    fn only_the_last_few_errors_are_retained() {
        let mut state = State::default();
        apply(
            &mut state,
            (0..ERROR_CAPACITY + 3)
                .map(|i| BenchmarkEvent::RequestFailed {
                    message: format!("error {i}"),
                })
                .collect(),
        );

        assert_eq!(state.recent_errors.len(), ERROR_CAPACITY);
        assert_eq!(state.recent_errors.back().unwrap(), "error 7");
    }

    #[test]
    fn progress_is_a_clamped_fraction_of_the_expected_total() {
        let mut state = State::default();
        apply(
            &mut state,
            vec![BenchmarkEvent::TargetStarted {
                name: "api".into(),
                index: 1,
                total_requests: 10,
            }],
        );
        apply(&mut state, (0..5).map(|_| completed(1.0, 200)).collect());
        assert_eq!(state.progress(), 0.5);

        // A duration-based mode can overshoot its hint; the gauge must not.
        apply(&mut state, (0..20).map(|_| completed(1.0, 200)).collect());
        assert_eq!(state.progress(), 1.0);
    }

    #[test]
    fn a_new_target_resets_counters_but_keeps_the_status_line() {
        let mut state = State::default();
        apply(
            &mut state,
            vec![
                BenchmarkEvent::Status {
                    message: "Setting up staging...".into(),
                },
                completed(10.0, 200),
                completed(20.0, 500),
            ],
        );
        assert_eq!(state.completed, 2);

        apply(
            &mut state,
            vec![BenchmarkEvent::TargetStarted {
                name: "staging".into(),
                index: 2,
                total_requests: 50,
            }],
        );

        assert_eq!(state.target, "staging");
        assert_eq!(state.completed, 0);
        assert_eq!(state.error_count, 0);
        assert!(state.recent_latencies.is_empty());
        assert!(state.rolling.is_none());
        assert_eq!(state.total_requests, 50);
        assert_eq!(
            state.status, "Setting up staging...",
            "the status line describes what is happening now, so it survives"
        );
    }

    #[test]
    fn endpoint_progress_is_tracked_without_disturbing_counters() {
        let mut state = State::default();
        apply(&mut state, vec![completed(10.0, 200)]);
        apply(
            &mut state,
            vec![BenchmarkEvent::EndpointStarted {
                name: "upload".into(),
                index: 2,
                total: 3,
            }],
        );

        assert_eq!(state.endpoint, "upload");
        assert_eq!((state.endpoint_index, state.total_endpoints), (2, 3));
        assert_eq!(state.completed, 1, "endpoints share the target's counters");
    }

    #[test]
    fn load_control_updates_are_recorded_for_display() {
        let mut state = State::default();
        apply(
            &mut state,
            vec![
                BenchmarkEvent::CurrentRpmUpdated { rpm: 1234.5 },
                BenchmarkEvent::LoadStepChanged {
                    step: 3,
                    target_rpm: 2000.0,
                },
            ],
        );

        assert_eq!(state.current_rpm, Some(1234.5));
        assert_eq!(state.current_step, Some(3));
        assert_eq!(state.target_rpm, Some(2000.0));
    }

    #[test]
    fn finishing_and_failing_are_distinguishable() {
        let mut ok = State::default();
        apply(&mut ok, vec![BenchmarkEvent::Finished]);
        assert!(ok.finished && ok.error.is_none());

        let mut bad = State::default();
        apply(
            &mut bad,
            vec![BenchmarkEvent::Failed {
                message: "target unreachable".into(),
            }],
        );
        assert!(bad.finished);
        assert_eq!(bad.error.as_deref(), Some("target unreachable"));
    }

    #[test]
    fn elapsed_time_starts_at_the_first_request_not_at_startup() {
        let mut state = State::default();
        let start = Instant::now();

        // Setup hooks can run for a while before any request is issued; that
        // time must not be counted against the measured throughput.
        state.reduce(
            start,
            BenchmarkEvent::Status {
                message: "setup".into(),
            },
        );
        assert!(state.started_at.is_none());

        state.reduce(start, completed(1.0, 200));
        assert!(state.started_at.is_some());
        assert_eq!(state.elapsed_s, 0.0);
    }
}
