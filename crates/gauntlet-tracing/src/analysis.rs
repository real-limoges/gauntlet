//! Aggregation: turning a pile of spans into the numbers a report shows.
//!
//! The Haskell version grouped spans by name alone. That is wrong the moment a
//! trace crosses services — `handle` in the gateway and `handle` in the backend
//! collapsed into one row with a bimodal, meaningless distribution. Here the
//! grouping key is **(service, span name)**, which is the granularity anyone
//! reading the table is actually reasoning about.
//!
//! [`analyze`] is the whole subsystem's entry point. Its contract is narrow on
//! purpose (ADR M6-A §7): `Ok(None)` means *tracing is switched off*, and
//! nothing else does. A window that legitimately contains no traces still
//! returns an analysis — an empty one — so the caller can distinguish "not
//! configured" from "configured, but Tempo had nothing", which are very
//! different things to be told at 2am.

use std::collections::BTreeMap;

use gauntlet_core::TempoSettings;
use serde::{Deserialize, Serialize};

use crate::client::TempoClient;
use crate::error::Result;
use crate::types::{Span, Trace, TraceQuery, TraceWindow};

/// Duration statistics for one (service, span name) group. All values are
/// milliseconds, matching every other latency figure gauntlet prints.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SpanAggregation {
    pub service_name: String,
    pub span_name: String,
    pub count: usize,
    /// Spans in this group whose OTLP status was `Error`.
    pub error_count: usize,
    pub mean_ms: f64,
    pub std_dev_ms: f64,
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
}

/// The finished trace analysis for one benchmark window.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TraceAnalysis {
    /// The service the search was filtered to, echoed back for the report header.
    pub service_name: String,
    pub window: TraceWindow,
    pub trace_count: usize,
    pub span_count: usize,
    pub error_span_count: usize,
    /// Per-(service, span) statistics, sorted by P95 descending — the slowest
    /// thing is what the reader came for, so it goes first.
    pub spans: Vec<SpanAggregation>,
    /// The traces as fetched, retained so the caller can dump them alongside
    /// the run's other artifacts (`traces-<timestamp>.json` in the Haskell tree).
    pub traces: Vec<Trace>,
}

impl TraceAnalysis {
    /// True when Tempo returned nothing usable for the window — a normal
    /// outcome when spans have not been ingested yet, not an error.
    pub fn is_empty(&self) -> bool {
        self.spans.is_empty()
    }
}

/// Whether the `tempo` section switches trace analysis on.
///
/// `enabled` is `Option<bool>` and documented as defaulting to **true**: a
/// config author who bothered to write a `tempo` block wants tracing, and
/// should not have to also write `"enabled": true`.
pub fn is_enabled(settings: &TempoSettings) -> bool {
    settings.enabled.unwrap_or(true)
}

/// Fetch and analyze traces for the run's window.
///
/// Returns `Ok(None)` when tracing is disabled in settings. Errors are the
/// caller's to *log*, never to fail on: trace analysis is a diagnostic, and a
/// Tempo outage must not turn a clean benchmark into a failed run.
pub async fn analyze(
    settings: &TempoSettings,
    window: TraceWindow,
) -> Result<Option<TraceAnalysis>> {
    if !is_enabled(settings) {
        return Ok(None);
    }

    let client = TempoClient::new(settings)?;
    let query = TraceQuery::for_service(&settings.service_name, window);
    let traces = client.fetch_traces_for_window(&query).await?;

    Ok(Some(build_analysis(&settings.service_name, window, traces)))
}

/// Assemble an analysis from already-fetched traces. Split out from [`analyze`]
/// so the aggregation is testable without a Tempo (or a network) in sight.
pub fn build_analysis(
    service_name: &str,
    window: TraceWindow,
    traces: Vec<Trace>,
) -> TraceAnalysis {
    let spans: Vec<&Span> = traces.iter().flat_map(|t| t.spans.iter()).collect();
    let error_span_count = spans.iter().filter(|s| s.status.is_error()).count();

    TraceAnalysis {
        service_name: service_name.to_string(),
        window,
        trace_count: traces.len(),
        span_count: spans.len(),
        error_span_count,
        spans: aggregate_spans(&spans),
        traces,
    }
}

/// Group spans by (service, name) and compute each group's duration statistics.
///
/// The result is sorted by P95 descending, with the group key as a tiebreaker
/// so equal-latency rows do not reshuffle between runs.
pub fn aggregate_spans(spans: &[&Span]) -> Vec<SpanAggregation> {
    let mut groups: BTreeMap<(&str, &str), Vec<&Span>> = BTreeMap::new();
    for span in spans {
        groups
            .entry((span.service_name.as_str(), span.name.as_str()))
            .or_default()
            .push(span);
    }

    let mut aggregations: Vec<SpanAggregation> = groups
        .into_iter()
        .map(|((service, name), group)| aggregate_group(service, name, &group))
        .collect();

    aggregations.sort_by(|a, b| {
        b.p95_ms
            .total_cmp(&a.p95_ms)
            .then_with(|| a.service_name.cmp(&b.service_name))
            .then_with(|| a.span_name.cmp(&b.span_name))
    });
    aggregations
}

fn aggregate_group(service: &str, name: &str, group: &[&Span]) -> SpanAggregation {
    // Sorted once: percentiles need it, and min/max then come for free.
    let mut durations: Vec<f64> = group.iter().map(|s| s.duration_ms()).collect();
    durations.sort_by(f64::total_cmp);

    SpanAggregation {
        service_name: service.to_string(),
        span_name: name.to_string(),
        count: group.len(),
        error_count: group.iter().filter(|s| s.status.is_error()).count(),
        mean_ms: gauntlet_stats::mean(&durations),
        std_dev_ms: gauntlet_stats::std_dev(&durations),
        p50_ms: gauntlet_stats::percentile_sorted(0.50, &durations),
        p95_ms: gauntlet_stats::percentile_sorted(0.95, &durations),
        p99_ms: gauntlet_stats::percentile_sorted(0.99, &durations),
        min_ms: durations.first().copied().unwrap_or(0.0),
        max_ms: durations.last().copied().unwrap_or(0.0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{SpanKind, SpanStatus};

    fn window() -> TraceWindow {
        TraceWindow::from_unix_nanos(1_750_000_000_000_000_000, 1_750_000_010_000_000_000)
    }

    fn span(service: &str, name: &str, duration_ms: f64, status: SpanStatus) -> Span {
        let duration_ns = (duration_ms * 1_000_000.0) as u64;
        Span {
            span_id: format!("{service}-{name}-{duration_ms}"),
            parent_span_id: None,
            name: name.to_string(),
            service_name: service.to_string(),
            kind: SpanKind::Server,
            start_time_ns: 1_750_000_000_000_000_000,
            end_time_ns: 1_750_000_000_000_000_000 + duration_ns,
            duration_ns,
            status,
            attributes: BTreeMap::new(),
        }
    }

    fn ok(service: &str, name: &str, duration_ms: f64) -> Span {
        span(service, name, duration_ms, SpanStatus::Ok)
    }

    fn settings(enabled: Option<bool>) -> TempoSettings {
        TempoSettings {
            url: "http://tempo:3200".into(),
            service_name: "api".into(),
            enabled,
            auth_token: None,
        }
    }

    #[test]
    fn tracing_defaults_to_enabled_when_the_tempo_section_omits_the_flag() {
        assert!(is_enabled(&settings(None)));
        assert!(is_enabled(&settings(Some(true))));
        assert!(!is_enabled(&settings(Some(false))));
    }

    #[tokio::test]
    async fn analyze_returns_none_without_touching_the_network_when_disabled() {
        // The URL is unroutable on purpose: reaching the client at all would
        // hang or error, so `Ok(None)` proves the gate short-circuits first.
        let mut disabled = settings(Some(false));
        disabled.url = "http://192.0.2.1:1/".into();
        assert!(analyze(&disabled, window()).await.expect("gated").is_none());
    }

    #[test]
    fn spans_are_grouped_by_service_and_name_not_name_alone() {
        let spans = [
            ok("gateway", "handle", 1.0),
            ok("backend", "handle", 100.0),
            ok("gateway", "handle", 3.0),
        ];
        let refs: Vec<&Span> = spans.iter().collect();
        let aggs = aggregate_spans(&refs);

        assert_eq!(aggs.len(), 2);
        // Sorted by P95 descending: the backend's 100ms group leads.
        assert_eq!(aggs[0].service_name, "backend");
        assert_eq!(aggs[0].count, 1);
        assert_eq!(aggs[1].service_name, "gateway");
        assert_eq!(aggs[1].count, 2);
    }

    #[test]
    fn aggregation_statistics_match_the_shared_stats_helpers() {
        let spans: Vec<Span> = [10.0, 20.0, 30.0, 40.0]
            .iter()
            .map(|d| ok("api", "q", *d))
            .collect();
        let refs: Vec<&Span> = spans.iter().collect();
        let agg = &aggregate_spans(&refs)[0];

        assert_eq!(agg.count, 4);
        assert_eq!(agg.mean_ms, 25.0);
        assert_eq!(agg.min_ms, 10.0);
        assert_eq!(agg.max_ms, 40.0);
        assert_eq!(agg.p50_ms, 25.0);
        // R-7 interpolation on 4 points: p95 index = 0.95·3 = 2.85.
        assert!((agg.p95_ms - 38.5).abs() < 1e-9, "{}", agg.p95_ms);
        assert!((agg.std_dev_ms - 12.909_944_487_358_056).abs() < 1e-9);
    }

    #[test]
    fn a_single_span_group_has_zero_dispersion_and_equal_percentiles() {
        let spans = [ok("api", "q", 7.0)];
        let refs: Vec<&Span> = spans.iter().collect();
        let agg = &aggregate_spans(&refs)[0];

        assert_eq!(agg.count, 1);
        assert_eq!(agg.std_dev_ms, 0.0);
        assert_eq!((agg.p50_ms, agg.p95_ms, agg.p99_ms), (7.0, 7.0, 7.0));
    }

    #[test]
    fn error_spans_are_counted_per_group_and_in_total() {
        let traces = vec![Trace::from_spans(
            "t1",
            vec![
                ok("api", "q", 1.0),
                span("api", "q", 2.0, SpanStatus::Error),
                span("api", "r", 3.0, SpanStatus::Unset),
            ],
        )];
        let analysis = build_analysis("api", window(), traces);

        assert_eq!(analysis.span_count, 3);
        assert_eq!(analysis.error_span_count, 1);
        let q = analysis
            .spans
            .iter()
            .find(|a| a.span_name == "q")
            .expect("group q");
        assert_eq!(q.error_count, 1);
        // `Unset` is OTLP's default, not a failure.
        let r = analysis
            .spans
            .iter()
            .find(|a| a.span_name == "r")
            .expect("group r");
        assert_eq!(r.error_count, 0);
    }

    #[test]
    fn an_empty_window_analyzes_to_an_empty_but_present_analysis() {
        let analysis = build_analysis("api", window(), vec![]);
        assert!(analysis.is_empty());
        assert_eq!(analysis.trace_count, 0);
        assert_eq!(analysis.span_count, 0);
        assert_eq!(analysis.service_name, "api");
    }

    #[test]
    fn traces_that_carry_no_spans_count_as_traces_but_contribute_nothing() {
        let analysis = build_analysis("api", window(), vec![Trace::from_spans("t1", vec![])]);
        assert_eq!(analysis.trace_count, 1);
        assert_eq!(analysis.span_count, 0);
        assert!(analysis.is_empty());
    }

    #[test]
    fn spans_from_every_trace_in_the_window_are_pooled() {
        let traces = vec![
            Trace::from_spans("t1", vec![ok("api", "q", 1.0)]),
            Trace::from_spans("t2", vec![ok("api", "q", 3.0)]),
        ];
        let analysis = build_analysis("api", window(), traces);
        assert_eq!(analysis.trace_count, 2);
        assert_eq!(analysis.spans.len(), 1);
        assert_eq!(analysis.spans[0].count, 2);
        assert_eq!(analysis.spans[0].mean_ms, 2.0);
    }

    #[test]
    fn equal_p95_groups_are_ordered_deterministically_by_key() {
        let spans = [ok("z-svc", "a", 5.0), ok("a-svc", "b", 5.0)];
        let refs: Vec<&Span> = spans.iter().collect();
        let aggs = aggregate_spans(&refs);
        assert_eq!(aggs[0].service_name, "a-svc");
        assert_eq!(aggs[1].service_name, "z-svc");
    }
}
