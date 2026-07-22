//! The domain model: what a trace, a span, and a query window are once the
//! Tempo/OTLP wire format has been decoded.
//!
//! These types are deliberately *not* the wire types. Tempo's JSON is OTLP
//! shaped — nested `batches → scopeSpans → spans`, timestamps as decimal
//! strings because they exceed JSON's safe integer range, attributes as
//! `{key, value: {stringValue: ...}}` triples. Decoding that shape lives in
//! [`crate::client`]; everything downstream works with the flat, already-typed
//! [`Span`] below. The Haskell port made the same split (`Tracing.Types` vs the
//! ad-hoc parsers in `Tracing.Client`), only there the domain type carried
//! `FromJSON` instances it never actually used.

use std::collections::BTreeMap;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

/// The wall-clock window a benchmark ran in. Tempo's search API is scoped by
/// time, so this is the only way to avoid pulling every trace the backend has.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceWindow {
    pub start: SystemTime,
    pub end: SystemTime,
}

impl TraceWindow {
    /// A window from Unix-epoch nanosecond bounds, which is how the engine
    /// records run timings.
    pub fn from_unix_nanos(start_ns: u64, end_ns: u64) -> Self {
        TraceWindow {
            start: UNIX_EPOCH + Duration::from_nanos(start_ns),
            end: UNIX_EPOCH + Duration::from_nanos(end_ns),
        }
    }

    /// Inclusive-by-construction Unix-second bounds for the search query: the
    /// start floors and the end ceilings, so a sub-second run still spans at
    /// least one whole second and cannot exclude its own spans.
    pub fn unix_seconds(&self) -> (u64, u64) {
        let start = self.start.duration_since(UNIX_EPOCH).unwrap_or_default();
        let end = self.end.duration_since(UNIX_EPOCH).unwrap_or_default();
        let end_secs = if end.subsec_nanos() > 0 {
            end.as_secs() + 1
        } else {
            end.as_secs()
        };
        (start.as_secs(), end_secs.max(start.as_secs()))
    }
}

/// Parameters for a TraceQL search. Only the service filter and the window are
/// ever populated by the benchmark runner today; the optional filters exist
/// because the Tempo query surface supports them and callers may want them.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraceQuery {
    /// `resource.service.name` filter — required, since an unfiltered TraceQL
    /// query against a shared Tempo is a denial-of-service on yourself.
    pub service: String,
    /// Optional span-name (`name`) equality filter.
    pub span_name: Option<String>,
    pub window: TraceWindow,
    /// Optional minimum-duration filter in TraceQL duration syntax, e.g. `100ms`.
    pub min_duration: Option<String>,
    /// Upper bound on traces returned by the search endpoint.
    pub limit: u32,
}

/// Tempo's `limit` for a benchmark-window search. Matches the Haskell client;
/// large enough that a normal run is never truncated.
pub const DEFAULT_SEARCH_LIMIT: u32 = 10_000;

impl TraceQuery {
    /// The query a benchmark run issues: everything from one service, inside
    /// the run's window.
    pub fn for_service(service: impl Into<String>, window: TraceWindow) -> Self {
        TraceQuery {
            service: service.into(),
            span_name: None,
            window,
            min_duration: None,
            limit: DEFAULT_SEARCH_LIMIT,
        }
    }

    /// Render the TraceQL selector, e.g. `{resource.service.name="api" && name="GET /x"}`.
    ///
    /// Values are quoted and backslash/quote escaped — a service name is config
    /// text, and an unescaped quote would silently change the query's meaning
    /// rather than fail.
    pub fn to_traceql(&self) -> String {
        let mut conditions = vec![format!(
            "resource.service.name=\"{}\"",
            traceql_escape(&self.service)
        )];
        if let Some(name) = &self.span_name {
            conditions.push(format!("name=\"{}\"", traceql_escape(name)));
        }
        if let Some(min) = &self.min_duration {
            conditions.push(format!("duration>{min}"));
        }
        format!("{{{}}}", conditions.join(" && "))
    }
}

fn traceql_escape(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}

/// One entry of a Tempo search result: enough to identify a trace and fetch it.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TraceMetadata {
    pub trace_id: String,
    pub root_service_name: String,
    pub root_trace_name: String,
    pub start_time_unix_nano: u64,
    pub duration_ms: f64,
}

/// A complete trace: every span Tempo returned for one trace ID.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Trace {
    pub trace_id: String,
    pub spans: Vec<Span>,
    /// Wall-clock span of the whole trace: last end minus first start.
    pub total_duration_ns: u64,
}

impl Trace {
    /// Build a trace from its spans, deriving the total duration as the extent
    /// of the span set. Empty span lists give a zero duration rather than an
    /// error — Tempo can return a trace whose batches carry no spans.
    pub fn from_spans(trace_id: impl Into<String>, spans: Vec<Span>) -> Self {
        let total_duration_ns = match (
            spans.iter().map(|s| s.start_time_ns).min(),
            spans.iter().map(|s| s.end_time_ns).max(),
        ) {
            (Some(first), Some(last)) => last.saturating_sub(first),
            _ => 0,
        };
        Trace {
            trace_id: trace_id.into(),
            spans,
            total_duration_ns,
        }
    }
}

/// A single span, flattened out of the OTLP batch nesting. `service_name` is
/// lifted from the enclosing batch's resource attributes, because that is where
/// OTLP puts it and no consumer wants to walk back up to find it.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub span_id: String,
    pub parent_span_id: Option<String>,
    pub name: String,
    pub service_name: String,
    pub kind: SpanKind,
    pub start_time_ns: u64,
    pub end_time_ns: u64,
    pub duration_ns: u64,
    pub status: SpanStatus,
    /// Span attributes, stringified. A `BTreeMap` so serialized traces are
    /// byte-stable across runs, which makes the raw dump diffable.
    pub attributes: BTreeMap<String, String>,
}

impl Span {
    /// Duration in milliseconds, the unit every report renders.
    pub fn duration_ms(&self) -> f64 {
        self.duration_ns as f64 / 1_000_000.0
    }
}

/// OpenTelemetry span kind. `Unspecified` absorbs both a missing field and an
/// unrecognized one: a new OTLP kind must not make a trace undecodable.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpanKind {
    #[default]
    Unspecified,
    Internal,
    Server,
    Client,
    Producer,
    Consumer,
}

impl SpanKind {
    /// Decode the OTLP numeric encoding (`1`–`5`).
    pub fn from_code(code: i64) -> Self {
        match code {
            1 => SpanKind::Internal,
            2 => SpanKind::Server,
            3 => SpanKind::Client,
            4 => SpanKind::Producer,
            5 => SpanKind::Consumer,
            _ => SpanKind::Unspecified,
        }
    }

    /// Decode the OTLP string encoding (`SPAN_KIND_SERVER`, …). Tempo emits
    /// whichever form its ingest path produced, so both must be accepted.
    pub fn from_name(name: &str) -> Self {
        match name {
            "SPAN_KIND_INTERNAL" => SpanKind::Internal,
            "SPAN_KIND_SERVER" => SpanKind::Server,
            "SPAN_KIND_CLIENT" => SpanKind::Client,
            "SPAN_KIND_PRODUCER" => SpanKind::Producer,
            "SPAN_KIND_CONSUMER" => SpanKind::Consumer,
            _ => SpanKind::Unspecified,
        }
    }
}

/// OpenTelemetry span status. `Unset` is the OTLP default and is *not* an
/// error — only `Error` counts against a span in the report.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpanStatus {
    #[default]
    Unset,
    Ok,
    Error,
}

impl SpanStatus {
    /// Decode the OTLP numeric status code (`0` unset, `1` ok, `2` error).
    pub fn from_code(code: i64) -> Self {
        match code {
            1 => SpanStatus::Ok,
            2 => SpanStatus::Error,
            _ => SpanStatus::Unset,
        }
    }

    /// Decode the OTLP string status code.
    pub fn from_name(name: &str) -> Self {
        match name {
            "STATUS_CODE_OK" => SpanStatus::Ok,
            "STATUS_CODE_ERROR" => SpanStatus::Error,
            _ => SpanStatus::Unset,
        }
    }

    pub fn is_error(self) -> bool {
        matches!(self, SpanStatus::Error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start_ns: u64, end_ns: u64) -> Span {
        Span {
            span_id: "a".into(),
            parent_span_id: None,
            name: "n".into(),
            service_name: "svc".into(),
            kind: SpanKind::Server,
            start_time_ns: start_ns,
            end_time_ns: end_ns,
            duration_ns: end_ns.saturating_sub(start_ns),
            status: SpanStatus::Ok,
            attributes: BTreeMap::new(),
        }
    }

    #[test]
    fn traceql_selector_contains_only_the_service_filter_by_default() {
        let q = TraceQuery::for_service("checkout", TraceWindow::from_unix_nanos(0, 1));
        assert_eq!(q.to_traceql(), r#"{resource.service.name="checkout"}"#);
    }

    #[test]
    fn traceql_selector_conjoins_span_name_and_min_duration_when_present() {
        let mut q = TraceQuery::for_service("checkout", TraceWindow::from_unix_nanos(0, 1));
        q.span_name = Some("GET /cart".into());
        q.min_duration = Some("100ms".into());
        assert_eq!(
            q.to_traceql(),
            r#"{resource.service.name="checkout" && name="GET /cart" && duration>100ms}"#
        );
    }

    #[test]
    fn traceql_escapes_quotes_so_a_service_name_cannot_alter_the_query() {
        let q = TraceQuery::for_service(r#"a" || true || ""#, TraceWindow::from_unix_nanos(0, 1));
        assert_eq!(
            q.to_traceql(),
            r#"{resource.service.name="a\" || true || \""}"#
        );
    }

    #[test]
    fn window_seconds_floor_the_start_and_ceiling_the_end() {
        // 1.5s → 2.75s must cover seconds 1 through 3.
        let w = TraceWindow::from_unix_nanos(1_500_000_000, 2_750_000_000);
        assert_eq!(w.unix_seconds(), (1, 3));
    }

    #[test]
    fn a_sub_second_window_still_covers_at_least_one_second() {
        let w = TraceWindow::from_unix_nanos(5_000_000_000, 5_000_000_001);
        let (start, end) = w.unix_seconds();
        assert_eq!(start, 5);
        assert!(end > start);
    }

    #[test]
    fn trace_total_duration_is_the_extent_of_its_spans() {
        let t = Trace::from_spans("abc", vec![span(100, 200), span(50, 175)]);
        assert_eq!(t.total_duration_ns, 150);
    }

    #[test]
    fn a_trace_with_no_spans_has_zero_duration_rather_than_failing() {
        let t = Trace::from_spans("abc", vec![]);
        assert_eq!(t.total_duration_ns, 0);
        assert!(t.spans.is_empty());
    }

    #[test]
    fn unknown_span_kinds_and_statuses_degrade_to_their_defaults() {
        assert_eq!(SpanKind::from_code(99), SpanKind::Unspecified);
        assert_eq!(
            SpanKind::from_name("SPAN_KIND_FUTURE"),
            SpanKind::Unspecified
        );
        assert_eq!(SpanStatus::from_code(-1), SpanStatus::Unset);
        assert_eq!(
            SpanStatus::from_name("STATUS_CODE_MAYBE"),
            SpanStatus::Unset
        );
    }
}
