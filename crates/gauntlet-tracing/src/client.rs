//! The Grafana Tempo HTTP client: TraceQL search, trace fetch, OTLP decoding.
//! See the crate docs for the two endpoints and the forgiving-decode rule.

use std::collections::BTreeMap;

use gauntlet_core::TempoSettings;
use serde::{Deserialize, Deserializer};
use tokio::task::JoinSet;

use crate::error::{Error, Result};
use crate::types::{Span, SpanKind, SpanStatus, Trace, TraceMetadata, TraceQuery};

/// How many `/api/traces/<id>` fetches are in flight at once. Enough to hide
/// per-request latency; low enough not to hammer a shared Tempo while the
/// benchmark's own results are still being written.
const MAX_CONCURRENT_TRACE_FETCHES: usize = 8;

/// Whole-request timeout. Tempo being slow must not extend the run.
const REQUEST_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30);

/// A configured Tempo endpoint. Cloning is cheap — `reqwest::Client` shares its
/// connection pool — which is what lets the fetch fan-out spawn tasks.
#[derive(Clone, Debug)]
pub struct TempoClient {
    http: reqwest::Client,
    /// Base URL with any trailing slash removed, so path concatenation is exact.
    base_url: String,
    auth_token: Option<String>,
}

impl TempoClient {
    /// Build a client from the `tempo` config section. The URL is validated here
    /// so a typo surfaces as `InvalidUrl`, not as a connection error.
    pub fn new(settings: &TempoSettings) -> Result<Self> {
        let base_url = settings.url.trim_end_matches('/').to_string();
        let parsed = reqwest::Url::parse(&base_url).map_err(|e| Error::InvalidUrl {
            url: settings.url.clone(),
            reason: e.to_string(),
        })?;
        if !matches!(parsed.scheme(), "http" | "https") {
            return Err(Error::InvalidUrl {
                url: settings.url.clone(),
                reason: format!("expected an http(s) URL, got scheme `{}`", parsed.scheme()),
            });
        }

        let http = reqwest::Client::builder()
            .timeout(REQUEST_TIMEOUT)
            .build()
            .map_err(Error::Client)?;

        Ok(TempoClient {
            http,
            base_url,
            auth_token: settings.auth_token.clone(),
        })
    }

    /// Search for traces, then fetch each in full. A failed *search* aborts;
    /// a failed individual *fetch* is dropped, so one bad trace costs only itself.
    pub async fn fetch_traces_for_window(&self, query: &TraceQuery) -> Result<Vec<Trace>> {
        let found = self.search(query).await?;

        let mut pending = found.into_iter().map(|m| m.trace_id).enumerate();
        let mut tasks: JoinSet<(usize, Result<Trace>)> = JoinSet::new();
        let mut collected: Vec<(usize, Trace)> = Vec::new();

        let mut spawn_next = |tasks: &mut JoinSet<(usize, Result<Trace>)>| {
            if let Some((idx, id)) = pending.next() {
                let client = self.clone();
                tasks.spawn(async move { (idx, client.fetch_trace(&id).await) });
            }
        };

        for _ in 0..MAX_CONCURRENT_TRACE_FETCHES {
            spawn_next(&mut tasks);
        }
        while let Some(joined) = tasks.join_next().await {
            spawn_next(&mut tasks);
            // A panicked join is treated like a fetch failure: skip the trace.
            if let Ok((idx, Ok(trace))) = joined {
                collected.push((idx, trace));
            }
        }

        // Restore Tempo's result order so repeated runs render identically.
        collected.sort_by_key(|(idx, _)| *idx);
        Ok(collected.into_iter().map(|(_, trace)| trace).collect())
    }

    /// Run a TraceQL search over the query's window.
    pub async fn search(&self, query: &TraceQuery) -> Result<Vec<TraceMetadata>> {
        let (start, end) = query.window.unix_seconds();
        let url = format!("{}/api/search", self.base_url);
        let body = self
            .get(
                &url,
                &[
                    ("q", query.to_traceql()),
                    ("start", start.to_string()),
                    ("end", end.to_string()),
                    ("limit", query.limit.to_string()),
                ],
            )
            .await?;

        let parsed: wire::SearchResponse = decode(&url, &body)?;
        Ok(parsed.traces.into_iter().map(Into::into).collect())
    }

    /// Fetch one complete trace by ID.
    pub async fn fetch_trace(&self, trace_id: &str) -> Result<Trace> {
        let url = format!("{}/api/traces/{trace_id}", self.base_url);
        let body = self.get(&url, &[]).await?;
        let parsed: wire::TraceResponse = decode(&url, &body)?;
        Ok(Trace::from_spans(trace_id, parsed.into_spans()))
    }

    /// GET with the optional bearer token attached, mapping transport failures
    /// and non-2xx statuses into [`Error`].
    async fn get(&self, url: &str, query: &[(&str, String)]) -> Result<Vec<u8>> {
        let mut request = self.http.get(url).query(query);
        if let Some(token) = &self.auth_token {
            request = request.bearer_auth(token);
        }

        let response = request.send().await.map_err(|source| Error::Http {
            url: url.to_string(),
            source,
        })?;

        let status = response.status();
        if !status.is_success() {
            return Err(Error::Status {
                url: url.to_string(),
                status: status.as_u16(),
            });
        }

        response
            .bytes()
            .await
            .map(|b| b.to_vec())
            .map_err(|source| Error::Http {
                url: url.to_string(),
                source,
            })
    }
}

/// Decode a response body, tagging the failure with the URL that produced it.
fn decode<T: for<'de> Deserialize<'de>>(url: &str, body: &[u8]) -> Result<T> {
    serde_json::from_slice(body).map_err(|source| Error::Decode {
        url: url.to_string(),
        source,
    })
}

/// Tempo's wire shapes. Private on purpose: they exist only to be converted
/// into [`crate::types`], and their field names are the API's, not ours.
mod wire {
    use super::*;

    #[derive(Debug, Deserialize)]
    pub(super) struct SearchResponse {
        #[serde(default)]
        pub traces: Vec<SearchTrace>,
    }

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub(super) struct SearchTrace {
        #[serde(rename = "traceID")]
        pub trace_id: String,
        #[serde(default)]
        pub root_service_name: String,
        #[serde(default)]
        pub root_trace_name: String,
        #[serde(default, deserialize_with = "de_u64")]
        pub start_time_unix_nano: u64,
        #[serde(default, deserialize_with = "de_f64")]
        pub duration_ms: f64,
    }

    impl From<SearchTrace> for TraceMetadata {
        fn from(t: SearchTrace) -> Self {
            TraceMetadata {
                trace_id: t.trace_id,
                root_service_name: t.root_service_name,
                root_trace_name: t.root_trace_name,
                start_time_unix_nano: t.start_time_unix_nano,
                duration_ms: t.duration_ms,
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct TraceResponse {
        #[serde(default)]
        pub batches: Vec<Batch>,
    }

    impl TraceResponse {
        /// Flatten `batches → scopeSpans → spans`, pushing each batch's
        /// resource-level `service.name` down onto its spans.
        pub fn into_spans(self) -> Vec<Span> {
            self.batches
                .into_iter()
                .flat_map(|batch| {
                    let service = batch
                        .resource
                        .as_ref()
                        .map(|r| attribute(&r.attributes, "service.name"))
                        .unwrap_or_default();
                    batch
                        .scope_spans
                        .into_iter()
                        .flat_map(|scope| scope.spans)
                        .map(move |span| span.into_domain(&service))
                })
                .collect()
        }
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct Batch {
        #[serde(default)]
        pub resource: Option<Resource>,
        // `instrumentationLibrarySpans` is the pre-1.0 OTLP name; Tempo still
        // serves it for spans ingested by older collectors.
        #[serde(default, rename = "scopeSpans", alias = "instrumentationLibrarySpans")]
        pub scope_spans: Vec<ScopeSpans>,
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct Resource {
        #[serde(default)]
        pub attributes: Vec<KeyValue>,
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct ScopeSpans {
        #[serde(default)]
        pub spans: Vec<WireSpan>,
    }

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub(super) struct WireSpan {
        #[serde(default)]
        pub span_id: String,
        #[serde(default)]
        pub parent_span_id: Option<String>,
        #[serde(default)]
        pub name: String,
        #[serde(default)]
        pub kind: Option<EnumValue>,
        #[serde(default, deserialize_with = "de_u64")]
        pub start_time_unix_nano: u64,
        #[serde(default, deserialize_with = "de_u64")]
        pub end_time_unix_nano: u64,
        #[serde(default)]
        pub status: Option<Status>,
        #[serde(default)]
        pub attributes: Vec<KeyValue>,
    }

    impl WireSpan {
        fn into_domain(self, service_name: &str) -> Span {
            // OTLP guarantees end ≥ start, but a clock-skewed producer can
            // violate it; saturating keeps the duration non-negative.
            let duration_ns = self
                .end_time_unix_nano
                .saturating_sub(self.start_time_unix_nano);
            Span {
                span_id: self.span_id,
                // An empty `parentSpanId` means "root" in OTLP JSON; treating it
                // as a parent would break any future tree reconstruction.
                parent_span_id: self.parent_span_id.filter(|p| !p.is_empty()),
                name: self.name,
                service_name: service_name.to_string(),
                kind: match self.kind {
                    Some(EnumValue::Name(n)) => SpanKind::from_name(&n),
                    Some(EnumValue::Code(c)) => SpanKind::from_code(c),
                    None => SpanKind::Unspecified,
                },
                start_time_ns: self.start_time_unix_nano,
                end_time_ns: self.end_time_unix_nano,
                duration_ns,
                status: match self.status.and_then(|s| s.code) {
                    Some(EnumValue::Name(n)) => SpanStatus::from_name(&n),
                    Some(EnumValue::Code(c)) => SpanStatus::from_code(c),
                    None => SpanStatus::Unset,
                },
                attributes: attribute_map(self.attributes),
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct Status {
        #[serde(default)]
        pub code: Option<EnumValue>,
    }

    /// An OTLP enum field, which may arrive as its ordinal or its name.
    #[derive(Debug, Deserialize)]
    #[serde(untagged)]
    pub(super) enum EnumValue {
        Code(i64),
        Name(String),
    }

    #[derive(Debug, Deserialize)]
    pub(super) struct KeyValue {
        #[serde(default)]
        pub key: String,
        #[serde(default)]
        pub value: Option<AnyValue>,
    }

    /// OTLP's `AnyValue` union. Only the scalar arms are kept — array and
    /// key-value arms stringify to empty, since nothing in the report reads
    /// structured attributes.
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub(super) struct AnyValue {
        #[serde(default)]
        pub string_value: Option<String>,
        // Int64 is JSON-encoded as a string by the OTLP spec, but not always.
        #[serde(default)]
        pub int_value: Option<EnumValue>,
        #[serde(default)]
        pub double_value: Option<f64>,
        #[serde(default)]
        pub bool_value: Option<bool>,
    }

    impl AnyValue {
        fn to_text(&self) -> Option<String> {
            if let Some(s) = &self.string_value {
                return Some(s.clone());
            }
            if let Some(i) = &self.int_value {
                return Some(match i {
                    EnumValue::Code(c) => c.to_string(),
                    EnumValue::Name(n) => n.clone(),
                });
            }
            if let Some(d) = self.double_value {
                return Some(d.to_string());
            }
            self.bool_value.map(|b| b.to_string())
        }
    }

    /// Look up one attribute by key, defaulting to empty — a batch with no
    /// `service.name` is unusual but not fatal.
    fn attribute(attrs: &[KeyValue], key: &str) -> String {
        attrs
            .iter()
            .find(|kv| kv.key == key)
            .and_then(|kv| kv.value.as_ref())
            .and_then(|v| v.to_text())
            .unwrap_or_default()
    }

    fn attribute_map(attrs: Vec<KeyValue>) -> BTreeMap<String, String> {
        attrs
            .into_iter()
            .filter(|kv| !kv.key.is_empty())
            .map(|kv| {
                let text = kv
                    .value
                    .as_ref()
                    .and_then(|v| v.to_text())
                    .unwrap_or_default();
                (kv.key, text)
            })
            .collect()
    }

    /// Accept a nanosecond timestamp as a decimal string (the OTLP JSON
    /// encoding) or as a number, and treat anything unparseable as 0 rather
    /// than failing the whole trace.
    fn de_u64<'de, D: Deserializer<'de>>(de: D) -> std::result::Result<u64, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Raw {
            Str(String),
            Num(f64),
        }
        Ok(match Option::<Raw>::deserialize(de)? {
            Some(Raw::Str(s)) => s.trim().parse().unwrap_or(0),
            Some(Raw::Num(n)) if n.is_finite() && n >= 0.0 => n as u64,
            _ => 0,
        })
    }

    /// Same leniency for a float field (`durationMs`).
    fn de_f64<'de, D: Deserializer<'de>>(de: D) -> std::result::Result<f64, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Raw {
            Str(String),
            Num(f64),
        }
        Ok(match Option::<Raw>::deserialize(de)? {
            Some(Raw::Str(s)) => s.trim().parse().unwrap_or(0.0),
            Some(Raw::Num(n)) if n.is_finite() => n,
            _ => 0.0,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn settings(url: &str) -> TempoSettings {
        TempoSettings {
            url: url.to_string(),
            service_name: "api".into(),
            enabled: None,
            auth_token: None,
        }
    }

    fn parse_trace(body: &str) -> Trace {
        let parsed: wire::TraceResponse = decode("test", body.as_bytes()).expect("decodes");
        Trace::from_spans("trace-1", parsed.into_spans())
    }

    fn parse_search(body: &str) -> Vec<TraceMetadata> {
        let parsed: wire::SearchResponse = decode("test", body.as_bytes()).expect("decodes");
        parsed.traces.into_iter().map(Into::into).collect()
    }

    #[test]
    fn a_trailing_slash_on_the_base_url_does_not_double_up_in_paths() {
        let client = TempoClient::new(&settings("http://tempo:3200/")).expect("valid");
        assert_eq!(client.base_url, "http://tempo:3200");
    }

    #[test]
    fn a_non_http_url_is_rejected_with_the_offending_value() {
        let err = TempoClient::new(&settings("ftp://tempo:3200")).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("ftp://tempo:3200"), "{message}");
    }

    #[test]
    fn a_syntactically_invalid_url_is_rejected() {
        assert!(TempoClient::new(&settings("not a url")).is_err());
    }

    #[test]
    fn search_results_decode_with_string_encoded_nanosecond_timestamps() {
        let metas = parse_search(
            r#"{"traces":[
                 {"traceID":"abc123","rootServiceName":"api","rootTraceName":"GET /health",
                  "startTimeUnixNano":"1750000000000000000","durationMs":42},
                 {"traceID":"def456","rootServiceName":"api","rootTraceName":"POST /order",
                  "startTimeUnixNano":"1750000001000000000","durationMs":7.5}
               ],"metrics":{"inspectedTraces":2}}"#,
        );
        assert_eq!(metas.len(), 2);
        assert_eq!(metas[0].trace_id, "abc123");
        assert_eq!(metas[0].start_time_unix_nano, 1_750_000_000_000_000_000);
        assert_eq!(metas[1].duration_ms, 7.5);
    }

    #[test]
    fn a_search_response_with_no_traces_key_decodes_as_empty() {
        assert!(parse_search(r#"{"metrics":{"inspectedTraces":0}}"#).is_empty());
        assert!(parse_search(r#"{"traces":[]}"#).is_empty());
    }

    #[test]
    fn search_results_tolerate_missing_optional_metadata() {
        let metas = parse_search(r#"{"traces":[{"traceID":"abc"}]}"#);
        assert_eq!(metas[0].root_service_name, "");
        assert_eq!(metas[0].start_time_unix_nano, 0);
        assert_eq!(metas[0].duration_ms, 0.0);
    }

    #[test]
    fn a_malformed_search_body_is_a_decode_error_naming_the_url() {
        let err = decode::<wire::SearchResponse>("http://tempo/api/search", b"<html>oops</html>")
            .unwrap_err();
        assert!(matches!(err, Error::Decode { .. }));
        assert!(err.to_string().contains("http://tempo/api/search"));
    }

    #[test]
    fn a_search_response_missing_the_trace_id_fails_to_decode() {
        // traceID is the one field with no sensible default: without it there
        // is nothing to fetch.
        assert!(
            decode::<wire::SearchResponse>("test", br#"{"traces":[{"durationMs":1}]}"#).is_err()
        );
    }

    const OTLP_TRACE: &str = r#"{
      "batches": [
        {
          "resource": {
            "attributes": [
              {"key": "service.name", "value": {"stringValue": "checkout"}},
              {"key": "host.name", "value": {"stringValue": "node-1"}}
            ]
          },
          "scopeSpans": [
            {
              "scope": {"name": "otel"},
              "spans": [
                {
                  "traceId": "abc", "spanId": "s1", "name": "GET /cart",
                  "kind": 2,
                  "startTimeUnixNano": "1750000000000000000",
                  "endTimeUnixNano":   "1750000000012000000",
                  "status": {"code": 1},
                  "attributes": [
                    {"key": "http.status_code", "value": {"intValue": "200"}},
                    {"key": "http.retry", "value": {"boolValue": false}}
                  ]
                },
                {
                  "traceId": "abc", "spanId": "s2", "parentSpanId": "s1",
                  "name": "db.query", "kind": "SPAN_KIND_CLIENT",
                  "startTimeUnixNano": "1750000000002000000",
                  "endTimeUnixNano":   "1750000000009000000",
                  "status": {"code": "STATUS_CODE_ERROR"}
                }
              ]
            }
          ]
        }
      ]
    }"#;

    #[test]
    fn an_otlp_trace_flattens_batches_and_scopes_into_spans() {
        let trace = parse_trace(OTLP_TRACE);
        assert_eq!(trace.spans.len(), 2);
        assert_eq!(trace.trace_id, "trace-1");
        // 1750000000.000 → 1750000000.012 is the extent of the two spans.
        assert_eq!(trace.total_duration_ns, 12_000_000);
    }

    #[test]
    fn the_resource_service_name_is_pushed_down_onto_every_span_in_the_batch() {
        let trace = parse_trace(OTLP_TRACE);
        assert!(trace.spans.iter().all(|s| s.service_name == "checkout"));
    }

    #[test]
    fn span_kind_and_status_decode_from_both_ordinals_and_enum_names() {
        let trace = parse_trace(OTLP_TRACE);
        assert_eq!(trace.spans[0].kind, SpanKind::Server);
        assert_eq!(trace.spans[0].status, SpanStatus::Ok);
        assert_eq!(trace.spans[1].kind, SpanKind::Client);
        assert_eq!(trace.spans[1].status, SpanStatus::Error);
    }

    #[test]
    fn span_durations_come_from_the_nanosecond_endpoints() {
        let trace = parse_trace(OTLP_TRACE);
        assert_eq!(trace.spans[0].duration_ns, 12_000_000);
        assert_eq!(trace.spans[0].duration_ms(), 12.0);
        assert_eq!(trace.spans[1].duration_ns, 7_000_000);
    }

    #[test]
    fn a_root_span_has_no_parent_and_a_child_does() {
        let trace = parse_trace(OTLP_TRACE);
        assert_eq!(trace.spans[0].parent_span_id, None);
        assert_eq!(trace.spans[1].parent_span_id.as_deref(), Some("s1"));
    }

    #[test]
    fn an_empty_parent_span_id_is_treated_as_a_root_span() {
        let trace = parse_trace(
            r#"{"batches":[{"scopeSpans":[{"spans":[
                 {"spanId":"s1","parentSpanId":"","name":"n",
                  "startTimeUnixNano":"0","endTimeUnixNano":"1"}]}]}]}"#,
        );
        assert_eq!(trace.spans[0].parent_span_id, None);
    }

    #[test]
    fn scalar_attributes_of_every_otlp_type_stringify() {
        let trace = parse_trace(OTLP_TRACE);
        let attrs = &trace.spans[0].attributes;
        assert_eq!(
            attrs.get("http.status_code").map(String::as_str),
            Some("200")
        );
        assert_eq!(attrs.get("http.retry").map(String::as_str), Some("false"));
    }

    #[test]
    fn numeric_timestamps_are_accepted_alongside_the_string_encoding() {
        let trace = parse_trace(
            r#"{"batches":[{"scopeSpans":[{"spans":[
                 {"spanId":"s1","name":"n","startTimeUnixNano":1000,"endTimeUnixNano":3000}]}]}]}"#,
        );
        assert_eq!(trace.spans[0].duration_ns, 2000);
    }

    #[test]
    fn an_unparseable_timestamp_degrades_to_zero_rather_than_failing_the_trace() {
        let trace = parse_trace(
            r#"{"batches":[{"scopeSpans":[{"spans":[
                 {"spanId":"s1","name":"n","startTimeUnixNano":"not-a-number",
                  "endTimeUnixNano":"5000"}]}]}]}"#,
        );
        assert_eq!(trace.spans[0].start_time_ns, 0);
        assert_eq!(trace.spans[0].duration_ns, 5000);
    }

    #[test]
    fn clock_skew_that_ends_a_span_before_it_starts_yields_zero_not_underflow() {
        let trace = parse_trace(
            r#"{"batches":[{"scopeSpans":[{"spans":[
                 {"spanId":"s1","name":"n","startTimeUnixNano":"9000","endTimeUnixNano":"1000"}]}]}]}"#,
        );
        assert_eq!(trace.spans[0].duration_ns, 0);
    }

    #[test]
    fn the_legacy_instrumentation_library_spans_key_is_still_accepted() {
        let trace = parse_trace(
            r#"{"batches":[{"instrumentationLibrarySpans":[{"spans":[
                 {"spanId":"s1","name":"legacy","startTimeUnixNano":"0","endTimeUnixNano":"1"}]}]}]}"#,
        );
        assert_eq!(trace.spans.len(), 1);
        assert_eq!(trace.spans[0].name, "legacy");
    }

    #[test]
    fn a_trace_with_empty_batches_decodes_to_a_trace_with_no_spans() {
        assert!(parse_trace(r#"{"batches":[]}"#).spans.is_empty());
        assert!(parse_trace(r#"{}"#).spans.is_empty());
        assert!(
            parse_trace(r#"{"batches":[{"resource":{"attributes":[]}}]}"#)
                .spans
                .is_empty()
        );
    }

    /// A loopback mock of the two Tempo endpoints. This is the only test that
    /// exercises the HTTP layer, and it exists to pin the things a fixture
    /// cannot: the query string we actually send and the bearer header.
    mod mock_tempo {
        use std::sync::{Arc, Mutex};

        use axum::extract::{Path, Query, State};
        use axum::http::HeaderMap;
        use axum::routing::get;
        use axum::Router;

        /// What the mock saw, so the test can assert on the request we sent.
        #[derive(Default)]
        pub struct Seen {
            pub search_query: Vec<(String, String)>,
            pub authorization: Option<String>,
            pub fetched_ids: Vec<String>,
        }

        pub async fn spawn() -> (String, Arc<Mutex<Seen>>) {
            let seen = Arc::new(Mutex::new(Seen::default()));
            let app = Router::new()
                .route("/api/search", get(search))
                .route("/api/traces/:id", get(trace))
                .with_state(seen.clone());

            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
            let addr = listener.local_addr().unwrap();
            tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
            (format!("http://{addr}"), seen)
        }

        async fn search(
            State(seen): State<Arc<Mutex<Seen>>>,
            Query(params): Query<Vec<(String, String)>>,
            headers: HeaderMap,
        ) -> &'static str {
            let mut seen = seen.lock().unwrap();
            seen.search_query = params;
            seen.authorization = headers
                .get("authorization")
                .and_then(|v| v.to_str().ok())
                .map(str::to_string);
            r#"{"traces":[{"traceID":"t-1"},{"traceID":"t-2"},{"traceID":"missing"}]}"#
        }

        async fn trace(
            State(seen): State<Arc<Mutex<Seen>>>,
            Path(id): Path<String>,
        ) -> (axum::http::StatusCode, String) {
            seen.lock().unwrap().fetched_ids.push(id.clone());
            if id == "missing" {
                // An expired trace: the run must survive it.
                return (axum::http::StatusCode::NOT_FOUND, String::new());
            }
            (
                axum::http::StatusCode::OK,
                format!(
                    r#"{{"batches":[{{"resource":{{"attributes":[
                         {{"key":"service.name","value":{{"stringValue":"api"}}}}]}},
                       "scopeSpans":[{{"spans":[
                         {{"spanId":"{id}","name":"GET /x","kind":2,
                          "startTimeUnixNano":"0","endTimeUnixNano":"5000000",
                          "status":{{"code":1}}}}]}}]}}]}}"#
                ),
            )
        }
    }

    #[tokio::test]
    async fn a_window_search_sends_the_traceql_query_and_bearer_token_then_fetches_each_trace() {
        let (url, seen) = mock_tempo::spawn().await;
        let mut settings = settings(&url);
        settings.service_name = "api".into();
        settings.auth_token = Some("s3cret".into());

        let client = TempoClient::new(&settings).expect("valid");
        let window = crate::types::TraceWindow::from_unix_nanos(
            1_750_000_000_000_000_000,
            1_750_000_002_500_000_000,
        );
        let traces = client
            .fetch_traces_for_window(&TraceQuery::for_service("api", window))
            .await
            .expect("search succeeds");

        let seen = seen.lock().unwrap();
        let param = |k: &str| {
            seen.search_query
                .iter()
                .find(|(name, _)| name == k)
                .map(|(_, v)| v.clone())
        };
        assert_eq!(
            param("q").as_deref(),
            Some(r#"{resource.service.name="api"}"#)
        );
        assert_eq!(param("start").as_deref(), Some("1750000000"));
        assert_eq!(param("end").as_deref(), Some("1750000003"));
        assert_eq!(param("limit").as_deref(), Some("10000"));
        assert_eq!(seen.authorization.as_deref(), Some("Bearer s3cret"));

        // The 404 trace is dropped; the other two survive, in search order.
        assert_eq!(seen.fetched_ids.len(), 3);
        assert_eq!(traces.len(), 2);
        assert_eq!(traces[0].trace_id, "t-1");
        assert_eq!(traces[1].trace_id, "t-2");
        assert_eq!(traces[0].spans[0].service_name, "api");
    }

    #[tokio::test]
    async fn no_authorization_header_is_sent_when_no_token_is_configured() {
        let (url, seen) = mock_tempo::spawn().await;
        let client = TempoClient::new(&settings(&url)).expect("valid");
        let window = crate::types::TraceWindow::from_unix_nanos(0, 1_000_000_000);
        client
            .search(&TraceQuery::for_service("api", window))
            .await
            .expect("search succeeds");
        assert!(seen.lock().unwrap().authorization.is_none());
    }

    #[test]
    fn a_batch_without_a_resource_yields_spans_with_an_empty_service_name() {
        let trace = parse_trace(
            r#"{"batches":[{"scopeSpans":[{"spans":[
                 {"spanId":"s1","name":"n","startTimeUnixNano":"0","endTimeUnixNano":"1"}]}]}]}"#,
        );
        assert_eq!(trace.spans[0].service_name, "");
    }
}
