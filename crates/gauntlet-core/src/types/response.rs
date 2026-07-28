//! Response-validation config and the runtime response record. [`FieldAssertion`]
//! is externally tagged: unit checks are bare strings, data checks single-key
//! objects.

use std::collections::BTreeMap;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::config::HttpMethod;
use super::units::{ns_to_ms, Nanoseconds};

/// A per-field assertion in a validation spec.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum FieldAssertion {
    /// Key exists (any value, including null).
    Present,
    /// Field is explicitly `null`.
    Null,
    /// Field exists and is not `null`.
    NotNull,
    /// Exact value match.
    Eq(serde_json::Value),
    /// JSON type: `"string"`, `"number"`, `"boolean"`, `"array"`, `"object"`, `"null"`.
    Type(String),
    /// String field matches a POSIX ERE.
    Matches(String),
    /// Numeric field within `[min, max]` (both bounds optional).
    Range {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        min: Option<f64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        max: Option<f64>,
    },
    /// Array has exactly N elements.
    ArrayLength(usize),
    /// Value is present in an array.
    ArrayContains(serde_json::Value),
}

/// Declarative validation rules applied to every response for an endpoint.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct ValidationSpec {
    /// Expected HTTP status code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub status: Option<u16>,
    /// Map from dot-path (e.g. `$.user.id`) to assertion.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fields: Option<BTreeMap<String, FieldAssertion>>,
}

/// An HTTP endpoint to benchmark: method, URL, optional body, headers, and
/// optional validation. Produced by `build_endpoints`; a runtime type.
#[derive(Clone, Debug, PartialEq)]
pub struct Endpoint {
    pub method: HttpMethod,
    pub url: String,
    pub body: Option<serde_json::Value>,
    pub headers: Vec<(String, String)>,
    pub validate: Option<ValidationSpec>,
}

/// The result of a single benchmarked HTTP request, constructed by the engine.
/// A `Some(error)` marks a transport failure, excluded from latency math.
#[derive(Clone, Debug, PartialEq)]
pub struct TestingResponse {
    pub duration: Nanoseconds,
    pub status: u16,
    pub error: Option<String>,
}

impl TestingResponse {
    /// This response's latency in milliseconds, or `None` if the request failed.
    pub fn latency_ms(&self) -> Option<f64> {
        match self.error {
            Some(_) => None,
            None => Some(ns_to_ms(self.duration).0),
        }
    }
}

/// Extract the successful-response latencies (ms) from a batch, dropping
/// failures. The feed into `gauntlet_stats::calculate_stats`.
pub fn extract_durations(responses: &[TestingResponse]) -> Vec<f64> {
    responses
        .iter()
        .filter_map(TestingResponse::latency_ms)
        .collect()
}

/// A single failed assertion against a response: the dot-path that failed and a
/// human-readable reason. Produced by the engine's validation pass.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct ValidationError {
    /// The field path that failed (e.g. `$.user.id`), or `status` for the
    /// status-code check.
    pub field: String,
    /// Why it failed.
    pub message: String,
}

/// Aggregate validation outcome for one endpoint. Errors are capped at
/// [`MAX_VALIDATION_ERRORS`].
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct ValidationSummary {
    pub total: usize,
    pub failed: usize,
    pub errors: Vec<ValidationError>,
}

/// Cap on retained validation errors per endpoint, so a fully-failing run cannot
/// accumulate unbounded detail.
pub const MAX_VALIDATION_ERRORS: usize = 50;
