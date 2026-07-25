//! Response-validation config and the runtime response record.
//!
//! `FieldAssertion` is a plain externally-tagged serde enum: unit checks
//! serialize as bare strings (`"present"`, `"not_null"`), data checks as
//! single-key objects (`{"eq": <value>}`). The validation *results*
//! (`ValidationError`/`ValidationSummary`) live here as shared vocabulary (so
//! reporters can read them without depending on the engine); the engine owns the
//! checking *logic* that produces them.

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
    /// String field matches a regular expression, in Rust `regex` crate syntax.
    /// (The Haskell used POSIX ERE; the two agree on ordinary patterns, but
    /// neither supports backreferences or lookaround.)
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

/// The result of a single benchmarked HTTP request. Constructed by the engine
/// (M3); a runtime type. A `Some(error)` marks a failed request — excluded from
/// latency math.
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

/// Aggregate validation outcome for one endpoint: how many responses were
/// checked, how many failed, and the collected errors (capped — see
/// `MAX_VALIDATION_ERRORS`).
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize, JsonSchema)]
pub struct ValidationSummary {
    pub total: usize,
    pub failed: usize,
    pub errors: Vec<ValidationError>,
}

/// Cap on retained validation errors per endpoint, so a fully-failing run can't
/// accumulate unbounded detail. Matches the Haskell 50-error cap.
pub const MAX_VALIDATION_ERRORS: usize = 50;
