//! The config JSON schema, derived from the types via `schemars` so it cannot
//! drift. See the crate docs.

use schemars::schema::RootSchema;
use schemars::schema_for;

use crate::types::config::BenchmarkConfig;

/// The derived JSON Schema for [`BenchmarkConfig`].
pub fn config_schema() -> RootSchema {
    schema_for!(BenchmarkConfig)
}

/// The derived config schema as pretty-printed JSON.
pub fn config_schema_string() -> String {
    serde_json::to_string_pretty(&config_schema()).expect("schema serializes")
}
