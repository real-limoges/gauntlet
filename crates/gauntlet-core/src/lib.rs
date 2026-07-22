//! `gauntlet-core` — the shared vocabulary for the rest of the workspace:
//! config types, time-unit newtypes, response/validation types, and the error
//! type, plus config loading (`.env` interpolation, accumulating validation,
//! endpoint expansion) and the schemars-derived JSON schema.

pub mod config;
pub mod error;
pub mod log;
pub mod schema;
pub mod types;

pub use error::{ConfigErrors, Error, Result};
pub use schema::{config_schema, config_schema_string};

pub use config::env::{interpolate_env, load_env_vars, parse_env_file};
pub use config::loader::{build_endpoints, load_benchmark_config};

pub use types::config::{
    BenchmarkConfig, HealthCheckConfig, HookCommand, HttpMethod, LifecycleHooks, LoadMode,
    LoadStep, LogLevel, NamedTarget, PayloadSpec, RetrySettings, Settings, TempoSettings,
    WarmupSettings,
};
pub use types::response::{
    extract_durations, Endpoint, FieldAssertion, TestingResponse, ValidationError, ValidationSpec,
    ValidationSummary, MAX_VALIDATION_ERRORS,
};
pub use types::units::{ns_to_ms, Milliseconds, Nanoseconds};
