//! `gauntlet-core` — the shared vocabulary for the rest of the workspace: config
//! types, time-unit newtypes, response and validation types, and the error type,
//! plus config loading and the derived JSON schema.
//!
//! # Loading a config
//!
//! [`load_benchmark_config`] is `read → ${VAR} interpolate → parse`. It does not
//! validate; [`BenchmarkConfig::validate`] is a separate call, so a caller that
//! only needs to parse (the `schema` subcommand, a test fixture) does not pay for
//! it.
//!
//! Validation **accumulates** every problem and reports them together rather than
//! failing on the first. It also has less to check than it looks: the config
//! types make invalid states unrepresentable wherever that is cheap —
//! `NonZeroU32` for counts and delays, an [`HttpMethod`] enum for methods, real
//! defaults instead of `Option`, `deny_unknown_fields` to reject typos. What is
//! left for `validate` is the float-range, non-empty-collection, and
//! cross-field rules the type system cannot express.
//!
//! The JSON contract is snake_case and Rust-native: field names map 1:1 with no
//! serde renaming.
//!
//! # Environment interpolation
//!
//! [`interpolate_env`] is hand-rolled rather than `dotenvy` or `subst`, because
//! the semantics are small and specific:
//!
//! - Precedence, highest first: `.env.local` > `.env` > process environment.
//! - `${VAR}` only. There is no `${VAR:-default}` form.
//! - An undefined variable is an **error**, not an empty string. A benchmark that
//!   silently pointed at `http://` because a variable was unset would produce a
//!   confidently wrong result.
//! - An unclosed `${` is left as a literal.
//!
//! # The schema is derived, not written
//!
//! [`config_schema`] comes from the config types via `schemars`, so it cannot
//! drift from the structs it describes. `schema/config-schema.json` is a
//! committed copy of that output, and a test fails if it goes stale.
//!
//! # Diagnostics versus output
//!
//! [`log`] draws one distinction: **diagnostics go through it and land on stderr;
//! program output does not.** A benchmark summary, a printed schema, and a
//! validation report are what the user ran the command for — they go to stdout
//! unfiltered. A warning that a baseline could not be written is a diagnostic and
//! is subject to `settings.log_level`.
//!
//! It is deliberately not the `tracing` crate: that name already means Grafana
//! Tempo in this workspace (`gauntlet-tracing` is the Tempo client), and this is
//! a handful of lines.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

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
