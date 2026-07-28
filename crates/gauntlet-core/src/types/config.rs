//! Parsed benchmark configuration. See the crate docs for how these types split
//! the work with `validate`.

use std::collections::BTreeMap;
use std::num::NonZeroU32;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::response::ValidationSpec;

/// Top-level benchmark configuration: one or more named targets, shared
/// settings, and the payloads run against every target.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct BenchmarkConfig {
    pub targets: Vec<NamedTarget>,
    pub settings: Settings,
    pub payloads: Vec<PayloadSpec>,
}

/// A single named target in a benchmark run.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct NamedTarget {
    pub name: String,
    pub url: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<LifecycleHooks>,
}

/// Per-target setup, teardown, and health-check hooks.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct LifecycleHooks {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub setup: Option<HookCommand>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub teardown: Option<HookCommand>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub health_check: Option<HealthCheckConfig>,
}

/// A shell command to execute as a lifecycle hook.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct HookCommand {
    pub cmd: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timeout_secs: Option<NonZeroU32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub working_dir: Option<String>,
}

/// Health check configuration: poll a URL until it responds 200.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct HealthCheckConfig {
    pub url: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timeout_secs: Option<NonZeroU32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub interval_ms: Option<NonZeroU32>,
}

/// Runtime settings controlling iterations, concurrency, timeouts, and optional
/// features. Real defaults stand in for absent optional sections.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct Settings {
    pub iterations: NonZeroU32,
    pub concurrency: NonZeroU32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub secrets: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_connections: Option<NonZeroU32>,
    #[serde(default = "default_request_timeout_secs")]
    pub request_timeout_secs: NonZeroU32,
    #[serde(default)]
    pub retry: RetrySettings,
    #[serde(default)]
    pub warmup: WarmupSettings,
    #[serde(default)]
    pub log_level: LogLevel,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tempo: Option<TempoSettings>,
    #[serde(default)]
    pub load_mode: LoadMode,
}

fn default_request_timeout_secs() -> NonZeroU32 {
    NonZeroU32::new(30).expect("30 is nonzero")
}

/// Retry configuration for failed requests.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct RetrySettings {
    /// Maximum retry attempts; 0 disables retries.
    #[serde(default = "default_max_attempts")]
    pub max_attempts: u32,
    #[serde(default = "default_initial_delay_ms")]
    pub initial_delay_ms: NonZeroU32,
    /// Delay multiplier for exponential backoff; must be ≥ 1.0 (checked in `validate`).
    #[serde(default = "default_backoff_multiplier")]
    pub backoff_multiplier: f64,
}

fn default_max_attempts() -> u32 {
    3
}
fn default_initial_delay_ms() -> NonZeroU32 {
    NonZeroU32::new(1000).expect("1000 is nonzero")
}
fn default_backoff_multiplier() -> f64 {
    2.0
}

impl Default for RetrySettings {
    fn default() -> Self {
        RetrySettings {
            max_attempts: default_max_attempts(),
            initial_delay_ms: default_initial_delay_ms(),
            backoff_multiplier: default_backoff_multiplier(),
        }
    }
}

/// Warmup configuration before benchmark runs.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct WarmupSettings {
    /// Warmup requests per endpoint; 0 disables warmup.
    #[serde(default = "default_warmup_iterations")]
    pub iterations: u32,
}

fn default_warmup_iterations() -> u32 {
    1
}

impl Default for WarmupSettings {
    fn default() -> Self {
        WarmupSettings {
            iterations: default_warmup_iterations(),
        }
    }
}

/// Optional Tempo (distributed-tracing) integration settings.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct TempoSettings {
    pub url: String,
    pub service_name: String,
    /// Defaults to true when the `tempo` section is present.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub auth_token: Option<String>,
}

/// Log levels for controlling output verbosity. Defaults to `Info`.
#[derive(
    Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum LogLevel {
    Debug,
    #[default]
    Info,
    Warning,
    Error,
}

/// An HTTP request method. The enum makes an invalid method unrepresentable —
/// a bad value fails to deserialize rather than passing through to `validate`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "UPPERCASE")]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
}

impl HttpMethod {
    /// The method as its canonical uppercase string.
    pub fn as_str(self) -> &'static str {
        match self {
            HttpMethod::Get => "GET",
            HttpMethod::Post => "POST",
            HttpMethod::Put => "PUT",
            HttpMethod::Delete => "DELETE",
            HttpMethod::Patch => "PATCH",
        }
    }
}

/// A specification for a single HTTP payload within a benchmark.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct PayloadSpec {
    pub name: String,
    pub method: HttpMethod,
    pub path: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub headers: BTreeMap<String, String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub validate: Option<ValidationSpec>,
}

/// A single step in a step-load profile.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct LoadStep {
    pub rpm: f64,
    pub duration_secs: f64,
}

/// Load control mode for pacing request dispatch, tagged on `mode`. Positivity
/// of the float fields is checked in `validate`.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "mode", rename_all = "snake_case")]
pub enum LoadMode {
    /// No throttling — fire as fast as concurrency allows.
    #[default]
    Unthrottled,
    /// Constant RPM — pace iterations at a fixed rate.
    ConstantRpm { target_rpm: f64 },
    /// Ramp-up — linearly increase RPM from start to end over a duration.
    RampUp {
        start_rpm: f64,
        end_rpm: f64,
        duration_secs: f64,
    },
    /// Step load — sequential steps, each with its own RPM and duration.
    StepLoad { steps: Vec<LoadStep> },
    /// Poisson-distributed load at a target mean RPM.
    PoissonRpm { target_rpm: f64 },
}

impl LoadMode {
    /// Total requests for a load mode given a fallback iteration count.
    pub fn total_requests(&self, fallback: u32) -> u32 {
        match self {
            LoadMode::Unthrottled | LoadMode::ConstantRpm { .. } | LoadMode::PoissonRpm { .. } => {
                fallback
            }
            LoadMode::RampUp {
                start_rpm,
                end_rpm,
                duration_secs,
            } => ((start_rpm + end_rpm) / 2.0 / 60.0 * duration_secs).round() as u32,
            LoadMode::StepLoad { steps } => steps
                .iter()
                .map(|s| (s.rpm / 60.0 * s.duration_secs).round() as u32)
                .sum(),
        }
    }

    /// Whether the load mode is duration-based (ignores `iterations`).
    pub fn is_duration_based(&self) -> bool {
        matches!(self, LoadMode::RampUp { .. } | LoadMode::StepLoad { .. })
    }

    /// Total duration in seconds for duration-based modes (0 otherwise).
    pub fn duration_secs(&self) -> f64 {
        match self {
            LoadMode::RampUp { duration_secs, .. } => *duration_secs,
            LoadMode::StepLoad { steps } => steps.iter().map(|s| s.duration_secs).sum(),
            _ => 0.0,
        }
    }
}
