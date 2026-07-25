//! Config loading, validation, and endpoint expansion.
//!
//! Loading is `read → ${VAR} interpolate → parse`. Validation is a method on
//! `BenchmarkConfig` that *accumulates* every problem (rather than failing on the
//! first) and reports them together — the type system already rejects bad
//! methods, zero counts, and zero delays at parse time, so this checks only the
//! float-range, non-empty-collection, and non-empty-string rules left over.

use std::path::Path;

use super::env::{interpolate_env, load_env_vars};
use crate::error::{ConfigErrors, Error, Result};
use crate::types::config::{BenchmarkConfig, LoadMode, NamedTarget, PayloadSpec};
use crate::types::response::Endpoint;

/// Load a benchmark config from a JSON file: read → `${VAR}` interpolate → parse.
/// Does not validate — call [`BenchmarkConfig::validate`] separately.
pub fn load_benchmark_config<P: AsRef<Path>>(path: P) -> Result<BenchmarkConfig> {
    let path = path.as_ref();
    let content = std::fs::read_to_string(path).map_err(|source| Error::ReadConfig {
        path: path.to_path_buf(),
        source,
    })?;
    let env = load_env_vars();
    let interpolated = interpolate_env(&env, &content).map_err(Error::UndefinedEnvVar)?;
    Ok(serde_json::from_str(&interpolated)?)
}

impl BenchmarkConfig {
    /// Validate the config, accumulating *all* problems. The type system already
    /// guarantees positive counts/delays and valid methods at parse time.
    pub fn validate(&self) -> std::result::Result<(), ConfigErrors> {
        let mut errs = Vec::new();

        if self.targets.is_empty() {
            errs.push("must define at least one target".to_owned());
        }
        if self.payloads.is_empty() {
            errs.push("must define at least one payload".to_owned());
        }
        // Names are identities downstream: targets name their baselines, payloads
        // are matched by name when restoring result order. A duplicate silently
        // makes one of the pair unreachable.
        duplicates(
            "targets",
            self.targets.iter().map(|t| t.name.as_str()),
            &mut errs,
        );
        duplicates(
            "payloads",
            self.payloads.iter().map(|p| p.name.as_str()),
            &mut errs,
        );
        if self.settings.retry.backoff_multiplier < 1.0 {
            errs.push("settings.retry.backoff_multiplier must be at least 1.0".to_owned());
        }
        validate_load_mode(&self.settings.load_mode, &mut errs);
        for (i, target) in self.targets.iter().enumerate() {
            validate_target_lifecycle(i, target, &mut errs);
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(ConfigErrors(errs))
        }
    }
}

/// Report any name appearing more than once in `names`, in first-seen order.
fn duplicates<'a>(field: &str, names: impl Iterator<Item = &'a str>, errs: &mut Vec<String>) {
    let mut seen = std::collections::BTreeSet::new();
    let mut reported = std::collections::BTreeSet::new();
    for name in names {
        if !seen.insert(name) && reported.insert(name) {
            errs.push(format!("{field}: duplicate name {name:?}"));
        }
    }
}

fn validate_load_mode(mode: &LoadMode, errs: &mut Vec<String>) {
    let positive = |label: &str, v: f64, errs: &mut Vec<String>| {
        if v <= 0.0 {
            errs.push(format!("settings.load_mode.{label} must be greater than 0"));
        }
    };
    match mode {
        LoadMode::Unthrottled => {}
        LoadMode::ConstantRpm { target_rpm } | LoadMode::PoissonRpm { target_rpm } => {
            positive("target_rpm", *target_rpm, errs);
        }
        LoadMode::RampUp {
            start_rpm,
            end_rpm,
            duration_secs,
        } => {
            positive("start_rpm", *start_rpm, errs);
            positive("end_rpm", *end_rpm, errs);
            positive("duration_secs", *duration_secs, errs);
        }
        LoadMode::StepLoad { steps } => {
            if steps.is_empty() {
                errs.push("settings.load_mode.steps must not be empty".to_owned());
            }
            for (i, step) in steps.iter().enumerate() {
                positive(&format!("steps[{i}].rpm"), step.rpm, errs);
                positive(
                    &format!("steps[{i}].duration_secs"),
                    step.duration_secs,
                    errs,
                );
            }
        }
    }
}

fn validate_target_lifecycle(idx: usize, target: &NamedTarget, errs: &mut Vec<String>) {
    let Some(hooks) = &target.lifecycle else {
        return;
    };
    let path = |suffix: &str| format!("targets[{idx}].lifecycle.{suffix}");
    if let Some(setup) = &hooks.setup {
        if setup.cmd.trim().is_empty() {
            errs.push(format!("{} must not be empty", path("setup.cmd")));
        }
    }
    if let Some(teardown) = &hooks.teardown {
        if teardown.cmd.trim().is_empty() {
            errs.push(format!("{} must not be empty", path("teardown.cmd")));
        }
    }
    if let Some(hc) = &hooks.health_check {
        if hc.url.trim().is_empty() {
            errs.push(format!("{} must not be empty", path("health_check.url")));
        }
    }
}

/// Expand a base URL and payload specs into concrete endpoints: `url = base_url +
/// path`, and a default `Content-Type: application/json` is prepended unless a
/// custom Content-Type header is present.
pub fn build_endpoints(base_url: &str, payloads: &[PayloadSpec]) -> Vec<Endpoint> {
    payloads
        .iter()
        .map(|spec| {
            // `BTreeMap` iterates ascending by key — deterministic header order.
            let custom: Vec<(String, String)> = spec
                .headers
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            let has_content_type = custom.iter().any(|(k, _)| k == "Content-Type");
            let mut headers = Vec::with_capacity(custom.len() + 1);
            if !has_content_type {
                headers.push(("Content-Type".to_owned(), "application/json".to_owned()));
            }
            headers.extend(custom);
            Endpoint {
                method: spec.method,
                url: format!("{base_url}{}", spec.path),
                body: spec.body.clone(),
                headers,
                validate: spec.validate.clone(),
            }
        })
        .collect()
}
