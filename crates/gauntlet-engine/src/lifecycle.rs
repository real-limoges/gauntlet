//! Per-target lifecycle: switch git branches, spawn a setup command, poll a
//! health-check URL until it's ready, and run a teardown command afterwards.
//! Process spawning is via `tokio::process`; the shell (`sh -c`) interprets the
//! command string.

use std::time::{Duration, Instant};

use gauntlet_core::{HealthCheckConfig, HookCommand};
use tokio::process::Command;

use crate::error::{EngineError, Result};

// Defaults match the Haskell implementation. They are generous on purpose:
// a setup hook is usually `docker compose up` or a build, and a health check
// waits on a service that has just been started.
const DEFAULT_HEALTH_TIMEOUT_SECS: u64 = 60;
const DEFAULT_HEALTH_INTERVAL_MS: u64 = 1000;
const DEFAULT_HOOK_TIMEOUT_SECS: u64 = 300;

/// Check out `branch` before benchmarking a target, via `git switch`.
///
/// Targets that name a branch are comparing two revisions of the same service;
/// without this the run benchmarks whatever happens to be checked out, once per
/// target, and reports a confident comparison of a build against itself. An
/// empty branch string is a no-op, matching the Haskell.
pub async fn switch_branch(target: &str, branch: &str) -> Result<()> {
    if branch.trim().is_empty() {
        return Ok(());
    }

    let output = Command::new("git")
        .arg("switch")
        .arg(branch)
        .output()
        .await
        .map_err(|e| lifecycle_err(target, format!("could not run git: {e}")))?;

    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    Err(lifecycle_err(
        target,
        format!("git switch {branch} failed: {}", stderr.trim()),
    ))
}

/// Run a setup/teardown command to completion, honoring `working_dir` and an
/// optional timeout. A non-zero exit or a timeout is an error.
pub async fn run_hook(target: &str, kind: &str, hook: &HookCommand) -> Result<()> {
    let mut cmd = Command::new("sh");
    cmd.arg("-c").arg(&hook.cmd);
    if let Some(dir) = &hook.working_dir {
        cmd.current_dir(dir);
    }
    let timeout = Duration::from_secs(
        hook.timeout_secs
            .map(|t| t.get() as u64)
            .unwrap_or(DEFAULT_HOOK_TIMEOUT_SECS),
    );

    let status = tokio::time::timeout(timeout, cmd.status())
        .await
        .map_err(|_| lifecycle_err(target, format!("{kind} hook timed out after {timeout:?}")))?
        .map_err(|e| lifecycle_err(target, format!("{kind} hook failed to spawn: {e}")))?;

    if status.success() {
        Ok(())
    } else {
        Err(lifecycle_err(
            target,
            format!("{kind} hook exited with {status}"),
        ))
    }
}

/// Poll `health_check.url` until it returns 200, or fail after the timeout.
pub async fn wait_healthy(target: &str, hc: &HealthCheckConfig) -> Result<()> {
    let client = reqwest::Client::new();
    let timeout = Duration::from_secs(
        hc.timeout_secs
            .map(|t| t.get() as u64)
            .unwrap_or(DEFAULT_HEALTH_TIMEOUT_SECS),
    );
    let interval = Duration::from_millis(
        hc.interval_ms
            .map(|i| i.get() as u64)
            .unwrap_or(DEFAULT_HEALTH_INTERVAL_MS),
    );
    let deadline = Instant::now() + timeout;

    loop {
        if let Ok(resp) = client.get(&hc.url).send().await {
            if resp.status().is_success() {
                return Ok(());
            }
        }
        if Instant::now() >= deadline {
            return Err(lifecycle_err(
                target,
                format!("health check {} not ready after {timeout:?}", hc.url),
            ));
        }
        tokio::time::sleep(interval).await;
    }
}

fn lifecycle_err(target: &str, message: String) -> EngineError {
    EngineError::Lifecycle {
        target: target.to_owned(),
        message,
    }
}
