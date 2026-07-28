//! Per-target lifecycle hooks: a setup command, a polled health-check URL, and a
//! teardown command. Commands are interpreted by `sh -c`.

use std::time::{Duration, Instant};

use gauntlet_core::{HealthCheckConfig, HookCommand};
use tokio::process::Command;

use crate::error::{EngineError, Result};

const DEFAULT_HEALTH_TIMEOUT_SECS: u64 = 30;
const DEFAULT_HEALTH_INTERVAL_MS: u64 = 500;
const DEFAULT_HOOK_TIMEOUT_SECS: u64 = 30;

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
