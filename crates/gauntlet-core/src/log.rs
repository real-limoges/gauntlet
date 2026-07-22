//! Leveled diagnostic output.
//!
//! `settings.log_level` existed in the config and in the schema but nothing
//! honoured it after the Rust port — the Haskell had a real logger, and the
//! port replaced it with unconditional `eprintln!`. This restores the field's
//! meaning without pulling in a logging framework.
//!
//! The distinction this module draws: **diagnostics go through here and land on
//! stderr; program output does not.** A benchmark summary, a printed schema, and
//! a validation report are the tool's *output* — they go to stdout and are never
//! filtered, because suppressing them would defeat the command the user ran.
//! Warnings about a failed baseline write are diagnostics.
//!
//! Deliberately not the `tracing` crate: that name already means Grafana Tempo
//! in this workspace (see `docs/adr/M0-foundation.md`), and this is a handful of
//! lines.

use std::io::Write;
use std::sync::atomic::{AtomicU8, Ordering};

use crate::types::config::LogLevel;

/// The active threshold. Messages below it are dropped.
static LEVEL: AtomicU8 = AtomicU8::new(rank(LogLevel::Info));

/// Ordering rank; higher is more severe.
const fn rank(level: LogLevel) -> u8 {
    match level {
        LogLevel::Debug => 0,
        LogLevel::Info => 1,
        LogLevel::Warning => 2,
        LogLevel::Error => 3,
    }
}

/// Set the threshold, normally once from `settings.log_level` at startup.
pub fn set_level(level: LogLevel) {
    LEVEL.store(rank(level), Ordering::Relaxed);
}

/// The active threshold.
pub fn level() -> LogLevel {
    match LEVEL.load(Ordering::Relaxed) {
        0 => LogLevel::Debug,
        1 => LogLevel::Info,
        2 => LogLevel::Warning,
        _ => LogLevel::Error,
    }
}

/// Whether a message at `level` would be emitted.
pub fn enabled(level: LogLevel) -> bool {
    rank(level) >= LEVEL.load(Ordering::Relaxed)
}

fn log(level: LogLevel, prefix: &str, message: &str) {
    if !enabled(level) {
        return;
    }
    // A failed write to stderr is not worth failing a benchmark over, and
    // there is nowhere left to report it to.
    let mut stderr = std::io::stderr().lock();
    let _ = writeln!(stderr, "{prefix}: {message}");
}

pub fn error(message: impl AsRef<str>) {
    log(LogLevel::Error, "error", message.as_ref());
}

pub fn warn(message: impl AsRef<str>) {
    log(LogLevel::Warning, "warning", message.as_ref());
}

pub fn info(message: impl AsRef<str>) {
    log(LogLevel::Info, "info", message.as_ref());
}

pub fn debug(message: impl AsRef<str>) {
    log(LogLevel::Debug, "debug", message.as_ref());
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The threshold is process-global, so these assertions run in one test to
    /// keep them from racing each other under the test harness.
    #[test]
    fn the_threshold_filters_less_severe_messages() {
        set_level(LogLevel::Info);
        assert!(!enabled(LogLevel::Debug), "debug is below info");
        assert!(enabled(LogLevel::Info));
        assert!(enabled(LogLevel::Warning));
        assert!(enabled(LogLevel::Error));

        set_level(LogLevel::Error);
        assert!(!enabled(LogLevel::Warning), "warnings are below error");
        assert!(enabled(LogLevel::Error), "errors are never suppressed");

        set_level(LogLevel::Debug);
        assert!(enabled(LogLevel::Debug), "debug shows everything");

        set_level(LogLevel::Info);
        assert_eq!(level(), LogLevel::Info, "the level round-trips");
    }
}
