//! The crate error type. Only the variants M2 actually constructs live here;
//! the engine, reporters, and tracing extend the error surface as they land.

use std::path::PathBuf;

use thiserror::Error;

/// A convenient `Result` alias for `gauntlet-core` fallible operations.
pub type Result<T> = std::result::Result<T, Error>;

/// Errors raised while loading and parsing configuration.
#[derive(Debug, Error)]
pub enum Error {
    /// The config file could not be read.
    #[error("could not read config {path}")]
    ReadConfig {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// The config could not be parsed as JSON into the expected shape.
    #[error("could not parse config")]
    ParseConfig(#[from] serde_json::Error),

    /// `${VAR}` interpolation referenced an undefined environment variable.
    #[error("undefined environment variable: {0}")]
    UndefinedEnvVar(String),

    /// The config parsed but failed semantic validation. Carries every problem.
    #[error(transparent)]
    Invalid(#[from] ConfigErrors),
}

/// One or more configuration validation failures, reported together.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfigErrors(pub Vec<String>);

impl ConfigErrors {
    /// The individual failure messages.
    pub fn messages(&self) -> &[String] {
        &self.0
    }
}

impl std::fmt::Display for ConfigErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() == 1 {
            write!(f, "invalid config: {}", self.0[0])
        } else {
            writeln!(f, "invalid config ({} problems):", self.0.len())?;
            for (i, msg) in self.0.iter().enumerate() {
                if i + 1 < self.0.len() {
                    writeln!(f, "  - {msg}")?;
                } else {
                    write!(f, "  - {msg}")?;
                }
            }
            Ok(())
        }
    }
}

impl std::error::Error for ConfigErrors {}
