//! Engine error surface. Distinct from `gauntlet_core::Error` (config/parse): the
//! engine adds the runtime failure modes — client construction, lifecycle hooks,
//! and CSV output.

use std::path::PathBuf;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, EngineError>;

#[derive(Debug, Error)]
pub enum EngineError {
    /// The config failed validation before any request was sent.
    #[error(transparent)]
    Config(#[from] gauntlet_core::ConfigErrors),

    /// One or more `matches` patterns in the config are not valid regexes.
    /// Caught before the run so a config typo is not reported as the service
    /// failing every assertion.
    #[error("invalid validation pattern(s):\n  {}", .0.join("\n  "))]
    Pattern(Vec<String>),

    /// The HTTP client could not be built from the settings.
    #[error("could not build HTTP client: {0}")]
    Client(String),

    /// A lifecycle hook (setup/teardown/health-check) failed.
    #[error("lifecycle hook failed for target {target}: {message}")]
    Lifecycle { target: String, message: String },

    /// Writing the latency CSV failed.
    #[error("could not read secrets file {path}")]
    TokenRead {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("could not write CSV {path}")]
    Csv {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}
