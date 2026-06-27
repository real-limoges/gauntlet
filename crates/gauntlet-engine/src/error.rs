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

    /// The HTTP client could not be built from the settings.
    #[error("could not build HTTP client: {0}")]
    Client(String),

    /// A lifecycle hook (setup/teardown/health-check) failed.
    #[error("lifecycle hook failed for target {target}: {message}")]
    Lifecycle { target: String, message: String },

    /// Writing the latency CSV failed.
    #[error("could not write CSV {path}: {source}")]
    Csv {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}
