//! Reporting errors. See the crate docs for why reporters are fallible.

use std::path::{Path, PathBuf};

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("could not read {path}")]
    Read {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("could not write {path}")]
    Write {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("baseline not found: {name} (looked in {path})")]
    BaselineNotFound { name: String, path: PathBuf },

    #[error(
        "{path} is a baseline from the Haskell implementation, which is no longer readable; \
         re-run with --save-baseline to regenerate it"
    )]
    LegacyBaseline { path: PathBuf },

    #[error("could not serialize report data")]
    Json(#[from] serde_json::Error),

    #[error("could not render chart: {0}")]
    Chart(String),

    #[error("could not push metrics to {url}")]
    Push {
        url: String,
        #[source]
        source: reqwest::Error,
    },
}

impl Error {
    pub(crate) fn read(path: impl AsRef<Path>, source: std::io::Error) -> Self {
        Error::Read {
            path: path.as_ref().to_path_buf(),
            source,
        }
    }

    pub(crate) fn write(path: impl AsRef<Path>, source: std::io::Error) -> Self {
        Error::Write {
            path: path.as_ref().to_path_buf(),
            source,
        }
    }
}
