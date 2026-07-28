//! Errors from talking to Tempo. Every variant is *advisory* — see the crate
//! docs.

use std::path::PathBuf;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("could not build the Tempo HTTP client: {0}")]
    Client(#[source] reqwest::Error),

    #[error("request to {url} failed")]
    Http {
        url: String,
        #[source]
        source: reqwest::Error,
    },

    #[error("Tempo returned HTTP {status} for {url}")]
    Status { url: String, status: u16 },

    #[error("could not parse the Tempo response from {url}")]
    Decode {
        url: String,
        #[source]
        source: serde_json::Error,
    },

    #[error("{url} is not a valid Tempo base URL: {reason}")]
    InvalidUrl { url: String, reason: String },

    #[error("could not write {path}")]
    Write {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}
