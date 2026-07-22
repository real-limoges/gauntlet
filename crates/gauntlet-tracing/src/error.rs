//! Errors from talking to Tempo.
//!
//! Every variant here is *advisory*. Trace analysis is a diagnostic bolted onto
//! a benchmark: a Tempo that is down, slow, or has not yet ingested the run's
//! spans must never turn a clean benchmark into a failed one (ADR M6-A §7). The
//! caller logs these and omits the section — so the messages are written to be
//! read by a human debugging their Tempo setup, and always name the URL.

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
