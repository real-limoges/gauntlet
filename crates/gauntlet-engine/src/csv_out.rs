//! Per-run latency CSV — the dump that reporters and the plot script consume.
//!
//! Seven columns, matching the existing format:
//! `target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso`.
//! `latency_ms = duration_ns / 1e6`; `timestamp_iso` is the request's wall-clock
//! start as RFC 3339.

use std::path::Path;
use std::time::SystemTime;

use chrono::{DateTime, Utc};
use gauntlet_core::{ns_to_ms, HttpMethod, TestingResponse};

use crate::error::{EngineError, Result};

/// One CSV row: an endpoint context joined with a single response.
pub struct CsvRow<'a> {
    pub target_name: &'a str,
    pub payload_id: &'a str,
    pub url: &'a str,
    pub method: HttpMethod,
    pub response: &'a TestingResponse,
    pub requested_at: SystemTime,
}

/// A latency-CSV writer with the header already emitted. Wrap in a mutex to share
/// across the concurrent per-endpoint tasks.
pub struct CsvSink {
    writer: csv::Writer<std::fs::File>,
    path: std::path::PathBuf,
}

impl CsvSink {
    /// Create (truncating) the CSV at `path` and write the header row.
    pub fn create(path: &Path) -> Result<Self> {
        // The results directory usually does not exist yet on a fresh checkout
        // or in CI, and failing the whole run over it would be absurd.
        if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
            std::fs::create_dir_all(parent).map_err(|source| EngineError::Csv {
                path: path.to_path_buf(),
                source,
            })?;
        }
        let mut writer = csv::Writer::from_path(path).map_err(|e| csv_err(path, e))?;
        writer
            .write_record([
                "target_name",
                "payload_id",
                "url",
                "method",
                "status_code",
                "latency_ms",
                "timestamp_iso",
            ])
            .map_err(|e| csv_err(path, e))?;
        Ok(CsvSink {
            writer,
            path: path.to_owned(),
        })
    }

    /// Append one response row.
    pub fn write_row(&mut self, row: &CsvRow<'_>) -> Result<()> {
        let latency_ms = ns_to_ms(row.response.duration).0;
        let timestamp = DateTime::<Utc>::from(row.requested_at).to_rfc3339();
        self.writer
            .write_record([
                row.target_name,
                row.payload_id,
                row.url,
                row.method.as_str(),
                &row.response.status.to_string(),
                &format!("{latency_ms}"),
                &timestamp,
            ])
            .map_err(|e| csv_err(&self.path, e))
    }

    /// Flush buffered rows to disk.
    pub fn flush(&mut self) -> Result<()> {
        self.writer.flush().map_err(|e| EngineError::Csv {
            path: self.path.clone(),
            source: e,
        })
    }
}

fn csv_err(path: &Path, e: csv::Error) -> EngineError {
    EngineError::Csv {
        path: path.to_owned(),
        // csv::Error is usually an io::Error underneath; flatten what we can,
        // otherwise wrap the message.
        source: match e.into_kind() {
            csv::ErrorKind::Io(io) => io,
            other => std::io::Error::other(format!("{other:?}")),
        },
    }
}
