//! Bearer-token resolution.
//!
//! `settings.secrets` points at a file holding a bearer token, read once at
//! startup and applied to every request that does not carry its own
//! `Authorization` header (see `client::send`). Keeping it in a file rather
//! than the config means the token never lands in a committed JSON file.

use std::path::Path;

use crate::error::{EngineError, Result};

/// Read a bearer token from `path`, trimming surrounding whitespace — token
/// files routinely end with a trailing newline, which would otherwise be sent
/// as part of the credential.
///
/// An empty or whitespace-only file yields `None` rather than an empty bearer
/// header, matching the Haskell `addAuth` no-op on an empty token.
pub fn read_token(path: impl AsRef<Path>) -> Result<Option<String>> {
    let path = path.as_ref();
    let raw = std::fs::read_to_string(path).map_err(|source| EngineError::TokenRead {
        path: path.to_path_buf(),
        source,
    })?;

    let token = raw.trim();
    Ok((!token.is_empty()).then(|| token.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(name: &str, contents: &str) -> std::path::PathBuf {
        let path =
            std::env::temp_dir().join(format!("gauntlet-token-{}-{name}", std::process::id()));
        std::fs::write(&path, contents).unwrap();
        path
    }

    #[test]
    fn a_token_file_is_read_and_trimmed() {
        let path = write("plain", "  s3cr3t-token\n");
        assert_eq!(read_token(&path).unwrap(), Some("s3cr3t-token".into()));
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn an_empty_token_file_means_no_auth_rather_than_an_empty_credential() {
        for (name, contents) in [("empty", ""), ("blank", "   \n\t ")] {
            let path = write(name, contents);
            assert_eq!(read_token(&path).unwrap(), None);
            let _ = std::fs::remove_file(path);
        }
    }

    #[test]
    fn a_missing_token_file_names_the_path_it_looked_for() {
        let err = read_token("/nonexistent/gauntlet-token.txt")
            .expect_err("a configured but unreadable secrets file is fatal");
        assert!(err.to_string().contains("/nonexistent/gauntlet-token.txt"));
    }
}
