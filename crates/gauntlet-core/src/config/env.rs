//! `.env` loading and `${VAR}` interpolation. See the crate docs for the
//! precedence rules and why this is hand-rolled.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Parse a `.env` file into key/value pairs, skipping blank and `#` lines and
/// stripping an optional `export ` prefix and matching surrounding quotes.
pub fn parse_env_file(content: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for line in content.lines() {
        let stripped = line.trim();
        if stripped.is_empty() || stripped.starts_with('#') {
            continue;
        }
        // Strip an optional `export ` prefix, then trim leading whitespace.
        let no_export = match stripped.strip_prefix("export ") {
            Some(rest) => rest.trim_start(),
            None => stripped,
        };
        // Split on the first '='. No '=' → skip the line.
        let Some(eq) = no_export.find('=') else {
            continue;
        };
        let key = no_export[..eq].trim();
        if key.is_empty() {
            continue;
        }
        let val = strip_quotes(no_export[eq + 1..].trim());
        map.insert(key.to_owned(), val.to_owned());
    }
    map
}

/// Strip a single pair of matching surrounding single or double quotes.
fn strip_quotes(t: &str) -> &str {
    let bytes = t.as_bytes();
    if bytes.len() >= 2 {
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
            return &t[1..t.len() - 1];
        }
    }
    t
}

/// Merge process env, `.env`, and `.env.local`, highest precedence last. Missing
/// files are ignored.
pub fn load_env_vars() -> HashMap<String, String> {
    let mut merged: HashMap<String, String> = std::env::vars().collect();
    // `.env` overrides process env; `.env.local` overrides both.
    merged.extend(read_env_file(".env"));
    merged.extend(read_env_file(".env.local"));
    merged
}

fn read_env_file<P: AsRef<Path>>(path: P) -> HashMap<String, String> {
    match fs::read_to_string(path) {
        Ok(content) => parse_env_file(&content),
        Err(_) => HashMap::new(),
    }
}

/// Interpolate `${VAR}` patterns in `input`. An undefined reference returns
/// `Err(var_name)`, leaving the message to the caller.
pub fn interpolate_env(env: &HashMap<String, String>, input: &str) -> Result<String, String> {
    let mut out = String::with_capacity(input.len());
    let mut remaining = input;
    loop {
        match remaining.find("${") {
            None => {
                out.push_str(remaining);
                return Ok(out);
            }
            Some(open) => {
                let before = &remaining[..open];
                let after_open = &remaining[open + 2..];
                match after_open.find('}') {
                    // Unclosed `${` → emit the remainder verbatim, stop.
                    None => {
                        out.push_str(before);
                        out.push_str(&remaining[open..]);
                        return Ok(out);
                    }
                    Some(close) => {
                        let var_name = &after_open[..close];
                        match env.get(var_name) {
                            None => return Err(var_name.to_owned()),
                            Some(val) => {
                                out.push_str(before);
                                out.push_str(val);
                                remaining = &after_open[close + 1..];
                            }
                        }
                    }
                }
            }
        }
    }
}
