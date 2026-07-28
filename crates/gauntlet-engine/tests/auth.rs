//! Bearer-token injection, end to end.
//!
//! `settings.secrets` names a file holding a token; every request that does not
//! carry its own `Authorization` header must arrive with
//! `Authorization: Bearer <token>`. These assert against what the server
//! actually received, not against the config that was parsed — the regression
//! this guards is precisely a token that resolves but never reaches the wire.

mod support;

use std::path::PathBuf;

fn write_token(name: &str, contents: &str) -> PathBuf {
    let path =
        std::env::temp_dir().join(format!("gauntlet-e2e-token-{}-{name}", std::process::id()));
    std::fs::write(&path, contents).unwrap();
    path
}

fn config(base: &str, secrets: Option<&PathBuf>, headers: &str) -> gauntlet_core::BenchmarkConfig {
    let secrets_line = secrets
        .map(|p| format!(r#""secrets": "{}","#, p.display()))
        .unwrap_or_default();
    support::parse_config(&format!(
        r#"{{
          "targets": [{{ "name": "api", "url": "{base}" }}],
          "settings": {{ "iterations": 2, "concurrency": 1, {secrets_line} "warmup": {{ "iterations": 0 }} }},
          "payloads": [{{ "name": "p", "method": "GET", "path": "/x"{headers} }}]
        }}"#
    ))
}

#[tokio::test]
async fn a_configured_token_reaches_the_server_on_every_request() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;
    let token = write_token("present", "s3cr3t\n");

    gauntlet_engine::run_benchmark(&config(&mock.base, Some(&token), ""), None)
        .await
        .expect("benchmark runs");

    let seen = mock.auth_headers();
    assert_eq!(seen.len(), 2, "both requests were observed");
    assert!(
        seen.iter().all(|h| h.as_deref() == Some("Bearer s3cr3t")),
        "every request carried the bearer token, got {seen:?}"
    );

    let _ = std::fs::remove_file(token);
}

#[tokio::test]
async fn no_secrets_configured_means_no_authorization_header() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;

    gauntlet_engine::run_benchmark(&config(&mock.base, None, ""), None)
        .await
        .expect("benchmark runs");

    assert!(
        mock.auth_headers().iter().all(|h| h.is_none()),
        "an unauthenticated config must not invent a credential"
    );
}

#[tokio::test]
async fn an_explicit_authorization_header_wins_over_the_token_file() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;
    let token = write_token("override", "from-file");

    let headers = r#", "headers": { "Authorization": "Basic abc123" }"#;
    gauntlet_engine::run_benchmark(&config(&mock.base, Some(&token), headers), None)
        .await
        .expect("benchmark runs");

    assert!(
        mock.auth_headers()
            .iter()
            .all(|h| h.as_deref() == Some("Basic abc123")),
        "a payload's own Authorization header is not overwritten"
    );

    let _ = std::fs::remove_file(token);
}

#[tokio::test]
async fn a_configured_but_unreadable_secrets_file_fails_the_run() {
    let mock = support::start(200, serde_json::json!({"ok": true}), 0).await;
    let missing = PathBuf::from("/nonexistent/gauntlet/token.txt");

    let err = gauntlet_engine::run_benchmark(&config(&mock.base, Some(&missing), ""), None)
        .await
        .expect_err("a missing secrets file must not silently run unauthenticated");

    assert!(err.to_string().contains("token.txt"));
    assert_eq!(mock.request_count(), 0, "no requests were sent");
}
