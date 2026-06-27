//! The M3 exit criterion: a localhost run produces a sample vector that the stats
//! core consumes, and the latency CSV is written with the expected 7 columns.

mod support;

use gauntlet_engine::run_benchmark;
use support::{parse_config, start};

fn temp_csv(tag: &str) -> std::path::PathBuf {
    std::env::temp_dir().join(format!("gauntlet_m3_{}_{}.csv", std::process::id(), tag))
}

#[tokio::test]
async fn run_produces_consumable_stats_and_csv() {
    let mock = start(200, serde_json::json!({"ok": true}), 1).await;
    let cfg = parse_config(&format!(
        r#"{{"targets":[{{"name":"prod","url":"{}"}}],
            "settings":{{"iterations":20,"concurrency":4,"warmup":{{"iterations":0}}}},
            "payloads":[{{"name":"ping","method":"GET","path":"/ping"}}]}}"#,
        mock.base
    ));

    let csv_path = temp_csv("e2e");
    let run = run_benchmark(&cfg, Some(&csv_path)).await.unwrap();

    // --- stats are finite, ordered, and reflect the sample ------------------
    let ep = &run.targets[0].endpoints[0];
    let s = &ep.stats;
    assert_eq!(s.total_requests, 20);
    assert_eq!(s.count_success, 20);
    assert_eq!(s.count_failure, 0);
    assert!(s.min_ms <= s.p50_ms, "min ≤ p50");
    assert!(s.p50_ms <= s.p95_ms, "p50 ≤ p95");
    assert!(s.p95_ms <= s.p99_ms, "p95 ≤ p99");
    assert!(s.p99_ms <= s.max_ms, "p99 ≤ max");
    assert!(s.mean_ms.is_finite() && s.mean_ms > 0.0);

    // --- CSV: header + one row per response, latency parseable --------------
    let text = std::fs::read_to_string(&csv_path).unwrap();
    let mut lines = text.lines();
    assert_eq!(
        lines.next().unwrap(),
        "target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso"
    );
    let rows: Vec<&str> = lines.collect();
    assert_eq!(rows.len(), 20);
    for row in rows {
        let cols: Vec<&str> = row.split(',').collect();
        assert_eq!(cols[0], "prod");
        assert_eq!(cols[1], "ping");
        assert_eq!(cols[3], "GET");
        assert_eq!(cols[4], "200");
        cols[5].parse::<f64>().expect("latency_ms is a number");
    }

    let _ = std::fs::remove_file(&csv_path);
}
