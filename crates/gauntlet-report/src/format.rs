//! Shared rendering helpers: number formatting and the escaping functions every
//! markup backend is required to route untrusted strings through.
//!
//! Target names, URLs, and validation messages come from config and from server
//! responses — none of it is trusted to be markup-safe. The Haskell JUnit
//! reporter concatenated these straight into XML, so a target named `a&b`
//! produced a document no parser would accept. Backends here must use
//! [`xml_escape`] / [`html_escape`] / [`prom_label_escape`] instead of
//! interpolating directly.

/// Latency in milliseconds, two decimals: `12.34`.
pub fn ms(value: f64) -> String {
    format!("{value:.2}")
}

/// A ratio rendered as a signed percentage: `0.153` → `+15.3%`.
pub fn signed_pct(ratio: f64) -> String {
    format!("{:+.1}%", ratio * 100.0)
}

/// A probability rendered as a percentage: `0.94` → `94.0%`.
pub fn prob_pct(probability: f64) -> String {
    format!("{:.1}%", probability * 100.0)
}

/// Escape text for an XML text node or attribute value.
pub fn xml_escape(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(ch),
        }
    }
    out
}

/// Escape text for an HTML text node or double-quoted attribute value.
pub fn html_escape(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

/// Escape a Prometheus label *value* per the exposition format: backslash,
/// double quote, and newline are the only characters that need it.
pub fn prom_label_escape(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            _ => out.push(ch),
        }
    }
    out
}

/// Sanitize a string into a Prometheus metric-name fragment: anything outside
/// `[a-zA-Z0-9_]` becomes `_`. Used for target names embedded in metric names.
pub fn prom_name_sanitize(input: &str) -> String {
    input
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

/// Escape a string for use inside a markdown table cell: `|` would otherwise
/// split the row, and a newline would end it.
pub fn md_cell_escape(input: &str) -> String {
    input.replace('|', "\\|").replace('\n', " ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_numbers() {
        assert_eq!(ms(12.3456), "12.35");
        assert_eq!(signed_pct(0.1534), "+15.3%");
        assert_eq!(signed_pct(-0.05), "-5.0%");
        assert_eq!(prob_pct(0.9412), "94.1%");
    }

    #[test]
    fn xml_escapes_all_five_predefined_entities() {
        assert_eq!(
            xml_escape(r#"a&b<c>d"e'f"#),
            "a&amp;b&lt;c&gt;d&quot;e&apos;f"
        );
    }

    #[test]
    fn html_escapes_angle_brackets_and_quotes() {
        assert_eq!(
            html_escape(r#"<script>alert("x")</script>"#),
            "&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;"
        );
    }

    #[test]
    fn prom_label_escapes_only_backslash_quote_newline() {
        assert_eq!(prom_label_escape("a\\b\"c\nd"), "a\\\\b\\\"c\\nd");
        // `<` and `&` are ordinary characters in a label value.
        assert_eq!(prom_label_escape("a<b&c"), "a<b&c");
    }

    #[test]
    fn prom_name_sanitizes_to_identifier_characters() {
        assert_eq!(prom_name_sanitize("api-v2 (prod)"), "api_v2__prod_");
    }

    #[test]
    fn md_cell_escape_protects_table_structure() {
        assert_eq!(md_cell_escape("a|b\nc"), "a\\|b c");
    }
}
