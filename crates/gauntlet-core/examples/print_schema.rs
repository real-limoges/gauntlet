//! Prints the derived config JSON schema. Used to regenerate
//! `schema/config-schema.json`: `cargo run -p gauntlet-core --example print_schema`.
fn main() {
    println!("{}", gauntlet_core::config_schema_string());
}
