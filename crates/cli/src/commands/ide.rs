//! `ide` subcommands: LSP-style query (hover + definition) and project symbol export.

use std::io::Read;
use std::path::PathBuf;

use compile::ide::{ide_query_at_position, ide_symbols_project};

pub fn run_query(file: PathBuf, line: u32, character: u32, project: Option<PathBuf>) {
  let mut source = String::new();
  std::io::stdin()
    .read_to_string(&mut source)
    .unwrap_or_else(|e| {
      eprintln!("{}", e);
      std::process::exit(1);
    });
  let out = ide_query_at_position(
    &source,
    &file,
    project.as_deref(),
    line,
    character,
  );
  println!(
    "{}",
    serde_json::to_string(&out).unwrap_or_else(|e| {
      format!("{{\"error\":\"serde: {}\"}}", e)
    })
  );
}

pub fn run_symbols(project: PathBuf) {
  match ide_symbols_project(&project) {
    Ok(symbols) => {
      println!(
        "{}",
        serde_json::to_string(&symbols).unwrap_or_else(|_| "[]".into())
      );
    }
    Err(e) => {
      eprintln!("{}", e);
      std::process::exit(1);
    }
  }
}
