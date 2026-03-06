//! `check` command: parse and report diagnostics as JSON (for LSP/editors).

use std::io::Read;
use std::path::PathBuf;

use serde::Serialize;

/// Run the check command. Reads from path or stdin, parses, and prints LSP-style diagnostics as JSON.
/// Caller must ensure `json` is true (exits with error otherwise).
pub fn run(path: Option<PathBuf>, json: bool) {
  if !json {
    eprintln!("check requires --json");
    std::process::exit(1);
  }
  let source = match path.as_ref() {
    Some(p) => std::fs::read_to_string(p).unwrap_or_else(|e| {
      eprintln!("{}", e);
      std::process::exit(1);
    }),
    None => {
      let mut s = String::new();
      std::io::stdin().read_to_string(&mut s).unwrap();
      s
    }
  };
  let parse_result = parser::parse(&source);
  let diagnostics: Vec<LspDiagnostic> = parse_result
    .errors
    .iter()
    .map(|e| {
      let start_byte: u32 = e.range.start().into();
      let end_byte: u32 = e.range.end().into();
      let (start_line, start_char) = offset_to_line_character(&source, start_byte as usize);
      let (end_line, end_char) = offset_to_line_character(&source, end_byte as usize);
      LspDiagnostic {
        range: LspRange {
          start: LspPosition {
            line: start_line,
            character: start_char,
          },
          end: LspPosition {
            line: end_line,
            character: end_char,
          },
        },
        message: e.to_string(),
        severity: 1, // Error
      }
    })
    .collect();
  println!("{}", serde_json::to_string(&diagnostics).unwrap());
}

fn offset_to_line_character(source: &str, byte_offset: usize) -> (u32, u32) {
  let mut line: u32 = 0;
  let mut line_start = 0;
  for (i, c) in source.char_indices() {
    if i >= byte_offset {
      let character = (byte_offset - line_start) as u32;
      return (line, character);
    }
    if c == '\n' {
      line += 1;
      line_start = i + 1;
    }
  }
  let character = (byte_offset.saturating_sub(line_start)) as u32;
  (line, character)
}

#[derive(Serialize)]
struct LspDiagnostic {
  range: LspRange,
  message: String,
  severity: u8,
}

#[derive(Serialize)]
struct LspRange {
  start: LspPosition,
  end: LspPosition,
}

#[derive(Serialize)]
struct LspPosition {
  line: u32,
  character: u32,
}
