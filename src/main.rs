pub mod repl;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use compile::{Compiler, TargetLang};
use rustyline::Result;
use serde::Serialize;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[command(subcommand)]
  command: Option<Command>,

  #[arg(short, long)]
  eval: Option<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
  /// Compile a Duckstruct file or project to the target language (JS by default, or from manifest).
  #[command(visible_aliases = ["c", "build"])]
  Compile {
    /// Path to a .duck file or project directory to compile
    path: PathBuf,
  },

  /// Parse and report diagnostics as JSON (for LSP/editors). Reads file or stdin.
  Check {
    /// Path to a .ds file (omit to read from stdin)
    path: Option<PathBuf>,
    /// Output LSP-style diagnostics as JSON
    #[arg(long)]
    json: bool,
  },
}

/// Convert byte offset in source to (line, character) 0-based for LSP.
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
  severity: u8, // 1=Error
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

fn main() -> Result<()> {
  let args = Args::parse();

  if let Some(Command::Check { path, json }) = args.command {
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
        use std::io::Read;
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
    return Ok(());
  }

  if let Some(Command::Compile { path }) = args.command {
    match compile::resolve_entry_and_project_root(&path) {
      Ok((entry_path, project_root)) => {
        let target = match &project_root {
          Some(root) => compile::target_from_manifest_dir(root).unwrap_or(TargetLang::Javascript),
          None => TargetLang::Javascript,
        };
        if let Err(err) =
          Compiler::new().compile_file(entry_path, project_root, target)
        {
          println!("Compilation failed: {}", err);
        }
      }
      Err(err) => println!("Compilation failed: {}", err),
    };
    return Ok(());
  }

  match args.eval {
    Some(source) => {
      let compiler = Compiler::new();
      let result = compiler.eval(&source);
      if let Ok(ty) = result {
        println!("{}", ty);
      } else {
        println!("{}", result.unwrap_err());
      }
      Ok(())
    }
    _ => repl::repl(),
  }
}
