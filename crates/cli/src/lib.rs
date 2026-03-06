//! CLI argument parsing and command dispatch for the Duckstruct binary.

mod commands;
mod repl;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use compile::Compiler;
use rustyline::Result;

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

  /// Create a new Duckstruct project in the given directory
  Init {
    /// Directory in which to create the project (default: current directory)
    path: Option<PathBuf>,
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

/// Re-export for use as the binary's main return type.
pub use rustyline::Result as CliResult;

/// Parse arguments and run the appropriate command or the REPL.
pub fn run() -> Result<()> {
  let args = Args::parse();

  if let Some(Command::Check { path, json }) = args.command {
    commands::check::run(path, json);
    return Ok(());
  }

  if let Some(Command::Init { path }) = args.command {
    commands::init::run(path);
    return Ok(());
  }

  if let Some(Command::Compile { path }) = args.command {
    commands::compile::run(path);
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
