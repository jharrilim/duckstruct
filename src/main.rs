pub mod repl;
use std::path::PathBuf;

use clap::*;
use compile::{Compiler, TargetLang};
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
}

fn main() -> Result<()> {
  let args = Args::parse();

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
