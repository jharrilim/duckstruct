pub mod repl;
use std::path::PathBuf;

use clap::*;
use compile::{Compiler, TargetLang};
use rustyline::Result;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[arg(short, long)]
  compile: Option<PathBuf>,

  #[arg(short, long)]
  eval: Option<String>,
}

fn main() -> Result<()> {
  let args = Args::parse();

  if let Some(path) = args.compile {
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
