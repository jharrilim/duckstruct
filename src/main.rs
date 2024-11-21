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

  if let Some(file_path) = args.compile {
    match Compiler::new().compile_file(file_path, TargetLang::Javascript) {
        Ok(()) => {}
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
