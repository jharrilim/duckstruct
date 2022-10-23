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
}

fn main() -> Result<()> {
  let args = Args::parse();

  match args.compile {
    Some(file_path) => {
      match Compiler::new().compile_file(file_path, TargetLang::Javascript) {
        Ok(()) => {}
        Err(err) => println!("Compilation failed: {}", err),
      };
      Ok(())
    }
    _ => {
      repl::repl()?;
      Ok(())
    }
  }
}
