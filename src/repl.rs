use ctrlc;
use parser::parse;
use std::{
  io::{self, Write},
  process::exit,
};

pub fn repl() -> io::Result<()> {
  let stdin = io::stdin();
  let mut stdout = io::stdout();

  let mut input = String::new();

  ctrlc::set_handler(move || {
    println!("\nbye");
    exit(0);
  })
  .expect("Error setting Ctrl-C handler");

  writeln!(stdout, "Duckstruct AST Perusal Machine 3000 (v0.0.1)")?;
  writeln!(stdout, "Type exit to quit.")?;
  loop {
    write!(stdout, "→ ")?;
    stdout.flush()?;

    stdin.read_line(&mut input)?;

    if input.trim() == "exit" {
      break Ok(());
    }

    let parse = parse(&input);
    println!("{}", parse.debug_tree());

    input.clear();
  }
}
