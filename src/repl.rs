use crate::parser::Parser;
use std::io::{self, Write};

pub fn repl() -> io::Result<()> {
  let stdin = io::stdin();
  let mut stdout = io::stdout();

  let mut input = String::new();

  ctrlc::set_handler(move || {
    println!("received Ctrl+C!");
    std::process::exit(0)
  })
  .expect("Error setting Ctrl-C handler");

  loop {
    write!(stdout, "→ ")?;
    stdout.flush()?;

    stdin.read_line(&mut input)?;

    let parse = Parser::new(&input).parse();

    println!("{}", parse.debug_tree());

    input.clear();
  }
}
