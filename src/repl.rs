use crate::parser::parse;
use std::io::{self, Write};

pub fn repl() -> io::Result<()> {
  let stdin = io::stdin();
  let mut stdout = io::stdout();

  let mut input = String::new();

  loop {
    write!(stdout, "→ ")?;
    stdout.flush()?;

    stdin.read_line(&mut input)?;

    let parse = parse(&input);
    println!("{}", parse.debug_tree());

    input.clear();
  }
}
