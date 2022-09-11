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
    let root = ast::Root::cast(parse.syntax()).unwrap();

    dbg!(root
      .stmts()
      .filter_map(|stmt| if let ast::stmt::Stmt::VariableDef(var_def) = stmt {
        Some(var_def.value())
      } else {
        None
      })
      .collect::<Vec<_>>());

    input.clear();
  }
}
