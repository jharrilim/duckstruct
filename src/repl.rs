use ctrlc;
use parser::parse;
use std::{
  io::{self, Write},
  process::exit,
};
use tycheck::TyCheck;

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

    let hir_db = hir::lower(root);
    let mut tycheck = TyCheck::new(hir_db);
    println!("{:#?}", tycheck.infer());

    input.clear();
  }
}
