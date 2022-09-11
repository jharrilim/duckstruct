use ctrlc;
use parser::parse;
use std::{
  io::{self, Write},
  process::exit,
};
use tycheck::TyCheck;

#[derive(Debug, Default)]
struct ReplSession {
  history: Vec<String>,
}

impl ReplSession {
  pub fn add_line(&mut self, line: String) {
    self.history.push(line);
  }

  pub fn code(&self) -> String {
    self.history.join("\n")
  }
}

pub fn repl() -> io::Result<()> {
  let mut session = ReplSession {
    history: Vec::new(),
  };
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
    session.add_line(std::mem::take(&mut input));

    let parse = parse(&session.code());
    let root = ast::Root::cast(parse.syntax()).unwrap();

    let hir_db = hir::lower(root);
    let mut tycheck = TyCheck::new(hir_db);
    println!("{:#?}", tycheck.infer());

  }
}
