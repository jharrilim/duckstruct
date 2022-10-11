use parser::parse;
use rustyline::{self, error::ReadlineError, Result};
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

pub fn repl() -> Result<()> {
  let mut session = ReplSession {
    history: Vec::new(),
  };
  let mut rl = rustyline::Editor::<()>::new()?;

  println!("Duckstruct AST Perusal Machine 3000 (v0.0.1)");
  println!("Type exit to quit.");
  loop {
    let readline = rl.readline(">>> ");
    match readline {
      Ok(line) => {
        if line.trim() == "exit" {
          break;
        }
        rl.add_history_entry(line.as_str());
        session.add_line(line);

        let parse = parse(&session.code());
        let root = ast::Root::cast(parse.syntax()).unwrap();

        let hir_db = hir::lower(root);
        let mut tycheck = TyCheck::new(hir_db);
        tycheck.infer();
        println!("{:#?}", tycheck);
      }
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
        break;
      }
      Err(ReadlineError::Eof) => {
        println!("CTRL-D");
        break;
      }
      Err(err) => {
        println!("Error: {:?}", err);
        break;
      }
    }
  }
  Ok(())
}
