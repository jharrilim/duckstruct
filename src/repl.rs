use parser::parse;
use rustyline::{self, error::ReadlineError, Result};
use tycheck::TyCheck;

#[derive(Debug, Default)]
struct ReplSession {
  statements: Vec<String>,
}

impl ReplSession {
  pub fn add_line(&mut self, line: String) {
    self.statements.push(line);
  }

  pub fn code(&self) -> String {
    let stmts = self
      .statements
      .clone()
      .into_iter()
      .rev()
      .collect::<Vec<String>>();
    stmts.join("\n")
  }

  pub fn eval(&self, line: Option<String>) {
    let lines = match line {
      Some(line) => {
        let mut lines = self.statements.clone();
        lines.push(line);
        lines.into_iter().rev().collect::<Vec<String>>().join("\n")
      }
      None => self.code(),
    };

    let parse = parse(&lines);
    let root = ast::Root::cast(parse.syntax()).unwrap();

    let hir_db = hir::lower(root);
    let mut tycheck = TyCheck::new(hir_db);
    tycheck.infer();

    if tycheck.diagnostics.has_errors() {
      println!("{:#?}", tycheck);
      tycheck.diagnostics.print_errors();
      return;
    }

    if let Some(def) = tycheck.ty_db.definition("") {
      let val = tycheck.ty_db.expr(def.value());
      println!("{}", val.ty());
    }
  }
}

pub fn repl() -> Result<()> {
  let mut session = ReplSession {
    statements: Vec::new(),
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

        // quick hack to only store statements in session
        if line.contains("f ") || line.contains("let ") {
          session.add_line(line);
          session.eval(None);
        } else {
          session.eval(Some(line));
        }
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
