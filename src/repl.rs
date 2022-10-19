use codegen::js::JsGenerator;
use parser::parse;
use rustyline::{self, error::ReadlineError, validate::MatchingBracketValidator, Result};
use rustyline_derive::*;
use tycheck::TyCheck;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
  #[rustyline(Validator)]
  pub brackets: MatchingBracketValidator,
}

#[derive(Debug, Default)]
struct ReplSession {
  statements: Vec<String>,
}

impl ReplSession {
  pub fn add_line(&mut self, line: String) {
    self.statements.push(line);
  }

  pub fn clear(&mut self) {
    self.statements.clear();
  }

  pub fn code(&self) -> String {
    let stmts = self.statements.clone().into_iter().collect::<Vec<String>>();
    stmts.join("\n")
  }

  pub fn eval(&mut self, line: Option<String>) {
    let lines = match &line {
      Some(line) => {
        if line.eq(".js") {
          self.code()
        } else {
          let mut lines = self.statements.clone();
          lines.push(line.clone());
          lines.into_iter().collect::<Vec<String>>().join("\n")
        }
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
      if line
        .map(|l| l.starts_with("f ") || l.starts_with("let "))
        .unwrap_or(false)
      {
        self.statements.pop();
      }
      return;
    }

    if line.is_some() && line.unwrap().eq(".js") {
      let js = JsGenerator::new(&tycheck).generate_js();
      println!("---\njavascript\n---\n{}", js);
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
  let validator = InputValidator {
    brackets: MatchingBracketValidator::new(),
  };
  let mut rl = rustyline::Editor::new()?;
  rl.set_helper(Option::Some(validator));

  println!("Duckstruct 🐣 (v0.0.1)");
  println!("Type exit to quit.");
  loop {
    let readline = rl.readline("λ ");
    match readline {
      Ok(line) => {
        if line.trim() == "exit" {
          break;
        }
        if line.trim() == "clear" {
          session.clear();
          continue;
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
