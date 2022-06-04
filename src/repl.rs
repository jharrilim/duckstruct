use ctrlc;
use parser::parser::parse;
use std::{
  io::{self, Write},
  sync::{
    atomic::{AtomicBool, Ordering},
    Arc, Mutex,
  },
};

struct Repl {
  exit: AtomicBool,
}

impl Repl {
  pub(super) fn new() -> Repl {
    Repl {
      exit: AtomicBool::new(false),
    }
  }

  fn should_exit(&self) -> bool {
    self.exit.load(Ordering::Relaxed)
  }

  pub(super) fn exit(&self) {
    self.exit.store(true, Ordering::Relaxed)
  }

  fn run(&self) {

  }
}

pub fn repl() -> io::Result<()> {
  let repl = Arc::new(Mutex::new(Repl::new()));
  let stdin = io::stdin();
  let mut stdout = io::stdout();

  let mut input = String::new();
  let reepl = repl.clone();
  ctrlc::set_handler(move || {
    reepl.lock().unwrap().exit();
    writeln!(io::stdout(), "bye").unwrap();
  })
  .expect("Error setting Ctrl-C handler");

  writeln!(stdout, "Duckstruct AST Perusal Machine 3000 (v0.0.1)")?;
  writeln!(stdout, "Type exit to quit.")?;
  loop {

    write!(stdout, "→ ")?;
    stdout.flush()?;

    stdin.read_line(&mut input)?;

    if input.as_str().trim() == "exit" {
      break Ok(());
    }

    if repl.lock().unwrap().should_exit() {
      break Ok(());
    }

    let parse = parse(&input);
    println!("{}", parse.debug_tree());

    input.clear();
  }
}
