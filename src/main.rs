pub mod repl;

use rustyline::Result;

fn main() -> Result<()> {
  repl::repl()
}
