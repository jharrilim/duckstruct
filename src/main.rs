pub mod repl;

use std::io;

fn main() -> io::Result<()> {
  repl::repl()
}
