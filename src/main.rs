pub mod lexer;
pub mod parser;
pub mod repl;
pub mod syntax;

use std::io;

fn main() -> io::Result<()> {
  repl::repl()
}
