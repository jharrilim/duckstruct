pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod repl;

use std::io;

fn main() -> io::Result<()> {
    repl::repl()
}