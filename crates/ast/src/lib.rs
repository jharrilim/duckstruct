/// Our AST is essentially a typed version of the CST that the parser produces.
/// It isn't quite abstract per-se since the underlying CST nodes are still
/// accessible internally.
// lib.rs
pub mod expr;
pub mod stmt;

pub use expr::Expr;
pub use stmt::Stmt;

use syntax::{SyntaxKind, SyntaxNode};

#[derive(Debug)]
pub struct Root(SyntaxNode);

impl Root {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    if node.kind() == SyntaxKind::Root {
      Some(Self(node))
    } else {
      None
    }
  }

  pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
    self.0.children().filter_map(Stmt::cast)
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    let result = 2 + 2;
    assert_eq!(result, 4);
  }
}
