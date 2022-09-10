// lib.rs
pub mod expr;
pub mod stmt;

use expr::Expr;
use stmt::Stmt;
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

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
