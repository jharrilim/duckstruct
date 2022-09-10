use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use crate::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
  VariableDef(VariableDef),
  Expr(Expr),
}

impl Stmt {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::LetStatement => Self::VariableDef(VariableDef(node)),
      _ => Self::Expr(Expr::cast(node)?),
    };

    Some(result)
  }
}

#[derive(Debug)]
pub struct VariableDef(SyntaxNode);

impl VariableDef {
  pub fn name(&self) -> Option<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| token.kind() == SyntaxKind::Identifier)
  }

  pub fn value(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}
