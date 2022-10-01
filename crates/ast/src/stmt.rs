use crate::expr::Expr;
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

#[derive(Debug)]
pub enum Stmt {
  VariableDef(VariableDef),
  FunctionDef(FunctionDef),
  Expr(Expr),
}

impl Stmt {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::LetStatement => Self::VariableDef(VariableDef(node)),
      SyntaxKind::NamedFunction => Self::FunctionDef(FunctionDef(node)),
      SyntaxKind::NamedFunctionExpression => Self::FunctionDef(FunctionDef(node)),

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

#[derive(Debug)]
pub struct FunctionDef(SyntaxNode);
impl FunctionDef {
  pub fn name(&self) -> Option<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| token.kind() == SyntaxKind::Identifier)
  }

  pub fn params(&self) -> Vec<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .find_map(|token_or_node| {
        if let Some(_) = token_or_node
          .clone()
          .into_token()
          .filter(|token| token.kind() == SyntaxKind::ArgumentList)
        {
          let node = token_or_node.into_node()?;
          let childs: Vec<SyntaxToken> = node
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .filter(|token| token.kind() == SyntaxKind::Identifier)
            .collect();
          Some(childs)
        } else {
          None
        }
      })
      .unwrap_or_default()
  }

  pub fn body(&self) -> Expr {
    self.0.children().find_map(Expr::cast).unwrap()
  }
}
