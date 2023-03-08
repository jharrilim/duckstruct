use crate::{expr::{Expr, Function}, pat::Pat};
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

  pub fn expr(&self) -> Option<Expr> {
    match self {
      Stmt::VariableDef(def) => def.value(),
      Stmt::FunctionDef(def) => def.body(),
      Stmt::Expr(expr) => Some(expr.clone()),
    }
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

  pub fn pattern(&self) -> Option<Pat> {
    self.0.children().find_map(Pat::cast)
  }


  pub fn value(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}

#[derive(Debug)]
pub struct FunctionDef(pub(crate) SyntaxNode);
impl FunctionDef {
  pub fn name(&self) -> Option<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| token.kind() == SyntaxKind::Identifier)
  }

  pub fn params(&self) -> impl Iterator<Item = SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .find(|t| t.kind() == SyntaxKind::ArgumentList)
      .unwrap()
      .into_node()
      .unwrap()
      .children_with_tokens()
      .filter(|token| token.kind() == SyntaxKind::VariableReference)
      .map(|t| t.into_node().unwrap().first_token().unwrap())
  }

  pub fn body(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}

impl From<Function> for FunctionDef {
  fn from(f: Function) -> Self {
    Self(f.0)
  }
}
