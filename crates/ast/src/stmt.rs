use crate::{
  expr::{Expr, Function},
  pat::Pat,
};
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

#[derive(Debug)]
pub enum Stmt {
  VariableDef(VariableDef),
  FunctionDef(FunctionDef),
  Use(UseStatement),
  Expr(Expr),
}

impl Stmt {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::LetStatement => Self::VariableDef(VariableDef(node)),
      SyntaxKind::NamedFunction => Self::FunctionDef(FunctionDef(node)),
      SyntaxKind::NamedFunctionExpression => Self::FunctionDef(FunctionDef(node)),
      SyntaxKind::UseStatement => Self::Use(UseStatement(node)),
      _ => Self::Expr(Expr::cast(node)?),
    };

    Some(result)
  }

  pub fn expr(&self) -> Option<Expr> {
    match self {
      Stmt::VariableDef(def) => def.value(),
      Stmt::FunctionDef(def) => def.func(),
      Stmt::Use(_) => None,
      Stmt::Expr(expr) => Some(expr.clone()),
    }
  }
}

#[derive(Debug, Clone)]
pub struct UseStatement(SyntaxNode);

impl UseStatement {
  /// Module path only (e.g. `helper` or `subdir::helper`).
  pub fn path(&self) -> Option<UsePath> {
    self.0.children().find_map(UsePath::cast)
  }

  /// Names imported from the module (inside the braces): `use helper::{ONE, double}` => `["ONE", "double"]`.
  pub fn items(&self) -> Vec<String> {
    self
      .0
      .children()
      .find_map(UseList::cast)
      .map(|list| list.names())
      .unwrap_or_default()
  }
}

#[derive(Debug, Clone)]
pub struct UsePath(SyntaxNode);

impl UsePath {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    if node.kind() == SyntaxKind::UsePath {
      Some(Self(node))
    } else {
      None
    }
  }

  /// Path segments (identifiers). For `use foo::bar::*`, returns `["foo", "bar"]`.
  pub fn segments(&self) -> Vec<String> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .filter(|t| t.kind() == SyntaxKind::Identifier)
      .map(|t| t.text().to_string())
      .collect()
  }

  /// True if the path ends with `::*` (glob import).
  pub fn is_glob(&self) -> bool {
    let tokens: Vec<_> = self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .collect();
    tokens.last().map(|t| t.kind() == SyntaxKind::Asterisk) == Some(true)
  }
}

#[derive(Debug, Clone)]
pub struct UseList(SyntaxNode);

impl UseList {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    if node.kind() == SyntaxKind::UseList {
      Some(Self(node))
    } else {
      None
    }
  }

  pub fn names(&self) -> Vec<String> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .filter(|t| t.kind() == SyntaxKind::Identifier)
      .map(|t| t.text().to_string())
      .collect()
  }
}

#[derive(Debug)]
pub struct VariableDef(SyntaxNode);

impl VariableDef {
  /// True if this definition is prefixed with `pub`.
  pub fn is_pub(&self) -> bool {
    self
      .0
      .children_with_tokens()
      .next()
      .and_then(SyntaxElement::into_token)
      .map(|t| t.kind() == SyntaxKind::Pub)
      == Some(true)
  }

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
  /// True if this definition is prefixed with `pub`.
  pub fn is_pub(&self) -> bool {
    self
      .0
      .children_with_tokens()
      .next()
      .and_then(SyntaxElement::into_token)
      .map(|t| t.kind() == SyntaxKind::Pub)
      == Some(true)
  }

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
    self.0.children().find_map(|e| Expr::cast(e))
  }

  pub fn func(&self) -> Option<Expr> {
    Expr::cast(self.0.clone())
  }
}

impl From<Function> for FunctionDef {
  fn from(f: Function) -> Self {
    Self(f.0)
  }
}
