use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::{stmt::FunctionDef, Stmt};

#[derive(Debug, Clone)]
pub enum Expr {
  BinaryExpr(BinaryExpr),
  NumberLit(Number),
  StringLit(Str),
  BooleanLit(Boolean),
  ParenExpr(ParenExpr),
  UnaryExpr(UnaryExpr),
  VariableRef(VariableRef),
  Function(Function),
  FunctionCall(FunctionCall),
  Block(Block),
  Array(Array),
  Object(Object),
  ObjectFieldAccess(ObjectFieldAccess),
  Conditional(Conditional),
}

impl Expr {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::InfixExpression => Self::BinaryExpr(BinaryExpr(node)),
      SyntaxKind::Number => Self::NumberLit(Number(node)),
      SyntaxKind::String => Self::StringLit(Str(node)),
      SyntaxKind::Boolean => Self::BooleanLit(Boolean(node)),
      SyntaxKind::ParenExpression => Self::ParenExpr(ParenExpr(node)),
      SyntaxKind::UnaryExpression => Self::UnaryExpr(UnaryExpr(node)),
      SyntaxKind::VariableReference => Self::VariableRef(VariableRef(node)),
      SyntaxKind::AnonymousFunction => Self::Function(Function(node)),
      SyntaxKind::AnonymousFunctionExpression => Self::Function(Function(node)),
      SyntaxKind::NamedFunction => Self::Function(Function(node)),
      SyntaxKind::NamedFunctionExpression => Self::Function(Function(node)),
      SyntaxKind::FunctionCallExpression => Self::FunctionCall(FunctionCall(node)),
      SyntaxKind::BlockExpression => Self::Block(Block(node)),
      SyntaxKind::ArrayExpression => Self::Array(Array(node)),
      SyntaxKind::ConditionalExpression => Self::Conditional(Conditional(node)),
      SyntaxKind::ObjectExpression => Self::Object(Object(node)),
      SyntaxKind::ObjectFieldAccessExpression => Self::ObjectFieldAccess(ObjectFieldAccess(node)),
      _ => return None,
    };

    Some(result)
  }
}

#[derive(Debug, Clone)]
pub struct BinaryExpr(SyntaxNode);
impl BinaryExpr {
  pub fn lhs(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }

  pub fn rhs(&self) -> Option<Expr> {
    self.0.children().filter_map(Expr::cast).nth(1)
  }

  pub fn op(&self) -> Option<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| {
        matches!(token.kind(), |SyntaxKind::Plus| SyntaxKind::Minus
          | SyntaxKind::Asterisk
          | SyntaxKind::ForwardSlash
          | SyntaxKind::DoubleEquals
          | SyntaxKind::NotEquals
          | SyntaxKind::LessThan
          | SyntaxKind::GreaterThan
          | SyntaxKind::LessThanOrEqual
          | SyntaxKind::GreaterThanOrEqual)
      })
  }
}

#[derive(Debug, Clone)]
pub struct Number(SyntaxNode);
impl Number {
  pub fn parse(&self) -> f64 {
    self.0.first_token().unwrap().text().parse().unwrap()
  }
}

#[derive(Debug, Clone)]
pub struct Str(SyntaxNode);
impl Str {
  pub fn parse(&self) -> String {
    self
      .0
      .first_token()
      .unwrap()
      .text()
      .strip_prefix('"')
      .unwrap()
      .strip_suffix('"')
      .unwrap()
      .to_string()
  }
}

#[derive(Debug, Clone)]
pub struct Boolean(SyntaxNode);
impl Boolean {
  pub fn parse(&self) -> bool {
    self.0.first_token().unwrap().text().parse().unwrap()
  }
}

#[derive(Debug, Clone)]
pub struct ParenExpr(SyntaxNode);
impl ParenExpr {
  pub fn expr(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr(SyntaxNode);
impl UnaryExpr {
  pub fn expr(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }

  pub fn op(&self) -> Option<SyntaxToken> {
    self
      .0
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| matches!(token.kind(), SyntaxKind::Minus | SyntaxKind::Bang))
  }
}

#[derive(Debug, Clone)]
pub struct VariableRef(SyntaxNode);
impl VariableRef {
  pub fn name(&self) -> String {
    // First token should be Identifier
    self.0.first_token().unwrap().text().to_string()
  }
}

#[derive(Debug, Clone)]
pub struct Function(pub(crate) SyntaxNode);
impl Function {
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

impl From<FunctionDef> for Function {
  fn from(function_def: FunctionDef) -> Self {
    Self(function_def.0)
  }
}

#[derive(Debug, Clone)]
pub struct FunctionCall(SyntaxNode);
impl FunctionCall {
  pub fn func(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }

  pub fn args(&self) -> impl Iterator<Item = Expr> {
    self
      .0
      .children_with_tokens()
      .find(|t| t.kind() == SyntaxKind::ArgumentList)
      .unwrap()
      .into_node()
      .unwrap()
      .children()
      .filter_map(Expr::cast)
  }
}

#[derive(Debug, Clone)]
pub struct Block(SyntaxNode);
impl Block {
  pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
    self.0.children().filter_map(Stmt::cast)
  }
}

#[derive(Debug, Clone)]
pub struct Array(SyntaxNode);
impl Array {
  pub fn elements(&self) -> impl Iterator<Item = Expr> {
    self.0.children().filter_map(Expr::cast)
  }
}

#[derive(Debug, Clone)]
pub struct Conditional(SyntaxNode);
impl Conditional {
  pub fn predicate(&self) -> Option<Expr> {
    self
      .0
      .children()
      .find(|t| t.kind() == SyntaxKind::ConditionalPredicate)
      .and_then(|t| t.first_child())
      .and_then(Expr::cast)
  }

  pub fn then_branch(&self) -> Option<Expr> {
    self
      .0
      .children()
      .find(|t| t.kind() == SyntaxKind::IfCondition)
      .and_then(|t| t.first_child())
      .and_then(Expr::cast)
  }

  pub fn else_branch(&self) -> Option<Expr> {
    self
      .0
      .children()
      .find(|t| t.kind() == SyntaxKind::ElseCondition)
      .and_then(|t| t.first_child())
      .and_then(Expr::cast)
  }
}

#[derive(Debug, Clone)]
pub struct Object(SyntaxNode);
impl Object {
  pub fn fields(&self) -> impl Iterator<Item = (String, Option<Expr>)> + '_ {
    self
      .0
      .children()
      .filter(|c| c.kind() == SyntaxKind::ObjectField)
      .map(|c| {
        let key = self.key(&c);
        let value = self.value(&c);
        (key, value)
      })
  }

  fn key(&self, object_field_node: &SyntaxNode) -> String {
    object_field_node
      .children()
      .find(|c| c.kind() == SyntaxKind::ObjectFieldKey)
      .unwrap()
      .first_token()
      .unwrap()
      .text()
      .to_string()
  }

  fn value(&self, object_field_node: &SyntaxNode) -> Option<Expr> {
    object_field_node
      .children()
      .find(|c| c.kind() == SyntaxKind::ObjectFieldValue)
      .and_then(|c| c.first_child())
      .and_then(Expr::cast)
  }
}

/// ObjectFieldAccessExpression@0..3
///   VariableReference@0..1
///     Identifier@0..1 "x"
///    Period@1..2 "."
///    ObjectFieldKey@2..3
///      Identifier@2..3 "y"
#[derive(Debug, Clone)]
pub struct ObjectFieldAccess(SyntaxNode);
impl ObjectFieldAccess {
  pub fn object(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }

  pub fn field(&self) -> String {
    self
      .0
      .children()
      .find(|c| c.kind() == SyntaxKind::ObjectFieldKey)
      .unwrap()
      .first_token()
      .unwrap()
      .text()
      .to_string()
  }
}
