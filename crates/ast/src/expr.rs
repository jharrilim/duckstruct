use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

#[derive(Debug)]
pub enum Expr {
  BinaryExpr(BinaryExpr),
  NumberLit(Number),
  StringLit(Str),
  ParenExpr(ParenExpr),
  UnaryExpr(UnaryExpr),
  VariableRef(VariableRef),
  Function(Function),
  FunctionCall(FunctionCall),
}

impl Expr {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::InfixExpression => Self::BinaryExpr(BinaryExpr(node)),
      SyntaxKind::Number => Self::NumberLit(Number(node)),
      SyntaxKind::String => Self::StringLit(Str(node)),
      SyntaxKind::ParenExpression => Self::ParenExpr(ParenExpr(node)),
      SyntaxKind::PrefixExpression => Self::UnaryExpr(UnaryExpr(node)),
      SyntaxKind::VariableReference => Self::VariableRef(VariableRef(node)),
      SyntaxKind::AnonymousFunction => Self::Function(Function(node)),
      SyntaxKind::AnonymousFunctionExpression => Self::Function(Function(node)),
      SyntaxKind::NamedFunction => Self::Function(Function(node)),
      SyntaxKind::NamedFunctionExpression => Self::Function(Function(node)),
      SyntaxKind::FunctionCallExpression => Self::FunctionCall(FunctionCall(node)),
      _ => return None,
    };

    Some(result)
  }
}

#[derive(Debug)]
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
        matches!(
          token.kind(),
          SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Asterisk | SyntaxKind::ForwardSlash,
        )
      })
  }
}

#[derive(Debug)]
pub struct Number(SyntaxNode);
impl Number {
  pub fn parse(&self) -> f64 {
    self.0.first_token().unwrap().text().parse().unwrap()
  }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ParenExpr(SyntaxNode);
impl ParenExpr {
  pub fn expr(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}

#[derive(Debug)]
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
      .find(|token| token.kind() == SyntaxKind::Minus)
  }
}

#[derive(Debug)]
pub struct VariableRef(SyntaxNode);
impl VariableRef {
  pub fn name(&self) -> String {
    self.0.first_token().unwrap().text().to_string()
  }
}

#[derive(Debug)]
pub struct Function(SyntaxNode);
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
      .filter_map(SyntaxElement::into_token)
      .filter(|token| token.kind() == SyntaxKind::Identifier)
  }

  pub fn body(&self) -> Option<Expr> {
    self.0.children().find_map(Expr::cast)
  }
}

#[derive(Debug)]
pub struct FunctionCall(SyntaxNode);
impl FunctionCall {
  pub fn name(&self) -> Option<SyntaxToken> {
    let n = self
      .0
      .children_with_tokens()
      .find(|node_or_token| node_or_token.kind() == SyntaxKind::VariableReference)
      .unwrap()
      .into_node()
      .unwrap()
      .children_with_tokens()
      .filter_map(SyntaxElement::into_token)
      .find(|token| token.kind() == SyntaxKind::Identifier);
    n
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
