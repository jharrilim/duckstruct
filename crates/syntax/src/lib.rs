use lexer::token::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;

#[derive(Debug, Copy, Clone, PartialEq, FromPrimitive, ToPrimitive, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
  // Literals
  Whitespace,
  Function, // the keyword
  Boolean,  // true or false
  Class,
  Let,
  If,
  Else,
  For,
  In,
  Where,
  While,
  Identifier,
  Number,
  Plus,
  Minus,
  Asterisk,
  ForwardSlash,
  DoubleEquals,
  LessThan,
  LessThanOrEqual,
  GreaterThan,
  GreaterThanOrEqual,
  And,
  Or,
  Bang,
  NotEquals,
  Equals,
  LeftParenthesis,
  RightParenthesis,
  DoubleLeftBrace,
  DoubleRightBrace,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,
  String,
  Comma,
  Colon,
  Semicolon,
  Pipe,
  Period,
  Comment,
  Error,
  Root,

  // Statements
  LetStatement,

  // Expressions
  InfixExpression,
  ParenExpression,
  UnaryExpression,
  ConditionalExpression,
  ConditionalPredicate,
  IfCondition,
  ElseCondition,
  VariableReference,
  AnonymousFunction,
  AnonymousFunctionExpression,
  NamedFunction,
  NamedFunctionExpression,
  FunctionCallExpression,
  BlockExpression,
  ArrayExpression,
  ObjectExpression,
  // Objects
  ObjectField,
  ObjectFieldKey,
  ObjectFieldValue,
  ObjectFieldAccessExpression,
  // Loops
  ForExpression,
  ForPattern,
  ForInExpression,
  ForWhereCondition,
  ForBody,
  ForPipePattern,
  // Patterns
  Pattern,
  StructPattern,
  ArrayPattern,
  RestPattern,
  ArgumentList,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
  fn from(kind: SyntaxKind) -> Self {
    Self(kind as u16)
  }
}

impl From<TokenKind> for SyntaxKind {
  fn from(token_kind: TokenKind) -> Self {
    match token_kind {
      TokenKind::Whitespace => Self::Whitespace,
      TokenKind::Boolean => Self::Boolean,
      TokenKind::Function => Self::Function,
      TokenKind::Class => Self::Class,
      TokenKind::Let => Self::Let,
      TokenKind::If => Self::If,
      TokenKind::Else => Self::Else,
      TokenKind::For => Self::For,
      TokenKind::In => Self::In,
      TokenKind::Where => Self::Where,
      TokenKind::While => Self::While,
      TokenKind::Identifier => Self::Identifier,
      TokenKind::Number => Self::Number,
      TokenKind::Plus => Self::Plus,
      TokenKind::Minus => Self::Minus,
      TokenKind::Asterisk => Self::Asterisk,
      TokenKind::ForwardSlash => Self::ForwardSlash,
      TokenKind::DoubleEquals => Self::DoubleEquals,
      TokenKind::LessThan => Self::LessThan,
      TokenKind::LessThanOrEqual => Self::LessThanOrEqual,
      TokenKind::GreaterThan => Self::GreaterThan,
      TokenKind::GreaterThanOrEqual => Self::GreaterThanOrEqual,
      TokenKind::And => Self::And,
      TokenKind::Or => Self::Or,
      TokenKind::NotEquals => Self::NotEquals,
      TokenKind::Bang => Self::Bang,
      TokenKind::Equals => Self::Equals,
      TokenKind::LeftParenthesis => Self::LeftParenthesis,
      TokenKind::RightParenthesis => Self::RightParenthesis,
      TokenKind::DoubleLeftBrace => Self::DoubleLeftBrace,
      TokenKind::DoubleRightBrace => Self::DoubleRightBrace,
      TokenKind::LeftBrace => Self::LeftBrace,
      TokenKind::RightBrace => Self::RightBrace,
      TokenKind::LeftBracket => Self::LeftBracket,
      TokenKind::RightBracket => Self::RightBracket,
      TokenKind::String => Self::String,
      TokenKind::Comma => Self::Comma,
      TokenKind::Colon => Self::Colon,
      TokenKind::Semicolon => Self::Semicolon,
      TokenKind::Pipe => Self::Pipe,
      TokenKind::Period => Self::Period,
      TokenKind::Comment => Self::Comment,
      TokenKind::Error => Self::Error,
    }
  }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Duckstruct {}

impl Language for Duckstruct {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    Self::Kind::from_u16(raw.0).unwrap()
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.to_u16().unwrap())
  }
}

pub type SyntaxNode = rowan::SyntaxNode<Duckstruct>;
pub type SyntaxElement = rowan::SyntaxElement<Duckstruct>;
pub type SyntaxToken = rowan::SyntaxToken<Duckstruct>;
