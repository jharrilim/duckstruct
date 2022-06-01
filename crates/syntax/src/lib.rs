use lexer::token::TokenKind;
use rowan::Language;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, FromPrimitive, ToPrimitive, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    Whitespace,
    Function,
    Class,
    Let,
    Identifier,
    Number,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Equals,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    String,
    Comma,
    Colon,
    Semicolon,
    Period,
    Comment,
    Error,
    Root,
    InfixExpression,
    Literal,
    ParenExpression,
    PrefixExpression,
    LetExpression,
    VariableReference,
    Assignment, // idk if i'll really need this, but for now it marks the lhs of a = expression
    Pattern,
    StructPattern,
    ArrayPattern,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::Function => Self::Function,
            TokenKind::Class => Self::Class,
            TokenKind::Let => Self::Let,
            TokenKind::Identifier => Self::Identifier,
            TokenKind::Number => Self::Number,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Asterisk => Self::Asterisk,
            TokenKind::ForwardSlash => Self::ForwardSlash,
            TokenKind::Equals => Self::Equals,
            TokenKind::LeftParenthesis => Self::LeftParenthesis,
            TokenKind::RightParenthesis => Self::RightParenthesis,
            TokenKind::LeftBrace => Self::LeftBrace,
            TokenKind::RightBrace => Self::RightBrace,
            TokenKind::LeftBracket => Self::LeftBracket,
            TokenKind::RightBracket => Self::RightBracket,
            TokenKind::String => Self::String,
            TokenKind::Comma => Self::Comma,
            TokenKind::Colon => Self::Colon,
            TokenKind::Semicolon => Self::Semicolon,
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
