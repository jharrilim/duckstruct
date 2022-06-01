use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use std::hash::Hash;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone, Copy, FromPrimitive, ToPrimitive, PartialOrd, Ord, Eq, Hash)]
pub enum TokenKind {
  #[token("f")]
  Function,

  #[token("let")]
  Let,

  #[token("class")]
  Class,

  #[token("{")]
  LeftBrace,

  #[token("}")]
  RightBrace,

  #[token("(")]
  LeftParenthesis,

  #[token(")")]
  RightParenthesis,

  #[token("[")]
  LeftBracket,

  #[token("]")]
  RightBracket,

  #[token(".")]
  Period,

  #[token(",")]
  Comma,

  #[regex(r"[0-9]+")]
  Number,

  #[token("+")]
  Plus,

  #[token("-")]
  Minus,

  #[token("*")]
  Asterisk,

  #[token("/")]
  ForwardSlash,

  #[token("=")]
  Equals,

  #[regex(r#""([^\\"]|\.)*""#)]
  String,

  #[token(":")]
  Colon,

  #[token(";")]
  Semicolon,

  #[regex(r"[a-zA-Z_][a-zA-Z\d_]*")]
  Identifier,

  #[regex(r"[\s]+")]
  Whitespace,

  #[regex("//.*")]
  Comment,

  #[error]
  Error,
}

impl TokenKind {
  pub fn is_trivia(self) -> bool {
      matches!(self, Self::Whitespace | Self::Comment)
  }
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      f.write_str(match self {
          Self::Whitespace => "whitespace",
          Self::Function => "'f'",
          Self::Class => "'class'",
          Self::Let => "'let'",
          Self::Identifier => "identifier",
          Self::Period => "'.'",
          Self::Comma => "','",
          Self::Colon => "':'",
          Self::Semicolon => "';'",
          Self::Number => "number",
          Self::Plus => "'+'",
          Self::Minus => "'-'",
          Self::Asterisk => "'*'",
          Self::ForwardSlash => "'/'",
          Self::Equals => "'='",
          Self::LeftParenthesis => "'('",
          Self::RightParenthesis => "')'",
          Self::LeftBrace => "'{'",
          Self::RightBrace => "'}'",
          Self::LeftBracket => "'['",
          Self::RightBracket => "']'",
          Self::String => "string",
          Self::Comment => "comment",
          Self::Error => "an unrecognized token",
      })
  }
}


#[cfg(test)]
mod tests {
  use super::TokenKind;
  use crate::Lexer;
  use crate::Token;

  fn check(input: &str, kind: TokenKind) {
    let mut lexer = Lexer::new(input);
    assert_eq!(lexer.next(), Some(Token { kind, text: input }));
  }

  #[test]
  fn lex_spaces_and_newlines() {
    check("  \n ", TokenKind::Whitespace);
  }

  #[test]
  fn lex_f_keyword() {
    check("f", TokenKind::Function);
  }

  #[test]
  fn lex_let_keyword() {
    check("let", TokenKind::Let);
  }

  #[test]
  fn lex_alphabetic_identifier() {
    check("abcd", TokenKind::Identifier);
  }

  #[test]
  fn lex_alphanumeric_identifier() {
    check("ab123cde456", TokenKind::Identifier);
  }

  #[test]
  fn lex_mixed_case_identifier() {
    check("ABCdef", TokenKind::Identifier);
  }

  #[test]
  fn lex_single_char_identifier() {
    check("x", TokenKind::Identifier);
  }

  #[test]
  fn lex_number() {
    check("123456", TokenKind::Number);
  }

  #[test]
  fn lex_plus() {
    check("+", TokenKind::Plus);
  }

  #[test]
  fn lex_minus() {
    check("-", TokenKind::Minus);
  }

  #[test]
  fn lex_star() {
    check("*", TokenKind::Asterisk);
  }

  #[test]
  fn lex_slash() {
    check("/", TokenKind::ForwardSlash);
  }

  #[test]
  fn lex_equals() {
    check("=", TokenKind::Equals);
  }

  #[test]
  fn lex_left_parenthesis() {
    check("(", TokenKind::LeftParenthesis);
  }

  #[test]
  fn lex_right_parenthesis() {
    check(")", TokenKind::RightParenthesis);
  }

  #[test]
  fn lex_left_brace() {
    check("{", TokenKind::LeftBrace);
  }

  #[test]
  fn lex_right_brace() {
    check("}", TokenKind::RightBrace);
  }

  #[test]
  fn lex_comment() {
    check("// im a comment", TokenKind::Comment);
  }
}
