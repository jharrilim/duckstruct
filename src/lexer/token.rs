use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Logos, Debug, PartialEq, Clone, Copy, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
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

  #[token(".")]
  Period,

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

  #[token("\"")]
  DoubleQuote,

  #[token("'")]
  SingleQuote,

  #[token(";")]
  Semicolon,

  #[token(":")]
  Colon,

  #[regex(r"[a-zA-Z_][a-zA-Z\d_]*")]
  Identifier,

  #[regex(r"[\s]+")]
  Whitespace,

  #[regex("//.*")]
  Comment,

  #[error]
  Error,

  BinaryExpression,
  PrefixExpression,

  Root,
}

#[cfg(test)]
mod tests {
  use crate::lexer::Lexer;
  use crate::lexer::token::SyntaxKind;
  use crate::lexer::Lexeme;

  fn check(input: &str, kind: SyntaxKind) {
      let mut lexer = Lexer::new(input);
      assert_eq!(lexer.next(), Some(Lexeme { kind, text: input }));
  }

  #[test]
  fn lex_spaces_and_newlines() {
      check("  \n ", SyntaxKind::Whitespace);
  }

  #[test]
  fn lex_f_keyword() {
      check("f", SyntaxKind::Function);
  }

  #[test]
  fn lex_let_keyword() {
      check("let", SyntaxKind::Let);
  }

  #[test]
  fn lex_alphabetic_identifier() {
      check("abcd", SyntaxKind::Identifier);
  }

  #[test]
  fn lex_alphanumeric_identifier() {
      check("ab123cde456", SyntaxKind::Identifier);
  }

  #[test]
  fn lex_mixed_case_identifier() {
      check("ABCdef", SyntaxKind::Identifier);
  }

  #[test]
  fn lex_single_char_identifier() {
      check("x", SyntaxKind::Identifier);
  }

  #[test]
  fn lex_number() {
      check("123456", SyntaxKind::Number);
  }

  #[test]
  fn lex_plus() {
      check("+", SyntaxKind::Plus);
  }

  #[test]
  fn lex_minus() {
      check("-", SyntaxKind::Minus);
  }

  #[test]
  fn lex_star() {
      check("*", SyntaxKind::Asterisk);
  }

  #[test]
  fn lex_slash() {
      check("/", SyntaxKind::ForwardSlash);
  }

  #[test]
  fn lex_equals() {
      check("=", SyntaxKind::Equals);
  }

  #[test]
  fn lex_left_parenthesis() {
      check("(", SyntaxKind::LeftParenthesis);
  }

  #[test]
  fn lex_right_parenthesis() {
      check(")", SyntaxKind::RightParenthesis);
  }

  #[test]
  fn lex_left_brace() {
      check("{", SyntaxKind::LeftBrace);
  }

  #[test]
  fn lex_right_brace() {
      check("}", SyntaxKind::RightBrace);
  }

  #[test]
  fn lex_comment() {
      check("// im a comment", SyntaxKind::Comment);
  }
}
