pub mod token;

use logos::Logos;
use token::SyntaxKind;

pub(crate) struct Lexer<'a> {
  inner: logos::Lexer<'a, SyntaxKind>,
}

impl<'a> Lexer<'a> {
  pub(crate) fn new(input: &'a str) -> Self {
    Self {
      inner: SyntaxKind::lexer(input),
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let kind = self.inner.next()?;
    let text = self.inner.slice();

    Some(Self::Item { kind, text })
  }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'a> {
  pub(crate) kind: SyntaxKind,
  pub(crate) text: &'a str,
}
