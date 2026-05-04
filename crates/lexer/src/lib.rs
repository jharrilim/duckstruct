pub mod token;

use std::ops::Range;

use logos::Logos;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Lexer<'a> {
  inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      inner: TokenKind::lexer(input),
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let kind = self.inner.next()?;
    let text = self.inner.slice();
    let line = self.inner.extras.line_heads.len();

    // Absolute byte offsets in the full source. (Previously ranges were line-relative, which
    // broke multi-line diagnostics and any consumer treating `TextRange` as file offsets.)
    let range = {
      let Range { start, end } = self.inner.span();
      let start = TextSize::try_from(start).unwrap();
      let end = TextSize::try_from(end).unwrap();
      TextRange::new(start, end)
    };

    Some(Self::Item {
      kind,
      text,
      range,
      line,
    })
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
  pub kind: TokenKind,
  pub text: &'a str,
  /// Byte range in the full source (`TextSize` offsets from the start of the file).
  pub range: TextRange,
  /// 0-based line index for this token (handy for messages; spans use [`Self::range`]).
  pub line: usize,
}
