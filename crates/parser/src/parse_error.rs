use lexer::token::TokenKind;
use std::fmt;
use text_size::TextRange;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expectations {
  Expression,
  Tokens(Vec<TokenKind>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError {
  pub(super) expected: Expectations,
  pub(super) found: Option<TokenKind>,
  /// Byte range in the source (for LSP/tooling).
  pub range: TextRange,
  /// 0-based line number.
  pub line: usize,
}

impl ParseError {
  /// Human-readable error text without embedding source offsets (use [`Self::range`] for spans).
  pub fn message_body(&self) -> String {
    match &self.expected {
      Expectations::Expression => {
        let found = self
          .found
          .map(|k| format!("{}", k))
          .unwrap_or_else(|| "end of input".to_string());
        format!("expected expression, found {}", found)
      }
      Expectations::Tokens(tokens) => {
        let num_expected = tokens.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        let mut s = String::from("expected ");
        for (idx, expected_kind) in tokens.iter().enumerate() {
          if is_first(idx) {
            s.push_str(&format!("{}", expected_kind));
          } else if is_last(idx) {
            s.push_str(&format!(" or {}", expected_kind));
          } else {
            s.push_str(&format!(", {}", expected_kind));
          }
        }

        if let Some(found) = self.found {
          s.push_str(&format!(", found {}", found));
        }
        s
      }
    }
  }
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "parse error on line {}: {}",
      self.line + 1,
      self.message_body()
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::ops::Range as StdRange;

  fn check(expected: Vec<TokenKind>, found: Option<TokenKind>, range: StdRange<u32>, output: &str) {
    let error = ParseError {
      expected: Expectations::Tokens(expected),
      found,
      range: {
        let start = range.start.into();
        let end = range.end.into();
        TextRange::new(start, end)
      },
      line: 0,
    };

    assert_eq!(error.message_body(), output);
  }

  #[test]
  fn one_expected_did_find() {
    check(
      vec![TokenKind::Equals],
      Some(TokenKind::Identifier),
      10..20,
      "expected '=', found identifier",
    );
  }

  #[test]
  fn one_expected_did_not_find() {
    check(
      vec![TokenKind::RightParenthesis],
      None,
      5..6,
      "expected ')'",
    );
  }

  #[test]
  fn two_expected_did_find() {
    check(
      vec![TokenKind::Plus, TokenKind::Minus],
      Some(TokenKind::Equals),
      0..1,
      "expected '+' or '-', found '='",
    );
  }

  #[test]
  fn multiple_expected_did_find() {
    check(
      vec![
        TokenKind::Number,
        TokenKind::Identifier,
        TokenKind::Minus,
        TokenKind::LeftParenthesis,
      ],
      Some(TokenKind::Let),
      100..105,
      "expected number, identifier, '-' or '(', found 'let'",
    );
  }
}
