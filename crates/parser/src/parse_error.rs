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
  pub(super) range: TextRange,
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.expected {
      Expectations::Expression => write!(
        f,
        "error at {}..{}: expected expression, but found {}",
        u32::from(self.range.start()),
        u32::from(self.range.end()),
        self.found.unwrap()
      ),
      Expectations::Tokens(tokens) => {
        write!(
          f,
          "error at {}..{}: expected ",
          u32::from(self.range.start()),
          u32::from(self.range.end()),
        )?;

        let num_expected = tokens.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in tokens.iter().enumerate() {
          if is_first(idx) {
            write!(f, "{}", expected_kind)?;
          } else if is_last(idx) {
            write!(f, " or {}", expected_kind)?;
          } else {
            write!(f, ", {}", expected_kind)?;
          }
        }

        if let Some(found) = self.found {
          write!(f, ", but found {}", found)?;
        }

        Ok(())
      }
    }
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
    };

    assert_eq!(format!("{}", error), output);
  }

  #[test]
  fn one_expected_did_find() {
    check(
      vec![TokenKind::Equals],
      Some(TokenKind::Identifier),
      10..20,
      "error at 10..20: expected '=', but found identifier",
    );
  }

  #[test]
  fn one_expected_did_not_find() {
    check(
      vec![TokenKind::RightParenthesis],
      None,
      5..6,
      "error at 5..6: expected ')'",
    );
  }

  #[test]
  fn two_expected_did_find() {
    check(
      vec![TokenKind::Plus, TokenKind::Minus],
      Some(TokenKind::Equals),
      0..1,
      "error at 0..1: expected '+' or '-', but found '='",
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
      "error at 100..105: expected number, identifier, '-' or '(', but found 'let'",
    );
  }
}
