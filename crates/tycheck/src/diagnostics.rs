use std::fmt::Display;

use syntax::TextRange;

/// Returns (line_1based, column_1based) for the start of the range, or None if out of bounds.
fn line_column(source: &str, range: TextRange) -> Option<(usize, usize)> {
  let start = range.start().into();
  if source.is_empty() || start > source.len() {
    return None;
  }
  let line = 1 + source.bytes().take(start).filter(|&b| b == b'\n').count();
  let line_start = source
    .bytes()
    .enumerate()
    .rev()
    .take(start)
    .find(|(_, b)| *b == b'\n')
    .map(|(i, _)| i + 1)
    .unwrap_or(0);
  let column = 1 + start - line_start;
  Some((line, column))
}

#[derive(Debug, Default)]
pub struct Diagnostics {
  pub errors: Vec<Error>,
}

impl Diagnostics {
  pub fn has_errors(&self) -> bool {
    !self.errors.is_empty()
  }

  pub fn print_errors(&self) {
    for error in &self.errors {
      println!("{}", error);
    }
  }

  /// Print errors with source location (line, column) when source is provided.
  /// If source is empty, only the message is printed (e.g. for tests).
  pub fn print_errors_with_source(&self, source: &str) {
    for line in self.format_errors_with_source(source) {
      println!("{}", line);
    }
  }

  /// Format errors with source location (line, column) when source is provided.
  /// Returns one string per error, for testing or custom display.
  pub fn format_errors_with_source(&self, source: &str) -> Vec<String> {
    self
      .errors
      .iter()
      .map(|error| {
        if let Some((line, col)) = line_column(source, error.span()) {
          format!("error at line {}, column {}: {}", line, col, error.message())
        } else {
          format!("error: {}", error.message())
        }
      })
      .collect()
  }

  pub fn push_error(&mut self, message: String, span: TextRange) {
    self.errors.push(Error { message, span });
  }
}

#[derive(Debug, Default)]
pub struct Error {
  message: String,
  span: TextRange,
}

impl Error {
  pub fn span(&self) -> TextRange {
    self.span
  }

  pub fn message(&self) -> &str {
    &self.message
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.message)
  }
}
