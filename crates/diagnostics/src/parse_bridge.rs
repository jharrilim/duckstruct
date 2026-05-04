use parser::ParseError;

use crate::{Diagnostic, Severity};

/// Build a [`Diagnostic`] from a parser [`ParseError`] (primary span is `err.range`).
pub fn diagnostic_from_parse_error(err: &ParseError) -> Diagnostic {
  Diagnostic {
    code: "parse::syntax".to_string(),
    severity: Severity::Error,
    message: err.message_body(),
    span: err.range,
    labels: Vec::new(),
    notes: Vec::new(),
  }
}

/// Convert all parse errors to a bundle.
pub fn bundle_from_parse_errors(errors: &[ParseError]) -> crate::DiagnosticBundle {
  let mut b = crate::DiagnosticBundle::default();
  for e in errors {
    b.push(diagnostic_from_parse_error(e));
  }
  b
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn bundle_from_parse_smoke() {
    let parsed = parser::parse("let x =");
    assert!(!parsed.errors.is_empty());
    let b = bundle_from_parse_errors(&parsed.errors);
    assert_eq!(b.items.len(), parsed.errors.len());
    assert!(!b.items[0].message.is_empty());
    assert_eq!(b.items[0].code, "parse::syntax");
  }

  #[test]
  fn bundle_to_lsp_json_from_incomplete_let() {
    let src = "let x =\n";
    let parsed = parser::parse(src);
    assert!(!parsed.errors.is_empty());
    let b = bundle_from_parse_errors(&parsed.errors);
    let j = crate::bundle_to_lsp_json(src, "file:///tmp/x.ds", &b);
    assert!(j.starts_with('['), "{}", j);
  }
}
