//! Shared diagnostics for parse, typecheck, and tooling (LSP JSON + human-readable reports).

mod human;
mod lsp;
mod parse_bridge;
mod position;

#[cfg(test)]
mod parse_lsp_snapshots;

pub use human::{emit_human, emit_human_string, HumanEmitConfig};
pub use lsp::{
  bundle_to_lsp_diagnostics, bundle_to_lsp_json, diagnostic_to_lsp, LspDiagnostic, LspPosition,
  LspRange, RelatedInformation,
};
pub use parse_bridge::{bundle_from_parse_errors, diagnostic_from_parse_error};
pub use position::byte_offset_to_line_character_utf16;

pub use syntax::TextRange;

/// Severity of a diagnostic (maps to LSP `DiagnosticSeverity`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
  Error = 1,
  Warning = 2,
  Information = 3,
  Hint = 4,
}

impl Severity {
  pub fn lsp_u8(self) -> u8 {
    self as u8
  }
}

/// One compiler diagnostic: stable code, primary span, optional secondary labels and notes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
  pub code: String,
  pub severity: Severity,
  pub message: String,
  pub span: TextRange,
  pub labels: Vec<SecondaryLabel>,
  pub notes: Vec<String>,
}

/// A secondary span with its own message (shown as related labels / LSP `relatedInformation`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SecondaryLabel {
  pub span: TextRange,
  pub message: String,
}

impl Diagnostic {
  pub fn error(code: impl Into<String>, message: impl Into<String>, span: TextRange) -> Self {
    Self {
      code: code.into(),
      severity: Severity::Error,
      message: message.into(),
      span,
      labels: Vec::new(),
      notes: Vec::new(),
    }
  }

  pub fn warning(code: impl Into<String>, message: impl Into<String>, span: TextRange) -> Self {
    Self {
      code: code.into(),
      severity: Severity::Warning,
      message: message.into(),
      span,
      labels: Vec::new(),
      notes: Vec::new(),
    }
  }

  pub fn with_label(mut self, span: TextRange, message: impl Into<String>) -> Self {
    self.labels.push(SecondaryLabel {
      span,
      message: message.into(),
    });
    self
  }

  pub fn with_note(mut self, note: impl Into<String>) -> Self {
    self.notes.push(note.into());
    self
  }

  pub fn message(&self) -> &str {
    &self.message
  }

  pub fn span(&self) -> TextRange {
    self.span
  }

  pub fn is_error(&self) -> bool {
    matches!(self.severity, Severity::Error)
  }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DiagnosticBundle {
  pub items: Vec<Diagnostic>,
}

impl DiagnosticBundle {
  pub fn push(&mut self, diagnostic: Diagnostic) {
    self.items.push(diagnostic);
  }

  pub fn extend_bundle(&mut self, other: DiagnosticBundle) {
    self.items.extend(other.items);
  }

  pub fn has_errors(&self) -> bool {
    self
      .items
      .iter()
      .any(|d| d.severity == Severity::Error)
  }

  pub fn is_empty(&self) -> bool {
    self.items.is_empty()
  }

  /// Diagnostics with severity `Error` (for tests and simple consumers).
  pub fn errors(&self) -> impl Iterator<Item = &Diagnostic> {
    self
      .items
      .iter()
      .filter(|d| d.severity == Severity::Error)
  }
}
