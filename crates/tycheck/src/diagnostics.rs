use diagnostics::{emit_human_string, Diagnostic, DiagnosticBundle, HumanEmitConfig, TextRange};

/// Type checker diagnostics (shared model with CLI / LSP).
#[derive(Debug, Default)]
pub struct Diagnostics {
  pub bundle: DiagnosticBundle,
}

impl Diagnostics {
  pub fn has_errors(&self) -> bool {
    self.bundle.has_errors()
  }

  /// Stable error code + message + primary span.
  pub fn push_error(&mut self, code: &str, message: String, span: TextRange) {
    self.bundle.push(Diagnostic::error(code, message, span));
  }

  pub fn push(&mut self, diagnostic: Diagnostic) {
    self.bundle.push(diagnostic);
  }

  /// All diagnostics with severity [`diagnostics::Severity::Error`].
  pub fn errors(&self) -> impl Iterator<Item = &Diagnostic> {
    self.bundle.errors()
  }

  pub fn items(&self) -> &[Diagnostic] {
    &self.bundle.items
  }

  pub fn print_errors(&self) {
    for d in &self.bundle.items {
      println!("{}", d.message);
    }
  }

  /// Print errors with rustc-style source snippets when `source` is non-empty.
  pub fn print_errors_with_source(&self, source: &str, file_label: &str) {
    if source.is_empty() {
      self.print_errors();
      return;
    }
    let cfg = HumanEmitConfig {
      colors: std::env::var_os("NO_COLOR").is_none(),
      file_label: file_label.to_string(),
    };
    let out = emit_human_string(source, &self.bundle, &cfg);
    eprint!("{}", out);
  }
}
