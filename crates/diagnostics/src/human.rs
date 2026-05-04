use std::io::Write;

use ariadne::{Config, Label, Report, ReportKind, Source};

use crate::{DiagnosticBundle, Severity};

pub struct HumanEmitConfig {
  /// When false, disable ANSI colors (also respect `NO_COLOR` in [`HumanEmitConfig::from_env`]).
  pub colors: bool,
  /// Label shown in the margin (file path or `"<stdin>"`).
  pub file_label: String,
}

impl HumanEmitConfig {
  pub fn from_env(file_label: impl Into<String>) -> Self {
    Self {
      colors: std::env::var_os("NO_COLOR").is_none(),
      file_label: file_label.into(),
    }
  }
}

impl Default for HumanEmitConfig {
  fn default() -> Self {
    Self::from_env("<source>")
  }
}

fn report_kind(sev: Severity) -> ReportKind<'static> {
  match sev {
    Severity::Error => ReportKind::Error,
    Severity::Warning => ReportKind::Warning,
    Severity::Information => ReportKind::Advice,
    Severity::Hint => ReportKind::Advice,
  }
}

/// Emit all diagnostics to `writer` using rustc-style snippets (via `ariadne`).
pub fn emit_human(
  source: &str,
  bundle: &DiagnosticBundle,
  writer: &mut dyn Write,
  config: &HumanEmitConfig,
) -> std::io::Result<()> {
  let fid = config.file_label.as_str();
  let mut cfg = Config::default();
  if !config.colors {
    cfg = cfg.with_color(false);
  }

  for d in &bundle.items {
    let start: usize = d.span.start().into();
    let end: usize = d.span.end().into();
    let primary = start..end;

    let mut rb = Report::build(report_kind(d.severity), (fid, primary.clone()))
      .with_config(cfg)
      .with_message(&d.message);

    if !d.code.is_empty() {
      rb = rb.with_code(&d.code);
    }

    rb = rb.with_label(Label::new((fid, primary)).with_message(&d.message));

    for (i, l) in d.labels.iter().enumerate() {
      let ls: usize = l.span.start().into();
      let le: usize = l.span.end().into();
      rb = rb.with_label(
        Label::new((fid, ls..le))
          .with_message(&l.message)
          .with_order(i as i32 + 1),
      );
    }

    for n in &d.notes {
      rb = rb.with_note(n);
    }

    let report = rb.finish();
    report.write((fid, Source::from(source)), &mut *writer)?;
  }
  Ok(())
}

pub fn emit_human_string(source: &str, bundle: &DiagnosticBundle, config: &HumanEmitConfig) -> String {
  let mut buf = Vec::new();
  if emit_human(source, bundle, &mut buf, config).is_ok() {
    String::from_utf8_lossy(&buf).into_owned()
  } else {
    String::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{Diagnostic, TextRange};
  use text_size::TextSize;

  #[test]
  fn human_output_snapshot() {
    let source = "let x = 1;\nlet y = x();\n";
    let span = TextRange::new(TextSize::from(22), TextSize::from(23));
    let bundle = DiagnosticBundle {
      items: vec![Diagnostic::error(
        "type::cannot_call",
        "cannot call this expression",
        span,
      )],
    };
    let cfg = HumanEmitConfig {
      colors: false,
      file_label: "test.ds".into(),
    };
    let out = emit_human_string(source, &bundle, &cfg);
    insta::assert_snapshot!(out);
  }
}
