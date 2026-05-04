use serde::Serialize;

use crate::{byte_offset_to_line_character_utf16, DiagnosticBundle, TextRange};

#[derive(Serialize)]
pub struct LspDiagnostic {
  pub range: LspRange,
  pub message: String,
  pub severity: u8,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub code: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub related_information: Option<Vec<RelatedInformation>>,
}

#[derive(Serialize)]
pub struct LspRange {
  pub start: LspPosition,
  pub end: LspPosition,
}

#[derive(Serialize)]
pub struct LspPosition {
  pub line: u32,
  pub character: u32,
}

#[derive(Serialize)]
pub struct RelatedInformation {
  pub location: RelatedLocation,
  pub message: String,
}

#[derive(Serialize)]
pub struct RelatedLocation {
  pub uri: String,
  pub range: LspRange,
}

fn range_to_lsp(source: &str, span: TextRange) -> LspRange {
  let start: usize = span.start().into();
  let end: usize = span.end().into();
  let (sl, sc) = byte_offset_to_line_character_utf16(source, start);
  let (el, ec) = byte_offset_to_line_character_utf16(source, end);
  LspRange {
    start: LspPosition {
      line: sl,
      character: sc,
    },
    end: LspPosition {
      line: el,
      character: ec,
    },
  }
}

/// Convert diagnostics to LSP JSON structs. `uri` is used for `relatedInformation[].location.uri`.
pub fn bundle_to_lsp_diagnostics(source: &str, uri: &str, bundle: &DiagnosticBundle) -> Vec<LspDiagnostic> {
  bundle.items.iter().map(|d| diagnostic_to_lsp(source, uri, d)).collect()
}

pub fn diagnostic_to_lsp(source: &str, uri: &str, d: &crate::Diagnostic) -> LspDiagnostic {
  let related_information = if d.labels.is_empty() {
    None
  } else {
    Some(
      d
        .labels
        .iter()
        .map(|label| RelatedInformation {
          location: RelatedLocation {
            uri: uri.to_string(),
            range: range_to_lsp(source, label.span),
          },
          message: label.message.clone(),
        })
        .collect(),
    )
  };

  LspDiagnostic {
    range: range_to_lsp(source, d.span),
    message: d.message.clone(),
    severity: d.severity.lsp_u8(),
    code: if d.code.is_empty() {
      None
    } else {
      Some(d.code.clone())
    },
    related_information,
  }
}

/// Serialize bundle as JSON array (for `check --json`).
pub fn bundle_to_lsp_json(source: &str, uri: &str, bundle: &DiagnosticBundle) -> String {
  let v = bundle_to_lsp_diagnostics(source, uri, bundle);
  serde_json::to_string(&v).unwrap_or_else(|_| "[]".to_string())
}
