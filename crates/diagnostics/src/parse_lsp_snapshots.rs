#[cfg(test)]
mod tests {
  use text_size::TextSize;

  use crate::{
    bundle_from_parse_errors, bundle_to_lsp_diagnostics, emit_human_string, Diagnostic,
    DiagnosticBundle, HumanEmitConfig, TextRange,
  };
  use parser::parse;

  fn snapshot_lsp_json(src: &str, bundle: &DiagnosticBundle) -> String {
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", bundle);
    serde_json::to_string_pretty(&lsp).expect("serialize LSP diagnostics")
  }

  fn snapshot_human(src: &str, bundle: &DiagnosticBundle) -> String {
    let cfg = HumanEmitConfig {
      colors: false,
      file_label: "test.ds".into(),
    };
    emit_human_string(src, bundle, &cfg)
  }

  /// Primary span on line 2 (0-based line index 1), columns derived from real UTF-8 offsets.
  #[test]
  fn synthetic_primary_on_second_line_lsp() {
    let src = "let a = 123;\nlet b = wobble;\n";
    let wobble_off = src.find("wobble").expect("wobble");
    let span = TextRange::new(
      TextSize::from(wobble_off as u32),
      TextSize::from((wobble_off + "wobble".len()) as u32),
    );
    let bundle = DiagnosticBundle {
      items: vec![Diagnostic::error(
        "test::synthetic",
        "unknown name `wobble`",
        span,
      )],
    };
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", &bundle);
    assert_eq!(lsp[0].range.start.line, 1);
    assert_eq!(lsp[0].range.start.character, 8);
    insta::assert_snapshot!(snapshot_lsp_json(src, &bundle));
  }

  #[test]
  fn synthetic_primary_on_second_line_human() {
    let src = "let a = 123;\nlet b = wobble;\n";
    let wobble_off = src.find("wobble").expect("wobble");
    let span = TextRange::new(
      TextSize::from(wobble_off as u32),
      TextSize::from((wobble_off + "wobble".len()) as u32),
    );
    let bundle = DiagnosticBundle {
      items: vec![Diagnostic::error(
        "test::synthetic",
        "unknown name `wobble`",
        span,
      )],
    };
    insta::assert_snapshot!(snapshot_human(src, &bundle));
  }

  /// BMP character on line 2: UTF-16 column must count code units, not UTF-8 bytes.
  #[test]
  fn synthetic_unicode_on_second_line_lsp() {
    let src = "let a = 1;\nlet b = é;\n";
    let é_off = src.find('é').expect("é");
    let span = TextRange::new(
      TextSize::from(é_off as u32),
      TextSize::from((é_off + 'é'.len_utf8()) as u32),
    );
    let bundle = DiagnosticBundle {
      items: vec![Diagnostic::error(
        "test::synthetic",
        "type mismatch",
        span,
      )],
    };
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", &bundle);
    assert_eq!(lsp[0].range.start.line, 1);
    assert_eq!(lsp[0].range.start.character, 8);
    insta::assert_snapshot!(snapshot_lsp_json(src, &bundle));
  }

  /// Regression: leading `//` line + blank line + `let` + bad `x + 1)()` — primary spans must not map to line 0.
  #[test]
  fn parse_error_after_comment_snippet_lsp_line() {
    let src = include_str!("../../../dscode/client/testFixture/parse-span.ds");
    let bundle = bundle_from_parse_errors(&parse(src).errors);
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", &bundle);
    assert!(
      lsp[0].range.start.line >= 2,
      "expected first parse error on `x + 1)()` line (LSP line >= 2), got line {} col {} msg={}",
      lsp[0].range.start.line,
      lsp[0].range.start.character,
      lsp[0].message
    );
  }

  /// Parser test idiom: incomplete first `let`, recovery at second `let`.
  #[test]
  fn parse_let_x_newline_let_y_lsp_snapshot() {
    let src = "let x =\nlet y = 1";
    let bundle = bundle_from_parse_errors(&parse(src).errors);
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", &bundle);
    assert_eq!(lsp[0].range.start.line, 1);
    assert_eq!(lsp[0].range.start.character, 0);
    assert!(lsp[0].message.contains("let"));
    insta::assert_snapshot!(snapshot_lsp_json(src, &bundle));
  }

  #[test]
  fn parse_let_x_newline_let_y_human_snapshot() {
    let src = "let x =\nlet y = 1";
    let bundle = bundle_from_parse_errors(&parse(src).errors);
    insta::assert_snapshot!(snapshot_human(src, &bundle));
  }

  /// User example: junk after `=` on the second `let`; first error must point at `)` on `b`'s line.
  #[test]
  fn parse_bad_tokens_second_statement_lsp_snapshot() {
    let src = "let a = 123;\nlet b = )'(;\n";
    let bundle = bundle_from_parse_errors(&parse(src).errors);
    let lsp = bundle_to_lsp_diagnostics(src, "file:///test.ds", &bundle);
    assert!(!lsp.is_empty());
    assert_eq!(lsp[0].range.start.line, 1, "first diagnostic on second physical line");
    assert_eq!(lsp[0].range.start.character, 8, "column at `)` after `let b = `");
    assert!(lsp[0].message.contains(')'), "{}", lsp[0].message);
    insta::assert_snapshot!(snapshot_lsp_json(src, &bundle));
  }

  #[test]
  fn parse_bad_tokens_second_statement_human_snapshot() {
    let src = "let a = 123;\nlet b = )'(;\n";
    let bundle = bundle_from_parse_errors(&parse(src).errors);
    insta::assert_snapshot!(snapshot_human(src, &bundle));
  }
}
