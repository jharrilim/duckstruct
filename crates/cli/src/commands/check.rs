//! `check` command: parse, typecheck, and report diagnostics as JSON (for LSP/editors).

use std::io::Read;
use std::path::PathBuf;

use ast::Root;
use diagnostics::{bundle_from_parse_errors, bundle_to_lsp_json};
use hir::lower;
use parser::parse;
use tycheck::TyCheck;

/// Run the check command. Reads from path or stdin, parses, typechecks when parse succeeds,
/// and prints LSP-style diagnostics as JSON.
///
/// When reading from stdin, set env **`DUCKSTRUCT_LSP_DOCUMENT_URI`** to the editor's `file://`
/// URI so JSON `related_information` locations match the open document (used by the VS Code
/// language server). When unset, stdin mode uses `file://<stdin>`.
pub fn run(path: Option<PathBuf>, json: bool) {
  if !json {
    eprintln!("check requires --json");
    std::process::exit(1);
  }
  let source = match path.as_ref() {
    Some(p) => std::fs::read_to_string(p).unwrap_or_else(|e| {
      eprintln!("{}", e);
      std::process::exit(1);
    }),
    None => {
      let mut s = String::new();
      std::io::stdin().read_to_string(&mut s).unwrap();
      s
    }
  };

  let uri = std::env::var("DUCKSTRUCT_LSP_DOCUMENT_URI")
    .ok()
    .or_else(|| path.as_ref().map(|p| format!("file://{}", p.display())))
    .unwrap_or_else(|| "file://<stdin>".to_string());

  let parse_result = parse(&source);
  let mut bundle = bundle_from_parse_errors(&parse_result.errors);

  if parse_result.errors.is_empty() {
    if let Some(root) = Root::cast(parse_result.syntax()) {
      let hir = lower(root);
      let mut tycheck = TyCheck::new(hir);
      tycheck.infer();
      bundle.extend_bundle(tycheck.diagnostics.bundle.clone());
    }
  }

  println!("{}", bundle_to_lsp_json(&source, &uri, &bundle));
}
