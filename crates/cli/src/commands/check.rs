//! `check` command: parse, typecheck, and report diagnostics as JSON (for LSP/editors).

use std::io::Read;
use std::path::{Path, PathBuf};

use ast::Root;
use compile::manifest::{self, Backend as ManifestBackend};
use diagnostics::{bundle_from_parse_errors, bundle_to_lsp_json};
use hir::lower;
use parser::parse;
use tycheck::TyCheck;

fn path_from_lsp_document_uri() -> Option<PathBuf> {
  std::env::var("DUCKSTRUCT_LSP_DOCUMENT_URI").ok().and_then(|uri| {
    let rest = uri.strip_prefix("file://")?;
    let p = if cfg!(windows) {
      PathBuf::from(rest.trim_start_matches('/'))
    } else {
      PathBuf::from(rest)
    };
    Some(p)
  })
}

/// Manifest directory for prelude/backend selection: explicit `--file` path, or LSP document URI.
fn manifest_context_path(cli_path: Option<&PathBuf>) -> Option<PathBuf> {
  cli_path
    .cloned()
    .or_else(path_from_lsp_document_uri)
}

fn std_backend_for_check(context: Option<&Path>) -> duckstruct_std::Backend {
  let Some(from) = context else {
    return duckstruct_std::Backend::Js;
  };
  let Ok(manifest_dir) = manifest::find_manifest_dir(from) else {
    return duckstruct_std::Backend::Js;
  };
  let Ok(m) = manifest::load_manifest(&manifest_dir) else {
    return duckstruct_std::Backend::Js;
  };
  match m.backend() {
    ManifestBackend::Js => duckstruct_std::Backend::Js,
    ManifestBackend::Llvm => duckstruct_std::Backend::Llvm,
  }
}

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
      let ctx_path = manifest_context_path(path.as_ref());
      let std_backend = std_backend_for_check(ctx_path.as_deref());
      let prelude = duckstruct_std::globals_for_backend(std_backend);
      let ext = duckstruct_std::external_signatures_for_backend(std_backend);
      let prelude_ref = (!prelude.is_empty()).then_some(prelude.as_slice());
      let ext_ref = (!ext.is_empty()).then_some(ext.as_slice());
      tycheck.infer_with_modules(
        None,
        prelude_ref,
        ext_ref,
        Some(duckstruct_std::PRIMITIVE_METHODS),
      );
      bundle.extend_bundle(tycheck.diagnostics.bundle.clone());
    }
  }

  println!("{}", bundle_to_lsp_json(&source, &uri, &bundle));
}
