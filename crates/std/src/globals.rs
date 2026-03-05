//! Standard library globals (top-level symbols, no import required).

use tycheck::typed_hir::TypedExpr;

/// Backend selector for which globals to provide.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
  Js,
  Llvm,
}

/// Returns globals to inject into the compiler's initial scope and ty_db.
/// Only LLVM globals are fleshed out first; JS can return empty or same set later.
pub fn globals_for_backend(backend: Backend) -> Vec<(String, TypedExpr)> {
  match backend {
    Backend::Llvm => globals_llvm(),
    Backend::Js => globals_js(),
  }
}

fn globals_llvm() -> Vec<(String, TypedExpr)> {
  vec![(
    "PI".to_string(),
    TypedExpr::Number {
      val: Some(std::f64::consts::PI),
    },
  )]
}

fn globals_js() -> Vec<(String, TypedExpr)> {
  // JS stdlib not fleshed out yet; return empty so architecture is in place.
  vec![]
}

/// Returns global external (builtin) function names and their parameter count for the given backend.
/// These are declared in the module and must be provided at link time (e.g. print for LLVM).
pub fn external_functions_for_backend(backend: Backend) -> Vec<(String, usize)> {
  match backend {
    Backend::Llvm => vec![("print".to_string(), 1)],
    Backend::Js => vec![],
  }
}
