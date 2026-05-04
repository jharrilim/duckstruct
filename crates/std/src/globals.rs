//! Prelude assembly: top-level globals (no `use` required) and external (builtin)
//! functions, filtered by backend. The actual entries live in the registry built
//! by `register_stdlib!` in `lib.rs`; this module just walks them.

use tycheck::typed_hir::TypedExpr;

use crate::registry::Backend;
use crate::{MODULES, PRELUDE_EXTERNALS, PRELUDE_GLOBALS};

/// Returns globals to inject into the compiler's initial scope and ty_db. Walks the
/// prelude plus each module's contributed globals, filtered by `backend`.
pub fn globals_for_backend(backend: Backend) -> Vec<(String, TypedExpr)> {
  let mut out = Vec::new();
  for g in PRELUDE_GLOBALS.iter() {
    if g.backends.contains(backend) {
      out.push((g.name.to_string(), (g.factory)()));
    }
  }
  for m in MODULES.iter() {
    for g in m.globals.iter() {
      if g.backends.contains(backend) {
        out.push((g.name.to_string(), (g.factory)()));
      }
    }
  }
  out
}

/// Returns global external (builtin) function names and their parameter count for the
/// given backend. LLVM: declared in the module and implemented/linked (e.g. print -> printf).
/// JS: print is emitted as console.log by the codegen.
pub fn external_functions_for_backend(backend: Backend) -> Vec<(String, usize)> {
  let mut out = Vec::new();
  for e in PRELUDE_EXTERNALS.iter() {
    if e.backends.contains(backend) {
      out.push((e.name.to_string(), e.params));
    }
  }
  for m in MODULES.iter() {
    for e in m.externals.iter() {
      if e.backends.contains(backend) {
        out.push((e.name.to_string(), e.params));
      }
    }
  }
  out
}
