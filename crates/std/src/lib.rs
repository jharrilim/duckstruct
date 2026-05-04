//! Duckstruct standard library. Provides a prelude (top-level globals + builtin
//! functions, no `use` required) and importable modules (e.g. `file`).
//!
//! Adding a new module is a two-step change:
//!   1. Create `crates/std/src/<name>.rs` with `pub const DESCRIPTOR: ModuleDescriptor = ...;`
//!   2. Add a single line to the `register_stdlib!` invocation below.
//!
//! Adding a prelude global or external is a single-line change to the same invocation.
//!
//! Built-in methods on primitives (e.g. array `length`, `push`) are listed in
//! `primitive_methods::PRIMITIVE_METHODS` and passed through `TyCheck::infer_with_modules`
//! by the compiler driver; see `tycheck::primitive_methods` for descriptor types and
//! `function_ty_for_receiver` when adding a new receiver kind or method.

mod globals;
mod primitive_methods;
mod registry;

pub use globals::{external_functions_for_backend, globals_for_backend};
pub use primitive_methods::PRIMITIVE_METHODS;
pub use registry::Backend;

use registry::build_module_tycheck;
use tycheck::typed_hir::TypedExpr;

/// Result of loading a stdlib module: name and type-checked state.
/// The compile crate converts this into its LoadedModule when needed.
#[derive(Debug)]
pub struct StdlibModule {
  pub name: String,
  pub tycheck: tycheck::TyCheck,
}

/// Declarative registry for the duckstruct stdlib. Expands to the per-module
/// `mod` declarations plus the static slices consumed by `is_stdlib_module`,
/// `load_stdlib_module`, `globals_for_backend`, and `external_functions_for_backend`.
///
/// Grammar:
/// ```ignore
/// register_stdlib! {
///   prelude {
///     globals { NAME = factory_fn, backends = (ALL|JS|LLVM|NONE); ... }
///     externals { NAME(arity), backends = (ALL|JS|LLVM|NONE); ... }
///   }
///   modules {
///     name => path::to::DESCRIPTOR;
///     ...
///   }
/// }
/// ```
macro_rules! register_stdlib {
  (
    prelude {
      globals {
        $( $g_name:ident = $g_factory:path , backends = $g_backends:ident ; )*
      }
      externals {
        $( $e_name:ident ( $e_params:literal ) , backends = $e_backends:ident ; )*
      }
    }
    modules {
      $( $m_name:ident => $m_path:path ; )*
    }
  ) => {
    $( mod $m_name; )*

    pub(crate) static MODULES: &[&$crate::registry::ModuleDescriptor] = &[
      $( &$m_path ),*
    ];

    pub(crate) static PRELUDE_GLOBALS: &[$crate::registry::GlobalDescriptor] = &[
      $(
        $crate::registry::GlobalDescriptor {
          name: stringify!($g_name),
          factory: $g_factory,
          backends: $crate::registry::BackendSet::$g_backends,
        },
      )*
    ];

    pub(crate) static PRELUDE_EXTERNALS: &[$crate::registry::ExternalDescriptor] = &[
      $(
        $crate::registry::ExternalDescriptor {
          name: stringify!($e_name),
          params: $e_params,
          backends: $crate::registry::BackendSet::$e_backends,
        },
      )*
    ];
  };
}

fn pi_value() -> TypedExpr {
  TypedExpr::Number {
    val: Some(std::f64::consts::PI),
  }
}

register_stdlib! {
  prelude {
    globals {
      PI = pi_value, backends = LLVM;
    }
    externals {
      print(1), backends = ALL;
    }
  }
  modules {
    file => file::DESCRIPTOR;
  }
}

/// Returns true if the given module name is a standard library module
/// (e.g. `file`) that should be resolved from this crate, not the filesystem.
pub fn is_stdlib_module(name: &str) -> bool {
  MODULES.iter().any(|m| m.name == name)
}

/// Load a standard library module by name. Returns the module's name and
/// type-checked TyCheck. Fails if the name is not a known stdlib module or
/// if the module fails to build (e.g. parse/typecheck error).
pub fn load_stdlib_module(name: &str) -> Result<StdlibModule, String> {
  let descriptor = MODULES
    .iter()
    .find(|m| m.name == name)
    .ok_or_else(|| format!("unknown stdlib module: {}", name))?;
  let tycheck = build_module_tycheck(descriptor.name, descriptor.source)?;
  Ok(StdlibModule {
    name: descriptor.name.to_string(),
    tycheck,
  })
}

#[cfg(test)]
mod tests {
  use super::*;

  use ast::Root;
  use hir::lower;
  use parser::parse;
  use tycheck::typed_hir::Ty;
  use tycheck::TyCheck;

  #[test]
  fn array_length_typechecks_with_primitive_table() {
    let code = "let xs = [1, 2, 3];\nxs.length()";
    let root = Root::cast(parse(code).syntax()).expect("parse");
    let hir = lower(root);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(None, None, None, Some(PRIMITIVE_METHODS));
    assert!(
      !tycheck.diagnostics.has_errors(),
      "{:?}",
      tycheck.diagnostics.items()
    );
    let v = tycheck.ty_db.definition("").expect("final expr").value();
    assert_eq!(tycheck.ty_db.expr(v).ty(), Ty::Number(None));
  }

  #[test]
  fn array_push_typechecks_with_primitive_table() {
    let code = "let xs = [1, 2];\nxs.push(3)";
    let root = Root::cast(parse(code).syntax()).expect("parse");
    let hir = lower(root);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(None, None, None, Some(PRIMITIVE_METHODS));
    assert!(
      !tycheck.diagnostics.has_errors(),
      "{:?}",
      tycheck.diagnostics.items()
    );
    let v = tycheck.ty_db.definition("").expect("final expr").value();
    assert_eq!(tycheck.ty_db.expr(v).ty(), Ty::Number(None));
  }

  #[test]
  fn registry_lists_file_module() {
    assert!(is_stdlib_module("file"));
    assert!(!is_stdlib_module("not_a_module"));
  }

  #[test]
  fn load_file_module_exposes_pub_read_write() {
    let m = load_stdlib_module("file").expect("file module loads");
    assert_eq!(m.name, "file");
    for name in &["read", "write"] {
      let def = m
        .tycheck
        .ty_db
        .definition(name)
        .unwrap_or_else(|| panic!("file module should define `{}`", name));
      let is_pub = matches!(
        def,
        tycheck::typed_hir::TypedStmt::FunctionDef { pub_vis: true, .. }
      );
      assert!(is_pub, "`{}` should be pub", name);
    }
  }

  #[test]
  fn load_unknown_module_errors() {
    let err = load_stdlib_module("nope").unwrap_err();
    assert!(err.contains("unknown stdlib module"), "got: {}", err);
  }

  #[test]
  fn prelude_globals_filtered_by_backend() {
    let llvm = globals_for_backend(Backend::Llvm);
    assert!(
      llvm.iter().any(|(n, _)| n == "PI"),
      "PI should be in LLVM prelude"
    );
    let js = globals_for_backend(Backend::Js);
    assert!(
      !js.iter().any(|(n, _)| n == "PI"),
      "PI should not be in JS prelude (LLVM-only)"
    );
  }

  #[test]
  fn prelude_externals_present_for_both_backends() {
    for b in [Backend::Js, Backend::Llvm] {
      let exts = external_functions_for_backend(b);
      let print = exts
        .iter()
        .find(|(n, _)| n == "print")
        .unwrap_or_else(|| panic!("print should be present for {:?}", b));
      assert_eq!(print.1, 1, "print arity for {:?}", b);
    }
  }
}
