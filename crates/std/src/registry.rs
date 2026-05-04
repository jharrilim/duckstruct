//! Registry data model for the duckstruct standard library.
//!
//! The stdlib is described as a set of `ModuleDescriptor`s plus a prelude (globals
//! and external/builtin functions). The `register_stdlib!` macro in `lib.rs` turns
//! a declarative listing into the static slices consumed by the compiler.

use ast::Root;
use parser::parse;
use tycheck::typed_hir::TypedExpr;
use tycheck::TyCheck;

/// Backend selector for which prelude items to surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
  Js,
  Llvm,
}

/// Set of backends a prelude item participates in. Constructed via the `JS` / `LLVM` /
/// `ALL` constants from the `register_stdlib!` macro DSL.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BackendSet {
  pub js: bool,
  pub llvm: bool,
}

impl BackendSet {
  /// No backend; useful as a default placeholder.
  #[allow(dead_code)]
  pub const NONE: Self = Self {
    js: false,
    llvm: false,
  };
  pub const ALL: Self = Self {
    js: true,
    llvm: true,
  };
  /// JS-only items; reserved for future descriptors.
  #[allow(dead_code)]
  pub const JS: Self = Self {
    js: true,
    llvm: false,
  };
  pub const LLVM: Self = Self {
    js: false,
    llvm: true,
  };

  pub const fn contains(self, b: Backend) -> bool {
    match b {
      Backend::Js => self.js,
      Backend::Llvm => self.llvm,
    }
  }
}

/// Describes an importable stdlib module (e.g. `file`). The `source` is parsed and
/// type-checked on demand to produce the `TyCheck` that the compiler links against.
/// `globals`/`externals` let a module contribute its own prelude items; usually empty.
#[derive(Debug)]
pub struct ModuleDescriptor {
  pub name: &'static str,
  pub source: &'static str,
  pub globals: &'static [GlobalDescriptor],
  pub externals: &'static [ExternalDescriptor],
}

/// A top-level constant injected into the prelude (no `use` required), e.g. `PI`.
/// `factory` builds the `TypedExpr` lazily so non-`const` constructors (like
/// `f64::consts::PI`) are still expressible.
#[derive(Debug)]
pub struct GlobalDescriptor {
  pub name: &'static str,
  pub backends: BackendSet,
  pub factory: fn() -> TypedExpr,
}

/// A top-level external (builtin) function, e.g. `print`. Bodies are provided by
/// the codegen backend; only the name + arity is part of the registry.
#[derive(Debug)]
pub struct ExternalDescriptor {
  pub name: &'static str,
  pub params: usize,
  pub backends: BackendSet,
}

/// Parse + lower + typecheck the source for a stdlib module. Shared by every
/// module so per-module files only carry data, not boilerplate.
pub(crate) fn build_module_tycheck(name: &str, source: &str) -> Result<TyCheck, String> {
  let parse = parse(source);
  if !parse.errors.is_empty() {
    return Err(format!(
      "stdlib module `{}` parse errors: {:?}",
      name, parse.errors
    ));
  }
  let ast = Root::cast(parse.syntax())
    .ok_or_else(|| format!("Failed to build AST for stdlib module `{}`", name))?;
  let hir = hir::lower(ast);
  let mut tycheck = TyCheck::new(hir);
  tycheck.infer();
  Ok(tycheck)
}
