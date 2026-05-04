//! Declarative descriptions of built-in methods on primitive types (e.g. array `length`).
//! `duckstruct-std` supplies a `&'static [PrimitiveMethodDescriptor]` slice; the compiler
//! passes it into `TyCheck::infer_with_modules`. Each descriptor carries:
//!
//! * `signature`: maps a receiver `Ty` to the method's `Ty::Function { params, ret }`. Used
//!   for arity / parameter-constraint checks regardless of constness.
//! * `evaluate`: optional partial evaluator. When the receiver / arg `Ty`s carry concrete
//!   values, the evaluator returns the constant result `Ty` so callers like `let a = xs.length()`
//!   resolve to `Ty::Number(Some(3.0))`. Returning `None` falls back to `signature`'s `ret`.
//!
//! Implementations live in `duckstruct-std`; this crate only defines the contract.

use crate::typed_hir::Ty;

/// Which surface type exposes the method (extend as new primitives gain methods).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveReceiverKind {
  Array,
}

/// JS runtime source needed at link time when a method's `js_call` is invoked. Multiple
/// descriptors may share the same `id` to deduplicate when several methods live in the
/// same runtime file (e.g. all list methods in `primitive/list.js`).
#[derive(Debug, Clone, Copy)]
pub struct JsRuntimeAsset {
  pub id: &'static str,
  pub source: &'static str,
}

/// One built-in method on a primitive receiver. `name` and `arity` mirror the surface call
/// (`receiver.name(arg1, ..., argN)`); the receiver is not counted as a parameter.
#[derive(Debug, Clone, Copy)]
pub struct PrimitiveMethodDescriptor {
  pub receiver: PrimitiveReceiverKind,
  pub name: &'static str,
  pub arity: usize,
  /// Function type for this method given the receiver `Ty`. Returns `None` when the
  /// receiver kind doesn't match (defensive: lookups already filter by `receiver`).
  pub signature: fn(&Ty) -> Option<Ty>,
  /// Partial evaluator; `Some(fn)` runs at type-check time when receiver/args may be
  /// constant. Returns `Some(ty)` to override the call's result with a concrete `Ty`,
  /// `None` to keep the schematic return type from `signature`.
  pub evaluate: Option<fn(&Ty, &[Ty]) -> Option<Ty>>,
  /// JS code emitter. When set, JS codegen routes `recv.method(args)` through this fn
  /// instead of emitting the literal `recv.method(args)` form. Used for methods whose
  /// runtime semantics differ from JS-native (e.g. duckstruct's `push` returns the array).
  pub js_call: Option<fn(receiver_js: &str, args_js: &[String]) -> String>,
  /// JS runtime asset bundled when `js_call` is invoked. Driver dedupes by `id` and
  /// prepends the sources to the final bundle.
  pub js_runtime: Option<JsRuntimeAsset>,
}
