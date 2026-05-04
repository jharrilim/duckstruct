//! Built-in methods on primitive types: signatures, partial evaluators, and JS runtime
//! routing. Add a method by writing a `signature` (and optionally `evaluate` / `js_call`)
//! function and adding one entry to `PRIMITIVE_METHODS`.

use tycheck::primitive_methods::{
  JsRuntimeAsset, PrimitiveMethodDescriptor, PrimitiveReceiverKind,
};
use tycheck::typed_hir::Ty;

/// Joined element type for an array receiver. `Ty::Generic` for unknown / mixed contents.
fn array_element_ty(recv: &Ty) -> Ty {
  match recv {
    Ty::Array(Some(elems)) if elems.is_empty() => Ty::Generic,
    Ty::Array(Some(elems)) => {
      let first = elems[0].clone();
      for e in elems.iter().skip(1) {
        if !first.type_eq(e) {
          return first.deconst();
        }
      }
      first.deconst()
    }
    Ty::Array(None) | Ty::Generic => Ty::Generic,
    _ => Ty::Generic,
  }
}

fn array_length_signature(recv: &Ty) -> Option<Ty> {
  if !matches!(recv, Ty::Array(_)) {
    return None;
  }
  Some(Ty::Function {
    params: vec![],
    ret: Some(Box::new(Ty::Number(None))),
  })
}

/// `[a, b, c].length()` collapses to the receiver's static element count even when the
/// individual elements are non-constant (`elems.len()` is known regardless).
fn array_length_evaluate(recv: &Ty, _args: &[Ty]) -> Option<Ty> {
  match recv {
    Ty::Array(Some(elems)) => Some(Ty::Number(Some(elems.len() as f64))),
    _ => None,
  }
}

fn array_push_signature(recv: &Ty) -> Option<Ty> {
  if !matches!(recv, Ty::Array(_)) {
    return None;
  }
  let elem = array_element_ty(recv);
  Some(Ty::Function {
    params: vec![elem],
    // Duckstruct's `.push` returns the array (unlike JS-native `Array.prototype.push`).
    ret: Some(Box::new(Ty::Array(None))),
  })
}

/// `[1, 2].push(3)` folds to the constant `Ty::Array(Some([1, 2, 3]))` so codegen can emit
/// a literal. When the receiver is non-constant, return `None` and codegen routes to the
/// runtime `Duckstruct.Lib.Primitive.List.push(arr, x)`.
fn array_push_evaluate(recv: &Ty, args: &[Ty]) -> Option<Ty> {
  match recv {
    Ty::Array(Some(elems)) => {
      let mut out = elems.clone();
      out.push(args.first().cloned().unwrap_or(Ty::Generic));
      Some(Ty::Array(Some(out)))
    }
    _ => None,
  }
}

fn array_push_js_call(recv: &str, args: &[String]) -> String {
  format!(
    "Duckstruct.Lib.Primitive.List.push({}, {})",
    recv,
    args.join(", ")
  )
}

const LIST_RUNTIME: JsRuntimeAsset = JsRuntimeAsset {
  id: "primitive_list",
  source: include_str!("js/primitive/list.js"),
};

/// Table passed to `TyCheck::infer_with_modules(..., Some(PRIMITIVE_METHODS))`.
pub static PRIMITIVE_METHODS: &[PrimitiveMethodDescriptor] = &[
  PrimitiveMethodDescriptor {
    receiver: PrimitiveReceiverKind::Array,
    name: "length",
    arity: 0,
    signature: array_length_signature,
    evaluate: Some(array_length_evaluate),
    js_call: None,
    js_runtime: None,
  },
  PrimitiveMethodDescriptor {
    receiver: PrimitiveReceiverKind::Array,
    name: "push",
    arity: 1,
    signature: array_push_signature,
    evaluate: Some(array_push_evaluate),
    js_call: Some(array_push_js_call),
    js_runtime: Some(LIST_RUNTIME),
  },
];
