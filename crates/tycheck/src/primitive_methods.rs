//! Declarative descriptions of built-in methods on primitive types (e.g. array `length`).
//! The `duckstruct-std` crate supplies a `&'static [PrimitiveMethodDescriptor]` slice; the
//! compiler passes it into `TyCheck::infer_with_modules`.

use crate::typed_hir::Ty;

/// Which surface type exposes the method (extend as new primitives gain methods).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveReceiverKind {
  Array,
}

/// One built-in method: receiver kind, method name (as written after `.`), and arity of
/// explicit arguments (the receiver before `.` is not counted).
#[derive(Debug, Clone, Copy)]
pub struct PrimitiveMethodDescriptor {
  pub receiver: PrimitiveReceiverKind,
  pub name: &'static str,
  pub arity: usize,
}

/// Element type for `[a, b, ...]` when inferring `push` and similar.
pub fn array_element_ty(receiver_ty: &Ty) -> Ty {
  match receiver_ty {
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

impl PrimitiveMethodDescriptor {
  /// `receiver_ty` must match `self.receiver` (e.g. `Ty::Array(_)` for array methods).
  pub fn function_ty_for_receiver(&self, receiver_ty: &Ty) -> Option<Ty> {
    match self.receiver {
      PrimitiveReceiverKind::Array => {
        if !matches!(receiver_ty, Ty::Array(_)) {
          return None;
        }
        let elem = array_element_ty(receiver_ty);
        match self.name {
          "length" if self.arity == 0 => Some(Ty::Function {
            params: vec![],
            ret: Some(Box::new(Ty::Number(None))),
          }),
          "push" if self.arity == 1 => Some(Ty::Function {
            params: vec![elem],
            ret: Some(Box::new(Ty::Number(None))),
          }),
          _ => None,
        }
      }
    }
  }
}
