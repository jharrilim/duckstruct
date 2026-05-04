//! Built-in methods on primitive types, registered in one place for the compiler driver.
//! Extend `PRIMITIVE_METHODS` when adding new surface methods; matching logic lives in `tycheck`.

use tycheck::{PrimitiveMethodDescriptor, PrimitiveReceiverKind};

/// Table passed to `TyCheck::infer_with_modules(..., Some(PRIMITIVE_METHODS))`.
pub static PRIMITIVE_METHODS: &[PrimitiveMethodDescriptor] = &[
  PrimitiveMethodDescriptor {
    receiver: PrimitiveReceiverKind::Array,
    name: "length",
    arity: 0,
  },
  PrimitiveMethodDescriptor {
    receiver: PrimitiveReceiverKind::Array,
    name: "push",
    arity: 1,
  },
];
