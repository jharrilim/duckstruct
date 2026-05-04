pub mod diagnostics;
pub mod inference;
pub mod primitive_methods;
pub mod scope;
pub mod typed_db;
pub mod typed_hir;

pub use inference::TyCheck;
pub use primitive_methods::{PrimitiveMethodDescriptor, PrimitiveReceiverKind};
