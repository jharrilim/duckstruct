use super::*;

impl TyCheck {
  pub(super) fn lookup_primitive_descriptor(
    &self,
    receiver_ty: &Ty,
    field: &str,
  ) -> Option<&'static PrimitiveMethodDescriptor> {
    let slice = self.primitive_methods.as_ref()?;
    let kind = match receiver_ty {
      Ty::Array(_) => PrimitiveReceiverKind::Array,
      _ => return None,
    };
    slice
      .iter()
      .find(|d| d.receiver == kind && d.name == field)
  }

  pub(super) fn is_primitive_builtin_field(
    &self,
    typed_object: &TypedDatabaseIdx,
    field: &str,
  ) -> bool {
    let ty = self.ty_db.expr(typed_object).ty();
    self.lookup_primitive_descriptor(&ty, field).is_some()
  }
}
