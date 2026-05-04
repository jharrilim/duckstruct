use super::*;

impl TyCheck {
  pub(super) fn receiver_type_key(ty: &Ty) -> Option<String> {
    match ty {
      Ty::Instance(name) => Some(name.clone()),
      Ty::Number(_) => Some("number".to_string()),
      Ty::String(_) => Some("string".to_string()),
      Ty::Boolean(_) => Some("boolean".to_string()),
      Ty::Array(_) => Some("array".to_string()),
      Ty::Object(_) => Some("object".to_string()),
      _ => None,
    }
  }

  pub(super) fn is_allowed_trait_impl_target(&self, scope: &Scope, for_type: &str) -> bool {
    const PRIMITIVES: &[&str] = &["number", "string", "boolean", "array", "object"];
    if PRIMITIVES.contains(&for_type) {
      return true;
    }
    if self.struct_names.contains(for_type) {
      return true;
    }
    scope
      .def(for_type)
      .is_some_and(|idx| matches!(self.ty_db.expr(&idx), TypedExpr::StructConstructor { .. }))
  }

  pub(super) fn validate_trait_impl(
    &mut self,
    scope: &mut Scope,
    trait_name: &str,
    for_type: &str,
    methods: &[hir::stmt::ImplMethod],
    module_map: Option<&ModuleMap<'_>>,
  ) {
    let fallback_span = methods
      .first()
      .and_then(|m| match self.hir_db.get_expr(&m.value) {
        Expr::Function { ast, .. } => Some(ast.span()),
        _ => None,
      })
      .unwrap_or_else(|| TextRange::empty(TextSize::from(0)));
    let impl_key = (trait_name.to_string(), for_type.to_string());
    if self.impl_registry.contains(&impl_key) {
      self.diagnostics.push_error(
        "type::trait_duplicate_impl",
        format!("duplicate impl of trait `{trait_name}` for type `{for_type}`"),
        fallback_span,
      );
      return;
    }
    self.impl_registry.insert(impl_key);

    if !self.is_allowed_trait_impl_target(scope, for_type) {
      self.diagnostics.push_error(
        "type::trait_invalid_target",
        format!(
          "trait impl target `{for_type}` is invalid; only structs and primitives are allowed"
        ),
        fallback_span,
      );
      return;
    }

    let Some(required_methods) = self.trait_requirements.get(trait_name).cloned() else {
      self.diagnostics.push_error(
        "type::trait_unknown",
        format!("unknown trait `{trait_name}`"),
        fallback_span,
      );
      return;
    };

    let mut impl_signatures = FxIndexMap::default();
    let mut impl_method_idxs = FxIndexMap::default();
    for method in methods {
      let (params, body, ast) = match self.hir_db.get_expr(&method.value).clone() {
        Expr::Function {
          name: _,
          params,
          body,
          ast,
        } => (params, body, ast),
        _ => continue,
      };
      let fn_idx = self.infer_function(
        scope,
        &Some(method.name.clone()),
        &params,
        &body,
        &ast,
        module_map,
      );
      impl_signatures.insert(method.name.clone(), params.len());
      impl_method_idxs.insert(method.name.clone(), fn_idx);
    }

    for (required_name, required_arity) in required_methods {
      let mut valid_for_dispatch = false;
      match impl_signatures.get(&required_name) {
        Some(arity) if *arity == required_arity => {
          valid_for_dispatch = true;
        }
        Some(arity) => {
          self.diagnostics.push_error(
            "type::trait_method_mismatch",
            format!(
              "impl method `{required_name}` for trait `{trait_name}` has {} params, expected {}",
              arity, required_arity
            ),
            fallback_span,
          );
        }
        None => {
          self.diagnostics.push_error(
            "type::trait_missing_method",
            format!(
              "impl for `{for_type}` is missing required method `{required_name}` from trait `{trait_name}`"
            ),
            fallback_span,
          );
        }
      }
      if valid_for_dispatch {
        if let Some(fn_idx) = impl_method_idxs.get(&required_name) {
          self.trait_impl_methods.insert(
            (
              trait_name.to_string(),
              for_type.to_string(),
              required_name.clone(),
            ),
            *fn_idx,
          );
        }
      }
    }
  }

  pub(super) fn make_dynamic_method_call(
    &mut self,
    receiver: TypedDatabaseIdx,
    method: &str,
    args: &[TypedDatabaseIdx],
  ) -> TypedDatabaseIdx {
    self.ty_db.alloc(TypedExpr::DynamicMethodCall {
      receiver,
      method: method.to_string(),
      args: args.to_vec(),
      ty: Ty::Generic,
    })
  }

  /// Returns impl function index and whether to inject the receiver as arg0.
  pub(super) fn lookup_static_trait_method(
    &self,
    receiver_ty: &Ty,
    method: &str,
    arg_count: usize,
  ) -> Option<(TypedDatabaseIdx, bool)> {
    let for_type = Self::receiver_type_key(receiver_ty)?;
    let mut found: Option<(TypedDatabaseIdx, bool)> = None;
    for (trait_name, reqs) in &self.trait_requirements {
      let Some(required_arity) = reqs.get(method) else {
        continue;
      };
      let Some(fn_idx) = self.trait_impl_methods.get(&(
        trait_name.clone(),
        for_type.clone(),
        method.to_string(),
      )) else {
        continue;
      };
      let include_receiver = *required_arity > 0;
      let expected_args = if include_receiver {
        required_arity.saturating_sub(1)
      } else {
        0
      };
      if expected_args != arg_count {
        continue;
      }
      if found.is_some() {
        return None;
      }
      found = Some((*fn_idx, include_receiver));
    }
    found
  }
}
