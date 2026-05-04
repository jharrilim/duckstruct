use super::*;

impl TyCheck {
  pub(super) fn infer_path_ref(
    &mut self,
    scope: &mut Scope,
    path: &[String],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    if path.is_empty() {
      return self.ty_db.alloc(TypedExpr::Error);
    }
    if path.len() == 1 {
      return self.infer_variable_ref(scope, &path[0]);
    }
    if let Some(map) = module_map {
      let mod_key = path[0..path.len() - 1].join("::");
      if let Some(dep) = map.get(&mod_key) {
        let item_name = path.last().unwrap();
        if let Some(typed_stmt) = dep.ty_db.definition(item_name) {
          let is_pub = match typed_stmt {
            TypedStmt::VariableDef { pub_vis, .. }
            | TypedStmt::FunctionDef { pub_vis, .. }
            | TypedStmt::StructDef { pub_vis, .. }
            | TypedStmt::TraitDef { pub_vis, .. } => *pub_vis,
            _ => false,
          };
          if is_pub {
            let ty = dep.ty_db.expr(typed_stmt.value()).ty().clone();
            return self.ty_db.alloc(TypedExpr::VariableRef {
              var: item_name.clone(),
              ty,
            });
          }
        }
      }
    }
    self.ty_db.alloc(TypedExpr::Error)
  }

  pub(super) fn infer_struct_literal(
    &mut self,
    scope: &mut Scope,
    type_expr: &DatabaseIdx,
    fields: &FxIndexMap<String, DatabaseIdx>,
    ast: &ast::expr::StructLiteral,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    if !fields.is_empty() {
      self.diagnostics.push_error(
        "type::error",
        "struct literals with fields are not yet supported".to_string(),
        ast.span(),
      );
      return self.ty_db.alloc(TypedExpr::Error);
    }
    let hir_db = Rc::clone(&self.hir_db);
    let struct_name = match hir_db.get_expr(type_expr) {
      Expr::VariableRef { var, .. } => {
        let Some(def) = scope.def(var) else {
          self
            .diagnostics
            .push_error("type::error", format!("undefined `{}`", var), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        match self.ty_db.expr(&def) {
          TypedExpr::StructConstructor { name } => name.clone(),
          _ => {
            self.diagnostics.push_error(
              "type::error",
              format!("`{}` is not a struct type", var),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
        }
      }
      Expr::PathRef { path, .. } => {
        let Some(map) = module_map else {
          self
            .diagnostics
            .push_error("type::error", "unknown module path".to_string(), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        if path.len() < 2 {
          self
            .diagnostics
            .push_error("type::error", "invalid struct literal type".to_string(), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        }
        let mod_key = path[0..path.len() - 1].join("::");
        let item_name = path.last().unwrap();
        let Some(dep) = map.get(&mod_key) else {
          self.diagnostics.push_error(
            "type::error",
            format!("unknown module `{}`", mod_key),
            ast.span(),
          );
          return self.ty_db.alloc(TypedExpr::Error);
        };
        let Some(typed_stmt) = dep.ty_db.definition(item_name) else {
          self
            .diagnostics
            .push_error("type::error", format!("unknown item `{}`", item_name), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        match typed_stmt {
          TypedStmt::StructDef {
            name,
            pub_vis: true,
            ..
          } => name.clone(),
          TypedStmt::StructDef { pub_vis: false, .. } => {
            self.diagnostics.push_error(
              "type::error",
              format!("struct `{}` is not public", item_name),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
          _ => {
            self.diagnostics.push_error(
              "type::error",
              format!("`{}` is not a struct in this module", item_name),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
        }
      }
      _ => {
        self
          .diagnostics
          .push_error("type::error", "invalid struct literal type".to_string(), ast.span());
        return self.ty_db.alloc(TypedExpr::Error);
      }
    };
    self.ty_db.alloc(TypedExpr::StructInstance { name: struct_name })
  }

  pub(super) fn infer_variable_ref(&mut self, scope: &mut Scope, var: &str) -> TypedDatabaseIdx {
    if scope.is_late_binding(var) {
      return self.ty_db.alloc(TypedExpr::VariableRef {
        var: var.to_string(),
        ty: Ty::Generic,
      });
    }
    match scope.def(var) {
      Some(t) => t,
      None => self.ty_db.alloc(TypedExpr::Error),
    }
  }

  pub(super) fn infer_object_field_access(
    &mut self,
    scope: &mut Scope,
    object: &DatabaseIdx,
    field: &str,
    ast: &ast::expr::ObjectFieldAccess,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let typed_object = self.infer_expr(scope, object, module_map);
    let field_ty = match self.ty_db.expr(&typed_object).ty() {
      Ty::Object(Some(fields)) => match fields.get(field) {
        Some(ty) => ty.clone(),
        None => Ty::Generic,
      },
      Ty::Object(None) => todo!("handle object with unknown fields"),
      Ty::Generic => Ty::Generic,
      Ty::Array(_) => {
        let receiver_ty = self.ty_db.expr(&typed_object).ty().clone();
        if let Some(desc) = self.lookup_primitive_descriptor(&receiver_ty, field) {
          (desc.signature)(&receiver_ty).unwrap_or_else(|| {
            self.diagnostics.push_error(
              "type::error",
              format!(
                "built-in method `{}` is not valid for receiver type {}",
                field, receiver_ty
              ),
              ast.span(),
            );
            Ty::Error
          })
        } else {
          let msg = if self.primitive_methods.is_none() {
            format!(
              "Unknown method `{}` on array (built-in array methods need the compiler stdlib table)",
              field
            )
          } else {
            format!("Unknown method `{}` on array", field)
          };
          self.diagnostics.push_error("type::error", msg, ast.span());
          Ty::Error
        }
      }
      ty => {
        if matches!(
          ty,
          Ty::Generic | Ty::Instance(_) | Ty::Number(_) | Ty::String(_) | Ty::Boolean(_)
        ) {
          return self.ty_db.alloc(TypedExpr::ObjectFieldAccess {
            object: typed_object,
            field: field.to_string(),
            ty: Ty::Generic,
          });
        }
        match self.query_object_name(object) {
          Some(name) => {
            if let Some(similar_name) = scope.def_name_similar_to(name) {
              self.diagnostics.push_error(
                "type::error",
                format!(
                  "Cannot access field `{}` on {}. Did you mean `{}`?",
                  field, name, similar_name
                ),
                ast.span(),
              );
            } else {
              self.diagnostics.push_error(
                "type::error",
                format!("Cannot access field `{}` on {}.", field, name),
                ast.span(),
              );
            }
          }
          None => {
            self.diagnostics.push_error(
              "type::error",
              format!("Cannot access field `{}` on {}.", field, ty),
              ast.span(),
            );
          }
        }
        Ty::Error
      }
    };
    if !self.is_primitive_builtin_field(&typed_object, field) {
      self.propogate_object_field_constraint(scope, &typed_object, field, &field_ty, ast);
    }
    self.ty_db.alloc(TypedExpr::ObjectFieldAccess {
      object: typed_object,
      field: field.to_string(),
      ty: field_ty,
    })
  }

  pub(super) fn propogate_object_field_constraint(
    &mut self,
    scope: &mut Scope,
    object: &TypedDatabaseIdx,
    field: &str,
    field_ty: &Ty,
    ast: &ast::expr::ObjectFieldAccess,
  ) {
    if self.is_primitive_builtin_field(object, field) {
      return;
    }
    match self.ty_db.expr(object) {
      TypedExpr::ObjectFieldAccess {
        object,
        field: inner_field,
        ..
      } => match self.ty_db.expr(object) {
        TypedExpr::Object { fields, .. } => {
          if let Some(field_expr) = fields.get(inner_field) {
            self.propogate_object_field_constraint(scope, &field_expr.clone(), field, field_ty, ast)
          } else {
            todo!("Doesn't seem possible?")
          }
        }
        _ => todo!("Doesn't seem possible?"),
      },
      TypedExpr::FunctionCall { ret, .. } => {
        self.propogate_object_field_constraint(scope, &ret.clone(), field, field_ty, ast)
      }
      TypedExpr::Block { stmts, .. } => {
        if let Some(last_typed_stmt) = stmts.last() {
          self.propogate_object_field_constraint(
            scope,
            &last_typed_stmt.value().clone(),
            field,
            field_ty,
            ast,
          );
        }
      }
      TypedExpr::FunctionParameter { name: var, .. } | TypedExpr::VariableRef { var, .. } => {
        if let Some(param_idx) = scope.def(var) {
          self.ty_db.edit_ty(&param_idx, |original_ty| match original_ty {
            Ty::Object(Some(fields)) => {
              let mut fields = fields;
              fields.insert(field.to_string(), field_ty.clone());
              Ty::Object(Some(fields))
            }
            Ty::Object(None) => {
              let fields = index_map!(field.to_string() => field_ty.clone());
              Ty::Object(Some(fields))
            }
            Ty::Generic => {
              let fields = index_map!(field.to_string() => field_ty.clone());
              Ty::Object(Some(fields))
            }
            _ => {
              self.diagnostics.push_error(
                "type::error",
                format!(
                  "Cannot access field `{}` on non-object type. Type: {:?}",
                  field, original_ty
                ),
                ast.span(),
              );
              Ty::Error
            }
          });
        }
      }
      _ => {}
    }
  }

  pub(super) fn query_object_name(&self, idx: &DatabaseIdx) -> Option<&str> {
    let expr = self.hir_db.get_expr(idx);
    let name = match expr {
      Expr::VariableRef { var, .. } => var,
      Expr::Object { .. } => "<anonymous>",
      _ => return None,
    };
    Some(name)
  }
}
