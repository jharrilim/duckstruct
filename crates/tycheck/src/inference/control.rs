use super::*;

impl TyCheck {
  pub(super) fn infer_conditional(
    &mut self,
    scope: &mut Scope,
    condition: &DatabaseIdx,
    then_branch: &DatabaseIdx,
    else_branch: &DatabaseIdx,
    ast: &ast::expr::Conditional,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let condition = self.infer_expr(scope, condition, module_map);
    match self.ty_db.expr(&condition).ty() {
      Ty::Boolean(Some(boolean)) => {
        if boolean {
          self.infer_expr(scope, then_branch, module_map)
        } else {
          self.infer_expr(scope, else_branch, module_map)
        }
      }
      Ty::Boolean(None) => {
        let then_branch = self.infer_expr(scope, then_branch, module_map);
        let else_branch = self.infer_expr(scope, else_branch, module_map);
        let then_ty = self.ty_db.expr(&then_branch).ty();
        let else_ty = self.ty_db.expr(&else_branch).ty();
        if !then_ty.type_eq(&else_ty) {
          self.diagnostics.push_error(
            "type::error",
            format!(
              "Type mismatch in conditional expression: {} and {}",
              then_ty, else_ty
            ),
            ast.span(),
          );
        }
        let ty = then_ty.deconst();
        let expr = TypedExpr::Conditional {
          condition,
          then_branch,
          else_branch,
          ty,
        };
        self.ty_db.alloc(expr)
      }
      _ => {
        self
          .diagnostics
          .push_error("type::error", "Condition must be a boolean".to_string(), ast.span());
        let then_branch = self.ty_db.alloc(TypedExpr::Error);
        let else_branch = self.ty_db.alloc(TypedExpr::Error);
        self.ty_db.alloc(TypedExpr::Conditional {
          condition,
          then_branch,
          else_branch,
          ty: Ty::Error,
        })
      }
    }
  }

  pub(super) fn join_element_types_for_iterable(ty: &Ty) -> Ty {
    match ty {
      Ty::Array(Some(elems)) => {
        if elems.is_empty() {
          return Ty::Generic;
        }
        let first = elems[0].clone();
        for e in elems.iter().skip(1) {
          if !first.type_eq(e) {
            return first.deconst();
          }
        }
        first.deconst()
      }
      _ => Ty::Generic,
    }
  }

  #[allow(clippy::too_many_arguments)]
  pub(super) fn infer_for_shaped_subexprs(
    &mut self,
    scope: &mut Scope,
    bind_name: &str,
    iterable_ty: &Ty,
    fold_params: Option<&(String, String)>,
    acc_init_typed: Option<&TypedDatabaseIdx>,
    where_clause: Option<&DatabaseIdx>,
    body: &DatabaseIdx,
    module_map: Option<&ModuleMap<'_>>,
  ) -> (Option<TypedDatabaseIdx>, TypedDatabaseIdx) {
    scope.push_frame();
    let elem_ty = Self::join_element_types_for_iterable(iterable_ty);
    scope.define(
      bind_name.to_string(),
      self.ty_db.alloc(TypedExpr::VariableRef {
        var: bind_name.to_string(),
        ty: elem_ty,
      }),
    );
    if let Some((acc_n, idx_n)) = fold_params {
      let acc_ty = acc_init_typed
        .map(|t| self.ty_db.expr(t).ty().clone())
        .unwrap_or(Ty::Generic);
      scope.define(
        acc_n.clone(),
        self.ty_db.alloc(TypedExpr::VariableRef {
          var: acc_n.clone(),
          ty: acc_ty,
        }),
      );
      scope.define(
        idx_n.clone(),
        self.ty_db.alloc(TypedExpr::VariableRef {
          var: idx_n.clone(),
          ty: Ty::Number(None),
        }),
      );
    }
    let where_shaped = where_clause.map(|w| self.infer_expr(scope, w, module_map));
    let body_shaped = self.infer_expr(scope, body, module_map);
    scope.pop_frame();
    (where_shaped, body_shaped)
  }

  #[allow(clippy::too_many_arguments)]
  pub(super) fn infer_for(
    &mut self,
    scope: &mut Scope,
    binding: &Pat,
    iterable: &DatabaseIdx,
    where_clause: Option<&DatabaseIdx>,
    acc_init: Option<&DatabaseIdx>,
    fold_params: Option<&(String, String)>,
    body: &DatabaseIdx,
    for_ast: &ast::expr::ForExpression,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let span = for_ast.span();
    let bind_name = match binding {
      Pat::Ident { name } => name.clone(),
      _ => {
        self
          .diagnostics
          .push_error("type::error", "for-loop binding must be a simple name".to_string(), span);
        return self.ty_db.alloc(TypedExpr::Error);
      }
    };
    if fold_params.is_some() && acc_init.is_none() {
      self.diagnostics.push_error(
        "type::error",
        "fold parameters `|acc, i|` require an accumulator initializer `| expr |`".to_string(),
        span,
      );
      return self.ty_db.alloc(TypedExpr::Error);
    }
    let iterable_typed = self.infer_expr(scope, iterable, module_map);
    let iterable_ty = self.ty_db.expr(&iterable_typed).ty().clone();
    let acc_init_typed = acc_init.map(|i| self.infer_expr(scope, i, module_map));
    let (where_shaped, body_shaped) = self.infer_for_shaped_subexprs(
      scope, &bind_name, &iterable_ty, fold_params, acc_init_typed.as_ref(), where_clause, body, module_map,
    );
    let mut determinate_ty: Option<Ty> = None;
    if let TypedExpr::Array { vals: Some(elem_idxs), ty: Ty::Array(Some(elem_tys)), .. } = self.ty_db.expr(&iterable_typed).clone() {
      if elem_idxs.len() == elem_tys.len() {
        let mut determinate_ok = true;
        let mut running_acc = acc_init_typed;
        let mut last_body: Option<TypedDatabaseIdx> = None;
        for (k, elem_idx) in elem_idxs.iter().enumerate() {
          scope.push_frame();
          scope.define(bind_name.clone(), *elem_idx);
          if let Some((acc_n, idx_n)) = fold_params {
            let acc_val = running_acc.expect("fold requires initializer");
            scope.define(acc_n.clone(), acc_val);
            let i_lit = self.ty_db.alloc(TypedExpr::Number { val: Some(k as f64) });
            scope.define(idx_n.clone(), i_lit);
          }
          if let Some(widx) = where_clause {
            let w_typed = self.infer_expr(scope, widx, module_map);
            match self.ty_db.expr(&w_typed).ty().clone() {
              Ty::Boolean(Some(false)) => {
                scope.pop_frame();
                continue;
              }
              Ty::Boolean(Some(true)) => {}
              Ty::Boolean(None) => {
                determinate_ok = false;
                scope.pop_frame();
                break;
              }
              _ => {
                self.diagnostics.push_error("type::error", "where clause must be boolean".to_string(), span);
                determinate_ok = false;
                scope.pop_frame();
                break;
              }
            }
          }
          let body_typed = self.infer_expr(scope, body, module_map);
          scope.pop_frame();
          if !determinate_ok {
            break;
          }
          last_body = Some(body_typed);
          if fold_params.is_some() {
            running_acc = Some(body_typed);
          }
        }
        if determinate_ok {
          determinate_ty = Some(if fold_params.is_some() {
            running_acc.map(|idx| self.ty_db.expr(&idx).ty().clone())
              .or_else(|| acc_init_typed.as_ref().map(|t| self.ty_db.expr(t).ty().clone()))
              .unwrap_or(Ty::Generic)
          } else if let Some(lb) = last_body {
            self.ty_db.expr(&lb).ty().clone()
          } else {
            acc_init_typed.as_ref().map(|t| self.ty_db.expr(t).ty().clone()).unwrap_or(Ty::Generic)
          });
        }
      }
    }
    let final_ty = if let Some(dt) = determinate_ty {
      dt
    } else {
      let body_ty = self.ty_db.expr(&body_shaped).ty().clone();
      match &acc_init_typed {
        Some(init_idx) => {
          let init_ty = self.ty_db.expr(init_idx).ty().clone();
          if !init_ty.type_eq(&body_ty) {
            self.diagnostics.push_error(
              "type::error",
              format!("Type mismatch in for-loop: initializer {} and body {}", init_ty, body_ty),
              span,
            );
            Ty::Error
          } else {
            body_ty.deconst()
          }
        }
        None => body_ty.deconst(),
      }
    };
    self.ty_db.alloc(TypedExpr::For {
      binding: bind_name,
      iterable: iterable_typed,
      where_clause: where_shaped,
      acc_init: acc_init_typed,
      fold_acc: fold_params.map(|(a, _)| a.clone()),
      fold_index: fold_params.map(|(_, i)| i.clone()),
      body: body_shaped,
      ty: final_ty,
    })
  }

  pub(super) fn infer_block(
    &mut self,
    scope: &mut Scope,
    stmts: &[Stmt],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    scope.push_frame();
    let stmts = stmts
      .iter()
      .map(|stmt| self.infer_stmt(scope, Either::Left(stmt), module_map))
      .collect::<Vec<_>>();
    let ty = if let Some(last) = stmts.last() {
      self.ty_db.expr(last.value()).ty()
    } else {
      Ty::Generic
    };
    scope.pop_frame();
    self.ty_db.alloc(TypedExpr::Block { stmts, ty })
  }
}
