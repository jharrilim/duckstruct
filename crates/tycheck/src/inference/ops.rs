use super::*;

impl TyCheck {
  pub(super) fn infer_object(
    &mut self,
    scope: &mut Scope,
    fields: &FxIndexMap<String, DatabaseIdx>,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let mut tys: FxIndexMap<String, Ty> = FxIndexMap::default();
    let mut typed_fields: FxIndexMap<String, TypedDatabaseIdx> = FxIndexMap::default();

    for (field, expr) in fields.iter() {
      let expr = self.infer_expr(scope, expr, module_map);
      let ty = self.ty_db.expr(&expr).ty();

      tys.insert(field.clone(), ty);
      typed_fields.insert(field.clone(), expr);
    }
    self.ty_db.alloc(TypedExpr::Object {
      fields: typed_fields,
      ty: Ty::Object(Some(tys)),
    })
  }

  pub(super) fn infer_array(
    &mut self,
    scope: &mut Scope,
    vals: &[DatabaseIdx],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let (vals, tys): (Vec<TypedDatabaseIdx>, Vec<Ty>) = vals
      .iter()
      .map(|val| {
        let val = self.infer_expr(scope, val, module_map);
        let val_ty = self.ty_db.expr(&val).ty();
        (val, val_ty)
      })
      .unzip();

    let ty = Ty::Array(Some(tys));
    let expr = TypedExpr::Array {
      vals: Some(vals),
      ty,
    };
    self.ty_db.alloc(expr)
  }

  pub(super) fn infer_unary(
    &mut self,
    scope: &mut Scope,
    op: &hir::UnaryOp,
    expr: &DatabaseIdx,
    ast: &ast::expr::UnaryExpr,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let expr = self.infer_expr(scope, expr, module_map);
    let expr_ty = self.ty_db.expr(&expr).ty();
    let ty = match op {
      hir::UnaryOp::Neg => match expr_ty {
        Ty::Number(Some(n)) => Ty::Number(Some(-n)),
        Ty::Number(None) => Ty::Number(None),
        Ty::Void => {
          self.diagnostics.push_error(
            "type::error",
            "cannot apply unary operator `-` to void".to_string(),
            ast.span(),
          );
          Ty::Error
        }
        _ => {
          self.diagnostics.push_error(
            "type::error",
            format!("cannot apply unary operator `-` to type `{}`", expr_ty),
            ast.span(),
          );
          Ty::Error
        }
      },
      hir::UnaryOp::Not => match expr_ty {
        Ty::Boolean(Some(b)) => Ty::Boolean(Some(!b)),
        Ty::Boolean(None) => Ty::Boolean(None),
        Ty::Number(Some(n)) => Ty::Boolean(Some(n == 0.0)),
        Ty::Number(None) => Ty::Boolean(None),
        Ty::String(Some(s)) => Ty::Boolean(Some(s.is_empty())),
        Ty::String(None) => Ty::Boolean(None),
        Ty::Array(_) => Ty::Boolean(Some(false)),
        Ty::Object(_) => Ty::Boolean(Some(false)),
        Ty::Instance(_) => Ty::Boolean(None),
        Ty::Generic => Ty::Boolean(None),
        Ty::Void => {
          self.diagnostics.push_error(
            "type::error",
            "cannot apply unary operator `!` to void".to_string(),
            ast.span(),
          );
          Ty::Error
        }
        _ => {
          self.diagnostics.push_error(
            "type::error",
            format!("cannot apply unary operator `!` to type `{}`", expr_ty),
            ast.span(),
          );
          Ty::Error
        }
      },
    };
    self.ty_db.alloc(TypedExpr::Unary {
      op: op.into(),
      expr,
      ty,
    })
  }

  /// Infererence rules:
  /// 1. Two known value types should unify into a single known value type
  /// 2. Unresolved types paired with a concrete type should unify into an unknown value type
  /// 3. Casting favours the left hand side type
  #[rustfmt::skip]
  pub(super) fn infer_binary(
    &mut self,
    scope: &mut Scope,
    op: &hir::BinaryOp,
    lhs_idx: &DatabaseIdx,
    rhs_idx: &DatabaseIdx,
    ast: &ast::expr::BinaryExpr,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let lhs_idx = self.infer_expr(scope, lhs_idx, module_map);
    let rhs_idx = self.infer_expr(scope, rhs_idx, module_map);
    let (lhs, rhs) = self.ty_db.exprs2(&lhs_idx, &rhs_idx);
    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();
    if matches!(lhs_ty, Ty::Void) || matches!(rhs_ty, Ty::Void) {
      self.diagnostics.push_error(
        "type::error",
        "cannot use void result (e.g. from `print(...)`) as a value".to_string(),
        ast.span(),
      );
      return self.ty_db.alloc(TypedExpr::Binary { op: op.into(), lhs: lhs_idx, rhs: rhs_idx, ty: Ty::Error });
    }
    let ty = match op {
      hir::BinaryOp::Add => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs + rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::String(Some(lhs + &rhs)),
        (Ty::String(_), Ty::Generic) => Ty::String(None),
        (Ty::Generic, Ty::String(_)) => Ty::String(None),
        (Ty::String(_), Ty::String(_)) => Ty::String(None),
        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Array(Some([lhs, rhs].concat())),
        (Ty::Array(_), Ty::Array(_)) => Ty::Array(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Sub => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Mul => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Div => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Eq => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        (Ty::Boolean(Some(lhs)), Ty::Boolean(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Array(_), Ty::Array(_)) => Ty::Boolean(None),
        (Ty::Function { ret: Some(lhs), .. }, Ty::Function { ret: Some(rhs), .. }) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Function { ret: None, .. }, Ty::Function { ret: None, .. }) => Ty::Boolean(None),
        (Ty::Instance(a), Ty::Instance(b)) => Ty::Boolean(Some(a == b)),
        (Ty::Generic, Ty::Generic) => Ty::Boolean(None),
        _ => Ty::Boolean(Some(false)),
      },
      hir::BinaryOp::Neq => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        (Ty::Boolean(Some(lhs)), Ty::Boolean(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Array(_), Ty::Array(_)) => Ty::Boolean(None),
        (Ty::Function { ret: Some(lhs), .. }, Ty::Function { ret: Some(rhs), .. }) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Function { ret: None, .. }, Ty::Function { ret: None, .. }) => Ty::Boolean(None),
        (Ty::Instance(a), Ty::Instance(b)) => Ty::Boolean(Some(a != b)),
        (Ty::Generic, Ty::Generic) => Ty::Boolean(None),
        _ => Ty::Boolean(Some(true)),
      },
      hir::BinaryOp::Lt => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs < rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs < rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Lte => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs <= rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs <= rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Gt => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs > rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs > rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Gte => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs >= rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs >= rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
    };
    self.ty_db.alloc(TypedExpr::Binary { op: op.into(), lhs: lhs_idx, rhs: rhs_idx, ty })
  }
}
