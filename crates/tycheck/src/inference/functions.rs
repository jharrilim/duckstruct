use super::*;

impl TyCheck {
  pub(super) fn infer_function_def(
    &mut self,
    scope: &mut Scope,
    name: &str,
    value: &DatabaseIdx,
    pub_vis: bool,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedStmt {
    let (params, body, ast) = match self.hir_db.get_expr(value).clone() {
      Expr::Function {
        name: _,
        params,
        body,
        ast,
      } => (params, body, ast),
      _ => unreachable!("Function definition must be a function"),
    };
    let func_value =
      self.infer_function(scope, &Some(name.to_string()), &params, &body, &ast, module_map);
    scope.define(name.to_string(), func_value);
    TypedStmt::FunctionDef {
      name: name.to_string(),
      value: func_value,
      pub_vis,
    }
  }

  pub(super) fn refine_function_parameter_method_from_call(
    &mut self,
    param_idx: &TypedDatabaseIdx,
    method: &str,
    args: &[TypedDatabaseIdx],
    _ast: &ast::expr::FunctionCall,
  ) -> Result<(), String> {
    let arg_tys: Vec<Ty> = args.iter().map(|a| self.ty_db.expr(a).ty()).collect();
    let call_sig = Ty::Function {
      params: arg_tys,
      ret: Some(Box::new(Ty::Generic)),
    };
    let orig = self.ty_db.expr(param_idx).ty().clone();
    let next = match orig {
      Ty::Generic => Ty::Object(Some(index_map!(method.to_string() => call_sig.clone()))),
      Ty::Object(None) => Ty::Object(Some(index_map!(method.to_string() => call_sig.clone()))),
      Ty::Object(Some(fs)) => {
        let merged = match fs.get(method) {
          None | Some(Ty::Generic) => call_sig.clone(),
          Some(Ty::Function { params: ep, ret: er }) => {
            let Ty::Function { params: cp, ret: cr } = &call_sig else {
              unreachable!()
            };
            if ep.len() != cp.len() {
              return Err(format!(
                "method `{}` was previously called with {} arguments but here with {}",
                method,
                ep.len(),
                cp.len()
              ));
            }
            Ty::Function {
              params: ep
                .iter()
                .zip(cp.iter())
                .map(|(e, c)| if e == c { e.clone() } else { Ty::Generic })
                .collect(),
              ret: match (er.as_ref(), cr.as_ref()) {
                (Some(e), Some(c)) if e == c => er.clone(),
                _ => Some(Box::new(Ty::Generic)),
              },
            }
          }
          Some(bad) => {
            return Err(format!(
              "cannot call `{}()`; parameter already has field `{}` of type `{}`",
              method, method, bad
            ));
          }
        };
        let mut fs = fs;
        fs.insert(method.to_string(), merged);
        Ty::Object(Some(fs))
      }
      other => return Err(format!("Cannot call `{}()` on non-object parameter type `{}`", method, other)),
    };
    self.ty_db.expr_mut(param_idx).replace_ty(next);
    Ok(())
  }

  pub(super) fn infer_function(
    &mut self,
    scope: &mut Scope,
    name: &Option<String>,
    params: &[String],
    body: &DatabaseIdx,
    _ast: &ast::expr::Function,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let params: FxIndexMap<String, TypedDatabaseIdx> = params
      .iter()
      .map(|name| {
        (
          name.clone(),
          scope.def(name).unwrap_or_else(|| {
            self.ty_db.alloc(TypedExpr::FunctionParameter {
              name: name.clone(),
              ty: Ty::Generic,
            })
          }),
        )
      })
      .collect();
    if let Some(name) = name {
      scope.push_named_frame(name.to_string());
    } else {
      scope.push_frame();
    }
    scope.define_args(&params);
    let body_idx = self.infer_expr(scope, body, module_map);
    let body_ty = self.ty_db.expr(&body_idx).ty();
    let ty = Ty::Function {
      ret: Some(Box::new(body_ty)),
      params: params.iter().map(|(_, v)| self.ty_db.expr(v).ty()).collect(),
    };
    let expr = TypedExpr::FunctionDef(FunctionDef {
      name: name.clone(),
      params,
      body: body_idx,
      body_hir: *body,
      closure_scope: scope.flatten(),
      ty,
    });
    scope.pop_frame();
    self.ty_db.alloc(expr)
  }
}
