use std::rc::Rc;

use crate::diagnostics::Diagnostics;
use crate::scope::Scope;
use crate::typed_db::{TypedDatabase, TypedDatabaseIdx};
use crate::typed_hir::{FunctionDef, Ty, TypedExpr, TypedStmt};
use data_structures::{index_map, FxIndexMap};
use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};

#[derive(Debug)]
pub enum Either<A, B> {
  Left(A),
  Right(B),
}

#[derive(Debug)]
pub struct TyCheck {
  pub hir_db: Rc<hir::Database>,
  pub ty_db: TypedDatabase,
  #[allow(unused)]
  pub diagnostics: Diagnostics,
}

impl TyCheck {
  pub fn new(hir_db: hir::Database) -> Self {
    Self {
      hir_db: Rc::new(hir_db),
      ty_db: TypedDatabase::default(),
      diagnostics: Diagnostics::default(),
    }
  }

  /// Infers types for all statements in the HIR database.
  pub fn infer(&mut self) {
    let mut scope = Scope::default();
    for (stmt_ident, statement) in self.hir_db.clone().defs_iter() {
      let typed_stmt = self.infer_stmt(&mut scope, Either::Left(statement));
      self.ty_db.define(stmt_ident.clone(), typed_stmt);
    }
  }

  pub fn infer_stmt(&mut self, scope: &mut Scope, stmt: Either<&Stmt, &DatabaseIdx>) -> TypedStmt {
    match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name, value } => {
          let value = self.infer_expr(scope, value);
          scope.define(name.clone(), value);
          TypedStmt::VariableDef {
            name: name.clone(),
            value,
          }
        }
        Stmt::FunctionDef { name, value } => self.infer_function_def(scope, name, value),
        Stmt::Expr(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx)),
      },
      Either::Right(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx)),
    }
  }

  pub fn infer_expr(&mut self, scope: &mut Scope, expr_idx: &DatabaseIdx) -> TypedDatabaseIdx {
    let hir_db = Rc::clone(&self.hir_db);
    let expr = hir_db.get_expr(expr_idx);

    let expr = match expr {
      Expr::VariableRef { var } => return self.infer_variable_ref(scope, var),
      Expr::Number { n } => TypedExpr::Number { val: Some(*n) },
      Expr::String { s } => TypedExpr::String {
        val: Some(s.clone()),
      },
      Expr::Block { stmts } => return self.infer_block(scope, stmts),
      Expr::Boolean { b } => TypedExpr::Boolean { val: Some(*b) },
      Expr::Binary { op, lhs, rhs } => return self.infer_binary(scope, op, lhs, rhs),
      Expr::Unary { op, expr } => return self.infer_unary(scope, op, expr),
      Expr::Function { name, params, body } => {
        return self.infer_function(scope, name, params, body)
      }
      Expr::FunctionCall { args, func } => return self.infer_function_call(scope, func, args),
      Expr::Array { vals } => return self.infer_array(scope, vals),
      Expr::Conditional {
        condition,
        then_branch,
        else_branch,
      } => return self.infer_conditional(scope, condition, then_branch, else_branch),
      Expr::Object { fields } => return self.infer_object(scope, fields),
      Expr::ObjectFieldAccess { object, field } => {
        return self.infer_object_field_access(scope, object, field)
      }
      Expr::Missing => {
        todo!("Handle expression missing: {:?}", expr);
      }
    };
    self.ty_db.alloc(expr)
  }

  fn infer_variable_ref(&mut self, scope: &mut Scope, var: &str) -> TypedDatabaseIdx {
    // Use this as a way to prevent infinite recursion during type inference
    // on a function definition.
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

  fn infer_object_field_access(
    &mut self,
    scope: &mut Scope,
    object: &DatabaseIdx,
    field: &str,
  ) -> TypedDatabaseIdx {
    let typed_object = self.infer_expr(scope, object);
    let field_ty = match self.ty_db.expr(&typed_object).ty() {
      Ty::Object(Some(fields)) => match fields.get(field) {
        Some(ty) => ty.clone(),
        None => Ty::Generic,
      },
      Ty::Object(None) => {
        todo!("handle object with unknown fields");
      }
      Ty::Generic => Ty::Generic,
      ty => {
        match self.query_object_name(object) {
          Some(name) => {
            if let Some(similar_name) = scope.def_name_similar_to(name) {
              self.diagnostics.push_error(format!(
                "Cannot access field `{}` on {}. Did you mean `{}`?",
                field, name, similar_name
              ));
            } else {
              self
                .diagnostics
                .push_error(format!("Cannot access field `{}` on {}.", field, name));
            }
          }
          None => {
            self
              .diagnostics
              .push_error(format!("Cannot access field `{}` on {}.", field, ty));
          }
        }
        Ty::Error
      }
    };

    self.propogate_object_field_constraint(scope, &typed_object, field, &field_ty);

    self.ty_db.alloc(TypedExpr::ObjectFieldAccess {
      object: typed_object,
      field: field.to_string(),
      ty: field_ty,
    })
  }

  fn propogate_object_field_constraint(
    &mut self,
    scope: &mut Scope,
    object: &TypedDatabaseIdx,
    field: &str,
    field_ty: &Ty,
  ) {
    // Propagate the type of the field to the object.
    match self.ty_db.expr(object) {
      TypedExpr::FunctionCall { ret, .. } => {
        self.propogate_object_field_constraint(scope, &ret.clone(), field, field_ty)
      }
      TypedExpr::Block { stmts, .. } => {
        if let Some(last_typed_stmt) = stmts.last() {
          self.propogate_object_field_constraint(
            scope,
            &last_typed_stmt.value().clone(),
            field,
            field_ty,
          );
        }
      }
      TypedExpr::FunctionParameter { name: var, .. } | TypedExpr::VariableRef { var, .. } => {
        if let Some(param_idx) = scope.def(var) {
          self
            .ty_db
            .edit_ty(&param_idx, |original_ty| match original_ty {
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
                self.diagnostics.push_error(format!(
                  "Cannot access field `{}` on non-object type. Type: {:?}",
                  field, original_ty
                ));
                Ty::Error
              }
            });
        }
      }
      _ => {}
    }
  }

  fn infer_object(
    &mut self,
    scope: &mut Scope,
    fields: &FxIndexMap<String, DatabaseIdx>,
  ) -> TypedDatabaseIdx {
    let mut tys: FxIndexMap<String, Ty> = FxIndexMap::default();
    let mut typed_fields: FxIndexMap<String, TypedDatabaseIdx> = FxIndexMap::default();

    for (field, expr) in fields.iter() {
      let expr = self.infer_expr(scope, expr);
      let ty = self.ty_db.expr(&expr).ty();

      tys.insert(field.clone(), ty);
      typed_fields.insert(field.clone(), expr);
    }
    self.ty_db.alloc(TypedExpr::Object {
      fields: typed_fields,
      ty: Ty::Object(Some(tys)),
    })
  }

  fn infer_conditional(
    &mut self,
    scope: &mut Scope,
    condition: &DatabaseIdx,
    then_branch: &DatabaseIdx,
    else_branch: &DatabaseIdx,
  ) -> TypedDatabaseIdx {
    let condition = self.infer_expr(scope, condition);
    match self.ty_db.expr(&condition).ty() {
      Ty::Boolean(Some(boolean)) => {
        if boolean {
          self.infer_expr(scope, then_branch)
        } else {
          self.infer_expr(scope, else_branch)
        }
      }
      Ty::Boolean(None) => {
        let then_branch = self.infer_expr(scope, then_branch);
        let else_branch = self.infer_expr(scope, else_branch);
        let then_ty = self.ty_db.expr(&then_branch).ty();
        let else_ty = self.ty_db.expr(&else_branch).ty();

        if !then_ty.type_eq(&else_ty) {
          self.diagnostics.push_error(format!(
            "Type mismatch in conditional expression: {} and {}",
            then_ty, else_ty
          ));
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
          .push_error("Condition must be a boolean".to_string());
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

  fn infer_array(&mut self, scope: &mut Scope, vals: &[DatabaseIdx]) -> TypedDatabaseIdx {
    let (vals, tys): (Vec<TypedDatabaseIdx>, Vec<Ty>) = vals
      .iter()
      .map(|val| {
        let val = self.infer_expr(scope, val);
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

  fn infer_function_def(
    &mut self,
    scope: &mut Scope,
    name: &str,
    value: &DatabaseIdx,
  ) -> TypedStmt {
    let (params, body) = match self.hir_db.get_expr(value).clone() {
      Expr::Function {
        name: _,
        params,
        body,
      } => (params, body),
      _ => unreachable!("Function definition must be a function"),
    };

    let func_value = self.infer_function(scope, &Some(name.to_string()), &params, &body);
    scope.define(name.to_string(), func_value);

    TypedStmt::FunctionDef {
      name: name.to_string(),
      value: func_value,
    }
  }

  fn infer_function_call(
    &mut self,
    scope: &mut Scope,
    lhs: &DatabaseIdx,
    args: &[DatabaseIdx],
  ) -> TypedDatabaseIdx {
    let lhs = self.infer_expr(scope, lhs);
    let args = args
      .iter()
      .map(|arg| self.infer_expr(scope, arg))
      .collect::<Vec<_>>();
    self.infer_function_call_impl(scope, &lhs, &args)
  }

  fn infer_function_call_impl(
    &mut self,
    scope: &mut Scope,
    lhs: &TypedDatabaseIdx,
    args: &Vec<TypedDatabaseIdx>,
  ) -> TypedDatabaseIdx {
    let lhs_expr = self.ty_db.expr(lhs);

    // Need to try to travel through the callee expression to find a function definition
    // to invoke. A function definition can be returned from a variable reference, the
    // result of a function call, the end of a block, or just the function definition itself.
    match lhs_expr.clone() {
      TypedExpr::FunctionParameter { name: _, ty: _ } => *lhs,
      TypedExpr::VariableRef { var, ty: _ } => {
        if scope.is_late_binding(&var) {
          return *lhs;
        }
        match scope.def(&var) {
          Some(def) => self.infer_function_call_impl(scope, &def, args),
          None => {
            self
              .diagnostics
              .push_error(format!("Undefined variable `{}`", var));
            self.ty_db.alloc(TypedExpr::Error)
          }
        }
      }
      TypedExpr::FunctionCall {
        args: these_args,
        def,
        ret,
        ty: _,
      } => {
        if let TypedExpr::FunctionDef(func) = self.ty_db.expr(&def) {
          scope.push_frame();

          if these_args
            .iter()
            .any(|arg| self.ty_db.expr(arg).ty() == Ty::Generic)
          {
            return ret;
          }
          let mut params = FxIndexMap::default();
          for ((param, _), arg) in func.params.iter().zip(these_args.iter()) {
            params.insert(param.clone(), *arg);
          }
          scope.define_args(&params);
          let result = self.infer_function_call_impl(scope, &ret, args);
          scope.pop_frame();
          result
        } else {
          unreachable!()
        }
      }
      TypedExpr::FunctionDef(FunctionDef {
        name,
        params,
        body: _,
        body_hir,
        ty: _,
        closure_scope,
      }) => {
        if args.len() != params.len() {
          self.diagnostics.push_error(format!(
            "function `{}` expected {} arguments, but got {}",
            name.unwrap_or_default(),
            params.len(),
            args.len()
          ));
          return self.ty_db.alloc(TypedExpr::Error);
        }

        let scope = &mut closure_scope.extend_frames(scope);

        let params: FxIndexMap<String, TypedDatabaseIdx> = params
          .iter()
          .zip(args.iter().cloned())
          .map(|((name, _), arg)| (name.clone(), arg))
          .collect();

        scope.push_frame();
        scope.define_args(&params);
        let body = self.infer_expr(scope, &body_hir);
        let expr = TypedExpr::FunctionCall {
          args: params.clone().values().copied().collect(),
          ty: self.ty_db.expr(&body).ty(),
          ret: body,
          def: *lhs,
        };
        scope.pop_frame();
        self.ty_db.alloc(expr)
      }
      TypedExpr::Unresolved => *lhs,
      TypedExpr::ObjectFieldAccess {
        object,
        field,
        ty: _,
      } => match self.ty_db.expr(&object) {
        TypedExpr::VariableRef { var, ty } => match scope.def(var) {
          Some(def) => match self.ty_db.expr(&def) {
            TypedExpr::Object { fields, ty: _ } => {
              let field = *fields.get(&field).unwrap();
              self.infer_function_call_impl(scope, &field, args)
            }
            TypedExpr::Unresolved => *lhs,
            TypedExpr::VariableRef { .. } => *lhs,
            TypedExpr::FunctionParameter { .. } => *lhs,
            _ => {
              self.diagnostics.push_error(format!(
                "Cannot call field `{}` on non-object type `{}`",
                field, ty
              ));
              self.ty_db.alloc(TypedExpr::Error)
            }
          },
          None => {
            self
              .diagnostics
              .push_error(format!("Undefined variable `{}`", var));
            self.ty_db.alloc(TypedExpr::Error)
          }
        },
        TypedExpr::Object { fields, ty: _ } => match fields.get(&field) {
          Some(field) => {
            let field = *field;
            self.infer_function_call_impl(scope, &field, args)
          }
          None => {
            self
              .diagnostics
              .push_error(format!("Object does not have field `{}`", field));
            self.ty_db.alloc(TypedExpr::Error)
          }
        },
        _ => {
          self.diagnostics.push_error(format!(
            "Cannot call function on non-object. {} {:#?}",
            field, object
          ));
          self.ty_db.alloc(TypedExpr::Error)
        }
      },
      TypedExpr::Block { stmts, ty: _ } => {
        match stmts.last().map(|s| self.ty_db.expr(s.value()).ty()) {
          Some(Ty::Function { .. }) => {
            self.infer_function_call_impl(scope, stmts.last().unwrap().value(), args)
          }
          _ => {
            self
              .diagnostics
              .push_error("Cannot call `block` that does not return a function".to_string());
            self.ty_db.alloc(TypedExpr::Error)
          }
        }
      }
      // An error here means the user probably typo'd
      TypedExpr::Error => {
        self
          .diagnostics
          .push_error(format!("Cannot call `{:?}`", self.ty_db.expr(lhs)));
        self.ty_db.alloc(TypedExpr::Error)
      }
      _ => {
        self
          .diagnostics
          .push_error(format!("Cannot call `{:?}`", self.ty_db.expr(lhs)));
        self.ty_db.alloc(TypedExpr::Error)
      }
    }
  }

  /// Infers the definition of a function as well as its parameters.
  fn infer_function(
    &mut self,
    scope: &mut Scope,
    name: &Option<String>,
    params: &[String],
    body: &DatabaseIdx,
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

    let body_idx = self.infer_expr(scope, body);
    let body_ty = self.ty_db.expr(&body_idx).ty();
    let ty = Ty::Function {
      ret: Some(Box::new(body_ty)),
      params: params
        .iter()
        .map(|(_, v)| {
          let ty = self.ty_db.expr(v).ty();
          ty
        })
        .collect(),
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

  fn infer_unary(
    &mut self,
    scope: &mut Scope,
    op: &hir::UnaryOp,
    expr: &DatabaseIdx,
  ) -> TypedDatabaseIdx {
    let expr = self.infer_expr(scope, expr);
    let expr_ty = self.ty_db.expr(&expr).ty();
    let ty = match op {
      hir::UnaryOp::Neg => match expr_ty {
        Ty::Number(Some(n)) => Ty::Number(Some(-n)),
        Ty::Number(None) => Ty::Number(None),
        _ => {
          self.diagnostics.push_error(format!(
            "cannot apply unary operator `-` to type `{}`",
            expr_ty
          ));
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
        Ty::Generic => Ty::Boolean(None),
        _ => {
          self.diagnostics.push_error(format!(
            "cannot apply unary operator `!` to type `{}`",
            expr_ty
          ));
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
  // rustfmt makes the match block have inconsistent formatting
  #[rustfmt::skip]
  fn infer_binary(
    &mut self,
    scope: &mut Scope,
    op: &hir::BinaryOp,
    lhs_idx: &DatabaseIdx,
    rhs_idx: &DatabaseIdx,
  ) -> TypedDatabaseIdx {
    let lhs_idx = self.infer_expr(scope, lhs_idx);
    let rhs_idx = self.infer_expr(scope, rhs_idx);

    let (lhs, rhs) = self.ty_db.exprs2(&lhs_idx, &rhs_idx);

    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();
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

  fn infer_block(&mut self, scope: &mut Scope, stmts: &[Stmt]) -> TypedDatabaseIdx {
    scope.push_frame();
    let stmts = stmts
      .iter()
      .map(|stmt| self.infer_stmt(scope, Either::Left(stmt)))
      .collect::<Vec<_>>();
    let ty = if let Some(last) = stmts.last() {
      self.ty_db.expr(last.value()).ty()
    } else {
      Ty::Generic
    };
    scope.pop_frame();
    self.ty_db.alloc(TypedExpr::Block { stmts, ty })
  }

  fn query_object_name(&self, idx: &DatabaseIdx) -> Option<&str> {
    let expr = self.hir_db.get_expr(idx);
    let name = match expr {
      Expr::VariableRef { var, .. } => var,
      Expr::Object { .. } => "<anonymous>",
      _ => return None,
    };
    println!("name: {}", name);
    Some(name)
  }
}
