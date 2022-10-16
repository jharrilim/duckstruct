use std::rc::Rc;

use crate::diagnostics::Diagnostics;
use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};
use rustc_hash::FxHashMap;
use crate::scope::Scope;
use crate::typed_db::{TypedDatabase, TypedDatabaseIdx};
use crate::typed_hir::{Ty, TypedExpr, TypedStmt};

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
    let hir_db = self.hir_db.clone();
    let expr = hir_db.get_expr(expr_idx);
    let expr = match expr {
      Expr::VariableRef { var } => match scope.def(var) {
        Some(t) => TypedExpr::VariableRef {
          var: var.clone(),
          ty: self.ty_db.expr(&t).ty(),
        },
        None => TypedExpr::Error,
      },
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
      Expr::Conditional { condition, then_branch, else_branch } => {
        return self.infer_conditional(scope, condition, then_branch, else_branch)
      },
      Expr::Missing => {
        todo!("Handle expression missing: {:?}", expr);
      }
    };
    self.ty_db.alloc(expr)
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
      },
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
      _ =>  {
        self.diagnostics.push_error("Condition must be a boolean".to_string());
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
    args: &Vec<DatabaseIdx>,
  ) -> TypedDatabaseIdx {
    let lhs = self.infer_expr(scope, lhs);
    self.infer_function_call_impl(scope, &lhs, args)
  }

  fn infer_function_call_impl(
    &mut self,
    scope: &mut Scope,
    lhs: &TypedDatabaseIdx,
    args: &Vec<DatabaseIdx>,
  ) -> TypedDatabaseIdx {
    let lhs_expr = self.ty_db.expr(&lhs);

    match lhs_expr.clone() {
      TypedExpr::VariableRef { var, ty: _ } => match scope.def(&var) {
        Some(def) => self.infer_function_call_impl(scope, &def, args),
        None => {
          self
            .diagnostics
            .push_error(format!("Undefined variable `{}`", var));
          self.ty_db.alloc(TypedExpr::Error)
        }
      },
      TypedExpr::FunctionCall {
        args: _,
        def: _,
        ret,
        ty: _,
      } => self.infer_function_call_impl(scope, &ret, args),
      TypedExpr::FunctionDef {
        name,
        params,
        body: _,
        body_hir,
        ty: _,
        closure_scope,
      } => {
        if args.len() != params.len() {
          self.diagnostics.push_error(format!(
            "function `{}` expected {} arguments, but got {}",
            name.unwrap_or("".to_string()),
            params.len(),
            args.len()
          ));
          return self.ty_db.alloc(TypedExpr::Error);
        }
        let scope = &mut scope.extend_frames(&closure_scope);
        let args_iter = args.iter().map(|arg| self.infer_expr(scope, arg));
        let params: FxHashMap<String, TypedDatabaseIdx> = params
          .iter()
          .zip(args_iter)
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
      TypedExpr::Unresolved => {
        // TODO: Label the argument as one that must be callable
        scope.push_frame();
        let args_iter = args.iter().map(|arg| self.infer_expr(scope, arg));
        let args: Vec<TypedDatabaseIdx> = args_iter.collect();

        scope.push_frame();

        let expr = TypedExpr::FunctionCall {
          args,
          ty: Ty::Generic,
          // This is likely wrong, I feel this should be allocating a new TypedExpr::Unresolved
          ret: *lhs,
          def: *lhs,
        };
        self.ty_db.alloc(expr)
      }
      TypedExpr::Block { stmts, ty: _ } => {
        match stmts.last().and_then(|s| Some(self.ty_db.expr(s.value()).ty())) {
          Some(Ty::Function { .. }) => self.infer_function_call_impl(scope, stmts.last().unwrap().value(), args),
          _ => {
            self.diagnostics.push_error("Cannot call `block` that does not return a function".to_string());
            self.ty_db.alloc(TypedExpr::Error)
          }
        }
      }
      // An error here means the user probably typo'd
      TypedExpr::Error => {
        self
          .diagnostics
          .push_error(format!("Cannot call `{:?}`", self.ty_db.expr(&lhs)));
        self.ty_db.alloc(TypedExpr::Error)
      }
      _ => {
        self
          .diagnostics
          .push_error(format!("Cannot call `{:?}`", self.ty_db.expr(&lhs)));
        self.ty_db.alloc(TypedExpr::Error)
      }
    }
  }

  fn infer_function(
    &mut self,
    scope: &mut Scope,
    name: &Option<String>,
    params: &Vec<String>,
    body: &DatabaseIdx,
  ) -> TypedDatabaseIdx {
    let params: FxHashMap<String, TypedDatabaseIdx> = params
      .iter()
      .map(|name| {
        (
          name.clone(),
          scope
            .def(name)
            .unwrap_or_else(|| self.ty_db.alloc(TypedExpr::Unresolved)),
        )
      })
      .collect();

    scope.push_frame();
    scope.define_args(&params);
    let body_idx = self.infer_expr(scope, body);
    let body_ty = self.ty_db.expr(&body_idx).ty();

    let expr = TypedExpr::FunctionDef {
      name: name.clone(),
      params: params.clone(),
      body: body_idx,
      body_hir: *body,
      closure_scope: scope.clone(),
      ty: Ty::Function {
        ret: Some(Box::new(body_ty)),
        params: (0..params.len()).map(|_| Ty::Generic).collect(),
      },
    };
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
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),

        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::String(Some(lhs + &rhs)),
        (Ty::String(Some(_)), Ty::String(None) | Ty::Generic) => Ty::String(None),
        (Ty::String(_), Ty::String(_)) => Ty::String(None),

        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Array(Some([lhs, rhs].concat())),
        (Ty::Array(_), Ty::Array(_)) => Ty::Array(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Sub => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Mul => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Div => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
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
}
