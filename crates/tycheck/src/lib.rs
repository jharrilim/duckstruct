pub mod diagnostics;
pub mod scope;
pub mod typed_db;
pub mod typed_hir;

use std::rc::Rc;

use diagnostics::Diagnostics;
use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};
use rustc_hash::FxHashMap;
use scope::Scope;
use typed_db::{TypedDatabase, TypedDatabaseIdx};
use typed_hir::{Ty, TypedExpr, TypedStmt};

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
        Stmt::FunctionDef { name, params, body } => {
          self.infer_function_def(scope, name, params, body)
        }
        Stmt::Expr(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx)),
      },
      Either::Right(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx)),
    }
  }

  pub fn infer_expr(&mut self, scope: &mut Scope, expr_idx: &DatabaseIdx) -> TypedDatabaseIdx {
    let hir_db = self.hir_db.clone();
    let expr = match hir_db.get_expr(expr_idx) {
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
      Expr::Function { name, params, body } => todo!("function declaration expressions"),
      Expr::FunctionCall { name: None, args } => todo!("anonymous function invocations"),
      Expr::FunctionCall {
        name: Some(name),
        args,
      } => return self.infer_function_call(scope, name, args),
      Expr::Array { vals } => return self.infer_array(scope, vals),
      Expr::Missing => {
        todo!("Handle expression missing")
      }
    };
    self.ty_db.alloc(expr)
  }

  fn infer_array(
    &mut self,
    scope: &mut Scope,
    vals: &[DatabaseIdx],
  ) -> TypedDatabaseIdx {
    let (vals, tys): (Vec<TypedDatabaseIdx>, Vec<Ty>) = vals
      .iter()
      .map(|val| {
        let val = self.infer_expr(scope, val);
        let val_ty = self.ty_db.expr(&val).ty();
        (val, val_ty)
      })
      .unzip();

    let ty = Ty::Array(Some(tys));
    let expr = TypedExpr::Array { vals: Some(vals), ty };
    self.ty_db.alloc(expr)
  }

  fn infer_function_def(
    &mut self,
    scope: &mut Scope,
    name: &str,
    params: &[String],
    body: &DatabaseIdx,
  ) -> TypedStmt {
    scope.push_frame();

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

    scope.define_args(&params);
    let body_idx = self.infer_expr(scope, body);
    let body_ty = self.ty_db.expr(&body_idx).ty();
    let func_def = TypedStmt::FunctionDef {
      name: name.to_string(),
      value: self.ty_db.alloc(TypedExpr::FunctionDef {
        name: Some(name.to_string()),
        params,
        body: body_idx,
        body_hir: *body,
        ty: if body_ty.has_value() {
          body_ty
        } else {
          Ty::Function(None)
        },
      }),
    };
    scope.pop_frame();
    func_def
  }

  fn infer_function_call(
    &mut self,
    scope: &mut Scope,
    name: &str,
    args: &Vec<DatabaseIdx>,
  ) -> TypedDatabaseIdx {
    match self.ty_db.definition(name).cloned() {
      Some(TypedStmt::FunctionDef { name: _, value }) => {
        if let TypedExpr::FunctionDef {
          name: Some(name),
          params,
          body: _,
          body_hir,
          ty: _,
        } = self.ty_db.expr(&value).clone()
        {
          if args.len() != params.len() {
            self.diagnostics.push_error(format!(
              "function `{}` expected {} arguments, but got {}",
              name,
              params.len(),
              args.len()
            ));
            return self.ty_db.alloc(TypedExpr::Error);
          }
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
            name: Some(name),
            args: params.clone().values().copied().collect(),
            ty: self.ty_db.expr(&body).ty(),
            def: value,
          };
          scope.pop_frame();
          self.ty_db.alloc(expr)
        } else {
          // invariant: TypedStmt::FunctionDef's should always have a value that points to a TypedExpr::FunctionDef
          unreachable!()
        }
      }
      // An error here means the user probably typo'd
      _ => {
        self
          .diagnostics
          .push_error(format!("function `{}` not found", name));
        self.ty_db.alloc(TypedExpr::Error)
      }
    }
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
      hir::UnaryOp::Neg => {
        match expr_ty {
          Ty::Number(Some(n)) => Ty::Number(Some(-n)),
          Ty::Number(None) => Ty::Number(None),
          _ => {
            self.diagnostics.push_error(format!(
              "cannot apply unary operator `-` to type `{}`",
              expr_ty
            ));
            Ty::Error
          }
        }
      }
      hir::UnaryOp::Not => {
        match expr_ty {
          Ty::Boolean(Some(b)) => Ty::Boolean(Some(!b)),
          Ty::Boolean(None) => Ty::Boolean(None),
          _ => {
            self.diagnostics.push_error(format!(
              "cannot apply unary operator `!` to type `{}`",
              expr_ty
            ));
            Ty::Error
          }
        }
      }
    };
    self.ty_db.alloc(TypedExpr::Unary { op: op.into(), expr, ty })
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
        (Ty::Function(Some(lhs)), Ty::Function(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Function(_), Ty::Function(_)) => Ty::Boolean(None),
        (Ty::Generic, Ty::Generic) => Ty::Boolean(None),
        _ => Ty::Generic,
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
