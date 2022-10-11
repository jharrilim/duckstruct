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
use typed_hir::{TypedExpr, TypedStmt, Ty};

#[derive(Debug)]
pub enum Either<A, B> {
  Left(A),
  Right(B),
}

#[derive(Debug)]
pub struct TyCheck {
  hir_db: Rc<hir::Database>,
  pub ty_db: TypedDatabase,
  #[allow(unused)]
  diagnostics: Diagnostics,
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
      let typed_stmt = self.infer_stmt(Either::Left(statement), &mut scope);
      self.ty_db.define(stmt_ident.clone(), typed_stmt);
    }
    println!("scope: {:#?}", scope);
  }

  pub fn infer_stmt(&mut self, stmt: Either<&Stmt, &DatabaseIdx>, scope: &mut Scope) -> TypedStmt {
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
          self.infer_function_def(scope, name, params.clone(), body)
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
          ty: self.ty_db.expr(&t).ty().clone(),
        },
        None => TypedExpr::Error,
      },
      Expr::Number { n } => TypedExpr::Number { val: Some(*n) },
      Expr::String { s } => TypedExpr::String {
        val: Some(s.clone()),
      },
      Expr::Block { stmts } => todo!("infer block"),
      Expr::Boolean { b } => TypedExpr::Boolean { val: Some(*b) },
      Expr::Binary { op, lhs, rhs } => return self.infer_binary(scope, op, lhs, rhs),
      Expr::Unary { op, expr } => todo!(),
      Expr::Function { name, params, body } => todo!(),
      Expr::FunctionCall { name, args } => todo!(),
      Expr::Missing => {
        todo!("wtf")
      }
    };
    self.ty_db.alloc(expr)
  }

  fn infer_function_def(
    &mut self,
    scope: &mut Scope,
    name: &str,
    params: Vec<String>,
    body: &DatabaseIdx,
  ) -> TypedStmt {
    scope.push_frame();

    let params: FxHashMap<String, TypedDatabaseIdx> = params
      .iter()
      .map(|name| (name.clone(), self.ty_db.alloc(TypedExpr::Unresolved)))
      .collect();

    scope.define_all(&params);
    let body = self.infer_expr(scope, body);
    let body_ty = self.ty_db.expr(&body).ty();
    let func_def = TypedStmt::FunctionDef {
      name: name.to_string(),
      value: self.ty_db.alloc(TypedExpr::FunctionDef {
        name: Some(name.to_string()),
        params,
        body,
        ty: if body_ty.has_value() { body_ty } else { Ty::Function(None) },
      }),
    };
    scope.pop_frame();
    func_def
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
    op: &hir::expr::BinaryOp,
    lhs_idx: &DatabaseIdx,
    rhs_idx: &DatabaseIdx,
  ) -> TypedDatabaseIdx {
    let lhs_idx = self.infer_expr(scope, lhs_idx);
    let rhs_idx = self.infer_expr(scope, rhs_idx);

    let (lhs, rhs) = self.ty_db.exprs2(&lhs_idx, &rhs_idx);

    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();
    let ty = match op {
      hir::expr::BinaryOp::Add => match (lhs_ty, rhs_ty) {
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
      hir::expr::BinaryOp::Sub => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::expr::BinaryOp::Mul => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::expr::BinaryOp::Div => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
        (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::expr::BinaryOp::Eq => todo!("eq"),
    };
    self.ty_db.alloc(TypedExpr::Binary { op: op.into(), lhs: lhs_idx, rhs: rhs_idx, ty })
  }
}
