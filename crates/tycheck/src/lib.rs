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
use typed_hir::{TypedExpr, TypedStmt};

#[derive(Debug)]
pub enum Either<A, B> {
  Left(A),
  Right(B),
}

#[derive(Debug, Clone)]
pub enum Ty {
  Number(Option<f64>),
  String(Option<String>),
  Boolean(Option<bool>),
  Array(Option<Vec<Ty>>),
  Object(Option<FxHashMap<String, Ty>>),
  Generic,
  Error,
}

pub struct TyCheck {
  hir_db: Rc<hir::Database>,
  ty_db: TypedDatabase,
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
      let ty = self.infer_stmt(Either::Left(statement), &mut scope);
      self.ty_db.define(stmt_ident.clone(), ty);
    }
  }

  pub fn infer_stmt(&mut self, stmt: Either<&Stmt, &DatabaseIdx>, scope: &mut Scope) -> TypedStmt {
    match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name, value } => TypedStmt::VariableDef {
          name: name.clone(),
          value: self.infer_expr(scope, value),
        },
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
          ty: self.ty_db.expr(t).ty().clone(),
        },
        None => TypedExpr::Error,
      },
      Expr::Number { n } => TypedExpr::Number { val: Some(*n) },
      Expr::String { s } => TypedExpr::String {
        val: Some(s.clone()),
      },
      Expr::Block { stmts } => todo!("infer block"),
      Expr::Boolean { b } => TypedExpr::Boolean { val: Some(*b) },
      Expr::Binary { op, lhs, rhs } => self.infer_binary(scope, op, lhs, rhs),
      Expr::Unary { op, expr } => todo!(),
      Expr::Function { name, params, body } => todo!(),
      Expr::FunctionCall { name, args } => todo!(),
      Expr::Missing => TypedExpr::Error,
    };
    self.ty_db.alloc(expr)
  }

  fn infer_function_def(
    &self,
    scope: &mut Scope,
    name: &str,
    params: Vec<String>,
    body: &DatabaseIdx,
  ) -> TypedStmt {
    scope.push_frame();

    let params: FxHashMap<String, Ty> = params
      .iter()
      .map(|name| (name.clone(), Ty::Generic))
      .collect();

    // TypedStmt::FunctionDef { name: name.to_string(), params: (), body: () };
    scope.pop_frame();
    todo!("infer function def")
  }

  // rustfmt makes the match block have inconsistent formatting
  #[rustfmt::skip]
  fn infer_binary(
    &mut self,
    scope: &mut Scope,
    op: &hir::expr::BinaryOp,
    lhs: &DatabaseIdx,
    rhs: &DatabaseIdx,
  ) -> TypedExpr {
    let lhs = self.infer_expr(scope, lhs);
    let rhs = self.infer_expr(scope, rhs);

    let (lhs, rhs) = self.ty_db.exprs2(&lhs, &rhs);

    match op {
      hir::expr::BinaryOp::Add => match (lhs, rhs) {
        // Numbers
        (TypedExpr::Number { val: Some(lhs) }, TypedExpr::Number { val: Some(rhs) }) => {
          TypedExpr::Number { val: Some(lhs + rhs) }
        }
        (TypedExpr::Number { val: None } | TypedExpr::Unresolved, TypedExpr::Number { val: _ }) => {
          TypedExpr::Number { val: None }
        }
        (TypedExpr::Number { val: _ }, TypedExpr::Number { val: None } | TypedExpr::Unresolved) => {
          TypedExpr::Number { val: None }
        }

        // Strings
        (TypedExpr::String { val: Some(lhs) }, TypedExpr::String { val: Some(rhs) }) => {
          TypedExpr::String { val: Some([lhs.as_str(), rhs.as_str()].concat()) }
        }
        (TypedExpr::String { val: _ }, TypedExpr::String { val: None } | TypedExpr::Unresolved) => {
          TypedExpr::String { val: None }
        }
        (TypedExpr::String { val: None } | TypedExpr::Unresolved, TypedExpr::String { val: _ }) => {
          TypedExpr::String { val: None }
        }

        // Arrays
        (TypedExpr::Array { val: Some(lhs) }, TypedExpr::Array { val: Some(rhs) }) => {
          TypedExpr::Array { val: Some([lhs.clone(), rhs.clone()].concat()) }
        }

        (TypedExpr::Unresolved, TypedExpr::Unresolved) => TypedExpr::Unresolved,
        _ => todo!("Other builtin binary ops")
      },
      hir::expr::BinaryOp::Sub => todo!("Subtraction op"),
      hir::expr::BinaryOp::Mul => todo!("Multiplication op"),
      hir::expr::BinaryOp::Div => todo!("Division op"),
      hir::expr::BinaryOp::Eq => todo!("Equality op"),
    }
  }
}
