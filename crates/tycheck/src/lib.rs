use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};
use rustc_hash::FxHashMap;

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
  Generic,
  Error,
}

pub struct TyCheck {
  hir_db: hir::Database,
  ty_db: FxHashMap<String, Ty>,
}

impl TyCheck {
  pub fn new(hir_db: hir::Database) -> Self {
    Self {
      hir_db,
      ty_db: FxHashMap::default(),
    }
  }

  pub fn infer(&mut self) -> &FxHashMap<String, Ty> {
    for (stmt_ident, statement) in self.hir_db.defs_iter() {
      let ty = self.infer_stmt(Either::Left(statement));
      self.ty_db.insert(stmt_ident.clone(), ty);
    }
    &self.ty_db
  }

  pub fn infer_stmt(&self, stmt: Either<&Stmt, &DatabaseIdx>) -> Ty {
    let expr = match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name: _, value } => value,
        Stmt::Expr(expr) => expr,
      },
      Either::Right(idx) => self.hir_db.get_expr(idx),
    };
    self.infer_expr(expr)
  }

  pub fn infer_expr(&self, expr: &Expr) -> Ty {
    match expr {
      Expr::VariableRef { var } => {
        let ty = self.ty_db.get(var).unwrap_or(&Ty::Error);
        ty.clone()
      }
      Expr::Number { n } => Ty::Number(Some(*n)),
      Expr::String { s } => Ty::String(Some(s.clone())),
      Expr::Binary { op, lhs, rhs } => {
        let lhs = self.infer_expr(self.hir_db.get_expr(lhs));
        let rhs = self.infer_expr(self.hir_db.get_expr(rhs));

        match op {
          hir::expr::BinaryOp::Add => match (lhs, rhs) {
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
          hir::expr::BinaryOp::Sub => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Mul => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Div => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
        }
      }
      Expr::Missing => Ty::Error,
      _ => Ty::Generic,
    }
  }
}
