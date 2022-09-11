use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};

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
}

pub struct TyCheck {
  hir_db: hir::Database,
  statements: Vec<Stmt>,
}

impl TyCheck {
  pub fn new(hir_db: hir::Database, statements: Vec<Stmt>) -> Self {
    Self { hir_db, statements }
  }

  pub fn check(&self) -> Vec<Ty> {
    let mut tys = Vec::<Ty>::new();
    for statement in self.statements.iter() {
      tys.push(self.check_statement(Either::Left(statement)));
    }
    tys
  }

  pub fn check_statement(&self, stmt: Either<&Stmt, &DatabaseIdx>) -> Ty {
    let expr = match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name: _, value } => value,
        Stmt::Expr(expr) => expr,
      },
      Either::Right(idx) => self.hir_db.get(idx),
    };
    self.check_expression(expr)
  }

  pub fn check_expression(&self, expr: &Expr) -> Ty {
    match expr {
      Expr::VariableRef { var: _ } => Ty::Generic,
      Expr::Number { n } => Ty::Number(Some(*n)),
      Expr::String { s } => Ty::String(Some(s.clone())),
      Expr::Binary { op, lhs, rhs } => {
        let lhs = self.check_expression(self.hir_db.get(lhs));
        let rhs = self.check_expression(self.hir_db.get(rhs));
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
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Mul => match (lhs, rhs) {
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Div => match (lhs, rhs) {
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
        }
      }
      _ => Ty::Generic,
    }
  }
}
