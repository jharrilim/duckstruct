use la_arena::Idx;

use crate::Stmt;

#[derive(Debug, Clone)]
pub enum Expr {
  Binary {
    op: BinaryOp,
    lhs: Idx<Self>,
    rhs: Idx<Self>,
  },
  Number {
    n: f64,
  },
  String {
    s: String,
  },
  Unary {
    op: UnaryOp,
    expr: Idx<Self>,
  },
  VariableRef {
    var: String,
  },
  Block {
    stmts: Vec<Stmt>,
  },
  Missing,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Neg,
}
