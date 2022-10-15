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
  Boolean {
    b: bool,
  },
  Block {
    stmts: Vec<Stmt>,
  },
  Unary {
    op: UnaryOp,
    expr: Idx<Self>,
  },
  VariableRef {
    var: String,
  },
  Function {
    name: Option<String>,
    params: Vec<String>,
    body: Idx<Self>,
  },
  FunctionCall {
    name: Option<String>,
    args: Vec<Idx<Self>>,
  },
  Missing,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,

  Eq,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Neg,
  Not,
}
