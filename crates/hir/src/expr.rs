use data_structures::{arena::Idx, FxIndexMap};

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
    func: Idx<Self>,
    args: Vec<Idx<Self>>,
  },
  Array {
    vals: Vec<Idx<Self>>,
  },
  Conditional {
    condition: Idx<Self>,
    then_branch: Idx<Self>,
    else_branch: Idx<Self>,
  },
  Object {
    fields: FxIndexMap<String, Idx<Self>>,
  },
  ObjectFieldAccess {
    object: Idx<Self>,
    field: String,
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
  Neq,
  Lt,
  Lte,
  Gt,
  Gte,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Neg,
  Not,
}
