use la_arena::Idx;

#[derive(Debug)]
pub enum Expr {
  Binary {
    op: BinaryOp,
    lhs: Idx<Self>,
    rhs: Idx<Self>,
  },
  Literal {
    n: u64,
  },
  Unary {
    op: UnaryOp,
    expr: Idx<Self>,
  },
  VariableRef {
    var: String,
  },
  Missing,
}

#[derive(Debug)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Debug)]
pub enum UnaryOp {
  Neg,
}
