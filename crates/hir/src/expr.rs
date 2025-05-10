use data_structures::{arena::Idx, FxIndexMap};

use crate::{pat::Pat, Stmt};

#[derive(Debug, Clone)]
pub enum Expr {
  Binary {
    op: BinaryOp,
    lhs: Idx<Self>,
    rhs: Idx<Self>,
    ast: ast::expr::BinaryExpr,
  },
  Number {
    n: f64,
    ast: ast::expr::Number,
  },
  String {
    s: String,
    ast: ast::expr::Str,
  },
  Boolean {
    b: bool,
    ast: ast::expr::Boolean,
  },
  Block {
    stmts: Vec<Stmt>,
    ast: ast::expr::Block,
  },
  Unary {
    op: UnaryOp,
    expr: Idx<Self>,
    ast: ast::expr::UnaryExpr,
  },
  VariableRef {
    var: String,
    ast: ast::expr::VariableRef,
  },
  Function {
    name: Option<String>,
    params: Vec<String>,
    body: Idx<Self>,
    ast: ast::expr::Function,
  },
  FunctionCall {
    func: Idx<Self>,
    args: Vec<Idx<Self>>,
    ast: ast::expr::FunctionCall,
  },
  Array {
    vals: Vec<Idx<Self>>,
    ast: ast::expr::Array,
  },
  Conditional {
    condition: Idx<Self>,
    then_branch: Idx<Self>,
    else_branch: Idx<Self>,
    ast: ast::expr::Conditional,
  },
  Object {
    fields: FxIndexMap<String, Idx<Self>>,
    ast: ast::expr::Object,
  },
  ObjectFieldAccess {
    object: Idx<Self>,
    field: String,
    ast: ast::expr::ObjectFieldAccess,
  },
  For {
    binding: Pat,
    iterable: Idx<Self>,
    where_clause: Idx<Self>,
    body: Idx<Self>,
    pipe_pattern: Pat,
    ast: ast::expr::ForExpression,
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
