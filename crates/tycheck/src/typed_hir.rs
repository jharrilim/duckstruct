use hir::DatabaseIdx;
use rustc_hash::FxHashMap;

use crate::{Stmt, typed_db::TypedDatabaseIdx};

#[derive(Debug, Clone)]
pub enum Ty {
  Number(Option<f64>),
  String(Option<String>),
  Boolean(Option<bool>),
  Array(Option<Vec<Ty>>),
  Object(Option<FxHashMap<String, Ty>>),
  Function(Option<Box<Ty>>),
  Generic,
  Error,
}

impl Ty {
  pub fn has_value(&self) -> bool {
    match self {
      Ty::Number(Some(_)) => true,
      Ty::String(Some(_)) => true,
      Ty::Boolean(Some(_)) => true,
      Ty::Array(Some(_)) => true,
      Ty::Object(Some(_)) => true,
      Ty::Function(Some(_)) => true,
      _ => false,
    }
  }
}


#[derive(Debug, Clone)]
pub enum TypedStmt {
  VariableDef {
    name: String,
    value: TypedDatabaseIdx,
  },
  FunctionDef {
    name: String,
    value: TypedDatabaseIdx,
  },
  Expr(TypedDatabaseIdx),
}

impl TypedStmt {
  pub fn value(&self) -> &TypedDatabaseIdx {
    match self {
      TypedStmt::VariableDef { value, .. } => value,
      TypedStmt::FunctionDef { value, .. } => value,
      TypedStmt::Expr(value) => value,
    }
  }
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
  Number {
    val: Option<f64>,
  },
  String {
    val: Option<String>,
  },
  Boolean {
    val: Option<bool>,
  },
  Array {
    val: Option<Vec<TypedDatabaseIdx>>,
  },

  Binary {
    op: BinaryOp,
    lhs: TypedDatabaseIdx,
    rhs: TypedDatabaseIdx,
    ty: Ty,
  },
  Unary {
    op: UnaryOp,
    expr: TypedDatabaseIdx,
    ty: Ty,
  },
  VariableRef {
    var: String,
    ty: Ty,
  },
  Block {
    stmts: Vec<Stmt>,
    ty: Ty,
  },
  FunctionDef {
    name: Option<String>,
    params: FxHashMap<String, TypedDatabaseIdx>,
    body: TypedDatabaseIdx,
    body_hir: DatabaseIdx,
    ty: Ty,
  },
  FunctionCall {
    name: Option<String>,
    args: Vec<TypedDatabaseIdx>,
    def: TypedDatabaseIdx,
    ty: Ty,
  },
  Unresolved,
  Error,
}

impl TypedExpr {
  pub fn ty(&self) -> Ty {
    match self {
      Self::Number { val } => Ty::Number(*val),
      Self::String { val } => Ty::String(val.as_ref().map(|s| s.clone())),
      Self::Boolean { val } => Ty::Boolean(*val),
      Self::Array { val } => todo!("Typed array expr"),
      Self::Binary { ty, .. } => ty.clone(),
      Self::Unary { ty, .. } => ty.clone(),
      Self::VariableRef { ty, .. } => ty.clone(),
      Self::Block { ty, .. } => ty.clone(),
      Self::FunctionDef { ty, .. } => ty.clone(),
      Self::FunctionCall { ty, .. } => ty.clone(),
      Self::Unresolved => Ty::Generic,
      Self::Error => Ty::Error,
    }
  }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Eq,
}

impl From<&hir::expr::BinaryOp> for BinaryOp {
  fn from(op: &hir::expr::BinaryOp) -> Self {
    match op {
      hir::expr::BinaryOp::Add => Self::Add,
      hir::expr::BinaryOp::Sub => Self::Sub,
      hir::expr::BinaryOp::Mul => Self::Mul,
      hir::expr::BinaryOp::Div => Self::Div,
      hir::expr::BinaryOp::Eq => Self::Eq,
    }
  }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Neg,
}
