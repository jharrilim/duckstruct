use std::fmt::Display;

use hir::DatabaseIdx;
use rustc_hash::FxHashMap;

use crate::{typed_db::TypedDatabaseIdx, Stmt};

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
    matches!(
      self,
      | Ty::Number(Some(_))
      | Ty::String(Some(_))
      | Ty::Boolean(Some(_))
      | Ty::Array(Some(_))
      | Ty::Object(Some(_))
      | Ty::Function(Some(_))
    )
  }
}

impl Display for Ty {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Ty::Number(Some(n)) => write!(f, "{}", n),
      Ty::String(Some(s)) => write!(f, "{}", s),
      Ty::Boolean(Some(b)) => write!(f, "{}", b),
      Ty::Array(Some(a)) => {
        write!(f, "[")?;
        for (i, ty) in a.iter().enumerate() {
          write!(f, "{}", ty)?;
          if i < a.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, "]")
      }
      Ty::Object(Some(o)) => {
        write!(f, "{{")?;
        for (i, (key, ty)) in o.iter().enumerate() {
          write!(f, "{}: {}", key, ty)?;
          if i < o.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, "}}")
      }
      Ty::Function(Some(ty)) => write!(f, "f () -> {}", ty),
      Ty::Number(None) => write!(f, "number"),
      Ty::String(None) => write!(f, "string"),
      Ty::Boolean(None) => write!(f, "boolean"),
      Ty::Array(None) => write!(f, "array"),
      Ty::Object(None) => write!(f, "object"),
      Ty::Function(None) => write!(f, "function"),
      Ty::Generic => write!(f, "generic"),
      Ty::Error => write!(f, "error"),
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
      Self::String { val } => Ty::String(val.as_ref().cloned()),
      Self::Boolean { val } => Ty::Boolean(*val),
      Self::Array { val: _ } => todo!("Typed array expr"),
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