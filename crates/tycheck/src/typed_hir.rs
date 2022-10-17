use std::fmt::Display;

use hir::DatabaseIdx;
use rustc_hash::FxHashMap;

use crate::{scope::Scope, typed_db::TypedDatabaseIdx};

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
  Number(Option<f64>),
  String(Option<String>),
  Boolean(Option<bool>),
  Array(Option<Vec<Ty>>),
  Object(Option<FxHashMap<String, Ty>>),
  Function {
    params: Vec<Ty>,
    ret: Option<Box<Ty>>,
  },
  Generic,
  Error,
}

impl Ty {
  pub fn has_value(&self) -> bool {
    matches!(self, |Ty::Number(Some(_))| Ty::String(Some(_))
      | Ty::Boolean(Some(_))
      | Ty::Array(Some(_))
      | Ty::Object(Some(_))
      | Ty::Function {
        params: _,
        ret: Some(_)
      })
  }

  pub fn type_eq(&self, other: &Ty) -> bool {
    match (self, other) {
      (Ty::Number(_), Ty::Number(_)) => true,
      (Ty::String(_), Ty::String(_)) => true,
      (Ty::Boolean(_), Ty::Boolean(_)) => true,
      (Ty::Array(_), Ty::Array(_)) => true,
      (Ty::Object(_), Ty::Object(_)) => true,
      (
        Ty::Function { params, ret },
        Ty::Function {
          params: other_params,
          ret: other_ret,
        },
      ) => {
        if params.len() != other_params.len() {
          return false;
        }

        for (param, other_param) in params.iter().zip(other_params.iter()) {
          if !param.type_eq(other_param) {
            return false;
          }
        }

        match (ret, other_ret) {
          (Some(ret), Some(other_ret)) => ret.type_eq(other_ret),
          (None, None) => true,
          _ => false,
        }
      }
      _ => false,
    }
  }

  pub fn deconst(&self) -> Ty {
    match self {
      Ty::Number(_) => Ty::Number(None),
      Ty::String(_) => Ty::String(None),
      Ty::Boolean(_) => Ty::Boolean(None),
      Ty::Array(_) => Ty::Array(None),
      Ty::Object(_) => Ty::Object(None),
      Ty::Function { params, ret } => Ty::Function {
        params: params.iter().map(|p| p.deconst()).collect(),
        ret: match ret {
          Some(ret) => Some(Box::new(ret.deconst())),
          None => None,
        },
      },
      Ty::Generic => Ty::Generic,
      Ty::Error => Ty::Error,
    }
  }

  pub fn truthy(&self) -> Truthiness {
    match self {
      Ty::Number(Some(val)) => Truthiness::Known(*val != 0.0),
      Ty::String(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Boolean(Some(val)) => Truthiness::Known(*val),
      Ty::Array(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Object(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Function { .. } => Truthiness::Known(true),
      Ty::Generic => Truthiness::Unknown,
      Ty::Error => Truthiness::Unknown,
      _ => Truthiness::Unknown,
    }
  }
}

pub enum Truthiness {
  Known(bool),
  Unknown,
}

impl Display for Ty {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Ty::Number(Some(n)) => write!(f, "{}", n),
      Ty::String(Some(s)) => write!(f, "'{}'", s),
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
      Ty::Function { params, ret } => {
        write!(f, "(")?;
        for (i, ty) in params.iter().enumerate() {
          write!(f, "{}", ty)?;
          if i < params.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, ") -> ")?;
        if let Some(ret) = ret {
          write!(f, "{}", ret)
        } else {
          write!(f, "???")
        }
      }
      Ty::Number(None) => write!(f, "number"),
      Ty::String(None) => write!(f, "string"),
      Ty::Boolean(None) => write!(f, "boolean"),
      Ty::Array(None) => write!(f, "array"),
      Ty::Object(None) => write!(f, "object"),
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
    vals: Option<Vec<TypedDatabaseIdx>>,
    ty: Ty,
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
    stmts: Vec<TypedStmt>,
    ty: Ty,
  },
  FunctionDef {
    name: Option<String>,
    params: FxHashMap<String, TypedDatabaseIdx>,
    body: TypedDatabaseIdx,
    body_hir: DatabaseIdx,
    closure_scope: Scope,
    ty: Ty,
  },
  FunctionCall {
    args: Vec<TypedDatabaseIdx>,
    def: TypedDatabaseIdx,
    ret: TypedDatabaseIdx,
    ty: Ty,
  },
  Conditional {
    condition: TypedDatabaseIdx,
    then_branch: TypedDatabaseIdx,
    else_branch: TypedDatabaseIdx,
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
      Self::Array { vals: _, ty } => ty.clone(),
      Self::Binary { ty, .. } => ty.clone(),
      Self::Unary { ty, .. } => ty.clone(),
      Self::VariableRef { ty, .. } => ty.clone(),
      Self::Block { ty, .. } => ty.clone(),
      Self::FunctionDef { ty, .. } => ty.clone(),
      Self::FunctionCall { ty, .. } => ty.clone(),
      Self::Conditional { ty, .. } => ty.clone(),
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
  Neq,
}

impl Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BinaryOp::Add => write!(f, "+"),
      BinaryOp::Sub => write!(f, "-"),
      BinaryOp::Mul => write!(f, "*"),
      BinaryOp::Div => write!(f, "/"),
      BinaryOp::Eq => write!(f, "==="),
      BinaryOp::Neq => write!(f, "!=="),
    }
  }
}

impl From<&hir::BinaryOp> for BinaryOp {
  fn from(op: &hir::BinaryOp) -> Self {
    match op {
      hir::BinaryOp::Add => Self::Add,
      hir::BinaryOp::Sub => Self::Sub,
      hir::BinaryOp::Mul => Self::Mul,
      hir::BinaryOp::Div => Self::Div,
      hir::BinaryOp::Eq => Self::Eq,
      hir::BinaryOp::Neq => Self::Neq,
    }
  }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Neg,
  Not,
}

impl From<&hir::UnaryOp> for UnaryOp {
  fn from(op: &hir::UnaryOp) -> Self {
    match op {
      hir::UnaryOp::Neg => Self::Neg,
      hir::UnaryOp::Not => Self::Not,
    }
  }
}

impl Display for UnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      UnaryOp::Neg => write!(f, "-"),
      UnaryOp::Not => write!(f, "!"),
    }
  }
}
