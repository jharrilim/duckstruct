use std::fmt::Display;

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

impl Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BinaryOp::Add => write!(f, "+"),
      BinaryOp::Sub => write!(f, "-"),
      BinaryOp::Mul => write!(f, "*"),
      BinaryOp::Div => write!(f, "/"),
      BinaryOp::Eq => write!(f, "==="),
      BinaryOp::Neq => write!(f, "!=="),
      BinaryOp::Lt => write!(f, "<"),
      BinaryOp::Lte => write!(f, "<="),
      BinaryOp::Gt => write!(f, ">"),
      BinaryOp::Gte => write!(f, ">="),
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
      hir::BinaryOp::Lt => Self::Lt,
      hir::BinaryOp::Lte => Self::Lte,
      hir::BinaryOp::Gt => Self::Gt,
      hir::BinaryOp::Gte => Self::Gte,
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
