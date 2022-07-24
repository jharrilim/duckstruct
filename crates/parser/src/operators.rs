pub(crate) enum InfixOp {
  Add,
  Sub,
  Mul,
  Div,
  Lt,
  Lte,
  Gt,
  Gte,
  Eq,
  Neq,
  
}

impl InfixOp {
  pub fn binding_power(&self) -> (u8, u8) {
    match self {
      Self::Eq | Self::Neq => (1, 2),
      Self::Lt | Self::Lte | Self::Gt | Self::Gte => (3, 4),
      Self::Add | Self::Sub => (5, 6),
      Self::Mul | Self::Div => (7, 8),
    }
  }
}

pub(crate) enum PrefixOp {
  Neg,
}

impl PrefixOp {
  pub fn binding_power(&self) -> ((), u8) {
    match self {
      Self::Neg => ((), 5),
    }
  }
}
