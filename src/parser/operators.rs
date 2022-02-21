pub(crate) enum InfixOp {
  Add,
  Sub,
  Mul,
  Div,
}

impl InfixOp {
  pub fn binding_power(&self) -> (u8, u8) {
    match self {
      Self::Add | Self::Sub => (1, 2),
      Self::Mul | Self::Div => (3, 4),
    }
  }
}

pub(crate) enum PrefixOp {
  Neg,
}

impl PrefixOp {
  fn binding_power(&self) -> ((), u8) {
    match self {
      Self::Neg => ((), 5),
    }
  }
}
