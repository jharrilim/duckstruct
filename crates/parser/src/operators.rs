#[derive(PartialEq)]
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
  And,
  Or,
  LParen, // For function calls
}

impl InfixOp {
  pub fn binding_power(&self) -> (u8, u8) {
    match self {
      Self::Eq | Self::Neq => (1, 2),
      Self::And | Self::Or => (3, 4),
      Self::Lt | Self::Lte | Self::Gt | Self::Gte => (5, 6),
      Self::Add | Self::Sub => (7, 8),
      Self::Mul | Self::Div => (9, 10),
      Self::LParen => (11, 11),
    }
  }
}

pub(crate) enum PrefixOp {
  Neg,
  LParen,
}

impl PrefixOp {
  pub fn binding_power(&self) -> ((), u8) {
    match self {
      Self::Neg | Self::LParen => ((), 5),
    }
  }
}
