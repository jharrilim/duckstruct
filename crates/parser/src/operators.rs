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
  /// A higher binding power essentially means that the operator has
  /// lower precedence. Binding power can be thought of as "how closely the
  /// operator binds to its operands". An especially useful notion in the context
  /// of parse trees. If you want an operator to bind more tightly to its operand,
  /// that means you want it to be more "nested" in the parse tree.
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
  Not,
  LParen,
}

impl PrefixOp {
  pub fn binding_power(&self) -> ((), u8) {
    match self {
      Self::Not => ((), 12),
      Self::Neg | Self::LParen => ((), 5),
    }
  }
}
