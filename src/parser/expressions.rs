use crate::lexer::token::SyntaxKind;
use crate::parser::Parser;

pub(super) fn expr(p: &mut Parser) {
  expr_binding_power(p, 0)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
  // Immediately start a checkpoint; this creates a branch
  let checkpoint = p.checkpoint();

  match p.peek() {
    // These are immediately e d i b l e
    Some(SyntaxKind::Number) | Some(SyntaxKind::Identifier) => p.bump(),
    Some(SyntaxKind::Minus) => {
      let op = PrefixOp::Neg;
      let ((), right_binding_power) = op.binding_power();

      p.bump();
      p.start_node_at(checkpoint, SyntaxKind::PrefixExpression);
      expr_binding_power(p, right_binding_power);
      p.finish_node();
    },
    Some(SyntaxKind::LeftParenthesis) => {
      p.bump();
      expr_binding_power(p, 0);
      assert_eq!(p.peek(), Some(SyntaxKind::RightParenthesis));
      p.bump();
    },
    _ => {}
  }

  loop {
    let op = match p.peek() {
      Some(SyntaxKind::Plus) => InfixOp::Add,
      Some(SyntaxKind::Minus) => InfixOp::Sub,
      Some(SyntaxKind::Asterisk) => InfixOp::Mul,
      Some(SyntaxKind::ForwardSlash) => InfixOp::Div,
      _ => return,
    };

    let (left_binding_power, right_binding_power) = op.binding_power();

    if left_binding_power < minimum_binding_power {
      return;
    }

    p.bump();

    p.start_node_at(checkpoint, SyntaxKind::BinaryExpression);
    expr_binding_power(p, right_binding_power);
    p.finish_node();
  }
}

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
