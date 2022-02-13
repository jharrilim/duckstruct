use crate::lexer::token::SyntaxKind;
use crate::parser::Parser;

pub(super) fn expr(p: &mut Parser) {
  expr_binding_power(p, 0)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
  match p.peek() {
    Some(SyntaxKind::Number) | Some(SyntaxKind::Identifier) => p.bump(),
    _ => {}
  }

  loop {
    let op = match p.peek() {
      Some(SyntaxKind::Plus) => Op::Add,
      Some(SyntaxKind::Minus) => Op::Sub,
      Some(SyntaxKind::Asterisk) => Op::Mul,
      Some(SyntaxKind::ForwardSlash) => Op::Div,
      _ => return,
    };

    let (left_binding_power, right_binding_power) = op.binding_power();

    if left_binding_power < minimum_binding_power {
      return;
    }

    p.bump();

    p.start_node_at(p.checkpoint(), SyntaxKind::BinaryOperation);
    expr_binding_power(p, right_binding_power);
    p.finish_node();
  }
}

pub(crate) enum Op {
  Add,
  Sub,
  Mul,
  Div,
}

impl Op {
  pub fn binding_power(&self) -> (u8, u8) {
    match self {
      Self::Add | Self::Sub => (1, 2),
      Self::Mul | Self::Div => (3, 4),
    }
  }
}
