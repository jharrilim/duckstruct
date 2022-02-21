use crate::lexer::token::SyntaxKind;
use crate::parser::Parser;

use super::operators::{InfixOp, PrefixOp};

pub(super) fn expr(p: &mut Parser) {
  expr_binding_power(p, 0)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
  let mut lhs = match p.peek() {
    Some(SyntaxKind::Number) => {
      let m = p.start();
      p.bump();
      m.complete(p, SyntaxKind::Literal)
    }
    Some(SyntaxKind::Identifier) => {
      let m = p.start();
      p.bump();
      m.complete(p, SyntaxKind::VariableRef)
    }
    Some(SyntaxKind::Minus) => {
      let m = p.start();

      let op = PrefixOp::Neg;
      let ((), right_binding_power) = op.binding_power();

      p.bump();
      expr_binding_power(p, right_binding_power);
      m.complete(p, SyntaxKind::PrefixExpression)
    }
    Some(SyntaxKind::LeftParenthesis) => {
      let m = p.start();

      p.bump();
      expr_binding_power(p, 0);
      assert_eq!(p.peek(), Some(SyntaxKind::RightParenthesis));
      p.bump();

      m.complete(p, SyntaxKind::ParenExpression)
    }
    _ => return,
  };

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

    let m = lhs.precede(p);
    expr_binding_power(p, right_binding_power);
    lhs = m.complete(p, SyntaxKind::BinaryExpression);
  }
}
