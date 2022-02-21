use super::{marker::CompletedMarker, operators::InfixOp, parsers};
use crate::lexer::token::SyntaxKind;
use crate::parser::Parser;

pub(super) fn expr(p: &mut Parser) {
  expr_binding_power(p, 0)
}

pub(super) fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
  let mut lhs = if let Some(lhs) = lhs(p) {
    lhs
  } else {
    return;
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

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
  let completed_marker = match p.peek() {
    Some(SyntaxKind::Number) => parsers::literal(p),
    Some(SyntaxKind::Identifier) => parsers::variable_ref(p),
    Some(SyntaxKind::Minus) => parsers::prefix_expr(p),
    Some(SyntaxKind::LeftParenthesis) => parsers::paren_expr(p),
    _ => return None,
  };
  Some(completed_marker)
}
