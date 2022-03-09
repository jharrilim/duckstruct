use super::{marker::CompletedMarker, operators::InfixOp, parsers};
use crate::parser::Parser;
use lexer::token::TokenKind;
use syntax::SyntaxKind;

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
      Some(TokenKind::Plus) => InfixOp::Add,
      Some(TokenKind::Minus) => InfixOp::Sub,
      Some(TokenKind::Asterisk) => InfixOp::Mul,
      Some(TokenKind::ForwardSlash) => InfixOp::Div,
      _ => return,
    };

    let (left_binding_power, right_binding_power) = op.binding_power();

    if left_binding_power < minimum_binding_power {
      return;
    }

    p.bump();

    let m = lhs.precede(p);
    expr_binding_power(p, right_binding_power);
    lhs = m.complete(p, SyntaxKind::InfixExpression);
  }
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
  let completed_marker = match p.peek() {
    Some(TokenKind::Number) => parsers::literal(p),
    Some(TokenKind::Identifier) => parsers::variable_ref(p),
    Some(TokenKind::Minus) => parsers::prefix_expr(p),
    Some(TokenKind::LeftParenthesis) => parsers::paren_expr(p),
    _ => return None,
  };
  Some(completed_marker)
}
