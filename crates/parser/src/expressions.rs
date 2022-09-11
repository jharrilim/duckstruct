use super::{marker::CompletedMarker, operators::InfixOp, parsers};
use crate::parser::Parser;
use lexer::token::TokenKind;
use syntax::SyntaxKind;

pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
  expr_binding_power(p, 0)
}

pub(super) fn expr_binding_power(
  p: &mut Parser,
  minimum_binding_power: u8,
) -> Option<CompletedMarker> {
  let mut lhs = lhs(p)?;

  loop {
    let op = match p.peek() {
      Some(TokenKind::Plus) => InfixOp::Add,
      Some(TokenKind::Minus) => InfixOp::Sub,
      Some(TokenKind::Asterisk) => InfixOp::Mul,
      Some(TokenKind::ForwardSlash) => InfixOp::Div,

      Some(TokenKind::LessThan) => InfixOp::Lt,
      Some(TokenKind::LessThanOrEqual) => InfixOp::Lte,
      Some(TokenKind::GreaterThan) => InfixOp::Gt,
      Some(TokenKind::GreaterThanOrEqual) => InfixOp::Gte,
      Some(TokenKind::DoubleEquals) => InfixOp::Eq,
      Some(TokenKind::NotEquals) => InfixOp::Neq,
      Some(TokenKind::And) => InfixOp::And,
      Some(TokenKind::Or) => InfixOp::Or,
      _ => break,
    };

    let (left_binding_power, right_binding_power) = op.binding_power();

    if left_binding_power < minimum_binding_power {
      break;
    }

    p.bump();

    let m = lhs.precede(p);
    expr_binding_power(p, right_binding_power);
    lhs = m.complete(p, SyntaxKind::InfixExpression);
  }
  Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
  let completed_marker = match p.peek() {
    Some(TokenKind::Number) => parsers::number_literal(p),
    Some(TokenKind::String) => parsers::string_literal(p),
    Some(TokenKind::LeftBrace) => todo!("object literal"),
    Some(TokenKind::LeftBracket) => todo!("array literal"),
    Some(TokenKind::Identifier) => parsers::variable_ref(p),
    Some(TokenKind::Minus) => parsers::prefix_expr(p),
    Some(TokenKind::LeftParenthesis) => parsers::paren_expr(p),
    Some(TokenKind::If) => parsers::conditional_expr(p),
    _ => return None,
  };
  Some(completed_marker)
}
