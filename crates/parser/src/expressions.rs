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
      Some(TokenKind::LeftParenthesis) => InfixOp::LParen,
      Some(TokenKind::Period) => InfixOp::Dot,
      _ => break,
    };

    let (left_binding_power, right_binding_power) = op.binding_power();

    if left_binding_power < minimum_binding_power {
      break;
    }

    match op {
      InfixOp::LParen => {
        let m = lhs.precede(p);
        parsers::argument_list(p);
        lhs = m.complete(p, SyntaxKind::FunctionCallExpression);
      }
      InfixOp::Dot => {
        let m = lhs.precede(p);
        p.bump();
        p.expect(TokenKind::Identifier);
        {
          let m = p.start();
          p.bump();
          m.complete(p, SyntaxKind::ObjectFieldKey);
        }
        lhs = m.complete(p, SyntaxKind::ObjectFieldAccessExpression);
      }
      _ => {
        p.bump();
        if p.at_end() {
          p.error();
        } else {
          let m = lhs.precede(p);
          expr_binding_power(p, right_binding_power);
          lhs = m.complete(p, SyntaxKind::InfixExpression);
        }
      }
    }
  }
  Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
  let completed_marker = match p.peek() {
    Some(TokenKind::Number) => parsers::number_literal(p),
    Some(TokenKind::Boolean) => parsers::boolean_literal(p),
    Some(TokenKind::String) => parsers::string_literal(p),
    Some(TokenKind::DoubleLeftBrace) => parsers::object_literal(p),
    Some(TokenKind::LeftBrace) => parsers::block_expr(p),
    Some(TokenKind::LeftBracket) => parsers::array_literal(p),
    Some(TokenKind::Identifier) => parsers::variable_ref(p),
    Some(TokenKind::Minus) => parsers::negation_expr(p),
    Some(TokenKind::Bang) => parsers::not_expr(p),
    Some(TokenKind::LeftParenthesis) => parsers::paren_expr(p),
    Some(TokenKind::If) => parsers::conditional_expr(p),
    Some(TokenKind::Function) => parsers::function_definition(p),
    Some(TokenKind::For) => parsers::for_expr(p),
    None => return None,
    Some(TokenKind::Let) => return None, // handled in a parslet
    _ => {
      p.error();
      return None;
    }
  };
  Some(completed_marker)
}
