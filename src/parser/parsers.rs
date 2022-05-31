use lexer::token::TokenKind;
use syntax::SyntaxKind;

use crate::parser::expressions::expr_binding_power;
use crate::parser::marker::CompletedMarker;
use crate::parser::operators::PrefixOp;
use crate::parser::Parser;

pub(super) fn literal(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(TokenKind::Number));

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::Literal)
}

pub(super) fn variable_ref(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(TokenKind::Identifier));

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::VariableReference)
}

pub(super) fn prefix_expr(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(TokenKind::Minus));

  let m = p.start();

  let op = PrefixOp::Neg;
  let ((), right_binding_power) = op.binding_power();

  // Eat the operator’s token.
  p.bump();

  expr_binding_power(p, right_binding_power);

  m.complete(p, SyntaxKind::PrefixExpression)
}

pub(super) fn paren_expr(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(TokenKind::LeftParenthesis));

  let m = p.start();

  p.bump();
  expr_binding_power(p, 0);

  assert!(p.at(TokenKind::RightParenthesis));
  p.bump();

  m.complete(p, SyntaxKind::ParenExpression)
}

pub(super) fn let_expr(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(TokenKind::Let));

  let m = p.start();

  p.bump();

  pattern_expr(p);

  assert!(p.at(TokenKind::Equals));
  p.bump();

  expr_binding_power(p, 0);

  m.complete(p, SyntaxKind::LetExpression)
}

pub(super) fn pattern_expr(p: &mut Parser) -> CompletedMarker {
  if let Some(token) = p.peek() {
    let m = p.start();
    match token {
      TokenKind::Identifier => p.bump(),
      TokenKind::LeftBrace => todo!("implement destructuring i guess"),
      _ => {
        panic!("todo: parse error here {}", token.to_string())
      }
    }
    return m.complete(p, SyntaxKind::Pattern);
  }
  panic!("todo: parse error here")
}