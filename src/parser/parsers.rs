use crate::lexer::token::SyntaxKind;
use crate::parser::expressions::expr_binding_power;
use crate::parser::marker::CompletedMarker;
use crate::parser::operators::PrefixOp;
use crate::parser::Parser;

pub(super) fn literal(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(SyntaxKind::Number));

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::Literal)
}

pub(super) fn variable_ref(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(SyntaxKind::Identifier));

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::VariableRef)
}

pub(super) fn prefix_expr(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(SyntaxKind::Minus));

  let m = p.start();

  let op = PrefixOp::Neg;
  let ((), right_binding_power) = op.binding_power();

  // Eat the operator’s token.
  p.bump();

  expr_binding_power(p, right_binding_power);

  m.complete(p, SyntaxKind::PrefixExpression)
}

pub(super) fn paren_expr(p: &mut Parser) -> CompletedMarker {
  assert!(p.at(SyntaxKind::LeftParenthesis));

  let m = p.start();

  p.bump();
  expr_binding_power(p, 0);

  assert!(p.at(SyntaxKind::RightParenthesis));
  p.bump();

  m.complete(p, SyntaxKind::ParenExpression)
}
