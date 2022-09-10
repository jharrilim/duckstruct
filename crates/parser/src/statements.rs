use lexer::token::TokenKind;

use super::expressions::expr;
use super::parsers;
use crate::{marker::CompletedMarker, parser::Parser};

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
  let r = match p.peek() {
    Some(TokenKind::Let) => Some(parsers::let_stmt(p)),
    Some(TokenKind::Function) => Some(parsers::function_definition(p)),
    _ => expr(p),
  };

  if p.at(TokenKind::Semicolon) {
    p.bump();
  }

  r
}
