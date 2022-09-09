use lexer::token::TokenKind;

use super::expressions::expr;
use super::parsers::let_stmt;
use crate::{marker::CompletedMarker, parser::Parser};

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
  let r = match p.peek() {
    Some(TokenKind::Let) => Some(let_stmt(p)),
    _ => expr(p),
  };

  if p.at(TokenKind::Semicolon) {
    p.bump();
  }

  r
}
