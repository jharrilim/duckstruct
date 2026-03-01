use lexer::token::TokenKind;

use super::expressions::expr;
use super::parsers;
use crate::{marker::CompletedMarker, parser::Parser};

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
  let r = match p.peek() {
    Some(TokenKind::Use) => Some(parsers::use_stmt(p)),
    Some(TokenKind::Pub) => match p.source.peek_next_kind() {
      Some(TokenKind::Let) => Some(parsers::let_stmt(p)),
      Some(TokenKind::Function) => Some(parsers::function_definition(p)),
      _ => {
        p.error();
        None
      }
    },
    Some(TokenKind::Let) => Some(parsers::let_stmt(p)),
    Some(TokenKind::Function) => Some(parsers::function_definition(p)),
    _ => expr(p),
  };

  if p.at(TokenKind::Semicolon) {
    p.bump();
  }

  r
}
