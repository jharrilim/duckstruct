use lexer::token::TokenKind;
use lexer::Token;
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

  if let Some(token) = p.peek() {
    let m = p.start();
    match token {
      TokenKind::Identifier => p.bump(),
      TokenKind::LeftBrace => {
        struct_pattern_expr(p);
      }
      _ => {
        panic!("todo: parse error here {}", token.to_string())
      }
    }
    m.complete(p, SyntaxKind::Assignment);
  }

  debug_assert!(p.at(TokenKind::Equals), "token: {}", p.peek().map_or_else(|| "".to_string(), |t| t.to_string()));
  p.bump();

  expr_binding_power(p, 0);

  m.complete(p, SyntaxKind::LetExpression)
}

fn struct_pattern_expr(p: &mut Parser) -> CompletedMarker {
  let m = p.start();
  p.bump();
  loop {
    match p.peek() {
      Some(TokenKind::LeftBrace) => {
        struct_pattern_expr(p);
      }
      Some(TokenKind::RightBrace) => {
        p.bump();
        break m.complete(p, SyntaxKind::Pattern);
      }
      Some(TokenKind::LeftBracket) => {
        array_pattern_expr(p);
      }
      Some(TokenKind::Identifier) => p.bump(),
      Some(TokenKind::Comma) => p.bump(),
      Some(TokenKind::Colon) => p.bump(),
      Some(token) => panic!("wth dude you cant just put whatever character you want here {}", token.to_string()),
      None => panic!("{}", "Missing '}'"), // gotta try to get comprehensive error reporting in here at some point
    }
  }
}

/// array_pattern = [ ident|struct_pattern|array_pattern ,* ]
fn array_pattern_expr(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::LeftBracket));

  let m = p.start();

  loop {
    match p.peek() {
      Some(TokenKind::RightBracket) => break m.complete(p, SyntaxKind::ArrayPattern),
      None => panic!("Missing ']'"),
      _ => panic!("aint no way bro 💀"),
    }
  }
}
