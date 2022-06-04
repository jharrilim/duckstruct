use lexer::token::TokenKind;
use syntax::SyntaxKind;

use crate::expressions::{expr, expr_binding_power};
use crate::marker::CompletedMarker;
use crate::operators::PrefixOp;
use crate::parser::Parser;

use super::marker::Marker;

// be nice if these returned a parse result instead

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
      TokenKind::LeftBracket => {
        array_pattern_expr(p);
      }
      _ => {
        panic!("todo: parse error here {}", token)
      }
    }
    m.complete(p, SyntaxKind::Assignment);
  }

  debug_assert!(
    p.at(TokenKind::Equals),
    "token: {}",
    p.peek().map_or_else(|| "".to_string(), |t| t.to_string())
  );
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
        break m.complete(p, SyntaxKind::StructPattern);
      }
      Some(TokenKind::LeftBracket) => {
        array_pattern_expr(p);
      }
      Some(TokenKind::Identifier) => p.bump(),
      Some(TokenKind::Comma) => p.bump(),
      Some(TokenKind::Colon) => p.bump(),
      Some(token) => panic!(
        "wth dude you cant just put whatever character you want here {}",
        token
      ),
      None => panic!("{}", "Missing '}'"), // gotta try to get comprehensive error reporting in here at some point
    }
  }
}

/// array_pattern = [ ident|struct_pattern|array_pattern ,* ]
fn array_pattern_expr(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::LeftBracket));

  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      Some(TokenKind::RightBracket) => {
        p.bump();
        break m.complete(p, SyntaxKind::ArrayPattern);
      }
      Some(TokenKind::LeftBrace) => {
        struct_pattern_expr(p);
      }
      Some(TokenKind::Identifier) => p.bump(),
      Some(TokenKind::Comma) => p.bump(),
      Some(token) => panic!("aint no way bro 💀 {}", token),
      None => panic!("Missing ']'"),
    }
  }
}

pub(super) fn function_definition(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::Function));
  let m = p.start();
  p.bump();

  match p.peek() {
    Some(TokenKind::Identifier) => named_f(p, m),
    Some(TokenKind::LeftParenthesis) => anonymous_f(p, m),
    _ => panic!("press f"),
  }
}

fn named_f(p: &mut Parser, m: Marker) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::Identifier));
  p.bump();
  argument_list(p);
  match p.peek() {
    Some(TokenKind::Equals) => {
      p.bump();
      expr(p);
      m.complete(p, SyntaxKind::NamedFunctionExpression)
    }
    Some(TokenKind::LeftBrace) => {
      function_body(p);
      m.complete(p, SyntaxKind::NamedFunction)
    }
    _ => panic!("nooo"),
  }
}

fn anonymous_f(p: &mut Parser, m: Marker) -> CompletedMarker {
  argument_list(p);
  match p.peek() {
    Some(TokenKind::Equals) => {
      p.bump();
      expr(p);
      m.complete(p, SyntaxKind::AnonymousFunctionExpression)
    }
    Some(TokenKind::LeftBrace) => {
      function_body(p);
      m.complete(p, SyntaxKind::AnonymousFunction)
    }
    _ => panic!("nooo"),
  }
}

fn argument_list(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::LeftParenthesis));
  let m = p.start();
  p.bump();
  let mut last_token: Option<TokenKind> = None;

  loop {
    match p.peek() {
      Some(TokenKind::RightParenthesis) => {
        p.bump();
        break m.complete(p, SyntaxKind::ArgumentList);
      }
      Some(TokenKind::Identifier) => {
        if last_token == Some(TokenKind::Identifier) {
          panic!("u forgor comma 💀")
        }
        last_token = Some(TokenKind::Identifier);
        p.bump();
      }
      Some(TokenKind::Comma) => {
        if last_token == Some(TokenKind::Comma) {
          panic!(",,,,,sorry cant do that")
        }
        last_token = Some(TokenKind::Comma);
        p.bump();
      }
      _ => panic!("...?!"),
    }
  }
}

fn function_body(p: &mut Parser) -> CompletedMarker {
  debug_assert!(p.at(TokenKind::LeftBrace));
  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      None => panic!("{}", "missing }"),
      Some(TokenKind::RightBrace) => {
        p.bump();
        break m.complete(p, SyntaxKind::FunctionBody);
      }
      _ => expr(p),
    }
  }
}
