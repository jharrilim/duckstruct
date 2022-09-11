use lexer::token::TokenKind;
use syntax::SyntaxKind;

use crate::expressions::{expr, expr_binding_power};
use crate::marker::{CompletedMarker, Marker};
use crate::operators::PrefixOp;
use crate::parser::Parser;
use crate::statements;

// be nice if these returned a parse result instead

pub(super) fn literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Number);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::Literal)
}

pub(super) fn variable_ref(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Identifier);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::VariableReference)
}

pub(super) fn prefix_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Minus);

  let m = p.start();

  let op = PrefixOp::Neg;
  let ((), right_binding_power) = op.binding_power();

  // Eat the operator’s token.
  p.bump();

  expr_binding_power(p, right_binding_power);

  m.complete(p, SyntaxKind::PrefixExpression)
}

pub(super) fn paren_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftParenthesis);

  let m = p.start();

  p.bump();
  expr_binding_power(p, 0);

  p.expect(TokenKind::RightParenthesis);
  p.bump();

  m.complete(p, SyntaxKind::ParenExpression)
}

pub(super) fn let_stmt(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Let);

  let m = p.start();

  p.bump();

  match p.peek() {
    Some(TokenKind::Identifier) => p.bump(),
    Some(TokenKind::LeftBrace) => {
      struct_pattern_expr(p);
    }
    Some(TokenKind::LeftBracket) => {
      array_pattern_expr(p);
    }
    None | Some(_) => {
      p.error_expected_one_of(&[
        TokenKind::Identifier,
        TokenKind::LeftBrace,
        TokenKind::LeftBracket,
      ]);
    }
  }

  p.expect(TokenKind::Equals);
  p.bump();

  if expr(p).is_none() {
    p.error();
  }

  m.complete(p, SyntaxKind::LetStatement)
}

pub(crate) fn struct_pattern_expr(p: &mut Parser) -> CompletedMarker {
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
pub(crate) fn array_pattern_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftBracket);
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
  p.expect(TokenKind::Function);
  let m = p.start();
  p.bump();

  match p.peek() {
    Some(TokenKind::Identifier) => named_f(p, m),
    Some(TokenKind::LeftParenthesis) => anonymous_f(p, m),
    _ => panic!("press f"),
  }
}

fn named_f(p: &mut Parser, m: Marker) -> CompletedMarker {
  p.expect(TokenKind::Identifier);
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
  p.expect(TokenKind::LeftParenthesis);
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

fn function_body(p: &mut Parser) -> Option<CompletedMarker> {
  p.expect(TokenKind::LeftBrace);
  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      None => panic!("{}", "missing }"),
      Some(TokenKind::RightBrace) => {
        p.bump();
        return Some(m.complete(p, SyntaxKind::FunctionBody));
      }
      _ => {
        statements::stmt(p);
      }
    }
  }
}

pub(crate) fn conditional_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::If);
  let conditional_expr_marker = p.start();
  p.bump();
  {
    let m = p.start();
    expr(p);
    m.complete(p, SyntaxKind::ConditionalPredicate);
  }
  match p.peek() {
    Some(TokenKind::LeftBrace) => {
      p.bump();
      let m = p.start();
      expr(p);
      m.complete(p, SyntaxKind::IfCondition);
      p.expect(TokenKind::RightBrace);
      p.bump();
    }
    Some(t) => panic!("expected {{, found {}", t),
    None => panic!("unexpected end of file"),
  }
  if p.peek() == Some(TokenKind::Else) {
    p.bump();

    if p.peek() == Some(TokenKind::LeftBrace) {
      p.bump();

      let m = p.start();
      expr(p);
      m.complete(p, SyntaxKind::ElseCondition);

      p.expect(TokenKind::RightBrace);
      p.bump();
    } else {
      let m = p.start();
      expr(p);
      m.complete(p, SyntaxKind::ElseCondition);
    }
  }
  conditional_expr_marker.complete(p, SyntaxKind::ConditionalExpression)
}
