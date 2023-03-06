/// This is where all of the parser's parslets live. Each parslet describes
/// the structure of a particular kind of expression or statement.
use lexer::token::TokenKind;
use syntax::SyntaxKind;

use crate::expressions::{expr, expr_binding_power};
use crate::marker::{CompletedMarker, Marker};
use crate::operators::PrefixOp;
use crate::parser::Parser;
use crate::statements;

pub(super) fn number_literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Number);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::Number)
}

pub(super) fn string_literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::String);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::String)
}

pub(super) fn boolean_literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Boolean);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::Boolean)
}

pub(super) fn variable_ref(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Identifier);

  let m = p.start();
  p.bump();
  m.complete(p, SyntaxKind::VariableReference)
}

pub(super) fn negation_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Minus);

  let m = p.start();

  let op = PrefixOp::Neg;
  let ((), right_binding_power) = op.binding_power();

  // Eat the operator’s token.
  p.bump();

  expr_binding_power(p, right_binding_power);

  m.complete(p, SyntaxKind::UnaryExpression)
}

pub(super) fn not_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Bang);

  let m = p.start();

  let op = PrefixOp::Not;
  let ((), right_binding_power) = op.binding_power();

  // Eat the operator’s token.
  p.bump();

  expr_binding_power(p, right_binding_power);

  m.complete(p, SyntaxKind::UnaryExpression)
}

pub(super) fn paren_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftParenthesis);

  let m = p.start();

  p.bump();

  let op = PrefixOp::LParen;
  let ((), right_binding_power) = op.binding_power();
  expr_binding_power(p, right_binding_power);

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
    _ => {
      p.error_expected_one_of(&[TokenKind::Identifier, TokenKind::LeftParenthesis]);
      m.complete(p, SyntaxKind::Error)
    },
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
      block_expr(p);
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
      block_expr(p);
      m.complete(p, SyntaxKind::AnonymousFunction)
    }
    _ => {
      p.error_expected_one_of(&[TokenKind::Equals, TokenKind::LeftBrace]);
      m.complete(p, SyntaxKind::AnonymousFunction)
    }
  }
}

pub(crate) fn argument_list(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftParenthesis);
  let m = p.start();
  p.bump();
  let mut on_expr = false;

  loop {
    match p.peek() {
      Some(TokenKind::RightParenthesis) => {
        p.bump();
        break m.complete(p, SyntaxKind::ArgumentList);
      }
      Some(TokenKind::Comma) => {
        if !on_expr {
          panic!(",,,,,sorry cant do that")
        }
        on_expr = false;
        p.bump();
      }
      _ => {
        if on_expr {
          panic!("u forgor comma 💀")
        }
        on_expr = true;
        expr(p);
      }
    }
  }
}

pub(crate) fn block_expr(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftBrace);
  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      None => panic!("{}", "missing }"),
      Some(TokenKind::RightBrace) => {
        p.bump();
        return m.complete(p, SyntaxKind::BlockExpression);
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

  let m = p.start();
  expr(p);
  m.complete(p, SyntaxKind::IfCondition);

  if p.peek() == Some(TokenKind::Else) {
    p.bump();
    let m = p.start();
    expr(p);
    m.complete(p, SyntaxKind::ElseCondition);
  }
  conditional_expr_marker.complete(p, SyntaxKind::ConditionalExpression)
}

pub(crate) fn array_literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::LeftBracket);
  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      Some(TokenKind::RightBracket) => {
        p.bump();
        break m.complete(p, SyntaxKind::ArrayExpression);
      }
      Some(TokenKind::Comma) => {
        p.bump();
      }
      _ => {
        expr(p);
      }
    }
  }
}

pub(crate) fn object_literal(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::DoubleLeftBrace);
  let m = p.start();
  p.bump();

  loop {
    match p.peek() {
      Some(TokenKind::DoubleRightBrace) => {
        p.bump();
        break m.complete(p, SyntaxKind::ObjectExpression);
      }
      None => {
        p.expected(TokenKind::DoubleRightBrace);
      }
      _ => {
        object_field(p);
        if p.peek() == Some(TokenKind::Comma) {
          p.bump();
        }
      }
    }
  }
}

fn object_field(p: &mut Parser) -> CompletedMarker {
  p.expect(TokenKind::Identifier);
  let m = p.start();
  {
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::ObjectFieldKey);
  }
  p.expect(TokenKind::Colon);
  p.bump();
  {
    let m = p.start();
    expr(p);
    m.complete(p, SyntaxKind::ObjectFieldValue);
  }
  m.complete(p, SyntaxKind::ObjectField)
}
