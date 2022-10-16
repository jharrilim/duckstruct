use self::parser::{Parse, Parser};
use lexer::Lexer;
use sink::Sink;

mod event;
mod expressions;
mod marker;
mod operators;
mod parse_error;
pub mod parser;
mod parsers;
mod sink;
mod source;
mod statements;

pub fn parse(input: &str) -> Parse {
  let tokens: Vec<_> = Lexer::new(input).collect();
  let parser = Parser::new(&tokens);
  let events = parser.parse();
  let sink = Sink::new(&tokens, events);

  sink.finish()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_nothing() {
    let parse = parse("");
    assert_eq!(parse.debug_tree(), r#"Root@0..0"#);
  }

  #[test]
  fn parse_number() {
    let parse = parse("123");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_variable_ref() {
    let parse = parse("aVariableName");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_simple_infix_expression() {
    let parse = parse("1+2");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_negation() {
    let parse = parse("-10");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_nested_parentheses() {
    let parse = parse("((((10))))");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parentheses_affect_precedence() {
    let parse = parse("5*(2+1)");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_number_preceded_by_whitespace() {
    let parse = parse("   9876");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_number_followed_by_whitespace() {
    let parse = parse("999   ");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_number_surrounded_by_whitespace() {
    let parse = parse(" 123     ");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_comment() {
    let parse = parse("// hello!");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_let_statement() {
    let parse = parse("let x = 100;");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_destructuring() {
    let parse = parse("let { x, y } = x;");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_destructuring_field_alias() {
    let parse = parse("let { x: asd, y } = z;");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_array_destructuring() {
    // might want to treat _ as a special ident at some point
    let parse = parse("let [_, y] = x;");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_nested_patterns() {
    let parse = parse("let [{ x, }, z] = weel;");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_function_expression() {
    let parse = parse(
      r#"
      f double(x) = x * 2;
    "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_anonymous_function_expression() {
    let parse = parse(
      r#"
      f(x) = x / 2;
    "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_function() {
    let parse = parse(
      r#"
      f upper(x) {
        let y = x * 5;
        y + 100
      }
    "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_function_definition_with_multiple_arguments() {
    let parse = parse(
      r#"
        f add(x, y) = x + y;
      "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_let_expression_followed_by_f_definition() {
    let parse = parse(
      r#"
        let x = 10;
        f y(x) = x * 2;
      "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_if_condition() {
    let parse = parse(
      r#"
        if x > 0 {
          x + 1
        }
      "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_if_condition_with_else() {
    let parse = parse(
      r#"
        if x > 0 {
          x + 1
        } else {
          x - 1
        }
      "#,
    );
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_and_expression() {
    let parse = parse("x && y");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_or_expression() {
    let parse = parse("x || y");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_conditional_assignment() {
    let parse = parse("let x = if true { 1 } else { 2 }");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn err_let_stmt() {
    let code = "let x =\nlet y = 1";
    let parsed = parse(code);
    assert_eq!(parsed.errors.len(), 1);
  }

  #[test]
  fn parse_function_call() {
    let parse = parse("foo()");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_function_call_with_argument() {
    let parse = parse("foo(1)");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_immediately_invoked_function() {
    let parse = parse("(f() = { 1 })()");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_curried_function_let_assignment() {
    let parse = parse("let add = f(x) = f(y) = x + y");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_curried_function_invocation() {
    let parse = parse("add(1)(2)");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_empty_array_expression() {
    let parse = parse("[]");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_array_expression() {
    let parse = parse("[1, 2, 3]");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_two_dimensional_array_expression() {
    let parse = parse("[[1, 2], [3, 4]]");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_array_expression_with_trailing_comma() {
    let parse = parse("[1, 2, 3,]");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_funky_array_expression() {
    let parse = parse("[(1 + 4), \"h[e]llo\", { let x = 1; x + 1 }]");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_array_assignment() {
    let parse = parse("let x = [1, 2, 3];");
    insta::assert_snapshot!(&parse.debug_tree());
  }

  #[test]
  fn parse_array_assignment_with_destructuring() {
    let parse = parse("let [x, y] = [1, 2];");
    insta::assert_snapshot!(&parse.debug_tree());
  }
}
