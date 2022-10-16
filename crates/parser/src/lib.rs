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
  use serde::Serialize;

  #[derive(Serialize)]
  struct ParseInfo {
    code: &'static str,
  }

  macro_rules! parse_snapshot {
    ($code:expr) => {
        let parse_info = ParseInfo { code: $code };
        let parse = parse($code);
        insta::with_settings!({
          info => &parse_info,
        }, {
          insta::assert_snapshot!(&parse.debug_tree());
        });
    };
  }

  #[test]
  fn parse_nothing() {
    let parse = parse("");
    assert_eq!(parse.debug_tree(), r#"Root@0..0"#);
  }

  #[test]
  fn parse_number() {
    let code = "123";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_variable_ref() {
    let code = "aVariableName";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_simple_infix_expression() {
    let code = "1+2";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_negation() {
    let code = "-10";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_nested_parentheses() {
    let code = "((((10))))";
    parse_snapshot!(code);
  }

  #[test]
  fn parentheses_affect_precedence() {
    let code = "5*(2+1)";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_number_preceded_by_whitespace() {
    let code = "   9876";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_number_followed_by_whitespace() {
    let code = "999   ";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_number_surrounded_by_whitespace() {
    let code = " 123     ";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_comment() {
    let code = "// hello!";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_boolean_literal_true() {
    let code = "true";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_boolean_literal_false() {
    let code = "false";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_string_literal() {
    let code = "\"hello\"";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_string_literal_empty() {
    let code = "\"\"";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_let_statement() {
    let code = "let x = 100;";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_destructuring() {
    let code = "let { x, y } = x;";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_destructuring_field_alias() {
    let code = "let { x: asd, y } = z;";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_array_destructuring() {
    // might want to treat _ as a special ident at some point
    let code = "let [_, y] = x;";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_nested_patterns() {
    let code = "let [{ x, }, z] = weel;";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_function_expression() {
    let code = r#"
      f double(x) = x * 2;
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_anonymous_function_expression() {
    let code = r#"
      f(x) = x / 2;
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_function() {
    let code = r#"
      f upper(x) {
        let y = x * 5;
        y + 100
      }
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_function_definition_with_multiple_arguments() {
    let code = r#"
      f add(x, y) = x + y;
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_let_expression_followed_by_f_definition() {
    let code = r#"
      let x = 10;
      f y(x) = x * 2;
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_if_condition() {
    let code = r#"
      if x > 0 {
        x + 1
      }
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_if_condition_with_else() {
    let code = r#"
      if x > 0 {
        x + 1
      } else {
        x - 1
      }
    "#;
    parse_snapshot!(code);
  }

  #[test]
  fn parse_and_expression() {
    let code = "x && y";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_or_expression() {
    let code = "x || y";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_conditional_assignment() {
    let code = "let x = if true { 1 } else { 2 }";
    parse_snapshot!(code);
  }

  #[test]
  fn err_let_stmt() {
    let code = "let x =\nlet y = 1";
    let parsed = parse(code);
    assert_eq!(parsed.errors.len(), 1);
  }

  #[test]
  fn parse_function_call() {
    let code = "foo()";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_function_call_with_argument() {
    let code = "foo(1)";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_immediately_invoked_function() {
    let code = "(f() = { 1 })()";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_curried_function_let_assignment() {
    let code = "let add = f(x) = f(y) = x + y";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_curried_function_invocation() {
    let code = "add(1)(2)";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_empty_array_expression() {
    let code = "[]";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_array_expression() {
    let code = "[1, 2, 3]";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_two_dimensional_array_expression() {
    let code = "[[1, 2], [3, 4]]";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_array_expression_with_trailing_comma() {
    let code = "[1, 2, 3,]";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_funky_array_expression() {
    let code = "[(1 + 4), \"h[e]llo\", { let x = 1; x + 1 }]";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_array_assignment() {
    let code = "let x = [1, 2, 3];";
    parse_snapshot!(code);
  }

  #[test]
  fn parse_array_assignment_with_destructuring() {
    let code = "let [x, y] = [1, 2];";
    parse_snapshot!(code);
  }
}
