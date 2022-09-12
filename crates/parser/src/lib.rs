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
  use expect_test::{expect, Expect};

  fn check(input: &str, expected_tree: Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
  }

  #[test]
  fn parse_nothing() {
    check("", expect![[r#"Root@0..0"#]])
  }

  #[test]
  fn parse_number() {
    check(
      "123",
      expect![[r#"
          Root@0..3
            Number@0..3
              Number@0..3 "123""#]],
    );
  }

  #[test]
  fn parse_variable_ref() {
    check(
      "aVariableName",
      expect![[r#"
          Root@0..13
            VariableReference@0..13
              Identifier@0..13 "aVariableName""#]],
    );
  }

  #[test]
  fn parse_simple_infix_expression() {
    check(
      "1+2",
      expect![[r#"
          Root@0..3
            InfixExpression@0..3
              Number@0..1
                Number@0..1 "1"
              Plus@1..2 "+"
              Number@2..3
                Number@2..3 "2""#]],
    );
  }

  #[test]
  fn parse_negation() {
    check(
      "-10",
      expect![[r#"
          Root@0..3
            PrefixExpression@0..3
              Minus@0..1 "-"
              Number@1..3
                Number@1..3 "10""#]],
    );
  }

  #[test]
  fn parse_nested_parentheses() {
    check(
      "((((10))))",
      expect![[r#"
          Root@0..10
            ParenExpression@0..10
              LeftParenthesis@0..1 "("
              ParenExpression@1..9
                LeftParenthesis@1..2 "("
                ParenExpression@2..8
                  LeftParenthesis@2..3 "("
                  ParenExpression@3..7
                    LeftParenthesis@3..4 "("
                    Number@4..6
                      Number@4..6 "10"
                    RightParenthesis@6..7 ")"
                  RightParenthesis@7..8 ")"
                RightParenthesis@8..9 ")"
              RightParenthesis@9..10 ")""#]],
    );
  }

  #[test]
  fn parentheses_affect_precedence() {
    check(
      "5*(2+1)",
      expect![[r#"
          Root@0..7
            InfixExpression@0..7
              Number@0..1
                Number@0..1 "5"
              Asterisk@1..2 "*"
              ParenExpression@2..7
                LeftParenthesis@2..3 "("
                InfixExpression@3..6
                  Number@3..4
                    Number@3..4 "2"
                  Plus@4..5 "+"
                  Number@5..6
                    Number@5..6 "1"
                RightParenthesis@6..7 ")""#]],
    );
  }

  #[test]
  fn parse_number_preceded_by_whitespace() {
    check(
      "   9876",
      expect![[r#"
          Root@0..7
            Whitespace@0..3 "   "
            Number@3..7
              Number@3..7 "9876""#]],
    );
  }

  #[test]
  fn parse_number_followed_by_whitespace() {
    check(
      "999   ",
      expect![[r#"
          Root@0..6
            Number@0..6
              Number@0..3 "999"
              Whitespace@3..6 "   ""#]],
    );
  }

  #[test]
  fn parse_number_surrounded_by_whitespace() {
    check(
      " 123     ",
      expect![[r#"
          Root@0..9
            Whitespace@0..1 " "
            Number@1..9
              Number@1..4 "123"
              Whitespace@4..9 "     ""#]],
    );
  }

  #[test]
  fn parse_comment() {
    check(
      "// hello!",
      expect![[r#"
              Root@0..9
                Comment@0..9 "// hello!""#]],
    );
  }

  #[test]
  fn parse_let_statement() {
    check(
      "let x = 100;",
      expect![[r#"
          Root@0..12
            LetStatement@0..11
              Let@0..3 "let"
              Whitespace@3..4 " "
              Identifier@4..5 "x"
              Whitespace@5..6 " "
              Equals@6..7 "="
              Whitespace@7..8 " "
              Number@8..11
                Number@8..11 "100"
            Semicolon@11..12 ";""#]],
    )
  }

  #[test]
  fn parse_destructuring() {
    check(
      "let { x, y } = x;",
      expect![[r#"
          Root@0..17
            LetStatement@0..16
              Let@0..3 "let"
              Whitespace@3..4 " "
              StructPattern@4..13
                LeftBrace@4..5 "{"
                Whitespace@5..6 " "
                Identifier@6..7 "x"
                Comma@7..8 ","
                Whitespace@8..9 " "
                Identifier@9..10 "y"
                Whitespace@10..11 " "
                RightBrace@11..12 "}"
                Whitespace@12..13 " "
              Equals@13..14 "="
              Whitespace@14..15 " "
              VariableReference@15..16
                Identifier@15..16 "x"
            Semicolon@16..17 ";""#]],
    )
  }

  #[test]
  fn parse_destructuring_field_alias() {
    check(
      "let { x: asd, y } = z;",
      expect![[r#"
          Root@0..22
            LetStatement@0..21
              Let@0..3 "let"
              Whitespace@3..4 " "
              StructPattern@4..18
                LeftBrace@4..5 "{"
                Whitespace@5..6 " "
                Identifier@6..7 "x"
                Colon@7..8 ":"
                Whitespace@8..9 " "
                Identifier@9..12 "asd"
                Comma@12..13 ","
                Whitespace@13..14 " "
                Identifier@14..15 "y"
                Whitespace@15..16 " "
                RightBrace@16..17 "}"
                Whitespace@17..18 " "
              Equals@18..19 "="
              Whitespace@19..20 " "
              VariableReference@20..21
                Identifier@20..21 "z"
            Semicolon@21..22 ";""#]],
    )
  }

  #[test]
  fn parse_array_destructuring() {
    // might want to treat _ as a special ident at some point
    check(
      "let [_, y] = x;",
      expect![[r#"
          Root@0..15
            LetStatement@0..14
              Let@0..3 "let"
              Whitespace@3..4 " "
              ArrayPattern@4..11
                LeftBracket@4..5 "["
                Identifier@5..6 "_"
                Comma@6..7 ","
                Whitespace@7..8 " "
                Identifier@8..9 "y"
                RightBracket@9..10 "]"
                Whitespace@10..11 " "
              Equals@11..12 "="
              Whitespace@12..13 " "
              VariableReference@13..14
                Identifier@13..14 "x"
            Semicolon@14..15 ";""#]],
    )
  }

  #[test]
  fn parse_nested_patterns() {
    check(
      "let [{ x, }, z] = weel;",
      expect![[r#"
          Root@0..23
            LetStatement@0..22
              Let@0..3 "let"
              Whitespace@3..4 " "
              ArrayPattern@4..16
                LeftBracket@4..5 "["
                StructPattern@5..11
                  LeftBrace@5..6 "{"
                  Whitespace@6..7 " "
                  Identifier@7..8 "x"
                  Comma@8..9 ","
                  Whitespace@9..10 " "
                  RightBrace@10..11 "}"
                Comma@11..12 ","
                Whitespace@12..13 " "
                Identifier@13..14 "z"
                RightBracket@14..15 "]"
                Whitespace@15..16 " "
              Equals@16..17 "="
              Whitespace@17..18 " "
              VariableReference@18..22
                Identifier@18..22 "weel"
            Semicolon@22..23 ";""#]],
    )
  }

  #[test]
  fn parse_function_expression() {
    check(
      r#"
      f double(x) = x * 2;
    "#,
      expect![[r#"
          Root@0..32
            Whitespace@0..7 "\n      "
            NamedFunctionExpression@7..26
              Function@7..8 "f"
              Whitespace@8..9 " "
              Identifier@9..15 "double"
              ArgumentList@15..19
                LeftParenthesis@15..16 "("
                Identifier@16..17 "x"
                RightParenthesis@17..18 ")"
                Whitespace@18..19 " "
              Equals@19..20 "="
              Whitespace@20..21 " "
              InfixExpression@21..26
                VariableReference@21..23
                  Identifier@21..22 "x"
                  Whitespace@22..23 " "
                Asterisk@23..24 "*"
                Whitespace@24..25 " "
                Number@25..26
                  Number@25..26 "2"
            Semicolon@26..27 ";"
            Whitespace@27..32 "\n    ""#]],
    )
  }

  #[test]
  fn parse_anonymous_function_expression() {
    check(
      r#"
      f(x) = x / 2;
    "#,
      expect![[r#"
          Root@0..25
            Whitespace@0..7 "\n      "
            AnonymousFunctionExpression@7..19
              Function@7..8 "f"
              ArgumentList@8..12
                LeftParenthesis@8..9 "("
                Identifier@9..10 "x"
                RightParenthesis@10..11 ")"
                Whitespace@11..12 " "
              Equals@12..13 "="
              Whitespace@13..14 " "
              InfixExpression@14..19
                VariableReference@14..16
                  Identifier@14..15 "x"
                  Whitespace@15..16 " "
                ForwardSlash@16..17 "/"
                Whitespace@17..18 " "
                Number@18..19
                  Number@18..19 "2"
            Semicolon@19..20 ";"
            Whitespace@20..25 "\n    ""#]],
    )
  }

  #[test]
  fn parse_function() {
    check(
      r#"
      f upper(x) {
        let y = x * 5;
        y + 100
      }
    "#,
      expect![[r#"
          Root@0..71
            Whitespace@0..7 "\n      "
            NamedFunction@7..71
              Function@7..8 "f"
              Whitespace@8..9 " "
              Identifier@9..14 "upper"
              ArgumentList@14..18
                LeftParenthesis@14..15 "("
                Identifier@15..16 "x"
                RightParenthesis@16..17 ")"
                Whitespace@17..18 " "
              FunctionBody@18..71
                LeftBrace@18..19 "{"
                Whitespace@19..28 "\n        "
                LetStatement@28..41
                  Let@28..31 "let"
                  Whitespace@31..32 " "
                  Identifier@32..33 "y"
                  Whitespace@33..34 " "
                  Equals@34..35 "="
                  Whitespace@35..36 " "
                  InfixExpression@36..41
                    VariableReference@36..38
                      Identifier@36..37 "x"
                      Whitespace@37..38 " "
                    Asterisk@38..39 "*"
                    Whitespace@39..40 " "
                    Number@40..41
                      Number@40..41 "5"
                Semicolon@41..42 ";"
                Whitespace@42..51 "\n        "
                InfixExpression@51..65
                  VariableReference@51..53
                    Identifier@51..52 "y"
                    Whitespace@52..53 " "
                  Plus@53..54 "+"
                  Whitespace@54..55 " "
                  Number@55..65
                    Number@55..58 "100"
                    Whitespace@58..65 "\n      "
                RightBrace@65..66 "}"
                Whitespace@66..71 "\n    ""#]],
    )
  }

  #[test]
  fn parse_let_expression_followed_by_f_definition() {
    check(
      r#"
        let x = 10;
        f y(x) = x * 2;
      "#,
      expect![[r#"
          Root@0..51
            Whitespace@0..9 "\n        "
            LetStatement@9..19
              Let@9..12 "let"
              Whitespace@12..13 " "
              Identifier@13..14 "x"
              Whitespace@14..15 " "
              Equals@15..16 "="
              Whitespace@16..17 " "
              Number@17..19
                Number@17..19 "10"
            Semicolon@19..20 ";"
            Whitespace@20..29 "\n        "
            NamedFunctionExpression@29..43
              Function@29..30 "f"
              Whitespace@30..31 " "
              Identifier@31..32 "y"
              ArgumentList@32..36
                LeftParenthesis@32..33 "("
                Identifier@33..34 "x"
                RightParenthesis@34..35 ")"
                Whitespace@35..36 " "
              Equals@36..37 "="
              Whitespace@37..38 " "
              InfixExpression@38..43
                VariableReference@38..40
                  Identifier@38..39 "x"
                  Whitespace@39..40 " "
                Asterisk@40..41 "*"
                Whitespace@41..42 " "
                Number@42..43
                  Number@42..43 "2"
            Semicolon@43..44 ";"
            Whitespace@44..51 "\n      ""#]],
    );
  }

  #[test]
  fn parse_if_condition() {
    check(
      r#"
        if x > 0 {
          x + 1
        }
      "#,
      expect![[r#"
          Root@0..52
            Whitespace@0..9 "\n        "
            ConditionalExpression@9..52
              If@9..11 "if"
              Whitespace@11..12 " "
              ConditionalPredicate@12..18
                InfixExpression@12..18
                  VariableReference@12..14
                    Identifier@12..13 "x"
                    Whitespace@13..14 " "
                  GreaterThan@14..15 ">"
                  Whitespace@15..16 " "
                  Number@16..18
                    Number@16..17 "0"
                    Whitespace@17..18 " "
              LeftBrace@18..19 "{"
              Whitespace@19..30 "\n          "
              IfCondition@30..44
                InfixExpression@30..44
                  VariableReference@30..32
                    Identifier@30..31 "x"
                    Whitespace@31..32 " "
                  Plus@32..33 "+"
                  Whitespace@33..34 " "
                  Number@34..44
                    Number@34..35 "1"
                    Whitespace@35..44 "\n        "
              RightBrace@44..45 "}"
              Whitespace@45..52 "\n      ""#]],
    );
  }

  #[test]
  fn parse_if_condition_with_else() {
    check(
      r#"
        if x > 0 {
          x + 1
        } else {
          x - 1
        }
      "#,
      expect![[r#"
          Root@0..85
            Whitespace@0..9 "\n        "
            ConditionalExpression@9..85
              If@9..11 "if"
              Whitespace@11..12 " "
              ConditionalPredicate@12..18
                InfixExpression@12..18
                  VariableReference@12..14
                    Identifier@12..13 "x"
                    Whitespace@13..14 " "
                  GreaterThan@14..15 ">"
                  Whitespace@15..16 " "
                  Number@16..18
                    Number@16..17 "0"
                    Whitespace@17..18 " "
              LeftBrace@18..19 "{"
              Whitespace@19..30 "\n          "
              IfCondition@30..44
                InfixExpression@30..44
                  VariableReference@30..32
                    Identifier@30..31 "x"
                    Whitespace@31..32 " "
                  Plus@32..33 "+"
                  Whitespace@33..34 " "
                  Number@34..44
                    Number@34..35 "1"
                    Whitespace@35..44 "\n        "
              RightBrace@44..45 "}"
              Whitespace@45..46 " "
              Else@46..50 "else"
              Whitespace@50..51 " "
              LeftBrace@51..52 "{"
              Whitespace@52..63 "\n          "
              ElseCondition@63..77
                InfixExpression@63..77
                  VariableReference@63..65
                    Identifier@63..64 "x"
                    Whitespace@64..65 " "
                  Minus@65..66 "-"
                  Whitespace@66..67 " "
                  Number@67..77
                    Number@67..68 "1"
                    Whitespace@68..77 "\n        "
              RightBrace@77..78 "}"
              Whitespace@78..85 "\n      ""#]],
    );
  }

  #[test]
  fn parse_and_expression() {
    check(
      "x && y",
      expect![[r#"
          Root@0..6
            InfixExpression@0..6
              VariableReference@0..2
                Identifier@0..1 "x"
                Whitespace@1..2 " "
              And@2..4 "&&"
              Whitespace@4..5 " "
              VariableReference@5..6
                Identifier@5..6 "y""#]],
    );
  }

  #[test]
  fn parse_or_expression() {
    check(
      "x || y",
      expect![[r#"
          Root@0..6
            InfixExpression@0..6
              VariableReference@0..2
                Identifier@0..1 "x"
                Whitespace@1..2 " "
              Or@2..4 "||"
              Whitespace@4..5 " "
              VariableReference@5..6
                Identifier@5..6 "y""#]],
    );
  }

  #[test]
  fn parse_conditional_assignment() {
    // TODO: Add booleans
    check(
      "let x = if true { 1 } else { 2 }",
      expect![[r#"
          Root@0..32
            LetStatement@0..32
              Let@0..3 "let"
              Whitespace@3..4 " "
              Identifier@4..5 "x"
              Whitespace@5..6 " "
              Equals@6..7 "="
              Whitespace@7..8 " "
              ConditionalExpression@8..32
                If@8..10 "if"
                Whitespace@10..11 " "
                ConditionalPredicate@11..16
                  Boolean@11..16
                    Boolean@11..15 "true"
                    Whitespace@15..16 " "
                LeftBrace@16..17 "{"
                Whitespace@17..18 " "
                IfCondition@18..20
                  Number@18..20
                    Number@18..19 "1"
                    Whitespace@19..20 " "
                RightBrace@20..21 "}"
                Whitespace@21..22 " "
                Else@22..26 "else"
                Whitespace@26..27 " "
                LeftBrace@27..28 "{"
                Whitespace@28..29 " "
                ElseCondition@29..31
                  Number@29..31
                    Number@29..30 "2"
                    Whitespace@30..31 " "
                RightBrace@31..32 "}""#]],
    )
  }

  #[test]
  fn err_let_stmt() {
    let code = "let x =\nlet y = 1";
    let parsed = parse(code);
    assert_eq!(parsed.errors.len(), 1);
  }
}
