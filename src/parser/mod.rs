mod event;
mod expressions;
mod marker;
mod operators;
mod parse_error;
mod parsers;
mod sink;
mod source;

use lexer::{token::TokenKind, Lexer, Token};

use syntax::{SyntaxKind, SyntaxNode};

use rowan::GreenNode;
use sink::Sink;

use self::{event::Event, expressions::expr, marker::Marker, source::Source};

struct Parser<'l, 'input> {
  source: Source<'l, 'input>,
  events: Vec<Event>,
}

impl<'l, 'input> Parser<'l, 'input> {
  fn new(tokens: &'l [Token<'input>]) -> Self {
    Self {
      source: Source::new(tokens),
      events: Vec::new(),
    }
  }

  fn parse(mut self) -> Vec<Event> {
    let root_marker = self.start();
    expr(&mut self);
    root_marker.complete(&mut self, SyntaxKind::Root);

    self.events
  }

  fn start(&mut self) -> Marker {
    let pos = self.events.len();
    self.events.push(Event::Placeholder);

    Marker::new(pos)
  }

  fn peek(&mut self) -> Option<TokenKind> {
    self.source.peek_kind()
  }

  // Consume the current token and add it to the AST
  fn bump(&mut self) {
    self.source.next_token().unwrap();
    self.events.push(Event::AddToken);
  }

  fn at(&mut self, kind: TokenKind) -> bool {
    self.peek() == Some(kind)
  }
}

pub(crate) fn parse(input: &str) -> Parse {
  let tokens: Vec<_> = Lexer::new(input).collect();
  let parser = Parser::new(&tokens);
  let events = parser.parse();
  let sink = Sink::new(&tokens, events);

  Parse {
    green_node: sink.finish(),
  }
}

pub(crate) struct Parse {
  green_node: GreenNode,
}

impl Parse {
  pub fn debug_tree(&self) -> String {
    let syntax_node = SyntaxNode::new_root(self.green_node.clone());
    let formatted = format!("{:#?}", syntax_node);

    // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
    formatted[0..formatted.len() - 1].to_string()
  }
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
            Literal@0..3
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
              Literal@0..1
                Number@0..1 "1"
              Plus@1..2 "+"
              Literal@2..3
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
              Literal@1..3
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
                    Literal@4..6
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
              Literal@0..1
                Number@0..1 "5"
              Asterisk@1..2 "*"
              ParenExpression@2..7
                LeftParenthesis@2..3 "("
                InfixExpression@3..6
                  Literal@3..4
                    Number@3..4 "2"
                  Plus@4..5 "+"
                  Literal@5..6
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
            Literal@3..7
              Number@3..7 "9876""#]],
    );
  }

  #[test]
  fn parse_number_followed_by_whitespace() {
    check(
      "999   ",
      expect![[r#"
          Root@0..6
            Literal@0..6
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
            Literal@1..9
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
          Root@0..11
            LetExpression@0..11
              Let@0..3 "let"
              Whitespace@3..4 " "
              Assignment@4..6
                Identifier@4..5 "x"
                Whitespace@5..6 " "
              Equals@6..7 "="
              Whitespace@7..8 " "
              Literal@8..11
                Number@8..11 "100""#]]
    )
  }

  #[test]
  fn parse_destructuring() {
    check(
      "let { x, y } = x;",
      expect![[r#"
          Root@0..16
            LetExpression@0..16
              Let@0..3 "let"
              Whitespace@3..4 " "
              Assignment@4..13
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
                Identifier@15..16 "x""#]]
    )
  }

  #[test]
  fn parse_destructuring_field_alias() {
    check(
      "let { x: asd, y } = z;",
      expect![[r#"
          Root@0..21
            LetExpression@0..21
              Let@0..3 "let"
              Whitespace@3..4 " "
              Assignment@4..18
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
                Identifier@20..21 "z""#]]
    )
  }
}
