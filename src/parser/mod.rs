mod event;
pub mod expressions;
mod sink;
mod source;

use crate::{
  lexer::{token::SyntaxKind, Lexer, Lexeme},
  syntax::SyntaxNode,
};
use rowan::GreenNode;
use sink::Sink;

use self::{event::Event, expressions::expr, source::Source};

struct Parser<'l, 'input> {
  source: Source<'l, 'input>,
  events: Vec<Event>,
}

impl<'l, 'input> Parser<'l, 'input> {
  fn new(lexemes: &'l [Lexeme<'input>]) -> Self {
    Self {
      source: Source::new(lexemes),
      events: Vec::new(),
    }
  }

  fn parse(mut self) -> Vec<Event> {
    self.start_node(SyntaxKind::Root);
    expr(&mut self);
    self.finish_node();

    self.events
  }

  // Sets the starting point in our AST
  fn start_node(&mut self, kind: SyntaxKind) {
    self.events.push(Event::StartNode { kind });
  }

  // Create a new branch in our AST
  fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
    self.events.push(Event::StartNodeAt { kind, checkpoint });
  }

  // Sets the ending point in our AST
  fn finish_node(&mut self) {
    self.events.push(Event::FinishNode);
  }

  // Take a look at the next non-whitespace token
  // without consuming it or adding it to the AST
  fn peek(&mut self) -> Option<SyntaxKind> {
    self.source.peek_kind()
  }

  // Consume the current token and add it to the AST
  fn bump(&mut self) {
    let Lexeme { kind, text } = self.source.next_lexeme().unwrap();

    self.events.push(Event::AddToken { kind: *kind, text: (*text).to_string() });
  }

  fn checkpoint(&self) -> usize {
    self.events.len()
  }
}

pub(crate) fn parse(input: &str) -> Parse {
  let lexemes: Vec<_> = Lexer::new(input).collect();
  let parser = Parser::new(&lexemes);
  let events = parser.parse();
  let sink = Sink::new(&lexemes, events);

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
              Number@0..3 "123""#]],
    );
  }

  #[test]
  fn parse_variable_ref() {
    check(
      "aVariableName",
      expect![[r#"
              Root@0..13
                Identifier@0..13 "aVariableName""#]],
    );
  }

  #[test]
  fn parse_simple_binary_expression() {
    check(
      "1+2",
      expect![[r#"
              Root@0..3
                BinaryExpression@0..3
                  Number@0..1 "1"
                  Plus@1..2 "+"
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
                  Number@1..3 "10""#]],
    );
  }

  #[test]
  fn parse_nested_parentheses() {
    check(
      "((((10))))",
      expect![[r#"
              Root@0..10
                LeftParenthesis@0..1 "("
                LeftParenthesis@1..2 "("
                LeftParenthesis@2..3 "("
                LeftParenthesis@3..4 "("
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
                BinaryExpression@0..7
                  Number@0..1 "5"
                  Asterisk@1..2 "*"
                  LeftParenthesis@2..3 "("
                  BinaryExpression@3..6
                    Number@3..4 "2"
                    Plus@4..5 "+"
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
                Number@3..7 "9876""#]],
      );
  }

  #[test]
  fn parse_number_followed_by_whitespace() {
      check(
          "999   ",
          expect![[r#"
              Root@0..6
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
                Number@1..4 "123"
                Whitespace@4..9 "     ""#]],
      );
  }
}
