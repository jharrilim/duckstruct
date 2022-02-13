pub mod expressions;

use crate::{
  lexer::{token::SyntaxKind, Lexer},
  syntax::{Duckstruct, SyntaxNode},
};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language};
use std::iter::Peekable;

use self::expressions::expr;

pub struct Parser<'a> {
  lexer: Peekable<Lexer<'a>>,
  builder: GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
  pub(crate) fn new(input: &'a str) -> Self {
    Self {
      lexer: Lexer::new(input).peekable(),
      builder: GreenNodeBuilder::new(),
    }
  }

  pub(crate) fn parse(mut self) -> Parse {
    self.start_node(SyntaxKind::Root);

    expr(&mut self);

    self.finish_node();

    Parse {
      green_node: self.builder.finish(),
    }
  }

  // Sets the starting point in our AST
  fn start_node(&mut self, kind: SyntaxKind) {
    self.builder.start_node(Duckstruct::kind_to_raw(kind));
  }

  // Create a new branch in our AST
  fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
    self
      .builder
      .start_node_at(checkpoint, Duckstruct::kind_to_raw(kind));
  }

  // Sets the ending point in our AST
  fn finish_node(&mut self) {
    self.builder.finish_node();
  }

  // Take a look at the next token without consuming it
  // or adding it to the AST
  fn peek(&mut self) -> Option<SyntaxKind> {
    self.lexer.peek().map(|(kind, _)| *kind)
  }

  // Consume the current token and add it to the AST
  fn bump(&mut self) {
    let (kind, text) = self.lexer.next().unwrap();

    self.builder.token(Duckstruct::kind_to_raw(kind), text);
  }

  fn checkpoint(&self) -> Checkpoint {
    self.builder.checkpoint()
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
    let parse = Parser::new(input).parse();
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
  fn parse_simple_binary_expression() {
    check(
      "1+2",
      expect![[r#"
              Root@0..3
                Number@0..1 "1"
                Plus@1..2 "+"
                BinaryOperation@2..3
                  Number@2..3 "2""#]],
    );
  }
}
