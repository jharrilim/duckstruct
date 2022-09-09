use lexer::{token::TokenKind, Token};

use syntax::{SyntaxKind, SyntaxNode};

use rowan::GreenNode;

use crate::statements::stmt;

use super::{event::Event, marker::Marker, source::Source};

pub struct Parser<'l, 'input> {
  pub(crate) source: Source<'l, 'input>,
  pub(crate) events: Vec<Event>,
  #[allow(unused)]
  pub(crate) errors: Vec<String>,
}

impl<'l, 'input> Parser<'l, 'input> {
  pub(crate) fn new(tokens: &'l [Token<'input>]) -> Self {
    Self {
      source: Source::new(tokens),
      events: Vec::new(),
      errors: Vec::new(),
    }
  }

  pub(crate) fn parse(mut self) -> Vec<Event> {
    let root_marker = self.start();
    while self.source.peek_kind().is_some() {
      stmt(&mut self);
    }
    root_marker.complete(&mut self, SyntaxKind::Root);

    self.events
  }

  pub(crate) fn start(&mut self) -> Marker {
    let pos = self.events.len();
    self.events.push(Event::Placeholder);

    Marker::new(pos)
  }

  pub(crate) fn peek(&mut self) -> Option<TokenKind> {
    self.source.peek_kind()
  }

  // Consume the current token and add it to the AST
  pub(crate) fn bump(&mut self) {
    self.source.next_token().unwrap();
    self.events.push(Event::AddToken);
  }

  pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
    self.peek() == Some(kind)
  }
}

pub struct Parse {
  pub root: GreenNode,
}

impl Parse {
  pub fn debug_tree(&self) -> String {
    let syntax_node = SyntaxNode::new_root(self.root.clone());
    let formatted = format!("{:#?}", syntax_node);

    // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
    formatted[0..formatted.len() - 1].to_string()
  }

  pub fn syntax(&self) -> SyntaxNode {
    SyntaxNode::new_root(self.root.clone())
  }
}