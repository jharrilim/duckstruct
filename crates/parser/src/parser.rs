use lexer::{token::TokenKind, Lexer, Token};

use syntax::{SyntaxKind, SyntaxNode};

use rowan::GreenNode;
use super::sink::Sink;

use super::{event::Event, expressions::expr, marker::Marker, source::Source};

pub struct Parser<'l, 'input> {
  pub(crate) source: Source<'l, 'input>,
  pub(crate) events: Vec<Event>,
}

impl<'l, 'input> Parser<'l, 'input> {
  pub fn new(tokens: &'l [Token<'input>]) -> Self {
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

pub fn parse(input: &str) -> Parse {
  let tokens: Vec<_> = Lexer::new(input).collect();
  let parser = Parser::new(&tokens);
  let events = parser.parse();
  let sink = Sink::new(&tokens, events);

  Parse {
    green_node: sink.finish(),
  }
}

pub struct Parse {
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