use crate::{
  lexer::{token::SyntaxKind, Token},
  syntax::Duckstruct,
};

use super::event::Event;

use rowan::{GreenNode, GreenNodeBuilder, Language};
use std::mem;

pub(super) struct Sink<'l, 'input> {
  builder: GreenNodeBuilder<'static>,
  tokens: &'l [Token<'input>],
  cursor: usize,
  events: Vec<Event>,
}

impl<'l, 'input> Sink<'l, 'input> {
  pub(super) fn new(tokens: &'l [Token<'input>], events: Vec<Event>) -> Self {
    Self {
      builder: GreenNodeBuilder::new(),
      tokens,
      cursor: 0,
      events,
    }
  }

  pub(super) fn finish(mut self) -> GreenNode {
    for idx in 0..self.events.len() {
      match mem::replace(&mut self.events[idx], Event::Placeholder) {
        Event::StartNode {
          kind,
          forward_parent,
        } => {
          let mut kinds = vec![kind];

          let mut idx = idx;
          let mut forward_parent = forward_parent;

          // Walk through the forward parent of the forward parent, and the forward parent
          // of that, and of that, etc. until we reach a StartNode event without a forward
          // parent.
          while let Some(fp) = forward_parent {
            idx += fp;

            forward_parent = if let Event::StartNode {
              kind,
              forward_parent,
            } = mem::replace(&mut self.events[idx], Event::Placeholder)
            {
              kinds.push(kind);
              forward_parent
            } else {
              unreachable!()
            };
          }
          for kind in kinds.into_iter().rev() {
            self.builder.start_node(Duckstruct::kind_to_raw(kind));
          }
        }
        Event::AddToken => self.token(),
        Event::FinishNode => self.builder.finish_node(),
        Event::Placeholder => {}
      }
      self.eat_trivia();
    }

    self.builder.finish()
  }

  fn token(&mut self) {
    let Token { kind, text } = self.tokens[self.cursor];
    self
      .builder
      .token(Duckstruct::kind_to_raw(kind), text);
    self.cursor += 1;
  }

  fn eat_trivia(&mut self) {
    while let Some(token) = self.tokens.get(self.cursor) {
      if !token.kind.is_trivia() {
        break;
      }

      self.token();
    }
  }
}
