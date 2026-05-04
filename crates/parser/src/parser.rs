use lexer::{token::TokenKind, Token};

use syntax::{SyntaxKind, SyntaxNode};

use rowan::GreenNode;

use crate::{
  parse_error::{Expectations, ParseError},
  statements::stmt,
};

use super::{event::Event, marker::Marker, source::Source};

const RECOVERY_SET: [TokenKind; 1] = [TokenKind::Let];

pub struct Parser<'l, 'input> {
  pub(crate) source: Source<'l, 'input>,
  pub(crate) events: Vec<Event>,
  expected_kinds: Vec<TokenKind>,
  /// Incremented for each [`Event::Error`] (used e.g. to avoid a redundant `let` rhs diagnostic).
  pub(crate) parse_errors_emitted: usize,
}

impl<'l, 'input> Parser<'l, 'input> {
  pub(crate) fn new(tokens: &'l [Token<'input>]) -> Self {
    Self {
      source: Source::new(tokens),
      events: Vec::new(),
      expected_kinds: Vec::new(),
      parse_errors_emitted: 0,
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

  pub(crate) fn expect(&mut self, kind: TokenKind) {
    if !self.at(kind) {
      self.error();
    }
  }

  pub(crate) fn error(&mut self) {
    let current_token = self.source.peek_token();

    let (found, range, line) = if let Some(Token {
      kind, range, line, ..
    }) = current_token
    {
      (Some(*kind), *range, *line)
    } else {
      // If we’re at the end of the input we use the range of the very last token in the
      // input.
      (None, self.source.last_token_range().unwrap(), 0)
    };

    if !self.expected_kinds.is_empty() {
      self.events.push(Event::Error(ParseError {
        expected: Expectations::Tokens(std::mem::take(&mut self.expected_kinds)),
        found,
        range,
        line,
      }));
      self.parse_errors_emitted += 1;
    } else {
      self.events.push(Event::Error(ParseError {
        expected: Expectations::Expression,
        found,
        range,
        line,
      }));
      self.parse_errors_emitted += 1;
    }

    if !self.at_recovery_set() && !self.at_end() {
      let m = self.start();
      self.bump();
      m.complete(self, SyntaxKind::Error);
      // Do not emit further diagnostics for junk on the same physical line; the next newline
      // (or `let` in the recovery set) starts a clean resync so following lines parse normally.
      self.recover_skip_rest_of_line(line);
    }
  }

  /// After a diagnostic on `anchor_line`, consume remaining tokens on that line as `Error`
  /// nodes without additional [`ParseError`] entries (see `error`).
  fn recover_skip_rest_of_line(&mut self, anchor_line: usize) {
    while !self.at_end() && !self.at_recovery_set() {
      let Some(tok) = self.source.peek_token() else {
        break;
      };
      if tok.line != anchor_line {
        break;
      }
      let m = self.start();
      self.bump();
      m.complete(self, SyntaxKind::Error);
    }
  }

  // Similar to error but allows passing in expected tokens. Useful within
  // match when you land on something other than one of the expected branches.
  pub(crate) fn error_expected_one_of(&mut self, expected: &[TokenKind]) {
    self.expected_kinds.extend_from_slice(expected);
    self.error();
  }

  pub(crate) fn expected(&mut self, expected: TokenKind) {
    self.expected_kinds.push(expected);
    self.error();
  }

  pub(crate) fn peek(&mut self) -> Option<TokenKind> {
    self.source.peek_kind()
  }

  // Consume the current token and add it to the AST
  pub(crate) fn bump(&mut self) {
    self.expected_kinds.clear();
    if self.source.next_token().is_some() {
      self.events.push(Event::AddToken);
    }
  }

  pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
    self.expected_kinds.push(kind);
    self.peek() == Some(kind)
  }

  pub(crate) fn at_end(&mut self) -> bool {
    self.peek().is_none()
  }

  fn at_recovery_set(&mut self) -> bool {
    self.peek().is_some_and(|k| RECOVERY_SET.contains(&k))
  }
}

#[derive(Debug)]
pub struct Parse {
  pub root: GreenNode,
  pub errors: Vec<ParseError>,
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
