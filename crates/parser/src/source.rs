use lexer::{token::TokenKind, Token};

pub(super) struct Source<'l, 'input> {
  tokens: &'l [Token<'input>],
  cursor: usize,
}

impl<'l, 'input> Source<'l, 'input> {
  pub(super) fn new(tokens: &'l [Token<'input>]) -> Self {
    Self { tokens, cursor: 0 }
  }

  pub(super) fn next_token(&mut self) -> Option<&'l Token<'input>> {
    self.eat_trivia();

    let token = self.tokens.get(self.cursor)?;
    self.cursor += 1;

    Some(token)
  }

  pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
    self.eat_trivia();
    self.peek_kind_raw()
  }

  fn peek_kind_raw(&self) -> Option<TokenKind> {
    self.peek_token_raw().map(|Token { kind, .. }| *kind)
  }

  fn eat_trivia(&mut self) {
    while self.at_trivia() {
      self.cursor += 1;
    }
  }

  fn at_trivia(&self) -> bool {
    self.peek_kind_raw().map_or(false, TokenKind::is_trivia)
  }

  fn peek_token_raw(&self) -> Option<&Token> {
    self.tokens.get(self.cursor)
  }
}
