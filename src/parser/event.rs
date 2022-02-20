use crate::lexer::token::SyntaxKind;

#[derive(Debug, Clone)]
pub(super) enum Event {
  StartNode { kind: SyntaxKind },
  StartNodeAt { kind: SyntaxKind, checkpoint: usize },
  AddToken { kind: SyntaxKind, text: String },
  FinishNode,
}
