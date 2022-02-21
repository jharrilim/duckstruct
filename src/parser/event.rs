use crate::lexer::token::SyntaxKind;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Event {
  StartNode {
    kind: SyntaxKind,
    forward_parent: Option<usize>,
  },
  StartNodeAt {
    kind: SyntaxKind,
    checkpoint: usize,
  },
  AddToken {
    kind: SyntaxKind,
    text: String,
  },
  FinishNode,
  Placeholder,
}
