use crate::lexer::token::SyntaxKind;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Event {
  StartNode {
    kind: SyntaxKind,
    forward_parent: Option<usize>,
  },
  AddToken {
    kind: SyntaxKind,
    text: String,
  },
  FinishNode,
  Placeholder,
}
