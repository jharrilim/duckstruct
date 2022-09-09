use syntax::SyntaxKind;

use crate::parse_error::ParseError;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Event {
  StartNode {
    kind: SyntaxKind,
    forward_parent: Option<usize>,
  },
  AddToken,
  FinishNode,
  Placeholder,
  Error(ParseError),
}
