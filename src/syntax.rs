use crate::lexer::token::SyntaxKind;
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct Duckstruct {}

impl Language for Duckstruct {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    Self::Kind::from_u16(raw.0).unwrap()
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind.to_u16().unwrap())
  }
}

pub(crate) type SyntaxNode = rowan::SyntaxNode<Duckstruct>;
