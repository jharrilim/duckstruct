use crate::lexer::token::Token;
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::{Language, SyntaxKind};

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct Duckstruct {}

impl Language for Duckstruct {
    type Kind = Token;

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind {
        SyntaxKind(kind.to_u16().unwrap())
    }
}

pub(crate) type SyntaxNode = rowan::SyntaxNode<Duckstruct>;
