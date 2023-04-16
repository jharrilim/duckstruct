use syntax::{SyntaxKind, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
  Ident(Ident),
  Array(Array),
  Object(Object),
  Rest(Rest),
}

impl Pat {
  pub fn cast(node: SyntaxNode) -> Option<Self> {
    let result = match node.kind() {
      SyntaxKind::Identifier => Self::Ident(Ident {
        name: node.text().to_string(),
      }),
      SyntaxKind::ArrayPattern => Self::Array(Array {
        items: node.children().filter_map(Pat::cast).collect(),
      }),
      SyntaxKind::StructPattern => Self::Object(Object {
        items: node
          .children()
          .filter_map(|node| {
            let mut children = node.children();
            let key = children.next()?;
            let value = children.next()?;
            Some((key.text().to_string(), Pat::cast(value)?))
          })
          .collect(),
      }),
      SyntaxKind::RestPattern => Self::Rest(Rest {
        name: node.text().to_string(),
      }),
      _ => return None,
    };

    Some(result)
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
  pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
  pub items: Vec<Pat>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object {
  pub items: Vec<(String, Pat)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rest {
  pub name: String,
}
