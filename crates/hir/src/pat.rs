#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pat {
    Ident {
      name: String,
    },
    Array {
      items: Vec<Pat>,
    },
    Object {
      items: Vec<(String, Pat)>,
    },
    Rest {
      name: String,
    },
    None,
}
