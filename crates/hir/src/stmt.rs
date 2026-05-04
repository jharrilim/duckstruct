use crate::DatabaseIdx;

#[derive(Debug, Clone)]
pub struct TraitMethodSig {
  pub name: String,
  pub params: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ImplMethod {
  pub name: String,
  pub value: DatabaseIdx,
}

#[derive(Debug, Clone)]
pub enum Stmt {
  VariableDef {
    name: String,
    value: DatabaseIdx,
    pub_vis: bool,
  },
  FunctionDef {
    name: String,
    value: DatabaseIdx,
    pub_vis: bool,
  },
  StructDef {
    name: String,
    pub_vis: bool,
  },
  TraitDef {
    name: String,
    methods: Vec<TraitMethodSig>,
    pub_vis: bool,
  },
  ImplDef {
    trait_name: String,
    for_type: String,
    methods: Vec<ImplMethod>,
  },
  /// path = module path (e.g. ["helper"] or ["subdir", "helper"]); items = names to import.
  Use {
    path: Vec<String>,
    items: Vec<String>,
  },
  Expr(DatabaseIdx),
}
