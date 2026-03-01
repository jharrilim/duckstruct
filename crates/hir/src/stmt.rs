use crate::DatabaseIdx;

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
  /// path = module path (e.g. ["helper"] or ["subdir", "helper"]); items = names to import.
  Use {
    path: Vec<String>,
    items: Vec<String>,
  },
  Expr(DatabaseIdx),
}
