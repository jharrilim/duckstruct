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
  Use {
    path: Vec<String>,
    alias: Option<String>,
  },
  Expr(DatabaseIdx),
}
