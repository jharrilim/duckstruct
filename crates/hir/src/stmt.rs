use crate::DatabaseIdx;

#[derive(Debug, Clone)]
pub enum Stmt {
  VariableDef { name: String, value: DatabaseIdx },
  FunctionDef { name: String, value: DatabaseIdx },
  Expr(DatabaseIdx),
}
