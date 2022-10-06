use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
  VariableDef { name: String, value: Expr },
  FunctionDef { name: String, params: Vec<String>, body: Expr },
  Expr(Expr),
}
