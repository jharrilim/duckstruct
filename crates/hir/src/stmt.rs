use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
  VariableDef { name: String, value: Expr },
  Expr(Expr),
}
