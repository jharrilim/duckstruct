use crate::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
  VariableDef { name: String, value: Expr },
  Expr(Expr),
}
