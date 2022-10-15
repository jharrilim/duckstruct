pub use db::Database;
pub use expr::{BinaryOp, UnaryOp, Expr};
pub use stmt::Stmt;

mod db;
pub mod expr;
pub mod stmt;

pub type DatabaseIdx = la_arena::Idx<Expr>;

pub fn lower(ast: ast::Root) -> Database {
  let mut db = Database::default();
  db.lower(ast);

  db
}
