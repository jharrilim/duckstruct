pub use db::Database;
pub use expr::{BinaryOp, Expr, UnaryOp};
pub use stmt::Stmt;

mod db;
pub mod expr;
pub mod stmt;
pub mod pat;

pub type DatabaseIdx = data_structures::arena::Idx<Expr>;

pub fn lower(ast: ast::Root) -> Database {
  let mut db = Database::default();
  db.lower(ast);

  db
}
