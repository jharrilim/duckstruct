pub use db::Database;
pub use expr::Expr;
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

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    let result = 2 + 2;
    assert_eq!(result, 4);
  }
}
