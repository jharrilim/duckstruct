use db::Database;
use stmt::Stmt;

mod db;
pub mod expr;
pub mod stmt;

pub fn lower(ast: ast::Root) -> (Database, Vec<Stmt>) {
  let mut db = Database::default();
  let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();

  (db, stmts)
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    let result = 2 + 2;
    assert_eq!(result, 4);
  }
}
