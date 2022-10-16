use ast::Root;
use parser::parse;

fn ast(code: &str) -> Root {
  Root::cast(parse(code).syntax()).unwrap()
}

#[test]
fn test_ast_number_literal() {
    let ast = ast("1");
    assert_eq!(ast.stmts().count(), 1);

    let stmt = ast.stmts().next();
    assert!(stmt.is_some());

    let stmt = stmt.unwrap();
    assert!(stmt.expr().is_some());

    let expr = stmt.expr().unwrap();
    assert!(matches!(expr, ast::Expr::NumberLit(_)));
}
