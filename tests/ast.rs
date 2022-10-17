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

#[test]
fn test_ast_conditional_assignment() {
    let ast = ast("let x = if true { 1 } else { 2 }");
    assert_eq!(ast.stmts().count(), 1);

    let stmt = ast.stmts().next();
    assert!(stmt.is_some());

    let stmt = stmt.unwrap();
    assert!(stmt.expr().is_some());

    let expr = stmt.expr().unwrap();
    assert!(matches!(expr, ast::Expr::Conditional(_)));

    let cond = match expr {
      ast::Expr::Conditional(cond) => {
        Some(cond)
      },
      _ => None,
    };
    assert!(cond.is_some());

    let cond = cond.unwrap();

    assert!(cond.predicate().is_some());

    assert!(cond.then_branch().is_some());
}

#[test]
fn test_ast_object_literal() {
    let ast = ast("let x = {{ a: 1, b: 2 }}");
    assert_eq!(ast.stmts().count(), 1);

    let stmt = ast.stmts().next();
    assert!(stmt.is_some());

    let stmt = stmt.unwrap();
    assert!(stmt.expr().is_some());

    let expr = stmt.expr().unwrap();
    assert!(matches!(expr, ast::Expr::Object(_)));

    let obj = match expr {
      ast::Expr::Object(obj) => {
        Some(obj)
      },
      _ => None,
    };
    assert!(obj.is_some());

    let obj = obj.unwrap();

    assert_eq!(obj.fields().count(), 2);
}

#[test]
fn test_ast_object_field_access() {
    let ast = ast("let x = {{ a: 1, b: 2 }}.a");
    assert_eq!(ast.stmts().count(), 1);

    let stmt = ast.stmts().next();
    assert!(stmt.is_some());

    let stmt = stmt.unwrap();
    assert!(stmt.expr().is_some());

    let expr = stmt.expr().unwrap();
    assert!(matches!(expr, ast::Expr::ObjectFieldAccess(_)));

    let field_access = match expr {
      ast::Expr::ObjectFieldAccess(field_access) => {
        Some(field_access)
      },
      _ => None,
    };
    assert!(field_access.is_some());

    let field_access = field_access.unwrap();

    assert!(field_access.object().is_some());
    assert_eq!(field_access.field(), "a");
}
