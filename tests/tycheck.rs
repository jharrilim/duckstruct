use ast::Root;
use data_structures::FxIndexMap;
use hir;
use parser::parse;
use tycheck::{typed_hir::Ty, TyCheck};

pub fn tycheck(code: &str) -> TyCheck {
  let root = Root::cast(parse(code).syntax()).unwrap();
  let hir = hir::lower(root);
  let mut tycheck = TyCheck::new(hir);
  tycheck.infer();
  tycheck
}

pub fn expect_type_for_definition(tycheck: &TyCheck, def: &str, ty: Ty) {
  let expr = tycheck.ty_db.definition_expr(def);

  assert!(expr.is_some());
  let expr = expr.unwrap();
  assert_eq!(expr.ty(), ty);
}

mod literals {
  use data_structures::index_map;

  use super::*;

  #[test]
  fn tycheck_number_literal() {
    let code = "1";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_string_literal() {
    let code = "\"hello\"";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::String(Some("hello".to_string())));
  }

  #[test]
  fn tycheck_boolean_literal() {
    let code = "true";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Boolean(Some(true)));
  }

  #[test]
  fn tycheck_array_literal() {
    let code = "[1, 2, 3]";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Array(Some(vec![
        Ty::Number(Some(1.0)),
        Ty::Number(Some(2.0)),
        Ty::Number(Some(3.0)),
      ])),
    );
  }

  #[test]
  fn tycheck_object_literal() {
    let code = "{{ a: 1, b: 2 }}";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(Some(index_map! {
        "a".to_string() => Ty::Number(Some(1.0)),
        "b".to_string() => Ty::Number(Some(2.0)),
      })),
    );
  }
}

mod expressions {
  use super::*;

  #[test]
  fn tycheck_conditional() {
    let code = "if true { 1 } else { 2 }";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_with_constant_body() {
    let code = "f() = 1 ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Function {
        params: vec![],
        ret: Some(Box::new(Ty::Number(Some(1.0)))),
      },
    );
  }

  #[test]
  fn tycheck_function_with_argument_returning_argument() {
    let code = "f(x) { x }";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Generic)),
      },
    );
  }

  #[test]
  fn tycheck_function_invocation() {
    let code = "
      f hello() { \"hello\" }
      hello()
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "hello",
      Ty::Function {
        params: vec![],
        ret: Some(Box::new(Ty::String(Some("hello".to_string())))),
      },
    );

    expect_type_for_definition(&tycheck, "", Ty::String(Some("hello".to_string())));
  }

  #[test]
  fn tycheck_object_property_accessor() {
    let code = "
      let obj = {{ a: 1, b: 2 }};
      obj.b
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }
}
