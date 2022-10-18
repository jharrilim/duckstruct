use ast::Root;
use hir;
use parser::parse;
use tycheck::{typed_hir::Ty, TyCheck};
use data_structures::FxIndexMap;

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
  pub fn tycheck_number_literal() {
    let code = "1";
    let tycheck = tycheck(code);
  
    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }
  
  #[test]
  pub fn tycheck_string_literal() {
    let code = "\"hello\"";
    let tycheck = tycheck(code);
  
    expect_type_for_definition(&tycheck, "", Ty::String(Some("hello".to_string())));
  }
  
  #[test]
  pub fn tycheck_boolean_literal() {
    let code = "true";
    let tycheck = tycheck(code);
  
    expect_type_for_definition(&tycheck, "", Ty::Boolean(Some(true)));
  }
  
  #[test]
  pub fn tycheck_array_literal() {
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
  pub fn tycheck_object_literal() {
    let code = "{{ a: 1, b: 2 }}";
    let tycheck = tycheck(code);
  
    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(Some(index_map!{
        "a".to_string() => Ty::Number(Some(1.0)),
        "b".to_string() => Ty::Number(Some(2.0)),
      })),
    );
  }
}

