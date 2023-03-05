use ast::Root;
use data_structures::FxIndexMap;

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

  assert!(tycheck.diagnostics.has_errors() == false);
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
  use data_structures::index_map;

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

  #[test]
  fn tycheck_object_property_accessor_function_call() {
    let code = "
      let obj = {{ a: 1, b: f() = 2 }};
      obj.b()
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_object_property_accessor_function_call_with_argument() {
    let code = "
      let obj = {{ a: 1, b: f(x) = x }};
      obj.b(2)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_empty_array() {
    let code = "[]";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Array(Some(vec![])));
  }

  #[test]
  fn tycheck_array_with_constant_elements() {
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
  fn tycheck_array_with_function_parameter() {
    let code = "f(x) { [1, 2, x] }";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Array(Some(vec![
          Ty::Number(Some(1.0)),
          Ty::Number(Some(2.0)),
          Ty::Generic,
        ])))),
      },
    );
  }

  #[test]
  fn tycheck_function_with_argument_plus_string() {
    let code = "f(x) { x + \"hello\" }";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::String(None))),
      },
    );
  }

  #[test]
  fn tycheck_function_named_finger() {
    let code = "
      f finger() { {{}} }
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "finger",
      Ty::Function {
        params: vec![],
        ret: Some(Box::new(Ty::Object(Some(index_map! {})))),
      },
    );
  }

  #[test]
  fn tycheck_function_reference() {
    let code = "
      f add(x, y) { x + y }
      add
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "add",
      Ty::Function {
        params: vec![Ty::Generic, Ty::Generic],
        ret: Some(Box::new(Ty::Generic)),
      },
    );
  }

  #[test]
  fn tycheck_function_passing_function_and_calling_it() {
    let code = "
      f a(x) { x() }
      a(f() = 1)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test] /* 🥴 */
  fn tycheck_function_passing_function_and_returning_function_and_calling_it() {
    let code = "
      f a(x) { x() }
      a(f() = f() = 1)()
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_pass_through_with_arg() {
    let code = "
      f a(x) = f(y) = x(y)
      a(f(x) = x + 1)(1)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_function_function_function() {
    let code = "
      let a = f(x) = f(y) = y(x);
      let b = a(\"hello\")(f(x) = x);
      b
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::String(Some("hello".to_string())));
  }

  #[test]
  fn tycheck_function_returning_step_by_step() {
    let code = "
      let a = f(x) = f(y) = f(z) = y(x);
      let b = a(1);
      let c = b(f(x) = x + 1);
      let d = c(true);
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "b",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Function {
          params: vec![Ty::Generic],
          ret: Some(Box::new(Ty::Generic)),
        })),
      },
    );

    expect_type_for_definition(
      &tycheck,
      "c",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Number(Some(2.0)))),
      },
    );

    expect_type_for_definition(&tycheck, "d", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_function_returning_step_by_step_2() {
    let code = "
      let a = f(x) = f(y) = f(z) = x(y);
      let b = a(f(x) = x + 1);
      let c = b(10);
      let d = c(true);
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "b",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Function {
          params: vec![Ty::Generic],
          ret: Some(Box::new(Ty::Number(None))),
        })),
      },
    );

    expect_type_for_definition(
      &tycheck,
      "c",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Number(Some(11.0)))),
      },
    );

    expect_type_for_definition(&tycheck, "d", Ty::Number(Some(11.0)));
  }

  #[test]
  fn tycheck_function_called_with_itself() {
    let code = "
      let I = f(x) = x;
      I(I)(true)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Boolean(Some(true)));
  }

  #[test]
  fn tycheck_calling_block_that_returns_a_function() {
    let code = "
      {
        let x = 1;
        f(y) = y + x
      }(2)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(3.0)));
  }

  #[test]
  fn tycheck_function_returning_function_returning_argument_from_first_function() {
    let code = "
      let TRUE = f(x) = f(y) = x;
      TRUE(1)(2)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_returning_function_returning_argument_from_second_function() {
    let code = "
      let FALSE = f(x) = f(y) = y;
      FALSE(1)(2)
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_lambda_calculus_if() {
    let code = "
      let TRUE = f t1(x) = f t2(y) = x;
      let IF = f a1(p) {
        f a2(x) {
          f a3(y) {
            p(x)(y);
          }
        }
      };
      IF(TRUE)(1)(2)
    ";
    let tycheck = tycheck(code);
    let v = tycheck.ty_db.definition("").unwrap().value();
    let expr = tycheck.ty_db.expr(v);
    println!("{:#?}", expr);
    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_factorial() {
    let code = "
      f factorial(n) {
        if n == 1 {
          1
        } else {
          factorial(n - 1) * n
        }
      }
      let a = factorial(5);
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(120.0)));
  }

  #[test]
  fn tycheck_function_with_object_argument() {
    let code = "
      let a = f(x) = x.a;
      a({{ a: 1 }})
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_with_object_argument_returning_new_object() {
    let code = "
      let a = f(x) = {{ a: x.a }};
      a({{ a: 1 }})
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(
        Some(
          index_map!(
            "a".to_string() => Ty::Number(Some(1.0))
          )
        )
      )
    );
  }

  #[test]
  fn tycheck_curried_function_with_object_arguments_returning_new_object() {
    let code = "
      let b = f(x) = f(y) = {{ asd: x.a + y.b }};
      b({{ a: 1 }})({{ b: 2 }})
    ";

    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(
        Some(
          index_map!(
            "asd".to_string() => Ty::Number(Some(3.0))
          )
        )
      )
    );
  }

  #[test]
  fn tycheck_function_with_object_and_accessing_field_inside() {
    let code = "
      let a = f(x) {
        x.a
      };

      a({{ a: \"nice\" }})
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::String(Some("nice".to_string())));
  }

  #[test]
  fn tycheck_function_with_object_constraint_on_parameter() {
    let code = "
    let a = f(x) = x.a;

    a({{ a: \"nice\" }})
  ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::String(Some("nice".to_string())));
    expect_type_for_definition(
      &tycheck,
      "a",
      Ty::Function {
        params: vec![Ty::Object(Some(index_map!("a".to_string() => Ty::Generic)))],
        ret: Some(Box::new(Ty::Generic)),
      },
    );
  }

  #[test]
  fn tycheck_object_literal_immediate_property_access() {
    let code = "
      let a = {{ a: 1 }}.a;
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_object_literal_nested_property_access() {
    let code = "
      let a = {{ a: {{ b: 1 }} }}.a.b;
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_object_parameter_fields_assigned_to_variables() {
    let code = "
      let a = f(x) {
        let a = x.a;
        let b = x.b;
        a + b
      };

      a({{ a: 1, b: 2 }})
    ";
    let tycheck = tycheck(code);

    let a_param_type = Ty::Object(Some(index_map!(
      "a".to_string() => Ty::Generic,
      "b".to_string() => Ty::Generic
    )));

    expect_type_for_definition(
      &tycheck,
      "a",
      Ty::Function { params: vec![a_param_type], ret: Some(Box::new(Ty::Generic)) }
    );
    expect_type_for_definition(&tycheck, "", Ty::Number(Some(3.0)));
  }
}
