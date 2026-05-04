use ast::Root;
use data_structures::FxIndexMap;
use diagnostics as diag_emit;

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

  assert!(
    !tycheck.diagnostics.has_errors(),
    "unexpected errors: {:?}",
    tycheck
      .diagnostics
      .items()
      .iter()
      .filter(|e| e.is_error())
      .map(|e| e.message().to_string())
      .collect::<Vec<_>>()
  );
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
    let code = "new { a: 1, b: 2 }";
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
      let obj = new { a: 1, b: 2 };
      obj.b
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_object_property_accessor_function_call() {
    let code = "
      let obj = new { a: 1, b: f() = 2 };
      obj.b()
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(2.0)));
  }

  #[test]
  fn tycheck_object_property_accessor_function_call_with_argument() {
    let code = "
      let obj = new { a: 1, b: f(x) = x };
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
      f finger() { new {} }
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
      a(new { a: 1 })
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_function_with_object_argument_returning_new_object() {
    let code = "
      let a = f(x) = new { a: x.a };
      a(new { a: 1 })
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(Some(index_map!(
        "a".to_string() => Ty::Number(Some(1.0))
      ))),
    );
  }

  #[test]
  fn tycheck_curried_function_with_object_arguments_returning_new_object() {
    let code = "
      let b = f(x) = f(y) = new { asd: x.a + y.b };
      b(new { a: 1 })(new { b: 2 })
    ";

    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "",
      Ty::Object(Some(index_map!(
        "asd".to_string() => Ty::Number(Some(3.0))
      ))),
    );
  }

  #[test]
  fn tycheck_function_with_object_and_accessing_field_inside() {
    let code = "
      let a = f(x) {
        x.a
      };

      a(new { a: \"nice\" })
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "", Ty::String(Some("nice".to_string())));
  }

  #[test]
  fn tycheck_function_with_object_constraint_on_parameter() {
    let code = "
    let a = f(x) = x.a;

    a(new { a: \"nice\" })
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
  fn tycheck_function_with_object_constraint_on_parameter_through_let_rebinding() {
    let code = "
      f add(pos) {
        let p = pos;
        p.x + p.y
      }
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "add",
      Ty::Function {
        params: vec![Ty::Object(Some(index_map!(
          "x".to_string() => Ty::Generic,
          "y".to_string() => Ty::Generic
        )))],
        ret: Some(Box::new(Ty::Generic)),
      },
    );
  }

  #[test]
  fn tycheck_object_literal_immediate_property_access() {
    let code = "
      let a = new { a: 1 }.a;
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(1.0)));
  }

  #[test]
  fn tycheck_object_literal_nested_property_access() {
    let code = "
      let a = new { a: new { b: 1 } }.a.b;
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

      a(new { a: 1, b: 2 })
    ";
    let tycheck = tycheck(code);

    let a_param_type = Ty::Object(Some(index_map!(
      "a".to_string() => Ty::Generic,
      "b".to_string() => Ty::Generic
    )));

    expect_type_for_definition(
      &tycheck,
      "a",
      Ty::Function {
        params: vec![a_param_type],
        ret: Some(Box::new(Ty::Generic)),
      },
    );
    expect_type_for_definition(&tycheck, "", Ty::Number(Some(3.0)));
  }

  #[test]
  fn tycheck_reassignment_holds_ref_to_original() {
    let code = "
      let a = 1;
      let b = a;
      a = 2;
      b
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "b", Ty::Number(Some(1.0)));
  }
}

mod loops {
  use super::*;
  use ast::Stmt;

  #[test]
  fn for_expression_binding_pattern_is_present() {
    let code = "let a = for x in [1, 2, 3] x;";
    let root = Root::cast(parse(code).syntax()).unwrap();
    let stmt = root.stmts().next().expect("stmt");
    let Stmt::VariableDef(def) = stmt else {
      panic!("expected let");
    };
    let ast::Expr::ForExpression(for_e) = def.value().expect("rhs") else {
      panic!("expected for");
    };
    assert!(
      for_e.binding_pattern().is_some(),
      "binding pattern should resolve (got {:?})",
      for_e.binding_pattern()
    );
  }

  #[test]
  fn tycheck_for_last_element_determinate() {
    let code = "let a = for x in [1, 2, 3] x;";
    let tycheck = tycheck(code);
    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(3.0)));
  }

  #[test]
  fn tycheck_for_fold_sum_determinate() {
    let code = "let a = for x in [1, 2, 3] |0| |acc, i| acc + x;";
    let tycheck = tycheck(code);
    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(6.0)));
  }

  #[test]
  fn tycheck_for_where_filter_determinate() {
    let code = "let a = for x in [1, 2, 3, 4] where x < 3 |0| |acc, i| acc + x;";
    let tycheck = tycheck(code);
    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(3.0)));
  }

  #[test]
  fn tycheck_for_empty_uses_initializer() {
    let code = "let a = for x in [] |42| 0;";
    let tycheck = tycheck(code);
    expect_type_for_definition(&tycheck, "a", Ty::Number(Some(42.0)));
  }

  #[test]
  fn tycheck_for_indeterminate_over_parameter() {
    let code = "
      f sum(xs) {
        for x in xs |0| |acc, i| acc + x
      }
    ";
    let tycheck = tycheck(code);
    expect_type_for_definition(
      &tycheck,
      "sum",
      Ty::Function {
        params: vec![Ty::Generic],
        ret: Some(Box::new(Ty::Number(None))),
      },
    );
  }

  #[test]
  fn tycheck_for_where_must_be_boolean_error() {
    let code = "let a = for x in [1] where 1 x;";
    let tycheck = tycheck(code);
    assert!(
      tycheck.diagnostics.has_errors(),
      "expected error when where is not boolean"
    );
    assert!(
      tycheck
        .diagnostics
        .errors()
        .any(|e| e.message().contains("boolean")),
      "expected boolean-related diagnostic"
    );
  }
}

mod structs {
  use super::*;

  #[test]
  fn tycheck_function_returning_struct_instance() {
    let code = "
      struct Foo { }
      let a = f() = new Foo { }
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(
      &tycheck,
      "a",
      Ty::Function {
        params: vec![],
        ret: Some(Box::new(Ty::Instance("Foo".to_string()))),
      },
    );
  }

  #[test]
  fn tycheck_struct_literal_empty() {
    let code = "
      struct Foo { }
      let x = new Foo { }
    ";
    let tycheck = tycheck(code);

    expect_type_for_definition(&tycheck, "x", Ty::Instance("Foo".to_string()));
  }

  #[test]
  fn tycheck_struct_constructor_call_errors() {
    let code = "
      struct Foo { }
      let x = Foo(1)
    ";
    let tycheck = tycheck(code);

    assert!(tycheck.diagnostics.has_errors());
    let first = tycheck.diagnostics.errors().next().unwrap();
    assert!(
      first.message().contains("new"),
      "expected hint to use `new` struct syntax, got: {}",
      first.message()
    );
  }
}

mod traits {
  use super::*;

  #[test]
  fn tycheck_trait_impl_for_struct_and_primitive() {
    let code = "
      trait Renderable {
        f render(x);
      }

      struct Foo { }

      impl Renderable for Foo {
        f render(x) { x }
      }

      impl Renderable for number {
        f render(x) { x }
      }
    ";
    let tycheck = tycheck(code);
    assert!(
      !tycheck.diagnostics.has_errors(),
      "unexpected errors: {:?}",
      tycheck
        .diagnostics
        .items()
        .iter()
        .map(|e| e.message().to_string())
        .collect::<Vec<_>>()
    );
  }

  #[test]
  fn tycheck_trait_impl_missing_required_method_errors() {
    let code = "
      trait Renderable {
        f render(x);
      }

      struct Foo { }

      impl Renderable for Foo {
        f other(x) { x }
      }
    ";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    assert!(
      tycheck
        .diagnostics
        .errors()
        .any(|e| e.code == "type::trait_missing_method"),
      "expected type::trait_missing_method diagnostic"
    );
  }

  #[test]
  fn tycheck_trait_impl_method_arity_mismatch_errors() {
    let code = "
      trait Renderable {
        f render(x);
      }

      struct Foo { }

      impl Renderable for Foo {
        f render(x, y) { x + y }
      }
    ";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    assert!(
      tycheck
        .diagnostics
        .errors()
        .any(|e| e.code == "type::trait_method_mismatch"),
      "expected type::trait_method_mismatch diagnostic"
    );
  }
}

mod typecheck_diagnostics {
  use std::collections::HashMap;

  use super::*;

  #[test]
  fn parameter_constraint_reports_missing_method_on_argument_object() {
    let code = "
      let moo = f(animal) {
        animal.moo()
      };

      moo(new { bark: f() = \"bark\" })
    ";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    let err = tycheck
      .diagnostics
      .items()
      .iter()
      .find(|d| d.code == "type::parameter_constraint")
      .expect("expected type::parameter_constraint diagnostic");
    assert!(
      err.message.contains("object anonymous must provide `moo()`"),
      "expected object anonymous + moo() message, got {:?}",
      err.message
    );
  }

  #[test]
  fn parameter_constraint_message_uses_variable_argument_name() {
    let code = "
      let moo = f(animal) {
        animal.moo()
      };

      let pet = new { bark: f() = \"bark\" };
      moo(pet)
    ";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    let err = tycheck
      .diagnostics
      .items()
      .iter()
      .find(|d| d.code == "type::parameter_constraint")
      .expect("expected type::parameter_constraint diagnostic");
    assert!(
      err.message.contains("object pet must provide `moo()`"),
      "expected object pet in message, got {:?}",
      err.message
    );
  }

  #[test]
  fn parameter_constraint_message_uses_struct_type_name() {
    let code = "
      struct Cow { }

      let moo = f(animal) {
        animal.moo()
      };

      moo(new Cow { })
    ";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    let err = tycheck
      .diagnostics
      .items()
      .iter()
      .find(|d| d.code == "type::parameter_constraint")
      .expect("expected type::parameter_constraint diagnostic");
    assert!(
      err.message.contains("object Cow must provide `moo()`"),
      "expected struct type name Cow in message, got {:?}",
      err.message
    );
  }

  /// Full ariadne-style human output for a parameter object missing a required method.
  #[test]
  fn parameter_constraint_missing_method_human_snapshot() {
    let code = "let moo = f(animal) {\n  animal.moo()\n};\n\nmoo(new { bark: f() = \"bark\" })\n";
    let tycheck = tycheck(code);
    assert!(tycheck.diagnostics.has_errors());
    // Snapshot only this diagnostic: re-inferring the body after substitution can emit
    // additional errors that are redundant with the caller-side constraint report.
    let mut bundle = tycheck.diagnostics.bundle.clone();
    bundle
      .items
      .retain(|d| d.code == "type::parameter_constraint");
    assert!(
      !bundle.items.is_empty(),
      "expected type::parameter_constraint diagnostic"
    );
    let output = diag_emit::emit_human_string(
      code,
      &bundle,
      &diag_emit::HumanEmitConfig {
        colors: false,
        file_label: "test.ds".into(),
      },
    );
    insta::assert_snapshot!(output);
  }

  #[test]
  fn invalid_code_reports_error_with_message() {
    let code = "x()";
    let tycheck = tycheck(code);

    assert!(tycheck.diagnostics.has_errors(), "expected type error for invalid code");
    let first = tycheck.diagnostics.errors().next().unwrap();
    let msg = first.message();
    assert!(
      msg.contains("Cannot call `x`") && msg.contains("it doesn't exist"),
      "expected cannot-call message, got: {}",
      msg
    );
  }

  #[test]
  fn cannot_call_suggests_module_when_pub_fn_exists_in_dependency() {
    let dep_code = "pub f hello() = 1";
    let root_dep = Root::cast(parse(dep_code).syntax()).unwrap();
    let hir_dep = hir::lower(root_dep);
    let mut dep_tc = TyCheck::new(hir_dep);
    dep_tc.infer();

    let entry_code = "hello()";
    let root_entry = Root::cast(parse(entry_code).syntax()).unwrap();
    let hir_entry = hir::lower(root_entry);
    let mut map = HashMap::new();
    map.insert("helper".to_string(), &dep_tc);

    let mut entry_tc = TyCheck::new(hir_entry);
    entry_tc.infer_with_modules(Some(&map), None, None, None);

    let msg = entry_tc.diagnostics.errors().next().unwrap().message();
    assert!(
      msg.contains("Did you mean `helper::hello`"),
      "expected import hint, got: {}",
      msg
    );
  }

  #[test]
  fn cannot_call_non_function_value_shows_not_a_function() {
    let tycheck = tycheck("let x = 1\nx()");
    let msg = tycheck.diagnostics.errors().next().unwrap().message();
    assert!(
      msg.contains("Cannot call `x`") && msg.contains("it is not a function"),
      "got: {}",
      msg
    );
  }

  #[test]
  fn error_format_includes_line_and_message_when_source_provided() {
    let code = "x()";
    let tycheck = tycheck(code);

    assert!(tycheck.diagnostics.has_errors());
    let output = diag_emit::emit_human_string(
      code,
      &tycheck.diagnostics.bundle,
      &diag_emit::HumanEmitConfig {
        colors: false,
        file_label: "test.ds".into(),
      },
    );
    assert!(
      !output.is_empty(),
      "human diagnostic output should be non-empty"
    );
    assert!(
      output.contains("x()") || output.contains("Error"),
      "expected snippet or error header in output, got: {}",
      output
    );
  }

  #[test]
  fn trait_impl_requires_trait_in_scope_from_imports() {
    let dep_code = "
      pub trait Renderable {
        f render(x);
      }
    ";
    let dep_root = Root::cast(parse(dep_code).syntax()).unwrap();
    let dep_hir = hir::lower(dep_root);
    let mut dep_tc = TyCheck::new(dep_hir);
    dep_tc.infer();

    let entry_code_missing_use = "
      impl Renderable for number {
        f render(x) { x }
      }
    ";
    let entry_root_missing_use = Root::cast(parse(entry_code_missing_use).syntax()).unwrap();
    let entry_hir_missing_use = hir::lower(entry_root_missing_use);
    let mut entry_tc_missing_use = TyCheck::new(entry_hir_missing_use);
    let mut map = HashMap::new();
    map.insert("helper".to_string(), &dep_tc);
    entry_tc_missing_use.infer_with_modules(Some(&map), None, None, None);
    assert!(
      entry_tc_missing_use
        .diagnostics
        .errors()
        .any(|e| e.code == "type::trait_unknown"),
      "expected unknown trait when trait is not imported into scope"
    );

    let entry_code_with_use = "
      use helper::{Renderable};
      impl Renderable for number {
        f render(x) { x }
      }
    ";
    let entry_root_with_use = Root::cast(parse(entry_code_with_use).syntax()).unwrap();
    let entry_hir_with_use = hir::lower(entry_root_with_use);
    let mut entry_tc_with_use = TyCheck::new(entry_hir_with_use);
    entry_tc_with_use.infer_with_modules(Some(&map), None, None, None);
    assert!(
      !entry_tc_with_use.diagnostics.has_errors(),
      "unexpected errors when trait is imported: {:?}",
      entry_tc_with_use
        .diagnostics
        .items()
        .iter()
        .map(|e| e.message().to_string())
        .collect::<Vec<_>>()
    );
  }
}
