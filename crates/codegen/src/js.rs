#![allow(dead_code, unused)]

/// Generate javascript code from the typed hir
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use tycheck::{
  primitive_methods::{PrimitiveMethodDescriptor, PrimitiveReceiverKind},
  typed_db::TypedDatabaseIdx,
  typed_hir::{FunctionDef, Ty, TypedExpr, TypedStmt},
  TyCheck,
};

use crate::CodeGenerator;

/// Constant [`Ty`] as a JS literal. [`Ty::Display`] uses duckstruct surface syntax for objects (`new` + braces);
/// JS object literals must use plain `{ ... }`, including when nested in arrays.
fn ty_as_js_literal(ty: &Ty) -> Option<String> {
  match ty {
    Ty::Number(Some(_)) | Ty::String(Some(_)) | Ty::Boolean(Some(_)) => Some(ty.to_string()),
    Ty::Array(Some(elts)) => {
      let mut parts = Vec::new();
      for t in elts {
        parts.push(ty_as_js_literal(t)?);
      }
      Some(format!("[{}]", parts.join(", ")))
    }
    Ty::Object(Some(o)) => {
      let mut parts = Vec::new();
      for (k, t) in o.iter() {
        parts.push(format!("{}: {}", k, ty_as_js_literal(t)?));
      }
      Some(format!("{{ {} }}", parts.join(", ")))
    }
    _ => None,
  }
}

pub struct JsGenerator<'tycheck> {
  tycheck: &'tycheck TyCheck,
  /// When generating a dependency module's bundle, prefix for top-level names and refs.
  prefix: Option<String>,
  /// When set with prefix, only emit these (pub) def names; and VariableRef to these get prefixed.
  module_pub_names: Option<std::collections::HashSet<String>>,
  /// When generating entry with imports: map local name -> (module_name, export_name) for prefixed emit.
  import_map: Option<HashMap<String, (String, String)>>,
  /// Builtin names (e.g. print) that are not emitted as defs; calls are emitted as console.log etc.
  external_functions: Option<std::collections::HashSet<String>>,
  /// Primitive method table consulted when emitting `recv.method(args)` calls; methods with a
  /// `js_call` are routed through it instead of the literal `recv.method(args)` form.
  primitive_methods: Option<&'static [PrimitiveMethodDescriptor]>,
  /// Runtime asset ids recorded whenever a `js_call`-bearing primitive method is emitted.
  /// The compile driver reads this after `generate_js` to assemble the prepended runtime.
  used_runtime_ids: RefCell<HashSet<&'static str>>,
}

impl<'tycheck> CodeGenerator for JsGenerator<'tycheck> {
  fn generate(&self) -> String {
    self.generate_js()
  }
}

impl<'tycheck> JsGenerator<'tycheck> {
  pub fn new(tycheck: &'tycheck TyCheck) -> Self {
    Self {
      tycheck,
      prefix: None,
      module_pub_names: None,
      import_map: None,
      external_functions: None,
      primitive_methods: None,
      used_runtime_ids: RefCell::new(HashSet::new()),
    }
  }

  /// Provide the primitive method table to route calls like `arr.push(x)` through a custom
  /// runtime emitter when the method's descriptor sets `js_call`.
  pub fn with_primitive_methods(
    mut self,
    methods: &'static [PrimitiveMethodDescriptor],
  ) -> Self {
    self.primitive_methods = Some(methods);
    self
  }

  /// Snapshot of the runtime ids recorded during this generator's `generate_js`. Stable
  /// identifiers (e.g. `"primitive_list"`) keyed off `JsRuntimeAsset::id`.
  pub fn used_runtime_ids(&self) -> HashSet<&'static str> {
    self.used_runtime_ids.borrow().clone()
  }

  fn lookup_primitive(
    &self,
    receiver_ty: &Ty,
    name: &str,
  ) -> Option<&'static PrimitiveMethodDescriptor> {
    let methods = self.primitive_methods?;
    let kind = match receiver_ty {
      Ty::Array(_) => PrimitiveReceiverKind::Array,
      _ => return None,
    };
    methods
      .iter()
      .find(|d| d.receiver == kind && d.name == name)
  }

  /// Skip emitting defs for these names (stdlib builtins like print; calls are special-cased in generate_expr).
  pub fn with_external_functions(
    mut self,
    names: impl IntoIterator<Item = String>,
  ) -> Self {
    self.external_functions = Some(names.into_iter().collect());
    self
  }

  /// Configure generator to emit a dependency module's pub defs with a prefix (for bundling).
  pub fn with_prefix(
    mut self,
    prefix: &str,
    pub_names: std::collections::HashSet<String>,
  ) -> Self {
    self.prefix = Some(prefix.to_string());
    self.module_pub_names = Some(pub_names);
    self
  }

  /// Configure generator to use prefixed names for imported refs (for bundling entry).
  pub fn with_import_map(mut self, map: HashMap<String, (String, String)>) -> Self {
    self.import_map = Some(map);
    self
  }

  fn prefixed_var(&self, var: &str) -> String {
    if let Some(map) = &self.import_map {
      if let Some((mod_name, export)) = map.get(var) {
        return format!("__{}__{}", mod_name, export);
      }
    }
    if let (Some(prefix), Some(pub_names)) = (&self.prefix, &self.module_pub_names) {
      if pub_names.contains(var) {
        return format!("{}{}", prefix, var);
      }
    }
    var.to_string()
  }

  pub fn generate_js(&self) -> String {
    let mut stmts: Vec<String> = Vec::new();
    let defs = self.tycheck.ty_db.defs_iter();
    for (name, stmt) in defs {
      if self
        .external_functions
        .as_ref()
        .is_some_and(|ext| ext.contains(name))
      {
        continue;
      }
      if let (Some(_), Some(pub_names)) = (&self.prefix, &self.module_pub_names) {
        if !pub_names.contains(name) {
          continue;
        }
      }
      let emit_name = if let Some(ref p) = self.prefix {
        format!("{}{}", p, name)
      } else {
        name.clone()
      };
      stmts.push(self.generate_stmt(&emit_name, stmt));
    }
    stmts.join("\n")
  }

  pub fn generate_stmt(&self, name: &str, stmt: &TypedStmt) -> String {
    match stmt {
      TypedStmt::VariableDef { value, pub_vis: _, .. } => {
        let value = self.generate_expr(value);
        format!("const {} = {};", name, value)
      }
      TypedStmt::StructDef { .. } => String::new(),
      TypedStmt::TraitDef { .. } => String::new(),
      TypedStmt::ImplDef { .. } => String::new(),
      TypedStmt::FunctionDef { value, pub_vis: _, .. } => {
        let (_stmt_name, params, body, _body_hir, _ty) = match self.tycheck.ty_db.expr(value) {
          TypedExpr::FunctionDef(FunctionDef {
            name: n,
            params,
            body,
            body_hir,
            ty,
            closure_scope: _,
          }) => (n, params, body, body_hir, ty),
          _ => unreachable!(),
        };
        let params: Vec<String> = params.iter().map(|(k, _v)| k.clone()).collect();
        let params = params.join(", ");
        let body = self.generate_expr(body);
        format!("function {}({}) {{ return {}; }}", name, params, body)
      }
      TypedStmt::Expr(expr) => {
        let expr = self.generate_expr(expr);
        format!("{};", expr)
      }
    }
  }

  pub fn generate_expr(&self, expr: &TypedDatabaseIdx) -> String {
    let expr_node = self.tycheck.ty_db.expr(expr);
    let ty = expr_node.ty();
    if ty.has_value() {
      if let Some(s) = ty_as_js_literal(&ty) {
        return s;
      }
    }
    let expr = match expr_node {
      TypedExpr::VariableRef { var, .. } => self.prefixed_var(var),
      TypedExpr::FunctionParameter { name, ty } => name.to_string(),
      TypedExpr::FunctionCall { def, args, ty: _, ret: _ } => {
        let args_str = args
          .iter()
          .map(|a| self.generate_expr(a))
          .collect::<Vec<_>>()
          .join(", ");

        if let TypedExpr::ObjectFieldAccess { object, field, .. } = self.tycheck.ty_db.expr(def) {
          let recv = self.generate_expr(object);
          let recv_ty = self.tycheck.ty_db.expr(object).ty();
          if let Some(desc) = self.lookup_primitive(&recv_ty, field.as_str()) {
            if let Some(emit) = desc.js_call {
              if let Some(rt) = desc.js_runtime {
                self.used_runtime_ids.borrow_mut().insert(rt.id);
              }
              let args_v: Vec<String> =
                args.iter().map(|a| self.generate_expr(a)).collect();
              return emit(&recv, &args_v);
            }
          }
          return format!("{}.{}({})", recv, field, args_str);
        }

        let callee = match self.tycheck.ty_db.expr(def) {
          TypedExpr::FunctionDef(FunctionDef { name, .. }) => name.as_deref().unwrap_or(""),
          TypedExpr::VariableRef { var, .. } => var.as_str(),
          TypedExpr::StructConstructor { name } => name.as_str(),
          _ => "",
        };

        if callee == "print" {
          format!("console.log({})", args_str)
        } else {
          let lhs = match self.tycheck.ty_db.expr(def) {
            TypedExpr::FunctionDef(FunctionDef { name, .. }) => match name {
              Some(s) => s.clone(),
              None => "".to_string(),
            },
            TypedExpr::VariableRef { var, .. } => var.clone(),
            TypedExpr::StructConstructor { name } => name.clone(),
            _ => {
              let expr = self.tycheck.ty_db.expr(def);
              expr.ty().to_string()
            }
          };
          format!("{}({})", lhs, args_str)
        }
      }
      TypedExpr::Number { val: Some(val) } => val.to_string(),
      TypedExpr::Number { val } => todo!(),
      TypedExpr::String { val } => match val {
        Some(s) => format!("\"{}\"", s),
        None => "\"\"".to_string(),
      },
      TypedExpr::Boolean { val } => todo!(),
      TypedExpr::Array { vals, .. } => {
        let vals = vals
          .as_ref()
          .unwrap()
          .iter()
          .map(|a| self.generate_expr(a))
          .collect::<Vec<_>>()
          .join(", ");
        format!("[{}]", vals)
      }
      TypedExpr::Binary { op, lhs, rhs, .. } => {
        let lhs = self.generate_expr(lhs);
        let rhs = self.generate_expr(rhs);
        format!("{} {} {}", lhs, op, rhs)
      }
      TypedExpr::Unary { op, expr, .. } => {
        let expr = self.generate_expr(expr);
        format!("{}{}", op, expr)
      }
      TypedExpr::Block { stmts, .. } => {
        let stmts = stmts
          .iter()
          .map(|s| self.generate_stmt("", s))
          .collect::<Vec<_>>();
        // return last statment
        let last = stmts.last().unwrap();
        let stmts = stmts[..stmts.len() - 1].join("\n return ");
        format!("(() => {{ {} }}())", stmts)
      }
      TypedExpr::FunctionDef(FunctionDef {
        name,
        params,
        body,
        body_hir,
        ty,
        closure_scope,
      }) => {
        let params: Vec<String> = params.iter().map(|(k, v)| k.clone()).collect();
        let params_str = if params.len() == 1 {
          params.join("")
        } else {
          format!("({})", params.join(", "))
        };
        let params = params.join(", ");
        let body_str = self.generate_expr(body);

        if let Ty::Object(_) = self.tycheck.ty_db.expr(body).ty() {
          format!("{} => ({})", params, body_str)
        } else {
          format!("{} => {}", params, body_str)
        }
      }
      TypedExpr::Conditional {
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        let condition = self.generate_expr(condition);
        let then_branch = self.generate_expr(then_branch);
        let else_branch = self.generate_expr(else_branch);
        format!("{} ? {} : {}", condition, then_branch, else_branch)
      }
      TypedExpr::Object { fields, .. } => {
        let fields = fields
          .iter()
          .map(|(k, v)| format!("{}: {}", k, self.generate_expr(v)))
          .collect::<Vec<_>>()
          .join(", ");
        format!("{{ {} }}", fields)
      }
      TypedExpr::ObjectFieldAccess { object, field, .. } => {
        let object = self.generate_expr(object);
        format!("{}.{}", object, field)
      }
      TypedExpr::StructConstructor { name } => name.clone(),
      TypedExpr::StructInstance { .. } => "({})".to_string(),
      TypedExpr::For {
        binding,
        iterable,
        where_clause,
        acc_init,
        fold_acc,
        fold_index,
        body,
        ..
      } => {
        let iter_e = self.generate_expr(iterable);
        let idx_var = fold_index
          .clone()
          .unwrap_or_else(|| "__ds_k".to_string());
        let where_guard = match where_clause.as_ref() {
          Some(w) => {
            let w_e = self.generate_expr(w);
            format!("if (!({})) continue;\n          ", w_e)
          }
          None => String::new(),
        };
        let body_e = self.generate_expr(body);
        let (setup, ret_name) = if let Some(acc_n) = fold_acc {
          let init_e = acc_init
            .as_ref()
            .map(|i| self.generate_expr(i))
            .unwrap_or_else(|| "undefined".to_string());
          (
            format!("let {} = {};", acc_n, init_e),
            acc_n.clone(),
          )
        } else {
          let setup_line = match acc_init.as_ref() {
            Some(i) => format!("let __ds_out = {};", self.generate_expr(i)),
            None => "let __ds_out;".to_string(),
          };
          (setup_line, "__ds_out".to_string())
        };
        format!(
          "(((__ds_iter) => {{\n        {}\n        for (let {} = 0; {} < __ds_iter.length; {}++) {{\n          const {} = __ds_iter[{}];\n          {}{} = {};\n        }}\n        return {};\n      }})({}))",
          setup,
          idx_var,
          idx_var,
          idx_var,
          binding,
          idx_var,
          where_guard,
          ret_name,
          body_e,
          ret_name,
          iter_e
        )
      }
      TypedExpr::Unresolved => todo!(),
      TypedExpr::Error => todo!("typedexpr errorrrr"),
    };
    expr
  }
}
