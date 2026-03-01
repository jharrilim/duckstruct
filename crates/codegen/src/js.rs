#![allow(dead_code, unused)]

/// Generate javascript code from the typed hir
use std::collections::HashMap;

use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{FunctionDef, Ty, TypedExpr, TypedStmt},
  TyCheck,
};

use crate::CodeGenerator;

pub struct JsGenerator<'tycheck> {
  tycheck: &'tycheck TyCheck,
  /// When generating a dependency module's bundle, prefix for top-level names and refs.
  prefix: Option<String>,
  /// When set with prefix, only emit these (pub) def names; and VariableRef to these get prefixed.
  module_pub_names: Option<std::collections::HashSet<String>>,
  /// When generating entry with imports: map local name -> (module_name, export_name) for prefixed emit.
  import_map: Option<HashMap<String, (String, String)>>,
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
    }
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
    let hir_db = self.tycheck.hir_db.clone();
    let expr = match self.tycheck.ty_db.expr(expr) {
      TypedExpr::VariableRef { var, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          self.prefixed_var(var)
        }
      }
      TypedExpr::FunctionParameter { name, ty } => name.to_string(),
      TypedExpr::FunctionCall { def, args, ty, ret } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let args = args
            .iter()
            .map(|a| self.generate_expr(a))
            .collect::<Vec<_>>()
            .join(", ");

          let lhs = match self.tycheck.ty_db.expr(def) {
            TypedExpr::FunctionDef(FunctionDef { name, .. }) => match name {
              Some(s) => s.clone(),
              None => "".to_string(),
            },
            TypedExpr::VariableRef { var, .. } => var.clone(),
            _ => {
              let expr = self.tycheck.ty_db.expr(def);
              expr.ty().to_string()
            }
          };

          format!("{}({})", lhs, args)
        }
      }
      TypedExpr::Number { val: Some(val) } => val.to_string(),
      TypedExpr::Number { val } => todo!(),
      TypedExpr::String { val } => match val {
        Some(s) => format!("\"{}\"", s),
        None => "\"\"".to_string(),
      },
      TypedExpr::Boolean { val } => todo!(),
      TypedExpr::Array { vals, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let vals = vals
            .as_ref()
            .unwrap()
            .iter()
            .map(|a| self.generate_expr(a))
            .collect::<Vec<_>>()
            .join(", ");
          format!("[{}]", vals)
        }
      }
      TypedExpr::Binary { op, lhs, rhs, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let lhs = self.generate_expr(lhs);
          let rhs = self.generate_expr(rhs);
          format!("{} {} {}", lhs, op, rhs)
        }
      }
      TypedExpr::Unary { op, expr, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let expr = self.generate_expr(expr);
          format!("{}{}", op, expr)
        }
      }
      TypedExpr::Block { stmts, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let stmts = stmts
            .iter()
            .map(|s| self.generate_stmt("", s))
            .collect::<Vec<_>>();
          // return last statment
          let last = stmts.last().unwrap();
          let stmts = stmts[..stmts.len() - 1].join("\n return ");
          format!("(() => {{ {} }}())", stmts)
        }
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
        ty,
      } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let condition = self.generate_expr(condition);
          let then_branch = self.generate_expr(then_branch);
          let else_branch = self.generate_expr(else_branch);
          format!("{} ? {} : {}", condition, then_branch, else_branch)
        }
      }
      TypedExpr::Object { fields, ty } => {
        println!("fields: {:?}", fields);
        println!("ty: {:?}", ty);
        let fields = fields
          .iter()
          .map(|(k, v)| format!("{}: {}", k, self.generate_expr(v)))
          .collect::<Vec<_>>()
          .join(", ");
        format!("{{ {} }}", fields)
      }
      TypedExpr::ObjectFieldAccess { object, field, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let object = self.generate_expr(object);
          format!("{}.{}", object, field)
        }
      }
      TypedExpr::For { .. } => todo!("Implement codegen for for loops"),
      TypedExpr::Unresolved => todo!(),
      TypedExpr::Error => todo!("typedexpr errorrrr"),
    };
    expr
  }
}
