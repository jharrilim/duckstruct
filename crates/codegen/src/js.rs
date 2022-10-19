#![allow(dead_code, unused)]

/// Generate javascript code from the typed hir
use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{FunctionDef, TypedExpr, TypedStmt},
  TyCheck,
};

pub struct JsGenerator<'tycheck> {
  tycheck: &'tycheck TyCheck,
}

impl<'tycheck> JsGenerator<'tycheck> {
  pub fn new(tycheck: &'tycheck TyCheck) -> Self {
    Self { tycheck }
  }

  pub fn generate_js(&self) -> String {
    let mut stmts: Vec<String> = Vec::new();
    for (name, stmt) in self.tycheck.ty_db.defs_iter() {
      stmts.push(self.generate_stmt(name, stmt));
    }
    stmts.join("\n")
  }

  pub fn generate_stmt(&self, name: &str, stmt: &TypedStmt) -> String {
    match stmt {
      TypedStmt::VariableDef { name, value } => {
        let value = self.generate_expr(value);
        format!("const {} = {};", name, value)
      }
      TypedStmt::FunctionDef { name, value } => {
        let (name, params, body, body_hir, ty) = match self.tycheck.ty_db.expr(value) {
          TypedExpr::FunctionDef(FunctionDef {
            name,
            params,
            body,
            body_hir,
            ty,
            closure_scope: _,
          }) => (name, params, body, body_hir, ty),
          _ => unreachable!(),
        };
        let params: Vec<String> = params.iter().map(|(k, v)| k.clone()).collect();
        let params = params.join(", ");
        let body = self.generate_expr(body);
        let name = match name {
          Some(s) => s.clone(),
          None => "".to_string(),
        };
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
          var.to_string()
        }
      }
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
      TypedExpr::Block { stmts, ty } => todo!(),
      TypedExpr::FunctionDef(FunctionDef {
        name,
        params,
        body,
        body_hir,
        ty,
        closure_scope,
      }) => {
        if self.tycheck.ty_db.expr(body).ty().has_value() {
          format!("{}", self.tycheck.ty_db.expr(body).ty())
        } else {
          let params: Vec<String> = params.iter().map(|(k, v)| k.clone()).collect();
          let params = params.join(", ");
          let body = self.generate_expr(body);
          let name = match name {
            Some(s) => s.clone(),
            None => "".to_string(),
          };
          format!("function {}({}) {{ return {}; }}", name, params, body)
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
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let fields = fields
            .iter()
            .map(|(k, v)| format!("{}: {}", k, self.generate_expr(v)))
            .collect::<Vec<_>>()
            .join(", ");
          format!("{{{}}}", fields)
        }
      }
      TypedExpr::ObjectFieldAccess { object, field, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let object = self.generate_expr(object);
          format!("{}.{}", object, field)
        }
      }
      TypedExpr::Unresolved => todo!(),
      TypedExpr::Error => todo!(),
    };
    expr
  }
}
