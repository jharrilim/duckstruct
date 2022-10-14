/// Generate javascript code from the typed hir
use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{TypedExpr, TypedStmt},
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
          TypedExpr::FunctionDef {
            name,
            params,
            body,
            body_hir,
            ty,
          } => (name, params, body, body_hir, ty),
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
          format!("{}", var)
        }
      }
      TypedExpr::FunctionCall {
        def,
        args,
        ty,
        name,
        ..
      } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let args = args
          .iter()
          .map(|a| self.generate_expr(a))
          .collect::<Vec<_>>()
          .join(", ");

        let name = match name {
          Some(s) => s.clone(),
          None => "".to_string(),
        };

        format!("{}({})", name, args)
        }
      }
      TypedExpr::Number { val: Some(val) } => val.to_string(),
      TypedExpr::Number { val } => todo!(),
      TypedExpr::String { val } => todo!(),
      TypedExpr::Boolean { val } => todo!(),
      TypedExpr::Array { val } => todo!(),
      TypedExpr::Binary { op, lhs, rhs, ty } => {
        if ty.has_value() {
          format!("{}", ty)
        } else {
          let lhs = self.generate_expr(lhs);
          let rhs = self.generate_expr(rhs);
          format!("{} {} {}", lhs, op, rhs)
        }
      },
      TypedExpr::Unary { op, expr, ty } => todo!(),
      TypedExpr::Block { stmts, ty } => todo!(),
      TypedExpr::FunctionDef {
        name,
        params,
        body,
        body_hir,
        ty,
      } => todo!(),
      TypedExpr::Unresolved => todo!(),
      TypedExpr::Error => todo!(),
    };
    expr
  }
}
