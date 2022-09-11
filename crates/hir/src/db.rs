use crate::{
  expr::{BinaryOp, Expr, UnaryOp},
  stmt::Stmt,
};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::SyntaxKind;

#[derive(Debug, Default)]
pub struct Database {
  exprs: Arena<Expr>,
  defs: FxHashMap<String, Stmt>,
}

impl Database {
  pub fn get_expr(&self, idx: &Idx<Expr>) -> &Expr {
    &self.exprs[*idx]
  }

  pub fn get_def_by_name(&self, name: &str) -> Option<&Stmt> {
    self.defs.get(name)
  }

  pub fn defs_iter(&self) -> Vec<(&String, &Stmt)> {
    let mut defs: Vec<(&String, &Stmt)> = self.defs.iter().map(|(k, v)| (k, v)).collect();
    defs.reverse(); // This doesn't seem tenable but it works for now
    defs
  }

  pub fn lower(&mut self, ast: ast::Root) {
    #[allow(clippy::needless_collect)] // Suggestion doesn't work due to https://doc.rust-lang.org/error-index.html#E0501
    let stmts: Vec<Stmt> = ast
      .stmts()
      .filter_map(|stmt| self.lower_stmt(stmt))
      .collect();
    for stmt in stmts.into_iter() {
      if let Stmt::VariableDef { ref name, value: _ } = stmt {
        self.defs.insert(name.clone(), stmt);
      }
    }
  }

  pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
    let result = match ast {
      ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
        name: ast.name()?.text().to_string(),
        value: self.lower_expr(ast.value()),
      },
      ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
    };

    Some(result)
  }

  pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
    if let Some(ast) = ast {
      match ast {
        ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
        ast::Expr::NumberLit(ast) => Expr::Number { n: ast.parse() },
        ast::Expr::StringLit(ast) => Expr::String { s: ast.parse() },
        ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
        ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
        ast::Expr::VariableRef(ast) => Expr::VariableRef { var: ast.name() },
      }
    } else {
      Expr::Missing
    }
  }

  fn lower_binary(&mut self, ast: ast::expr::BinaryExpr) -> Expr {
    let op = match ast.op().unwrap().kind() {
      SyntaxKind::Plus => BinaryOp::Add,
      SyntaxKind::Minus => BinaryOp::Sub,
      SyntaxKind::Asterisk => BinaryOp::Mul,
      SyntaxKind::ForwardSlash => BinaryOp::Div,
      _ => unreachable!(),
    };
    let lhs = self.lower_expr(ast.lhs());
    let rhs = self.lower_expr(ast.rhs());
    Expr::Binary {
      op,
      lhs: self.exprs.alloc(lhs),
      rhs: self.exprs.alloc(rhs),
    }
  }

  fn lower_unary(&mut self, ast: ast::expr::UnaryExpr) -> Expr {
    let op = match ast.op().unwrap().kind() {
      SyntaxKind::Minus => UnaryOp::Neg,
      _ => unreachable!(),
    };

    let expr = self.lower_expr(ast.expr());
    Expr::Unary {
      op,
      expr: self.exprs.alloc(expr),
    }
  }
}
