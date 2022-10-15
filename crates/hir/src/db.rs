use std::hash::BuildHasherDefault;

use crate::{
  expr::{BinaryOp, Expr, UnaryOp},
  stmt::Stmt,
  DatabaseIdx,
};
use indexmap::IndexMap;
use la_arena::{Arena, Idx};
use rustc_hash::FxHasher;
use syntax::SyntaxKind;

type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;


#[derive(Debug, Default)]
pub struct Database {
  exprs: Arena<Expr>,
  defs: FxIndexMap<String, Stmt>,
}

impl Database {
  pub fn get_expr(&self, idx: &Idx<Expr>) -> &Expr {
    &self.exprs[*idx]
  }

  pub fn get_def_by_name(&self, name: &str) -> Option<&Stmt> {
    self.defs.get(name)
  }

  pub fn defs_iter(&self) -> Vec<(&String, &Stmt)> {
    let defs: Vec<(&String, &Stmt)> = self.defs.iter().map(|(k, v)| (k, v)).collect();
    defs
  }

  pub fn lower(&mut self, ast: ast::Root) {
    #[allow(clippy::needless_collect)]
    // Suggestion doesn't work due to https://doc.rust-lang.org/error-index.html#E0501
    let stmts: Vec<Stmt> = ast
      .stmts()
      .filter_map(|stmt| self.lower_stmt(stmt))
      .collect();
    for stmt in stmts.into_iter() {
      match stmt {
        Stmt::VariableDef { ref name, value: _ } => {
          self.defs.insert(name.clone(), stmt);
        }
        Stmt::FunctionDef { name, params, body } => {
          self
            .defs
            .insert(name.clone(), Stmt::FunctionDef { name, params, body });
        }
        Stmt::Expr(expr) => {
          self.defs.insert("".to_string(), Stmt::Expr(expr));
        }
      }
    }
  }

  pub(crate) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
    let result = match ast {
      ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
        name: ast.name()?.text().to_string(),
        value: self.lower_expr(ast.value()),
      },
      ast::Stmt::FunctionDef(ast) => Stmt::FunctionDef {
        name: ast.name()?.text().to_string(),
        params: ast.params().map(|param| param.text().to_string()).collect(),
        body: self.lower_expr(ast.body()),
      },
      ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
    };

    Some(result)
  }

  pub(crate) fn lower_expr(&mut self, ast: Option<ast::Expr>) -> DatabaseIdx {
    if let Some(ast) = ast {
      match ast {
        ast::Expr::NumberLit(ast) => self.exprs.alloc(Expr::Number { n: ast.parse() }),
        ast::Expr::StringLit(ast) => self.exprs.alloc(Expr::String { s: ast.parse() }),
        ast::Expr::VariableRef(ast) => self.exprs.alloc(Expr::VariableRef { var: ast.name() }),
        ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
        ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
        ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
        ast::Expr::Function(ast) => self.lower_function(ast),
        ast::Expr::FunctionCall(ast) => self.lower_function_call(ast),
        ast::Expr::Block(ast) => self.lower_block(ast),
      }
    } else {
      self.exprs.alloc(Expr::Missing)
    }
  }

  fn lower_block(&mut self, ast: ast::expr::Block) -> DatabaseIdx {
    let stmts: Vec<Stmt> = ast
      .stmts()
      .filter_map(|stmt| self.lower_stmt(stmt))
      .collect();
    self.exprs.alloc(Expr::Block { stmts })
  }

  fn lower_function_call(&mut self, ast: ast::expr::FunctionCall) -> DatabaseIdx {
    let args = ast
      .args()
      .map(|arg| self.lower_expr(Some(arg)))
      .collect::<Vec<_>>();

    let expr = Expr::FunctionCall {
      name: ast.name().map(|name| name.text().to_string()),
      args: args.into_iter().collect(),
    };
    self.exprs.alloc(expr)
  }

  fn lower_function(&mut self, ast: ast::expr::Function) -> DatabaseIdx {
    let expr = Expr::Function {
      name: ast.name().map(|name| name.text().to_string()),
      body: self.lower_expr(ast.body()),
      params: ast.params().map(|param| param.text().to_string()).collect(),
    };
    self.exprs.alloc(expr)
  }

  fn lower_binary(&mut self, ast: ast::expr::BinaryExpr) -> DatabaseIdx {
    let op = match ast.op().unwrap().kind() {
      SyntaxKind::Plus => BinaryOp::Add,
      SyntaxKind::Minus => BinaryOp::Sub,
      SyntaxKind::Asterisk => BinaryOp::Mul,
      SyntaxKind::ForwardSlash => BinaryOp::Div,
      SyntaxKind::DoubleEquals => BinaryOp::Eq,
      _ => unreachable!(),
    };
    let lhs = self.lower_expr(ast.lhs());
    let rhs = self.lower_expr(ast.rhs());
    let expr = Expr::Binary { op, lhs, rhs };
    self.exprs.alloc(expr)
  }

  fn lower_unary(&mut self, ast: ast::expr::UnaryExpr) -> DatabaseIdx {
    let op = match ast.op().unwrap().kind() {
      SyntaxKind::Minus => UnaryOp::Neg,
      SyntaxKind::Bang => UnaryOp::Not,
      _ => unreachable!(),
    };

    let expr = Expr::Unary {
      op,
      expr: self.lower_expr(ast.expr()),
    };
    self.exprs.alloc(expr)
  }
}
