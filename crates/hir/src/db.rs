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

pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;

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
        Stmt::FunctionDef { name, value } => {
          self
            .defs
            .insert(name.clone(), Stmt::FunctionDef { name, value });
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
        value: self.lower_function(ast.into()),
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
        ast::Expr::BooleanLit(ast) => self.exprs.alloc(Expr::Boolean { b: ast.parse() }),
        ast::Expr::VariableRef(ast) => self.exprs.alloc(Expr::VariableRef { var: ast.name() }),
        ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
        ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
        ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
        ast::Expr::Function(ast) => self.lower_function(ast),
        ast::Expr::FunctionCall(ast) => self.lower_function_call(ast),
        ast::Expr::Block(ast) => self.lower_block(ast),
        ast::Expr::Array(ast) => self.lower_array(ast),
        ast::Expr::Conditional(ast) => self.lower_conditional(ast),
        ast::Expr::Object(ast) => self.lower_object(ast),
      }
    } else {
      self.exprs.alloc(Expr::Missing)
    }
  }

  fn lower_object(&mut self, ast: ast::expr::Object) -> DatabaseIdx {
    let mut fields = FxIndexMap::default();
    for (key, value) in ast.fields() {
      let value = self.lower_expr(value);
      fields.insert(key, value);
    }
    self.exprs.alloc(Expr::Object { fields })
  }

  fn lower_conditional(&mut self, ast: ast::expr::Conditional) -> DatabaseIdx {
    let condition = self.lower_expr(ast.predicate());
    let then_branch = self.lower_expr(ast.then_branch());
    let else_branch = self.lower_expr(ast.else_branch());
    self.exprs.alloc(Expr::Conditional {
      condition,
      then_branch,
      else_branch,
    })
  }

  fn lower_array(&mut self, ast: ast::expr::Array) -> DatabaseIdx {
    let vals = ast
      .elements()
      .map(|val| self.lower_expr(Some(val)))
      .collect::<Vec<_>>();
    self.exprs.alloc(Expr::Array { vals })
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

    let func = ast.func();
    match func {
      Some(func) => {
        let expr = Expr::FunctionCall {
          func: self.lower_expr(Some(func)),
          args,
        };
        self.exprs.alloc(expr)
      }
      None => todo!("Handle invalid function call in hir"),
    }
  }

  fn lower_function(&mut self, ast: ast::expr::Function) -> DatabaseIdx {
    let name = ast.name().map(|name| name.text().to_string());
    let expr = Expr::Function {
      body: self.lower_expr(ast.body()),
      name,
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
      SyntaxKind::NotEquals => BinaryOp::Neq,
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
