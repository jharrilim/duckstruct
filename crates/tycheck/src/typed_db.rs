use crate::typed_hir::{TypedExpr, TypedStmt};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

pub type TypedDatabaseIdx = Idx<TypedExpr>;
pub type TyContext = FxHashMap<String, TypedStmt>;

/// This is basically the same as the hir Database, but with types solved.
#[derive(Debug, Default)]
pub struct TypedDatabase {
  exprs: Arena<TypedExpr>,
  defs: TyContext,
}

impl TypedDatabase {
  pub fn alloc(&mut self, expr: TypedExpr) -> TypedDatabaseIdx {
    self.exprs.alloc(expr)
  }

  pub fn expr(&self, idx: &TypedDatabaseIdx) -> &TypedExpr {
    &self.exprs[*idx]
  }

  // Used for when lifetimes are the same
  pub fn exprs2(&self, idx1: &TypedDatabaseIdx, idx2: &TypedDatabaseIdx) -> (&TypedExpr, &TypedExpr) {
    (&self.exprs[*idx1], &self.exprs[*idx2])
  }

  pub fn define(&mut self, name: String, stmt: TypedStmt) {
    self.defs.insert(name, stmt);
  }

  pub fn def(&self, name: &str) -> Option<&TypedStmt> {
    self.defs.get(name)
  }
}
