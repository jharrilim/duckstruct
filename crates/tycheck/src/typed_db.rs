use crate::typed_hir::{TypedExpr, TypedStmt};
use data_structures::FxIndexMap;
use data_structures::arena::{Arena, Idx};

pub type TypedDatabaseIdx = Idx<TypedExpr>;
pub type TyContext = FxIndexMap<String, TypedStmt>;

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

  pub fn expr_mut(&mut self, idx: &TypedDatabaseIdx) -> &mut TypedExpr {
    &mut self.exprs[*idx]
  }

  // Used for when lifetimes are the same
  pub fn exprs2(
    &self,
    idx1: &TypedDatabaseIdx,
    idx2: &TypedDatabaseIdx,
  ) -> (&TypedExpr, &TypedExpr) {
    (&self.exprs[*idx1], &self.exprs[*idx2])
  }

  pub fn define(&mut self, name: String, stmt: TypedStmt) {
    self.defs.insert(name, stmt);
  }

  pub fn definition(&self, name: &str) -> Option<&TypedStmt> {
    self.defs.get(name)
  }

  pub fn definition_expr(&self, name: &str) -> Option<&TypedExpr> {
    let def = self.definition(name);
    def.map(|d| self.expr(d.value()))
  }

  pub fn defs_iter(&self) -> Vec<(&String, &TypedStmt)> {
    let vec: Vec<(&String, &TypedStmt)> = self.defs.iter().collect();
    vec
  }
}
