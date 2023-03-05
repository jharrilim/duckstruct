use crate::typed_hir::{TypedExpr, TypedStmt, Ty};
use data_structures::arena::{Arena, Idx};
use data_structures::FxIndexMap;

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

  pub fn expr_replace(&mut self, idx: &TypedDatabaseIdx, expr: TypedExpr) {
    self.exprs[*idx] = expr;
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

  pub fn edit_ty(&mut self, idx: &TypedDatabaseIdx, mut edit_fn: impl FnMut(Ty) -> Ty) {
    let expr = self.expr_mut(idx);
    let ty = edit_fn(expr.ty());
    expr.replace_ty(ty);
  }
}
