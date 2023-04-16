mod operators;
mod ty;

use crate::{scope::Scope, typed_db::TypedDatabaseIdx};
use data_structures::FxIndexMap;
use hir::DatabaseIdx;
pub use operators::*;
pub use ty::*;

#[derive(Debug, Clone)]
pub enum TypedStmt {
  VariableDef {
    name: String,
    value: TypedDatabaseIdx,
  },
  FunctionDef {
    name: String,
    value: TypedDatabaseIdx,
  },
  Expr(TypedDatabaseIdx),
}

impl TypedStmt {
  pub fn value(&self) -> &TypedDatabaseIdx {
    match self {
      TypedStmt::VariableDef { value, .. } => value,
      TypedStmt::FunctionDef { value, .. } => value,
      TypedStmt::Expr(value) => value,
    }
  }
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
  Number {
    val: Option<f64>,
  },
  String {
    val: Option<String>,
  },
  Boolean {
    val: Option<bool>,
  },
  Array {
    vals: Option<Vec<TypedDatabaseIdx>>,
    ty: Ty,
  },
  Binary {
    op: BinaryOp,
    lhs: TypedDatabaseIdx,
    rhs: TypedDatabaseIdx,
    ty: Ty,
  },
  Unary {
    op: UnaryOp,
    expr: TypedDatabaseIdx,
    ty: Ty,
  },
  VariableRef {
    var: String,
    ty: Ty,
  },
  Block {
    stmts: Vec<TypedStmt>,
    ty: Ty,
  },
  FunctionDef(FunctionDef),
  FunctionCall {
    args: Vec<TypedDatabaseIdx>,
    def: TypedDatabaseIdx,
    ret: TypedDatabaseIdx,
    ty: Ty,
  },
  FunctionParameter {
    name: String,
    ty: Ty,
  },
  Conditional {
    condition: TypedDatabaseIdx,
    then_branch: TypedDatabaseIdx,
    else_branch: TypedDatabaseIdx,
    ty: Ty,
  },
  Object {
    fields: FxIndexMap<String, TypedDatabaseIdx>,
    ty: Ty,
  },
  ObjectFieldAccess {
    object: TypedDatabaseIdx,
    field: String,
    ty: Ty,
  },
  For {
    binding: String,
    iterable: TypedDatabaseIdx,
    where_clause: Option<TypedDatabaseIdx>,
    body: TypedDatabaseIdx,
    ty: Ty,
  },
  Unresolved,
  Error,
}

impl TypedExpr {
  pub fn ty(&self) -> Ty {
    match self {
      Self::Number { val } => Ty::Number(*val),
      Self::String { val } => Ty::String(val.as_ref().cloned()),
      Self::Boolean { val } => Ty::Boolean(*val),
      Self::Array { vals: _, ty } => ty.clone(),
      Self::Binary { ty, .. } => ty.clone(),
      Self::Unary { ty, .. } => ty.clone(),
      Self::VariableRef { ty, .. } => ty.clone(),
      Self::Block { ty, .. } => ty.clone(),
      Self::FunctionDef(FunctionDef { ty, .. }) => ty.clone(),
      Self::FunctionCall { ty, .. } => ty.clone(),
      Self::FunctionParameter { ty, .. } => ty.clone(),
      Self::Conditional { ty, .. } => ty.clone(),
      Self::Object { ty, .. } => ty.clone(),
      Self::ObjectFieldAccess { ty, .. } => ty.clone(),
      Self::For { ty, .. } => ty.clone(),
      Self::Unresolved => Ty::Generic,
      Self::Error => Ty::Error,
    }
  }

  pub fn ty_mut(&mut self) -> &mut Ty {
    match self {
      Self::Number { val: _ } => unimplemented!(),
      Self::String { val: _ } => unimplemented!(),
      Self::Boolean { val: _ } => unimplemented!(),
      Self::Array { vals: _, ty } => ty,
      Self::Binary { ty, .. } => ty,
      Self::Unary { ty, .. } => ty,
      Self::VariableRef { ty, .. } => ty,
      Self::Block { ty, .. } => ty,
      Self::FunctionDef(FunctionDef { ty, .. }) => ty,
      Self::FunctionCall { ty, .. } => ty,
      Self::FunctionParameter { ty, .. } => ty,
      Self::Conditional { ty, .. } => ty,
      Self::Object { ty, .. } => ty,
      Self::ObjectFieldAccess { ty, .. } => ty,
      Self::For { ty, ..} => ty,
      Self::Unresolved => unimplemented!(),
      Self::Error => unimplemented!(),
    }
  }

  pub fn replace_ty(&mut self, ty: Ty) {
    *self.ty_mut() = ty;
  }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
  pub name: Option<String>,
  pub params: FxIndexMap<String, TypedDatabaseIdx>,
  pub body: TypedDatabaseIdx,
  pub body_hir: DatabaseIdx,
  pub closure_scope: Scope,
  pub ty: Ty,
}
