//! Type inference for the Duckstruct language. It does more than just inference.
//! It acts as a partial evaluator for code during type checking.
//! It also has the ability to refine parameter types based on the context of the function call.

use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostics::Diagnostics;
use crate::primitive_methods::PrimitiveMethodDescriptor;
use crate::primitive_methods::PrimitiveReceiverKind;
use crate::scope::Scope;
use crate::typed_db::{TypedDatabase, TypedDatabaseIdx};
use crate::typed_hir::{FunctionDef, Ty, TypedExpr, TypedStmt};
use ast::Expr as AstExpr;
use data_structures::{index_map, FxIndexMap};
use hir::{expr::Expr, pat::Pat, stmt::Stmt, DatabaseIdx};

/// Map from module name to its type-checked state (for resolving `foo::bar` and use bindings).
pub type ModuleMap<'a> = HashMap<String, &'a TyCheck>;

#[derive(Debug)]
pub enum Either<A, B> {
  Left(A),
  Right(B),
}

#[derive(Debug)]
pub struct TyCheck {
  pub hir_db: Rc<hir::Database>,
  pub ty_db: TypedDatabase,
  #[allow(unused)]
  pub diagnostics: Diagnostics,
  /// Types at **use sites** for variable and path reference HIR expressions.
  ///
  /// `infer_variable_ref` returns the defining expression's index, so hover cannot use the
  /// returned `TypedDatabaseIdx` alone to distinguish a reference from its definition.
  pub expr_ty_at_use_site: FxIndexMap<DatabaseIdx, Ty>,
  /// Built-in methods on primitives (e.g. array `length`), from `duckstruct-std` when compiling.
  pub(crate) primitive_methods: Option<&'static [PrimitiveMethodDescriptor]>,
}

impl TyCheck {
  pub fn new(hir_db: hir::Database) -> Self {
    Self {
      hir_db: Rc::new(hir_db),
      ty_db: TypedDatabase::default(),
      diagnostics: Diagnostics::default(),
      expr_ty_at_use_site: FxIndexMap::default(),
      primitive_methods: None,
    }
  }

  /// Type of a variable/path **reference** HIR expression, if recorded during inference.
  pub fn ty_at_hir_expr(&self, idx: &DatabaseIdx) -> Option<Ty> {
    self.expr_ty_at_use_site.get(idx).cloned()
  }

  /// Infers types for all statements in the HIR database.
  pub fn infer(&mut self) {
    self.infer_with_modules(None, None, None, None);
  }

  /// Infers types with an optional module map for resolving imports and path refs,
  /// an optional prelude (e.g. stdlib globals) injected into the initial scope,
  /// optional global external functions `(name, signature)` from `duckstruct-std` (each
  /// [`Ty`] must be [`Ty::Function`]), and optional built-in primitive methods.
  pub fn infer_with_modules(
    &mut self,
    module_map: Option<&ModuleMap<'_>>,
    prelude: Option<&[(String, TypedExpr)]>,
    external_functions: Option<&[(String, Ty)]>,
    primitive_methods: Option<&'static [PrimitiveMethodDescriptor]>,
  ) {
    self.primitive_methods = primitive_methods;
    let mut scope = Scope::default();
    if let Some(prelude_items) = prelude {
      for (name, expr) in prelude_items.iter() {
        let idx = self.ty_db.alloc(expr.clone());
        self.ty_db.define(
          name.clone(),
          TypedStmt::VariableDef {
            name: name.clone(),
            value: idx,
            pub_vis: false,
          },
        );
        scope.define(name.clone(), idx);
      }
    }
    if let Some(ext_fns) = external_functions {
      for (name, ty) in ext_fns.iter() {
        if !matches!(ty, Ty::Function { .. }) {
          debug_assert!(
            false,
            "duckstruct-std external `{name}` must use Ty::Function, got {ty}"
          );
          continue;
        }
        let idx = self.ty_db.alloc(TypedExpr::VariableRef {
          var: name.clone(),
          ty: ty.clone(),
        });
        self.ty_db.define(
          name.clone(),
          TypedStmt::VariableDef {
            name: name.clone(),
            value: idx,
            pub_vis: false,
          },
        );
        scope.define(name.clone(), idx);
      }
    }
    if let Some(map) = module_map {
      for (path, alias) in self.hir_db.uses.iter() {
        if path.len() >= 2 {
          let mod_key = path[0..path.len() - 1].join("::");
          if let Some(dep) = map.get(&mod_key) {
            let item_name = path.last().unwrap();
            if let Some(typed_stmt) = dep.ty_db.definition(item_name) {
              let is_pub = match typed_stmt {
                TypedStmt::VariableDef { pub_vis, .. }
                | TypedStmt::FunctionDef { pub_vis, .. }
                | TypedStmt::StructDef { pub_vis, .. } => {
                  *pub_vis
                }
                _ => false,
              };
              if is_pub {
                let ty = dep.ty_db.expr(typed_stmt.value()).ty().clone();
                let idx = self.ty_db.alloc(TypedExpr::VariableRef {
                  var: item_name.clone(),
                  ty,
                });
                let bind_name = alias.as_deref().unwrap_or(item_name);
                scope.define(bind_name.to_string(), idx);
              }
            }
          }
        }
      }
    }
    for (stmt_ident, statement) in self.hir_db.clone().defs_iter() {
      let typed_stmt = self.infer_stmt(&mut scope, Either::Left(statement), module_map);
      self.ty_db.define(stmt_ident.clone(), typed_stmt);
    }
  }

  fn lookup_primitive_descriptor(
    &self,
    receiver_ty: &Ty,
    field: &str,
  ) -> Option<&'static PrimitiveMethodDescriptor> {
    let slice = self.primitive_methods.as_ref()?;
    let kind = match receiver_ty {
      Ty::Array(_) => PrimitiveReceiverKind::Array,
      _ => return None,
    };
    slice
      .iter()
      .find(|d| d.receiver == kind && d.name == field)
  }

  fn is_primitive_builtin_field(&self, typed_object: &TypedDatabaseIdx, field: &str) -> bool {
    let ty = self.ty_db.expr(typed_object).ty();
    self.lookup_primitive_descriptor(&ty, field).is_some()
  }

  pub fn infer_stmt(
    &mut self,
    scope: &mut Scope,
    stmt: Either<&Stmt, &DatabaseIdx>,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedStmt {
    match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name, value, pub_vis } => {
          let value = self.infer_expr(scope, value, module_map);
          scope.define(name.clone(), value);
          TypedStmt::VariableDef {
            name: name.clone(),
            value,
            pub_vis: *pub_vis,
          }
        }
        Stmt::FunctionDef { name, value, pub_vis } => {
          self.infer_function_def(scope, name, value, *pub_vis, module_map)
        }
        Stmt::StructDef { name, pub_vis } => {
          let ctor = self.ty_db.alloc(TypedExpr::StructConstructor {
            name: name.clone(),
          });
          scope.define(name.clone(), ctor);
          TypedStmt::StructDef {
            name: name.clone(),
            value: ctor,
            pub_vis: *pub_vis,
          }
        }
        Stmt::Expr(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx, module_map)),
        Stmt::Use { .. } => todo!("use statements are resolved in infer_with_modules"),
      },
      Either::Right(expr_idx) => TypedStmt::Expr(self.infer_expr(scope, expr_idx, module_map)),
    }
  }

  pub fn infer_expr(
    &mut self,
    scope: &mut Scope,
    expr_idx: &DatabaseIdx,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let hir_db = Rc::clone(&self.hir_db);
    let expr = hir_db.get_expr(expr_idx);

    let expr = match expr {
      Expr::VariableRef { var, .. } => {
        let t = self.infer_variable_ref(scope, var);
        self
          .expr_ty_at_use_site
          .insert(*expr_idx, self.ty_db.expr(&t).ty().clone());
        return t;
      }
      Expr::Number { n, .. } => TypedExpr::Number { val: Some(*n) },
      Expr::String { s, .. } => TypedExpr::String {
        val: Some(s.clone()),
      },
      Expr::Block { stmts, .. } => return self.infer_block(scope, stmts, module_map),
      Expr::Boolean { b, .. } => TypedExpr::Boolean { val: Some(*b) },
      Expr::Binary {
        op,
        lhs,
        rhs,
        ast,
      } => {
        return self.infer_binary(scope, op, lhs, rhs, ast, module_map)
      }
      Expr::Unary { op, expr, ast } => {
        return self.infer_unary(scope, op, expr, ast, module_map)
      }
      Expr::Function {
        name,
        params,
        body,
        ast,
      } => return self.infer_function(scope, name, params, body, ast, module_map),
      Expr::FunctionCall { args, func, ast } => {
        return self.infer_function_call(scope, func, args, ast, module_map)
      }
      Expr::Array { vals, .. } => return self.infer_array(scope, vals, module_map),
      Expr::Conditional {
        condition,
        then_branch,
        else_branch,
        ast,
      } => return self.infer_conditional(
        scope,
        condition,
        then_branch,
        else_branch,
        ast,
        module_map,
      ),
      Expr::Object { fields, .. } => return self.infer_object(scope, fields, module_map),
      Expr::ObjectFieldAccess { object, field, ast } => {
        return self.infer_object_field_access(scope, object, field, ast, module_map)
      }
      Expr::StructLiteral {
        type_expr,
        fields,
        ast,
      } => return self.infer_struct_literal(scope, type_expr, fields, ast, module_map),
      Expr::PathRef { path, .. } => {
        let t = self.infer_path_ref(scope, path, module_map);
        self
          .expr_ty_at_use_site
          .insert(*expr_idx, self.ty_db.expr(&t).ty().clone());
        return t;
      }
      Expr::Missing => {
        todo!("Handle expression missing: {:?}", expr);
      }
      Expr::For {
        binding,
        iterable,
        where_clause,
        acc_init,
        fold_params,
        body,
        ast,
      } => {
        return self.infer_for(
          scope,
          binding,
          iterable,
          where_clause.as_ref(),
          acc_init.as_ref(),
          fold_params.as_ref(),
          body,
          ast,
          module_map,
        );
      }
    };
    self.ty_db.alloc(expr)
  }

  fn infer_path_ref(
    &mut self,
    scope: &mut Scope,
    path: &[String],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    if path.is_empty() {
      return self.ty_db.alloc(TypedExpr::Error);
    }
    if path.len() == 1 {
      return self.infer_variable_ref(scope, &path[0]);
    }
    if let Some(map) = module_map {
      let mod_key = path[0..path.len() - 1].join("::");
      if let Some(dep) = map.get(&mod_key) {
        let item_name = path.last().unwrap();
        if let Some(typed_stmt) = dep.ty_db.definition(item_name) {
          let is_pub = match typed_stmt {
            TypedStmt::VariableDef { pub_vis, .. }
            | TypedStmt::FunctionDef { pub_vis, .. }
            | TypedStmt::StructDef { pub_vis, .. } => *pub_vis,
            _ => false,
          };
          if is_pub {
            let ty = dep.ty_db.expr(typed_stmt.value()).ty().clone();
            return self.ty_db.alloc(TypedExpr::VariableRef {
              var: item_name.clone(),
              ty,
            });
          }
        }
      }
    }
    self.ty_db.alloc(TypedExpr::Error)
  }

  fn infer_struct_literal(
    &mut self,
    scope: &mut Scope,
    type_expr: &DatabaseIdx,
    fields: &FxIndexMap<String, DatabaseIdx>,
    ast: &ast::expr::StructLiteral,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    if !fields.is_empty() {
      self.diagnostics.push_error(
        "type::error",
        "struct literals with fields are not yet supported".to_string(),
        ast.span(),
      );
      return self.ty_db.alloc(TypedExpr::Error);
    }

    let hir_db = Rc::clone(&self.hir_db);
    let struct_name = match hir_db.get_expr(type_expr) {
      Expr::VariableRef { var, .. } => {
        let Some(def) = scope.def(var) else {
          self
            .diagnostics
            .push_error("type::error", format!("undefined `{}`", var), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        match self.ty_db.expr(&def) {
          TypedExpr::StructConstructor { name } => name.clone(),
          _ => {
            self.diagnostics.push_error("type::error",
              format!("`{}` is not a struct type", var),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
        }
      }
      Expr::PathRef { path, .. } => {
        let Some(map) = module_map else {
          self
            .diagnostics
            .push_error("type::error", "unknown module path".to_string(), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        if path.len() < 2 {
          self
            .diagnostics
            .push_error("type::error", "invalid struct literal type".to_string(), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        }
        let mod_key = path[0..path.len() - 1].join("::");
        let item_name = path.last().unwrap();
        let Some(dep) = map.get(&mod_key) else {
          self.diagnostics.push_error("type::error",
            format!("unknown module `{}`", mod_key),
            ast.span(),
          );
          return self.ty_db.alloc(TypedExpr::Error);
        };
        let Some(typed_stmt) = dep.ty_db.definition(item_name) else {
          self
            .diagnostics
            .push_error("type::error", format!("unknown item `{}`", item_name), ast.span());
          return self.ty_db.alloc(TypedExpr::Error);
        };
        match typed_stmt {
          TypedStmt::StructDef {
            name,
            pub_vis: true,
            ..
          } => name.clone(),
          TypedStmt::StructDef { pub_vis: false, .. } => {
            self.diagnostics.push_error("type::error",
              format!("struct `{}` is not public", item_name),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
          _ => {
            self.diagnostics.push_error("type::error",
              format!("`{}` is not a struct in this module", item_name),
              ast.span(),
            );
            return self.ty_db.alloc(TypedExpr::Error);
          }
        }
      }
      _ => {
        self
          .diagnostics
          .push_error("type::error", "invalid struct literal type".to_string(), ast.span());
        return self.ty_db.alloc(TypedExpr::Error);
      }
    };

    self.ty_db.alloc(TypedExpr::StructInstance { name: struct_name })
  }

  fn infer_variable_ref(&mut self, scope: &mut Scope, var: &str) -> TypedDatabaseIdx {
    // Use this as a way to prevent infinite recursion during type inference
    // on a function definition.
    if scope.is_late_binding(var) {
      return self.ty_db.alloc(TypedExpr::VariableRef {
        var: var.to_string(),
        ty: Ty::Generic,
      });
    }

    match scope.def(var) {
      Some(t) => t,
      None => self.ty_db.alloc(TypedExpr::Error),
    }
  }

  fn infer_object_field_access(
    &mut self,
    scope: &mut Scope,
    object: &DatabaseIdx,
    field: &str,
    ast: &ast::expr::ObjectFieldAccess,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let typed_object = self.infer_expr(scope, object, module_map);
    let field_ty = match self.ty_db.expr(&typed_object).ty() {
      Ty::Object(Some(fields)) => match fields.get(field) {
        Some(ty) => ty.clone(),
        None => Ty::Generic,
      },
      Ty::Object(None) => {
        todo!("handle object with unknown fields");
      }
      Ty::Generic => Ty::Generic,
      Ty::Array(_) => {
        let receiver_ty = self.ty_db.expr(&typed_object).ty().clone();
        if let Some(desc) = self.lookup_primitive_descriptor(&receiver_ty, field) {
          (desc.signature)(&receiver_ty).unwrap_or_else(|| {
            self.diagnostics.push_error(
              "type::error",
              format!(
                "built-in method `{}` is not valid for receiver type {}",
                field, receiver_ty
              ),
              ast.span(),
            );
            Ty::Error
          })
        } else {
          let msg = if self.primitive_methods.is_none() {
            format!(
              "Unknown method `{}` on array (built-in array methods need the compiler stdlib table)",
              field
            )
          } else {
            format!("Unknown method `{}` on array", field)
          };
          self.diagnostics.push_error("type::error", msg, ast.span());
          Ty::Error
        }
      }
      ty => {
        match self.query_object_name(object) {
          Some(name) => {
            if let Some(similar_name) = scope.def_name_similar_to(name) {
              self.diagnostics.push_error("type::error",
                format!(
                  "Cannot access field `{}` on {}. Did you mean `{}`?",
                  field, name, similar_name
                ),
                ast.span(),
              );
            } else {
              self.diagnostics.push_error("type::error",
                format!("Cannot access field `{}` on {}.", field, name),
                ast.span(),
              );
            }
          }
          None => {
            self.diagnostics.push_error("type::error",
              format!("Cannot access field `{}` on {}.", field, ty),
              ast.span(),
            );
          }
        }
        Ty::Error
      }
    };

    if !self.is_primitive_builtin_field(&typed_object, field) {
      self.propogate_object_field_constraint(scope, &typed_object, field, &field_ty, ast);
    }

    self.ty_db.alloc(TypedExpr::ObjectFieldAccess {
      object: typed_object,
      field: field.to_string(),
      ty: field_ty,
    })
  }

  fn propogate_object_field_constraint(
    &mut self,
    scope: &mut Scope,
    object: &TypedDatabaseIdx,
    field: &str,
    field_ty: &Ty,
    ast: &ast::expr::ObjectFieldAccess,
  ) {
    if self.is_primitive_builtin_field(object, field) {
      return;
    }
    // Propagate the type of the field to the object.
    match self.ty_db.expr(object) {
      TypedExpr::ObjectFieldAccess {
        object,
        field: inner_field,
        ..
      } => match self.ty_db.expr(object) {
        TypedExpr::Object { fields, .. } => {
          if let Some(field_expr) = fields.get(inner_field) {
            self.propogate_object_field_constraint(scope, &field_expr.clone(), field, field_ty, ast)
          } else {
            todo!("Doesn't seem possible?")
          }
        }
        _ => todo!("Doesn't seem possible?"),
      },
      TypedExpr::FunctionCall { ret, .. } => {
        self.propogate_object_field_constraint(scope, &ret.clone(), field, field_ty, ast)
      }
      TypedExpr::Block { stmts, .. } => {
        if let Some(last_typed_stmt) = stmts.last() {
          self.propogate_object_field_constraint(
            scope,
            &last_typed_stmt.value().clone(),
            field,
            field_ty,
            ast,
          );
        }
      }
      TypedExpr::FunctionParameter { name: var, .. } | TypedExpr::VariableRef { var, .. } => {
        if let Some(param_idx) = scope.def(var) {
          self
            .ty_db
            .edit_ty(&param_idx, |original_ty| match original_ty {
              Ty::Object(Some(fields)) => {
                let mut fields = fields;
                fields.insert(field.to_string(), field_ty.clone());
                Ty::Object(Some(fields))
              }
              Ty::Object(None) => {
                let fields = index_map!(field.to_string() => field_ty.clone());
                Ty::Object(Some(fields))
              }
              Ty::Generic => {
                let fields = index_map!(field.to_string() => field_ty.clone());
                Ty::Object(Some(fields))
              }
              _ => {
                self.diagnostics.push_error("type::error",
                  format!(
                    "Cannot access field `{}` on non-object type. Type: {:?}",
                    field, original_ty
                  ),
                  ast.span(),
                );
                Ty::Error
              }
            });
        }
      }
      _ => {}
    }
  }

  fn infer_object(
    &mut self,
    scope: &mut Scope,
    fields: &FxIndexMap<String, DatabaseIdx>,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let mut tys: FxIndexMap<String, Ty> = FxIndexMap::default();
    let mut typed_fields: FxIndexMap<String, TypedDatabaseIdx> = FxIndexMap::default();

    for (field, expr) in fields.iter() {
      let expr = self.infer_expr(scope, expr, module_map);
      let ty = self.ty_db.expr(&expr).ty();

      tys.insert(field.clone(), ty);
      typed_fields.insert(field.clone(), expr);
    }
    self.ty_db.alloc(TypedExpr::Object {
      fields: typed_fields,
      ty: Ty::Object(Some(tys)),
    })
  }

  fn infer_conditional(
    &mut self,
    scope: &mut Scope,
    condition: &DatabaseIdx,
    then_branch: &DatabaseIdx,
    else_branch: &DatabaseIdx,
    ast: &ast::expr::Conditional,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let condition = self.infer_expr(scope, condition, module_map);
    match self.ty_db.expr(&condition).ty() {
      Ty::Boolean(Some(boolean)) => {
        if boolean {
          self.infer_expr(scope, then_branch, module_map)
        } else {
          self.infer_expr(scope, else_branch, module_map)
        }
      }
      Ty::Boolean(None) => {
        let then_branch = self.infer_expr(scope, then_branch, module_map);
        let else_branch = self.infer_expr(scope, else_branch, module_map);
        let then_ty = self.ty_db.expr(&then_branch).ty();
        let else_ty = self.ty_db.expr(&else_branch).ty();

        if !then_ty.type_eq(&else_ty) {
          self.diagnostics.push_error("type::error",
            format!(
              "Type mismatch in conditional expression: {} and {}",
              then_ty, else_ty
            ),
            ast.span(),
          );
        }

        let ty = then_ty.deconst();
        let expr = TypedExpr::Conditional {
          condition,
          then_branch,
          else_branch,
          ty,
        };
        self.ty_db.alloc(expr)
      }
      _ => {
        self
          .diagnostics
          .push_error("type::error", "Condition must be a boolean".to_string(), ast.span());
        let then_branch = self.ty_db.alloc(TypedExpr::Error);
        let else_branch = self.ty_db.alloc(TypedExpr::Error);
        self.ty_db.alloc(TypedExpr::Conditional {
          condition,
          then_branch,
          else_branch,
          ty: Ty::Error,
        })
      }
    }
  }

  fn join_element_types_for_iterable(ty: &Ty) -> Ty {
    match ty {
      Ty::Array(Some(elems)) => {
        if elems.is_empty() {
          return Ty::Generic;
        }
        let first = elems[0].clone();
        for e in elems.iter().skip(1) {
          if !first.type_eq(e) {
            return first.deconst();
          }
        }
        first.deconst()
      }
      _ => Ty::Generic,
    }
  }

  /// Infer `where` and `body` once with loop-shaped bindings (for diagnostics and codegen).
  #[allow(clippy::too_many_arguments)]
  fn infer_for_shaped_subexprs(
    &mut self,
    scope: &mut Scope,
    bind_name: &str,
    iterable_ty: &Ty,
    fold_params: Option<&(String, String)>,
    acc_init_typed: Option<&TypedDatabaseIdx>,
    where_clause: Option<&DatabaseIdx>,
    body: &DatabaseIdx,
    module_map: Option<&ModuleMap<'_>>,
  ) -> (Option<TypedDatabaseIdx>, TypedDatabaseIdx) {
    scope.push_frame();
    let elem_ty = Self::join_element_types_for_iterable(iterable_ty);
    scope.define(
      bind_name.to_string(),
      self.ty_db.alloc(TypedExpr::VariableRef {
        var: bind_name.to_string(),
        ty: elem_ty,
      }),
    );
    if let Some((acc_n, idx_n)) = fold_params {
      let acc_ty = acc_init_typed
        .map(|t| self.ty_db.expr(t).ty().clone())
        .unwrap_or(Ty::Generic);
      scope.define(
        acc_n.clone(),
        self.ty_db.alloc(TypedExpr::VariableRef {
          var: acc_n.clone(),
          ty: acc_ty,
        }),
      );
      scope.define(
        idx_n.clone(),
        self.ty_db.alloc(TypedExpr::VariableRef {
          var: idx_n.clone(),
          ty: Ty::Number(None),
        }),
      );
    }

    let where_shaped = where_clause.map(|w| self.infer_expr(scope, w, module_map));
    let body_shaped = self.infer_expr(scope, body, module_map);
    scope.pop_frame();
    (where_shaped, body_shaped)
  }

  #[allow(clippy::too_many_arguments)]
  fn infer_for(
    &mut self,
    scope: &mut Scope,
    binding: &Pat,
    iterable: &DatabaseIdx,
    where_clause: Option<&DatabaseIdx>,
    acc_init: Option<&DatabaseIdx>,
    fold_params: Option<&(String, String)>,
    body: &DatabaseIdx,
    for_ast: &ast::expr::ForExpression,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let span = for_ast.span();
    let bind_name = match binding {
      Pat::Ident { name } => name.clone(),
      _ => {
        self
          .diagnostics
          .push_error("type::error", "for-loop binding must be a simple name".to_string(), span);
        return self.ty_db.alloc(TypedExpr::Error);
      }
    };

    if fold_params.is_some() && acc_init.is_none() {
      self.diagnostics.push_error(
        "type::error",
        "fold parameters `|acc, i|` require an accumulator initializer `| expr |`".to_string(),
        span,
      );
      return self.ty_db.alloc(TypedExpr::Error);
    }

    let iterable_typed = self.infer_expr(scope, iterable, module_map);
    let iterable_ty = self.ty_db.expr(&iterable_typed).ty().clone();

    let acc_init_typed = acc_init.map(|i| self.infer_expr(scope, i, module_map));

    let (where_shaped, body_shaped) = self.infer_for_shaped_subexprs(
      scope,
      &bind_name,
      &iterable_ty,
      fold_params,
      acc_init_typed.as_ref(),
      where_clause,
      body,
      module_map,
    );

    let mut determinate_ty: Option<Ty> = None;

    if let TypedExpr::Array {
      vals: Some(elem_idxs),
      ty: Ty::Array(Some(elem_tys)),
      ..
    } = self.ty_db.expr(&iterable_typed).clone()
    {
      if elem_idxs.len() == elem_tys.len() {
        let mut determinate_ok = true;
        let mut running_acc = acc_init_typed;
        let mut last_body: Option<TypedDatabaseIdx> = None;

        for (k, elem_idx) in elem_idxs.iter().enumerate() {
          scope.push_frame();
          scope.define(bind_name.clone(), *elem_idx);

          if let Some((acc_n, idx_n)) = fold_params {
            let acc_val = running_acc.expect("fold requires initializer");
            scope.define(acc_n.clone(), acc_val);
            let i_lit = self.ty_db.alloc(TypedExpr::Number {
              val: Some(k as f64),
            });
            scope.define(idx_n.clone(), i_lit);
          }

          if let Some(widx) = where_clause {
            let w_typed = self.infer_expr(scope, widx, module_map);
            match self.ty_db.expr(&w_typed).ty().clone() {
              Ty::Boolean(Some(false)) => {
                scope.pop_frame();
                continue;
              }
              Ty::Boolean(Some(true)) => {}
              Ty::Boolean(None) => {
                determinate_ok = false;
                scope.pop_frame();
                break;
              }
              _ => {
                self
                  .diagnostics
                  .push_error("type::error", "where clause must be boolean".to_string(), span);
                determinate_ok = false;
                scope.pop_frame();
                break;
              }
            }
          }

          let body_typed = self.infer_expr(scope, body, module_map);
          scope.pop_frame();

          if !determinate_ok {
            break;
          }

          last_body = Some(body_typed);
          if fold_params.is_some() {
            running_acc = Some(body_typed);
          }
        }

        if determinate_ok {
          determinate_ty = Some(if fold_params.is_some() {
            running_acc
              .map(|idx| self.ty_db.expr(&idx).ty().clone())
              .or_else(|| acc_init_typed.as_ref().map(|t| self.ty_db.expr(t).ty().clone()))
              .unwrap_or(Ty::Generic)
          } else if let Some(lb) = last_body {
            self.ty_db.expr(&lb).ty().clone()
          } else {
            acc_init_typed
              .as_ref()
              .map(|t| self.ty_db.expr(t).ty().clone())
              .unwrap_or(Ty::Generic)
          });
        }
      }
    }

    let final_ty = if let Some(dt) = determinate_ty {
      dt
    } else {
      let body_ty = self.ty_db.expr(&body_shaped).ty().clone();
      match &acc_init_typed {
        Some(init_idx) => {
          let init_ty = self.ty_db.expr(init_idx).ty().clone();
          if !init_ty.type_eq(&body_ty) {
            self.diagnostics.push_error("type::error",
              format!(
                "Type mismatch in for-loop: initializer {} and body {}",
                init_ty, body_ty
              ),
              span,
            );
            Ty::Error
          } else {
            body_ty.deconst()
          }
        }
        None => body_ty.deconst(),
      }
    };

    self.ty_db.alloc(TypedExpr::For {
      binding: bind_name,
      iterable: iterable_typed,
      where_clause: where_shaped,
      acc_init: acc_init_typed,
      fold_acc: fold_params.map(|(a, _)| a.clone()),
      fold_index: fold_params.map(|(_, i)| i.clone()),
      body: body_shaped,
      ty: final_ty,
    })
  }

  fn infer_array(
    &mut self,
    scope: &mut Scope,
    vals: &[DatabaseIdx],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let (vals, tys): (Vec<TypedDatabaseIdx>, Vec<Ty>) = vals
      .iter()
      .map(|val| {
        let val = self.infer_expr(scope, val, module_map);
        let val_ty = self.ty_db.expr(&val).ty();
        (val, val_ty)
      })
      .unzip();

    let ty = Ty::Array(Some(tys));
    let expr = TypedExpr::Array {
      vals: Some(vals),
      ty,
    };
    self.ty_db.alloc(expr)
  }

  fn infer_function_def(
    &mut self,
    scope: &mut Scope,
    name: &str,
    value: &DatabaseIdx,
    pub_vis: bool,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedStmt {
    let (params, body, ast) = match self.hir_db.get_expr(value).clone() {
      Expr::Function {
        name: _,
        params,
        body,
        ast,
      } => (params, body, ast),
      _ => unreachable!("Function definition must be a function"),
    };

    let func_value =
      self.infer_function(scope, &Some(name.to_string()), &params, &body, &ast, module_map);
    scope.define(name.to_string(), func_value);

    TypedStmt::FunctionDef {
      name: name.to_string(),
      value: func_value,
      pub_vis,
    }
  }

  /// Name for diagnostics when an argument is an object (or wrapped in parens/unary); otherwise `"anonymous"`.
  fn argument_object_label(expr: &AstExpr) -> String {
    match expr {
      AstExpr::VariableRef(v) => v.name(),
      AstExpr::PathExpr(p) => {
        let s = p.segments().join("::");
        if s.is_empty() {
          "anonymous".to_string()
        } else {
          s
        }
      }
      AstExpr::ParenExpr(p) => p
        .expr()
        .as_ref()
        .map(Self::argument_object_label)
        .unwrap_or_else(|| "anonymous".to_string()),
      AstExpr::UnaryExpr(u) => u
        .expr()
        .as_ref()
        .map(Self::argument_object_label)
        .unwrap_or_else(|| "anonymous".to_string()),
      AstExpr::StructLiteral(sl) => sl
        .type_expr()
        .as_ref()
        .map(Self::argument_object_label)
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "anonymous".to_string()),
      _ => "anonymous".to_string(),
    }
  }

  fn infer_function_call(
    &mut self,
    scope: &mut Scope,
    lhs: &DatabaseIdx,
    args: &[DatabaseIdx],
    ast: &ast::expr::FunctionCall,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let lhs = self.infer_expr(scope, lhs, module_map);
    let args = args
      .iter()
      .map(|arg| self.infer_expr(scope, arg, module_map))
      .collect::<Vec<_>>();
    self.infer_function_call_impl(scope, &lhs, &args, ast, module_map)
  }

  fn infer_function_call_impl(
    &mut self,
    scope: &mut Scope,
    lhs: &TypedDatabaseIdx,
    args: &Vec<TypedDatabaseIdx>,
    ast: &ast::expr::FunctionCall,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let lhs_expr = self.ty_db.expr(lhs);

    // Need to try to travel through the callee expression to find a function definition
    // to invoke. A function definition can be returned from a variable reference, the
    // result of a function call, the end of a block, or just the function definition itself.
    match lhs_expr.clone() {
      TypedExpr::FunctionParameter { name: _, ty: _ } => *lhs,
      TypedExpr::StructConstructor { name } => {
        self.diagnostics.push_error("type::error",
          format!(
            "construct struct `{}` with a struct literal, e.g. `new {} {{ }}`",
            name, name
          ),
          ast.span(),
        );
        self.ty_db.alloc(TypedExpr::Error)
      }
      TypedExpr::VariableRef { var, ty } => {
        if scope.is_late_binding(&var) {
          return *lhs;
        }
        match scope.def(&var) {
          Some(def) if def == *lhs => {
            // Imported proxy (same idx): type the call from the variable's function type to avoid infinite recursion.
            if let Ty::Function { ret, params, .. } = ty {
              if args.len() != params.len() {
                self.diagnostics.push_error("type::error",
                  format!(
                    "function expected {} arguments, but got {}",
                    params.len(),
                    args.len()
                  ),
                  ast.span(),
                );
                return self.ty_db.alloc(TypedExpr::Error);
              }
              let ret_ty = ret
                .as_ref()
                .map(|t| (**t).clone())
                .unwrap_or(Ty::Void);
              let ret_idx = self.ty_db.alloc(TypedExpr::VariableRef {
                var: var.clone(),
                ty: ret_ty.clone(),
              });
              self.ty_db.alloc(TypedExpr::FunctionCall {
                args: args.clone(),
                def: *lhs,
                ret: ret_idx,
                ty: ret_ty,
              })
            } else {
              self.ty_db.alloc(TypedExpr::Error)
            }
          }
          Some(def) => self.infer_function_call_impl(scope, &def, args, ast, module_map),
          None => {
            self
              .diagnostics
              .push_error("type::error", format!("Undefined variable `{}`", var), ast.span());
            self.ty_db.alloc(TypedExpr::Error)
          }
        }
      }
      TypedExpr::FunctionCall {
        args: these_args,
        def,
        ret,
        ty: _,
      } => {
        if let TypedExpr::FunctionDef(func) = self.ty_db.expr(&def) {
          scope.push_frame();

          if these_args
            .iter()
            .any(|arg| self.ty_db.expr(arg).ty() == Ty::Generic)
          {
            return ret;
          }
          let mut params = FxIndexMap::default();
          for ((param, _), arg) in func.params.iter().zip(these_args.iter()) {
            params.insert(param.clone(), *arg);
          }
          scope.define_args(&params);
          let result = self.infer_function_call_impl(scope, &ret, args, ast, module_map);
          scope.pop_frame();
          result
        } else {
          unreachable!()
        }
      }
      TypedExpr::FunctionDef(FunctionDef {
        name,
        params,
        body: _,
        body_hir,
        ty: _,
        closure_scope,
      }) => {
        if args.len() != params.len() {
          self.diagnostics.push_error("type::error",
            format!(
              "function `{}` expected {} arguments, but got {}",
              name.unwrap_or_default(),
              params.len(),
              args.len()
            ),
            ast.span(),
          );
          return self.ty_db.alloc(TypedExpr::Error);
        }

        for (((_param_name, formal_param_idx), arg_idx), arg_ast) in params
          .iter()
          .zip(args.iter())
          .zip(ast.args())
        {
          let required = self.ty_db.expr(formal_param_idx).ty();
          let actual = self.ty_db.expr(arg_idx).ty();
          let object_label = Self::argument_object_label(&arg_ast);
          if let Some(msg) =
            Ty::parameter_argument_constraint_message(&actual, &required, &object_label)
          {
            self.diagnostics.push_error(
              "type::parameter_constraint",
              msg,
              arg_ast.span(),
            );
          }
        }

        let scope = &mut closure_scope.extend_frames(scope);

        let params: FxIndexMap<String, TypedDatabaseIdx> = params
          .iter()
          .zip(args.iter().cloned())
          .map(|((name, _), arg)| (name.clone(), arg))
          .collect();

        scope.push_frame();
        scope.define_args(&params);
        let body = self.infer_expr(scope, &body_hir, module_map);
        let expr = TypedExpr::FunctionCall {
          args: params.clone().values().copied().collect(),
          ty: self.ty_db.expr(&body).ty(),
          ret: body,
          def: *lhs,
        };
        scope.pop_frame();
        self.ty_db.alloc(expr)
      }
      TypedExpr::Unresolved => *lhs,
      TypedExpr::ObjectFieldAccess {
        object,
        field,
        ty: _,
      } => {
        let object_ty = self.ty_db.expr(&object).ty();
        if let Ty::Array(_) = &object_ty {
          if let Some(desc) = self.lookup_primitive_descriptor(&object_ty, field.as_str()) {
            if let Some(Ty::Function { params, ret }) = (desc.signature)(&object_ty) {
              if args.len() != params.len() {
                self.diagnostics.push_error(
                  "type::error",
                  format!(
                    "method `{}` expects {} arguments, got {}",
                    field,
                    params.len(),
                    args.len()
                  ),
                  ast.span(),
                );
                return self.ty_db.alloc(TypedExpr::Error);
              }
              for ((formal, arg_idx), arg_ast) in params.iter().zip(args.iter()).zip(ast.args()) {
                let actual = self.ty_db.expr(arg_idx).ty();
                let object_label = Self::argument_object_label(&arg_ast);
                if let Some(msg) =
                  Ty::parameter_argument_constraint_message(&actual, formal, &object_label)
                {
                  self.diagnostics.push_error(
                    "type::parameter_constraint",
                    msg,
                    arg_ast.span(),
                  );
                }
              }
              let mut ret_ty = ret.map(|b| *b).unwrap_or(Ty::Generic);
              if let Some(eval_fn) = desc.evaluate {
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.ty_db.expr(a).ty()).collect();
                if let Some(constant) = eval_fn(&object_ty, &arg_tys) {
                  ret_ty = constant;
                }
              }
              let ret_idx = self.ty_db.alloc(TypedExpr::Unresolved);
              return self.ty_db.alloc(TypedExpr::FunctionCall {
                args: args.clone(),
                def: *lhs,
                ret: ret_idx,
                ty: ret_ty,
              });
            }
          }
        }
        match self.ty_db.expr(&object) {
          TypedExpr::FunctionParameter { .. } => {
          if let Err(message) =
            self.refine_function_parameter_method_from_call(&object, field.as_str(), args, ast)
          {
            self.diagnostics.push_error("type::error", message, ast.span());
            return self.ty_db.alloc(TypedExpr::Error);
          }
          let ret = self.ty_db.alloc(TypedExpr::Unresolved);
          self.ty_db.alloc(TypedExpr::FunctionCall {
            args: args.clone(),
            def: *lhs,
            ret,
            ty: Ty::Generic,
          })
        }
        TypedExpr::VariableRef { var, ty } => match scope.def(var) {
          Some(def) => match self.ty_db.expr(&def) {
            TypedExpr::Object { fields, ty: _ } => {
              let field = *fields.get(&field).unwrap();
              self.infer_function_call_impl(scope, &field, args, ast, module_map)
            }
            TypedExpr::Unresolved => *lhs,
            TypedExpr::VariableRef { .. } => *lhs,
            TypedExpr::FunctionParameter { .. } => *lhs,
            _ => {
              self.diagnostics.push_error("type::error",
                format!("Cannot call field `{}` on non-object type `{}`", field, ty),
                ast.span(),
              );
              self.ty_db.alloc(TypedExpr::Error)
            }
          },
          None => {
            self
              .diagnostics
              .push_error("type::error", format!("Undefined variable `{}`", var), ast.span());
            self.ty_db.alloc(TypedExpr::Error)
          }
        },
        TypedExpr::Object { fields, ty: _ } => match fields.get(&field) {
          Some(field) => {
            let field = *field;
            self.infer_function_call_impl(scope, &field, args, ast, module_map)
          }
          None => {
            self.diagnostics.push_error("type::error",
              format!("Object does not have field `{}`", field),
              ast.span(),
            );
            self.ty_db.alloc(TypedExpr::Error)
          }
        },
        _ => {
          self.diagnostics.push_error("type::error",
            format!(
              "Cannot call function on non-object. {} {:#?}",
              field, object
            ),
            ast.span(),
          );
          self.ty_db.alloc(TypedExpr::Error)
        }
      }
      },
      TypedExpr::Block { stmts, ty: _ } => {
        match stmts.last().map(|s| self.ty_db.expr(s.value()).ty()) {
          Some(Ty::Function { .. }) => self.infer_function_call_impl(
            scope,
            stmts.last().unwrap().value(),
            args,
            ast,
            module_map,
          ),
          _ => {
            self.diagnostics.push_error("type::error",
              "Cannot call `block` that does not return a function".to_string(),
              ast.span(),
            );
            self.ty_db.alloc(TypedExpr::Error)
          }
        }
      }
      // An error here means the user probably typo'd
      TypedExpr::Error => {
        self.diagnostics.push_error("type::error",
          format!("Cannot call `{:?}`", self.ty_db.expr(lhs)),
          ast.span(),
        );
        self.ty_db.alloc(TypedExpr::Error)
      }
      _ => {
        self.diagnostics.push_error("type::error",
          format!("Cannot call `{:?}`", self.ty_db.expr(lhs)),
          ast.span(),
        );
        self.ty_db.alloc(TypedExpr::Error)
      }
    }
  }

  /// Refines a function-parameter object type when the callee is `param.method(...)` on that parameter.
  fn refine_function_parameter_method_from_call(
    &mut self,
    param_idx: &TypedDatabaseIdx,
    method: &str,
    args: &[TypedDatabaseIdx],
    _ast: &ast::expr::FunctionCall,
  ) -> Result<(), String> {
    let arg_tys: Vec<Ty> = args.iter().map(|a| self.ty_db.expr(a).ty()).collect();
    let call_sig = Ty::Function {
      params: arg_tys,
      ret: Some(Box::new(Ty::Generic)),
    };

    let orig = self.ty_db.expr(param_idx).ty().clone();
    let next = match orig {
      Ty::Generic => Ty::Object(Some(index_map!(method.to_string() => call_sig.clone()))),
      Ty::Object(None) => Ty::Object(Some(index_map!(method.to_string() => call_sig.clone()))),
      Ty::Object(Some(fs)) => {
        let merged = match fs.get(method) {
          None | Some(Ty::Generic) => call_sig.clone(),
          Some(Ty::Function {
            params: ep,
            ret: er,
          }) => {
            let Ty::Function {
              params: cp,
              ret: cr,
            } = &call_sig
            else {
              unreachable!()
            };
            if ep.len() != cp.len() {
              return Err(format!(
                "method `{}` was previously called with {} arguments but here with {}",
                method,
                ep.len(),
                cp.len()
              ));
            }
            Ty::Function {
              params: ep
                .iter()
                .zip(cp.iter())
                .map(|(e, c)| if e == c { e.clone() } else { Ty::Generic })
                .collect(),
              ret: match (er.as_ref(), cr.as_ref()) {
                (Some(e), Some(c)) if e == c => er.clone(),
                _ => Some(Box::new(Ty::Generic)),
              },
            }
          }
          Some(bad) => {
            return Err(format!(
              "cannot call `{}()`; parameter already has field `{}` of type `{}`",
              method, method, bad
            ));
          }
        };
        let mut fs = fs;
        fs.insert(method.to_string(), merged);
        Ty::Object(Some(fs))
      }
      other => {
        return Err(format!(
          "Cannot call `{}()` on non-object parameter type `{}`",
          method, other
        ));
      }
    };

    self.ty_db.expr_mut(param_idx).replace_ty(next);
    Ok(())
  }

  /// Infers the definition of a function as well as its parameters.
  fn infer_function(
    &mut self,
    scope: &mut Scope,
    name: &Option<String>,
    params: &[String],
    body: &DatabaseIdx,
    _ast: &ast::expr::Function,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let params: FxIndexMap<String, TypedDatabaseIdx> = params
      .iter()
      .map(|name| {
        (
          name.clone(),
          scope.def(name).unwrap_or_else(|| {
            self.ty_db.alloc(TypedExpr::FunctionParameter {
              name: name.clone(),
              ty: Ty::Generic,
            })
          }),
        )
      })
      .collect();

    if let Some(name) = name {
      scope.push_named_frame(name.to_string());
    } else {
      scope.push_frame();
    }
    scope.define_args(&params);

    let body_idx = self.infer_expr(scope, body, module_map);
    let body_ty = self.ty_db.expr(&body_idx).ty();
    let ty = Ty::Function {
      ret: Some(Box::new(body_ty)),
      params: params
        .iter()
        .map(|(_, v)| {
          let ty = self.ty_db.expr(v).ty();
          ty
        })
        .collect(),
    };
    let expr = TypedExpr::FunctionDef(FunctionDef {
      name: name.clone(),
      params,
      body: body_idx,
      body_hir: *body,
      closure_scope: scope.flatten(),
      ty,
    });

    scope.pop_frame();
    self.ty_db.alloc(expr)
  }

  fn infer_unary(
    &mut self,
    scope: &mut Scope,
    op: &hir::UnaryOp,
    expr: &DatabaseIdx,
    ast: &ast::expr::UnaryExpr,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let expr = self.infer_expr(scope, expr, module_map);
    let expr_ty = self.ty_db.expr(&expr).ty();
    let ty = match op {
      hir::UnaryOp::Neg => match expr_ty {
        Ty::Number(Some(n)) => Ty::Number(Some(-n)),
        Ty::Number(None) => Ty::Number(None),
        Ty::Void => {
          self.diagnostics.push_error(
            "type::error",
            "cannot apply unary operator `-` to void".to_string(),
            ast.span(),
          );
          Ty::Error
        }
        _ => {
          self.diagnostics.push_error("type::error",
            format!("cannot apply unary operator `-` to type `{}`", expr_ty),
            ast.span(),
          );
          Ty::Error
        }
      },
      hir::UnaryOp::Not => match expr_ty {
        Ty::Boolean(Some(b)) => Ty::Boolean(Some(!b)),
        Ty::Boolean(None) => Ty::Boolean(None),
        Ty::Number(Some(n)) => Ty::Boolean(Some(n == 0.0)),
        Ty::Number(None) => Ty::Boolean(None),
        Ty::String(Some(s)) => Ty::Boolean(Some(s.is_empty())),
        Ty::String(None) => Ty::Boolean(None),
        Ty::Array(_) => Ty::Boolean(Some(false)),
        Ty::Object(_) => Ty::Boolean(Some(false)),
        Ty::Instance(_) => Ty::Boolean(None),
        Ty::Generic => Ty::Boolean(None),
        Ty::Void => {
          self.diagnostics.push_error(
            "type::error",
            "cannot apply unary operator `!` to void".to_string(),
            ast.span(),
          );
          Ty::Error
        }
        _ => {
          self.diagnostics.push_error("type::error",
            format!("cannot apply unary operator `!` to type `{}`", expr_ty),
            ast.span(),
          );
          Ty::Error
        }
      },
    };
    self.ty_db.alloc(TypedExpr::Unary {
      op: op.into(),
      expr,
      ty,
    })
  }

  /// Infererence rules:
  /// 1. Two known value types should unify into a single known value type
  /// 2. Unresolved types paired with a concrete type should unify into an unknown value type
  /// 3. Casting favours the left hand side type
  // rustfmt makes the match block have inconsistent formatting
  #[rustfmt::skip]
  fn infer_binary(
    &mut self,
    scope: &mut Scope,
    op: &hir::BinaryOp,
    lhs_idx: &DatabaseIdx,
    rhs_idx: &DatabaseIdx,
    ast: &ast::expr::BinaryExpr,
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    let lhs_idx = self.infer_expr(scope, lhs_idx, module_map);
    let rhs_idx = self.infer_expr(scope, rhs_idx, module_map);

    let (lhs, rhs) = self.ty_db.exprs2(&lhs_idx, &rhs_idx);

    let lhs_ty = lhs.ty();
    let rhs_ty = rhs.ty();
    if matches!(lhs_ty, Ty::Void) || matches!(rhs_ty, Ty::Void) {
      self.diagnostics.push_error(
        "type::error",
        "cannot use void result (e.g. from `print(...)`) as a value".to_string(),
        ast.span(),
      );
      return self.ty_db.alloc(TypedExpr::Binary {
        op: op.into(),
        lhs: lhs_idx,
        rhs: rhs_idx,
        ty: Ty::Error,
      });
    }
    let ty = match op {
      hir::BinaryOp::Add => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs + rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),

        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::String(Some(lhs + &rhs)),
        (Ty::String(_), Ty::Generic) => Ty::String(None),
        (Ty::Generic, Ty::String(_)) => Ty::String(None),
        (Ty::String(_), Ty::String(_)) => Ty::String(None),

        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Array(Some([lhs, rhs].concat())),
        (Ty::Array(_), Ty::Array(_)) => Ty::Array(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Sub => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Mul => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Div => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
        (Ty::Number(_), Ty::Generic) => Ty::Number(None),
        (Ty::Generic, Ty::Number(_)) => Ty::Number(None),
        (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
        _ => Ty::Generic,
      },
      hir::BinaryOp::Eq => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        (Ty::Boolean(Some(lhs)), Ty::Boolean(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Array(_), Ty::Array(_)) => Ty::Boolean(None),
        (Ty::Function { ret: Some(lhs), .. }, Ty::Function { ret: Some(rhs), .. }) => Ty::Boolean(Some(lhs == rhs)),
        (Ty::Function { ret: None, .. }, Ty::Function { ret: None, .. }) => Ty::Boolean(None),
        (Ty::Instance(a), Ty::Instance(b)) => Ty::Boolean(Some(a == b)),
        (Ty::Generic, Ty::Generic) => Ty::Boolean(None),
        _ => Ty::Boolean(Some(false)),
      },
      hir::BinaryOp::Neq => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        (Ty::Boolean(Some(lhs)), Ty::Boolean(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Array(_), Ty::Array(_)) => Ty::Boolean(None),
        (Ty::Function { ret: Some(lhs), .. }, Ty::Function { ret: Some(rhs), .. }) => Ty::Boolean(Some(lhs != rhs)),
        (Ty::Function { ret: None, .. }, Ty::Function { ret: None, .. }) => Ty::Boolean(None),
        (Ty::Instance(a), Ty::Instance(b)) => Ty::Boolean(Some(a != b)),
        (Ty::Generic, Ty::Generic) => Ty::Boolean(None),
        _ => Ty::Boolean(Some(true)),
      },
      hir::BinaryOp::Lt => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs < rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs < rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Lte => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs <= rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs <= rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Gt => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs > rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs > rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
      hir::BinaryOp::Gte => match (lhs_ty, rhs_ty) {
        (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Boolean(Some(lhs >= rhs)),
        (Ty::Number(_), Ty::Number(_)) => Ty::Boolean(None),
        (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::Boolean(Some(lhs >= rhs)),
        (Ty::String(_), Ty::String(_)) => Ty::Boolean(None),
        _ => Ty::Boolean(None),
      },
    };
    self.ty_db.alloc(TypedExpr::Binary { op: op.into(), lhs: lhs_idx, rhs: rhs_idx, ty })
  }

  fn infer_block(
    &mut self,
    scope: &mut Scope,
    stmts: &[Stmt],
    module_map: Option<&ModuleMap<'_>>,
  ) -> TypedDatabaseIdx {
    scope.push_frame();
    let stmts = stmts
      .iter()
      .map(|stmt| self.infer_stmt(scope, Either::Left(stmt), module_map))
      .collect::<Vec<_>>();
    let ty = if let Some(last) = stmts.last() {
      self.ty_db.expr(last.value()).ty()
    } else {
      Ty::Generic
    };
    scope.pop_frame();
    self.ty_db.alloc(TypedExpr::Block { stmts, ty })
  }

  fn query_object_name(&self, idx: &DatabaseIdx) -> Option<&str> {
    let expr = self.hir_db.get_expr(idx);
    let name = match expr {
      Expr::VariableRef { var, .. } => var,
      Expr::Object { .. } => "<anonymous>",
      _ => return None,
    };
    Some(name)
  }
}
