//! Type inference for the Duckstruct language. It does more than just inference.
//! It acts as a partial evaluator for code during type checking.
//! It also has the ability to refine parameter types based on the context of the function call.

mod access;
mod control;
mod functions;
mod ops;
mod primitives;
mod traits;

use std::collections::{HashMap, HashSet};
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
use syntax::{TextRange, TextSize};

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
  trait_requirements: FxIndexMap<String, FxIndexMap<String, usize>>,
  impl_registry: HashSet<(String, String)>,
  trait_impl_methods: HashMap<(String, String, String), TypedDatabaseIdx>,
  struct_names: HashSet<String>,
}

impl TyCheck {
  pub fn new(hir_db: hir::Database) -> Self {
    let struct_names = hir_db
      .defs_iter()
      .into_iter()
      .filter_map(|(_, stmt)| match stmt {
        Stmt::StructDef { name, .. } => Some(name.clone()),
        _ => None,
      })
      .collect();
    Self {
      hir_db: Rc::new(hir_db),
      ty_db: TypedDatabase::default(),
      diagnostics: Diagnostics::default(),
      expr_ty_at_use_site: FxIndexMap::default(),
      primitive_methods: None,
      trait_requirements: FxIndexMap::default(),
      impl_registry: HashSet::default(),
      trait_impl_methods: HashMap::default(),
      struct_names,
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
                | TypedStmt::StructDef { pub_vis, .. }
                | TypedStmt::TraitDef { pub_vis, .. } => {
                  *pub_vis
                }
                _ => false,
              };
              if is_pub {
                // Import trait requirements into the current module scope so impl validation
                // can resolve traits only when they are explicitly brought into scope.
                if matches!(typed_stmt, TypedStmt::TraitDef { .. }) {
                  if let Some(reqs) = dep.trait_requirements.get(item_name) {
                    let bind_name = alias.as_deref().unwrap_or(item_name);
                    self
                      .trait_requirements
                      .insert(bind_name.to_string(), reqs.clone());
                  }
                }
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
        Stmt::TraitDef {
          name,
          methods,
          pub_vis,
        } => {
          let requirements = methods
            .iter()
            .map(|method| (method.name.clone(), method.params.len()))
            .collect();
          self.trait_requirements.insert(name.clone(), requirements);
          let value = self.ty_db.alloc(TypedExpr::Unresolved);
          TypedStmt::TraitDef {
            name: name.clone(),
            value,
            pub_vis: *pub_vis,
          }
        }
        Stmt::ImplDef {
          trait_name,
          for_type,
          methods,
        } => {
          self.validate_trait_impl(scope, trait_name, for_type, methods, module_map);
          let value = self.ty_db.alloc(TypedExpr::Unresolved);
          TypedStmt::ImplDef {
            trait_name: trait_name.clone(),
            for_type: for_type.clone(),
            value,
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

  /// Callee text for user-facing call errors, e.g. `foo` or `a::b`.
  fn function_call_callee_display_name(ast: &ast::expr::FunctionCall) -> Option<String> {
    let f = ast.func()?;
    match f {
      AstExpr::VariableRef(v) => Some(v.name()),
      AstExpr::PathExpr(p) => {
        let segs = p.segments();
        if segs.is_empty() {
          None
        } else {
          Some(segs.join("::"))
        }
      }
      _ => None,
    }
  }

  /// Last path segment, for matching a `pub f name` export in a dependency.
  fn basename_for_module_lookup(callee_display: &str) -> &str {
    callee_display
      .rsplit("::")
      .next()
      .unwrap_or(callee_display)
  }

  /// If a dependency module exports a public function with this basename, return `mod::name`.
  fn suggest_module_path_for_function(
    module_map: Option<&ModuleMap<'_>>,
    function_basename: &str,
  ) -> Option<String> {
    let map = module_map?;
    let mut keys: Vec<String> = map.keys().cloned().collect();
    keys.sort();
    for mod_name in keys {
      let dep = map.get(&mod_name)?;
      if matches!(
        dep.ty_db.definition(function_basename),
        Some(TypedStmt::FunctionDef { pub_vis: true, .. })
      ) {
        return Some(format!("{mod_name}::{function_basename}"));
      }
    }
    None
  }

  fn message_cannot_call_undefined(
    module_map: Option<&ModuleMap<'_>>,
    callee_display: &str,
  ) -> String {
    let base = Self::basename_for_module_lookup(callee_display);
    if let Some(suggested) = Self::suggest_module_path_for_function(module_map, base) {
      format!("Cannot call `{callee_display}`; Did you mean `{suggested}`?")
    } else {
      format!("Cannot call `{callee_display}`; it doesn't exist.")
    }
  }

  fn message_cannot_call_non_function(callee_display: &str) -> String {
    format!("Cannot call `{callee_display}`; it is not a function.")
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
        // Resolution precedence (current phase):
        // 1) direct object / primitive methods
        // 2) trait methods
        // Future: typed parameter trait bounds should be able to override this precedence.
        if !matches!(object_ty, Ty::Object(_)) {
          if let Some((impl_fn, include_receiver)) =
            self.lookup_static_trait_method(&object_ty, field.as_str(), args.len())
          {
            let mut call_args = Vec::with_capacity(args.len() + usize::from(include_receiver));
            if include_receiver {
              call_args.push(object);
            }
            call_args.extend(args.iter().copied());
            return self.infer_function_call_impl(scope, &impl_fn, &call_args, ast, module_map);
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
          self.make_dynamic_method_call(object, &field, args)
        }
        TypedExpr::VariableRef { var, ty } => match scope.def(var) {
          Some(def) => match self.ty_db.expr(&def) {
            TypedExpr::Object { fields, ty: _ } => {
              let field = *fields.get(&field).unwrap();
              self.infer_function_call_impl(scope, &field, args, ast, module_map)
            }
            TypedExpr::Unresolved => self.make_dynamic_method_call(object, &field, args),
            TypedExpr::VariableRef { .. } => self.make_dynamic_method_call(object, &field, args),
            TypedExpr::FunctionParameter { .. } => {
              self.make_dynamic_method_call(object, &field, args)
            }
            _ => {
              self.diagnostics.push_error("type::error",
                format!("Cannot call field `{}` on non-object type `{}`", field, ty),
                ast.span(),
              );
              self.ty_db.alloc(TypedExpr::Error)
            }
          },
          None => {
            self.make_dynamic_method_call(object, &field, args)
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
        _ => self.make_dynamic_method_call(object, &field, args),
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
      // Callee inferred to Error (e.g. undefined identifier): message names the callee from AST.
      TypedExpr::Error => {
        let msg = match Self::function_call_callee_display_name(ast) {
          Some(name) => Self::message_cannot_call_undefined(module_map, &name),
          None => "Cannot call this expression; it doesn't exist.".to_string(),
        };
        self.diagnostics.push_error("type::error", msg, ast.span());
        self.ty_db.alloc(TypedExpr::Error)
      }
      _ => {
        let msg = match Self::function_call_callee_display_name(ast) {
          Some(name) => Self::message_cannot_call_non_function(&name),
          None => "Cannot call this expression; it is not a function.".to_string(),
        };
        self.diagnostics.push_error("type::error", msg, ast.span());
        self.ty_db.alloc(TypedExpr::Error)
      }
    }
  }

  
}
