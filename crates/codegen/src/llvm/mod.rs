//! Generate LLVM IR from the typed HIR.
//! Supports a subset: numbers, functions, arithmetic, conditionals, blocks, and
//! fixed-size arrays of numbers with `.length()` / `.push()` (duckstruct semantics:
//! `push` returns the new array). Values are either `f64` scalars or array blobs
//! (`pointer + length`); only scalars may appear in binops, `print`, or function
//! parameters / returns — returning or passing arrays from user functions is rejected.

use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{
  BasicMetadataValueEnum, FunctionValue, PointerValue, ValueKind,
};
use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{BinaryOp, FunctionDef, Ty, TypedExpr, TypedStmt, UnaryOp},
  TyCheck,
};

use crate::CodeGenerator;

mod array;
mod module;

use array::CompiledVal;

pub struct LlvmGenerator<'tycheck> {
  tycheck: &'tycheck TyCheck,
  /// External (stdlib) function names -> param count. These get declared, not defined.
  external_functions: HashMap<String, usize>,
}

impl<'tycheck> CodeGenerator for LlvmGenerator<'tycheck> {
  fn generate(&self) -> String {
    self.generate_llvm().unwrap_or_else(|e| panic!("LLVM codegen: {}", e))
  }
}

#[cfg(test)]
mod tests {
  use super::LlvmGenerator;

  use ast::Root;
  use hir::lower;
  use insta::assert_snapshot;
  use parser::parse;
  use tempfile::NamedTempFile;
  use tycheck::TyCheck;

  fn llvm_for(source: &str) -> String {
    let parse = parse(source);
    assert!(
      parse.errors.is_empty(),
      "parse errors: {:?}",
      parse.errors
    );
    let ast = Root::cast(parse.syntax()).expect("cast root");
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      None,
      None,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    assert!(
      !tycheck.diagnostics.has_errors(),
      "tycheck errors: {:?}",
      tycheck.diagnostics.items()
    );
    LlvmGenerator::new(&tycheck)
      .generate_llvm()
      .expect("llvm generation")
  }

  /// Make snapshots stable across small formatter/whitespace shifts.
  fn normalize_llvm_ir(ir: &str) -> String {
    let mut out = String::new();
    for line in ir.lines() {
      out.push_str(line.trim_end());
      out.push('\n');
    }
    out
  }

  #[test]
  fn llvm_ir_contains_main_and_user_function() {
    let ir = llvm_for("f forty_two() { 40 + 2 }\nforty_two()");
    assert!(ir.contains("define i32 @main()"), "ir:\n{}", ir);
    assert!(ir.contains("define double @f_forty_two()"), "ir:\n{}", ir);
  }

  #[test]
  fn llvm_ir_array_push_length_has_malloc_declaration() {
    let ir = llvm_for("f wrap(x) { let ys = [x].push(1); ys.length() }\nwrap(2)");
    assert!(ir.contains("@malloc"), "ir:\n{}", ir);
    assert!(ir.contains("define double @f_wrap"), "ir:\n{}", ir);
  }

  #[test]
  fn llvm_can_emit_object_file_smoke() {
    let parse = parse("40 + 2");
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    let ast = Root::cast(parse.syntax()).expect("cast root");
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      None,
      None,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    assert!(
      !tycheck.diagnostics.has_errors(),
      "tycheck errors: {:?}",
      tycheck.diagnostics.items()
    );
    let file = NamedTempFile::new().expect("tmp file");
    LlvmGenerator::new(&tycheck)
      .compile_to_object_file(file.path())
      .expect("emit object");
    let md = std::fs::metadata(file.path()).expect("metadata");
    assert!(md.len() > 0, "object file is empty");
  }

  #[test]
  fn llvm_ir_snapshot_constant_add() {
    let ir = llvm_for("let v = 1 + 2\nv");
    assert_snapshot!("llvm_ir_constant_add", normalize_llvm_ir(&ir));
  }

  #[test]
  fn llvm_ir_snapshot_array_push_length_path() {
    let ir = llvm_for("f wrap(x) { let ys = [x].push(1); ys.length() }\nwrap(2)");
    assert_snapshot!("llvm_ir_array_push_length_path", normalize_llvm_ir(&ir));
  }
}

impl<'tycheck> LlvmGenerator<'tycheck> {
  pub fn new(tycheck: &'tycheck TyCheck) -> Self {
    Self {
      tycheck,
      external_functions: HashMap::new(),
    }
  }

  /// Register external (e.g. stdlib) functions that should be declared in the module
  /// (name -> number of f64 params). Call sites will call these; implementations must be linked.
  pub fn with_external_functions(
    mut self,
    name_to_param_count: impl IntoIterator<Item = (String, usize)>,
  ) -> Self {
    self.external_functions = name_to_param_count.into_iter().collect();
    self
  }

  pub(crate) fn tycheck(&self) -> &'tycheck TyCheck {
    self.tycheck
  }

  pub(crate) fn external_functions(&self) -> &HashMap<String, usize> {
    &self.external_functions
  }

  pub fn generate_llvm(&self) -> Result<String, String> {
    let context = Context::create();
    let module = module::build_module(self, &context)?;
    Ok(module.print_to_string().to_string())
  }

  /// Compile the program to a native object file using LLVM's backend.
  /// Initializes the native target and writes `.o` to the given path.
  pub fn compile_to_object_file(&self, path: &Path) -> Result<(), String> {
    let context = Context::create();
    let module = module::build_module(self, &context)?;
    module::emit_object_file(&module, path)
  }

  /// Emit LLVM IR to `ir_path` and compile to native object file at `obj_path` in one pass.
  /// Builds the module once and runs LLVM's backend for the object file.
  pub fn compile_to_files(&self, ir_path: &Path, obj_path: &Path) -> Result<(), String> {
    let context = Context::create();
    let module = module::build_module(self, &context)?;
    std::fs::write(ir_path, module.print_to_string().to_string())
      .map_err(|e| format!("Failed to write IR: {}", e))?;
    module::emit_object_file(&module, obj_path)
  }

  #[allow(clippy::too_many_arguments)]
  #[allow(clippy::only_used_in_recursion)]
  pub(crate) fn compile_expr_with_locals<'ctx>(
    &self,
    expr: &TypedDatabaseIdx,
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    global_ptrs: &HashMap<String, PointerValue<'ctx>>,
    global_arrays: &HashMap<String, (PointerValue<'ctx>, usize)>,
    functions: &HashMap<String, FunctionValue<'ctx>>,
    locals: &HashMap<String, CompiledVal<'ctx>>,
    _current_fn: Option<FunctionValue<'ctx>>,
  ) -> Result<CompiledVal<'ctx>, String> {
    let expr_node = self.tycheck.ty_db.expr(expr);

    match expr_node {
      TypedExpr::Number { val: Some(v) } => Ok(CompiledVal::Float(context.f64_type().const_float(*v))),
      TypedExpr::Number { val: None } => Ok(CompiledVal::Float(context.f64_type().const_float(0.0))),
      TypedExpr::VariableRef { var, .. } => {
        if let Some(l) = locals.get(var) {
          return Ok(*l);
        }
        if let Some((storage_ptr, n)) = global_arrays.get(var) {
          let data = array::ge_ptr_to_first_double(
            builder,
            context.f64_type(),
            context.i32_type(),
            *storage_ptr,
            *n,
          )?;
          return Ok(CompiledVal::Array {
            data_ptr: data,
            len: *n,
          });
        }
        if let Some(ptr) = global_ptrs.get(var) {
          let loaded = builder
            .build_load(context.f64_type(), *ptr, var)
            .map_err(|e| e.to_string())?;
          return Ok(CompiledVal::Float(loaded.into_float_value()));
        }
        Err(format!("unknown variable: {}", var))
      }
      TypedExpr::Binary { op, lhs, rhs, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(CompiledVal::Float(context.f64_type().const_float(*v)));
          }
        }
        let lhs_val = self.compile_expr_with_locals(
          lhs,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        let rhs_val = self.compile_expr_with_locals(
          rhs,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        let lhs_f = lhs_val.into_float()?;
        let rhs_f = rhs_val.into_float()?;
        let result = match op {
          BinaryOp::Add => builder.build_float_add(lhs_f, rhs_f, "add").map_err(|e| e.to_string())?,
          BinaryOp::Sub => builder.build_float_sub(lhs_f, rhs_f, "sub").map_err(|e| e.to_string())?,
          BinaryOp::Mul => builder.build_float_mul(lhs_f, rhs_f, "mul").map_err(|e| e.to_string())?,
          BinaryOp::Div => builder.build_float_div(lhs_f, rhs_f, "div").map_err(|e| e.to_string())?,
          BinaryOp::Eq => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OEQ, lhs_f, rhs_f, "eq")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "eq_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
          BinaryOp::Neq => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::ONE, lhs_f, rhs_f, "neq")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "neq_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
          BinaryOp::Lt => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OLT, lhs_f, rhs_f, "lt")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "lt_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
          BinaryOp::Lte => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OLE, lhs_f, rhs_f, "le")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "le_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
          BinaryOp::Gt => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OGT, lhs_f, rhs_f, "gt")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "gt_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
          BinaryOp::Gte => {
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OGE, lhs_f, rhs_f, "ge")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            let zero = context.f64_type().const_float(0.0);
            builder
              .build_select(cmp, one, zero, "ge_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
        };
        Ok(CompiledVal::Float(result))
      }
      TypedExpr::Unary { op, expr, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(CompiledVal::Float(context.f64_type().const_float(*v)));
          }
        }
        let inner = self.compile_expr_with_locals(
          expr,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        let inner_f = inner.into_float()?;
        let result = match op {
          UnaryOp::Neg => builder.build_float_neg(inner_f, "neg").map_err(|e| e.to_string())?,
          UnaryOp::Not => {
            let zero = context.f64_type().const_float(0.0);
            let cmp = builder
              .build_float_compare(inkwell::FloatPredicate::OEQ, inner_f, zero, "not_cmp")
              .map_err(|e| e.to_string())?;
            let one = context.f64_type().const_float(1.0);
            builder
              .build_select(cmp, one, zero, "not_sel")
              .map_err(|e| e.to_string())?
              .into_float_value()
          }
        };
        Ok(CompiledVal::Float(result))
      }
      TypedExpr::Block { stmts, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(CompiledVal::Float(context.f64_type().const_float(*v)));
          }
        }
        let mut last: Option<CompiledVal<'ctx>> = None;
        for stmt in stmts {
          let val = match stmt {
            TypedStmt::Expr(e) => Some(
              self.compile_expr_with_locals(
                e,
                context,
                module,
                builder,
                global_ptrs,
                global_arrays,
                functions,
                locals,
                _current_fn,
              )?,
            ),
            _ => None,
          };
          if let Some(v) = val {
            last = Some(v);
          }
        }
        last.ok_or_else(|| "empty block".to_string())
      }
      TypedExpr::Conditional {
        condition,
        then_branch,
        else_branch,
        ty,
      } => {
        if matches!(ty, Ty::Array(_)) {
          return Err(
            "LLVM backend: conditional expressions cannot yield arrays".to_string(),
          );
        }
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(CompiledVal::Float(context.f64_type().const_float(*v)));
          }
        }
        let cond_val = self.compile_expr_with_locals(
          condition,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        let cond_f = cond_val.into_float()?;
        let zero = context.f64_type().const_float(0.0);
        let cond_bool = builder
          .build_float_compare(inkwell::FloatPredicate::ONE, cond_f, zero, "cond")
          .map_err(|e| e.to_string())?;

        let then_val = self.compile_expr_with_locals(
          then_branch,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        let else_val = self.compile_expr_with_locals(
          else_branch,
          context,
          module,
          builder,
          global_ptrs,
          global_arrays,
          functions,
          locals,
          _current_fn,
        )?;
        match (then_val, else_val) {
          (CompiledVal::Float(then_f), CompiledVal::Float(else_f)) => {
            let result = builder
              .build_select(cond_bool, then_f, else_f, "select")
              .map_err(|e| e.to_string())?;
            Ok(CompiledVal::Float(result.into_float_value()))
          }
          _ => Err(
            "LLVM backend: conditional branches must both produce numbers".to_string(),
          ),
        }
      }
      TypedExpr::FunctionCall { def, args, ty, .. } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(CompiledVal::Float(context.f64_type().const_float(*v)));
          }
        }
        if let Ty::Array(Some(elems)) = ty {
          if elems
            .iter()
            .all(|e| matches!(e, Ty::Number(Some(_))))
          {
            let vals: Vec<f64> = elems
              .iter()
              .map(|e| {
                if let Ty::Number(Some(n)) = e {
                  *n
                } else {
                  0.0
                }
              })
              .collect();
            return array::build_stack_array_from_f64s(context, builder, &vals);
          }
        }
        if let TypedExpr::ObjectFieldAccess { object, field, .. } = self.tycheck.ty_db.expr(def) {
          if field == "length" && args.is_empty() {
            let arr = self.compile_expr_with_locals(
              object,
              context,
              module,
              builder,
              global_ptrs,
              global_arrays,
              functions,
              locals,
              _current_fn,
            )?;
            return match arr {
              CompiledVal::Array { len, .. } => Ok(CompiledVal::Float(
                context.f64_type().const_float(len as f64),
              )),
              _ => Err("LLVM backend: `.length` requires an array receiver".to_string()),
            };
          }
          if field == "push" && args.len() == 1 {
            let arr = self.compile_expr_with_locals(
              object,
              context,
              module,
              builder,
              global_ptrs,
              global_arrays,
              functions,
              locals,
              _current_fn,
            )?;
            let arg = self.compile_expr_with_locals(
              &args[0],
              context,
              module,
              builder,
              global_ptrs,
              global_arrays,
              functions,
              locals,
              _current_fn,
            )?
            .into_float()?;
            return match arr {
              CompiledVal::Array { data_ptr, len } => {
                array::llvm_array_append(context, module, builder, data_ptr, len, arg)
              }
              _ => Err("LLVM backend: `.push` requires an array receiver".to_string()),
            };
          }
        }
        let callee = match self.tycheck.ty_db.expr(def) {
          TypedExpr::FunctionDef(FunctionDef { name, .. }) => {
            let n = name.as_deref().unwrap_or("");
            functions.get(n).copied().ok_or_else(|| format!("unknown function: {}", n))?
          }
          TypedExpr::VariableRef { var, .. } => functions
            .get(var.as_str())
            .copied()
            .ok_or_else(|| format!("unknown function: {}", var))?,
          _ => return Err("call target must be a function".to_string()),
        };
        let mut arg_vals = Vec::new();
        for a in args {
          let v = self.compile_expr_with_locals(
            a,
            context,
            module,
            builder,
            global_ptrs,
            global_arrays,
            functions,
            locals,
            _current_fn,
          )?;
          arg_vals.push(v.into_float()?);
        }
        let args_meta: Vec<BasicMetadataValueEnum> = arg_vals
          .iter()
          .map(|&f| BasicMetadataValueEnum::FloatValue(f))
          .collect();
        let call_site = builder
          .build_call(callee, &args_meta, "call")
          .map_err(|e| e.to_string())?;
        let result = match call_site.try_as_basic_value() {
          ValueKind::Basic(bv) => bv,
          _ => return Err("call must return a value".to_string()),
        };
        Ok(CompiledVal::Float(result.into_float_value()))
      }
      TypedExpr::StructConstructor { .. } | TypedExpr::StructInstance { .. } => {
        Err("LLVM backend: struct types not yet supported".to_string())
      }
      TypedExpr::FunctionDef(_) => Err("nested function definitions not supported in LLVM backend".to_string()),
      TypedExpr::FunctionParameter { .. } => Err("parameter should be in locals".to_string()),
      TypedExpr::String { .. } => Err("LLVM backend: strings not yet supported".to_string()),
      TypedExpr::Boolean { .. } => Err("LLVM backend: booleans not yet supported".to_string()),
      TypedExpr::Array { vals: Some(idxs), .. } => {
        let mut elems = Vec::with_capacity(idxs.len());
        for idx in idxs {
          let v = self.compile_expr_with_locals(
            idx,
            context,
            module,
            builder,
            global_ptrs,
            global_arrays,
            functions,
            locals,
            _current_fn,
          )?;
          elems.push(v.into_float()?);
        }
        array::build_stack_array_from_float_vals(context, builder, &elems)
      }
      TypedExpr::Array { vals: None, .. } => {
        Err("LLVM backend: incomplete array literal".to_string())
      }
      TypedExpr::Object { .. } => Err("LLVM backend: objects not yet supported".to_string()),
      TypedExpr::ObjectFieldAccess { .. } => Err("LLVM backend: object field access not yet supported".to_string()),
      TypedExpr::DynamicMethodCall { .. } => {
        Err("LLVM backend: dynamic method dispatch not yet supported".to_string())
      }
      TypedExpr::For { .. } => Err("LLVM backend: for loops not yet supported".to_string()),
      TypedExpr::Unresolved => Err("LLVM backend: unresolved expression".to_string()),
      TypedExpr::Error => Err("LLVM backend: typed expression error".to_string()),
    }
  }
}
