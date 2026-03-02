//! Generate LLVM IR from the typed HIR.
//! Supports a subset: numbers, functions, arithmetic, conditionals, blocks.
//! Objects, arrays, strings, and for-loops are not yet supported.

use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target};
use inkwell::OptimizationLevel;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind};
use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{BinaryOp, FunctionDef, Ty, TypedExpr, TypedStmt, UnaryOp},
  TyCheck,
};

use crate::CodeGenerator;

pub struct LlvmGenerator<'tycheck> {
  tycheck: &'tycheck TyCheck,
}

impl<'tycheck> CodeGenerator for LlvmGenerator<'tycheck> {
  fn generate(&self) -> String {
    self.generate_llvm().unwrap_or_else(|e| panic!("LLVM codegen: {}", e))
  }
}

impl<'tycheck> LlvmGenerator<'tycheck> {
  pub fn new(tycheck: &'tycheck TyCheck) -> Self {
    Self { tycheck }
  }

  pub fn generate_llvm(&self) -> Result<String, String> {
    let context = Context::create();
    let module = self.build_module(&context)?;
    Ok(module.print_to_string().to_string())
  }

  /// Compile the program to a native object file using LLVM's backend.
  /// Initializes the native target and writes `.o` to the given path.
  pub fn compile_to_object_file(&self, path: &Path) -> Result<(), String> {
    let context = Context::create();
    let module = self.build_module(&context)?;
    Self::emit_object_file(&module, path)
  }

  /// Emit LLVM IR to `ir_path` and compile to native object file at `obj_path` in one pass.
  /// Builds the module once and runs LLVM's backend for the object file.
  pub fn compile_to_files(&self, ir_path: &Path, obj_path: &Path) -> Result<(), String> {
    let context = Context::create();
    let module = self.build_module(&context)?;
    std::fs::write(ir_path, module.print_to_string().to_string())
      .map_err(|e| format!("Failed to write IR: {}", e))?;
    Self::emit_object_file(&module, obj_path)
  }

  fn emit_object_file(module: &Module, path: &Path) -> Result<(), String> {
    Target::initialize_native(&InitializationConfig::default())
      .map_err(|e| format!("Failed to initialize native target: {}", e))?;

    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
      .map_err(|e| format!("Failed to get target for triple: {}", e))?;

    let cpu = inkwell::targets::TargetMachine::get_host_cpu_name();
    let cpu_str = cpu.to_str().map_err(|e| format!("Invalid host CPU name: {:?}", e))?;
    let features = inkwell::targets::TargetMachine::get_host_cpu_features();
    let features_str = features
      .to_str()
      .map_err(|e| format!("Invalid host CPU features: {:?}", e))?;

    let target_machine = target
      .create_target_machine(
        &triple,
        cpu_str,
        features_str,
        OptimizationLevel::Default,
        RelocMode::Default,
        CodeModel::Default,
      )
      .ok_or_else(|| "Failed to create target machine".to_string())?;

    target_machine
      .write_to_file(module, FileType::Object, path)
      .map_err(|e| format!("Failed to write object file: {}", e))?;
    Ok(())
  }

  /// Build the LLVM module (IR) from the typed HIR. Used by both IR emission and object compilation.
  fn build_module<'ctx>(&self, context: &'ctx Context) -> Result<Module<'ctx>, String> {
    let module = context.create_module("main");
    let builder = context.create_builder();

    let f64_type = context.f64_type();

    // Global variable pointers (for loading on reference). Only constant initializers supported.
    let mut global_ptrs: HashMap<String, PointerValue> = HashMap::new();
    let mut functions: HashMap<String, FunctionValue> = HashMap::new();

    for (name, stmt) in self.tycheck.ty_db.defs_iter() {
      match stmt {
        TypedStmt::VariableDef { value, .. } => {
          let init_const = match self.tycheck.ty_db.expr(value) {
            TypedExpr::Number { val: Some(v) } => context.f64_type().const_float(*v),
            _ => context.f64_type().const_float(0.0),
          };
          let global_name = format!("g_{}", name.replace("-", "_"));
          let global = module.add_global(f64_type, None, &global_name);
          global.set_initializer(&init_const);
          global_ptrs.insert(name.clone(), global.as_pointer_value());
        }
        TypedStmt::FunctionDef { value, .. } => {
          let expr = self.tycheck.ty_db.expr(value);
          if let TypedExpr::FunctionDef(FunctionDef {
            name: _,
            params,
            body,
            ..
          }) = expr
          {
            let param_tys: Vec<_> = (0..params.len()).map(|_| f64_type.into()).collect();
            let fn_type = f64_type.fn_type(param_tys.as_slice(), false);
            let fn_name = format!("f_{}", name.replace("-", "_"));
            let func = module.add_function(&fn_name, fn_type, None);
            let entry = context.append_basic_block(func, "entry");
            builder.position_at_end(entry);

            let mut local_map = HashMap::new();
            for (i, (param_name, _)) in params.iter().enumerate() {
              let param = func.get_nth_param(i as u32).unwrap();
              let param_val = param.into_float_value();
              local_map.insert(param_name.clone(), BasicValueEnum::FloatValue(param_val));
            }

            let ret_val = self.compile_expr_with_locals(
              body,
              context,
              &module,
              &builder,
              &global_ptrs,
              &functions,
              &local_map,
              Some(func),
            )?;
            if let BasicValueEnum::FloatValue(fv) = ret_val {
              builder.build_return(Some(&fv)).map_err(|e| e.to_string())?;
            } else {
              return Err("function body must return f64".to_string());
            }
            functions.insert(name.clone(), func);
          }
        }
        TypedStmt::Expr(_expr) => {
          // Top-level expression: skip for LLVM (no main entry point yet)
        }
      }
    }

    // Add C main() so the linker can produce an executable when linking.
    let i32_type = context.i32_type();
    let main_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_type, None);
    let entry = context.append_basic_block(main_fn, "entry");
    builder.position_at_end(entry);
    builder
      .build_return(Some(&i32_type.const_int(0, false)))
      .map_err(|e| e.to_string())?;

    Ok(module)
  }

  fn compile_expr_with_locals<'ctx>(
    &self,
    expr: &TypedDatabaseIdx,
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    global_ptrs: &HashMap<String, PointerValue<'ctx>>,
    functions: &HashMap<String, FunctionValue<'ctx>>,
    locals: &HashMap<String, BasicValueEnum<'ctx>>,
    _current_fn: Option<FunctionValue<'ctx>>,
  ) -> Result<BasicValueEnum<'ctx>, String> {
    let expr_node = self.tycheck.ty_db.expr(expr);

    match expr_node {
      TypedExpr::Number { val: Some(v) } => {
        Ok(context.f64_type().const_float(*v).into())
      }
      TypedExpr::Number { val: None } => {
        Ok(context.f64_type().const_float(0.0).into())
      }
      TypedExpr::VariableRef { var, .. } => {
        if let Some(l) = locals.get(var) {
          return Ok(*l);
        }
        if let Some(ptr) = global_ptrs.get(var) {
          let loaded = builder
            .build_load(context.f64_type(), *ptr, var)
            .map_err(|e| e.to_string())?;
          return Ok(loaded.into());
        }
        Err(format!("unknown variable: {}", var))
      }
      TypedExpr::Binary { op, lhs, rhs, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(context.f64_type().const_float(*v).into());
          }
        }
        let lhs_val = self.compile_expr_with_locals(
          lhs,
          context,
          module,
          builder,
          global_ptrs,
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
          functions,
          locals,
          _current_fn,
        )?;
        let lhs_f = lhs_val.into_float_value();
        let rhs_f = rhs_val.into_float_value();
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
        Ok(result.into())
      }
      TypedExpr::Unary { op, expr, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(context.f64_type().const_float(*v).into());
          }
        }
        let inner = self.compile_expr_with_locals(
          expr,
          context,
          module,
          builder,
          global_ptrs,
          functions,
          locals,
          _current_fn,
        )?;
        let inner_f = inner.into_float_value();
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
        Ok(result.into())
      }
      TypedExpr::Block { stmts, ty } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(context.f64_type().const_float(*v).into());
          }
        }
        let mut last: Option<BasicValueEnum> = None;
        for stmt in stmts {
          let val = match stmt {
            TypedStmt::Expr(e) => Some(
              self.compile_expr_with_locals(
                e,
                context,
                module,
                builder,
                global_ptrs,
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
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(context.f64_type().const_float(*v).into());
          }
        }
        let cond_val = self.compile_expr_with_locals(
          condition,
          context,
          module,
          builder,
          global_ptrs,
          functions,
          locals,
          _current_fn,
        )?;
        let cond_f = cond_val.into_float_value();
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
          functions,
          locals,
          _current_fn,
        )?;
        let then_f = then_val.into_float_value();
        let else_f = else_val.into_float_value();
        let result = builder
          .build_select(cond_bool, then_f, else_f, "select")
          .map_err(|e| e.to_string())?;
        Ok(result.into())
      }
      TypedExpr::FunctionCall { def, args, ty, .. } => {
        if ty.has_value() {
          if let Ty::Number(Some(v)) = ty {
            return Ok(context.f64_type().const_float(*v).into());
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
            functions,
            locals,
            _current_fn,
          )?;
          arg_vals.push(v.into_float_value());
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
        Ok(result)
      }
      TypedExpr::FunctionDef(_) => Err("nested function definitions not supported in LLVM backend".to_string()),
      TypedExpr::FunctionParameter { .. } => Err("parameter should be in locals".to_string()),
      TypedExpr::String { .. } => Err("LLVM backend: strings not yet supported".to_string()),
      TypedExpr::Boolean { .. } => Err("LLVM backend: booleans not yet supported".to_string()),
      TypedExpr::Array { .. } => Err("LLVM backend: arrays not yet supported".to_string()),
      TypedExpr::Object { .. } => Err("LLVM backend: objects not yet supported".to_string()),
      TypedExpr::ObjectFieldAccess { .. } => Err("LLVM backend: object field access not yet supported".to_string()),
      TypedExpr::For { .. } => Err("LLVM backend: for loops not yet supported".to_string()),
      TypedExpr::Unresolved => Err("LLVM backend: unresolved expression".to_string()),
      TypedExpr::Error => Err("LLVM backend: typed expression error".to_string()),
    }
  }
}
