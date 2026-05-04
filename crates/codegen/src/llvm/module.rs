//! LLVM `Module` construction: globals, externals, `main`, and object-file emission.

use std::collections::HashMap;
use std::path::Path;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::values::{FunctionValue, PointerValue};
use tycheck::{
  typed_db::TypedDatabaseIdx,
  typed_hir::{FunctionDef, Ty, TypedExpr, TypedStmt},
  TyCheck,
};

use super::array::CompiledVal;
use super::LlvmGenerator;

pub(crate) fn emit_object_file(module: &Module, path: &Path) -> Result<(), String> {
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
pub(crate) fn build_module<'tycheck, 'ctx>(
  generator: &LlvmGenerator<'tycheck>,
  context: &'ctx Context,
) -> Result<Module<'ctx>, String> {
  let tycheck: &TyCheck = generator.tycheck();
  let external_functions = generator.external_functions();

  let module = context.create_module("main");
  let builder = context.create_builder();

  let f64_type = context.f64_type();
  let i32_type = context.i32_type();
  let i64_type = context.i64_type();
  // malloc — used for `.push` results (see `array::llvm_array_append`).
  let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::default());
  let malloc_fn_ty = i8_ptr_ty.fn_type(&[i64_type.into()], false);
  let malloc_fn = module.add_function("malloc", malloc_fn_ty, None);
  malloc_fn.set_linkage(inkwell::module::Linkage::External);

  let mut global_ptrs: HashMap<String, PointerValue> = HashMap::new();
  let mut global_arrays: HashMap<String, (PointerValue, usize)> = HashMap::new();
  let mut functions: HashMap<String, FunctionValue> = HashMap::new();
  let mut main_inits: Vec<(String, TypedDatabaseIdx)> = Vec::new();
  let mut main_expr: Option<TypedDatabaseIdx> = None;

  for (name, param_count) in external_functions.iter() {
    let param_tys: Vec<_> = (0..*param_count).map(|_| f64_type.into()).collect();
    let fn_type = f64_type.fn_type(param_tys.as_slice(), false);
    let fn_name = format!("f_{}", name.replace("-", "_"));
    let decl = module.add_function(&fn_name, fn_type, None);
    if name != "print" {
      decl.set_linkage(inkwell::module::Linkage::External);
    }
    functions.insert(name.clone(), decl);
  }

  if external_functions.get("print") == Some(&1) {
    let print_fn = functions.get("print").copied().expect("print in external_functions");
    crate::stdlib::add_print_implementation(
      context,
      &module,
      &builder,
      print_fn,
      f64_type,
    )?;
  }

  for (name, stmt) in tycheck.ty_db.defs_iter() {
    match stmt {
      TypedStmt::VariableDef { value, .. } => {
        if external_functions.contains_key(name) {
          continue;
        }
        let global_name = format!("g_{}", name.replace("-", "_"));
        let expr_node = tycheck.ty_db.expr(value);
        if let Some(elem_vals) = super::array::const_f64_array_contents(tycheck, expr_node) {
          let n = elem_vals.len();
          let arr_ty = f64_type.array_type(n as u32);
          let global = module.add_global(arr_ty, None, &global_name);
          let init_elems: Vec<_> = elem_vals
            .iter()
            .map(|x| f64_type.const_float(*x))
            .collect();
          global.set_initializer(&f64_type.const_array(&init_elems));
          global_arrays.insert(name.clone(), (global.as_pointer_value(), n));
          continue;
        }
        let (init_const, needs_runtime_init) = match expr_node {
          TypedExpr::Number { val: Some(v) } => (context.f64_type().const_float(*v), false),
          _ => (context.f64_type().const_float(0.0), true),
        };
        let global = module.add_global(f64_type, None, &global_name);
        global.set_initializer(&init_const);
        global_ptrs.insert(name.clone(), global.as_pointer_value());
        if needs_runtime_init {
          main_inits.push((name.clone(), *value));
        }
      }
      TypedStmt::StructDef { .. } => {}
      TypedStmt::TraitDef { .. } => {}
      TypedStmt::ImplDef { .. } => {}
      TypedStmt::FunctionDef { value, .. } => {
        let expr = tycheck.ty_db.expr(value);
        if let TypedExpr::FunctionDef(FunctionDef {
          ty: fn_ty,
          params,
          body,
          ..
        }) = expr
        {
          if let Ty::Function {
            ret: Some(ret_ty),
            ..
          } = fn_ty
          {
            if matches!(ret_ty.as_ref(), Ty::Array(_)) {
              return Err(
                "LLVM backend: functions cannot return arrays (only numbers)".to_string(),
              );
            }
          }
          let param_tys: Vec<_> = (0..params.len()).map(|_| f64_type.into()).collect();
          let fn_type = f64_type.fn_type(param_tys.as_slice(), false);
          let fn_name = format!("f_{}", name.replace("-", "_"));
          let func = module.add_function(&fn_name, fn_type, None);
          let entry = context.append_basic_block(func, "entry");
          builder.position_at_end(entry);

          let mut local_map: HashMap<String, CompiledVal> = HashMap::new();
          for (i, (param_name, param_db)) in params.iter().enumerate() {
            let param_ty = tycheck.ty_db.expr(param_db).ty();
            if matches!(param_ty, Ty::Array(_)) {
              return Err(format!(
                "LLVM backend: unsupported array parameter `{}`",
                param_name
              ));
            }
            let param = func.get_nth_param(i as u32).unwrap();
            let param_val = param.into_float_value();
            local_map.insert(param_name.clone(), CompiledVal::Float(param_val));
          }

          let ret_val = generator.compile_expr_with_locals(
            body,
            context,
            &module,
            &builder,
            &global_ptrs,
            &global_arrays,
            &functions,
            &local_map,
            Some(func),
          )?;
          match ret_val {
            CompiledVal::Float(fv) => {
              builder.build_return(Some(&fv)).map_err(|e| e.to_string())?;
            }
            CompiledVal::Array { .. } => {
              return Err("LLVM backend: function body produced an array value".to_string());
            }
          }
          functions.insert(name.clone(), func);
        }
      }
      TypedStmt::Expr(expr) => {
        main_expr = Some(*expr);
      }
    }
  }

  let main_type = i32_type.fn_type(&[], false);
  let main_fn = module.add_function("main", main_type, None);
  let entry = context.append_basic_block(main_fn, "entry");
  builder.position_at_end(entry);
  let empty_locals: HashMap<String, CompiledVal> = HashMap::new();
  for (name, value_idx) in main_inits {
    let ptr = global_ptrs
      .get(&name)
      .copied()
      .ok_or_else(|| format!("global {} not found", name))?;
    let val = generator.compile_expr_with_locals(
      &value_idx,
      context,
      &module,
      &builder,
      &global_ptrs,
      &global_arrays,
      &functions,
      &empty_locals,
      None,
    )?;
    let f = val.into_float()?;
    builder.build_store(ptr, f).map_err(|e| e.to_string())?;
  }
  if let Some(expr_idx) = main_expr {
    let _ = generator.compile_expr_with_locals(
      &expr_idx,
      context,
      &module,
      &builder,
      &global_ptrs,
      &global_arrays,
      &functions,
      &empty_locals,
      None,
    )?;
  }
  builder
    .build_return(Some(&i32_type.const_int(0, false)))
    .map_err(|e| e.to_string())?;

  Ok(module)
}
