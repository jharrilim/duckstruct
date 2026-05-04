//! Array / list lowering for the LLVM backend: stack literals, `malloc` + copy for `.push`.

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::AddressSpace;
use inkwell::values::{
  BasicMetadataValueEnum, FloatValue, PointerValue, ValueKind,
};
use tycheck::typed_hir::{Ty, TypedExpr};
use tycheck::TyCheck;

/// LLVM value: scalar `f64` or a contiguous `f64` array (`data_ptr` → `len` elements).
#[derive(Debug, Copy, Clone)]
pub(crate) enum CompiledVal<'ctx> {
  Float(FloatValue<'ctx>),
  Array {
    data_ptr: PointerValue<'ctx>,
    len: usize,
  },
}

impl<'ctx> CompiledVal<'ctx> {
  pub(crate) fn into_float(self) -> Result<FloatValue<'ctx>, String> {
    match self {
      CompiledVal::Float(f) => Ok(f),
      CompiledVal::Array { .. } => Err(
        "LLVM backend: this position expects a number, but an array value was produced"
          .to_string(),
      ),
    }
  }
}

/// If `expr` is a constant array of literal numbers, or a constant-folded call like
/// `[1,2].push(3)`, returns the element values for a global array initializer.
pub(crate) fn const_f64_array_contents(
  tycheck: &TyCheck,
  expr: &TypedExpr,
) -> Option<Vec<f64>> {
  match expr {
    TypedExpr::Array { vals: Some(idxs), .. } => {
      let mut v = Vec::new();
      for i in idxs {
        match tycheck.ty_db.expr(i) {
          TypedExpr::Number { val: Some(n) } => v.push(*n),
          _ => return None,
        }
      }
      Some(v)
    }
    TypedExpr::FunctionCall { ty, .. } => {
      if let Ty::Array(Some(elems)) = ty {
        let mut v = Vec::new();
        for e in elems {
          if let Ty::Number(Some(n)) = e {
            v.push(*n);
          } else {
            return None;
          }
        }
        Some(v)
      } else {
        None
      }
    }
    _ => None,
  }
}

pub(crate) fn ge_ptr_to_first_double<'ctx>(
  builder: &Builder<'ctx>,
  f64_t: inkwell::types::FloatType<'ctx>,
  i32_t: inkwell::types::IntType<'ctx>,
  array_storage_ptr: PointerValue<'ctx>,
  n: usize,
) -> Result<PointerValue<'ctx>, String> {
  let arr_ty = f64_t.array_type(n as u32);
  let zero = i32_t.const_int(0, false);
  unsafe { builder.build_gep(arr_ty, array_storage_ptr, &[zero, zero], "arr_data") }
    .map_err(|e| e.to_string())
}

pub(crate) fn build_stack_array_from_float_vals<'ctx>(
  context: &'ctx Context,
  builder: &Builder<'ctx>,
  elems: &[FloatValue<'ctx>],
) -> Result<CompiledVal<'ctx>, String> {
  let n = elems.len();
  let f64_t = context.f64_type();
  let i32_t = context.i32_type();
  if n == 0 {
    return Err("LLVM backend: empty array literals are not supported".to_string());
  }
  let arr_ty = f64_t.array_type(n as u32);
  let arr_storage = builder
    .build_alloca(arr_ty, "lit_arr")
    .map_err(|e| e.to_string())?;
  for (i, fv) in elems.iter().enumerate() {
    let elem_ptr = unsafe {
      builder.build_gep(
        arr_ty,
        arr_storage,
        &[i32_t.const_int(0, false), i32_t.const_int(i as u64, false)],
        "lit_elem",
      )
    }
    .map_err(|e| e.to_string())?;
    builder
      .build_store(elem_ptr, *fv)
      .map_err(|e| e.to_string())?;
  }
  let data = ge_ptr_to_first_double(builder, f64_t, i32_t, arr_storage, n)?;
  Ok(CompiledVal::Array {
    data_ptr: data,
    len: n,
  })
}

pub(crate) fn build_stack_array_from_f64s<'ctx>(
  context: &'ctx Context,
  builder: &Builder<'ctx>,
  floats: &[f64],
) -> Result<CompiledVal<'ctx>, String> {
  let f64_t = context.f64_type();
  let elems: Vec<FloatValue<'ctx>> = floats.iter().map(|x| f64_t.const_float(*x)).collect();
  build_stack_array_from_float_vals(context, builder, &elems)
}

pub(crate) fn llvm_array_append<'ctx>(
  context: &'ctx Context,
  module: &Module<'ctx>,
  builder: &Builder<'ctx>,
  old_data: PointerValue<'ctx>,
  old_len: usize,
  new_elt: FloatValue<'ctx>,
) -> Result<CompiledVal<'ctx>, String> {
  let f64_t = context.f64_type();
  let i32_t = context.i32_type();
  let i64_t = context.i64_type();
  let malloc_fn = module
    .get_function("malloc")
    .ok_or_else(|| "LLVM module missing malloc declaration".to_string())?;
  let new_len = old_len + 1;
  let nbytes = i64_t.const_int((new_len * core::mem::size_of::<f64>()) as u64, false);
  let call = builder
    .build_call(
      malloc_fn,
      &[BasicMetadataValueEnum::IntValue(nbytes)],
      "malloc_push",
    )
    .map_err(|e| e.to_string())?;
  let raw = match call.try_as_basic_value() {
    ValueKind::Basic(bv) => bv.into_pointer_value(),
    _ => return Err("malloc produced no value".to_string()),
  };
  let new_base = builder
    .build_bit_cast(
      raw,
      f64_t.ptr_type(AddressSpace::default()),
      "push_data",
    )
    .map_err(|e| e.to_string())?
    .into_pointer_value();

  for i in 0..old_len {
    let idx = i32_t.const_int(i as u64, false);
    let src_gep =
      unsafe { builder.build_gep(f64_t, old_data, &[idx], "push_src") }.map_err(|e| e.to_string())?;
    let v = builder
      .build_load(f64_t, src_gep, "push_lv")
      .map_err(|e| e.to_string())?;
    let dst_gep =
      unsafe { builder.build_gep(f64_t, new_base, &[idx], "push_dst") }.map_err(|e| e.to_string())?;
    builder.build_store(dst_gep, v).map_err(|e| e.to_string())?;
  }
  let last_idx = i32_t.const_int(old_len as u64, false);
  let dst_last =
    unsafe { builder.build_gep(f64_t, new_base, &[last_idx], "push_last") }.map_err(|e| e.to_string())?;
  builder
    .build_store(dst_last, new_elt)
    .map_err(|e| e.to_string())?;

  Ok(CompiledVal::Array {
    data_ptr: new_base,
    len: new_len,
  })
}
