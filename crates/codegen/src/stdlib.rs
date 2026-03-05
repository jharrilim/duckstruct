//! LLVM codegen for standard library builtins (e.g. print).
//! Implementations are emitted into the module; link with -lc for libc (printf).

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue};

/// Adds the body of the `print` function to the module.
/// `print_fn` must be the already-declared function (e.g. `f_print(double) -> double`).
/// Implements print by calling printf from libc with format "%f\n".
pub fn add_print_implementation<'ctx>(
  context: &'ctx Context,
  module: &Module<'ctx>,
  builder: &Builder<'ctx>,
  print_fn: FunctionValue<'ctx>,
  f64_type: inkwell::types::FloatType<'ctx>,
) -> Result<(), String> {
  let i8_type = context.i8_type();
  let i8_ptr = i8_type.ptr_type(inkwell::AddressSpace::default());
  let i32_type = context.i32_type();
  let printf_ty = i32_type.fn_type(&[i8_ptr.into()], true);
  let printf_fn = module.add_function("printf", printf_ty, None);
  printf_fn.set_linkage(inkwell::module::Linkage::External);

  let entry = context.append_basic_block(print_fn, "entry");
  builder.position_at_end(entry);
  let format_str = builder
    .build_global_string_ptr("%f\n", "print_fmt")
    .map_err(|e| e.to_string())?;
  let format_ptr = format_str.as_pointer_value();
  let arg = print_fn.get_nth_param(0).unwrap().into_float_value();
  let args: Vec<BasicMetadataValueEnum> = vec![format_ptr.into(), arg.into()];
  let _ = builder.build_call(printf_fn, &args, "printf_call");
  builder
    .build_return(Some(&f64_type.const_float(0.0)))
    .map_err(|e| e.to_string())?;
  Ok(())
}
