//! Generate LLVM IR from the typed HIR.
//! Supports a subset: numbers, functions, arithmetic, conditionals, blocks.
//! Objects, arrays, strings, and for-loops are not yet supported.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
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

  fn generate_llvm(&self) -> Result<String, String> {
    let context = Context::create();
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
              &context,
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

    let ir = module.print_to_string().to_string();
    Ok(ir)
  }

  fn compile_expr_with_locals(
    &self,
    expr: &TypedDatabaseIdx,
    context: &Context,
    module: &Module,
    builder: &Builder,
    global_ptrs: &HashMap<String, PointerValue>,
    functions: &HashMap<String, FunctionValue>,
    locals: &HashMap<String, BasicValueEnum>,
    _current_fn: Option<FunctionValue>,
  ) -> Result<BasicValueEnum, String> {
    let expr_node = self.tycheck.ty_db.expr(expr);
    let f64_type = context.f64_type();

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
                globals,
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
        last.ok_or_else(|| "empty block".to_string())?
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
            globals,
            functions,
            locals,
            _current_fn,
          )?;
          arg_vals.push(v.into_float_value());
        }
        let args_ref: Vec<_> = arg_vals.iter().collect();
        let result = builder
          .build_call(callee, &args_ref, "call")
          .map_err(|e| e.to_string())?
          .try_as_basic_value()
          .left()
          .ok_or_else(|| "call must return a value".to_string())?;
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
