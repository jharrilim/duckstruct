pub mod manifest;
mod modules;

pub mod ide;

use std::path::Path;

use diagnostics::{bundle_from_parse_errors, emit_human_string, HumanEmitConfig};
use parser::ParseError;
#[cfg(feature = "llvm")]
use std::process::Command;

use ast::Root;
use codegen::js::JsGenerator;
use hir::lower;
use parser::parse;
use tycheck::TyCheck;

#[cfg(feature = "llvm")]
use codegen::llvm::LlvmGenerator;

/// Link an object file to an executable using the system linker (clang or cc).
#[cfg(feature = "llvm")]
fn link_object_to_executable(obj_path: &Path, exe_path: &Path) -> Result<(), String> {
  for linker in ["clang", "cc"] {
    let status = match Command::new(linker)
      .arg(obj_path)
      .arg("-o")
      .arg(exe_path)
      .arg("-lc")
      .status()
    {
      Ok(s) => s,
      Err(_) => continue,
    };
    if status.success() {
      return Ok(());
    }
    return Err(format!(
      "Linker {} exited with code {:?}",
      linker,
      status.code()
    ));
  }
  Err(
    "No linker found (tried clang, cc). Install clang or gcc to link executables.".to_string(),
  )
}

pub enum TargetLang {
  Javascript,
  Llvm,
}

fn format_parse_errors(source: &str, file_label: &str, errors: &[ParseError]) -> String {
  let b = bundle_from_parse_errors(errors);
  emit_human_string(source, &b, &HumanEmitConfig::from_env(file_label))
}

fn format_tycheck_errors(source: &str, file_label: &str, tycheck: &TyCheck) -> String {
  emit_human_string(
    source,
    &tycheck.diagnostics.bundle,
    &HumanEmitConfig::from_env(file_label),
  )
}

/// Prepend the JS runtime block before the generated body. Empty runtime returns the body
/// unchanged so projects that never touch a routed primitive method don't ship dead code.
fn prepend_runtime(runtime: &str, body: &str) -> String {
  if runtime.is_empty() {
    body.to_string()
  } else if runtime.ends_with('\n') {
    format!("{}{}", runtime, body)
  } else {
    format!("{}\n{}", runtime, body)
  }
}

impl From<manifest::Backend> for TargetLang {
  fn from(b: manifest::Backend) -> Self {
    match b {
      manifest::Backend::Js => TargetLang::Javascript,
      manifest::Backend::Llvm => TargetLang::Llvm,
    }
  }
}

pub struct Compiler;

/// Resolve the target (backend) from the project manifest. Use when compiling with a project root.
pub fn target_from_manifest_dir(manifest_dir: &Path) -> Result<TargetLang, String> {
  let m = manifest::load_manifest(manifest_dir)?;
  Ok(m.backend().into())
}

/// Resolve the entry file path and optional project root from a path (file or directory).
/// When `path` is a directory, looks for `duckstruct.toml` and uses its `entrypoint`; returns error if no manifest.
/// When `path` is a file, uses it as entry and optionally the manifest dir as project root.
pub fn resolve_entry_and_project_root(
  path: &Path,
) -> Result<(std::path::PathBuf, Option<std::path::PathBuf>), String> {
  if path.is_dir() {
    let root = manifest::find_manifest_dir(path).map_err(|e| {
      format!(
        "{}. When compiling a directory, a manifest is required.",
        e
      )
    })?;
    let m = manifest::load_manifest(&root)?;
    let entry = root.join(&m.entrypoint);
    Ok((entry, Some(root)))
  } else {
    let root = manifest::find_manifest_dir(path).ok();
    Ok((path.to_path_buf(), root))
  }
}

impl Default for Compiler {
  fn default() -> Self {
    Self
  }
}

impl Compiler {
  pub fn new() -> Self {
    Self
  }

  pub fn compile_js(&self, source: &str) -> Result<String, String> {
    let parse = parse(source);
    if !parse.errors.is_empty() {
      return Err(format_parse_errors(source, "<input>", &parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      None,
      None,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    if tycheck.diagnostics.has_errors() {
      return Err(format_tycheck_errors(source, "<input>", &tycheck));
    }
    let generator = JsGenerator::new(&tycheck)
      .with_primitive_methods(duckstruct_std::PRIMITIVE_METHODS);
    let body = generator.generate_js();
    let runtime = duckstruct_std::js_runtime_for_ids(&generator.used_runtime_ids());
    Ok(prepend_runtime(&runtime, &body))
  }

  #[cfg(feature = "llvm")]
  pub fn compile_llvm(&self, source: &str) -> Result<String, String> {
    let parse = parse(source);
    if !parse.errors.is_empty() {
      return Err(format_parse_errors(source, "<input>", &parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      None,
      None,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    if tycheck.diagnostics.has_errors() {
      return Err(format_tycheck_errors(source, "<input>", &tycheck));
    }
    LlvmGenerator::new(&tycheck).generate_llvm()
  }

  /// Compile the file at `entry_path`. `project_root` is used to resolve `use root::...` imports;
  /// when `None`, any `root::` use is an error. When `project_root` is set, output goes to
  /// &lt;output_dir&gt;/js/index.js or &lt;output_dir&gt;/llvm/index.ll (output_dir from manifest `output` key, default "output").
  pub fn compile_file(
    &self,
    entry_path: std::path::PathBuf,
    project_root: Option<std::path::PathBuf>,
    target: TargetLang,
  ) -> Result<(), String> {
    #[cfg_attr(not(feature = "llvm"), allow(unused_variables))]
    let (output_path, link_executable, executable_name) = match &project_root {
      Some(root) => {
        let m = manifest::load_manifest(root)?;
        let output_dir = m
          .output
          .as_deref()
          .unwrap_or(manifest::DEFAULT_OUTPUT_DIR);
        let exe_name = Some(m.executable_name().to_string());
        match target {
          TargetLang::Javascript => {
            (root.join(output_dir).join("js").join("index.js"), false, None)
          }
          TargetLang::Llvm => (
            root.join(output_dir).join("llvm").join("index.ll"),
            m.link_executable(),
            exe_name,
          ),
        }
      }
      None => match target {
        TargetLang::Javascript => (entry_path.with_extension("js"), false, None),
        TargetLang::Llvm => (entry_path.with_extension("ll"), false, None),
      },
    };
    if let Some(parent) = output_path.parent() {
      std::fs::create_dir_all(parent)
        .map_err(|e| format!("Failed to create output directory {}: {}", parent.display(), e))?;
    }
    println!("{} => {}", entry_path.display(), output_path.display());
    let source = match std::fs::read_to_string(&entry_path) {
      Ok(source) => source,
      Err(e) => return Err(format!("Failed to read file: {}", e)),
    };
    let parse = parse(&source);
    if !parse.errors.is_empty() {
      return Err(format_parse_errors(
        &source,
        &entry_path.to_string_lossy(),
        &parse.errors,
      ));
    }
    let ast = Root::cast(parse.syntax())
      .ok_or_else(|| "Failed to generate AST from source".to_string())?;

    let std_backend = match target {
      TargetLang::Javascript => duckstruct_std::Backend::Js,
      TargetLang::Llvm => duckstruct_std::Backend::Llvm,
    };
    let prelude = duckstruct_std::globals_for_backend(std_backend);
    let prelude_ref = if prelude.is_empty() {
      None
    } else {
      Some(prelude.as_slice())
    };
    let global_external_sigs = duckstruct_std::external_signatures_for_backend(std_backend);
    let global_external_sigs_ref = if global_external_sigs.is_empty() {
      None
    } else {
      Some(global_external_sigs.as_slice())
    };
    let global_external_fns = duckstruct_std::external_functions_for_backend(std_backend);

    let (code, llvm_object_path) = if modules::collect_use_deps(&ast).is_empty() {
      match target {
        TargetLang::Javascript => {
          let hir = lower(ast.clone());
          let mut tycheck = TyCheck::new(hir);
          tycheck.infer_with_modules(
            None,
            prelude_ref,
            global_external_sigs_ref,
            Some(duckstruct_std::PRIMITIVE_METHODS),
          );
          if tycheck.diagnostics.has_errors() {
            return Err(format_tycheck_errors(
              &source,
              &entry_path.to_string_lossy(),
              &tycheck,
            ));
          }
          let ext_names: std::collections::HashSet<String> =
            global_external_fns.iter().map(|(n, _)| n.clone()).collect();
          let generator = JsGenerator::new(&tycheck)
            .with_primitive_methods(duckstruct_std::PRIMITIVE_METHODS)
            .with_external_functions(ext_names);
          let body = generator.generate_js();
          let runtime =
            duckstruct_std::js_runtime_for_ids(&generator.used_runtime_ids());
          (prepend_runtime(&runtime, &body), None::<std::path::PathBuf>)
        }
        TargetLang::Llvm => {
          #[cfg(not(feature = "llvm"))]
          return Err("LLVM backend requires building with --features llvm and having LLVM installed".to_string());
          #[cfg(feature = "llvm")]
          {
            let hir = lower(ast.clone());
            let mut tycheck = TyCheck::new(hir);
            tycheck.infer_with_modules(
              None,
              prelude_ref,
              global_external_sigs_ref,
              Some(duckstruct_std::PRIMITIVE_METHODS),
            );
            if tycheck.diagnostics.has_errors() {
              return Err(format_tycheck_errors(
                &source,
                &entry_path.to_string_lossy(),
                &tycheck,
              ));
            }
            let external_fns_map: std::collections::HashMap<String, usize> =
              global_external_fns.into_iter().collect();
            let generator = LlvmGenerator::new(&tycheck)
              .with_external_functions(external_fns_map);
            let obj_path = output_path.with_extension("o");
            generator.compile_to_files(&output_path, &obj_path)?;
            println!("{} => {}", entry_path.display(), obj_path.display());
            if link_executable {
              let exe_path = output_path
                .parent()
                .unwrap()
                .join(executable_name.as_deref().unwrap_or("index"));
              link_object_to_executable(&obj_path, &exe_path)?;
              println!("{} => {}", entry_path.display(), exe_path.display());
            }
            (generator.generate_llvm()?, Some(obj_path))
          }
        }
      }
    } else {
      let (entry_hir, deps) =
        modules::load_module_tree(&entry_path, project_root.as_deref(), None)?;
      let entry_uses = entry_hir.uses.clone();
      let mut module_map: std::collections::HashMap<String, &TyCheck> =
        std::collections::HashMap::new();
      for dep in &deps {
        module_map.insert(dep.name.clone(), &dep.tycheck);
      }
      let mut entry_tycheck = TyCheck::new(entry_hir);
      entry_tycheck.infer_with_modules(
        Some(&module_map),
        prelude_ref,
        global_external_sigs_ref,
        Some(duckstruct_std::PRIMITIVE_METHODS),
      );
      if entry_tycheck.diagnostics.has_errors() {
        return Err(format_tycheck_errors(
          &source,
          &entry_path.to_string_lossy(),
          &entry_tycheck,
        ));
      }

      let all_deps_are_stdlib = deps
        .iter()
        .all(|d| duckstruct_std::is_stdlib_module(&d.name));

      match target {
        TargetLang::Javascript => {
          let mut bundle = String::new();
          let mut combined_runtime_ids: std::collections::HashSet<&'static str> =
            std::collections::HashSet::new();
          for dep in &deps {
            let pub_names: std::collections::HashSet<String> = dep
              .tycheck
              .ty_db
              .defs_iter()
              .into_iter()
              .filter_map(|(name, stmt)| {
                let pub_vis = match stmt {
                  tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. }
                  | tycheck::typed_hir::TypedStmt::FunctionDef { pub_vis, .. }
                  | tycheck::typed_hir::TypedStmt::StructDef { pub_vis, .. }
                  | tycheck::typed_hir::TypedStmt::TraitDef { pub_vis, .. } => *pub_vis,
                  _ => false,
                };
                if pub_vis {
                  Some(name.clone())
                } else {
                  None
                }
              })
              .collect();
            let prefix = format!("__{}__", dep.name.replace("::", "__"));
            let dep_generator = JsGenerator::new(&dep.tycheck)
              .with_primitive_methods(duckstruct_std::PRIMITIVE_METHODS)
              .with_prefix(&prefix, pub_names);
            let dep_js = dep_generator.generate_js();
            combined_runtime_ids.extend(dep_generator.used_runtime_ids());
            if !dep_js.is_empty() {
              bundle.push_str(&dep_js);
              bundle.push('\n');
            }
          }

          let import_map: std::collections::HashMap<String, (String, String)> = entry_uses
            .iter()
            .filter(|(path, _)| path.len() >= 2)
            .map(|(path, alias)| {
              let bind = alias
                .as_deref()
                .unwrap_or_else(|| path.last().unwrap())
                .to_string();
              let mod_name = path[0..path.len() - 1].join("__");
              let export = path.last().unwrap().clone();
              (bind, (mod_name, export))
            })
            .collect();

          let ext_names: std::collections::HashSet<String> = global_external_fns
            .iter()
            .map(|(n, _)| n.clone())
            .collect();
          let entry_generator = JsGenerator::new(&entry_tycheck)
            .with_primitive_methods(duckstruct_std::PRIMITIVE_METHODS)
            .with_import_map(import_map)
            .with_external_functions(ext_names);
          let entry_js = entry_generator.generate_js();
          combined_runtime_ids.extend(entry_generator.used_runtime_ids());
          bundle.push_str(&entry_js);

          let runtime = duckstruct_std::js_runtime_for_ids(&combined_runtime_ids);
          (prepend_runtime(&runtime, &bundle), None)
        }
        TargetLang::Llvm => {
          if !all_deps_are_stdlib {
            return Err("LLVM backend does not support multi-module projects yet".to_string());
          }
          #[cfg(not(feature = "llvm"))]
          return Err("LLVM backend requires building with --features llvm".to_string());
          #[cfg(feature = "llvm")]
          {
            let mut external_fns: std::collections::HashMap<String, usize> = global_external_fns
              .into_iter()
              .collect();
            for dep in &deps {
              for (name, stmt) in dep.tycheck.ty_db.defs_iter() {
                if let tycheck::typed_hir::TypedStmt::FunctionDef {
                  pub_vis: true,
                  value,
                  ..
                } = stmt
                {
                  let expr = dep.tycheck.ty_db.expr(value);
                  if let tycheck::typed_hir::TypedExpr::FunctionDef(
                    tycheck::typed_hir::FunctionDef { params, .. },
                  ) = expr
                  {
                    external_fns.insert(name.clone(), params.len());
                  }
                }
              }
            }
            let generator = LlvmGenerator::new(&entry_tycheck)
              .with_external_functions(external_fns);
            let obj_path = output_path.with_extension("o");
            generator.compile_to_files(&output_path, &obj_path)?;
            println!("{} => {}", entry_path.display(), obj_path.display());
            if link_executable {
              let exe_path = output_path
                .parent()
                .unwrap()
                .join(executable_name.as_deref().unwrap_or("index"));
              link_object_to_executable(&obj_path, &exe_path)?;
              println!("{} => {}", entry_path.display(), exe_path.display());
            }
            (generator.generate_llvm()?, Some(obj_path))
          }
        }
      }
    };

    if llvm_object_path.is_none() {
      if let Err(err) = std::fs::write(output_path, code) {
        return Err(format!("Failed to write to file: {}", err));
      }
    }
    Ok(())
  }

  pub fn eval(&self, source: &str) -> Result<String, String> {
    let parse = parse(source);
    if !parse.errors.is_empty() {
      return Err(format_parse_errors(source, "<eval>", &parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      None,
      None,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    if tycheck.diagnostics.has_errors() {
      return Err(format_tycheck_errors(source, "<eval>", &tycheck));
    }
    if let Some(def) = tycheck.ty_db.definition("") {
      let val = tycheck.ty_db.expr(def.value());
      Ok(val.ty().to_string())
    } else {
      Err("No value to evaluate".to_string())
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  /// Constant-fold path: receiver and arg are both compile-time known, so the call's `Ty`
  /// becomes `Ty::Array(Some([..]))` with full values and codegen emits a JS literal. The
  /// list runtime must NOT be prepended because no `js_call` was invoked.
  #[test]
  fn js_push_constant_emits_array_literal_without_runtime() {
    let out = Compiler::new()
      .compile_js("let a = [1]; let b = a.push(2);")
      .expect("compile ok");
    assert!(
      out.contains("[1, 2]"),
      "expected literal [1, 2] in output, got: {}",
      out
    );
    assert!(
      !out.contains("Duckstruct.Lib.Primitive.List"),
      "constant push must not invoke runtime, got: {}",
      out
    );
  }

  /// Non-constant path: `[x]` carries `Ty::Generic` so the partial-evaluated `Ty`
  /// (`Array(Some([Generic, Number(Some(1.0))]))`) can't be rendered as a literal. Codegen
  /// routes the call through `Duckstruct.Lib.Primitive.List.push` and the driver prepends
  /// the list runtime once, before any usage.
  #[test]
  fn js_push_nonconstant_routes_to_runtime_and_prepends_source() {
    // Force the call to live in a `let` value so codegen actually emits it (the block-body
    // codegen drops the trailing expression — pre-existing bug; using a `let` sidesteps it).
    let out = Compiler::new()
      .compile_js("f wrap(x) { let r = [x].push(1); r } wrap(2)")
      .expect("compile ok");
    assert!(
      out.contains("Duckstruct.Lib.Primitive.List.push("),
      "expected runtime call, got: {}",
      out
    );
    assert!(
      out.contains("Duckstruct.Lib.Primitive.List = Duckstruct.Lib.Primitive.List ||"),
      "runtime preamble missing, got: {}",
      out
    );
    let preamble_idx = out
      .find("Duckstruct.Lib.Primitive.List = Duckstruct.Lib.Primitive.List ||")
      .unwrap();
    let call_idx = out.find("Duckstruct.Lib.Primitive.List.push(").unwrap();
    assert!(
      preamble_idx < call_idx,
      "runtime should be prepended before usage, got: {}",
      out
    );
  }

  /// Sanity: a program that never invokes a routed primitive method must not include any
  /// runtime preamble. The empty-runtime branch of `prepend_runtime` should leave the body
  /// untouched.
  #[test]
  fn js_no_primitive_routing_means_no_runtime() {
    let out = Compiler::new()
      .compile_js("let a = [1, 2]; let b = a.length();")
      .expect("compile ok");
    assert!(
      !out.contains("Duckstruct.Lib"),
      "no primitive method routed; runtime must be absent, got: {}",
      out
    );
  }

  #[test]
  fn js_compile_accepts_trait_and_impl_declarations() {
    let out = Compiler::new().compile_js(
      "trait Renderable { f render(x); } struct Foo { } impl Renderable for Foo { f render(x) { x } } let v = 1; v",
    );
    assert!(out.is_ok(), "compile should succeed, got: {:?}", out.err());
  }

  #[test]
  fn js_compile_trait_method_call_dispatches() {
    let out = Compiler::new().compile_js(
      "trait Increment { f inc(value); } impl Increment for number { f inc(value) { value + 1 } } let x = 1.inc(); x",
    );
    assert!(
      out.is_ok(),
      "trait method dispatch compile should succeed, got: {:?}",
      out.err()
    );
  }
}
