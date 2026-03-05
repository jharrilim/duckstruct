mod manifest;
mod modules;

use std::path::Path;
#[cfg(feature = "llvm")]
use std::process::Command;

use ast::Root;
use codegen::{js::JsGenerator, CodeGenerator};
use duckstruct_std;
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
    Compiler::default()
  }

  pub fn compile_js(&self, source: &str) -> Result<String, String> {
    let parse = parse(source);
    if !parse.errors.is_empty() {
      return Err(format!("{:?}", parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer();
    Ok(JsGenerator::new(&tycheck).generate())
  }

  #[cfg(feature = "llvm")]
  pub fn compile_llvm(&self, source: &str) -> Result<String, String> {
    let parse = parse(source);
    if !parse.errors.is_empty() {
      return Err(format!("{:?}", parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer();
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
      return Err(format!("Parse errors: {:?}", parse.errors));
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
    let global_external_fns = duckstruct_std::external_functions_for_backend(std_backend);
    let global_external_fns_ref = if global_external_fns.is_empty() {
      None
    } else {
      Some(global_external_fns.as_slice())
    };

    let (code, llvm_object_path) = if modules::collect_use_deps(&ast).is_empty() {
      match target {
        TargetLang::Javascript => (self.compile_js(&source)?, None::<std::path::PathBuf>),
        TargetLang::Llvm => {
          #[cfg(not(feature = "llvm"))]
          return Err("LLVM backend requires building with --features llvm and having LLVM installed".to_string());
          #[cfg(feature = "llvm")]
          {
            let hir = lower(ast.clone());
            let mut tycheck = TyCheck::new(hir);
            tycheck.infer_with_modules(None, prelude_ref, global_external_fns_ref);
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
        modules::load_module_tree(&entry_path, project_root.as_deref())?;
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
        global_external_fns_ref,
      );

      let all_deps_are_stdlib = deps
        .iter()
        .all(|d| duckstruct_std::is_stdlib_module(&d.name));

      match target {
        TargetLang::Javascript => {
          let mut bundle = String::new();
          for dep in &deps {
            let pub_names: std::collections::HashSet<String> = dep
              .tycheck
              .ty_db
              .defs_iter()
              .into_iter()
              .filter_map(|(name, stmt)| {
                let pub_vis = match stmt {
                  tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. }
                  | tycheck::typed_hir::TypedStmt::FunctionDef { pub_vis, .. } => *pub_vis,
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
            let dep_js = JsGenerator::new(&dep.tycheck)
              .with_prefix(&prefix, pub_names)
              .generate_js();
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

          let entry_js = JsGenerator::new(&entry_tycheck)
            .with_import_map(import_map)
            .generate_js();
          bundle.push_str(&entry_js);

          (bundle, None)
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
      return Err(format!("{:?}", parse.errors));
    }
    let ast = match Root::cast(parse.syntax()) {
      Some(ast) => ast,
      None => return Err("Failed to generate AST from source".to_string()),
    };
    let hir = lower(ast);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer();
    if let Some(def) = tycheck.ty_db.definition("") {
      let val = tycheck.ty_db.expr(def.value());
      Ok(val.ty().to_string())
    } else {
      Err("No value to evaluate".to_string())
    }
  }
}
