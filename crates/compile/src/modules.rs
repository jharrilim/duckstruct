//! Module resolution and dependency loading for the Rust-like import system.
//! One file = one module; module name = filename without extension.
//! Standard library modules (e.g. `file`) are resolved from duckstruct_std, not the filesystem.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ast::Root;
use duckstruct_std;
use hir;
use parser::parse;
use tycheck::TyCheck;

/// Resolve a module name to a file path. When the first segment is `root`, resolve from
/// `project_root`; otherwise resolve relative to the current file's directory.
/// Returns an error if `root::` is used but `project_root` is None.
pub fn resolve_module_path(
  current_file: &Path,
  module_name: &str,
  project_root: Option<&Path>,
) -> Result<PathBuf, String> {
  let parts: Vec<&str> = module_name.split("::").collect();
  let (base, path_parts): (&Path, _) = if parts.first().map(|s| *s == "root").unwrap_or(false) {
    let root = match project_root {
      Some(r) => r,
      None => {
        return Err(
          "`root::` requires a project with a manifest file (duckstruct.toml)".to_string(),
        );
      }
    };
    (root, &parts[1..])
  } else {
    let base = current_file
      .parent()
      .unwrap_or_else(|| Path::new("."));
    (base, parts.as_slice())
  };

  if path_parts.is_empty() {
    return Err(format!("Invalid module path: {}", module_name));
  }
  if path_parts.len() == 1 {
    Ok(base.join(format!("{}.ds", path_parts[0])))
  } else {
    let mut p = base.to_path_buf();
    for (i, part) in path_parts.iter().enumerate() {
      if i == path_parts.len() - 1 {
        p.push(format!("{}.ds", part));
      } else {
        p.push(part);
      }
    }
    Ok(p)
  }
}

/// Collect module names that are used in the given AST.
/// For `use foo::bar` the module is `foo`; for `use subdir::helper::item` the module is `subdir::helper`.
pub fn collect_use_deps(ast: &Root) -> Vec<String> {
  let mut deps = Vec::new();
  for stmt in ast.stmts() {
    if let ast::Stmt::Use(use_stmt) = stmt {
      if let Some(path) = use_stmt.path() {
        let segments = path.segments();
        if segments.is_empty() {
          continue;
        }
        let mod_key = if segments.len() == 1 {
          segments[0].clone()
        } else {
          segments.join("::")
        };
        if !deps.contains(&mod_key) {
          deps.push(mod_key);
        }
      }
    }
  }
  deps
}

/// Result of loading a single module: name and type-checked state (includes HIR).
pub struct LoadedModule {
  pub name: String,
  pub tycheck: TyCheck,
}

/// Load a module and all its dependencies. Returns the entry's HIR (with uses) and
/// dependency modules in topological order. The entry is NOT typechecked here;
/// the caller should typecheck it with a module map from deps.
/// Detects circular dependencies. `project_root` is used to resolve `use root::...` paths.
pub fn load_module_tree(
  entry_path: &Path,
  project_root: Option<&Path>,
) -> Result<(hir::Database, Vec<LoadedModule>), String> {
  let source = std::fs::read_to_string(entry_path)
    .map_err(|e| format!("Failed to read {}: {}", entry_path.display(), e))?;
  let parse = parse(&source);
  if !parse.errors.is_empty() {
    return Err(format!("Parse errors: {:?}", parse.errors));
  }
  let ast = Root::cast(parse.syntax())
    .ok_or_else(|| "Failed to build AST".to_string())?;

  let mut loading = HashSet::new();
  let mut loaded_paths: HashSet<PathBuf> = HashSet::new();
  let mut dep_order: Vec<LoadedModule> = Vec::new();

  load_deps_recurse(
    entry_path,
    &ast,
    project_root,
    &mut loading,
    &mut loaded_paths,
    &mut dep_order,
  )?;

  let entry_hir = hir::lower(ast);
  Ok((entry_hir, dep_order))
}

fn load_deps_recurse(
  current_path: &Path,
  ast: &Root,
  project_root: Option<&Path>,
  loading: &mut HashSet<PathBuf>,
  loaded_paths: &mut HashSet<PathBuf>,
  dep_order: &mut Vec<LoadedModule>,
) -> Result<(), String> {
  let deps = collect_use_deps(ast);
  for mod_name in deps {
    if duckstruct_std::is_stdlib_module(&mod_name) {
      let synthetic_path = PathBuf::from(format!("std::{}", mod_name));
      if loading.contains(&synthetic_path) {
        return Err(format!(
          "Circular dependency involving module `{}`",
          mod_name
        ));
      }
      if loaded_paths.contains(&synthetic_path) {
        continue;
      }
      loading.insert(synthetic_path.clone());
      let stdlib_module = duckstruct_std::load_stdlib_module(&mod_name)?;
      dep_order.push(LoadedModule {
        name: stdlib_module.name,
        tycheck: stdlib_module.tycheck,
      });
      loaded_paths.insert(synthetic_path.clone());
      loading.remove(&synthetic_path);
      continue;
    }

    let dep_path = resolve_module_path(current_path, &mod_name, project_root)?;
    let canonical = dep_path.canonicalize().unwrap_or_else(|_| dep_path.clone());

    if loading.contains(&canonical) {
      return Err(format!(
        "Circular dependency involving module `{}`",
        mod_name
      ));
    }
    if loaded_paths.contains(&canonical) {
      continue;
    }

    loading.insert(canonical.clone());
    let dep_source = std::fs::read_to_string(&dep_path)
      .map_err(|e| format!("Failed to read {}: {}", dep_path.display(), e))?;
    let dep_parse = parse(&dep_source);
    if !dep_parse.errors.is_empty() {
      return Err(format!(
        "Parse errors in {}: {:?}",
        dep_path.display(),
        dep_parse.errors
      ));
    }
    let dep_ast = Root::cast(dep_parse.syntax())
      .ok_or_else(|| format!("Failed to build AST for {}", dep_path.display()))?;

    load_deps_recurse(
      &dep_path,
      &dep_ast,
      project_root,
      loading,
      loaded_paths,
      dep_order,
    )?;

    let dep_hir = hir::lower(dep_ast);
    let mut dep_tycheck = TyCheck::new(dep_hir);
    dep_tycheck.infer();

    dep_order.push(LoadedModule {
      name: mod_name,
      tycheck: dep_tycheck,
    });
    loaded_paths.insert(canonical.clone());
    loading.remove(&canonical);
  }
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  /// Importing pub items from another file in the same directory.
  #[test]
  fn test_import_pub_items_from_file() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("main.ds");
    let helper_ds = root.join("helper.ds");

    fs::write(
      &main_ds,
      r#"
use helper::{ONE};
let x = ONE;
x
"#,
    )
    .expect("write main.ds");
    fs::write(
      &helper_ds,
      r#"
pub let ONE = 1;
"#,
    )
    .expect("write helper.ds");

    let result = load_module_tree(&main_ds, None);
    assert!(result.is_ok(), "load_module_tree failed: {:?}", result.err());
    let (_entry_hir, deps) = result.unwrap();
    assert_eq!(deps.len(), 1, "expected one dependency");
    assert_eq!(deps[0].name, "helper");
    let dep = &deps[0];
    let def = dep
      .tycheck
      .ty_db
      .definition("ONE")
      .expect("helper should define ONE");
    let is_pub = match def {
      tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. } => *pub_vis,
      _ => false,
    };
    assert!(is_pub, "ONE should be public");
  }

  /// Importing from a file that lives in a subdirectory (e.g. subdir/helper.ds).
  #[test]
  fn test_import_from_file_in_directory() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("main.ds");
    let subdir = root.join("subdir");
    let helper_ds = subdir.join("helper.ds");

    fs::create_dir_all(&subdir).expect("create subdir");
    fs::write(
      &main_ds,
      r#"
use subdir::helper::{TWO};
let x = TWO;
x
"#,
    )
    .expect("write main.ds");
    fs::write(
      &helper_ds,
      r#"
pub let TWO = 2;
"#,
    )
    .expect("write subdir/helper.ds");

    let result = load_module_tree(&main_ds, None);
    assert!(result.is_ok(), "load_module_tree failed: {:?}", result.err());
    let (_entry_hir, deps) = result.unwrap();
    assert_eq!(deps.len(), 1, "expected one dependency");
    assert_eq!(deps[0].name, "subdir::helper");
    let dep = &deps[0];
    let def = dep
      .tycheck
      .ty_db
      .definition("TWO")
      .expect("subdir/helper should define TWO");
    let is_pub = match def {
      tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. } => *pub_vis,
      _ => false,
    };
    assert!(is_pub, "TWO should be public");
  }

  /// No conflict between a module name and an exported item of the same name.
  /// `use foo::{foo}` imports the item `foo` from module `foo`; the syntax
  /// disambiguates (module path before `::`, items inside `{}`).
  #[test]
  fn test_no_conflict_module_name_and_exported_item_same_name() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("main.ds");
    let foo_ds = root.join("foo.ds");

    fs::write(
      &main_ds,
      r#"
use foo::{foo};
let x = foo;
x
"#,
    )
    .expect("write main.ds");
    fs::write(
      &foo_ds,
      r#"
pub let foo = 42;
"#,
    )
    .expect("write foo.ds");

    let result = load_module_tree(&main_ds, None);
    assert!(result.is_ok(), "load_module_tree failed: {:?}", result.err());
    let (_entry_hir, deps) = result.unwrap();
    assert_eq!(deps.len(), 1, "expected one dependency");
    assert_eq!(deps[0].name, "foo");
    let dep = &deps[0];
    let def = dep
      .tycheck
      .ty_db
      .definition("foo")
      .expect("foo module should define foo");
    let is_pub = match def {
      tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. } => *pub_vis,
      _ => false,
    };
    assert!(is_pub, "foo should be public");
  }

  /// `use root::lib::util::{X}` resolves from project root when project_root is Some.
  #[test]
  fn test_use_root_resolves_from_project_root() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("src").join("main.ds");
    let lib_util_ds = root.join("lib").join("util.ds");
    fs::create_dir_all(main_ds.parent().unwrap()).expect("create src");
    fs::create_dir_all(lib_util_ds.parent().unwrap()).expect("create lib");

    fs::write(
      &main_ds,
      r#"
use root::lib::util::{THREE};
let x = THREE;
x
"#,
    )
    .expect("write main.ds");
    fs::write(
      &lib_util_ds,
      r#"
pub let THREE = 3;
"#,
    )
    .expect("write lib/util.ds");

    let result = load_module_tree(&main_ds, Some(root));
    assert!(result.is_ok(), "load_module_tree failed: {:?}", result.err());
    let (_entry_hir, deps) = result.unwrap();
    assert_eq!(deps.len(), 1, "expected one dependency");
    assert_eq!(deps[0].name, "root::lib::util");
    let dep = &deps[0];
    let def = dep
      .tycheck
      .ty_db
      .definition("THREE")
      .expect("lib::util should define THREE");
    let is_pub = match def {
      tycheck::typed_hir::TypedStmt::VariableDef { pub_vis, .. } => *pub_vis,
      _ => false,
    };
    assert!(is_pub, "THREE should be public");
  }

  /// `use file::{read}` loads the stdlib file module, not a file.ds from disk.
  #[test]
  fn test_use_file_loads_stdlib_module() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("main.ds");
    fs::write(
      &main_ds,
      r#"
use file::{read};
read(0)
"#,
    )
    .expect("write main.ds");

    let result = load_module_tree(&main_ds, None);
    assert!(result.is_ok(), "load_module_tree failed: {:?}", result.err());
    let (_entry_hir, deps) = result.unwrap();
    assert_eq!(deps.len(), 1, "expected one dependency (file)");
    assert_eq!(deps[0].name, "file");
    let def = deps[0]
      .tycheck
      .ty_db
      .definition("read")
      .expect("file module should define read");
    let is_pub = match def {
      tycheck::typed_hir::TypedStmt::FunctionDef { pub_vis, .. } => *pub_vis,
      _ => false,
    };
    assert!(is_pub, "read should be public");
  }

  /// `use root::...` without a project root (no manifest) returns an error.
  #[test]
  fn test_use_root_without_manifest_errors() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    let main_ds = root.join("main.ds");
    fs::write(
      &main_ds,
      r#"
use root::foo::{bar};
let x = bar;
x
"#,
    )
    .expect("write main.ds");

    let result = load_module_tree(&main_ds, None);
    let err = match &result {
      Err(e) => e.clone(),
      Ok(_) => panic!("expected load_module_tree to fail with root:: and no manifest"),
    };
    assert!(
      err.contains("root::"),
      "error should mention root::: {}",
      err
    );
    assert!(
      err.contains("manifest"),
      "error should mention manifest: {}",
      err
    );
  }
}
