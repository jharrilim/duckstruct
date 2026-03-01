//! Module resolution and dependency loading for the Rust-like import system.
//! One file = one module; module name = filename without extension.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ast::Root;
use hir;
use parser::parse;
use tycheck::TyCheck;

/// Resolve a module name to a file path relative to the current file's directory.
/// E.g. `resolve_module_path("/dir/main.ds", "foo")` -> `/dir/foo.ds`
pub fn resolve_module_path(current_file: &Path, module_name: &str) -> PathBuf {
  current_file
    .parent()
    .unwrap_or_else(|| Path::new("."))
    .join(format!("{}.ds", module_name))
}

/// Collect module names that are used in the given AST (first segment of each `use` path).
pub fn collect_use_deps(ast: &Root) -> Vec<String> {
  let mut deps = Vec::new();
  for stmt in ast.stmts() {
    if let ast::Stmt::Use(use_stmt) = stmt {
      if let Some(path) = use_stmt.path() {
        let segments = path.segments();
        if let Some(first) = segments.first() {
          if !deps.contains(first) {
            deps.push(first.clone());
          }
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
/// Detects circular dependencies.
pub fn load_module_tree(
  entry_path: &Path,
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
  loading: &mut HashSet<PathBuf>,
  loaded_paths: &mut HashSet<PathBuf>,
  dep_order: &mut Vec<LoadedModule>,
) -> Result<(), String> {
  let deps = collect_use_deps(ast);
  for mod_name in deps {
    let dep_path = resolve_module_path(current_path, &mod_name);
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
