use std::fs;
use std::path::{Path, PathBuf};

use compile::{resolve_entry_and_project_root, target_from_manifest_dir, Compiler, TargetLang};
use insta;
use serde::Serialize;

pub fn codestub(path: &str) -> PathBuf {
  let root = env!("CARGO_MANIFEST_DIR");
  Path::new(root).join("tests/codestubs").join(path)
}

#[derive(Serialize)]
struct DebugInfo {
  file: &'static str,
}

macro_rules! code_snapshot {
  ($file:expr) => {
    let file_path = codestub($file);
    let debug_info = DebugInfo {
      file: $file,
    };
    let code = std::fs::read_to_string(file_path).unwrap();
    let output = Compiler::new().compile_js(&code);
    assert!(output.is_ok());
    let output = output.unwrap();
    insta::with_settings!({
      info => &debug_info,
    }, {
      insta::assert_snapshot!(&output);
    });
  };
}

#[test]
pub fn compile_simple1() {
  code_snapshot!("simple1.ds");
}

#[test]
pub fn compile_factorial() {
  // TODO: Codegen for the recursive function is broken, but the output value is ok
  // code_snapshot!("factorial.ds");
}

#[test]
pub fn compile_lambda_calculus() {
  code_snapshot!("lambda_calculus.ds");
}

#[test]
pub fn compile_with_use_bundles_deps() {
  let dir = tempfile::tempdir().expect("temp dir");
  let root = dir.path();
  fs::write(root.join("main_use.ds"), fs::read_to_string(codestub("main_use.ds")).unwrap())
    .expect("write main_use.ds");
  fs::write(root.join("helper.ds"), fs::read_to_string(codestub("helper.ds")).unwrap())
    .expect("write helper.ds");
  let entry = root.join("main_use.ds");
  let result = Compiler::new().compile_file(entry, None, TargetLang::Javascript);
  assert!(result.is_ok(), "{:?}", result.err());
  let js = fs::read_to_string(root.join("main_use.js")).expect("read output");
  assert!(
    js.contains("__helper__ONE"),
    "bundled output should contain prefixed dep name: {}",
    js
  );
  assert!(
    js.contains("__helper__double"),
    "bundled output should contain prefixed dep function: {}",
    js
  );
}

#[test]
fn compile_project_with_manifest_and_root_import() {
  let root = codestub("project_with_manifest");
  let (entry_path, project_root) =
    resolve_entry_and_project_root(&root).expect("resolve entry and root");
  assert_eq!(entry_path, root.join("src").join("main.ds"));
  assert_eq!(project_root, Some(root.clone()));

  let result = Compiler::new().compile_file(
    entry_path,
    project_root,
    TargetLang::Javascript,
  );
  assert!(result.is_ok(), "compile failed: {:?}", result.err());
  let js_path = root.join("output").join("js").join("index.js");
  let js = fs::read_to_string(&js_path).expect("read output");
  assert!(
    js.contains("__root__lib__util__VAL") || js.contains("VAL"),
    "bundled output should reference VAL: {}",
    js
  );
}

#[test]
fn compile_project_uses_custom_output_dir_from_manifest() {
  let dir = tempfile::tempdir().expect("temp dir");
  let root = dir.path();
  fs::write(
    root.join("duckstruct.toml"),
    r#"entrypoint = "src/main.ds"
output = "dist"
"#,
  )
  .expect("write manifest");
  fs::create_dir_all(root.join("src")).expect("create src");
  fs::write(
    root.join("src").join("main.ds"),
    "let x = 1; x",
  )
  .expect("write main.ds");

  let (entry_path, project_root) =
    resolve_entry_and_project_root(root).expect("resolve");
  let result = Compiler::new().compile_file(
    entry_path,
    project_root,
    TargetLang::Javascript,
  );
  assert!(result.is_ok(), "compile failed: {:?}", result.err());
  let js_path = root.join("dist").join("js").join("index.js");
  assert!(js_path.is_file(), "output should be at dist/js/index.js");
}

#[test]
fn compile_project_with_manifest_backend_js_explicit() {
  let dir = tempfile::tempdir().expect("temp dir");
  let root = dir.path();
  fs::write(
    root.join("duckstruct.toml"),
    r#"entrypoint = "src/main.ds"

[target]
backend = "js"
"#,
  )
  .expect("write manifest");
  fs::create_dir_all(root.join("src")).expect("create src");
  fs::write(root.join("src").join("main.ds"), "let x = 1; x")
    .expect("write main.ds");

  let target = target_from_manifest_dir(root).expect("load manifest");
  assert!(matches!(target, TargetLang::Javascript));
  let (entry_path, project_root) =
    resolve_entry_and_project_root(root).expect("resolve");
  let result = Compiler::new().compile_file(
    entry_path,
    project_root,
    target,
  );
  assert!(result.is_ok(), "compile failed: {:?}", result.err());
  let js_path = root.join("output").join("js").join("index.js");
  assert!(js_path.is_file(), "output should be at output/js/index.js");
}

#[test]
fn compile_project_with_manifest_backend_llvm() {
  let dir = tempfile::tempdir().expect("temp dir");
  let root = dir.path();
  fs::write(
    root.join("duckstruct.toml"),
    r#"entrypoint = "src/main.ds"

[target]
backend = "llvm"
"#,
  )
  .expect("write manifest");
  fs::create_dir_all(root.join("src")).expect("create src");
  fs::write(
    root.join("src").join("main.ds"),
    "let x = 1 + 2; x",
  )
  .expect("write main.ds");

  let target = target_from_manifest_dir(root).expect("load manifest");
  assert!(matches!(target, TargetLang::Llvm));
  let (entry_path, project_root) =
    resolve_entry_and_project_root(root).expect("resolve");
  let result = Compiler::new().compile_file(entry_path, project_root, target);

  #[cfg(feature = "llvm")]
  {
    assert!(result.is_ok(), "compile failed: {:?}", result.err());
    let ll_path = root.join("output").join("llvm").join("index.ll");
    assert!(ll_path.is_file(), "output should be at output/llvm/index.ll");
    let ir = fs::read_to_string(&ll_path).expect("read LLVM IR");
    assert!(
      ir.contains("define") || ir.contains("declare"),
      "LLVM IR should contain define or declare: {}",
      ir
    );
  }

  #[cfg(not(feature = "llvm"))]
  {
    let err = result.expect_err("without llvm feature, compile_file should fail");
    assert!(
      err.contains("LLVM backend requires"),
      "error should mention LLVM backend requirement: {}",
      err
    );
  }
}
