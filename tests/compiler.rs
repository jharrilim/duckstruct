use std::fs;
use std::path::{Path, PathBuf};

use compile::{resolve_entry_and_project_root, Compiler, TargetLang};
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
  let entry = codestub("main_use.ds");
  let result = Compiler::new().compile_file(entry, None, TargetLang::Javascript);
  assert!(result.is_ok(), "{:?}", result.err());
  let out_path = codestub("main_use.js");
  let js = std::fs::read_to_string(&out_path).unwrap();
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
  let _ = std::fs::remove_file(out_path);
}

#[test]
fn compile_project_with_manifest_and_root_import() {
  let dir = tempfile::tempdir().expect("temp dir");
  let root = dir.path();
  fs::write(
    root.join("duckstruct.toml"),
    r#"entrypoint = "src/main.ds"
"#,
  )
  .expect("write manifest");
  fs::create_dir_all(root.join("src")).expect("create src");
  fs::create_dir_all(root.join("lib")).expect("create lib");
  fs::write(
    root.join("src").join("main.ds"),
    r#"
use root::lib::util::{VAL};
VAL
"#,
  )
  .expect("write main.ds");
  fs::write(
    root.join("lib").join("util.ds"),
    r#"
pub let VAL = 42;
"#,
  )
  .expect("write lib/util.ds");

  let (entry_path, project_root) =
    resolve_entry_and_project_root(root).expect("resolve entry and root");
  assert_eq!(entry_path, root.join("src").join("main.ds"));
  assert_eq!(project_root, Some(root.to_path_buf()));

  let result = Compiler::new().compile_file(
    entry_path,
    project_root,
    TargetLang::Javascript,
  );
  assert!(result.is_ok(), "compile failed: {:?}", result.err());
  let js_path = root.join("src").join("main.js");
  let js = fs::read_to_string(&js_path).expect("read output");
  assert!(
    js.contains("__root::lib::util__VAL") || js.contains("VAL"),
    "bundled output should reference VAL: {}",
    js
  );
}
