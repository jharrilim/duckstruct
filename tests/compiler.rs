use std::path::{Path, PathBuf};

use compile::Compiler;
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
  // code_snapshot!("factorial.ds");
}

#[test]
pub fn compile_lambda_calculus() {
  code_snapshot!("lambda_calculus.ds");
}
