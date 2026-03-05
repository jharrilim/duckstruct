//! Standard library `file` module (use file::{ read, write }).

use ast::Root;
use hir;
use parser::parse;
use tycheck::TyCheck;

/// Embedded source for the file module. Stub bodies so that typecheck succeeds;
/// LLVM codegen will emit declarations and the actual symbols are provided at link time.
const FILE_MODULE_SOURCE: &str = r#"
pub f read(path) = 0;
pub f write(path, content) = 0;
"#;

/// Build the type-checked file module.
pub fn file_module_tycheck() -> Result<TyCheck, String> {
  let parse = parse(FILE_MODULE_SOURCE);
  if !parse.errors.is_empty() {
    return Err(format!("file module parse errors: {:?}", parse.errors));
  }
  let ast = Root::cast(parse.syntax())
    .ok_or_else(|| "Failed to build AST for file module".to_string())?;
  let hir = hir::lower(ast);
  let mut tycheck = TyCheck::new(hir);
  tycheck.infer();
  Ok(tycheck)
}
