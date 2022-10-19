use ast::Root;
use codegen::{js::JsGenerator, CodeGenerator};
use hir::lower;
use parser::parse;
use tycheck::TyCheck;

pub enum TargetLang {
  Javascript,
}

pub struct Compiler;

impl Compiler {
  pub fn new() -> Self {
    Self {}
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
    let tycheck = TyCheck::new(hir);

    Ok(JsGenerator::new(&tycheck).generate())
  }

  pub fn compile_file(
    &self,
    path: std::path::PathBuf,
    target: TargetLang
  ) -> Result<(), String> {
    let output_path = path.with_extension("js");
    let source = match std::fs::read_to_string(path) {
      Ok(source) => source,
      Err(e) => return Err(format!("Failed to read file: {}", e)),
    };
    let code = match match target {
      TargetLang::Javascript => self.compile_js(&source),
    } {
      Ok(code) => code,
      Err(err) => return Err(err),
    };

    // write to file with .ds file extension
    match std::fs::write(output_path, code) {
      Ok(()) => Ok(()),
      Err(err) => Err(format!("Failed to write to file: {}", err)),
    }
  }
}
