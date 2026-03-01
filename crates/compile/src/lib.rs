mod modules;

use ast::Root;
use codegen::{js::JsGenerator, CodeGenerator};
use hir::lower;
use parser::parse;
use tycheck::TyCheck;

pub enum TargetLang {
  Javascript,
}

pub struct Compiler;

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

  pub fn compile_file(&self, path: std::path::PathBuf, target: TargetLang) -> Result<(), String> {
    let output_path = path.with_extension("js");
    println!("{} => {}", path.display(), output_path.display());
    let source = match std::fs::read_to_string(&path) {
      Ok(source) => source,
      Err(e) => return Err(format!("Failed to read file: {}", e)),
    };
    let parse = parse(&source);
    if !parse.errors.is_empty() {
      return Err(format!("Parse errors: {:?}", parse.errors));
    }
    let ast = Root::cast(parse.syntax())
      .ok_or_else(|| "Failed to generate AST from source".to_string())?;

    let code = if modules::collect_use_deps(&ast).is_empty() {
      match target {
        TargetLang::Javascript => self.compile_js(&source)?,
      }
    } else {
      let (entry_hir, deps) = modules::load_module_tree(&path)?;
      let entry_uses = entry_hir.uses.clone();
      let mut module_map: std::collections::HashMap<String, &TyCheck> =
        std::collections::HashMap::new();
      for dep in &deps {
        module_map.insert(dep.name.clone(), &dep.tycheck);
      }
      let mut entry_tycheck = TyCheck::new(entry_hir);
      entry_tycheck.infer_with_modules(Some(&module_map));

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
        let prefix = format!("__{}__", dep.name);
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
          let mod_name = path[0].clone();
          let export = path.last().unwrap().clone();
          (bind, (mod_name, export))
        })
        .collect();

      let entry_js = JsGenerator::new(&entry_tycheck)
        .with_import_map(import_map)
        .generate_js();
      bundle.push_str(&entry_js);

      match target {
        TargetLang::Javascript => bundle,
      }
    };

    match std::fs::write(output_path, code) {
      Ok(()) => Ok(()),
      Err(err) => Err(format!("Failed to write to file: {}", err)),
    }
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
