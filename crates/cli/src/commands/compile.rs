//! `compile` command: compile a Duckstruct file or project to the target language.

use std::path::PathBuf;

use compile::{Compiler, TargetLang};

/// Run the compile command. Compiles the file or project at the given path.
pub fn run(path: PathBuf) {
  match compile::resolve_entry_and_project_root(&path) {
    Ok((entry_path, project_root)) => {
      let target = match &project_root {
        Some(root) => compile::target_from_manifest_dir(root).unwrap_or(TargetLang::Javascript),
        None => TargetLang::Javascript,
      };
      if let Err(err) = Compiler::new().compile_file(entry_path, project_root, target) {
        println!("Compilation failed: {}", err);
      }
    }
    Err(err) => println!("Compilation failed: {}", err),
  }
}
