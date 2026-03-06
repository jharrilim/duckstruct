//! `init` command: create a new Duckstruct project.

use std::path::{Path, PathBuf};

const MANIFEST_FILENAME: &str = "duckstruct.toml";

/// Run the init command. Creates manifest, src/, .gitignore, and src/main.ds in the given directory.
pub fn run(path: Option<PathBuf>) {
  let dir = path.unwrap_or_else(|| PathBuf::from("."));
  if let Err(e) = run_init(&dir) {
    eprintln!("init failed: {}", e);
    std::process::exit(1);
  }
}

fn run_init(dir: &Path) -> std::result::Result<(), String> {
  std::fs::create_dir_all(dir)
    .map_err(|e| format!("create directory {}: {}", dir.display(), e))?;

  let manifest_path = dir.join(MANIFEST_FILENAME);
  let manifest_content = r#"entrypoint = "src/main.ds"
"#;
  std::fs::write(&manifest_path, manifest_content)
    .map_err(|e| format!("write {}: {}", manifest_path.display(), e))?;

  let src_dir = dir.join("src");
  std::fs::create_dir_all(&src_dir).map_err(|e| format!("create src directory: {}", e))?;

  let gitignore_path = dir.join(".gitignore");
  std::fs::write(&gitignore_path, "output/\n")
    .map_err(|e| format!("write .gitignore: {}", e))?;

  let main_ds = src_dir.join("main.ds");
  std::fs::write(&main_ds, "")
    .map_err(|e| format!("write {}: {}", main_ds.display(), e))?;

  println!("Initialized Duckstruct project in {}", dir.display());
  Ok(())
}
