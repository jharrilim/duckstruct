//! Manifest file (duckstruct.toml) parsing and project root discovery.

use std::path::{Path, PathBuf};

pub const MANIFEST_FILENAME: &str = "duckstruct.toml";

/// Manifest schema. First property is `entrypoint`.
#[derive(Debug, serde::Deserialize)]
pub struct Manifest {
  /// Entry point path relative to the directory containing the manifest.
  pub entrypoint: String,
}

/// Walk up from the given path (file or directory) until we find `duckstruct.toml`.
/// Returns the directory that contains the manifest, or `None` if not found.
pub fn find_manifest_dir(from: &Path) -> Option<PathBuf> {
  let mut current = if from.is_file() {
    from.parent().map(PathBuf::from)?
  } else {
    from.to_path_buf()
  };

  loop {
    let manifest_path = current.join(MANIFEST_FILENAME);
    if manifest_path.is_file() {
      return Some(current);
    }
    current = current.parent()?.to_path_buf();
  }
}

/// Load and parse the manifest from the given directory (which must contain duckstruct.toml).
pub fn load_manifest(manifest_dir: &Path) -> Result<Manifest, String> {
  let path = manifest_dir.join(MANIFEST_FILENAME);
  let contents = std::fs::read_to_string(&path)
    .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
  toml::from_str(&contents).map_err(|e| format!("Invalid manifest: {}", e))
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  #[test]
  fn test_parse_manifest() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    fs::write(
      root.join(MANIFEST_FILENAME),
      r#"entrypoint = "src/main.ds"
"#,
    )
    .expect("write manifest");
    let m = load_manifest(root).expect("parse");
    assert_eq!(m.entrypoint, "src/main.ds");
  }

  #[test]
  fn test_find_manifest_dir_from_file() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    fs::write(root.join(MANIFEST_FILENAME), "entrypoint = \"main.ds\"\n")
      .expect("write manifest");
    let sub = root.join("src");
    fs::create_dir_all(&sub).expect("create src");
    let main_ds = sub.join("main.ds");
    fs::write(&main_ds, "").expect("write main.ds");
    let found = find_manifest_dir(&main_ds);
    assert_eq!(found, Some(root.to_path_buf()));
  }

  #[test]
  fn test_find_manifest_dir_from_directory() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    fs::write(root.join(MANIFEST_FILENAME), "entrypoint = \"main.ds\"\n")
      .expect("write manifest");
    let found = find_manifest_dir(root);
    assert_eq!(found, Some(root.to_path_buf()));
  }

  #[test]
  fn test_find_manifest_dir_not_found() {
    let dir = tempfile::tempdir().expect("temp dir");
    let sub = dir.path().join("a").join("b");
    fs::create_dir_all(&sub).expect("create dirs");
    let found = find_manifest_dir(&sub);
    assert!(found.is_none());
  }
}
