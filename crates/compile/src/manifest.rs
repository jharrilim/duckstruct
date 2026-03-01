//! Manifest file (duckstruct.toml) parsing and project root discovery.

use std::path::{Path, PathBuf};

pub const MANIFEST_FILENAME: &str = "duckstruct.toml";

/// Default output directory relative to project root when `output` is not set.
pub const DEFAULT_OUTPUT_DIR: &str = "output";

/// Target backend: `js` or `llvm`. Used when compiling with a project root.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Backend {
  #[default]
  Js,
  Llvm,
}

impl Backend {
  pub fn as_str(self) -> &'static str {
    match self {
      Backend::Js => "js",
      Backend::Llvm => "llvm",
    }
  }
}

impl std::str::FromStr for Backend {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s.to_ascii_lowercase().as_str() {
      "js" => Ok(Backend::Js),
      "llvm" => Ok(Backend::Llvm),
      _ => Err(format!("Unknown backend '{}', expected 'js' or 'llvm'", s)),
    }
  }
}

#[derive(Debug, serde::Deserialize)]
struct TargetSection {
  #[serde(default)]
  backend: Option<String>,
}

/// Manifest schema. First property is `entrypoint`. Optional `output` configures the output directory.
/// Optional `[target]` section with `backend = "js"` or `backend = "llvm"` (default: "js").
#[derive(Debug, serde::Deserialize)]
pub struct Manifest {
  /// Entry point path relative to the directory containing the manifest.
  pub entrypoint: String,
  /// Output directory relative to project root (e.g. "output" → emit JS to output/js/index.js). Default: "output".
  #[serde(default)]
  pub output: Option<String>,
  /// Optional [target] section. When present, backend selects codegen (default: "js").
  #[serde(default)]
  target: Option<TargetSection>,
}

impl Manifest {
  /// Resolved backend from [target] backend field. Defaults to Js when missing or invalid.
  pub fn backend(&self) -> Backend {
    let s = self
      .target
      .as_ref()
      .and_then(|t| t.backend.as_deref())
      .unwrap_or("js");
    s.parse().unwrap_or(Backend::Js)
  }
}

impl From<&str> for Manifest {
  fn from(s: &str) -> Self {
    toml::from_str(&s).unwrap()
  }
}

impl From<&Path> for Manifest {
  fn from(p: &Path) -> Self {
    let path = p.join(MANIFEST_FILENAME);
    let contents = std::fs::read_to_string(&path).unwrap();
    Self::from(contents.as_str())
  }
}

/// Load and parse the manifest from the given directory (which must contain duckstruct.toml).
/// Returns an error on I/O or parse failure.
pub fn load_manifest(manifest_dir: &Path) -> Result<Manifest, String> {
  let path = manifest_dir.join(MANIFEST_FILENAME);
  let contents = std::fs::read_to_string(&path)
    .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
  toml::from_str(&contents).map_err(|e| format!("Invalid manifest: {}", e))
}

/// Maximum number of parent directories to traverse when searching for the manifest.
/// Prevents infinite loops from symlink cycles.
const MAX_MANIFEST_SEARCH_DEPTH: u32 = 100;

/// Walk up from the given path (file or directory) until we find `duckstruct.toml`.
/// Returns the directory that contains the manifest, or an error if not found or if
/// the search exceeds `MAX_MANIFEST_SEARCH_DEPTH` (e.g. symlink loop).
pub fn find_manifest_dir(from: &Path) -> Result<PathBuf, String> {
  let mut current = if from.is_file() {
    from
      .parent()
      .map(PathBuf::from)
      .ok_or_else(|| "Path has no parent".to_string())?
  } else {
    from.to_path_buf()
  };

  for _ in 0..MAX_MANIFEST_SEARCH_DEPTH {
    let manifest_path = current.join(MANIFEST_FILENAME);
    if manifest_path.is_file() {
      return Ok(current);
    }
    current = match current.parent() {
      Some(p) => p.to_path_buf(),
      None => {
        return Err(format!(
          "No {} found in {} or any parent directory",
          MANIFEST_FILENAME,
          from.display()
        ));
      }
    };
  }

  Err(format!(
    "Manifest search exceeded maximum depth of {} (possible link cycle)",
    MAX_MANIFEST_SEARCH_DEPTH
  ))
}



#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  #[test]
  fn test_parse_manifest() {
    let m = Manifest::from(
      r#"entrypoint = "src/main.ds"
"#,
    );
    assert_eq!(m.entrypoint, "src/main.ds");
    assert_eq!(m.output, None);
  }

  #[test]
  fn test_parse_manifest_with_output() {
    let m = Manifest::from(
      r#"entrypoint = "src/main.ds"
output = "dist"
"#,
    );
    assert_eq!(m.entrypoint, "src/main.ds");
    assert_eq!(m.output.as_deref(), Some("dist"));
  }

  #[test]
  fn test_parse_manifest_default_backend_is_js() {
    let m = Manifest::from(
      r#"entrypoint = "src/main.ds"
"#,
    );
    assert_eq!(m.backend(), Backend::Js);
  }

  #[test]
  fn test_parse_manifest_target_backend_llvm() {
    let m = Manifest::from(
      r#"entrypoint = "src/main.ds"

[target]
backend = "llvm"
"#,
    );
    assert_eq!(m.entrypoint, "src/main.ds");
    assert_eq!(m.backend(), Backend::Llvm);
  }

  #[test]
  fn test_parse_manifest_target_backend_js_explicit() {
    let m = Manifest::from(
      r#"entrypoint = "src/main.ds"

[target]
backend = "js"
"#,
    );
    assert_eq!(m.backend(), Backend::Js);
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
    let found = find_manifest_dir(&main_ds).expect("should find manifest");
    assert_eq!(found, root.to_path_buf());
  }

  #[test]
  fn test_find_manifest_dir_from_directory() {
    let dir = tempfile::tempdir().expect("temp dir");
    let root = dir.path();
    fs::write(root.join(MANIFEST_FILENAME), "entrypoint = \"main.ds\"\n")
      .expect("write manifest");
    let found = find_manifest_dir(root).expect("should find manifest");
    assert_eq!(found, root.to_path_buf());
  }

  #[test]
  fn test_find_manifest_dir_not_found() {
    let dir = tempfile::tempdir().expect("temp dir");
    let sub = dir.path().join("a").join("b");
    fs::create_dir_all(&sub).expect("create dirs");
    let found = find_manifest_dir(&sub);
    assert!(found.is_err());
  }
}
