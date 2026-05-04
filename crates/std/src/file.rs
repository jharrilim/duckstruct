//! Standard library `file` module (use file::{ read, write }).

use crate::registry::ModuleDescriptor;

/// Embedded source for the file module. Stub bodies so that typecheck succeeds;
/// LLVM codegen will emit declarations and the actual symbols are provided at link time.
const SOURCE: &str = r#"
pub f read(path) = 0;
pub f write(path, content) = 0;
"#;

pub const DESCRIPTOR: ModuleDescriptor = ModuleDescriptor {
  name: "file",
  source: SOURCE,
  globals: &[],
  externals: &[],
};
