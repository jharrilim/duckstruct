//! Duckstruct standard library. Provides globals (top-level, no import) and
//! the `file` module (use file::{ ... }). Architecture supports both JS and
//! LLVM codegen; only the LLVM std library is fleshed out first.

mod file;
mod globals;

pub use file::file_module_tycheck;
pub use globals::{external_functions_for_backend, globals_for_backend, Backend};

/// Result of loading a stdlib module: name and type-checked state.
/// The compile crate converts this into its LoadedModule when needed.
#[derive(Debug)]
pub struct StdlibModule {
  pub name: String,
  pub tycheck: tycheck::TyCheck,
}

/// Returns true if the given module name is a standard library module
/// (e.g. `file`) that should be resolved from this crate, not the filesystem.
pub fn is_stdlib_module(name: &str) -> bool {
  matches!(name, "file")
}

/// Load a standard library module by name. Returns the module's name and
/// type-checked TyCheck. Fails if the name is not a known stdlib module or
/// if the module fails to build (e.g. parse/typecheck error).
pub fn load_stdlib_module(name: &str) -> Result<StdlibModule, String> {
  match name {
    "file" => {
      let tycheck = file::file_module_tycheck()?;
      Ok(StdlibModule {
        name: "file".to_string(),
        tycheck,
      })
    }
    _ => Err(format!("unknown stdlib module: {}", name)),
  }
}
