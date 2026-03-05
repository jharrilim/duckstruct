pub mod js;
#[cfg(feature = "llvm")]
pub mod llvm;
#[cfg(feature = "llvm")]
pub mod stdlib;

pub trait CodeGenerator {
  fn generate(&self) -> String;
}
