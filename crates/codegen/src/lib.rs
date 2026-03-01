pub mod js;
#[cfg(feature = "llvm")]
pub mod llvm;

pub trait CodeGenerator {
  fn generate(&self) -> String;
}
