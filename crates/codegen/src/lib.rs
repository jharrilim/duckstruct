pub mod js;

pub trait CodeGenerator {
    fn generate(&self) -> String;
}
