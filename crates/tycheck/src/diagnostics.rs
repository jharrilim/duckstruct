use std::fmt::Display;


#[derive(Debug, Default)]
pub struct Diagnostics {
  pub errors: Vec<Error>,
}

impl Diagnostics {
  pub fn print_errors(&self) {
    for error in &self.errors {
      println!("{}", error);
    }
  }
}

#[derive(Debug, Default)]
pub struct Error {
  message: &'static str,
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.message)
  }
}
