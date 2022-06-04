use syntax::SyntaxKind;

pub fn analyze() {
  // fiddle
  let input = r#"
    let x = 10;
    f y(x) = x * 2;
  "#;
  let p = parser::parse(input);
  p.syntax().children().for_each(|c| {
    match c.kind() {
      SyntaxKind::LetExpression => {
        println!("letletlet!");
      }
      SyntaxKind::NamedFunctionExpression => {
        println!("namednamednamed!");
      }
      _ => {
        println!("noop");
      },
    }
  });
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn analyze_test() {
    analyze();
  }
}