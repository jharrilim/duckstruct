//! cargo run -p parser --example dump_parse_errors -- path/to/file.ds
fn main() {
  let path = std::env::args().nth(1).expect("path to .ds file");
  let s = std::fs::read_to_string(&path).expect("read");
  let p = parser::parse(&s);
  for e in &p.errors {
    let start: usize = e.range.start().into();
    let end: usize = e.range.end().into();
    println!("bytes {start}..{end} {:?}", e.message_body());
  }
}
