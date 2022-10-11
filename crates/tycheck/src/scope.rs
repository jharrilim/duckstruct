use rustc_hash::FxHashMap;

use crate::typed_db::TypedDatabaseIdx;

/// A frame is created whenever a new scope is entered. This happens for things
/// like blocks, functions, and loops.
#[derive(Debug, Default)]
pub struct Frame {
  #[allow(unused)]
  debug_name: String,
  #[allow(unused)]
  imports: FxHashMap<String, TypedDatabaseIdx>,
  defs: FxHashMap<String, TypedDatabaseIdx>,
}

impl Frame {
  pub fn global() -> Frame {
    Frame {
      debug_name: "global".to_string(),
      ..Default::default()
    }
  }
}

/// A scope is a stack of frames. The top frame is the current scope. We can use
/// this to resolve variables and functions in the current scope, and in the
/// correct order.
pub struct Scope {
  frames: Vec<Frame>,
}

impl Default for Scope {
  fn default() -> Self {
    Self { frames: vec![Frame::global()] }
  }
}

impl Scope {
  pub fn current_frame(&self) -> &Frame {
    self.frames.last().unwrap()
  }

  pub fn push_frame(&mut self) {
    self.frames.push(Frame::default());
  }

  pub fn pop_frame(&mut self) -> Option<Frame> {
    self.frames.pop()
  }

  pub fn def(&self, name: &str) -> Option<&TypedDatabaseIdx> {
    self
      .frames
      .iter()
      .rev()
      .find_map(|frame| frame.defs.get(name))
  }
}
