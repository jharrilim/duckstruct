use rustc_hash::FxHashMap;

use crate::typed_db::{TypedDatabaseIdx, FxIndexMap};

/// A frame is created whenever a new scope is entered. This happens for things
/// like blocks, functions, and loops.
#[derive(Debug, Default, Clone)]
pub struct Frame {
  #[allow(unused)]
  debug_name: String,
  id: usize,
  #[allow(unused)]
  imports: FxHashMap<String, TypedDatabaseIdx>,
  defs: FxHashMap<String, TypedDatabaseIdx>,
  args: FxHashMap<String, TypedDatabaseIdx>,
  /// used for identifying anonymous functions
  anon_counter: usize,
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
#[derive(Debug, Clone)]
pub struct Scope {
  frames: Vec<Frame>,
}

impl Default for Scope {
  fn default() -> Self {
    Self {
      frames: vec![Frame::global()],
    }
  }
}

impl Scope {
  pub fn current_frame(&self) -> &Frame {
    self.frames.last().unwrap()
  }
  pub fn current_frame_mut(&mut self) -> &mut Frame {
    self.frames.last_mut().unwrap()
  }

  pub fn push_frame(&mut self) {
    self.frames.push(self.new_frame());
  }

  fn new_frame(&self) -> Frame {
    Frame {
      debug_name: format!("frame {}", self.frames.len() - 1),
      id: self.frames.len() - 1,
      ..Frame::default()
    }
  }

  pub fn pop_frame(&mut self) -> Option<Frame> {
    self.frames.pop()
  }

  pub fn extend_frames(&self, other: &Scope) -> Scope {
    let mut scope = self.clone();
    for frame in other.frames.iter().rev() {
      let new_frame = scope.new_frame();
      scope.frames.push(Frame {
        debug_name: new_frame.debug_name,
        id: new_frame.id,
        ..frame.clone()
      });
    }
    scope.frames.extend(other.frames.iter().cloned());
    scope
  }

  pub fn anonymous_function_name(&mut self) -> String {
    let frame = self.current_frame();
    // Starting this with a special symbol prevents collisions with user-defined
    // functions since they can't use those symbols.
    let name = format!("<>__anon__<{},{}>", frame.id, frame.anon_counter);
    self.current_frame_mut().anon_counter += 1;
    name
  }

  pub fn define(&mut self, name: String, idx: TypedDatabaseIdx) {
    self.current_frame_mut().defs.insert(name, idx);
  }

  /// Defines variables for the current frame in scope.
  pub fn define_all(&mut self, defs: &FxIndexMap<String, TypedDatabaseIdx>) {
    self.current_frame_mut().defs.extend(defs.clone());
  }

  pub fn define_args(&mut self, args: &FxIndexMap<String, TypedDatabaseIdx>) {
    self.current_frame_mut().args.extend(args.clone());
  }

  /// Looks up a definition to see if it's in scope.
  pub fn def(&self, name: &str) -> Option<TypedDatabaseIdx> {
    let d = self
      .frames
      .iter()
      .rev()
      .find_map(|frame| frame.args.get(name).or_else(|| frame.defs.get(name)));
    d.copied()
  }
}
