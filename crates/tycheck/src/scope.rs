use data_structures::FxIndexMap;

use crate::typed_db::TypedDatabaseIdx;

/// A frame is created whenever a new scope is entered. This happens for things
/// like blocks, functions, and loops.
#[derive(Debug, Default, Clone)]
pub struct Frame {
  /// Optionally defined for functions to allow for recursion detection.
  name: Option<String>,
  #[allow(unused)]
  debug_name: String,
  id: usize,
  #[allow(unused)]
  imports: FxIndexMap<String, TypedDatabaseIdx>,
  defs: FxIndexMap<String, TypedDatabaseIdx>,
  args: FxIndexMap<String, TypedDatabaseIdx>,
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

  /// Creates a new frame with a name. This is used for named functions.
  pub fn push_named_frame(&mut self, name: String) {
    self.frames.push(self.new_named_frame(name));
  }

  pub fn new_named_frame(&self, name: String) -> Frame {
    Frame {
      name: Some(name),
      debug_name: format!(
        "{}#{}",
        self.current_frame().debug_name,
        self.frames.len() - 1
      ),
      id: self.frames.len() - 1,
      ..Default::default()
    }
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

  pub fn is_late_binding(&self, name: &str) -> bool {
    self.frames.iter().any(|frame| {
      frame
        .name
        .as_ref()
        .map(|frame_name| frame_name == name)
        .unwrap_or(false)
    })
  }

  /// Looks up a definition to see if it's in scope.
  pub fn def(&self, name: &str) -> Option<TypedDatabaseIdx> {
    let d = self
      .frames
      .iter()
      .rev()
      .find_map(|frame| frame.defs.get(name).or_else(|| frame.args.get(name)));
    d.copied()
  }

  /// Return a new function scope with all of the frames flattened into one.
  /// Any args defined in the non-topmost frame will become defs in the new
  /// scope's topmost frame. This is useful for closure capture.
  pub fn flatten(&self) -> Scope {
    let mut scope = Scope::default();
    let mut args = FxIndexMap::default();
    let mut defs = FxIndexMap::default();
    for frame in self.frames.iter() {
      args.extend(frame.args.clone());
      defs.extend(frame.defs.clone());
    }
    let mut frame = scope.current_frame_mut();
    frame.defs = defs;
    frame.args = args;
    scope
  }

  /// Tries to find a function parameter in the current scope based on its name.
  pub fn param(&self, name: &str) -> Option<TypedDatabaseIdx> {
    self.frames.iter().rev().find_map(|frame| frame.args.get(name)).copied()
  }

  pub fn is_param(&self, idx: TypedDatabaseIdx) -> bool {
    println!("current params: {:#?}", self.current_frame().args);
    println!("given: {:#?}", idx);
    self
      .current_frame()
      .args
      .iter()
      .any(|(_, &v)| v == idx)
  }

  pub fn def_name_similar_to(&self, name: &str) -> Option<String> {
    self.frames.iter().rev().find_map(|frame| {
      frame
        .defs
        .iter()
        .chain(frame.args.iter())
        .find(|(def_name, _)| distance::levenshtein(name, def_name) < 3)
        .map(|(s, _)| s.to_string())
    })
  }
}

#[cfg(test)]
mod tests {
  use data_structures::index_map;

  use crate::{typed_db::TypedDatabase, typed_hir::TypedExpr};

  use super::*;

  fn mock_ty_db() -> TypedDatabase {
    TypedDatabase::default()
  }

  impl TypedDatabase {
    fn mock_ty(&mut self) -> TypedDatabaseIdx {
      self.alloc(TypedExpr::Unresolved)
    }
  }

  #[test]
  pub fn test_scope() {
    Scope::default();
  }

  #[test]
  pub fn test_scope_with_definition() {
    let mut ty_db = mock_ty_db();
    let mut scope = Scope::default();
    let mock_ty = ty_db.mock_ty();

    scope.define("x".to_string(), mock_ty);

    assert_eq!(scope.def("x"), Some(mock_ty));
  }

  #[test]
  pub fn test_scope_with_arg() {
    let mut ty_db = mock_ty_db();
    let mut scope = Scope::default();
    let mock_ty = ty_db.mock_ty();

    scope.define_args(&index_map!("x".to_string() => mock_ty));

    assert_eq!(scope.def("x"), Some(mock_ty));
  }

  #[test]
  pub fn test_scope_with_definition_shadowing() {
    let mut ty_db = mock_ty_db();
    let mut scope = Scope::default();
    let mock_ty = ty_db.mock_ty();
    let mock_ty2 = ty_db.mock_ty();

    scope.define("x".to_string(), mock_ty);
    scope.push_frame();
    scope.define("x".to_string(), mock_ty2);

    assert_eq!(scope.def("x"), Some(mock_ty2));
  }

  #[test]
  pub fn test_scope_def_shadowing_arg_in_same_frame() {
    let mut ty_db = mock_ty_db();
    let mut scope = Scope::default();
    let mock_ty = ty_db.mock_ty();
    let mock_ty2 = ty_db.mock_ty();

    scope.define("x".to_string(), mock_ty);
    scope.define_args(&index_map!("x".to_string() => mock_ty2));

    assert_eq!(scope.def("x"), Some(mock_ty));
  }

  #[test]
  pub fn test_scope_flattened_uses_shadowed_definition() {
    let mut ty_db = mock_ty_db();
    let mut scope = Scope::default();
    let mock_ty = ty_db.mock_ty();
    let mock_ty2 = ty_db.mock_ty();

    scope.define("x".to_string(), mock_ty);
    scope.push_frame();
    scope.define("x".to_string(), mock_ty2);

    let flattened = scope.flatten();

    assert_eq!(flattened.def("x"), Some(mock_ty2));
  }
}
