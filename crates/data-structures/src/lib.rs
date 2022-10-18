use indexmap::IndexMap;

use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

pub mod arena {
  pub use la_arena::*;
}

pub type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;

#[macro_export]
macro_rules! index_map {
  ($($key:expr => $value:expr),* $(,)?) => {{
    let mut map = FxIndexMap::default();
    $(
      map.insert($key, $value);
    )*
    map
  }};
}
