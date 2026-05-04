// Duckstruct primitive list runtime. Loaded only when a list primitive method is
// used in a way that defers to the runtime (e.g. non-constant `.push`).
// All assignments are idempotent so the same file may be embedded multiple times.

var Duckstruct = (typeof Duckstruct === "object" && Duckstruct) || {};
Duckstruct.Lib = Duckstruct.Lib || {};
Duckstruct.Lib.Primitive = Duckstruct.Lib.Primitive || {};
Duckstruct.Lib.Primitive.List = Duckstruct.Lib.Primitive.List || {
  // Returns a new array with `x` appended. Mirrors duckstruct's `.push` semantics
  // (returns the array, not the new length).
  push: function (arr, x) {
    return arr.concat([x]);
  },
};
