---
sidebar_position: 5
---

# Functions and Traits

## Functions

Functions are introduced with `f`:

```text
f add(a, b) {
  a + b
}
```

The last expression in a block is typically the result.

## Duck constraints (structural requirements)

When a function uses fields or methods on a parameter, Duckstruct infers what
that parameter must support.

```text
f slope(coords) {
  coords.x / coords.y
}
```

Any value passed to `slope` must be able to provide `x` and `y` in a compatible
way. This is structural typing: it is about capabilities, not declared class
names.

## Traits

Traits describe a set of operations that a type can implement.

```text
pub trait Renderable {
  f render(x);
  f name();
}
```

## Implementations

Use `impl TraitName for TypeName { ... }` to provide trait methods:

```text
impl Renderable for Foo {
  f render(x) { x }
  f name() { "foo" }
}
```
