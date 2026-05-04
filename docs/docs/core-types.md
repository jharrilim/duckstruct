---
sidebar_position: 4
---

# Core Types and Collections

Duckstruct is dynamically typed at the surface, but the compiler tracks precise
information about values and shapes as it typechecks your program.

## Primitive-ish values

- Numbers
- Strings
- Booleans

## Objects

Objects are written with `{ key: value, ... }` syntax. Field access uses `.`.

```text
let point = { x: 3, y: 4 }
point.x
```

## Arrays

Array literals use `[ ... ]`.

```text
let xs = [1, 2, 3]
```

## Functions

Functions are first-class values. See [Functions and Traits](./functions-and-traits).

## Literal types

Duckstruct tracks many literals as *specific* values during typechecking (not
just "string" vs "number"). This enables stronger checking and more precise
inference in some cases.

## Notes

This section is intentionally high-level. As the language evolves, prefer the
compiler's diagnostics as the source of truth for what is accepted.
