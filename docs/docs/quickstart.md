---
sidebar_position: 2
---

# Quickstart

This guide walks through creating and running a small Duckstruct program.

## 1) Create a project

Use the CLI to scaffold a new project:

```bash
ds init hello-duckstruct
cd hello-duckstruct
```

## 2) Write a small program

Create a simple function and call it:

```duckstruct
f slope(coords) {
  coords.x / coords.y
}

let result = slope({ x: 10, y: 2 })
```

Duckstruct infers that `coords` must support `x` and `y`.

## 3) Compile (includes type checking)

From the project directory:

```bash
ds compile .
```

If compilation succeeds, Duckstruct also typechecked your program. If it fails,
you should see human-readable diagnostics in the terminal.

By default, JavaScript is emitted to `output/js/index.js` (relative to the
directory that contains `duckstruct.toml`).

## 4) Next steps

- Learn the core syntax in [Syntax Basics](./syntax-basics)
- Learn structural typing patterns in [Functions and Traits](./functions-and-traits)
