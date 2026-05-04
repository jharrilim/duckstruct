---
sidebar_position: 7
---

# CLI Reference

The Duckstruct compiler is shipped as the `ds` executable (built from this
repository).

## Common commands

### `ds init [path]`

Creates a new project in the given directory (or the current directory):

- `duckstruct.toml`
- `src/main.ds`
- `.gitignore`

### `ds compile <path>`

Compiles a `.ds` file or a project directory.

- If `<path>` is inside a project (a directory containing `duckstruct.toml`),
  Duckstruct uses the manifest to choose the entrypoint and output location.
- Compilation includes type checking; failures print human-readable
  diagnostics.

Default project output locations:

- JavaScript: `output/js/index.js` (unless `output` is set in `duckstruct.toml`)
- LLVM: `output/llvm/index.ll` (when configured)

Aliases: `ds c`, `ds build`

### `ds check`

This command is intended for editor integrations: it prints **LSP-style JSON**
diagnostics and requires `--json`.

For day-to-day use, prefer `ds compile` to see human-readable errors.

### `ds ide ...`

IDE support commands used by the VS Code extension / language server.

### `ds` (no subcommand)

Running `ds` with no subcommand starts the REPL.

### `ds --eval "<source>"`

Typecheck and evaluate a small snippet, printing an inferred type string.

## `duckstruct.toml`

Common keys:

```toml
entrypoint = "src/main.ds"

# Optional: where build artifacts go (default: "output")
output = "dist"

[target]
# "js" (default) or "llvm"
backend = "js"

# LLVM-only: link an executable (optional)
link = true

# Used as the executable name when linking (optional)
name = "my-tool"
```
