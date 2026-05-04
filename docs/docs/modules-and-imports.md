---
sidebar_position: 6
---

# Modules and Imports

Duckstruct programs can be split across multiple `.ds` files.

## Projects and entrypoints

A typical project contains:

- `duckstruct.toml` (manifest)
- `src/main.ds` (entrypoint by convention)

`ds init` creates this layout for you.

## Import syntax

Imports use `use`:

```text
use helper::{ONE, double};
```

Imported symbols must be exported from the module with `pub`:

```text
pub let ONE = 1;
pub f double(x) { x + x }
```

## `root::` imports (project modules)

When compiling a project (a directory containing `duckstruct.toml`), you can
import modules relative to the project root using `root::`:

```text
use root::lib::util::{VAL};
```

This corresponds to a file like `lib/util.ds` at the project root.

`root::` requires a manifest; compiling a single file without a project cannot
resolve `root::` imports.

## Builtin modules

Some module names are reserved for the standard library. For example, `file` is
a builtin module, not a local `file.ds` on disk:

```text
use file::{read};
```

## Tips

- Prefer `root::...` for anything that is not adjacent to the entry file.
- Keep module cycles in mind: circular imports are rejected.
