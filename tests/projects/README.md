# Example projects

- **llvm_executable** – Single-module project that compiles to LLVM IR, object file, and a linked executable. Requires `--features llvm` and LLVM installed.

  ```bash
  cargo run --features llvm -- compile tests/projects/llvm_executable
  ```

  Output appears under `tests/projects/llvm_executable/output/llvm/` (`.ll`, `.o`, and the executable when `link = true` in `duckstruct.toml`).
