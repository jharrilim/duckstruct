#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DUCKSTRUCT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
DS="$DUCKSTRUCT_ROOT/target/debug/ds"
if [[ -x "$DS" ]]; then
	export DUCKSTRUCT_E2E_DS_PATH="$DS"
else
	echo "warning: $DS is missing or not executable; diagnostic e2e may be skipped (build with: cargo build --bin ds)" >&2
fi

cd "$SCRIPT_DIR/.."
export CODE_TESTS_PATH="$(pwd)/client/out/test"
export CODE_TESTS_WORKSPACE="$(pwd)/client/testFixture"

node "$(pwd)/client/out/test/runTest"