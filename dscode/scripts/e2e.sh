#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DUCKSTRUCT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
# Use the same target directory as Cargo (respects CARGO_TARGET_DIR). A stale
# repo-root target/debug/ds is easy to run by mistake and can mis-map diagnostic lines.
DS=""
if [[ -n "${DUCKSTRUCT_E2E_DS_PATH:-}" ]]; then
	DS="$DUCKSTRUCT_E2E_DS_PATH"
else
	METADATA_DS="$(cd "$DUCKSTRUCT_ROOT" && cargo metadata --format-version 1 --no-deps 2>/dev/null | python3 -c "import json,sys,os; m=json.load(sys.stdin); print(os.path.join(m['target_directory'], 'debug', 'ds'))" 2>/dev/null || true)"
	if [[ -n "$METADATA_DS" && -x "$METADATA_DS" ]]; then
		DS="$METADATA_DS"
	elif [[ -x "$DUCKSTRUCT_ROOT/target/debug/ds" ]]; then
		DS="$DUCKSTRUCT_ROOT/target/debug/ds"
	fi
fi
if [[ -n "$DS" && -x "$DS" ]]; then
	export DUCKSTRUCT_E2E_DS_PATH="$DS"
else
	echo "warning: no usable ds binary (build with: cargo build --bin ds); diagnostic e2e may be skipped" >&2
fi

cd "$SCRIPT_DIR/.."
export CODE_TESTS_PATH="$(pwd)/client/out/test"
export CODE_TESTS_WORKSPACE="$(pwd)/client/testFixture"

node "$(pwd)/client/out/test/runTest"