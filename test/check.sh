#!/bin/bash
# The one gate: the test suite, the compile-error cases, and the doc
# snippets. Exits 0 only if all three pass.
set -u
cd "$(dirname "$0")/.."

# A suite that hangs or dies without an error can still exit 0, so the
# last line is the only proof that every test ran.
output="$(iris test 2>&1)"
echo "$output" | tail -3
if ! grep -qxF "All tests passed." <<<"$output"; then
  echo "FAIL iris test did not reach \"All tests passed.\""
  exit 1
fi

bash test/compile-errors.sh || exit 1
python3 test/check-snippets.py || exit 1
