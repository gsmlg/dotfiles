#!/usr/bin/env bash
set -e

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
: "${EMACS:=emacs}"

test_args=()
while IFS= read -r test_file; do
  test_args+=(-l "$test_file")
done < <(find "$script_dir/test" -type f -name '*-test.el' -print | LC_ALL=C sort)

"$EMACS" -Q --batch -L "$script_dir" -L "$script_dir/test" \
  -l ert "${test_args[@]}" -f ert-run-tests-batch-and-exit
