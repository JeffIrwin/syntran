#!/usr/bin/env bash
# corpus-diff.sh — differential oracle for the AST-walker removal (chore/rm-ast).
#
# Runs every .syntran file under samples/, src/tests/test-src/, and
# src/tests/long/aoc/ through two binaries (a pinned reference build and the
# current working build) and diffs stdout + exit status.  This replaces the
# in-tree AST backend as a correctness oracle once it is deleted, since it
# pins against *shipped* behavior at the point the worktree was branched
# rather than a second in-tree implementation.
#
# Usage:
#   bash utils/corpus-diff.sh [ref_binary] [cur_binary]
#
# Defaults:
#   ref_binary = build/ref-bin/syntran-ref   (see: git worktree add ../syntran-ref)
#   cur_binary = build/v1/bin/syntran.exe    (see: fpm install --profile release --prefix ./build/v1/)
#
# Re-baseline the reference binary only at a release tag -- do not regenerate
# it mid-port, or it stops being an oracle.

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

REF="${1:-build/ref-bin/syntran-ref}"
CUR="${2:-build/v1/bin/syntran}"

if [ ! -x "$REF" ]; then
	echo "error: reference binary not found or not executable: $REF" >&2
	exit 1
fi
if [ ! -x "$CUR" ]; then
	echo "error: current binary not found or not executable: $CUR" >&2
	exit 1
fi

# Directories to scan for .syntran files.  Error repro files under
# src/tests/test-src/errors/ are intentionally EXCLUDED: many are designed to
# be invalid syntran that halts with a diagnostic, and comparing raw exit
# codes there is noisy (E vs R codes, line/col text) without adding real
# coverage beyond what unit_test_error_codes already checks. Everything else
# -- ordinary scripts, module tests, array/struct/enum tests, REPL corpora,
# and the AoC long-test inputs -- is fair game.
SEARCH_DIRS=(
	samples
	src/tests/test-src
	src/tests/long/aoc
)
EXCLUDE_DIR="src/tests/test-src/errors"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

total=0
diffs=0
diff_list=()

# Read the file list on fd 3, not stdin (fd 0): the child `syntran`
# invocations below must keep their own stdin (redirected from /dev/null)
# separate from the list-reading `read`, or `read` and the children fight
# over the same pipe and the loop silently truncates after a handful of
# files.
exec 3< <(find "${SEARCH_DIRS[@]}" -name '*.syntran' -print0 | sort -z)
while IFS= read -r -d '' -u 3 f; do
	case "$f" in
		"$EXCLUDE_DIR"/*) continue ;;
	esac

	total=$((total + 1))

	# --cd chdirs into the script's own directory before running it, so
	# relative runtime file I/O (e.g. the AoC inputs' open("input.txt"))
	# resolves the same way `fpm test long` runs them (chdir_ = .true.,
	# see src/tests/long.f90:38,54).  Harmless for files that do no I/O.
	"$REF" -q --cd "$f" </dev/null >"$TMP/ref.out" 2>"$TMP/ref.err"
	ref_status=$?

	"$CUR" -q --cd "$f" </dev/null >"$TMP/cur.out" 2>"$TMP/cur.err"
	cur_status=$?

	if [ "$ref_status" -ne "$cur_status" ] || ! diff -q "$TMP/ref.out" "$TMP/cur.out" >/dev/null 2>&1; then
		diffs=$((diffs + 1))
		diff_list+=("$f")
		echo "DIFF: $f  (exit ref=$ref_status cur=$cur_status)"
		diff -u "$TMP/ref.out" "$TMP/cur.out" | head -20
		echo "---"
	fi
done
exec 3<&-

echo
echo "$diffs / $total files differ"

if [ "$diffs" -ne 0 ]; then
	exit 1
fi
