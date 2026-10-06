#!/bin/bash
# benchmark-slice.sh — measure the current build's array-slicing performance
# against a pinned reference binary, for the slicing features added in
# commits 15bcb866 and 9c0740bf.
#
# Usage:
#   bash src/benchmarks/benchmark-slice.sh [ref_binary]
#
# Defaults:
#   ref_binary = build/ref-bin/syntran-ref (see utils/corpus-diff.sh's header
#                for how to produce one: `git worktree add ../syntran-ref
#                <tag/commit>` + `fpm build --profile release` there)
#
# What this measures
# -------------------
# All four workloads compile the slice to OP_SLICE.  This originally compared
# the bytecode VM against the (now-removed, 1.6.0) AST walker to decide
# whether a native OP_SLICE_NAT opcode was worthwhile; with only one backend
# left, it instead tracks wall time, compile time, and peak RSS against a
# pinned reference build, to catch perf regressions as the slice/subscript
# fallback paths (runtime_array.f90) evolve.
#
#   slice-small  — tiny 2-elem slices, overhead-dominated.
#   slice-large  — 5000-elem slices, copy-dominated (negative control).
#   slice-bounds — optional-bound forms (a[:k], a[k:], a[:s:]).
#   slice-str    — string char-slices and string-array char-indexing.

set -euo pipefail

# --------------------------------------------------------------------------
# 1. Build release
# --------------------------------------------------------------------------
echo "Building release ..."
fpm build --profile release 2>&1

# Find the just-built release binary (most recently modified under build/).
CUR=$(ls -t build/gfortran_*/app/syntran 2>/dev/null | head -1)
if [[ -z "$CUR" ]]; then
	echo "Error: release binary not found under build/gfortran_*/app/syntran"
	exit 1
fi

REF="${1:-build/ref-bin/syntran-ref}"
if [[ ! -x "$REF" ]]; then
	echo "Warning: reference binary '$REF' not found or not executable."
	echo "  See utils/corpus-diff.sh's header for how to build one."
	echo "  Continuing with current build only (no ref comparison)."
	REF=""
fi

echo "Current binary:   $CUR"
[[ -n "$REF" ]] && echo "Reference binary: $REF"
echo ""

# --------------------------------------------------------------------------
# 2. Helpers
# --------------------------------------------------------------------------
BENCHMARKS=(
	src/benchmarks/slice-small.syntran
	src/benchmarks/slice-large.syntran
	src/benchmarks/slice-bounds.syntran
	src/benchmarks/slice-str.syntran
)

N=3   # repetitions per (file, binary) pair

# Portable "time + peak RSS" wrapper: macOS's `/usr/bin/time -l` reports
# "peak memory footprint" in bytes; GNU `time -v` reports "Maximum resident
# set size" in kbytes.  Print whichever this platform's /usr/bin/time gives.
run_timed() {
	# $1 = binary, $2.. = its args (kept separate so callers can pass
	# multiple CLI flags without word-splitting surprises)
	local binary="$1"
	shift
	if [[ "$(uname)" == "Darwin" ]]; then
		/usr/bin/time -l "$binary" "$@" > /dev/null 2>/tmp/bench-slice-time.$$
	else
		/usr/bin/time -v "$binary" "$@" > /dev/null 2>/tmp/bench-slice-time.$$
	fi
	grep -E 'real|user|sys|peak memory footprint|Maximum resident set size|Elapsed' \
		/tmp/bench-slice-time.$$ || cat /tmp/bench-slice-time.$$
	rm -f /tmp/bench-slice-time.$$
}

run_bench() {
	local label="$1"
	local binary="$2"
	local file="$3"
	echo "  [$label]"
	for (( i=1; i<=N; i++ )); do
		run_timed "$binary" "$file"
	done
}

# compile_tree() (lex -> parse -> compile -> vm_run) has no standalone CLI
# switch to time in isolation.  `-s`/`--syntax-only` stops after parsing, so
# it excludes compile_tree and vm_run both -- not a compile_tree timing by
# itself, but subtracting this from the full run above isolates
# "compile + execute" from "lex + parse", which is the split that matters
# for these slice-heavy workloads (parsing is O(source size), not O(data)).
run_parse_only() {
	local label="$1"
	local binary="$2"
	local file="$3"
	echo "  [$label parse-only, --syntax-only]"
	for (( i=1; i<=N; i++ )); do
		run_timed "$binary" --syntax-only "$file"
	done
}

sanity_check() {
	local file="$1"
	local cur_out ref_out
	cur_out=$("$CUR" "$file" 2>/dev/null)
	if [[ -z "$REF" ]]; then
		echo "  [sanity] current output: $cur_out"
		return
	fi
	ref_out=$("$REF" "$file" 2>/dev/null)
	if [[ "$cur_out" == "$ref_out" ]]; then
		echo "  [sanity] current == ref output: ok ($cur_out)"
	else
		echo "  [sanity] MISMATCH: current='$cur_out'  ref='$ref_out'"
	fi
}

# --------------------------------------------------------------------------
# 3. Run benchmarks
# --------------------------------------------------------------------------
for bench in "${BENCHMARKS[@]}"; do
	echo "=============================================================="
	echo "  $bench"
	echo "--------------------------------------------------------------"
	sanity_check "$bench"
	echo ""
	run_bench current "$CUR" "$bench"
	echo ""
	run_parse_only current "$CUR" "$bench"
	if [[ -n "$REF" ]]; then
		echo ""
		run_bench reference "$REF" "$bench"
	fi
	echo ""
done

echo "=============================================================="
echo "Done.  Compare wall time / peak RSS above: current vs. reference"
echo "per workload.  A regression on slice-small/slice-bounds/slice-str"
echo "with slice-large flat is a good signal it's overhead, not copy cost."
