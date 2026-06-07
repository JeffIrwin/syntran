#!/bin/bash
# benchmark-slice.sh — measure VM vs AST performance for the new array
# slicing features added in commits 15bcb866 and 9c0740bf.
#
# Usage:
#   bash src/benchmarks/benchmark-slice.sh
#
# What this measures
# ------------------
# All four workloads compile the slice to OP_SLICE, which currently
# delegates to the AST evaluator (eval_name_expr).  The VM-vs-AST
# gap on each workload tells us the maximum headroom for a native
# OP_SLICE_NAT opcode:
#
#   slice-small  — tiny 2-elem slices, overhead-dominated.
#                  Large VM/AST gap → native opcode is worthwhile.
#                  Small gap       → already copy/alloc-dominated; skip opcodes.
#   slice-large  — 5000-elem slices, copy-dominated (negative control).
#                  Expected: VM ≈ AST regardless.
#   slice-bounds — optional-bound forms (a[:k], a[k:], a[:s:]).
#   slice-str    — string char-slices and string-array char-indexing.
#
# Decision gate
# -------------
# If slice-small shows VM markedly faster than AST:
#   → A native OP_SLICE_NAT (compile bounds onto operand stack, inline copy)
#     is justified.  Plan it, mirroring OP_INDEX_NAT in compile_ctrl.f90.
# If slice-small is VM ≈ AST (copy/alloc-dominated):
#   → Skip new opcodes; pursue the backend-agnostic win instead:
#     reuse scratch buffers (asubs/lsubs/ssubs/usubs/subs/tmp) in
#     get_subscript_range / eval_name_expr to cut allocator traffic.
#
# Backends
# --------
# SYNTRAN_BACKEND=bytecode  — default VM (since the feature/bytecode merge)
# SYNTRAN_BACKEND=ast       — legacy AST walker (deprecated but still present)

set -euo pipefail

# --------------------------------------------------------------------------
# 1. Build release
# --------------------------------------------------------------------------
echo "Building release ..."
fpm build --profile release 2>&1

# Find the just-built release binary (most recently modified under build/).
BINARY=$(ls -t build/gfortran_*/app/syntran 2>/dev/null | head -1)
if [[ -z "$BINARY" ]]; then
	echo "Error: release binary not found under build/gfortran_*/app/syntran"
	exit 1
fi
echo "Using binary: $BINARY"
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

N=3   # repetitions per (file, backend) pair

run_bench() {
	local backend="$1"
	local file="$2"
	echo "  [SYNTRAN_BACKEND=$backend]"
	for (( i=1; i<=N; i++ )); do
		SYNTRAN_BACKEND="$backend" time "$BINARY" "$file"
	done
}

sanity_check() {
	local file="$1"
	local vm_out ast_out
	vm_out=$(SYNTRAN_BACKEND=bytecode "$BINARY" "$file" 2>/dev/null)
	ast_out=$(SYNTRAN_BACKEND=ast    "$BINARY" "$file" 2>/dev/null)
	if [[ "$vm_out" == "$ast_out" ]]; then
		echo "  [sanity] VM == AST output: ok ($vm_out)"
	else
		echo "  [sanity] MISMATCH: VM='$vm_out'  AST='$ast_out'"
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
	run_bench bytecode "$bench"
	echo ""
	run_bench ast      "$bench"
	echo ""
done

echo "=============================================================="
echo "Done.  Compare 'real' times above: bytecode vs. ast per workload."
echo "Large gap on slice-small  → native OP_SLICE_NAT is justified."
echo "Small gap on slice-small  → pursue alloc-reuse in get_subscript_range."
