#!/bin/bash
# benchmark-array.sh -- whole-array benchmarks: syntran vs gfortran vs numpy vs
# pure python.  Writes the timing tables in markdown to README.md
#
# Usage:
#   bash src/benchmarks/benchmark-array.sh
#
# Each benchmark <name> has four implementations in src/benchmarks/array/:
#
#   <name>.syntran  <name>.f90  <name>_np.py  <name>_py.py
#
# which each print a single checksum.  The checksums are compared (rel tol
# 1e-8) so a fast-but-wrong result shows up as a failed check in the table.
# syntran, fortran, and numpy report the best of $RUNS runs.  Pure python is
# slow, so it runs only once.  All times are wall-clock seconds, including
# process startup (and the numpy import)
#
# Environment variables:
#   SYNTRAN           syntran binary         (default: build/Release/syntran)
#   FC                fortran compiler       (default: gfortran)
#   FFLAGS            fortran flags          (default: -O2)
#   PYTHON            python interpreter     (default: python3)
#   RUNS              runs per timing        (default: 3)
#   OUT               markdown output file   (default: src/benchmarks/README.md)
#   SKIP_PURE_PYTHON  set to 1 to skip the slow pure python column (~2 min)
#
# If numpy is not importable from $PYTHON, its column is n/a.  This script
# never installs anything.  Try a throwaway venv:
#
#   python3 -m venv /tmp/venv && /tmp/venv/bin/pip install numpy
#   PYTHON=/tmp/venv/bin/python bash src/benchmarks/benchmark-array.sh

set -u

cd "$(dirname "$0")/../.."

SYNTRAN=${SYNTRAN:-build/Release/syntran}
FC=${FC:-gfortran}
FFLAGS=${FFLAGS:--O2}
PYTHON=${PYTHON:-python3}
RUNS=${RUNS:-3}
OUT=${OUT:-src/benchmarks/README.md}
SKIP_PURE_PYTHON=${SKIP_PURE_PYTHON:-0}

SRC=src/benchmarks/array
BIN=build/benchmarks

BENCHES="axpy elem stencil reduce matmul small"
SWEEP_NS="3 10 100 1000 10000 100000 1000000"

if [ ! -x "$SYNTRAN" ] ; then
	echo "error: syntran binary '$SYNTRAN' not found.  Build it first, e.g." >&2
	echo "           ./build.sh release" >&2
	echo "       or set SYNTRAN=/path/to/syntran" >&2
	exit 1
fi
if ! command -v "$FC" > /dev/null ; then
	echo "error: fortran compiler '$FC' not found.  Set FC=..." >&2
	exit 1
fi
if ! command -v "$PYTHON" > /dev/null ; then
	echo "error: python '$PYTHON' not found.  Set PYTHON=..." >&2
	exit 1
fi

HAVE_NUMPY=1
"$PYTHON" -c 'import numpy' 2> /dev/null || HAVE_NUMPY=0
if [ "$HAVE_NUMPY" = 0 ] ; then
	echo "note: numpy not importable from '$PYTHON'.  Its column will be n/a" >&2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

mkdir -p "$BIN"

#===============================================================================

# Time one command, in seconds.  Sets T (elapsed) and CK (the checksum that the
# command printed on stdout, as a normalized number).  T is "ERR" if the
# command failed or printed nothing
time_once()
{
	local TIMEFORMAT=%R
	T=$( { time "$@" > "$TMP/out" 2> "$TMP/err" ; } 2>&1 )
	CK=$(tr -d ' \n' < "$TMP/out" | awk '{ if (NF) printf "%.15g", $1 }')
	if [ -z "$CK" ] ; then
		T=ERR
		CK=
	fi
}

# Best of $RUNS.  Sets T and CK as above
time_best()
{
	local i best=ERR
	for ((i=1; i<=RUNS; i++)) ; do
		time_once "$@"
		if [ "$T" = ERR ] ; then
			best=ERR
			break
		fi
		best=$(awk -v a="$T" -v b="$best" \
			'BEGIN { if (b == "ERR" || a + 0 < b + 0) print a; else print b }')
	done
	T=$best
}

# Format a time cell: 3 decimals, or whatever non-numeric marker was passed
fmt_t()
{
	awk -v t="$1" 'BEGIN { if (t ~ /^[0-9.]+$/) printf "%.3f", t; else printf "%s", t }'
}

# Format a ratio cell $1/$2, "n/a" if either is not a number
fmt_ratio()
{
	awk -v a="$1" -v b="$2" 'BEGIN {
		if (a ~ /^[0-9.]+$/ && b ~ /^[0-9.]+$/ && b + 0 > 0) printf "%.1fx", a / b
		else printf "n/a"
	}'
}

# Do all of the non-empty checksums "$@" agree with the first one?
cks_agree()
{
	awk 'BEGIN {
		ok = 1; ref = ARGV[1] + 0
		for (i = 2; i < ARGC; i++) {
			if (ARGV[i] == "") continue
			d = ARGV[i] + 0 - ref; if (d < 0) d = -d
			s = ref < 0 ? -ref : ref
			if (d > 1e-8 * (s > 1e-300 ? s : 1e-300)) ok = 0
		}
		print ok ? "ok" : "MISMATCH"
	}' "$@"
}

#===============================================================================

echo "Compiling fortran benchmarks with: $FC $FFLAGS" >&2
for f in $BENCHES sweep ; do
	"$FC" $FFLAGS "$SRC/$f.f90" -o "$BIN/$f" || exit 1
done

# Run all four implementations of benchmark $1 and append a row to $TMP/main.md.
# $2... is an optional argument list passed to every implementation
run_bench()
{
	local name=$1 ; shift
	local label=$name
	[ $# -gt 0 ] && label="$name n=$1"

	echo "  $label" >&2

	time_best "$SYNTRAN" -q "$SRC/$name.syntran" -- "$@"
	local ts=$T cs=$CK

	time_best "$BIN/$name" "$@"
	local tf=$T cf=$CK

	local tn=n/a cn=
	if [ "$HAVE_NUMPY" = 1 ] ; then
		time_best "$PYTHON" "$SRC/${name}_np.py" "$@"
		tn=$T cn=$CK
	fi

	local tp=n/a cp=
	if [ "$SKIP_PURE_PYTHON" != 1 ] ; then
		time_once "$PYTHON" "$SRC/${name}_py.py" "$@"
		tp=$T cp=$CK
	fi

	local check
	check=$(cks_agree "${cs:-0}" "$cf" "$cn" "$cp")
	[ -z "$cs" ] && check=MISMATCH

	ROW_LABEL=$label ; ROW_TS=$ts ; ROW_TF=$tf ; ROW_TN=$tn ; ROW_TP=$tp
	ROW_CHECK=$check
}

{
	echo "| benchmark | syntran | gfortran | numpy | python | syntran / gfortran | syntran / numpy | checksums |"
	echo "|---|--:|--:|--:|--:|--:|--:|:-:|"
} > "$TMP/main.md"

echo "Running benchmarks (best of $RUNS) ..." >&2
for b in $BENCHES ; do
	run_bench "$b"
	printf '| %s | %s | %s | %s | %s | %s | %s | %s |\n' \
		"$ROW_LABEL" "$(fmt_t "$ROW_TS")" "$(fmt_t "$ROW_TF")" \
		"$(fmt_t "$ROW_TN")" "$(fmt_t "$ROW_TP")" \
		"$(fmt_ratio "$ROW_TS" "$ROW_TF")" "$(fmt_ratio "$ROW_TS" "$ROW_TN")" \
		"$ROW_CHECK" >> "$TMP/main.md"
done

{
	echo "| array size n | syntran | gfortran | numpy | python | syntran / gfortran | syntran / numpy | checksums |"
	echo "|--:|--:|--:|--:|--:|--:|--:|:-:|"
} > "$TMP/sweep.md"

echo "Running size sweep ..." >&2
for n in $SWEEP_NS ; do
	run_bench sweep "$n"
	printf '| %s | %s | %s | %s | %s | %s | %s | %s |\n' \
		"$n" "$(fmt_t "$ROW_TS")" "$(fmt_t "$ROW_TF")" \
		"$(fmt_t "$ROW_TN")" "$(fmt_t "$ROW_TP")" \
		"$(fmt_ratio "$ROW_TS" "$ROW_TF")" "$(fmt_ratio "$ROW_TS" "$ROW_TN")" \
		"$ROW_CHECK" >> "$TMP/sweep.md"
done

#===============================================================================

cpu=$(sysctl -n machdep.cpu.brand_string 2> /dev/null \
	|| grep -m1 'model name' /proc/cpuinfo 2> /dev/null | cut -d: -f2 | sed 's/^ *//')
syn_ver=$("$SYNTRAN" --version 2>&1 | grep -m1 -i syntran | sed 's/^ *//')
fc_ver=$("$FC" --version 2>&1 | head -1)
py_ver=$("$PYTHON" -c 'import platform; print(platform.python_version())')
np_ver=n/a
[ "$HAVE_NUMPY" = 1 ] && np_ver=$("$PYTHON" -c 'import numpy; print(numpy.__version__)')

{
	echo "# Whole-array benchmarks"
	echo
	echo "Generated by \`src/benchmarks/benchmark-array.sh\` on $(date '+%Y-%m-%d').  Times are"
	echo "wall-clock seconds (lower is better), including process startup.  syntran,"
	echo "gfortran, and numpy are the best of $RUNS runs; pure python is a single run."
	echo
	echo "- CPU: $cpu"
	echo "- syntran: $syn_ver (\`$SYNTRAN\`)"
	echo "- fortran: $fc_ver, \`$FFLAGS\`"
	echo "- python: $py_ver, numpy: $np_ver"
	echo
	echo "## Benchmarks"
	echo
	echo "| name | workload |"
	echo "|---|---|"
	echo "| axpy | \`a = a*0.999 + b\`, 10M f64 elements, 50 iterations |"
	echo "| elem | \`b = b + sqrt(abs(a)) + exp(-a*a)\`, 5M elements, 20 iterations |"
	echo "| stencil | 1D heat equation via slices \`u[1:n-1] += 0.25*(u[0:n-2] - 2*u[1:n-1] + u[2:n])\`, 1M elements, 200 iterations |"
	echo "| reduce | \`sum(a*b) + a @ b + maxval(a-b)\`, 10M elements, 50 iterations |"
	echo "| matmul | \`c = 1e-3 * (m @ c)\`, 300x300, 20 iterations.  Native code finishes in tens of milliseconds here, so this row is dominated by process startup and its ratios are not meaningful |"
	echo "| small | \`v = v + dt*g; x = x + dt*v\` on 3-element vectors, 2M steps |"
	echo
	cat "$TMP/main.md"
	echo
	echo "## Array size sweep"
	echo
	echo "\`v = v + dt*g; x = x + dt*v\` with 60M total elements processed per run, so the"
	echo "number of iterations is \`60M / n\`.  Small arrays are dominated by the fixed cost"
	echo "of each array operation."
	echo
	cat "$TMP/sweep.md"
	echo
	echo "The checksums column compares the printed result of all implementations"
	echo "(rel tol 1e-8).  Anything other than \`ok\` means an implementation printed a"
	echo "different answer, or failed to run, and its timing should not be trusted."
} > "$OUT"

cat "$OUT"
echo "Wrote $OUT" >&2

