#!/usr/bin/env bash
# test-transpile.sh -- differential test of the Fortran transpiler backend.
#
# Runs every .syntran file under samples/ and src/tests/test-src/ through both
# backends and diffs the results:
#
#   1. the interpreter (the VM) runs the file directly, and
#   2. `syntran --transpile` writes a Fortran program, which is compiled with a
#      Fortran compiler and run.
#
# Their stdout and exit status must be identical.  Each file ends up as one of:
#
#   PASS   both backends agree
#   SKIP   not comparable: the transpiler doesn't support a construct in the
#          file yet (diagnostic E115), or the file is meant to fail (parse
#          error, runtime error), or it needs stdin
#   FAIL   the transpiler accepted the file but the generated program doesn't
#          compile, or its output differs.  This is always a bug
#
# Every file listed in utils/transpile-pass.txt must PASS, so a file which used
# to be supported can't quietly regress to SKIP.  When transpiler coverage
# grows, add the newly passing files to that list with --update-pass-list.
#
# Usage:
#   bash utils/test-transpile.sh [options] [syntran_binary] [fortran_compiler]
#
# Options:
#   --update-pass-list   rewrite utils/transpile-pass.txt from this run's PASSes
#   -v, --verbose        print a diff for each FAIL
#   -k, --keep           keep the temporary directory of generated programs
#   --only <regex>       only test files whose path matches this extended regex
#   --ref <binary>       interpreter to produce the expected output with, if
#                        not the same binary.  Don't use a build with -Ofast,
#                        because fast-math changes floating point results
#   -j <n>               number of files to test at once [default: all cores]
#
# Defaults:
#   syntran_binary   = build/v1/bin/syntran     (see: fpm install --prefix ./build/v1/)
#   fortran_compiler = gfortran, honoring $FC
#
# Exit status is nonzero if anything FAILs or a required file doesn't PASS.

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

# Per-run timeout for the interpreter and the generated program.  macOS has no
# `timeout`, but Homebrew coreutils installs `gtimeout`, and perl is always there
with_timeout() {
	if command -v timeout > /dev/null 2>&1; then
		timeout 60 "$@"
	elif command -v gtimeout > /dev/null 2>&1; then
		gtimeout 60 "$@"
	elif command -v perl > /dev/null 2>&1; then
		perl -e 'alarm 60; exec @ARGV' -- "$@"
	else
		"$@"
	fi
}

#===============================================================================
# Worker: test one file, and write the verdict to "$base.result".  This runs in
# a subprocess of the driver below, several at once.  The environment has CUR,
# REF, FC, and VERBOSE

run_one() {
	local f="$1" base="$2"
	local dir exp_status out_status
	dir="$(dirname "$f")"

	# Transpile.  Both unsupported constructs and an invalid program are
	# reported as diagnostics with a nonzero status
	if ! "$CUR" --color off --cd -t "$base.f90" "$f" > "$base.tp.log" 2>&1 < /dev/null; then
		# Remember why, to see what the transpiler should learn next
		grep -o 'E115\]: .* is not supported by the Fortran transpiler yet' "$base.tp.log" \
			| sed -e 's/^E115\]: //' -e 's/ is not supported by.*$//' > "$base.reasons"
		if [ -s "$base.reasons" ]; then
			echo "SKIP unsupported" > "$base.result"
		else
			# Some other diagnostic, i.e. a program that's invalid.  That's
			# expected for some tests
			echo "SKIP invalid $f" > "$base.result"
		fi
		return
	fi

	# The interpreter's result.  Its banner and "Interpreting file" line are
	# the first 5 lines of stdout
	with_timeout "$REF" --color off --cd "$f" > "$base.exp.raw" 2> "$base.exp.err" < /dev/null
	exp_status=$?
	tail -n +6 "$base.exp.raw" > "$base.exp"

	if grep -q 'Runtime error' "$base.exp.raw" "$base.exp.err" 2> /dev/null \
			|| [ "$exp_status" -eq 124 ] || [ "$exp_status" -eq 142 ]; then
		echo "SKIP runtime-error $f" > "$base.result"
		return
	fi

	# The compiler writes the .mod files of the program's modules in the working
	# directory, so each file gets its own for them to not clash when running
	# several at once
	mkdir -p "$base.d"
	if ! (cd "$base.d" && "$FC" -fcheck=all -w -o "$base.exe" "$base.f90") > "$base.fc.log" 2>&1; then
		{
			echo "FAIL (does not compile): $f"
			[ "$VERBOSE" -eq 1 ] && head -20 "$base.fc.log"
		} > "$base.result"
		return
	fi

	# Run in the source's own directory, like `--cd` does for the interpreter
	(cd "$dir" && with_timeout "$base.exe" > "$base.out" 2> "$base.err" < /dev/null)
	out_status=$?

	if [ "$out_status" -ne "$exp_status" ] || ! cmp -s "$base.out" "$base.exp"; then
		{
			echo "FAIL (output differs, status $out_status vs $exp_status): $f"
			[ "$VERBOSE" -eq 1 ] && diff "$base.exp" "$base.out" | head -20 | cut -c1-200
		} > "$base.result"
		return
	fi

	echo "PASS $f" > "$base.result"
}

if [ "${1:-}" = "--worker" ]; then
	run_one "$2" "$3"
	exit 0
fi

#===============================================================================
# Driver

update_pass_list=0
VERBOSE=0
keep=0
only=""
ref=""
jobs=""
positional=()
while [ $# -gt 0 ]; do
	case "$1" in
		--update-pass-list) update_pass_list=1 ;;
		-v|--verbose) VERBOSE=1 ;;
		-k|--keep) keep=1 ;;
		--only) shift; only="$1" ;;
		--ref) shift; ref="$1" ;;
		-j) shift; jobs="$1" ;;
		*) positional+=("$1") ;;
	esac
	shift
done

CUR="${positional[0]:-build/v1/bin/syntran}"
FC="${positional[1]:-${FC:-gfortran}}"
PASS_LIST="utils/transpile-pass.txt"
REF="${ref:-$CUR}"

if [ ! -x "$CUR" ]; then
	echo "error: syntran binary not found or not executable: $CUR" >&2
	exit 1
fi
if [ ! -x "$REF" ]; then
	echo "error: reference binary not found or not executable: $REF" >&2
	exit 1
fi
if ! command -v "$FC" > /dev/null 2>&1; then
	echo "error: fortran compiler not found: $FC" >&2
	exit 1
fi

if [ -z "$jobs" ]; then
	jobs="$(getconf _NPROCESSORS_ONLN 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null || echo 4)"
fi

TMP="$(mktemp -d "${TMPDIR:-/tmp}/syntran-transpile.XXXXXX")"
if [ "$keep" -eq 0 ]; then
	trap 'rm -rf "$TMP"' EXIT
else
	echo "keeping generated programs in $TMP"
fi

# Error repro files are designed to be invalid, and the stack trace tests depend
# on the interpreter's own runtime error output
files=$(
	{
		find samples -name '*.syntran'
		find src/tests/test-src -name '*.syntran' \
			-not -path 'src/tests/test-src/errors/*' \
			-not -path 'src/tests/test-src/stacktrace/*'
	} | sort
)
if [ -n "$only" ]; then
	files=$(echo "$files" | grep -E "$only")
fi

# Test each file in a subprocess, `jobs` at a time.  A file's working files are
# all named "$TMP/t<index>.*"
export CUR REF FC VERBOSE
# (No path in the repo has a space in it)
echo "$files" | awk -v tmp="$TMP" 'NF { print $0; print tmp "/t" NR }' \
	| xargs -n 2 -P "$jobs" bash "${BASH_SOURCE[0]}" --worker

#===============================================================================
# Summarize

npass=0
nskip=0
nfail=0
passed=()
other_skips=()

for res in "$TMP"/t*.result; do
	[ -f "$res" ] || continue
	verdict="$(head -1 "$res")"
	case "$verdict" in
		PASS*)
			npass=$((npass + 1))
			passed+=("${verdict#PASS }")
			;;
		SKIP*)
			nskip=$((nskip + 1))
			# Not a skip because of an unsupported construct
			[ "$verdict" != "SKIP unsupported" ] && other_skips+=("$verdict")
			;;
		*)
			nfail=$((nfail + 1))
			cat "$res"
			;;
	esac
done

# Sorted, so that the pass list is stable
if [ "${#passed[@]}" -gt 0 ]; then
	sorted_passed=()
	while IFS= read -r line; do sorted_passed+=("$line"); done < <(printf '%s\n' "${passed[@]}" | sort)
	passed=("${sorted_passed[@]}")
fi

cat "$TMP"/t*.reasons 2> /dev/null > "$TMP/reasons.txt"
if [ -s "$TMP/reasons.txt" ]; then
	echo
	echo "unsupported constructs, by number of diagnostics:"
	sort "$TMP/reasons.txt" | uniq -c | sort -rn | head -25
fi

if [ "$VERBOSE" -eq 1 ] && [ "${#other_skips[@]}" -gt 0 ]; then
	echo
	echo "skipped for another reason (invalid program, or a runtime error):"
	printf '  %s\n' "${other_skips[@]}"
fi

echo
echo "transpile: $npass passed, $nskip skipped (unsupported or not comparable), $nfail failed"

status=0
[ "$nfail" -gt 0 ] && status=1

if [ "$update_pass_list" -eq 1 ]; then
	printf '%s\n' "${passed[@]}" > "$PASS_LIST"
	echo "wrote $PASS_LIST (${#passed[@]} files)"
elif [ -f "$PASS_LIST" ] && [ -z "$only" ]; then
	# Anything required which didn't pass has regressed
	missing=0
	while IFS= read -r req; do
		[ -z "$req" ] && continue
		found=0
		for p in "${passed[@]+"${passed[@]}"}"; do
			if [ "$p" = "$req" ]; then found=1; break; fi
		done
		if [ "$found" -eq 0 ]; then
			echo "REGRESSION: $req is in $PASS_LIST but no longer passes"
			missing=$((missing + 1))
		fi
	done < "$PASS_LIST"
	[ "$missing" -gt 0 ] && status=1
fi

exit $status
