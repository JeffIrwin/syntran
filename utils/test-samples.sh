#!/usr/bin/env bash
set -eu

# Run (or, where that's not practical, syntax-check) every file in samples/.
# Samples are not covered by src/tests/, so they are the one part of the repo
# most likely to drift out of compliant syntax unnoticed.  This is the
# local/CI entry point that catches that drift.
#
# Every *.syntran file under samples/ must be classified into exactly one of
# the four lists below:
#
#   run_list      Run in file mode from the repo root, stdin closed, must
#                 exit 0.  Per-sample extra args/stdin are set in the `case`
#                 inside run_one() below.
#   run_cd_list   Same, but with `--cd` so bare relative file reads (e.g.
#                 `input.txt`) resolve against the script's own directory
#                 instead of the repo root.
#   syntax_list   Only `--syntax-only` checked (parse + type check, no
#                 execution) -- used for samples that need interactive stdin,
#                 an external dictionary file, a pre-existing output
#                 directory, or that are just too slow to run on every CI
#                 build.
#   fail_list     Expected to fail (nonzero exit) -- deliberately invalid
#                 syntax.
#
# A sample that isn't in any list fails the "unclassified sample" check below,
# so newly added samples can't silently skip coverage.
#
# Usage:
#   utils/test-samples.sh [-v] [path/to/syntran]
#
#   -v   Print each sample's output as it runs.

cd "$(dirname "$0")/.."
repo_root="$(pwd)"

verbose=0
syntran_bin=""
for arg in "$@" ; do
	case "$arg" in
		-v) verbose=1 ;;
		*) syntran_bin="$arg" ;;
	esac
done

# ---- resolve the interpreter to test ----------------------------------------
# (same as utils/test-readme.sh)

resolve_syntran()
{
	local cand
	if [[ -n "$syntran_bin" ]]; then
		cand="$syntran_bin"
	elif [[ -n "${SYNTRAN:-}" ]]; then
		cand="$SYNTRAN"
	else
		for cand in "$repo_root/build/Debug/syntran" "$repo_root/build/Debug/syntran.exe" \
		            "$repo_root/build/Release/syntran" "$repo_root/build/Release/syntran.exe" \
		            "$repo_root/syntran" "$repo_root/syntran.exe" ; do
			[[ -x "$cand" ]] && { echo "$cand"; return; }
		done
		echo ""
		return
	fi
	if [[ -x "$cand" ]]; then
		( cd "$(dirname "$cand")" && echo "$(pwd)/$(basename "$cand")" )
	else
		echo ""
	fi
}

syntran_bin="$(resolve_syntran)"
if [[ -z "$syntran_bin" ]]; then
	echo "No built syntran found; building with ./build.sh debug ..." >&2
	bash "$repo_root/build.sh" debug >&2
	syntran_bin="$repo_root/build/Debug/syntran"
	[[ -x "$syntran_bin" ]] || syntran_bin="$repo_root/build/Debug/syntran.exe"
fi
[[ -x "$syntran_bin" ]] || { echo "error: could not resolve a syntran interpreter to test" >&2; exit 2; }

# ---- classification ---------------------------------------------------------

run_list="
samples/aes.syntran
samples/args.syntran
samples/arithmetic.syntran
samples/arithmetic2.syntran
samples/array-fns.syntran
samples/arrays.syntran
samples/base64.syntran
samples/bin-search.syntran
samples/blocks.syntran
samples/bridge/bridge.syntran
samples/dict_i64_fn.syntran
samples/else-if.syntran
samples/fibonacci-1.syntran
samples/fibonacci-2.syntran
samples/fibonacci-3.syntran
samples/fibonacci-4.syntran
samples/for.syntran
samples/gcd.syntran
samples/hello_name.syntran
samples/logo.syntran
samples/md5.syntran
samples/pi-1.syntran
samples/pi-2.syntran
samples/primes-1.syntran
samples/primes-2.syntran
samples/primes-3.syntran
samples/qsort.syntran
samples/queue_i32.syntran
samples/queue_vec_i32.syntran
samples/quine-1.syntran
samples/quine-2.syntran
samples/quine-3.syntran
samples/random.syntran
samples/sha256.syntran
samples/shebang.syntran
samples/simpson-integrate.syntran
samples/struct.syntran
samples/variables.syntran
samples/vec_i32.syntran
samples/while.syntran
"

# bridge.syntran hardcodes repo-root-relative paths (samples/bridge/input-*.txt
# etc.), so it must run from the repo root -- it is NOT in run_cd_list.

run_cd_list="
samples/aoc/2020/1/main.syntran
samples/aoc/2020/2/main.syntran
samples/aoc/2020/3/main.syntran
"

syntax_list="
samples/betweenle.syntran
samples/poople.syntran
samples/wordle.syntran
samples/wave-equation-1d/wave.syntran
samples/wave-equation-2d/wave.syntran
samples/wave-equation-2d-paraview/wave.syntran
samples/wave-equation-2d-vector/wave.syntran
samples/bridge/utils.syntran
"
# betweenle/poople/wordle: interactive stdin solvers that also need a real
# dictionary file at ~/.local/share/dict/american-english
# wave-equation-*: too slow to run on every CI build; the -paraview/-vector
# variants also require a pre-existing samples/wave-equation-*/frames/ dir
# bridge/utils.syntran: an #include fragment, not a standalone program

fail_list="
samples/bad-syntax.syntran
"
# bad-syntax.syntran is deliberately invalid syntran

# ---- completeness gate -------------------------------------------------------
# Every *.syntran file under samples/ must appear in exactly one list above,
# so a newly added sample can't silently escape coverage.

all_samples="$(find samples -name '*.syntran' | sort)"
classified="$(printf '%s\n%s\n%s\n%s\n' "$run_list" "$run_cd_list" "$syntax_list" "$fail_list" \
	| sed '/^[[:space:]]*$/d' | sort)"

unclassified="$(comm -23 <(printf '%s\n' "$all_samples") <(printf '%s\n' "$classified"))"
duplicated="$(printf '%s\n' "$classified" | sort | uniq -d)"

n_fail=0
fail_report=""

if [[ -n "$unclassified" ]]; then
	n_fail=$((n_fail + 1))
	fail_report="$fail_report
=== unclassified sample(s) -- add to a list in utils/test-samples.sh ===
$unclassified
"
fi
if [[ -n "$duplicated" ]]; then
	n_fail=$((n_fail + 1))
	fail_report="$fail_report
=== sample(s) listed more than once in utils/test-samples.sh ===
$duplicated
"
fi

# ---- artifact cleanup --------------------------------------------------------
# A few samples write into the repo root (both patterns are gitignored).  Only
# remove what this run actually created.

ppm="$repo_root/logo-syntran-v17.ppm"
gv="$repo_root/graph-dummy.gv"
had_ppm=0; [[ -e "$ppm" ]] && had_ppm=1
had_gv=0;  [[ -e "$gv"  ]] && had_gv=1

cleanup()
{
	local status=$?
	[[ "$had_ppm" == 0 && -e "$ppm" ]] && rm -f "$ppm"
	[[ "$had_gv"  == 0 && -e "$gv"  ]] && rm -f "$gv"
	exit "$status"
}
trap cleanup EXIT

# ---- run one sample -----------------------------------------------------------

run_one()
{
	# run_one <mode: run|run_cd|syntax|fail> <path>
	local mode="$1" path="$2" rc=0 out=""
	case "$mode" in
		run)
			case "$path" in
				samples/args.syntran)
					out="$("$syntran_bin" -q --color off "$path" -- foo bar baz < /dev/null 2>&1)" || rc=$? ;;
				samples/hello_name.syntran)
					out="$(printf 'World\n' | "$syntran_bin" -q --color off "$path" 2>&1)" || rc=$? ;;
				*)
					out="$("$syntran_bin" -q --color off "$path" < /dev/null 2>&1)" || rc=$? ;;
			esac
			;;
		run_cd)
			out="$("$syntran_bin" -q --color off --cd "$path" < /dev/null 2>&1)" || rc=$?
			;;
		syntax)
			out="$("$syntran_bin" -q --color off -s "$path" < /dev/null 2>&1)" || rc=$?
			;;
		fail)
			out="$("$syntran_bin" -q --color off "$path" < /dev/null 2>&1)" && rc=0 || rc=$?
			;;
	esac

	if [[ "$verbose" == 1 ]]; then
		echo "--- $path ($mode) ---"
		printf '%s\n' "$out"
	fi

	if [[ "$mode" == "fail" ]]; then
		if [[ "$rc" == 0 ]]; then
			n_fail=$((n_fail + 1))
			fail_report="$fail_report
=== '$path' (mode=fail) expected a nonzero exit but got 0 ===
"
		fi
	elif [[ "$rc" != 0 ]]; then
		n_fail=$((n_fail + 1))
		fail_report="$fail_report
=== '$path' (mode=$mode) exited $rc ===
$out
"
	fi
}

n_run=0
n_syntax=0
n_expect_fail=0

while IFS= read -r path ; do
	[[ -z "$path" ]] && continue
	n_run=$((n_run + 1))
	run_one run "$path"
done <<< "$run_list"

while IFS= read -r path ; do
	[[ -z "$path" ]] && continue
	n_run=$((n_run + 1))
	run_one run_cd "$path"
done <<< "$run_cd_list"

while IFS= read -r path ; do
	[[ -z "$path" ]] && continue
	n_syntax=$((n_syntax + 1))
	run_one syntax "$path"
done <<< "$syntax_list"

while IFS= read -r path ; do
	[[ -z "$path" ]] && continue
	n_expect_fail=$((n_expect_fail + 1))
	run_one fail "$path"
done <<< "$fail_list"

echo ""
echo "syntran samples test summary: $n_run run, $n_syntax syntax-checked, $n_expect_fail expected-fail, $n_fail failed"

if [[ "$n_fail" -gt 0 ]]; then
	printf '%s\n' "$fail_report"
	exit 1
fi

exit 0
