#!/usr/bin/env bash
set -eu

# Style linter for syntran source, compiler/lint.syntran, which checks the
# rules that doc/style.md marks as checkable.  This wraps it so that it takes
# dirs and finds the syntran executable, like utils/test-lex.sh.
#
# Usage:
#   utils/lint.sh [--native] [path/to/syntran] [file-or-dir ...]
#
#   --native   Compile compiler/lint.syntran once with `syntran --native`
#              (which needs LLVM), and run that, instead of the VM
#
# With no files or dirs, it lints compiler/.  Any argument that's a .syntran
# file or a dir is an input, and the other is the syntran executable.  Prints
# `path:line:col: rule: message` for each violation, sorted by location.
#
# Exits 0 if clean, 1 if there are violations, and 2 for a problem with the
# setup, like no syntran executable or a lint.syntran that doesn't run.

cd "$(dirname "$0")/.."
repo_root="$(pwd)"

native=0
syntran_bin=""
inputs=()
for arg in "$@" ; do
	case "$arg" in
		--native) native=1 ;;
		*.syntran) inputs+=("$arg") ;;
		*)
			if [[ -d "$arg" ]]; then
				inputs+=("$arg")
			else
				syntran_bin="$arg"
			fi
			;;
	esac
done
[[ ${#inputs[@]} -gt 0 ]] || inputs=("compiler")

if [[ -z "$syntran_bin" ]]; then
	for cand in "$repo_root/build/v1/bin/syntran" "$repo_root/build/Debug/syntran" \
	            "$repo_root/build/Release/syntran" ; do
		[[ -x "$cand" ]] && { syntran_bin="$cand"; break; }
	done
fi
[[ -x "$syntran_bin" ]] || { echo "error: no syntran executable found" >&2; exit 2; }
syntran_bin="$(cd "$(dirname "$syntran_bin")" && pwd)/$(basename "$syntran_bin")"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

files=()
for input in "${inputs[@]}" ; do
	if [[ -d "$input" ]]; then
		while IFS= read -r f ; do
			files+=("$f")
		done < <(find "$input" -type f -name '*.syntran' | sort)
	else
		files+=("$input")
	fi
done
[[ ${#files[@]} -gt 0 ]] || { echo "error: no .syntran files to lint" >&2; exit 2; }

# One command that runs the linter on all of the files, in one process
if [[ $native -eq 1 ]]; then
	"$syntran_bin" -q --native "$work/lint" compiler/lint.syntran
	run_lint() { "$work/lint" "$@"; }
else
	run_lint() { "$syntran_bin" -q compiler/lint.syntran -- "$@"; }
fi

# The interpreter ends with its own "Exiting syntran with status N" message when
# lint.syntran calls std::exit(), so drop that.  An `Error[...]` is lint.syntran
# itself failing to compile or run, which is not a violation
status=0
run_lint "${files[@]}" > "$work/out" 2>&1 || status=$?
if grep -q '^Error\[' "$work/out" ; then
	cat "$work/out" >&2
	exit 2
fi

grep -v '^ Exiting syntran with status' "$work/out" \
	| sort -t: -k1,1 -k2,2n -k3,3n > "$work/sorted" || true
cat "$work/sorted"

nviol=$(wc -l < "$work/sorted" | tr -d ' ')
if [[ $status -ne 0 && $nviol -eq 0 ]]; then
	echo "error: lint.syntran failed with status $status and no violations" >&2
	exit 2
fi
echo "lint: $nviol violation(s) in ${#files[@]} file(s)" >&2
[[ $nviol -eq 0 ]]
