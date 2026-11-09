#!/usr/bin/env bash
set -eu

# Differential test of the syntran port of the lexer, compiler/lex.syntran,
# against the Fortran lexer.  For every .syntran file, `syntran --lex` dumps the
# Fortran lexer's tokens, compiler/lex-dump.syntran dumps the port's, and the
# two have to be identical.  See lex_dump() in src/lex.f90 for the format.
#
# Usage:
#   utils/test-lex.sh [--native] [-v] [path/to/syntran] [file-or-dir ...]
#
#   --native   Compile compiler/lex-dump.syntran once with `syntran --native`
#              (which needs LLVM), and run that, instead of the VM
#   -v         Print the first lines of the diff for each failing file, instead
#              of just the file name
#
# With no files or dirs, it tests every .syntran file in src/tests/test-src.
# Any argument that's a .syntran file or a dir is an input, and the other is the
# syntran executable.  Exits 1 if any file doesn't match.

cd "$(dirname "$0")/.."
repo_root="$(pwd)"

native=0
verbose=0
syntran_bin=""
inputs=()
for arg in "$@" ; do
	case "$arg" in
		--native) native=1 ;;
		-v) verbose=1 ;;
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
[[ ${#inputs[@]} -gt 0 ]] || inputs=("src/tests/test-src")

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

# One command that runs the port on a file
if [[ $native -eq 1 ]]; then
	"$syntran_bin" -q --native "$work/lex-dump" compiler/lex-dump.syntran
	run_port() { "$work/lex-dump" "$1"; }
else
	run_port() { "$syntran_bin" -q compiler/lex-dump.syntran -- "$1"; }
fi

files=()
for input in "${inputs[@]}" ; do
	while IFS= read -r f ; do
		files+=("$f")
	done < <(find "$input" -type f -name '*.syntran' | sort)
done

npass=0
nfail=0
for f in "${files[@]}" ; do
	"$syntran_bin" --lex "$f" > "$work/expect" 2>&1 || true
	run_port "$f" > "$work/actual" 2>&1 || true

	if cmp -s "$work/expect" "$work/actual" ; then
		npass=$((npass + 1))
	else
		nfail=$((nfail + 1))
		echo "FAIL: $f"
		if [[ $verbose -eq 1 ]]; then
			diff "$work/expect" "$work/actual" | head -10 | sed 's/^/    /'
		fi
	fi
done

echo "lex port: $npass passed, $nfail failed, of ${#files[@]} files"
[[ $nfail -eq 0 ]]
