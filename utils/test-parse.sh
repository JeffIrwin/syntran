#!/usr/bin/env bash
set -eu

# Differential test of the syntran port of the parser, compiler/parse.syntran,
# against the Fortran parser.  For every .syntran file, `syntran --parse` dumps
# the Fortran parser's typed tree, compiler/parse-dump.syntran dumps the port's,
# and the two have to be identical.  See parse_dump() in src/types_ops.f90 for
# the format.
#
# The port is not finished yet, so most files don't match.  Only the files in
# utils/parse-pass.txt are required to, and the list grows with each milestone
# of doc/compiler-roadmap.md.  A file in the list that no longer matches is a
# failure, and so is one that is in the list but is missing.
#
# Usage:
#   utils/test-parse.sh [--native] [-v] [--update-pass-list] [path/to/syntran]
#                       [file-or-dir ...]
#
#   --native             Compile compiler/parse-dump.syntran once with
#                        `syntran --native` (which needs LLVM), and run that,
#                        instead of the VM
#   -v                   Print the first lines of the diff for each file that
#                        fails and is required to pass, instead of just its name
#   --update-pass-list   Rewrite utils/parse-pass.txt from this run's matches.
#                        It needs the default inputs, so that the list covers
#                        all of them
#
# With no files or dirs, it tests every .syntran file in src/tests/test-src,
# samples, and src/tests/long.  Any argument that's a .syntran file or a dir is
# an input, and the other is the syntran executable.  Exits 1 if a file in the
# pass list doesn't match.

cd "$(dirname "$0")/.."
repo_root="$(pwd)"

PASS_LIST="utils/parse-pass.txt"

native=0
verbose=0
update_pass_list=0
syntran_bin=""
inputs=()
for arg in "$@" ; do
	case "$arg" in
		--native) native=1 ;;
		-v) verbose=1 ;;
		--update-pass-list) update_pass_list=1 ;;
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

default_inputs=1
if [[ ${#inputs[@]} -eq 0 ]]; then
	inputs=("src/tests/test-src" "samples" "src/tests/long")
else
	default_inputs=0
fi

if [[ $update_pass_list -eq 1 && $default_inputs -eq 0 ]]; then
	echo "error: --update-pass-list can't be combined with file or dir arguments" >&2
	exit 2
fi

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
	"$syntran_bin" -q --native "$work/parse-dump" compiler/parse-dump.syntran
	run_port() { "$work/parse-dump" "$1"; }
else
	run_port() { "$syntran_bin" -q compiler/parse-dump.syntran -- "$1"; }
fi

# The files that are required to pass, one path per line, as a lookup file
required="$work/required"
if [[ -f "$PASS_LIST" ]]; then
	grep -v '^[[:space:]]*\(#\|$\)' "$PASS_LIST" > "$required" || true
else
	: > "$required"
fi

files=()
for input in "${inputs[@]}" ; do
	while IFS= read -r f ; do
		files+=("$f")
	done < <(find "$input" -type f -name '*.syntran' | sort)
done

npass=0
nfail=0
nregress=0
passed=()
newly=0
for f in "${files[@]}" ; do
	"$syntran_bin" --parse "$f" > "$work/expect" 2>&1 || true
	run_port "$f" > "$work/actual" 2>&1 || true

	# The Fortran appends the diagnostics that only its first pass raises, like
	# E22, after the others, and the port raises each where it happens.  So the
	# order of the diagnostics isn't compared, but what and how many are
	if head -n 1 "$work/expect" | grep -q '^diag ' ; then
		sort -o "$work/expect" "$work/expect"
		sort -o "$work/actual" "$work/actual"
	fi

	is_required=0
	grep -qxF -- "$f" "$required" && is_required=1

	if cmp -s "$work/expect" "$work/actual" ; then
		npass=$((npass + 1))
		passed+=("$f")
		[[ $is_required -eq 1 ]] || newly=$((newly + 1))
	else
		nfail=$((nfail + 1))
		if [[ $is_required -eq 1 ]]; then
			nregress=$((nregress + 1))
			echo "REGRESSION: $f is in $PASS_LIST but no longer matches"
			if [[ $verbose -eq 1 ]]; then
				diff "$work/expect" "$work/actual" | head -10 | sed 's/^/    /'
			fi
		fi
	fi
done

# A file in the list that wasn't in this run at all, e.g. it was deleted or
# moved, would otherwise silently stop being checked
nmissing=0
if [[ $default_inputs -eq 1 && $update_pass_list -eq 0 ]]; then
	while IFS= read -r req ; do
		if [[ ! -f "$req" ]]; then
			nmissing=$((nmissing + 1))
			echo "MISSING: $req is in $PASS_LIST but doesn't exist"
		fi
	done < "$required"
fi

echo "parse port: $npass match, $nfail don't, of ${#files[@]} files"

if [[ $update_pass_list -eq 1 ]]; then
	{
		echo "# Files that compiler/parse-dump.syntran must dump the same as"
		echo "# \`syntran --parse\`.  Written by \`utils/test-parse.sh --update-pass-list\`"
		[[ ${#passed[@]} -eq 0 ]] || printf '%s\n' "${passed[@]}"
	} > "$PASS_LIST"
	echo "wrote $PASS_LIST (${#passed[@]} files)"
elif [[ $newly -gt 0 ]]; then
	echo "$newly files match that aren't in $PASS_LIST yet, see --update-pass-list"
fi

[[ $nregress -eq 0 && $nmissing -eq 0 ]]
