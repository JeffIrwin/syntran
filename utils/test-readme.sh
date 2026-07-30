#!/usr/bin/env bash
set -eu

# Extract every marked code block from README.md, run it against the syntran
# interpreter, and verify the output matches the hidden `syntran-expect`
# comment.  This is the local/CI entry point for README doc-testing.
#
# Markup format (invisible when the doc is rendered):
#
#   <!-- syntran-begin mode=repl group=NAME -->
#   ```cpp
#   ... syntran source, copied verbatim from the doc ...
#   ```
#   <!-- syntran-expect
#   ... exact normalized output, or omitted to assert empty output ...
#   -->
#   <!-- syntran-end -->
#
# Blocks sharing the same `group=` are concatenated in document order and run
# as a single unit (needed where later blocks in a doc section reuse
# variables from earlier ones).  `dir=NAME` puts a group in a shared temp
# directory (default: the group name) -- used by the module examples, where
# one block writes a module source file (`file=NAME`, not executed on its
# own) and later blocks `use` it.  `mode=skip reason="..."` blocks are not
# run at all.
#
# Usage:
#   utils/test-readme.sh [--update] [-v] [path/to/syntran]
#
#   --update   Regenerate the `syntran-expect` bodies in README.md from the
#              interpreter's actual current output, instead of checking them.
#   -v         Print each group's normalized actual output as it runs.

cd "$(dirname "$0")/.."
repo_root="$(pwd)"
readme="$repo_root/README.md"

update=0
verbose=0
syntran_bin=""
for arg in "$@" ; do
	case "$arg" in
		--update) update=1 ;;
		-v) verbose=1 ;;
		*) syntran_bin="$arg" ;;
	esac
done

# ---- resolve the interpreter to test ----------------------------------------

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
	# Resolve to an absolute path: the interpreter is invoked from per-group
	# temp directories later, so a relative path given on the command line
	# or via $SYNTRAN would otherwise stop resolving after the first `cd`.
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

run_syntran()
{
	# run_syntran <mode: repl|file|help> <cwd> [scriptfile]
	local mode="$1" cwd="$2" script="${3:-}"
	case "$mode" in
		help) ( cd "$cwd" && "$syntran_bin" -h ) 2>&1 ;;
		repl) ( cd "$cwd" && "$syntran_bin" -q --color off ) 2>&1 ;;
		file) ( cd "$cwd" && "$syntran_bin" -q --color off "$script" ) 2>&1 ;;
	esac
}

# ---- workdir -----------------------------------------------------------------

workdir="$(mktemp -d "${TMPDIR:-/tmp}/syntran-readme-test.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT
dirs="$workdir/dirs"
mkdir -p "$dirs"
manifest="$workdir/manifest.tsv"
: > "$manifest"

# ---- pass 1: extract every marked block into per-group files -----------------
#
# Writes:
#   dirs/<dir>/<file>          for role=src blocks (module source files)
#   dirs/<dir>/<group>.in      concatenated source for repl/file groups
#   dirs/<dir>/<group>.expect  the block's hidden expected output, if any
#   manifest.tsv               one row per executable group, in doc order:
#                                 group \t mode \t dir \t readme_line

awk -v workdir="$workdir" -v manifest="$manifest" '
function get_attr(str, key,    re, val) {
	re = key "=\"[^\"]*\""
	if (match(str, re)) {
		val = substr(str, RSTART, RLENGTH)
		sub("^" key "=\"", "", val)
		sub("\"$", "", val)
		return val
	}
	re = key "=[^ ]+"
	if (match(str, re)) {
		val = substr(str, RSTART, RLENGTH)
		sub("^" key "=", "", val)
		return val
	}
	return ""
}
BEGIN { state = "OUTSIDE" }
{
	line = $0
	sub(/\r$/, "", line)

	if (state == "OUTSIDE") {
		if (line ~ /^<!-- syntran-begin .* -->$/) {
			attrs = line
			sub(/^<!-- syntran-begin /, "", attrs)
			sub(/ -->$/, "", attrs)

			mode  = get_attr(attrs, "mode")
			group = get_attr(attrs, "group")
			dir   = get_attr(attrs, "dir")
			file_ = get_attr(attrs, "file")
			if (dir == "" && group != "") dir = group
			is_src = (file_ != "")

			if (!(dir in seen_dir) && dir != "") {
				system("mkdir -p \"" workdir "/dirs/" dir "\"")
				seen_dir[dir] = 1
			}

			if (mode == "skip") {
				dest = "/dev/null"
			} else if (mode == "help") {
				dest = "/dev/null"
			} else if (is_src) {
				dest = workdir "/dirs/" dir "/" file_
			} else {
				dest = workdir "/dirs/" dir "/" group ".in"
			}

			if (mode != "skip" && !is_src) {
				key = dir SUBSEP group
				if (!(key in seen_group)) {
					seen_group[key] = 1
					print group "\t" mode "\t" dir "\t" NR > manifest
				}
			}
			expect_dest = workdir "/dirs/" dir "/" group ".expect"

			state = "WAIT_FENCE_OPEN"
			next
		}
		next
	}

	if (state == "WAIT_FENCE_OPEN") {
		if (line !~ /^```/) {
			print "malformed README.md: expected fenced code block after syntran-begin at line " NR > "/dev/stderr"
			exit 2
		}
		state = "IN_FENCE"
		next
	}

	if (state == "IN_FENCE") {
		if (line == "```") {
			state = "AFTER_FENCE"
			next
		}
		print line > dest
		next
	}

	if (state == "AFTER_FENCE") {
		if (line == "<!-- syntran-expect") {
			state = "IN_EXPECT"
			next
		}
		if (line == "<!-- syntran-end -->") {
			state = "OUTSIDE"
			next
		}
		print "malformed README.md: expected syntran-expect or syntran-end after fence, line " NR > "/dev/stderr"
		exit 2
	}

	if (state == "IN_EXPECT") {
		if (line == "-->") {
			state = "WAIT_END_AFTER_EXPECT"
			next
		}
		print line > expect_dest
		next
	}

	if (state == "WAIT_END_AFTER_EXPECT") {
		if (line != "<!-- syntran-end -->") {
			print "malformed README.md: expected syntran-end after syntran-expect, line " NR > "/dev/stderr"
			exit 2
		}
		state = "OUTSIDE"
		next
	}
}
' "$readme"

# ---- normalization (applied to live output only; .expect files are already
#      normalized by construction) ---------------------------------------------

normalize()
{
	# normalize <mode>   -- reads stdin, writes normalized text to stdout
	local mode="$1"
	awk -v mode="$mode" '
	{
		line = $0
		gsub(/\r$/, "", line)
		if (mode == "repl") {
			while (1) {
				if (line ~ /^syntran\$ /) { sub(/^syntran\$ /, "", line); continue }
				if (line ~ /^> /)         { sub(/^> /, "", line); continue }
				if (line ~ /^\[Hint `[^`]*`\]> /) { sub(/^\[Hint `[^`]*`\]> /, "", line); continue }
				break
			}
		}
		if (mode == "help") {
			gsub(/syntran [0-9]+\.[0-9]+\.[0-9]+/, "syntran X.Y.Z", line)
		}
		sub(/[ \t]+$/, "", line)
		if (line != "") print line
	}'
}

# ---- pass 2: run each group and compare ---------------------------------------

n_groups=0
n_fail=0
n_skip=$(grep -c 'syntran-begin mode=skip' "$readme" || true)

fail_report=""

while IFS=$'\t' read -r group mode dir readme_line ; do
	n_groups=$((n_groups + 1))
	gdir="$dirs/$dir"
	infile="$gdir/$group.in"
	expectfile="$gdir/$group.expect"

	case "$mode" in
		help)
			actual_raw="$(run_syntran help "$gdir")"
			;;
		repl)
			actual_raw="$( ( cd "$gdir" && "$syntran_bin" -q --color off < "$infile" ) 2>&1 )"
			;;
		file)
			mainfile="main_$group.syntran"
			cp "$infile" "$gdir/$mainfile"
			actual_raw="$(run_syntran file "$gdir" "$mainfile")"
			;;
		*)
			echo "unknown mode '$mode' for group $group" >&2
			exit 2
			;;
	esac

	actual="$(printf '%s\n' "$actual_raw" | normalize "$mode")"

	if [[ -f "$expectfile" ]]; then
		expected="$(cat "$expectfile")"
	else
		expected=""
	fi

	if [[ "$verbose" == 1 ]]; then
		echo "--- $group ($mode, README:$readme_line) ---"
		printf '%s\n' "$actual"
	fi

	if [[ "$update" == 1 ]]; then
		if [[ -n "$actual" ]]; then
			printf '%s\n' "$actual" > "$gdir/$group.actual"
		else
			rm -f "$gdir/$group.actual"
		fi
		continue
	fi

	if [[ "$actual" != "$expected" ]]; then
		n_fail=$((n_fail + 1))
		fail_report="$fail_report
=== group '$group' (mode=$mode, README.md:$readme_line) MISMATCH ===
$(diff -u <(printf '%s\n' "$expected") <(printf '%s\n' "$actual") || true)
"
	fi
done < "$manifest"

if [[ "$update" == 1 ]]; then
	# Rewrite README.md's syntran-expect bodies from the freshly captured
	# <group>.actual files written above.
	awk -v readme_path="$readme" -v dirs="$dirs" -f "$repo_root/utils/readme-update-expect.awk" \
		"$readme" > "$readme.new"
	mv "$readme.new" "$readme"
	echo "Updated syntran-expect blocks in README.md from actual interpreter output."
	exit 0
fi

echo ""
echo "syntran README.md test summary: $n_groups groups run, $n_skip blocks skipped, $n_fail failed"

if [[ "$n_fail" -gt 0 ]]; then
	printf '%s\n' "$fail_report"
	exit 1
fi

exit 0
