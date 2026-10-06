#!/usr/bin/env bash
set -eu

# Verify that every data member of a "manually copied" derived type -- one
# with a hand-written copy/destroy/move/move_into quartet instead of relying
# on gfortran's default recursive assignment (see the NOTE on syntax_node_t
# in types.f90) -- is actually referenced by all of them. A member added to
# the type declaration but forgotten in one of the four procedures is a
# silent bug: a dangling ref on copy, a leak on destroy, or a stale value
# left in dst after a move.
#
# This is a heuristic text check, not a Fortran parser: it looks for
# `%<member>` (word-bounded) in each procedure's source text. False
# negatives are possible for unusually-formatted declarations; a match
# failure should be treated as "go look", not as an infallible verdict.
#
# Usage: utils/check-node-sync.sh

cd "$(dirname "$0")/.."

fail=0

# extract_fields <decl_file> <type_name>
#   Prints one line per data member declared in `type <type_name> ... end
#   type`, as "<name> <is_bare_intrinsic_scalar:0|1>". Declarations after
#   `contains` (procedure bindings) are not data members and are skipped.
extract_fields()
{
	local decl_file="$1" type_name="$2"
	awk -v type_name="$type_name" '
	BEGIN { in_type = 0; in_contains = 0; buf = "" }
	{
		line = $0
		if (!in_type) {
			if (line ~ ("^[ \t]*type[ \t]+" type_name "[ \t]*$")) in_type = 1
			next
		}
		if (line ~ ("^[ \t]*end[ \t]+type[ \t]+" type_name "[ \t]*$")) { in_type = 0; next }
		if (line ~ /^[ \t]*contains[ \t]*$/) { in_contains = 1 }
		if (in_contains) next

		# strip inline/full-line comments
		sub(/!.*/, "", line)
		if (line ~ /^[ \t]*$/) next

		# accumulate continuation lines (trailing &) into one logical line
		trimmed = line
		sub(/[ \t]+$/, "", trimmed)
		if (trimmed ~ /&[ \t]*$/) {
			sub(/&[ \t]*$/, "", trimmed)
			buf = buf trimmed " "
			next
		}
		decl = buf trimmed
		buf = ""

		if (decl !~ /::/) next

		split(decl, halves, "::")
		typespec = halves[1]
		namelist = halves[2]

		is_alloc = (typespec ~ /allocatable/) ? 1 : 0
		# A bare (non-allocatable) integer/logical scalar has nothing to
		# deallocate, so destroy() legitimately never mentions it
		is_bare_scalar = (typespec ~ /^[ \t]*(integer|logical)/ && !is_alloc) ? 1 : 0

		n = split(namelist, names, ",")
		for (i = 1; i <= n; i++) {
			nm = names[i]
			sub(/^[ \t]+/, "", nm)
			sub(/=.*/, "", nm)   # drop default value
			sub(/\(.*/, "", nm)  # drop array-dim suffix
			sub(/[ \t]+$/, "", nm)
			if (nm == "") continue
			print nm, is_bare_scalar
		}
	}
	' "$decl_file"
}

# extract_body <impl_file> <proc_name>
#   Prints the source lines of `[recursive] module subroutine <proc_name>(...)`
#   up to its matching `end subroutine <proc_name>`.
extract_body()
{
	local impl_file="$1" proc_name="$2"
	awk -v proc_name="$proc_name" '
	BEGIN { on = 0 }
	{
		if (!on && $0 ~ ("subroutine[ \t]+" proc_name "\\(")) on = 1
		if (on) print
		if (on && $0 ~ ("^[ \t]*end[ \t]+subroutine[ \t]+" proc_name "[ \t]*$")) exit
	}
	' "$impl_file"
}

# check_sync <label> <decl_file> <type_name> <impl_file> <proc:kind> [...]
#   kind is "full" (every member must appear) or "destroy" (bare
#   integer/logical scalar members are exempt)
check_sync()
{
	local label="$1" decl_file="$2" type_name="$3" impl_file="$4"
	shift 4

	local fields
	fields="$(extract_fields "$decl_file" "$type_name")"
	if [[ -z "$fields" ]]; then
		echo "check-node-sync: found no data members for $type_name in $decl_file -- parser bug?" >&2
		exit 2
	fi

	local spec name kind body missing name_field is_bare_scalar
	for spec in "$@"; do
		name="${spec%%:*}"
		kind="${spec##*:}"
		body="$(extract_body "$impl_file" "$name")"
		if [[ -z "$body" ]]; then
			echo "check-node-sync: could not find subroutine '$name' in $impl_file" >&2
			exit 2
		fi

		missing=""
		while read -r name_field is_bare_scalar; do
			[[ -z "$name_field" ]] && continue
			if [[ "$kind" == "destroy" && "$is_bare_scalar" == "1" ]]; then
				continue
			fi
			if ! grep -qE "%${name_field}([^A-Za-z0-9_]|\$)" <<< "$body"; then
				missing="$missing $name_field"
			fi
		done <<< "$fields"

		if [[ -n "$missing" ]]; then
			echo "check-node-sync: $label: ${name}() is missing:$missing" >&2
			fail=1
		fi
	done
}

check_sync "syntax_node_t" src/types.f90 syntax_node_t src/types_copy.f90 \
	syntax_node_copy:full \
	syntax_node_destroy:destroy \
	syntax_node_move:full \
	syntax_node_move_into:full

if [[ "$fail" -ne 0 ]]; then
	exit 1
fi

echo "check-node-sync: OK"
