#!/usr/bin/env bash

set -exu

# Generate WET fortran source for binary math operators from a single template
#
# TODO: remove generated files from git tracking.  I assume mac will be a pain
# to get awk working, Windows might be tricky too

#===============================================================================

# Table of function names and operator strings
table=()
table+=("add"      "+" )
table+=("subtract" "-" )
table+=("mul"      "*" )
table+=("div"      "\/" )
table+=("pow"      "**")

#echo "table = ${table[@]}"

ncols=2
nrows=$(( ${#table[@]} / $ncols ))

#echo "nrows = $nrows"
#echo "table 0 = ${table[0]}"

bin_str_case="
	case        (magic**2 * str_type + magic * str_type + str_type)
		res%sca%str%s = left%sca%str%s // right%sca%str%s
"

for i in $(seq 0 $(( $nrows - 1 )) ) ; do
	echo "$i"

	binfn=${table[$(( $i * $ncols + 0 ))]}
	binop=${table[$(( $i * $ncols + 1 ))]}

	ofile="src/math_bin_${binfn}.f90"
	cp src/math_bin_template.f90 "$ofile"

	sed -i "s/BINFN/$binfn/g" "$ofile"
	sed -i "s/BINOP/$binop/g" "$ofile"

	if [[ "$binfn" == "add" ]] ; then
		# Strings only have the "+" operator defined
		#
		# Multi-line replacement requires awk
		mv "$ofile" temp
		awk -v r="$bin_str_case" '{gsub(/BIN_STR_CASE/,r)}1' "temp" > "$ofile"
		rm temp
	else
		sed -i "s/BIN_STR_CASE//g" "$ofile"
	fi
done

