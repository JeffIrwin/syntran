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

# Strings only have the "+" operator defined -- these two placeholders are
# only filled in for binfn=="add" below, and blanked out for every other
# operator.  BIN_STR_CASE holds the standalone top-level cases (scalar+scalar,
# array+scalar, scalar+array); BIN_STR_ARR_CASE holds the array+array case,
# which must live *inside* the shared array_type+array_type+array_type
# nested `select case (magic * left%array%type + right%array%type)` block
# (its outer case value collides with every other element type combination,
# so it can't be a second top-level case)
bin_str_case="
	case        (magic**2 * array_type + magic * array_type + str_type)
		! str array + str scalar (elementwise concat).  Array shape/size is
		! not validated here, matching the numeric array+array case below
		select case (left%array%type)
		case (str_type)
			res%array = mold(left%array, str_type)
			block
				integer(kind = 8) :: i_
				allocate(res%array%str( size(left%array%str) ))
				do i_ = 1, size(left%array%str, kind = 8)
					res%array%str(i_)%s = left%array%str(i_)%s // right%str%s
				end do
			end block
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic**2 * array_type + magic * str_type + array_type)
		! str scalar + str array (elementwise concat)
		select case (right%array%type)
		case (str_type)
			res%array = mold(right%array, str_type)
			block
				integer(kind = 8) :: i_
				allocate(res%array%str( size(right%array%str) ))
				do i_ = 1, size(right%array%str, kind = 8)
					res%array%str(i_)%s = left%str%s // right%array%str(i_)%s
				end do
			end block
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic**2 * str_type + magic * str_type + str_type)
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = left%str%s // right%str%s
"

bin_str_arr_case="
		case (magic * str_type + str_type)
			! str array + str array (elementwise concat).  Array shape/size
			! is not validated here, matching the numeric cases above
			res%array = mold(left%array, str_type)
			block
				integer(kind = 8) :: i_
				allocate(res%array%str( size(left%array%str) ))
				do i_ = 1, size(left%array%str, kind = 8)
					res%array%str(i_)%s = left%array%str(i_)%s // right%array%str(i_)%s
				end do
			end block
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
		# Multi-line replacement requires awk
		mv "$ofile" temp
		awk -v r="$bin_str_case" '{gsub(/BIN_STR_CASE/,r)}1' "temp" > "$ofile"
		mv "$ofile" temp
		awk -v r="$bin_str_arr_case" '{gsub(/BIN_STR_ARR_CASE/,r)}1' "temp" > "$ofile"
		rm temp
	else
		sed -i "s/BIN_STR_CASE//g" "$ofile"
		sed -i "s/BIN_STR_ARR_CASE//g" "$ofile"
	fi
done

