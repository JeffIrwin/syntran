
!===============================================================================

module syntran__errors_m

	use syntran__utils_m

	implicit none

	character(len = :), allocatable :: err_prefix, err_int_prefix, err_rt_prefix

	! A text span indicates which characters to underline in a faulty line of
	! code
	type text_span_t
		integer :: start, length
	end type text_span_t

	! Span is different for each error.  Other things, like the src_file name,
	! text, and lines, stay the same (at least per parser invocation for now).
	! Those constants are in text_context_t, which is constructed within
	! new_parser() and new_lexer()
	type text_context_t

		! Text is the full text of the source code with filename src_file.  The
		! array lines(:) contains the character indices of the start of each
		! line
		character(len = :), allocatable :: text, src_file
		integer, allocatable :: lines(:)

	end type text_context_t

	!********

	! With include files, the context needs to be a vector with one element per
	! file
	type text_context_vector_t
		type(text_context_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_context
	end type text_context_vector_t

!===============================================================================

contains

!===============================================================================

function err_bad_i32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad i32 integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end function err_bad_i32

!===============================================================================

function err_bad_i64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad i64 integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end function err_bad_i64

!===============================================================================

function err_bad_hex32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad hexadecimal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end function err_bad_hex32

!===============================================================================

function err_bad_hex64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad hexadecimal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end function err_bad_hex64

!===============================================================================

function err_bad_oct32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad octal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end function err_bad_oct32

!===============================================================================

function err_bad_oct64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad octal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end function err_bad_oct64

!===============================================================================

function err_bad_bin32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad binary integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end function err_bad_bin32

!===============================================================================

function err_bad_bin64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad binary integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end function err_bad_bin64

!===============================================================================

function err_unterminated_str(context, span, str) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: str
	err = err_prefix//'unterminated str literal `'//str &
		//'`' &
		//underline(context, span) &
		//' unterminated str'//color_reset

end function err_unterminated_str

!===============================================================================

function err_array_struct_slice(context, span, array) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: array

	err = err_prefix &
		//'slices are not implemented for arrays of structs, on array `' &
		//array//'`' &
		//underline(context, span) &
		//" slice subscript not implemented"//color_reset

end function err_array_struct_slice

!===============================================================================

function err_struct_array_slice(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_prefix//'slices are not implemented for structs of arrays.  ' &
		//'Only scalar subscripts can be used here' &
		//underline(context, span) &
		//' slice subscript not implemented'//color_reset

end function err_struct_array_slice

!===============================================================================

function err_non_int_subscript(context, span, subscript) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: subscript
	err = err_prefix//'array subscript `'//subscript &
		//'` is not an integer' &
		//underline(context, span) &
		//' non-integer subscript'//color_reset

end function err_non_int_subscript

!===============================================================================

function err_bad_f32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad f32 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end function err_bad_f32

!===============================================================================

function err_bad_f64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'bad f64 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end function err_bad_f64

!===============================================================================

function err_bad_type(context, span, type) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: type
	err = err_prefix//'bad type annotation `'//type//'`' &
		//underline(context, span) &
		//' bad type'//color_reset

end function err_bad_type

!===============================================================================

function err_bad_type_suffix(context, span, type, literal_kind) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: type, literal_kind
	err = err_prefix//'bad literal type suffix `'//type//'` after ' &
		//literal_kind//' literal' &
		//underline(context, span) &
		//' bad type suffix'//color_reset

end function err_bad_type_suffix

!===============================================================================

function err_unexpected_char(context, span, c) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: c
	err = err_prefix &
		//"unexpected character `"//c//"`"//underline(context, span) &
		//" unexpected character"//color_reset

end function err_unexpected_char

!===============================================================================

function err_unexpected_token(context, span, got, kind, expect) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: got, kind ,expect
	err = err_prefix &
		//'unexpected token `'//got//'` of kind `'//kind &
		//'`, expected `'//expect//'`'//underline(context, span) &
		//" unexpected token"//color_reset

end function err_unexpected_token

!===============================================================================

function err_redeclare_var(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_prefix &
		//'variable `'//var//'` has already been declared in this scope' &
		//underline(context, span)//" variable already declared"//color_reset

end function err_redeclare_var

!===============================================================================

function err_redeclare_mem(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_prefix &
		//'member `'//var//'` has already been declared in this struct' &
		//underline(context, span)//" member already declared"//color_reset

end function err_redeclare_mem

!===============================================================================

function err_redeclare_fn(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_prefix &
		//'function `'//fn//'` has already been declared' &
		//underline(context, span)//" function already declared"//color_reset

end function err_redeclare_fn

!===============================================================================

function err_redeclare_struct(context, span, struct) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: struct
	err = err_prefix &
		//'struct `'//struct//'` has already been declared' &
		//underline(context, span)//" struct already declared"//color_reset

end function err_redeclare_struct

!===============================================================================

function err_redeclare_primitive(context, span, struct) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: struct
	err = err_prefix &
		//'struct name `'//struct//'` is reserved for a primitive type' &
		//underline(context, span)//" cannot redeclare primitives"//color_reset

end function err_redeclare_primitive

!===============================================================================

function err_undeclare_var(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_prefix &
		//'variable `'//var//'` has not been declared in this scope' &
		//underline(context, span)//" variable undeclared"//color_reset

end function err_undeclare_var

!===============================================================================

function err_undeclare_fn(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_prefix &
		//'function `'//fn//'` has not been defined' &
		//underline(context, span)//" undefined function"//color_reset

end function err_undeclare_fn

!===============================================================================

function err_no_return(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_prefix &
		//'function `'//fn//'` does not have any return statements' &
		//underline(context, span)//" function without returns"//color_reset

end function err_no_return

!===============================================================================

function err_bad_arg_count(context, span, fn, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, argument_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: fn

	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_prefix &
		//'function `'//fn//'` requires '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong argument count"//color_reset

end function err_bad_arg_count

!===============================================================================

function err_too_few_args(context, span, fn, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, argument_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: fn

	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_prefix &
		//'variadic function `'//fn//'` requires at least '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" not enough arguments"//color_reset

end function err_too_few_args

!===============================================================================

function err_bad_sub_count(context, span, array, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, subscript_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: array

	if (expect == 1) then
		subscript_s = 'subscript'
	else
		subscript_s = 'subscripts'
	end if

	err = err_prefix &
		//'array `'//array//'` requires '//str(expect) &
		//' '//subscript_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong subscript count"//color_reset

end function err_bad_sub_count

!===============================================================================

!function err_bad_sub_rank(context, span, array, expect) result(err)
!	type(text_context_t) :: context
!	type(text_span_t), intent(in) :: span
!	character(len = :), allocatable :: err, subscript_s
!	integer, intent(in):: expect
!
!	character(len = *), intent(in) :: array
!
!	if (expect == 1) then
!		subscript_s = 'subscript'
!	else
!		subscript_s = 'subscripts'
!	end if
!
!	err = err_prefix &
!		//'LHS array `'//array//'` requires scalar ' &
!		//subscript_s//' but was given a range' &
!		//underline(context, span)//" non-scalar subscript"//color_reset
!
!end function err_bad_sub_rank

function err_bad_sub_rank(context, span, rank_) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	integer, intent(in) :: rank_

	err = err_prefix &
		//"subscript index array of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector subscript"//color_reset

end function err_bad_sub_rank

!===============================================================================

function err_scalar_subscript(context, span, scalar) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: scalar

	err = err_prefix &
		//'scalar `'//scalar//'` cannot have subscripts' &
		//underline(context, span)//" unexpected subscripts"//color_reset

end function err_scalar_subscript

!===============================================================================

function err_bad_array_arg_type(context, span, fn, iarg, param, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param, expect, actual

	err = err_prefix &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires an array value of ['//expect &
		//'] but was given an array value of ['//actual//']' &
		//underline(context, span)//" wrong array argument type"//color_reset

end function err_bad_array_arg_type

!===============================================================================

function err_bad_ret_rank(context, span, fn, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	integer, intent(in) :: expect, actual

	err = err_prefix &
		//'function `'//fn &
		//'` requires an array return value of rank-'//str(expect) &
		//' but returns an array of rank-'//str(actual) &
		//underline(context, span)//" wrong return rank"//color_reset

end function err_bad_ret_rank

!===============================================================================

function err_bad_cat_rank(context, span, rank_) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	integer, intent(in) :: rank_

	err = err_prefix &
		//"concatenated array of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector concatenation"//color_reset

end function err_bad_cat_rank

!===============================================================================

function err_bad_array_ret_type(context, span, fn, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn, expect, actual

	err = err_prefix &
		//'function `'//fn &
		//'` requires an array return value of ['//expect &
		//'] but returns an array value of ['//actual//']' &
		//underline(context, span)//" wrong array return type"//color_reset

end function err_bad_array_ret_type

!===============================================================================

function err_bad_ret_type(context, span, fn, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn, expect, actual

	err = err_prefix &
		//'function `'//fn &
		//'` requires return value of '//expect//' but returns a value of ' &
		//actual &
		//underline(context, span)//" wrong return type"//color_reset

end function err_bad_ret_type

!===============================================================================

function err_bad_arg_type(context, span, fn, iarg, param, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param, expect, actual

	err = err_prefix &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires type `'//expect//'` but was given `' &
		//actual//'`' &
		//underline(context, span)//" wrong argument type"//color_reset

end function err_bad_arg_type

!===============================================================================

function err_bad_arg_val(context, span, fn, iarg, param) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param

	err = err_prefix &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a `&` reference but was given a value argument' &
		//underline(context, span)//" missing `&` ref"//color_reset

end function err_bad_arg_val

!===============================================================================

function err_bad_arg_ref(context, span, fn, iarg, param) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param

	err = err_prefix &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a value but was given a `&` reference argument' &
		//underline(context, span)//" bad `&` ref"//color_reset

end function err_bad_arg_ref

!===============================================================================

function err_non_name_ref(context, span) result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_prefix &
		//'`&` reference to unexpected expression kind.  references can only ' &
		//'be made to variable name expressions' &
		//underline(context, span)//" non-name `&` ref"//color_reset

end function err_non_name_ref

!===============================================================================

function err_sub_ref(context, span) result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_prefix &
		//'`&` reference to unexpected subscripted expression.  references can only ' &
		//'be made to name expressions without subscripts' &
		//underline(context, span)//" subscripted `&` ref"//color_reset

end function err_sub_ref

!===============================================================================

function err_bad_arg_rank(context, span, fn, iarg, param, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg, expect, actual

	character(len = *), intent(in) :: fn, param

	err = err_prefix &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires rank-'//str(expect)//' array but was given a rank-' &
		//str(actual)//' array' &
		//underline(context, span)//" wrong argument rank"//color_reset

end function err_bad_arg_rank

!===============================================================================

function err_binary_types(context, span, op, left, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, left, right

	!print *, 'starting err_binary_types'

	err = err_prefix &
		//'binary operator `'//op//'` is not defined for types ' &
		//left//' and '//right//underline(context, span) &
		//" wrong types for this binary operator"//color_reset

end function err_binary_types

!===============================================================================

function err_binary_ranks(context, span, op, left, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op
	integer, intent(in) :: left, right

	!print *, 'starting err_binary_ranks'

	err = err_prefix &
		//'rank mismatch for binary operator `'//op//'` with ranks ' &
		//str(left)//' and '//str(right)//underline(context, span) &
		//" array rank mismatch"//color_reset

end function err_binary_ranks

!===============================================================================

function err_unary_types(context, span, op, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, right
	err = err_prefix &
		//'unary operator `'//op//'` is not defined for type ' &
		//right//underline(context, span) &
		//" wrong type for this unary operator"//color_reset

end function err_unary_types

!===============================================================================

function err_non_array_loop(context, span, range_) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range_
	err = err_prefix &
		//'range `'//trimw(range_)//'` of for loop is not an array' &
		//underline(context, span) &
		//" non-array range"//color_reset

end function err_non_array_loop

!===============================================================================

function err_non_bool_condition(context, span, condition, statement) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: condition, statement
	err = err_prefix &
		//'condition `'//trimw(condition)//'` of '//statement//' is not bool' &
		//underline(context, span) &
		//" non-bool condition"//color_reset

end function err_non_bool_condition

!===============================================================================

function err_non_float_len_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range
	err = err_prefix &
		//'bound `'//range//'` of length-based array range is not a float' &
		//underline(context, span) &
		//" non-float bound"//color_reset

end function err_non_float_len_range

!===============================================================================

function err_non_int_len(context, span, len) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: len
	err = err_prefix &
		//'length `'//len//'` of array is not an integer' &
		//underline(context, span) &
		//" non-int length"//color_reset

end function err_non_int_len

!===============================================================================

function err_bound_type_mismatch(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_prefix &
		//'types of lower and upper range bounds do not match' &
		//underline(context, span) &
		//" mismatched types"//color_reset

end function err_bound_type_mismatch

!===============================================================================

function err_non_num_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range

	! This language will be technically wrong if I add a complex number type
	! (complex numbers are numbers, but un-ordered and hence unsuitable for
	! ranges)
	err = err_prefix &
		//'bound `'//range//'` of array range is not a numeric type' &
		//underline(context, span) &
		//" non-numeric range"//color_reset

end function err_non_num_range

!===============================================================================

function err_non_sca_val(context, span, val) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: val

	err = err_prefix &
		//'value `'//val//'` of uniform array is not a scalar' &
		//underline(context, span) &
		//" non-scalar array value"//color_reset

end function err_non_sca_val

!===============================================================================

function err_non_int_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range
	err = err_prefix &
		//'bound `'//range//'` of array range is not an i32 integer' &
		//underline(context, span) &
		//" non-i32 range"//color_reset

end function err_non_int_range

!===============================================================================

function err_het_array(context, span, elem) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: elem
	err = err_prefix &
		//'array is heterogeneous.  Element `'//elem  &
		//"` does not match the first element's type" &
		//underline(context, span) &
		//" heterogeneous array element"//color_reset

end function err_het_array

!===============================================================================

function err_unset_member(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_prefix &
		//'not all members in struct `'//struct_name//'` are initialized.  ' &
		//'Member `'//mem_name//'` is uninitialized' &
		//underline(context, span) &
		//" uninitialized member(s)"//color_reset

end function err_unset_member

!===============================================================================

function err_reset_member(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_prefix &
		//'member `'//mem_name//'` is already initialized in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" duplicate member"//color_reset

end function err_reset_member

!===============================================================================

function err_non_struct_dot(context, span, ident) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: ident
	err = err_prefix &
		//'dot member access cannot be performed on non-struct variable `' &
		//ident//'`' &
		//underline(context, span) &
		//" dot on a non-struct"//color_reset

end function err_non_struct_dot

!===============================================================================

function err_bad_member_name(context, span, mem_name, struct_var_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	! This msg yells about both the variable name `struct_var_name` and its
	! "class" `struct_name`.  Its useful for dot expressions `var.mem`

	character(len = *), intent(in) :: mem_name, struct_var_name, struct_name
	err = err_prefix &
		//'member `'//mem_name//'` does not exist in struct `'//struct_var_name//'`' &
		//' of type `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

end function err_bad_member_name

!===============================================================================

function err_bad_member_name_short(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	! This msg yells only about the "class" `struct_name`.  Its useful for
	! struct instantiations as in `return Class{mem = val};` where there may not
	! be a variable identifier like in the longer fn above

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_prefix &
		//'member `'//mem_name//'` does not exist in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

end function err_bad_member_name_short

!===============================================================================

function err_bad_member_type(context, span, mem_name, struct_name, act_type, exp_type) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name, exp_type, act_type
	err = err_prefix &
		//'member `'//mem_name//'` in struct `'//struct_name//'` has the wrong type.  ' &
		//'Member requires type `'//exp_type//'` but was given `'//act_type//'`' &
		//underline(context, span) &
		//" bad member type"//color_reset

end function err_bad_member_type

!===============================================================================

function err_inc_404(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_prefix &
		//'#include file `'//filename//'` not found' &
		//underline(context, span) &
		//" file not found"//color_reset

end function err_inc_404

!===============================================================================

function err_inc_read(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_prefix &
		//'#include file `'//filename//'` cannot be read' &
		//underline(context, span) &
		//" cannot read file"//color_reset

end function err_inc_read

!===============================================================================

function err_404(filename) result(err)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: err
	err = err_prefix//'file `'//filename//'` not found'//color_reset
end function err_404

!===============================================================================

function err_eval_unary_type(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unary operator `'//op//'` cannot be evaluated for operand type ' &
		//color_reset

end function err_eval_unary_type

!===============================================================================

function err_eval_binary_types(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'binary operator `'//op//'` cannot be evaluated for operand types ' &
		//color_reset

end function err_eval_binary_types

!===============================================================================

function err_eval_len_array(type_name) result(err)
	character(len = *), intent(in) :: type_name
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'len array cannot be evaluated for type `' &
		//type_name//'`'//color_reset

end function err_eval_len_array

!===============================================================================

function err_eval_unary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected unary operator `'//op//'`'//color_reset

end function err_eval_unary_op

!===============================================================================

function err_eval_node(node_kind) result(err)
	character(len = *), intent(in) :: node_kind
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected node `'//node_kind//'`'//color_reset

end function err_eval_node

!===============================================================================

function err_eval_binary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected binary operator `'//op//'`'//color_reset

end function err_eval_binary_op

!===============================================================================

function err_eval_i32_itr(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'loop iterator `'//op//'` must be of type i32'//color_reset

end function err_eval_i32_itr

!===============================================================================

function new_span(start, length) result(span)
	! Maybe this should take end pos instead of length?  Seems like I end up
	! back-calculating len from pos most of the time

	integer, intent(in) :: start, length

	type(text_span_t) :: span

	span%start    = start
	span%length   = max(length, 1)

end function new_span

!===============================================================================

function new_context(text, src_file, lines) result(context)

	character(len = *), intent(in) :: text, src_file
	integer, intent(in) :: lines(:)
	type(text_context_t) :: context

	context%text = text
	context%src_file = src_file

	! gfortran warns if i just let this auto-allocate in the next line?
	allocate(context%lines( 1: size(lines) ))  

	context%lines = lines

end function new_context

!===============================================================================

function new_context_vector() result(vector)

	type(text_context_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_context_vector

!===============================================================================

subroutine push_context(vector, val)

	class(text_context_vector_t) :: vector
	type(text_context_t) :: val

	!********

	type(text_context_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_context

!===============================================================================

function underline(context, span)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: underline

	character(len = :), allocatable :: str_i, spaces, fg1, rst, col, text
	integer :: i1(1), i, j, str_i_len, start, last, length

	! Get line number.  Ideally use a binary search, but it's so nice to do
	! something with a Fortran intrinsic for a change
	i1 = maxloc(context%lines, context%lines <= span%start)
	i = min(size(context%lines)-1, max(1, i1(1)))

	str_i = str(i)
	str_i_len = len(str_i)

	!print *, 'line # = ', i

	! Pad spaces the same length as the line number string
	spaces = repeat(' ', str_i_len + 2)

	! First and last character indices of line
	start = context%lines(i)
	last  = context%lines(i+1) - 1

	! Trim whitespace from end of line.  Make sure interpreter looks ok with
	! errors at final character of line.  TODO: use is_whitespace()
	do while (context%text(last:last) == line_feed .or. &
	          context%text(last:last) == carriage_return)
		last = last - 1
	end do

	! Without substitution, this doesn't work if the line is indented with tabs,
	! because it depends on how wide the console displays a tab!
	text = tabs2spaces(context%text(start: last))

	! Length of error token(s) within the line
	length = span%length

	! Same idea as 'last' adjustment: trim whitespace from error tokens.  It
	! would make more sense to clamp length directly instead of clamping
	! j :shrug:
	j = max(min(len(context%text), span%start + length - 1), 1)
	!print *, 'char = "', context%text(j:j), '"'
	do while (is_whitespace(context%text(j:j)))
		length = length - 1
		j = min(len(context%text), span%start + length - 1)
	end do
	length = max(length, 1)

	! Here's an example of a rust error message, from which I'm stealing UX:
	!
	! """
	!
	!    Compiling skillet v0.4.0 (C:\git\skillet)
	! error[E0433]: failed to resolve: use of undeclared crate or module `st`
	!  --> src\main.rs:6:5
	!   |
	! 6 | use st::path::PathBuf;
	!   |     ^^ use of undeclared crate or module `st`
	!   |
	! help: there is a crate or module with a similar name
	!   |
	! 6 | use std::path::PathBuf;
	!   |     ~~~
	!
	! """

	col = str(span%start - context%lines(i) + 1)

	fg1 = fg_bright_cyan
	!fg1 = fg_bright_blue

	rst = color_reset

	underline = line_feed//fg1//spaces(2:)//"--> "//rst//context%src_file &
		//":"//str_i//":"//col//line_feed &
		//fg1//     spaces//"| "//line_feed &
		//fg1//" "//str_i//" | "//rst//text//line_feed &
		//fg1//     spaces//"| " &
		//repeat(' ', max(span%start - context%lines(i), 0)) &
		//fg_bright_red//repeat('^', length)

end function underline

!===============================================================================

subroutine internal_error()

	! The goal is for this to be unreachable

	write(*,*) fg_bold_bright_red//'Fatal error'//color_reset
	call exit(exit_failure)

end subroutine internal_error

!===============================================================================

end module syntran__errors_m

!===============================================================================

