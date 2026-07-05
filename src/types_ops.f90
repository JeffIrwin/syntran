
!===============================================================================

submodule (syntran__types_m) syntran__types_ops

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module function syntax_node_str(node, indent) result(str_)

	! Convert tree to string in JSON-ish format.  Incomplete since I've added so
	! many new members

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str_

	!********

	character(len = :), allocatable :: indentl, kind, left, op, right, val, &
		type, identifier, block

	integer :: i

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind  = '//kind_name(node%kind)//line_feed

	left  = ''
	op    = ''
	right = ''
	val   = ''
	block = ''

	identifier = ''

	type  = indentl//'    type  = '//kind_name(node%val%type)//line_feed

	! FIXME: add str conversions for more recent kinds: condition, if_clause,
	! etc.

	if      (node%kind == binary_expr) then

		left  = indentl//'    left  = '//node%left %str(indentl//'    ') &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == fn_declaration) then
		val = indentl//'    body = '//node%body%str(indentl//'    ')//line_feed

	else if (node%kind == fn_call_expr) then
		val = indentl//'    id_index = '//str(node%id_index)//line_feed

	else if (node%kind == return_statement) then
		val = indentl//'    expr = '//node%right%str(indentl//'    ')//line_feed

	else if (node%kind == block_statement) then

		do i = 1, size(node%members)
			block = block // node%members(i)%str(indentl//'    ')
		end do
		block = block // line_feed

	else if (node%kind == translation_unit) then

		type = ''
		do i = 1, size(node%members)
			block = block // node%members(i)%str(indentl//'    ')
		end do
		block = block // line_feed

	else if (node%kind == assignment_expr) then

		identifier  = indentl//'    identifier = '//node%identifier%text &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == unary_expr) then

		op    = indentl//'    op    = '//node%op%text//line_feed
		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == literal_expr) then
		val   = indentl//'    val   = '//node%val%to_str()//line_feed
	end if

	str_ = line_feed// &
		indentl//'{'//line_feed// &
			kind       // &
			type       // &
			identifier // &
			left       // &
			op         // &
			right      // &
			val        // &
			block      // &
		indentl//'}'

end function syntax_node_str

!===============================================================================

module subroutine log_diagnostics(node, ou)

	class(syntax_node_t), intent(in) :: node
	integer, optional   , intent(in) :: ou

	!********

	character(len = :), allocatable :: s
	integer :: i, oul, nlog, nomit

	oul = output_unit
	if (present(ou)) oul = ou

	! This is nice for unclosed parens instead of having a huge number of
	! cascading errors.  4 seems like a good default bc 4 errors fit on my
	! laptop screen

	if (maxerr > 0) then
		nlog = min(node%diagnostics%len_, maxerr)
	else
		nlog = node%diagnostics%len_
	end if

	do i = 1, nlog
		write(oul, '(a)') node%diagnostics%v(i)%s
		write(oul,*)
	end do

	nomit = node%diagnostics%len_ - nlog
	if (nomit > 0) then
		if (nomit > 1) then
			s = "s"
		else
			s = ""
		end if
		write(oul, *) fg_bold//'[[ '//str(nomit) &
			//' more error'//s//' omitted ]]'//color_reset
	end if

end subroutine log_diagnostics

!===============================================================================

module integer function lookup_type(name, structs, struct) result(type)

	character(len = *), intent(in) :: name

	type(structs_t), intent(in) :: structs

	type(struct_t), intent(out) :: struct

	!********

	integer :: io, struct_id

	! Immo also has an "any" type.  Should I allow that?

	select case (name)
		case ("bool")
			type = bool_type

		case ("f32")
			type = f32_type
		case ("f64")
			type = f64_type

		case ("i32")
			type = i32_type
		case ("i64")
			type = i64_type

		case ("str")
			type = str_type

		case default

			! TODO: this should be able to use %exists instead of %search,
			! possible minor perf boost
			call structs%search(name, struct_id, io, struct)
			!print *, "struct search io = ", io

			if (io == 0) then
				type = struct_type
				!print *, "struct num vars = ", struct%num_vars
			else
				type = unknown_type
			end if

	end select
	!print *, 'lookup_type = ', type

end function lookup_type

!===============================================================================

module integer function get_keyword_kind(text) result(kind)

	character(len = *), intent(in) :: text

	! Here we start to depart from Fortran syntax (true, not .true.)
	select case (text)

		case ("true")
			kind = true_keyword

		case ("false")
			kind = false_keyword

		case ("not")
			kind = not_keyword

		case ("and")
			kind = and_keyword

		case ("or")
			kind = or_keyword

		case ("let")
			kind = let_keyword

		case ("if")
			kind = if_keyword

		case ("else")
			kind = else_keyword

		case ("for")
			kind = for_keyword

		case ("in")
			kind = in_keyword

		case ("while")
			kind = while_keyword

		case ("fn")
			kind = fn_keyword

		case ("struct")
			kind = struct_keyword

		case ("include")
			kind = include_keyword

		case ("return")
			kind = return_keyword

		case ("break")
			kind = break_keyword

		case ("continue")
			kind = continue_keyword

		case ("use")
			kind = use_keyword

		case ("const")
			kind = const_keyword

		case default
			kind = identifier_token

	end select

	!print *, 'get_keyword_kind = ', kind

end function get_keyword_kind

!===============================================================================

module logical function is_keyword(text) result(is_kw)

	! Returns true if text is a keyword

	character(len = *), intent(in) :: text

	is_kw = get_keyword_kind(text) /= identifier_token

end function is_keyword

!===============================================================================

module logical function is_identifier_or_keyword(kind)

	! Check if a token kind is an identifier or a keyword. This is used when
	! parsing module names in `use` statements, where keywords like `struct` can
	! be used as module names or path segments (e.g., `use struct;`, `use path/struct;`)
	!
	! IMPORTANT: This array must be kept in sync with get_keyword_kind(). If you
	! add a new keyword to get_keyword_kind(), add it here as well.

	integer, intent(in) :: kind

	is_identifier_or_keyword = kind == identifier_token .or. any(kind == [ &
		true_keyword, false_keyword, not_keyword, and_keyword, or_keyword, &
		let_keyword, if_keyword, else_keyword, for_keyword, in_keyword, &
		while_keyword, fn_keyword, struct_keyword, include_keyword, &
		return_keyword, break_keyword, continue_keyword, use_keyword &
	])

end function is_identifier_or_keyword

!===============================================================================

module logical function is_assignment_op(op)

	! Is the operator some type of assignment operator, either regular or
	! compound (augmented)?

	integer, intent(in) :: op

	is_assignment_op = any(op == [equals_token, plus_equals_token, &
		minus_equals_token, star_equals_token, slash_equals_token, &
		sstar_equals_token, percent_equals_token, &
		amp_equals_token, pipe_equals_token, caret_equals_token, &
		lless_equals_token, ggreater_equals_token])

end function is_assignment_op

!===============================================================================

module logical function is_binary_op_allowed(left, op, right, left_arr, right_arr) &
		result(allowed)

	! Is an operation allowed with the types of operator op and left/right
	! operands?

	integer, intent(in) :: left, op, right
	integer, intent(in), optional :: left_arr, right_arr

	!print *, 'left, right = ', left, right

	!! This dynamic variable typing can be useful for testing
	!allowed = .true.
	!return

	allowed = .false.

	if (left == unknown_type .or. right == unknown_type) then
		! Stop cascading errors
		allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, plus_equals_token)
			! the + operator works on numbers and strings

			if (left == array_type .and. right == array_type) then

				! Would recursion help for arrays here?  It seems like it
				! wouldn't reduce very many LOC

				! TODO: should vec str + scalar str be allowed?

				allowed = &
					(is_num_type(left_arr) .and. is_num_type(right_arr))

			else if (left == array_type) then
				allowed = &
					(is_num_type(left_arr) .and. is_num_type(right))

			else if (right == array_type) then
				allowed = &
					(is_num_type(left) .and. is_num_type(right_arr))

			else
				allowed = &
					(is_num_type(left) .and. is_num_type(right)) .or. &
					(left == str_type  .and. right == str_type)

			end if

		case (minus_token, star_token, sstar_token, slash_token, &
			minus_equals_token, star_equals_token, slash_equals_token, &
			sstar_equals_token, percent_token, percent_equals_token, &
			greater_token, less_token, greater_equals_token, &
			less_equals_token)
			! these operators work on numbers but not strings

			if (left == array_type .and. right == array_type) then
				allowed = is_num_type(left_arr) .and. is_num_type(right_arr)
			else if (left == array_type) then
				allowed = is_num_type(left_arr) .and. is_num_type(right)
			else if (right == array_type) then
				allowed = is_num_type(left) .and. is_num_type(right_arr)
			else
				allowed = is_num_type(left) .and. is_num_type(right)
			end if

		case ( &
				lless_token, ggreater_token, &
				lless_equals_token, ggreater_equals_token)

			! Bitwise shift operators work on any combination of ints

			if (left == array_type .and. right == array_type) then
				allowed = is_int_type(left_arr) .and. is_int_type(right_arr)
			else if (left == array_type) then
				allowed = is_int_type(left_arr) .and. is_int_type(right)
			else if (right == array_type) then
				allowed = is_int_type(left) .and. is_int_type(right_arr)
			else
				allowed = is_int_type(left) .and. is_int_type(right)
			end if

		case ( &
				caret_token, pipe_token, amp_token, &
				caret_equals_token, pipe_equals_token, amp_equals_token)

			! Other bitwise binary operators (besides shift) only work on ints
			! of matching sizes (both 32 or 64 bit)

			if (left == array_type .and. right == array_type) then
				allowed = is_int_type(left_arr) .and. left_arr == right_arr
			else if (left == array_type) then
				allowed = is_int_type(left_arr) .and. left_arr == right
			else if (right == array_type) then
				allowed = is_int_type(left) .and. left == right_arr
			else
				allowed = is_int_type(left) .and. left == right
			end if

		case (and_keyword, or_keyword)

			if (left == array_type .and. right == array_type) then
				allowed = left_arr == bool_type .and. right_arr == bool_type

			else if (left  == array_type) then
				allowed = left_arr == bool_type .and. right == bool_type

			else if (right == array_type) then
				allowed = left == bool_type .and. right_arr == bool_type

			else
				allowed = left == bool_type .and. right == bool_type

			end if

		case (equals_token)

			! `array = scalar` is allowed but `scalar = array` is not

			if (left == array_type) then
				allowed = &
					(is_float_type(left_arr) .and. is_float_type(right)) .or. &
					(is_int_type  (left_arr) .and. is_int_type  (right)) .or. &
					(left_arr == right) .or. (left == right)

			else
				allowed = &
					(is_float_type(left) .and. is_float_type(right)) .or. &
					(is_int_type  (left) .and. is_int_type  (right)) .or. &
					(left == right)

			end if

		case (eequals_token, bang_equals_token)

			if (left == file_type .or. right == file_type) then
				allowed = .false.
				return
			end if

			! Allow and then implement comparisons on mixed float types? Might
			! be a bad idea like float to int equality, noted below

			if (left == array_type .and. right == array_type) then
				allowed = &
					(is_int_type(left_arr) .and. is_int_type(right_arr)) .or. &
					(left_arr == right_arr)

			else if (left  == array_type) then
				allowed = &
					(is_int_type(left_arr) .and. is_int_type(right)) .or. &
					(left_arr == right)

			else if (right == array_type) then
				allowed = &
					(is_int_type(left) .and. is_int_type(right_arr)) .or. &
					(left == right_arr)

			else

				! Fortran allows comparing ints and floats for strict equality, e.g.
				! 1 == 1.0 is indeed true.  I'm not sure if I like that
				allowed = &
					(is_int_type(left) .and. is_int_type(right)) .or. &
					(left == right)

			end if

		case (matmul_token)
			! matmul requires both operands to be arrays of numeric type
			if (left == array_type .and. right == array_type) then
				allowed = is_num_type(left_arr) .and. is_num_type(right_arr)
			else
				allowed = .false.
			end if

	end select

end function is_binary_op_allowed

!===============================================================================

module logical function is_unary_op_allowed(op, right, right_arr)

	! Is a unary operation allowed with kinds operator op and right operand?

	integer, intent(in) :: op, right, right_arr

	is_unary_op_allowed = .false.

	if (right == unknown_type) then
		! Stop cascading errors
		is_unary_op_allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, minus_token)
			if (right == array_type) then
				is_unary_op_allowed = is_num_type(right_arr)
			else
				is_unary_op_allowed = is_num_type(right)
			end if

		case (bang_token)
			if (right == array_type) then
				is_unary_op_allowed = is_int_type(right_arr)
			else
				is_unary_op_allowed = is_int_type(right)
			end if

		case (not_keyword)
			if (right == array_type) then
				is_unary_op_allowed = right_arr == bool_type
			else
				is_unary_op_allowed = right == bool_type
			end if

	end select

end function is_unary_op_allowed

!===============================================================================

module integer function get_unary_op_prec(kind) result(prec)

	! Get unary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (plus_token, minus_token, not_keyword, bang_token)
			! arithmetic +, arithmetic -, logical not, bitwise not
			prec = 12

		case default
			prec = 0

	end select

end function get_unary_op_prec

!===============================================================================

module integer function get_binary_op_prec(kind) result(prec)

	! Get binary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		! Syntran operator precedence is closest to rust:
		!
		!     https://doc.rust-lang.org/reference/expressions.html
		!
		! The exception is that ordering comparisons <, >, <=, and >= have
		! higher precedence than (in)equality comparisons == and !=.  In rust,
		! all comparisons have the same precedence.
		!
		! This is somewhat similar to C, except for bitwise and `&`, bitwise or
		! `|`, and bitwise xor `^`, which have higher precedence here (and in
		! rust) than comparisons.  The fact that C works this way could be
		! considered a poor design, but it is due to historical reasons
		! predating even C, according to Dennis Ritchie:
		!
		!    http://cm.bell-labs.co/who/dmr/chist.html
		!
		! C (and C++) precedence:
		!
		!    https://en.cppreference.com/w/c/language/operator_precedence
		!
		! Note that here, a higher `prec` int return value means higher
		! precedence, while the C++ ref is the opposite numerically (but the
		! same top to bottom)

		!********

		! FIXME: increment the unary operator precedence in the fn above after
		! increasing the max binary precedence
		case (sstar_token)
			prec = 11

		case (star_token, slash_token, percent_token, matmul_token)
			prec = 10

		case (plus_token, minus_token)
			prec = 9

		case (lless_token, ggreater_token) ! `<<`, `>>`
			prec = 8

		case (amp_token) ! `&`
			prec = 7

		case (caret_token) ! `^`, aka circumflex, hat
			prec = 6

		case (pipe_token) ! `|`
			prec = 5

		case (less_token, less_equals_token, &
				greater_token, greater_equals_token)
			prec = 4

		case (eequals_token, bang_equals_token)
			prec = 3

		case (and_keyword)  ! `and` (logical, not bitwise)
			prec = 2

		case (or_keyword)  ! `or` (logical)
			prec = 1

		case default
			prec = 0

	end select
	!print *, "prec = ", prec

end function get_binary_op_prec

!===============================================================================

module logical function is_num_type(type)

	integer, intent(in) :: type

	is_num_type = any(type == [i32_type, i64_type, f32_type, f64_type])

end function is_num_type

!===============================================================================

module logical function is_int_type(type)

	integer, intent(in) :: type

	is_int_type = any(type == [i32_type, i64_type])

end function is_int_type

!===============================================================================

module logical function is_float_type(type)

	integer, intent(in) :: type

	is_float_type = any(type == [f32_type, f64_type])

end function is_float_type

!===============================================================================

recursive module integer function get_binary_op_kind( &
		left, op, right, &
		left_arr, right_arr &
		) &
		result(kind_)

	! Return the resulting type yielded by operator op on operands left and
	! right

	integer, intent(in) :: left, op, right
	integer, intent(in) :: left_arr, right_arr

	! Propagate unknown_type to prevent cascading errors (matches
	! is_binary_op_allowed behavior)
	if (left == unknown_type .or. right == unknown_type) then
		kind_ = unknown_type
		return
	end if

	select case (op)
	case ( &
			eequals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)
		!print *, 'bool_type'

		! Comparison operations can take 2 numbers, but always return a bool of
		! some rank

		if (left == array_type .or. right == array_type) then
			kind_ = bool_array_type
		else
			kind_ = bool_type
		end if

	case (lless_token, ggreater_token)
		! Bitwise shifts return the left operand's type for scalars
		if (left == array_type .or. right == array_type) then

			! This logic could be refactored, could probably lose an indentation
			! level
			if (left == array_type) then
				! Left array, right scalar or array
				if (left_arr == i32_type) then
					kind_ = i32_array_type
				else
					kind_ = i64_array_type
				end if
			else
				! Left scalar, right array
				if (left == i32_type) then
					kind_ = i32_array_type
				else
					kind_ = i64_array_type
				end if
			end if

		else
			! All scalars
			kind_ = left
		end if

	case default
		!print *, 'default'

		! Other operations return the same type as their operands if they match,
		! or cast "up" to the type of the operand with the greatest range or
		! precision
		!
		! FIXME: i64, f64, etc.

		kind_ = unknown_type

		if (left == array_type .and. right == array_type) then
			kind_ = get_binary_op_kind(left_arr, op, right_arr, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else if (left == array_type) then
			kind_ = get_binary_op_kind(left_arr, op, right, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else if (right == array_type) then
			kind_ = get_binary_op_kind(left, op, right_arr, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else
			! Default scalar case (no arrays)

			if (left == right) then
				kind_ = left

			else if (left == f64_type .or. right == f64_type) then
				! int + float casts to float, f32 + f64 casts to f64
				!
				! Order matters compared to next condition branch!
				kind_ = f64_type

			else if (left == f32_type .or. right == f32_type) then
				! int + float casts to float
				kind_ = f32_type

			else if ( &
				(left  == i64_type .and. is_int_type(right)) .or. &
				(right == i64_type .and. is_int_type(left ))) then

				! i32+i64 and i64+i32 cast to i64
				kind_ = i64_type

			end if
		end if
	end select

end function get_binary_op_kind

!===============================================================================

module function scalar_to_array_type(scalar_type_) result(array_type_)

	! Convert a scalar type to its corresponding array type

	integer, intent(in) :: scalar_type_
	integer :: array_type_

	select case (scalar_type_)
	case (bool_type)
		array_type_ = bool_array_type

	case (f32_type)
		array_type_ = f32_array_type

	case (f64_type)
		array_type_ = f64_array_type

	case (i32_type)
		array_type_ = i32_array_type

	case (i64_type)
		array_type_ = i64_array_type

	case (str_type)
		array_type_ = str_array_type

	! TODO: file_type?

	case default
		array_type_ = unknown_type

	end select

end function scalar_to_array_type

!===============================================================================

module function array_to_scalar_type(array_type_) result(scalar_type_)

	! Convert an array type to its corresponding scalar type

	integer, intent(in) :: array_type_
	integer :: scalar_type_

	select case (array_type_)
	case (bool_array_type)
		scalar_type_ = bool_type

	case (f32_array_type)
		scalar_type_ = f32_type

	case (f64_array_type)
		scalar_type_ = f64_type

	case (i32_array_type)
		scalar_type_ = i32_type

	case (i64_array_type)
		scalar_type_ = i64_type

	case (str_array_type)
		scalar_type_ = str_type

	! TODO: file_type?

	case default
		scalar_type_ = unknown_type

	end select

end function array_to_scalar_type

!===============================================================================

module function type_name(a) result(str_)
	! c.f. lookup_type() which is mostly the inverse of this
	type(value_t), intent(in) :: a
	character(len = :), allocatable :: str_, array_name

	if (a%type == struct_type) then
		str_ = a%struct_name
	else if (a%type == array_type) then

		if (a%array%type == struct_type) then
			array_name = a%struct_name
		else
			array_name = type_name_primitive(a%array%type)
		end if

		str_ = "["//array_name//"; "

		! Repeat ":, " appropriately
		str_ = str_//repeat(":, ", max(a%array%rank - 1, 0))
		str_ = str_//":]"

	else
		str_ = type_name_primitive(a%type)
	end if

end function type_name

!===============================================================================

module function type_name_primitive(itype) result(str_)
	! c.f. lookup_type() which is mostly the inverse of this
	integer, intent(in) :: itype
	character(len = :), allocatable :: str_

	select case (itype)
	case (i32_type)
		str_ = "i32"
	case (i64_type)
		str_ = "i64"
	case (f32_type)
		str_ = "f32"
	case (f64_type)
		str_ = "f64"
	case (str_type)
		str_ = "str"
	case (bool_type)
		str_ = "bool"
	case (any_type)
		str_ = "any"
	case (void_type)
		str_ = "void"
	case default
		str_ = "unknown"
	end select

end function type_name_primitive

!===============================================================================

module integer function types_match(a, b) result(io)

	! Check if the type of value `a` matches value `b`. Arguments are not
	! transitive!  If `a` is of value any_type, enforcement is less strict.
	!
	! Numeric casting, e.g. i32 to f32, is not allowed.  Maybe we could add a
	! flag if some callers need to allow casting

	type(value_t), intent(in) :: a, b

	!****************

	io = TYPE_MATCH

	! Note that is_binary_op_allowed() specifically allows operations on unknown
	! types.  Maybe we should allow unknowns here too.  Test cases such as
	! instantiating a struct with a fn that hasn't been defined above

	if (.not. (a%type == any_type .or. a%type == b%type)) then
		! Top-level type mismatch (e.g. f32 vs str)
		io = TYPE_MISMATCH
		return
	end if

	if (a%type == struct_type) then
		if (struct_kind_mismatch(a, b)) then
			! Both are structs but different kinds of structs
			io = TYPE_STRUCT_MISMATCH
			return
		end if
	end if

	if (a%type == array_type) then

		if (.not. (a%array%type == any_type .or. a%array%type == b%array%type)) then
			! Both arrays but with different types of elements
			io = TYPE_ARRAY_MISMATCH
			return
		end if

		if (.not. (a%array%rank < 0 .or. a%array%rank == b%array%rank)) then
			! Both arrays but with different ranks (e.g. vector vs matrix)
			io = TYPE_RANK_MISMATCH
			return
		end if

		if (a%array%type == struct_type) then
			if (struct_kind_mismatch(a, b)) then
				! Both are arrays of structs but different kinds of structs
				io = TYPE_ARRAY_STRUCT_MISMATCH
				return
			end if
		end if

	end if

end function types_match

!===============================================================================

logical function struct_kind_mismatch(a, b) result(mismatch)

	! Check whether two struct (or struct-array) values are different kinds of
	! structs.  Prefer the alias-independent struct_cookie (set once at struct
	! declaration time from "<defining src file>::<local struct name>") so the
	! same struct reached via two different module aliases/import paths is
	! still recognized as the same type.  Fall back to struct_name (which can
	! be re-qualified per import path, c.f. qualify_value_struct_name()) if
	! either side lacks a cookie

	type(value_t), intent(in) :: a, b

	if (allocated(a%struct_cookie) .and. allocated(b%struct_cookie)) then
		mismatch = a%struct_cookie /= b%struct_cookie
	else
		mismatch = a%struct_name /= b%struct_name
	end if

end function struct_kind_mismatch

!===============================================================================

module integer function matmul_out_rank(lrank, rrank) result(out_rank)

	! Compute the output rank of a matmul/@-operator expression given the
	! ranks of its operands:
	!   (2,2) -> 2   matrix @ matrix -> matrix
	!   (2,1) -> 1   matrix @ vector -> vector
	!   (1,2) -> 1   vector @ matrix -> vector
	!   (1,1) -> 0   vector @ vector -> scalar (dot product)

	integer, intent(in) :: lrank, rrank

	if (lrank == 1 .and. rrank == 1) then
		out_rank = 0
	else if (lrank == 2 .and. rrank == 2) then
		out_rank = 2
	else
		out_rank = 1
	end if

end function matmul_out_rank

!===============================================================================

end submodule syntran__types_ops

!===============================================================================

