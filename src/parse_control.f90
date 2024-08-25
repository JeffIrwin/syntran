
!===============================================================================

submodule (syntran__parse_m) syntran__parse_control

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

module function parse_return_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: right_beg, right_end, exp_type, act_type, exp_rank, act_rank

	logical :: types_match

	type(syntax_token_t) :: return_token, semi
	type(text_span_t) :: span

	right_beg = parser%peek_pos(0)
	return_token = parser%match(return_keyword)
	parser%returned = .true.

	statement%kind = return_statement

	allocate(statement%right)

	if (parser%current_kind() == semicolon_token) then
		! Void return statement

		!! already matched below
		!semi = parser%match(semicolon_token)

		!right_end = parser%peek_pos(0) - 1
		right_end = parser%peek_pos(0)

		statement%right%val%type = void_type

	else
		! expr or statement?
		right_beg = parser%peek_pos(0)
		statement%right = parser%parse_expr()
		right_end = parser%peek_pos(0) - 1

	end if
	semi = parser%match(semicolon_token)

	act_type = statement%right%val%type
	if (act_type == struct_type) then
		print *, "return struct type"
	end if
	if (statement%right%kind == dot_expr) then
		print *, "return dot expr"
		!if (expr%right%kind == dot_expr) then
		!rtype = expr%right%val%struct(expr%right%right%id_index)%type
		act_type = statement%right%val%struct( statement%right%right%id_index )%type
	end if

	! Check return type (unless we're at global level ifn == 1).  That's half
	! the point of return statements
	!
	! There should also be a check that every branch of a fn has a return
	! statement, but that seems more difficult
	types_match = &
		parser%fn_type == any_type .or. parser%fn_type == act_type

	if (.not. types_match) then
		span = new_span(right_beg, right_end - right_beg + 1)
		call parser%diagnostics%push( &
			err_bad_ret_type(parser%context(), &
			span, parser%fn_name, &
			kind_name(parser%fn_type), &
			kind_name(act_type)))

		return
	end if

	! Check array type and rank if needed
	if (parser%fn_type == array_type) then
		exp_type = parser%fn_array_type
		act_type = statement%right%val%array%type
		types_match = exp_type == any_type .or. exp_type == act_type
	end if

	if (.not. types_match) then
		span = new_span(right_beg, right_end - right_beg + 1)
		call parser%diagnostics%push( &
			err_bad_array_ret_type(parser%context(), &
			span, parser%fn_name, &
			kind_name(exp_type), kind_name(act_type)))

		return
	end if

	if (parser%fn_type == array_type) then
		exp_rank = parser%fn_rank
		act_rank = statement%right%val%array%rank

		if (exp_rank >= 0 .and. exp_rank /= act_rank) then
			span = new_span(right_beg, right_end - right_beg + 1)
			call parser%diagnostics%push( &
				err_bad_ret_rank(parser%context(), &
				span, parser%fn_name, &
				exp_rank, act_rank))

			return
		end if
	end if

end function parse_return_statement

!===============================================================================

module function parse_if_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end

	type(syntax_node_t)  :: condition, if_clause, else_clause
	type(syntax_token_t) :: if_token, else_token
	type(text_span_t) :: span

	!print *, 'parse_if_statement'

	if_token  = parser%match(if_keyword)

	cond_beg  = parser%peek_pos(0)
	condition = parser%parse_expr()

	!cond_end  = parser%peek_pos(-1)
	cond_end  = parser%peek_pos(0) - 1

	!print *, 'cond_beg, cond_end = ', cond_beg, cond_end

	! Check that condition type is bool
	if (condition%val%type /= bool_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context(), span, parser%text(cond_beg, cond_end), &
			"if-statement"))
	end if

	if_clause = parser%parse_statement()  ! Immo calls this "then statement"

	allocate(statement%condition, statement%if_clause)

	statement%kind = if_statement
	statement%condition = condition
	statement%if_clause = if_clause

	if (parser%current_kind() == else_keyword) then
		!print *, 'parsing else clause'

		else_token = parser%match(else_keyword)
		else_clause = parser%parse_statement()

		allocate(statement%else_clause)
		statement%else_clause = else_clause

	!else
	!	print *, 'no else clause'
	end if

	! No additional parsing work is required to handle "else if".  That's just
	! an else clause which contains another if statement

	!print *, 'done parse_if_statement'

end function parse_if_statement

!===============================================================================

module function parse_for_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: arr_beg, arr_end

	type(syntax_node_t)  :: array, body
	type(syntax_token_t) :: for_token, in_token, identifier

	type(text_span_t) :: span

	type(value_t) :: dummy

	!  For loop syntax:
	!
	!    for i in [1: 5]
	!       { i; }
	!    // 1, 2, 3, 4 // ubound not inclusive
	!
	!  steps:
	!
	!    for i in [1: 2: 7]
	!    // 1,    3,    5
	!
	!   // And finally, after doing some array handling work, something like:
	!    for i in [1, 2, 4, 5]
	!    // 1, 2,   4, 5  // last elem *is* included
	!
	!  * For steps, rust has `for x in (1..10).step_by(2) {}`, which I hate

	for_token  = parser%match(for_keyword)

	call parser%vars%push_scope()

	identifier = parser%match(identifier_token)

	in_token   = parser%match(in_keyword)

	arr_beg  = parser%peek_pos(0)
	!array      = parser%parse_array_expr()
	array      = parser%parse_primary_expr()
	arr_end  = parser%peek_pos(0) - 1

	parser%num_vars = parser%num_vars + 1
	statement%id_index = parser%num_vars

	! Auto declare loop iterator in for statement (HolyC doesn't let you do
	! that!).  The 'let' keyword is not used:
	!
	!     for i in [lower, upper]
	!        {}

	! Insert the identifier's type into the dict. This is a local scope, so
	! there's no need to check io

	!print *, 'identifier%text = ', identifier%text
	!print *, 'allocated(array%lbound) = ', allocated(array%lbound)

	!if (allocated(array%lbound)) then
	if (allocated(array%val%array)) then

		!print *, 'array%val%type = ', kind_name(array%val%type) ! "array_type" :(
		!print *, 'array type = ', kind_name(array%val%array%type)

		! Pathological code like `for <EOF>` can crash the parser :(

		! Array iterator type could be i32 or i64, and lbound type might not
		! match ubound type!
		dummy%type = array%val%array%type
		call parser%vars%insert(identifier%text, dummy, &
			statement%id_index)

	else

		dummy%type = array%val%type
		call parser%vars%insert(identifier%text, dummy, &
			statement%id_index)

		! I guess we could allow a 1-loop iteration on a scalar if that's
		! worthwhile.  Eval would need some work
		span = new_span(arr_beg, arr_end - arr_beg + 1)
		call parser%diagnostics%push(err_non_array_loop( &
			parser%context(), span, parser%text(arr_beg, arr_end)))

	end if

	body = parser%parse_statement()

	allocate(statement%array)
	allocate(statement%body)

	statement%kind = for_statement

	statement%identifier = identifier
	statement%array      = array
	statement%body       = body

	call parser%vars%pop_scope()

end function parse_for_statement

!===============================================================================

module function parse_while_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end

	type(syntax_node_t)  :: body, condition
	type(syntax_token_t) :: while_token
	type(text_span_t) :: span

	while_token  = parser%match(while_keyword)

	cond_beg  = parser%peek_pos(0)
	condition = parser%parse_expr()
	cond_end  = parser%peek_pos(0) - 1

	! Check that condition type is bool
	if (condition%val%type /= bool_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context(), span, parser%text(cond_beg, cond_end), &
			"while-loop"))
	end if

	body = parser%parse_statement()

	allocate(statement%condition, statement%body)

	statement%kind = while_statement

	statement%condition = condition
	statement%body      = body

end function parse_while_statement

!===============================================================================

module function parse_block_statement(parser) result(block)

	class(parser_t) :: parser

	type(syntax_node_t) :: block

	!********

	type(syntax_node_vector_t) :: members
	type(syntax_token_t) :: left, right, dummy

	integer :: i, pos0

	members = new_syntax_node_vector()
	i = 0

	left  = parser%match(lbrace_token)

	call parser%vars%push_scope()

	do while ( &
		parser%current_kind() /= eof_token .and. &
		parser%current_kind() /= rbrace_token)

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		call members%push(parser%parse_statement())

		! Avoid infinite loops on malformed blocks like this:
		!   {
		!     4) + 5;
		!   }
		if (parser%pos == pos0) dummy = parser%next()

	end do

	call parser%vars%pop_scope()

	right = parser%match(rbrace_token)

	block%kind = block_statement

	! Convert to standard array
	call syntax_nodes_copy(block%members, members%v( 1: members%len_ ))

end function parse_block_statement

!===============================================================================

module function parse_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	type(syntax_token_t) :: semi

	select case (parser%current_kind())

		case (lbrace_token)
			statement = parser%parse_block_statement()

		case (if_keyword)
			statement = parser%parse_if_statement()

		case (for_keyword)
			statement = parser%parse_for_statement()

		case (while_keyword)
			statement = parser%parse_while_statement()

		case (return_keyword)
			statement = parser%parse_return_statement()

		case default
			statement = parser%parse_expr_statement()
			semi      = parser%match(semicolon_token)

	end select

end function parse_statement

!===============================================================================

end submodule syntran__parse_control

!===============================================================================

