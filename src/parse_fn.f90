
!===============================================================================

submodule (syntran__parser_m) syntran__parse_fn

	implicit none

contains

function parse_fn_call(parser) result(fn_call)

	class(parser_t) :: parser

	type(syntax_node_t) :: fn_call

	!********

	character(len = :), allocatable :: param_type, arg_type
	integer :: i, io, id_index, param_rank, arg_rank, ptype, atype, pos0, type_
	logical :: types_match

	type(fn_t) :: fn
	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: identifier, comma, lparen, rparen, dummy
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_fn_call'

	! Function call expression
	identifier = parser%match(identifier_token)

	!print *, 'identifier = ', identifier%text

	args = new_syntax_node_vector()
	lparen  = parser%match(lparen_token)

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		arg = parser%parse_expr()
		call args%push(arg)

		!! TODO: we need a delete method for syntax_node_t (i.e.
		!! arg).  There was a bug here where the fact that the
		!! subscripts for the 1st fn arg were allocated, leaked into
		!! the 2nd arg because of this loop.  For example:
		!!
		!!     let result = my_fn_call(str1[beg:end], str2);
		!!
		!! We should delete the whole thing just to be safe, to
		!! prevent anything else from leaking.

		!if (allocated(arg%lsubscripts)) then
		!	deallocate(arg%lsubscripts)
		!end if

		if (parser%current_kind() /= rparen_token) then
			comma = parser%match(comma_token)
		end if

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	rparen  = parser%match(rparen_token)

	fn_call%kind = fn_call_expr
	fn_call%identifier = identifier

	! Resolve special overloaded intrinsic fns
	select case (identifier%text)
	case ("min")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (i64_type)
			fn_call%identifier%text = "0min_i64"
		case default
			fn_call%identifier%text = "0min_i32"
		end select

	case ("max")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (i64_type)
			fn_call%identifier%text = "0max_i64"
		case default
			fn_call%identifier%text = "0max_i32"
		end select

	case ("i32")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)
			!print *, "resolving 0i32_arr"
			fn_call%identifier%text = "0i32_arr"

			if (args%len_ >= 1) then
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case default
			fn_call%identifier%text = "0i32_sca"
		end select

	end select

	! Lookup by fn_call%identifier%text (e.g. "0min_i32"), but log
	! diagnostics based on identifier%text (e.g. "min")
	!
	! Might need to add separate internal/external fn names for
	! overloaded cases
	fn = parser%fns%search(fn_call%identifier%text, id_index, io)
	if (io /= exit_success) then

		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_undeclare_fn(parser%context(), &
			span, identifier%text))

		! No more tokens are consumed below, so we can just return
		! to skip cascading fn arg count/type errors
		return

	end if

	fn_call%val%type = fn%type
	if (fn%type == array_type) then
		if (.not. allocated(fn_call%val%array)) allocate(fn_call%val%array)
		fn_call%val%array%type = fn%array_type

		! i32_arr_fn returns same rank as arg (not -1)
		if (fn%rank > 0) fn_call%val%array%rank = fn%rank

	end if

	! Intrinsic fns don't have a syntax node: they are implemented
	! in Fortran, not syntran
	if (associated(fn%node)) then
		!print *, 'assigning fn node'

		! If I understand my own code, this is inlining:  every fn
		! call gets its own copy of the fn body.  This expansion
		! happens at parse time, not eval time, so fn calls in
		! a loop will all share one body

		! TODO: could we do this with a pointer instead? I think
		! copying is a waste of memory.  Also try to encapsulate
		! both body and params into a wrapped type (fn_t?)

		allocate(fn_call%body)
		fn_call%body = fn%node%body
		fn_call%params = fn%node%params

	end if

	! TODO: does fn need to be a syntax node member?  I think we can
	! just look it up later by identifier/id_index like we do for
	! variable value
	!fn_call%fn = fn

	!print *, 'fn params size = ', size(fn%params)
	if (fn%variadic_min < 0 .and. size(fn%params) /= args%len_) then

		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_bad_arg_count(parser%context(), &
			span, identifier%text, size(fn%params), args%len_))
		return

	else if (args%len_ < size(fn%params) + fn%variadic_min) then

		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_too_few_args(parser%context(), &
			span, identifier%text, &
			size(fn%params) + fn%variadic_min, args%len_))
		return

	end if

	do i = 1, args%len_
		!print *, kind_name(args%v(i)%val%type)
		!print *, kind_name(fn%params(i)%type)

		! For variadic fns, check the argument type against the type
		! of the last required parameter.  This may need to change,
		! e.g. writeln(file) should write a blank line to a file,
		! but writeln(file, string1, string2), where string* is not
		! the same type as file?

		! TODO: re-test min/max arg count/type checking

		!! We want println() to just print an empty line
		!if (fn%variadic_min == 0) exit

		if (i <= size(fn%params)) then
			ptype = fn%params(i)%type
		else
			ptype = fn%variadic_type
		end if

		!j = i
		!if (fn%variadic_min > 0) j = fn%variadic_min
		!ptype = fn%params(j)%type

		types_match = &
			ptype == any_type .or. ptype == args%v(i)%val%type

		!! make a fn for use here and for array `atype` below? this
		!! could be more easily extended if i add fn's with something
		!! generic like `int_type` or `num_type`
		!types_match = .false.
		!select case (ptype)
		!case (any_type)
		!	types_match = .true.
		!case default
		!	types_match = ptype == args%v(i)%val%type
		!end select

		if (.not. types_match) then

			! TODO: get span of individual arg, not whole arg list
			span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
			call parser%diagnostics%push( &
				err_bad_arg_type(parser%context(), &
				span, identifier%text, i, fn%params(i)%name, &
				kind_name(ptype), &
				kind_name(args%v(i)%val%type)))
			return

		end if

		! TODO: fns w/ variadic array params are not implemented
		if (fn%variadic_min >= 0 .and. i > size(fn%params)) cycle

		if (ptype == array_type) then
			atype = fn%params(i)%array_type
			types_match = &
				atype == any_type .or. &
				atype == args%v(i)%val%array%type
		end if

		if (.not. types_match) then

			span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
			param_type = kind_name( atype)
			arg_type   = kind_name(args%v(i)%val%array%type)

			call parser%diagnostics%push( &
				err_bad_array_arg_type(parser%context(), &
				span, identifier%text, i, fn%params(i)%name, &
				param_type, arg_type))
			return

		end if

		if (ptype == array_type) then
			param_rank = fn%params(i)%rank
			arg_rank = args%v(i)%val%array%rank

			if (param_rank >= 0 .and. param_rank /= arg_rank) then

				span = new_span(lparen%pos, &
					rparen%pos - lparen%pos + 1)

				call parser%diagnostics%push( &
					err_bad_arg_rank(parser%context(), &
					span, identifier%text, i, fn%params(i)%name, &
					param_rank, arg_rank))
				return

			end if

		end if

	end do

	fn_call%id_index = id_index

	call syntax_nodes_copy(fn_call%args, &
		args%v( 1: args%len_ ))

	!print *, 'done parsing fn_call'

end function parse_fn_call

end submodule syntran__parse_fn

!===============================================================================

