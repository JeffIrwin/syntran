
!===============================================================================

submodule (syntran__parse_m) syntran__parse_fn

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

module function parse_fn_call(parser) result(fn_call)

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
		case (f32_type)
			fn_call%identifier%text = "0min_f32"
		case default
			fn_call%identifier%text = "0min_i32"
		end select

	case ("max")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (i64_type)
			fn_call%identifier%text = "0max_i64"
		case (f32_type)
			fn_call%identifier%text = "0max_f32"
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

	case ("i64")

		type_ = i64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)
			!print *, "resolving 0i64_arr"
			fn_call%identifier%text = "0i64_arr"

			if (args%len_ >= 1) then
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case default
			!print *, "resolving 0i64_sca"
			fn_call%identifier%text = "0i64_sca"
		end select

	case ("sum")

		type_ = i32_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0sum_f32"
		case (i64_type)
			fn_call%identifier%text = "0sum_i64"
		case default
			fn_call%identifier%text = "0sum_i32"
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
	!if (associated(fn%node)) then
	if (allocated(fn%node)) then
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

!===============================================================================

module function parse_fn_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: i, pos0, pos1, pos2, rank, itype, fn_beg, fn_name_end

	type(fn_t) :: fn

	type( string_vector_t) :: names, types
	type(logical_vector_t) :: is_array
	type(integer_vector_t) :: ranks

	type(syntax_node_t) :: body
	type(syntax_token_t) :: fn_kw, identifier, lparen, rparen, colon, &
		name, comma, dummy

	type(text_span_t) :: span

	type(value_t) :: val

	! Like a for statement, a fn declaration has its own scope (for its
	! parameters).  Its block body will have yet another scope
	call parser%vars%push_scope()

	parser%returned = .false.
	fn_beg = parser%peek_pos(0)
	fn_kw = parser%match(fn_keyword)

	identifier = parser%match(identifier_token)
	fn_name_end = parser%peek_pos(0) - 1

	!print *, "parsing fn ", identifier%text

	! TODO: be careful with parser%pos (token index) vs parser%current_pos()
	! (character index) when constructing a span.  I probably have similar bugs
	! throughout to the one that I just fixed here
	pos1 = parser%current_pos()

	!print *, 'matching lparen'
	lparen = parser%match(lparen_token)

	! Parse parameter names and types.  Save in temp string vectors initially
	names    = new_string_vector()
	types    = new_string_vector()
	is_array = new_logical_vector()
	ranks    = new_integer_vector()

	! Array params use this syntax:
	!
	!     fn sum_fn(v: [i32; :]): i32
	!     {
	!         let s = 0;
	!         for i in [0: size(v, 0)]
	!             s = s + v[i];
	!         s;
	!     }
	!
	!     fn mat_fn(a: [i32; :,:]): i32
	!     {
	!         // do something with a[i,j]
	!     }

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%current_pos()

		!print *, 'matching name'
		name  = parser%match(identifier_token)
		!print *, 'matching colon'
		colon = parser%match(colon_token)

		call parser%parse_type(type_text, rank)

		call names%push( name%text )
		call types%push( type_text )
		call ranks%push( rank      )

		! This array is technically redundant but helps readability?
		call is_array%push( rank >= 0 )

		if (parser%current_kind() /= rparen_token) then
			!print *, 'matching comma'
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do

	!print *, 'matching rparen'
	rparen = parser%match(rparen_token)
	pos2 = parser%current_pos()

	! Now that we have the number of params, save them

	allocate(fn  %params( names%len_ ))
	allocate(decl%params( names%len_ ))

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		fn%params(i)%name = names%v(i)%s

		itype = lookup_type( types%v(i)%s )
		if (itype == unknown_type) then

			! TODO: make an array of pos's for each param to underline
			! individual param, not whole param list.  Struct parser does this
			! slightly better

			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, types%v(i)%s))
				!parser%contexts%v(name%unit_), span, types%v(i)%s))

		end if

		if (is_array%v(i)) then
			fn%params(i)%type = array_type
			fn%params(i)%array_type = itype
			fn%params(i)%rank = ranks%v(i)
			!print *, "rank = ", fn%params(i)%rank
		else
			fn%params(i)%type = itype
			!print *, "(scalar)"
		end if

		! Declare the parameter variable
		parser%num_vars = parser%num_vars + 1

		! Save parameters by id_index.  TODO: stack frames
		decl%params(i) = parser%num_vars

		! Create a value_t object to store the type
		val%type = fn%params(i)%type
		if (is_array%v(i)) then
			if (allocated(val%array)) deallocate(val%array)
			allocate(val%array)
			val%array%type = fn%params(i)%array_type
			val%array%rank = fn%params(i)%rank
			!print *, "rank = ", val%array%rank
		end if

		!print *, "insert var type ", kind_name(val%type)
		call parser%vars%insert(fn%params(i)%name, val, parser%num_vars)

	end do

	! Parse fn return type

	! Rust uses "->" as a delimiter between the fn and its return type.  Here
	! I choose ":" instead as it seems more consistent, at least for normal
	! non-assignable fns.  There is some discussion on the Rust reasoning here:
	!
	!     https://stackoverflow.com/questions/35018919/whats-the-origin-of-in-rust-function-definition-return-types
	!

	fn%type = void_type
	rank = 0
	if (parser%current_kind() == colon_token) then

		colon = parser%match(colon_token)

		pos1 = parser%current_pos()
		call parser%parse_type(type_text, rank)
		pos2 = parser%current_pos()

		itype = lookup_type(type_text)

		if (itype == unknown_type) then
			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, type_text))
				!parser%contexts%v(parser%current_unit()), span, type_text))
		end if

		if (rank >= 0) then
			fn%type = array_type
			fn%rank = rank
			fn%array_type = itype
		else
			fn%type = itype
		end if

	end if
	!print *, 'fn%type = ', fn%type

	! Copy for later return type checking while parsing body
	parser%fn_name = identifier%text
	parser%fn_type = fn%type
	if (rank >= 0) then
		parser%fn_rank = fn%rank
		parser%fn_array_type = fn%array_type
	end if

	body = parser%parse_statement()

	if (.not. parser%returned) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	end if

	! Reset to allow the global scope to return anything
	parser%fn_type = any_type

	! Insert fn into parser%fns

	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	allocate(decl%body)

	decl%kind = fn_declaration

	decl%identifier = identifier
	decl%body       = body

	call parser%vars%pop_scope()

	allocate(fn%node)
	fn%node = decl

	call parser%fns%insert(identifier%text, fn, decl%id_index)
	! TODO: error if fn already declared. be careful in future if fn prototypes
	! are added

	!print *, 'size(decl%params) = ', size(decl%params)
	!print *, 'decl%params = ', decl%params

end function parse_fn_declaration

!===============================================================================

module function parse_struct_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: itype, i, pos0, pos1, pos2, rank

	type(struct_t) :: struct

	type(syntax_token_t) :: identifier, comma, lbrace, rbrace, dummy, &
		colon, name, struct_kw

	type(text_span_t) :: span

	type( string_vector_t) :: names, types
	type(logical_vector_t) :: is_array
	type(integer_vector_t) :: ranks, pos_mems

	type(value_t) :: val

	!! TODO?
	!call parser%vars%push_scope()

	struct_kw = parser%match(struct_keyword)

	identifier = parser%match(identifier_token)
	print *, "parsing struct ", identifier%text

	pos1 = parser%current_pos()

	lbrace = parser%match(lbrace_token)

	! Structs use this syntax:
	!
	!     struct time
	!     {
	!     	hh: i32,
	!     	mm: i32,
	!     	ss: f32,
	!     }
	!     let t1 = time{hh = 9, mm = 20, ss = 0.030,};
	!     t1.hh = 10;
	!
	! A struct declaration is a lot like a fn declaration.  Instead of a list of
	! fn parameters, we have a list of struct members.  Unlike a fn declaration,
	! there is no "body" for a struct, only members.

	! Parse member names and types.  Save in temp vectors initially
	names    = new_string_vector()
	types    = new_string_vector()
	is_array = new_logical_vector()
	ranks    = new_integer_vector()

	! For diagnostic text spans
	pos_mems = new_integer_vector()

	do while ( &
		parser%current_kind() /= rbrace_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%current_pos()

		!print *, 'matching name'
		name  = parser%match(identifier_token)
		!print *, "name = ", name%text
		call pos_mems%push( name%pos )
		!print *, 'matching colon'
		colon = parser%match(colon_token)

		call parser%parse_type(type_text, rank)
		!print *, "type = ", type_text

		call names%push( name%text )
		call types%push( type_text )
		call ranks%push( rank      )

		! This array is technically redundant but helps readability?
		call is_array%push( rank >= 0 )

		if (parser%current_kind() /= rbrace_token) then
			!print *, 'matching comma'
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do

	!print *, 'matching rbrace'
	rbrace = parser%match(rbrace_token)
	call pos_mems%push( rbrace%pos )
	pos2 = parser%current_pos() - 1

	! Now that we have the number of members, save them

	allocate(struct%members( names%len_ ))
	!allocate(decl  %params( names%len_ ))  ! if this is needed, we need a new
	!! name.  "members" already means the member statements of a block statement

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		struct%members(i)%name = names%v(i)%s

		itype = lookup_type( types%v(i)%s )
		if (itype == unknown_type) then

			!span = new_span(pos1, pos2 - pos1 - 1)
			!span = new_span(lbrace%pos, rbrace%pos - lbrace%pos + 1)
			span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, types%v(i)%s))

		end if

		if (is_array%v(i)) then
			struct%members(i)%type = array_type
			struct%members(i)%array_type = itype
			struct%members(i)%rank = ranks%v(i)
			!print *, "rank = ", struct%members(i)%rank
		else
			struct%members(i)%type = itype
			!print *, "(scalar)"
		end if

		! Declare the parameter variable
		parser%num_vars = parser%num_vars + 1

		!! Save parameters by id_index
		!decl%params(i) = parser%num_vars

		! Create a value_t object to store the type
		val%type = struct%members(i)%type
		if (is_array%v(i)) then
			if (allocated(val%array)) deallocate(val%array)
			allocate(val%array)
			val%array%type = struct%members(i)%array_type
			val%array%rank = struct%members(i)%rank
			!print *, "rank = ", val%array%rank
		end if

		!print *, "insert var type ", kind_name(val%type)
		call parser%vars%insert(struct%members(i)%name, val, parser%num_vars)

	end do

	! TODO: insert struct into a new dict type and save its members somewhere
	!call parser%structs%insert(identifier%text, fn, decl%id_index)

	decl%kind = struct_declaration

	!print *, "done parsing struct"

end function parse_struct_declaration

!===============================================================================

module function parse_struct_instance(parser) result(instance)

	! A struct instantiator initializes all the members of an instance of a
	! struct

	class(parser_t) :: parser

	type(syntax_node_t) :: instance

	!********

	integer :: pos0

	type(syntax_node_t) :: mem
	type(syntax_node_vector_t) :: mems

	type(syntax_token_t) :: identifier, name, equals, comma, lbrace, rbrace, dummy

	!print *, "starting parse_struct_instance()"

	identifier = parser%match(identifier_token)

	!print *, 'identifier = ', identifier%text

	mems = new_syntax_node_vector()
	lbrace  = parser%match(lbrace_token)

	do while ( &
		parser%current_kind() /= rbrace_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		name   = parser%match(identifier_token)
		equals = parser%match(equals_token)
		mem    = parser%parse_expr()

		call mems%push(mem)

		if (parser%current_kind() /= rbrace_token) then
			comma = parser%match(comma_token)
		end if

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	rbrace  = parser%match(rbrace_token)

	instance%kind = struct_instance_expr
	!instance%identifier = identifier

	! TODO: save everything in the instance syntax node

	! TODO: check number and type of members match

	!print *, "ending parse_struct_instance()"

end function parse_struct_instance

!===============================================================================

module subroutine parse_type(parser, type_text, rank)

	! TODO: encapsulate out-args in struct if adding any more

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	integer, intent(out) :: rank

	!********

	integer :: pos0

	type(syntax_token_t) :: colon, type, comma, lbracket, rbracket, semi, dummy

	if (parser%current_kind() == lbracket_token) then

		! Array param
		lbracket = parser%match(lbracket_token)
		type     = parser%match(identifier_token)
		semi     = parser%match(semicolon_token)

		rank  = 0
		do while ( &
			parser%current_kind() /= rbracket_token .and. &
			parser%current_kind() /= eof_token)

			pos0 = parser%pos

			rank = rank + 1
			colon = parser%match(colon_token)
			if (parser%current_kind() /= rbracket_token) then
				comma = parser%match(comma_token)
			end if

			! break infinite loop
			if (parser%pos == pos0) dummy = parser%next()

		end do
		!print *, 'rank = ', rank

		rbracket = parser%match(rbracket_token)

	else
		! Scalar param
		type = parser%match(identifier_token)
		rank = -1
	end if

	type_text = type%text

end subroutine parse_type

!===============================================================================

end submodule syntran__parse_fn

!===============================================================================

