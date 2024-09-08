
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

	character(len = :), allocatable :: param_type, arg_type, exp_type, act_type

	integer :: i, io, id_index, param_rank, arg_rank, pos0, type_

	type(fn_t) :: fn

	type(integer_vector_t) :: ranks, pos_args

	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: identifier, comma, lparen, rparen, dummy

	type(text_span_t) :: span

	type(value_t) :: param_val

	if (debug > 1) print *, 'parse_fn_call'

	! Function call expression
	identifier = parser%match(identifier_token)

	!print *, 'identifier = ', identifier%text

	args = new_syntax_node_vector()
	pos_args = new_integer_vector()
	lparen  = parser%match(lparen_token)

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		call pos_args%push(parser%current_pos())
		arg = parser%parse_expr()
		call args%push(arg)

		if (parser%current_kind() /= rparen_token) then
			comma = parser%match(comma_token)
		end if

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do
	call pos_args%push(parser%current_pos() + 1)

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

	fn_call%val = fn%type
	!fn_call%val%type = fn%type
	!if (fn%type == array_type) then
	!	if (.not. allocated(fn_call%val%array)) allocate(fn_call%val%array)
	!	fn_call%val%array%type = fn%array_type

	!	! i32_arr_fn returns same rank as arg (not -1)
	!	if (fn%rank > 0) fn_call%val%array%rank = fn%rank

	!end if

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

	allocate(param_val%array)
	do i = 1, args%len_

		! For variadic fns, check the argument type against the type
		! of the last required parameter.  This may need to change,
		! e.g. writeln(file) should write a blank line to a file,
		! but writeln(file, string1, string2), where string* is not
		! the same type as file?

		! TODO: re-test min/max arg count/type checking

		! Construct a param val just for type checking.  I think this is the
		! only way to do it for intrinsic fns, which don't actually have a val
		! anywhere
		if (i <= size(fn%params)) then
			param_val%type = fn%params(i)%type
			param_val%array%type  = fn%params(i)%array_type
			param_val%array%rank  = fn%params(i)%rank
			param_val%struct_name = fn%params(i)%struct_name
		else
			param_val%type = fn%variadic_type
			param_val%array%type = unknown_type
			param_val%array%rank = 0
			param_val%struct_name = ""
		end if

		if (types_match(param_val, args%v(i)%val) /= TYPE_MATCH) then

			exp_type = type_name(param_val)
			act_type = type_name(args%v(i)%val)

			! This used to call a different diagnostic fn depending on whether
			! it was a top-level type mismatch, array mismatch, or rank
			! mismatch.  types_match() returns an enum so we could make it that
			! way again if there's a need. Currently err_bad_arg_rank() is
			! unused

			span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
			call parser%diagnostics%push(err_bad_arg_type( &
				parser%context(), &
				span, &
				identifier%text, &
				i - 1, &  ! 0-based index in err msg
				fn%params(i)%name, &
				exp_type, &
				act_type))

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

	integer :: i, j, io, pos0, pos1, pos2, rank, itype, fn_beg, fn_name_end

	type(fn_t) :: fn

	type( string_vector_t) :: names, types
	type(logical_vector_t) :: is_array
	type(integer_vector_t) :: ranks, pos_args

	type(struct_t) :: struct

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
	pos_args = new_integer_vector()  ! technically params not args

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
		call pos_args%push(pos0)

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
	call pos_args%push(parser%current_pos() + 1)

	!print *, 'matching rparen'
	rparen = parser%match(rparen_token)
	pos2 = parser%current_pos()

	! Now that we have the number of params, save them

	allocate(fn  %params( names%len_ ))
	allocate(decl%params( names%len_ ))

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		fn%params(i)%name = names%v(i)%s

		itype = lookup_type(types%v(i)%s, parser%structs, struct)
		!print *, "itype = ", itype
		if (itype == unknown_type) then
			span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, types%v(i)%s))
		end if

		if (itype == struct_type) then
			!print *, "struct_type"
			!print *, "struct num vars = ", struct%num_vars
			!print *, "struct name = ", types%v(i)%s

			!! members are allocated here, vars%vals are not. probably ok, maybe
			!! need a deep copy if the vars dict is really needed
			!print *, "allocated = ", allocated(struct%members)
			!print *, "allocated = ", allocated(struct%vars%vals)

			val%struct_name = types%v(i)%s
			allocate(val%struct( struct%num_vars ))
			!allocate(val%members( struct%num_vars ))
			!val = struct
			do j = 1, struct%num_vars
				!val%struct(j) = struct%members(j)%val
				val%struct(j)%type = struct%members(j)%type
				!val%struct(j) = struct%vars%vals(j)

				!inst%val%struct( member_id ) = mem%val

				! TODO: test a fn with a 2nd-order struct arg (i.e. a struct
				! made up of other structs).  Maybe more data needs to be copied
				! here, especially struct_name.  Essentially every %type should
				! be bundled along with a %struct_name as in
				! parse_struct_declaration()

			end do

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

		if (itype == struct_type) then
			fn%params(i)%struct_name = val%struct_name
			!print *, "struct_name = ", val%struct_name
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

	fn%type%type = void_type
	rank = 0
	if (parser%current_kind() == colon_token) then

		colon = parser%match(colon_token)

		pos1 = parser%current_pos()
		call parser%parse_type(type_text, rank)
		pos2 = parser%current_pos()

		itype = lookup_type(type_text, parser%structs, struct)

		if (itype == unknown_type) then
			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, type_text))
				!parser%contexts%v(parser%current_unit()), span, type_text))
		end if

		if (rank >= 0) then
			fn%type%type = array_type
			allocate(fn%type%array)
			fn%type%array%rank = rank
			fn%type%array%type = itype
		else
			fn%type%type = itype
		end if
		!fn%struct_name = type_text
		fn%type%struct_name = type_text

	end if
	!print *, 'fn%type = ', fn%type

	! Copy for later return type checking while parsing body
	parser%fn_name = identifier%text
	parser%fn_type = fn%type
	!!parser%fn_type = fn%type
	!parser%fn_type%type = fn%type
	!parser%fn_type%struct_name = fn%struct_name
	!if (rank >= 0) then
	!	!parser%fn_rank = fn%rank
	!	!parser%fn_array_type = fn%array_type
	!	parser%fn_type%array%rank = fn%rank
	!	parser%fn_type%array%type = fn%array_type
	!end if

	body = parser%parse_statement()

	if (.not. parser%returned) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	end if

	! Reset to allow the global scope to return anything
	parser%fn_type%type = any_type
	!parser%fn_type = any_type

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

	call parser%fns%insert(identifier%text, fn, decl%id_index, io)
	!print *, "fn insert io = ", io

	! error if fn already declared. be careful in future if fn prototypes are
	! added
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_redeclare_fn(parser%context(), &
			span, identifier%text))
	end if

	!print *, 'size(decl%params) = ', size(decl%params)
	!print *, 'decl%params = ', decl%params

end function parse_fn_declaration

!===============================================================================

module function parse_struct_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: itype, i, io, pos0, pos1, pos2, rank

	!type(struct_t), save :: struct
	type(struct_t) :: struct, dummy_struct

	type(syntax_token_t) :: identifier, comma, lbrace, rbrace, dummy, &
		colon, name, struct_kw

	type(text_span_t) :: span

	type( string_vector_t) :: names, types
	type(logical_vector_t) :: is_array
	type(integer_vector_t) :: ranks, pos_mems

	type(value_t) :: val

	!call parser%vars%push_scope()

	struct_kw = parser%match(struct_keyword)

	identifier = parser%match(identifier_token)
	!print *, "parsing struct ", identifier%text

	itype = lookup_type(identifier%text, parser%structs, dummy_struct)
	!print *, "itype = ", itype, kind_name(itype)
	if (itype /= unknown_type .and. itype /= struct_type) then
		! Redeclared structs are caught below
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_primitive( &
			parser%context(), &
			span, &
			identifier%text))
	end if

	pos1 = parser%current_pos()

	lbrace = parser%match(lbrace_token)

	! Structs use this syntax:
	!
	!     // declaration
	!     struct Time
	!     {
	!     	hh: i32,
	!     	mm: i32,
	!     	ss: f32,
	!     }
	!
	!     // instance
	!     let t1 = Time{hh = 9, mm = 20, ss = 0.030,};
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

	struct%num_vars = 0
	if (allocated(struct%members)) deallocate(struct%members)
	allocate(struct%members( names%len_ ))
	!allocate(decl  %params( names%len_ ))  ! if this is needed, we need a new
	!! name.  "members" already means the member statements of a block statement

	!allocate(struct%vars)

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		struct%members(i)%name = names%v(i)%s

		! TODO: consume dummy_struct for nested structs
		itype = lookup_type(types%v(i)%s, parser%structs, dummy_struct)
		if (itype == unknown_type) then
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

		! Declare the member
		!parser%num_vars = parser%num_vars + 1
		struct%num_vars = struct%num_vars + 1
		!print *, "struct%num_vars = ", struct%num_vars

		!! Save parameters by id_index
		!decl%params(i) = parser%num_vars

		! Create a value_t object to store the type
		val%type = struct%members(i)%type
		val%struct_name = types%v(i)%s
		if (is_array%v(i)) then
			if (allocated(val%array)) deallocate(val%array)
			allocate(val%array)
			val%array%type = struct%members(i)%array_type
			val%array%rank = struct%members(i)%rank
			!print *, "rank = ", val%array%rank
		end if

		! Each struct has its own dict of members.  Create one and insert the
		! member name into that dict instead of the (global) vars dict here.
		! Might not need a new type, could probably just re-use the `vars_t`
		! type, just like `parser%vars`.  Just add one inside of the `struct_t`
		! type.

		!print *, "insert var type ", kind_name(val%type)
		!print *, "insert var name = ", struct%members(i)%name
		!call parser%vars%insert(struct%members(i)%name, val, parser%num_vars)
		!call struct%vars%insert(struct%members(i)%name, val, struct%num_vars)

		call struct%vars%insert(struct%members(i)%name, val, &
			struct%num_vars, io, overwrite = .false.)
		!print *, 'io = ', io
		if (io /= exit_success) then
			span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
			call parser%diagnostics%push(err_redeclare_mem( &
				parser%context(), &
				span, &
				struct%members(i)%name))
		end if

	end do

	! Insert struct into parser dict

	parser%num_structs = parser%num_structs + 1
	decl%id_index  = parser%num_structs

	!print *, "inserting identifier ", identifier%text, " into parser structs"
	call parser%structs%insert( &
		identifier%text, struct, decl%id_index, io, overwrite = .false.)
	!print *, "io = ", io
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_struct( &
			parser%context(), &
			span, &
			identifier%text))
	end if

	!print *, "parser structs root     = ", parser%structs%dict%root%split_char
	!print *, "parser structs root mid = ", parser%structs%dict%root%mid%split_char
	!call ternary_tree_final(struct%vars%dicts(1)%root)

	decl%kind = struct_declaration

	!print *, "done parsing struct"

end function parse_struct_declaration

!===============================================================================

module function parse_struct_instance(parser) result(inst)

	! A struct instantiator initializes all the members of an instance of a
	! struct

	class(parser_t) :: parser

	type(syntax_node_t) :: inst

	!********

	character(len = :), allocatable :: unset_name, exp_type, act_type

	integer :: io, pos0, pos1, struct_id, member_id, id1(1)

	logical :: is_ok
	logical, allocatable :: member_set(:)

	type(struct_t) :: struct

	type(syntax_node_t) :: mem
	type(syntax_node_vector_t) :: mems

	type(syntax_token_t) :: identifier, name, equals, comma, lbrace, rbrace, dummy

	type(text_span_t) :: span

	type(value_t) :: member

	!print *, "starting parse_struct_instance()"

	identifier = parser%match(identifier_token)

	!print *, "parsing struct instance of identifier = ", identifier%text

	!print *, ""
	!print *, "in parse_struct_instance():"
	!print *, "parser structs root     = ", parser%structs%dict%root%split_char
	!print *, "parser structs root mid = ", parser%structs%dict%root%mid%split_char

	!struct = parser%structs%search(identifier%text, struct_id, io)
	call parser%structs%search(identifier%text, struct_id, io, struct)
	!print *, "struct io = ", io

	! TODO: do we need `mems`? Or just inst%members
	mems = new_syntax_node_vector()

	lbrace  = parser%match(lbrace_token)

	inst%kind = struct_instance_expr
	!inst%identifier = identifier

	! Save everything in the inst syntax node
	inst%val%type = struct_type
	allocate(inst%val%struct( struct%num_vars ))
	allocate(inst%members   ( struct%num_vars ))

	member_set = spread(.false., 1, struct%num_vars)

	!if (allocated(inst%struct)) deallocate(inst%struct)
	!allocate(inst%struct)
	!inst%struct = struct
	inst%struct_name = identifier%text

	inst%val%struct_name = identifier%text

	!print *, "struct name = ", inst%struct_name

	do while ( &
		parser%current_kind() /= rbrace_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		! TODO: allow "anonymous" members where the name (and type) is implied
		! by the order?  This is the way that structs are printed, so unless I
		! change print str conversion is might be nice to allow print output to
		! be pasted back into syntran source code.  Could be dangerous tho

		name   = parser%match(identifier_token)
		equals = parser%match(equals_token)
		pos1   = parser%current_pos()
		mem    = parser%parse_expr()

		!print *, "name%text = ", name%text

		!call struct%vars%insert(struct%members(i)%name, val, &
		!	struct%num_vars, io, overwrite = .false.)

		!print *, "allocated = ", allocated(struct%vars%dicts(1)%root)
		!print *, "char root = ", struct%vars%dicts(1)%root%split_char
		!print *, "char mid  = ", struct%vars%dicts(1)%root%mid%split_char

		call struct%vars%search(name%text, member_id, io, member)
		!print *, "member io = ", io
		!print *, "member id = ", member_id
		is_ok = io == 0
		if (.not. is_ok) then
			span = new_span(name%pos, len(name%text))
			call parser%diagnostics%push(err_bad_member_name_short( &
				parser%context(), &
				span, &
				name%text, &
				identifier%text))
			!return
		end if

		!print *, "member type = ", kind_name(member%type)
		!print *, "mem    type = ", kind_name(mem%val%type)
		if (is_ok) then

		if (types_match(member, mem%val) /= TYPE_MATCH) then

			exp_type = type_name(member)
			act_type = type_name(mem%val)

			!span = new_span(name%pos, parser%current_pos() - name%pos + 1)      ! `mem = expr`
			span = new_span(pos1, parser%current_pos() - pos1) ! just `expr`
			call parser%diagnostics%push(err_bad_member_type( &
				parser%context(), &
				span, &
				name%text, &
				identifier%text, &
				act_type, &
				exp_type))

		end if
		end if
		!print *, "mem type = ", kind_name(mem%val%type)

		! member_id may be out-of-bounds.  Probably want to parse the rest of
		! the tokens in this loop but not try any indexing by member_id if not
		! ok

		if (is_ok) then

			if (member_set(member_id)) then
				span = new_span(name%pos, len(name%text))
				call parser%diagnostics%push(err_reset_member( &
					parser%context(), &
					span, &
					name%text, &
					identifier%text))
			end if

			! Members can be instantiated out of order.  Insert by id, not loop iterator
			inst%val%struct( member_id ) = mem%val
			inst%members( member_id ) = mem
			member_set(member_id) = .true.

		end if

		call mems%push(mem)

		if (parser%current_kind() /= rbrace_token) then
			comma = parser%match(comma_token)
		end if

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	rbrace  = parser%match(rbrace_token)

	! Use a boolean array to check if all members are set.  You could have the
	! correct number but with duplicates and other members missing
	!print *, "member_set = ", member_set
	if (.not. all(member_set)) then

		! There could be more than 1 unset member but we only log diag for the
		! 1st one
		id1 = findlocl1(member_set, .false.)
		unset_name = struct%members(id1(1))%name
		!print *, "id1 = ", id1
		!print *, "name = ", unset_name

		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_unset_member( &
			parser%context(), &
			span, &
			unset_name, &
			identifier%text))
	end if

	!print *, "size = ", struct%num_vars
	!print *, "size = ", mems%len_
	if (mems%len_ < struct%num_vars) then
		! I think this is unreachable given the other checks
		write(*,*) err_prefix//"struct instance does not have enough members"//color_reset
		call internal_error()
	!else if (mems%len_ > struct%num_vars) then
	!	write(*,*) err_prefix//"struct instance has too many members"//color_reset
	!	call internal_error()
	end if

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

