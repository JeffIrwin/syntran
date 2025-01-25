
!===============================================================================

submodule (syntran__parse_m) syntran__parse_fn

	use syntran__intr_fns_m

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

recursive module function parse_fn_call(parser) result(fn_call)

	class(parser_t) :: parser

	type(syntax_node_t) :: fn_call

	!********

	character(len = :), allocatable :: exp_type, act_type, param_name

	integer :: i, io, id_index, pos0, rank

	logical :: has_rank, param_is_ref, arg_is_ref

	type(fn_t) :: fn

	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref

	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: identifier, comma, lparen, rparen, dummy, amp

	type(text_span_t) :: span

	type(value_t) :: param_val

	!print *, ''
	!print *, 'parse_fn_call'

	! Function call expression
	identifier = parser%match(identifier_token)

	!print *, "identifier = ", identifier%text

	args     = new_syntax_node_vector()
	pos_args = new_integer_vector()
	is_ref   = new_logical_vector()

	lparen  = parser%match(lparen_token)

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		call pos_args%push(parser%current_pos())

		arg_is_ref = .false.
		if (parser%current_kind() == amp_token) then
			amp = parser%match(amp_token)
			arg_is_ref = .true.
		end if
		call is_ref%push(arg_is_ref)

		arg = parser%parse_expr()

		! Check that arg expr is name_expr.  Maybe it can be extended later to
		! subscript exprs, but for now only names work
		if (arg_is_ref .and. arg%kind /= name_expr) then
			! This also catches dot exprs, but not subscripted name exprs
			span = new_span(amp%pos, parser%current_pos() - amp%pos + 1)
			call parser%diagnostics%push(err_non_name_ref( &
				parser%context(), &
				span &
			))
		end if

		if (arg_is_ref .and. allocated(arg%lsubscripts)) then
			span = new_span(amp%pos, parser%current_pos() - amp%pos + 1)
			call parser%diagnostics%push(err_sub_ref( &
				parser%context(), &
				span &
			))
		end if

		call args%push(arg)

		if (parser%current_kind() /= rparen_token) then
			comma = parser%match(comma_token)
		end if

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do
	call pos_args%push(parser%current_pos() + 1)
	!print *, "args%len_ = ", args%len_

	rparen  = parser%match(rparen_token)

	fn_call%kind = fn_call_expr
	fn_call%identifier = identifier

	call resolve_overload(args, fn_call, has_rank)
	if (has_rank) rank = fn_call%val%array%rank

	! Lookup by fn_call%identifier%text (e.g. "0min_i32"), but log
	! diagnostics based on identifier%text (e.g. "min")
	!
	! Might need to add separate internal/external fn names for
	! overloaded cases
	fn = parser%fns%search(fn_call%identifier%text, id_index, io)
	!print *, "fn id_index = ", id_index
	if (io /= exit_success) then ! .and. parser%ipass > 0) then

		!if (parser%ipass > 0) stop
		if (parser%ipass == 0) then
			fn_call%id_index = 0
			fn_call%kind = fn_call_expr
			return
		end if

		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_undeclare_fn(parser%context(), &
			span, identifier%text))

		! No more tokens are consumed below, so we can just return
		! to skip cascading fn arg count/type errors
		return

	end if

	!print *, "fn id_index = ", id_index
	!print *, "is_intr = ", fn%is_intr

	if (fn%is_intr) fn_call%kind = fn_call_intr_expr

	fn_call%val = fn%type
	if (has_rank) then
		! The line above overwrites the rank for overloaded intrinsics like
		! i32() and i64().  TODO: cover a low-res version of logo.syntran in a
		! unit test
		if (.not. allocated(fn_call%val%array)) allocate(fn_call%val%array)
		fn_call%val%array%rank = rank

		! Not sure if these 2 lines are required. Maybe not since it should only
		! apply to intrinsics fns, but it might be safer to copy anyway
		if (.not. allocated(fn%type%array)) allocate(fn%type%array)
		fn%type%array%rank = rank

	end if
	!print *, "rank = ", fn_call%val%array%rank

	! Intrinsic fns don't have a syntax node: they are implemented
	! in Fortran, not syntran

	!if (allocated(fn%node)) then
	if (.not. fn%is_intr) then

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

		fn_call%num_locs = fn%node%num_locs

		!print *, 'fn params size = ', size(fn%params)
		!print *, 'fn call params size = ', size(fn_call%params)

	end if

	! TODO: does fn need to be a syntax node member?  I think we can
	! just look it up later by identifier/id_index like we do for
	! variable value
	!fn_call%fn = fn

	!print *, 'fn params size = ', size(fn%params)
	!print *, 'fn param names size = ', size(fn%param_names%v)
	if (fn%variadic_min < 0 .and. size(fn%params) /= args%len_ &
		.and. parser%ipass > 0) then

		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_bad_arg_count(parser%context(), &
			span, identifier%text, size(fn%params), args%len_))
		return

	else if (args%len_ < size(fn%params) + fn%variadic_min &
		.and. parser%ipass > 0) then

		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_too_few_args(parser%context(), &
			span, identifier%text, &
			size(fn%params) + fn%variadic_min, args%len_))
		return

	end if

	fn_call%is_ref = is_ref%v(1: is_ref%len_)

	allocate(param_val%array)
	do i = 1, args%len_
		if (parser%ipass == 0) cycle

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
			param_val  = fn%params(i)
			param_name = fn%param_names%v(i)%s
		else
			param_val%type = fn%variadic_type

			if (size(fn%params) > 0) then
				param_name = fn%param_names%v( size(fn%params) )%s
			else
				param_name = ""
			end if
		end if

		param_is_ref = .false.
		if (allocated(fn%node)) param_is_ref = fn%node%is_ref(i)

		if (param_is_ref .neqv. is_ref%v(i)) then

			! The "param" is in the decl, the "arg" is in the call
			span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
			if (param_is_ref) then
				call parser%diagnostics%push(err_bad_arg_val( &
					parser%context(), &
					span, &
					identifier%text, &
					i - 1, &  ! 0-based index in err msg
					param_name &
				))
			else
				call parser%diagnostics%push(err_bad_arg_ref( &
					parser%context(), &
					span, &
					identifier%text, &
					i - 1, &
					param_name &
				))
			end if

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
				param_name, &
				exp_type, &
				act_type))

		end if
	end do

	fn_call%id_index = id_index

	call syntax_nodes_copy(fn_call%args, args%v( 1: args%len_ ))

	!print *, 'done parsing fn_call'

end function parse_fn_call

!===============================================================================

module function parse_fn_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: i, io, pos0, rank, fn_beg, fn_name_end

	logical :: overwrite

	type(fn_t) :: fn

	type( string_vector_t) :: names
	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref

	type(syntax_node_t) :: body
	type(syntax_token_t) :: fn_kw, identifier, lparen, rparen, colon, &
		name, comma, dummy, amp

	type(text_span_t) :: span

	type(value_t) :: type
	type(value_vector_t) :: types

	! Like a for statement, a fn declaration has its own scope (for its
	! parameters).  Its block body will have yet another scope
	call parser%vars%push_scope()
	call parser%locs%push_scope()
	parser%is_loc = .true.
	parser%num_locs = 0

	parser%returned = .false.
	fn_beg = parser%peek_pos(0)
	fn_kw = parser%match(fn_keyword)

	identifier = parser%match(identifier_token)
	fn_name_end = parser%peek_pos(0) - 1
	fn%is_intr = .false.

	!print *, "parsing fn ", identifier%text

	! TODO: be careful with parser%pos (token index) vs parser%current_pos()
	! (character index) when constructing a span.  I probably have similar bugs
	! throughout to the one that I just fixed here

	!print *, 'matching lparen'

	! This is a bit of a hack, but parser can crash otherwise if parens are
	! missing.  Should parens be optional for fns without params?
	if (parser%current_kind() /= lparen_token) return

	lparen = parser%match(lparen_token)

	! Parse parameter names and types.  Save in temp vectors initially
	names    = new_string_vector()
	pos_args = new_integer_vector()  ! technically params not args
	is_ref   = new_logical_vector()
	types    = new_value_vector()

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

	! Parse fn parameters (arguments)
	i = 0
	do while ( &
			parser%current_kind() /= rparen_token .and. &
			parser%current_kind() /= eof_token)
		i = i + 1

		pos0 = parser%current_pos()
		call pos_args%push(pos0)

		!print *, 'matching name'
		name  = parser%match(identifier_token)
		!print *, 'matching colon'
		colon = parser%match(colon_token)

		! Should this be part of parse_type()?  I think not, as refs can appear
		! in fn decls but not struct decls.  Fn calls do not even call
		! parse_type, they call parse_expr instead
		if (parser%current_kind() == amp_token) then
			amp = parser%match(amp_token)
			call is_ref%push(.true.)
		else
			call is_ref%push(.false.)
		end if

		call parser%parse_type(type_text, type)

		call names%push( name%text )
		call types%push(type)

		if (parser%current_kind() /= rparen_token) then
			!print *, 'matching comma'
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do
	call pos_args%push(parser%current_pos() + 1)

	!print *, "names len = ", names%len_
	!print *, 'matching rparen'
	rparen = parser%match(rparen_token)

	! Now that we have the number of params, save them

	allocate(fn  %params     ( names%len_ ))
	allocate(fn%param_names%v( names%len_ ))
	allocate(decl%params     ( names%len_ ))
	allocate(decl%is_ref     ( names%len_ ))

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		fn%param_names%v(i)%s = names%v(i)%s

		! Copy a value_t object to store the type
		fn%params(i) = types%v(i)

		! Declare the local parameter variable
		parser%num_locs = parser%num_locs + 1

		! Save parameters by id_index
		decl%params(i) = parser%num_locs
		decl%is_ref(i) = is_ref%v(i)

		call parser%locs%insert(fn%param_names%v(i)%s, fn%params(i), parser%num_locs)

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
		call parser%parse_type(type_text, type)

		! TODO: ban &references as return types

		fn%type = type
	end if
	!print *, 'fn%type = ', fn%type

	! Copy for later return type checking while parsing body
	parser%fn_name = identifier%text
	parser%fn_type = fn%type

	body = parser%parse_statement()

	if (.not. parser%returned) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	end if

	! Reset to allow the global scope to return anything
	parser%fn_type%type = any_type

	! Insert fn into parser%fns

	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	allocate(decl%body)

	decl%kind = fn_declaration

	decl%identifier = identifier
	decl%num_locs   = parser%num_locs
	decl%body       = body
	!print *, "decl num_locs = ", decl%num_locs

	call parser%vars%pop_scope()
	call parser%locs%pop_scope()

	!deallocate(dict%dicts(i)%root)
	!if (allocated(parser%locs%dicts(1)%root)) deallocate(parser%locs%dicts(1)%root)

	!print *, "popping is_loc = false"
	parser%is_loc = .false.  ! no nested fn declarations

	allocate(fn%node)
	fn%node = decl

	!overwrite = .false.
	!if (parser%ipass == 0) overwrite = .true.
	overwrite = .true.
	if (parser%ipass == 0) overwrite = .false.

	io = 0
	call parser%fns%insert(identifier%text, fn, decl%id_index, io, overwrite = overwrite)
	!print *, "fn insert io = ", io
	if (parser%ipass == 0) call parser%fn_names%push( identifier%text )

	! error if fn already declared. be careful in future if fn prototypes are
	! added
	if (io /= 0) then ! .and. parser%ipass == 0) then
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

	integer :: itype, i, io, pos0

	logical :: overwrite

	type(struct_t) :: struct, dummy_struct

	type(syntax_token_t) :: identifier, comma, lbrace, rbrace, dummy, &
		colon, name, struct_kw

	type(text_span_t) :: span

	type( string_vector_t) :: names
	type(integer_vector_t) :: pos_mems

	type(value_t) :: member  ! local type meta-data
	type(value_t) :: type
	type(value_vector_t) :: types

	struct_kw = parser%match(struct_keyword)

	identifier = parser%match(identifier_token)
	!print *, "parsing struct ", identifier%text

	itype = lookup_type(identifier%text, parser%structs, dummy_struct)
	!print *, "itype = ", itype, kind_name(itype)
	if (itype /= unknown_type .and. itype /= struct_type .and. parser%ipass == 0) then
		! Redeclared structs are caught below
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_primitive( &
			parser%context(), &
			span, &
			identifier%text))
	end if

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
	names  = new_string_vector()
	types = new_value_vector()

	! For diagnostic text spans
	pos_mems = new_integer_vector()

	i = 0
	do while ( &
			parser%current_kind() /= rbrace_token .and. &
			parser%current_kind() /= eof_token)
		i = i + 1

		pos0 = parser%current_pos()

		name  = parser%match(identifier_token)
		call pos_mems%push( name%pos )
		colon = parser%match(colon_token)

		call parser%parse_type(type_text, type)
		!print *, "type = ", type_text

		call types%push(type)
		call names%push( name%text )

		if (parser%current_kind() /= rbrace_token) then
			! Delimiting commas are required; trailing comma is optional
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do

	!print *, 'matching rbrace'
	rbrace = parser%match(rbrace_token)
	call pos_mems%push( rbrace%pos )

	! Now that we have the number of members, save them

	struct%num_vars = 0
	allocate(struct%member_names%v( names%len_ ))
	allocate(struct%vars%dicts(1))

	do i = 1, names%len_
		!print *, "name = ", names%v(i)%s

		struct%member_names%v(i)%s = names%v(i)%s

		! Copy a value_t object to store the type
		member = types%v(i)

		! Declare the member
		!parser%num_vars = parser%num_vars + 1
		struct%num_vars = struct%num_vars + 1
		!print *, "struct%num_vars = ", struct%num_vars

		!! Save parameters by id_index
		!decl%params(i) = parser%num_vars

		! Each struct has its own dict of members.  Create one and insert the
		! member name into that dict instead of the (global) vars dict here.
		! Might not need a new type, could probably just re-use the `vars_t`
		! type, just like `parser%vars`.  Just add one inside of the `struct_t`
		! type.

		call struct%vars%insert(names%v(i)%s, member, &
			struct%num_vars, io, overwrite = .false.)
		!print *, 'io = ', io
		if (io /= exit_success .and. parser%ipass == 0) then
			span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
			call parser%diagnostics%push(err_redeclare_mem( &
				parser%context(), &
				span, &
				names%v(i)%s))
		end if

		if (allocated(member%array)) deallocate(member%array)
	end do

	! Insert struct into parser dict

	parser%num_structs = parser%num_structs + 1
	decl%id_index  = parser%num_structs

	overwrite = .false.
	if (parser%ipass > 0) overwrite = .true.

	!print *, "inserting identifier ", identifier%text, " into parser structs"
	call parser%structs%insert( &
		identifier%text, struct, decl%id_index, io, overwrite = overwrite)
	!print *, "io = ", io
	if (io /= 0 .and. parser%ipass == 0) then
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

recursive module function parse_struct_instance(parser) result(inst)

	! A struct instantiator initializes all the members of an instance of a
	! struct

	class(parser_t) :: parser

	type(syntax_node_t) :: inst

	!********

	character(len = :), allocatable :: unset_name, exp_type, act_type

	integer :: io, pos0, pos1, struct_id, member_id, id1(1), num_mems

	logical :: is_ok
	logical, allocatable :: member_set(:)

	type(struct_t) :: struct

	type(syntax_node_t) :: mem

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

	call parser%structs%search(identifier%text, struct_id, io, struct)
	!print *, "struct io = ", io

	num_mems = 0

	lbrace  = parser%match(lbrace_token)

	inst%kind = struct_instance_expr
	!inst%identifier = identifier

	! Save everything in the inst syntax node
	inst%val%type = struct_type
	allocate(inst%val%struct( struct%num_vars ))
	allocate(inst%members   ( struct%num_vars ))

	member_set = spread(.false., 1, struct%num_vars)

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

		num_mems = num_mems + 1

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
		unset_name = struct%member_names%v(id1(1))%s
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
	!print *, "size = ", num_mems
	if (num_mems < struct%num_vars) then
		! I think this is unreachable given the other checks.  TODO: don't do
		! anything, just let the diag get thrown
		write(*,*) err_prefix//"struct instance does not have enough members"//color_reset
		call internal_error()
	!else if (num_mems > struct%num_vars) then
	!	write(*,*) err_prefix//"struct instance has too many members"//color_reset
	!	call internal_error()
	end if

	!print *, "ending parse_struct_instance()"

end function parse_struct_instance

!===============================================================================

module subroutine parse_type(parser, type_text, type)

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	type(value_t), intent(out) :: type

	!********

	integer :: rank, itype
	integer :: pos0, pos1, pos2

	type(struct_t) :: struct

	type(syntax_token_t) :: colon, ident, comma, lbracket, rbracket, semi, dummy

	type(text_span_t) :: span

	pos1 = parser%current_pos()
	if (parser%current_kind() == lbracket_token) then

		! Array param
		lbracket = parser%match(lbracket_token)
		ident    = parser%match(identifier_token)
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
		ident = parser%match(identifier_token)
		rank = -1
	end if
	pos2 = parser%current_pos()

	type_text = ident%text

	itype = lookup_type(type_text, parser%structs, struct)

	if (itype == unknown_type) then
		span = new_span(pos1, pos2 - pos1)
		call parser%diagnostics%push(err_bad_type( &
			parser%context(), span, type_text))
	end if

	if (rank >= 0) then
		type%type = array_type
		allocate(type%array)
		type%array%rank = rank
		type%array%type = itype
	else
		type%type = itype
		!if (allocated(type%array)) deallocate(type%array)
	end if

	if (itype == struct_type) type%struct_name = type_text

end subroutine parse_type

!===============================================================================

end submodule syntran__parse_fn

!===============================================================================

