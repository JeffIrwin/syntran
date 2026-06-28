
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

recursive module subroutine parse_fn_call(parser, module_prefix, identifier, fn_call)

	class(parser_t) :: parser
	character(len = *), intent(in), optional :: module_prefix
	type(syntax_token_t), intent(in), optional :: identifier
	type(syntax_node_t), intent(out) :: fn_call

	!********

	character(len = :), allocatable :: exp_type, act_type, param_name, &
		lookup_name, display_name

	integer :: i, io, io_std, id_index, id_index_tmp, pos0, rank, arr_type_result

	logical :: has_rank, has_arr_type, param_is_ref, param_is_const_ref, &
		arg_is_ref, is_ok, is_const_var

	type(fn_t) :: fn

	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref

	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: identifier_, comma, lparen, rparen, dummy, amp

	type(text_span_t) :: span

	type(value_t) :: param_val, const_check_val

	!print *, ''
	!print *, 'parse_fn_call'

	! Function call expression
	if (present(identifier)) then
		identifier_ = identifier
	else
		identifier_ = parser%match(identifier_token)
	end if

	!print *, "identifier_ = ", identifier_%text

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

		call parser%parse_expr(expr=arg)

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
	fn_call%identifier = identifier_

	! Set module prefix if provided (for qualified calls like std::println)
	if (present(module_prefix)) then
		fn_call%module_prefix = module_prefix
	end if

	call resolve_overload(args, fn_call, has_rank, has_arr_type, arr_type_result)
	if (has_rank) rank = fn_call%val%array%rank

	! If any argument has unknown_type, return early to prevent cascading errors.
	! The actual error (e.g. undefined function) was already pushed earlier.
	do i = 1, args%len_
		if (args%v(i)%val%type == unknown_type) then
			fn_call%val%type = unknown_type
			return
		end if
	end do

	! Lookup by fn_call%identifier%text (e.g. "0min_i32"), but log
	! diagnostics based on identifier_%text (e.g. "min")
	!
	! Might need to add separate internal/external fn names for
	! overloaded cases
	!
	! For qualified calls, determine lookup name based on module prefix
	if (present(module_prefix)) then
		if (module_prefix == "std") then
			! For std::, first try std-only functions (registered with "std::" prefix)
			lookup_name = "std::" // fn_call%identifier%text
			fn = parser%fns%search(lookup_name, id_index, io)
			if (io /= exit_success) then
				! Fall back to regular intrinsic lookup without prefix (legacy intrinsics)
				lookup_name = fn_call%identifier%text
				fn = parser%fns%search(lookup_name, id_index, io)
			end if
		else
			! For user modules, look up with qualified name
			lookup_name = module_prefix // "::" // fn_call%identifier%text
			fn = parser%fns%search(lookup_name, id_index, io)
		end if
		display_name = module_prefix // "::" // identifier_%text
	else
		lookup_name = fn_call%identifier%text
		display_name = identifier_%text
		fn = parser%fns%search(lookup_name, id_index, io)
	end if

	!print *, "fn id_index = ", id_index
	if (io /= exit_success) then

		! Check if this is an std-only function that requires the std:: prefix.
		! Do this before the ipass==0 early return so the type is set correctly
		! in both passes, preventing cascading errors.
		if (.not. present(module_prefix)) then
			fn = parser%fns%search("std::" // fn_call%identifier%text, id_index, io_std)
			if (io_std == exit_success) then
				! Set the return type from the found function to prevent
				! cascading errors from the untyped result
				fn_call%val = fn%type
				fn_call%kind = fn_call_intr_expr

				! Only push the error in pass 1
				if (parser%ipass /= 0) then
					span = new_span(identifier_%pos, len(identifier_%text))
					call parser%diagnostics%push( &
						err_std_only_fn(parser%context(), &
						span, identifier_%text))
				end if
				return
			end if
		end if

		if (parser%ipass == 0) then
			fn_call%id_index = 0
			fn_call%kind = fn_call_expr
			return
		end if

		span = new_span(identifier_%pos, len(identifier_%text))
		call parser%diagnostics%push( &
			err_undeclare_fn(parser%context(), &
			span, display_name, &
			parser%fns%closest(display_name)))

		fn_call%val%type = unknown_type

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

		! For functions like std::reshape whose element type depends on their
		! arguments, restore the element type that resolve_overload determined.
		! fn_call%val = fn%type above would otherwise overwrite it with any_type.
		if (has_arr_type) then
			fn_call%val%array%type = arr_type_result
		end if

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

	if (fn%variadic_min < 0 .and. size(fn%params) /= args%len_) then

		! Bad arg count (non-variadic)
		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_bad_arg_count(parser%context(), &
			span, identifier_%text, size(fn%params), args%len_))
		return

	else if (args%len_ < size(fn%params) + fn%variadic_min) then

		! Too few args
		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_too_few_args(parser%context(), &
			span, identifier_%text, &
			size(fn%params) + fn%variadic_min, args%len_))
		return

	else if (fn%variadic_max >= 0 .and. &
		args%len_ > size(fn%params) + fn%variadic_max) then

		! Too many args
		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call parser%diagnostics%push( &
			err_too_many_args(parser%context(), &
			span, identifier_%text, &
			size(fn%params) + fn%variadic_max, args%len_))
		return

	end if

	fn_call%is_ref = is_ref%v(1: is_ref%len_)

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
			param_val  = fn%params(i)
			param_name = fn%param_names%v(i)%s
		else
			param_val%type = fn%variadic_type
			param_name = fn%variadic_name
		end if

		param_is_ref       = .false.
		param_is_const_ref = .false.
		if (allocated(fn%node)) then
			param_is_ref = fn%node%is_ref(i)
			if (allocated(fn%node%is_const_ref)) &
				param_is_const_ref = fn%node%is_const_ref(i)
		end if

		span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
		call check_call_arg(parser, args%v(i), is_ref%v(i), span, &
			identifier_%text, i - 1, param_val, param_name, &
			param_is_ref, param_is_const_ref)

	end do

	fn_call%id_index = id_index

	! Move args from vector (avoids deep copy)
	allocate(fn_call%args(args%len_))
	do i = 1, args%len_
		call syntax_node_move_into(args%v(i), fn_call%args(i))
	end do

	!print *, 'done parsing fn_call'

end subroutine parse_fn_call

!===============================================================================

recursive module subroutine parse_qualified_expr(parser, expr)

	! Parse qualified names like `std::println()`, `mod::name`,
	! or nested namespaces like `math::vectors::fn()`

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: expr

	!********

	character(len = :), allocatable :: module_name, fn_name, lookup_name

	type(syntax_token_t) :: mod_identifier, double_colon, fn_identifier
	type(value_t) :: var_val
	type(text_span_t) :: span
	integer :: id_index, iostat

	! Get first part of module name
	mod_identifier = parser%match(identifier_token)
	module_name = mod_identifier%text

	! Consume ::
	double_colon = parser%match(double_colon_token)

	! Get the next identifier (could be another namespace or the fn/var name)
	fn_identifier = parser%match(identifier_token)
	fn_name = fn_identifier%text

	! Handle nested namespaces: math::vectors::fn()
	! Keep consuming ::identifier pairs while we see more :: tokens
	do while (parser%current_kind() == double_colon_token)
		! The current fn_name is actually part of the module path
		module_name = module_name // "::" // fn_name
		double_colon = parser%match(double_colon_token)
		fn_identifier = parser%match(identifier_token)
		fn_name = fn_identifier%text
	end do

	if (parser%current_kind() == lparen_token) then
		! Qualified function call: std::println(...) or math::vectors::fn(...)
		call parser%parse_fn_call(module_name, fn_identifier, expr)
		if (parser%current_kind() == lbracket_token) then
			call parser%parse_subscripts(expr)
		end if
		if (parser%current_kind() == dot_token .and. &
				expr%val%type == struct_type) then
			call parser%parse_dot(expr)
		end if

	else if (parser%current_kind() == lbrace_token) then
		! Qualified struct instance: mod::Struct{...}
		lookup_name = module_name // "::" // fn_name
		if (parser%structs%exists(lookup_name)) then
			call parser%parse_struct_instance(expr, lookup_name)
		else
			! Struct not found
			span = new_span(fn_identifier%pos, len(fn_identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context(), span, fn_name, &
				parser%vars%closest(fn_name)))
		end if

	else
		! Qualified variable access: mod::var
		lookup_name = module_name // "::" // fn_name
		call parser%vars%search(lookup_name, id_index, iostat, var_val)

		if (iostat /= exit_success) then
			span = new_span(fn_identifier%pos, len(fn_identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context(), span, fn_name, &
				parser%vars%closest(fn_name)))
			return
		end if

		call new_name_expr(fn_identifier, var_val, expr)
		expr%id_index = id_index
		expr%module_prefix = module_name

		call parser%parse_subscripts(expr)
		call parser%parse_dot(expr)
	end if

end subroutine parse_qualified_expr

!===============================================================================

module subroutine parse_fn_declaration(parser, decl)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: i, io, pos0, rank, fn_beg, fn_name_end

	logical :: overwrite, const_param

	type(fn_t) :: fn

	type( string_vector_t) :: names
	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref, is_const_ref

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
	names         = new_string_vector()
	pos_args      = new_integer_vector()  ! technically params not args
	is_ref        = new_logical_vector()
	is_const_ref  = new_logical_vector()
	types         = new_value_vector()

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
			! &const means the callee cannot modify through this reference
			if (parser%current_kind() == const_keyword) then
				dummy = parser%next()
				call is_const_ref%push(.true.)
			else
				call is_const_ref%push(.false.)
			end if
		else
			call is_ref%push(.false.)
			call is_const_ref%push(.false.)
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

	allocate(fn  %params         ( names%len_ ))
	allocate(fn%param_names%v   ( names%len_ ))
	allocate(decl%params        ( names%len_ ))
	allocate(decl%is_ref        ( names%len_ ))
	allocate(decl%is_const_ref  ( names%len_ ))

	do i = 1, names%len_
		!print *, "name, type = ", names%v(i)%s, ", ", types%v(i)%s

		fn%param_names%v(i)%s = names%v(i)%s

		! Copy a value_t object to store the type
		fn%params(i) = types%v(i)

		! Declare the local parameter variable
		parser%num_locs = parser%num_locs + 1

		! Save parameters by id_index
		decl%params(i)       = parser%num_locs
		decl%is_ref(i)       = is_ref%v(i)
		decl%is_const_ref(i) = is_const_ref%v(i)

		! &const params are immutable inside the function body.
		! Use const_param (default-kind logical) to avoid kind(1) mismatch
		const_param = is_const_ref%v(i)
		call parser%locs%insert(fn%param_names%v(i)%s, fn%params(i), parser%num_locs, &
			is_const = const_param)

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

	call parser%parse_statement(body)

	! A void fn has nothing to return, so the lack of any return statement is
	! not an error for it.  Only non-void fns require at least one return
	if (.not. parser%returned .and. fn%type%type /= void_type) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	else if (.not. parser%returned) then
		! Void fn with no returns: nothing to check
	else if (.not. all_paths_return(body)) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		if (permissive_return) then
			if (parser%ipass /= 0) write(error_unit, '(a)') warn_missing_return(parser%context(), &
				span, identifier%text)
		else
			call parser%diagnostics%push( &
				err_missing_return(parser%context(), &
				span, identifier%text))
		end if
	end if

	! Reset to allow the global scope to return anything
	parser%fn_type%type = any_type

	! Insert fn into parser%fns

	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	decl%kind       = fn_declaration
	decl%identifier = identifier
	decl%num_locs   = parser%num_locs
	call syntax_node_move(body, decl%body)
	!print *, "decl num_locs = ", decl%num_locs

	call parser%vars%pop_scope()
	call parser%locs%pop_scope()

	!deallocate(dict%dicts(i)%root)
	!if (allocated(parser%locs%dicts(1)%root)) deallocate(parser%locs%dicts(1)%root)

	!print *, "popping is_loc = false"
	parser%is_loc = .false.  ! no nested fn declarations

	allocate(fn%node)
	fn%node = decl

	overwrite = .true.
	if (parser%ipass == 0) overwrite = .false.

	io = 0
	call parser%fns%insert(identifier%text, fn, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%fn_names%push( identifier%text )
	!print *, "fn insert io = ", io

	! error if fn already declared. be careful in future if fn prototypes are
	! added
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_redeclare_fn(parser%context(), &
			span, identifier%text))
	end if

	! Check if this would shadow an overloaded intrinsic function.
	! Non-overloaded intrinsics are handled by the err_redeclare_fn() check
	! above
	if (is_overloaded_intr(identifier%text)) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_redeclare_intr_fn(parser%context(), span, identifier%text))
	end if

	!print *, 'size(decl%params) = ', size(decl%params)
	!print *, 'decl%params = ', decl%params

end subroutine parse_fn_declaration

!===============================================================================

module subroutine parse_struct_declaration(parser, decl)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: itype, i, io, pos0

	logical :: overwrite

	type(struct_t) :: struct, dummy_struct

	type(syntax_node_t) :: method_decl

	type(syntax_node_vector_t) :: method_decls

	logical :: is_const_meth

	integer :: j

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
	if (itype /= unknown_type .and. itype /= struct_type) then
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
			parser%current_kind() /= eof_token .and. &
			parser%current_kind() /= fn_keyword .and. &
			.not. (parser%current_kind() == const_keyword .and. &
			       parser%peek_kind(1) == fn_keyword))
		i = i + 1

		pos0 = parser%current_pos()

		name  = parser%match(identifier_token)
		call pos_mems%push( name%pos )
		colon = parser%match(colon_token)

		call parser%parse_type(type_text, type)
		!print *, "type = ", type_text

		call types%push(type)
		call names%push( name%text )

		if (parser%current_kind() /= rbrace_token .and. &
		    parser%current_kind() /= fn_keyword .and. &
		    .not. (parser%current_kind() == const_keyword .and. &
		           parser%peek_kind(1) == fn_keyword)) then
			! Delimiting commas are required; trailing comma is optional
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do

	! Sentinel for duplicate-member span: end of the last field (or start of method/rbrace)
	call pos_mems%push(parser%current_pos())

	! Now that we have the number of members, save them

	struct%num_vars = 0
	allocate(struct%member_names%v( names%len_ ))
	allocate(struct%vars%dicts(1))

	! Canonical alias-independent identity, set once at declaration time.  This
	! is invariant to the module alias/import path used to reach the struct, so
	! it lets type_match() recognize the same struct imported two different ways
	! as the same type (c.f. struct_cookie in types_ops.f90)
	struct%cookie = parser%contexts%v(parser%current_unit())%src_file &
		// "::" // identifier%text

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
		if (io /= exit_success) then
			span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
			call parser%diagnostics%push(err_redeclare_mem( &
				parser%context(), &
				span, &
				names%v(i)%s))
		end if

		if (allocated(member%array)) deallocate(member%array)
	end do

	! Parse method declarations (fn / const fn inside the struct body)
	method_decls = new_syntax_node_vector()

	do while ( &
			parser%current_kind() /= rbrace_token .and. &
			parser%current_kind() /= eof_token)

		pos0 = parser%current_pos()

		is_const_meth = (parser%current_kind() == const_keyword)
		if (is_const_meth) dummy = parser%next()   ! consume 'const'

		call parser%parse_method_declaration(method_decl, struct, is_const_meth, &
			identifier%text)

		! Save method decl node for the bytecode compiler pre-pass
		call method_decls%push(method_decl)

		! Break infinite loop
		if (parser%current_pos() == pos0) dummy = parser%next()

	end do

	! Store method nodes in decl%members so the bytecode compiler can compile them
	if (method_decls%len_ > 0) then
		allocate(decl%members(method_decls%len_))
		do j = 1, method_decls%len_
			decl%members(j) = method_decls%v(j)
		end do
	end if

	rbrace = parser%match(rbrace_token)

	! Insert struct into parser dict

	parser%num_structs = parser%num_structs + 1
	decl%id_index  = parser%num_structs

	overwrite = .false.
	if (parser%ipass > 0) overwrite = .true.

	!print *, "inserting identifier ", identifier%text, " into parser structs"
	call parser%structs%insert( &
		identifier%text, struct, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%struct_names%push(identifier%text)
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

end subroutine parse_struct_declaration

!===============================================================================

module subroutine parse_method_declaration(parser, decl, struct, is_const, struct_name)

	! Parse a method declared inside a struct body.
	! The implicit first parameter "0self" is the struct passed by reference.
	! Field names in the method body are transformed to dot_expr on "0self"
	! by parse_name_expr and parse_expr_statement (via parser%in_method context).

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl
	type(struct_t), intent(in) :: struct
	logical, intent(in) :: is_const
	character(len = *), intent(in) :: struct_name

	!********

	character(len = :), allocatable :: type_text, mangled_name

	integer :: i, io, pos0, rank, fn_beg, fn_name_end

	logical :: overwrite, const_param

	type(fn_t) :: fn

	type(string_vector_t) :: names
	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref, is_const_ref

	type(syntax_node_t) :: body
	type(syntax_token_t) :: fn_kw, identifier, lparen, rparen, colon, &
		name, comma, dummy, amp

	type(text_span_t) :: span

	type(value_t) :: type, self_val
	type(value_vector_t) :: types

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
	fn%is_method = .true.
	fn%is_const_method = is_const

	! Insert implicit "0self" as first local (the struct receiver)
	self_val%type = struct_type
	self_val%struct_name = struct_name
	if (allocated(struct%cookie)) self_val%struct_cookie = struct%cookie
	parser%num_locs = parser%num_locs + 1
	const_param = is_const
	call parser%locs%insert("0self", self_val, parser%num_locs, io, &
		is_const = const_param)
	parser%self_loc_id = parser%num_locs

	! Set method parsing context so field names resolve to dot_expr("0self", field)
	parser%in_method = .true.
	parser%in_const_method = is_const
	parser%method_struct = struct

	if (parser%current_kind() /= lparen_token) then
		parser%in_method = .false.
		parser%in_const_method = .false.
		call parser%vars%pop_scope()
		call parser%locs%pop_scope()
		parser%is_loc = .false.
		return
	end if

	lparen = parser%match(lparen_token)

	names        = new_string_vector()
	pos_args     = new_integer_vector()
	is_ref       = new_logical_vector()
	is_const_ref = new_logical_vector()
	types        = new_value_vector()

	i = 0
	do while ( &
			parser%current_kind() /= rparen_token .and. &
			parser%current_kind() /= eof_token)
		i = i + 1

		pos0 = parser%current_pos()
		call pos_args%push(pos0)

		name  = parser%match(identifier_token)
		colon = parser%match(colon_token)

		if (parser%current_kind() == amp_token) then
			amp = parser%match(amp_token)
			call is_ref%push(.true.)
			if (parser%current_kind() == const_keyword) then
				dummy = parser%next()
				call is_const_ref%push(.true.)
			else
				call is_const_ref%push(.false.)
			end if
		else
			call is_ref%push(.false.)
			call is_const_ref%push(.false.)
		end if

		call parser%parse_type(type_text, type)

		call names%push( name%text )
		call types%push(type)

		if (parser%current_kind() /= rparen_token) then
			comma = parser%match(comma_token)
		end if

		if (parser%current_pos() == pos0) dummy = parser%next()

	end do
	call pos_args%push(parser%current_pos() + 1)

	rparen = parser%match(rparen_token)

	! params(1) = self slot, params(2..) = explicit params
	! is_ref(1) = true (self is always by-ref), is_const_ref(1) = is_const
	allocate(fn%params          ( names%len_ ))
	allocate(fn%param_names%v   ( names%len_ ))
	allocate(decl%params        ( 1 + names%len_ ))
	allocate(decl%is_ref        ( 1 + names%len_ ))
	allocate(decl%is_const_ref  ( 1 + names%len_ ))

	! Self is slot 1
	decl%params(1)       = parser%self_loc_id
	decl%is_ref(1)       = .true.
	decl%is_const_ref(1) = is_const

	do i = 1, names%len_
		fn%param_names%v(i)%s = names%v(i)%s
		fn%params(i) = types%v(i)

		parser%num_locs = parser%num_locs + 1
		decl%params(1 + i)       = parser%num_locs
		decl%is_ref(1 + i)       = is_ref%v(i)
		decl%is_const_ref(1 + i) = is_const_ref%v(i)

		const_param = is_const_ref%v(i)
		call parser%locs%insert(fn%param_names%v(i)%s, fn%params(i), parser%num_locs, &
			is_const = const_param)
	end do

	fn%type%type = void_type
	rank = 0
	if (parser%current_kind() == colon_token) then
		colon = parser%match(colon_token)
		call parser%parse_type(type_text, type)
		fn%type = type
	end if

	parser%fn_name = identifier%text
	parser%fn_type = fn%type

	call parser%parse_statement(body)

	if (.not. parser%returned .and. fn%type%type /= void_type) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	else if (parser%returned .and. .not. all_paths_return(body)) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		if (permissive_return) then
			if (parser%ipass /= 0) write(error_unit, '(a)') warn_missing_return(parser%context(), &
				span, identifier%text)
		else
			call parser%diagnostics%push( &
				err_missing_return(parser%context(), &
				span, identifier%text))
		end if
	end if

	parser%fn_type%type = any_type

	! Clear method context before registering fn
	parser%in_method = .false.
	parser%in_const_method = .false.

	! Register method in global fns with mangled name "0StructName::method_name"
	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	decl%kind       = fn_declaration
	decl%identifier = identifier
	decl%num_locs   = parser%num_locs
	call syntax_node_move(body, decl%body)

	call parser%vars%pop_scope()
	call parser%locs%pop_scope()
	parser%is_loc = .false.

	allocate(fn%node)
	fn%node = decl

	overwrite = .true.
	if (parser%ipass == 0) overwrite = .false.

	mangled_name = "0" // struct_name // "::" // identifier%text
	io = 0
	call parser%fns%insert(mangled_name, fn, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%fn_names%push(mangled_name)

end subroutine parse_method_declaration

!===============================================================================

recursive module subroutine parse_struct_instance(parser, inst, struct_name)

	! A struct instantiator initializes all the members of an instance of a
	! struct

	class(parser_t) :: parser

	type(syntax_node_t), intent(out) :: inst
	character(len = *), intent(in), optional :: struct_name

	!********

	character(len = :), allocatable :: unset_name, exp_type, act_type, lookup_name

	integer :: io, pos0, pos1, struct_id, member_id, id1(1)

	logical :: is_ok
	logical, allocatable :: member_set(:)

	type(struct_t) :: struct

	type(syntax_node_t) :: mem

	type(syntax_token_t) :: identifier, name, equals, comma, lbrace, rbrace, dummy

	type(text_span_t) :: span

	type(value_t) :: member

	!print *, "starting parse_struct_instance()"

	if (present(struct_name)) then
		! Called from parse_qualified_expr with pre-parsed qualified name
		lookup_name = struct_name
	else
		! Original path: parse identifier from current position
		identifier = parser%match(identifier_token)
		lookup_name = identifier%text
	end if

	!print *, "parsing struct instance of lookup_name = ", lookup_name

	!print *, ""
	!print *, "in parse_struct_instance():"
	!print *, "parser structs root     = ", parser%structs%dict%root%split_char
	!print *, "parser structs root mid = ", parser%structs%dict%root%mid%split_char

	call parser%structs%search(lookup_name, struct_id, io, struct)
	!print *, "struct io = ", io

	lbrace  = parser%match(lbrace_token)

	inst%kind = struct_instance_expr
	!inst%identifier = identifier

	! Save everything in the inst syntax node
	inst%val%type = struct_type
	allocate(inst%val%struct( struct%num_vars ))
	allocate(inst%members   ( struct%num_vars ))

	member_set = spread(.false., 1, struct%num_vars)

	inst%struct_name = lookup_name
	inst%val%struct_name = lookup_name
	inst%val%struct_cookie = struct%cookie

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
		call parser%parse_expr(expr=mem)

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
				lookup_name))
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
				lookup_name, &
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
					lookup_name))
			end if

			! Members can be instantiated out of order.  Insert by id, not loop iterator
			inst%val%struct( member_id ) = mem%val
			inst%members( member_id ) = mem
			member_set(member_id) = .true.

		end if

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

		if (present(struct_name)) then
			! Use lbrace position for qualified struct instances
			span = new_span(lbrace%pos, len(lookup_name))
		else
			span = new_span(identifier%pos, len(identifier%text))
		end if
		call parser%diagnostics%push(err_unset_member( &
			parser%context(), &
			span, &
			unset_name, &
			lookup_name))
	end if

	!print *, "ending parse_struct_instance()"

end subroutine parse_struct_instance

!===============================================================================

module subroutine parse_type(parser, type_text, type)

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	type(value_t), intent(out) :: type

	!********

	integer :: rank, itype
	integer :: pos0, pos1, pos2

	type(struct_t) :: struct

	type(syntax_token_t) :: colon, ident, comma, lbracket, rbracket, semi, dummy, &
		double_colon

	type(text_span_t) :: span

	pos1 = parser%current_pos()
	if (parser%current_kind() == lbracket_token) then

		! Array param
		lbracket = parser%match(lbracket_token)
		ident    = parser%match(identifier_token)
		type_text = ident%text
		do while (parser%current_kind() == double_colon_token)
			! Qualified element type, e.g. [mod::Struct; :]
			double_colon = parser%match(double_colon_token)
			ident = parser%match(identifier_token)
			type_text = type_text//"::"//ident%text
		end do
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
		type_text = ident%text
		do while (parser%current_kind() == double_colon_token)
			! Qualified type, e.g. mod::Struct
			double_colon = parser%match(double_colon_token)
			ident = parser%match(identifier_token)
			type_text = type_text//"::"//ident%text
		end do
		rank = -1
	end if
	pos2 = parser%current_pos()

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

	if (itype == struct_type) then
		type%struct_name = type_text
		type%struct_cookie = struct%cookie
	end if

end subroutine parse_type

!===============================================================================

!===============================================================================

recursive function all_paths_return(node) result(returns)

	! Determine whether the given AST node guarantees a return on every
	! execution path.  Used at parse-time to diagnose functions that may
	! fall off the end without returning.
	!
	! Rules (conservative):
	!   return_statement  -> always returns.
	!   block_statement   -> returns iff any member returns on all paths
	!                        (statements after an unconditional return are
	!                        unreachable).
	!   if_statement      -> returns iff else_clause is present AND both
	!                        if_clause and else_clause return on all paths.
	!                        else-if chains recurse through else_clause.
	!   while/for         -> never guaranteed (loop may execute zero times).
	!   everything else   -> does not return.

	type(syntax_node_t), intent(in) :: node
	logical :: returns

	!********

	integer :: i

	returns = .false.
	select case (node%kind)

	case (return_statement)
		returns = .true.

	case (block_statement)
		! The block returns as soon as any member is guaranteed to return.
		if (allocated(node%members)) then
			do i = 1, size(node%members)
				if (all_paths_return(node%members(i))) then
					returns = .true.
					exit
				end if
			end do
		end if

	case (if_statement)
		! Requires both a then-branch and an else-branch that both return.
		! else-if chains are an else_clause that is itself an if_statement,
		! so recursion handles them naturally.
		if (allocated(node%else_clause) .and. allocated(node%if_clause)) then
			returns = all_paths_return(node%if_clause) .and. &
			          all_paths_return(node%else_clause)
		end if

	! while_statement / for_statement: body may run zero times -> no guarantee.
	! All other kinds: not a return.

	end select

end function all_paths_return

!===============================================================================

module subroutine check_call_arg(parser, arg, call_is_ref_i, arg_span, &
		fn_name, i_0based, param_val, param_name, param_is_ref, param_is_const_ref)

	class(parser_t), intent(inout) :: parser
	type(syntax_node_t), intent(in) :: arg
	logical(kind = 1), intent(in) :: call_is_ref_i
	type(text_span_t), intent(in) :: arg_span
	character(len = *), intent(in) :: fn_name, param_name
	integer, intent(in) :: i_0based
	type(value_t), intent(in) :: param_val
	logical, intent(in) :: param_is_ref, param_is_const_ref

	!********

	integer :: id_index_tmp, io_tmp
	logical :: is_const_var, is_ok
	character(len = :), allocatable :: exp_type, act_type
	type(value_t) :: const_check_val

	! Passing a const variable to a mutable-ref param is an error
	if (param_is_ref .and. .not. param_is_const_ref .and. arg%kind == name_expr) then
		is_const_var = .false.
		if (arg%is_loc) then
			call parser%locs%search(arg%identifier%text, &
				id_index_tmp, io_tmp, const_check_val, is_const = is_const_var)
		else
			call parser%vars%search(arg%identifier%text, &
				id_index_tmp, io_tmp, const_check_val, is_const = is_const_var)
		end if
		if (is_const_var) then
			call parser%diagnostics%push(err_const_assign( &
				parser%context(), arg_span, arg%identifier%text))
		end if
	end if

	! Ref/val mismatch
	if (param_is_ref .neqv. call_is_ref_i) then
		if (param_is_ref) then
			call parser%diagnostics%push(err_bad_arg_val( &
				parser%context(), arg_span, fn_name, i_0based, param_name))
		else
			call parser%diagnostics%push(err_bad_arg_ref( &
				parser%context(), arg_span, fn_name, i_0based, param_name))
		end if
	end if

	! Type mismatch
	is_ok = types_match(param_val, arg%val) == TYPE_MATCH
	is_ok = is_ok .or. arg%val%type == unknown_type
	if (.not. is_ok) then
		exp_type = type_name(param_val)
		act_type = type_name(arg%val)
		call parser%diagnostics%push(err_bad_arg_type( &
			parser%context(), arg_span, fn_name, i_0based, param_name, exp_type, act_type))
	end if

end subroutine check_call_arg

!===============================================================================

end submodule syntran__parse_fn

!===============================================================================

