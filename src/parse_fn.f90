
!===============================================================================

submodule (syntran__parse_m) syntran__parse_fn

	use syntran__intr_fns_m
	use syntran_rt, only: rt_str_t, rt_fmt_spec_t, rt_fmt_split, rt_fmt_parse

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine parse_fn_call(parser, module_prefix, identifier, fn_call)

	class(parser_t), target :: parser
	character(len = *), intent(in), optional :: module_prefix
	type(syntax_token_t), intent(in), optional :: identifier
	type(syntax_node_t), intent(out) :: fn_call

	!********

	character(len = :), allocatable :: exp_type, act_type, param_name, &
		lookup_name, display_name

	integer :: i, io, io_std, id_index, id_index_tmp, pos0, rank, arr_type_result, arr_type_src, slot
	integer :: var_io, var_id_index, method_slot, method_fn_id
	integer :: field_id, field_io, ifmt, cast_type

	logical :: has_rank, has_arr_type, param_is_ref, param_is_const_ref, self_name, &
		arg_is_ref, is_ok, is_const_var, var_is_loc

	logical(kind = 1) :: eff_is_ref

	type(fn_t), pointer :: fn, method_fn

	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref

	type(syntax_node_t) :: arg, self_receiver, member_receiver
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: identifier_, comma, lparen, rparen, dummy, amp, self_token

	type(text_span_t) :: span
	type(text_span_t), allocatable :: alias_spans(:)

	logical, allocatable :: alias_mut(:)

	type(value_t) :: param_val, var_val, self_val, field_val

	!print *, ''
	!print *, 'parse_fn_call'

	! Function call expression
	if (present(identifier)) then
		identifier_ = identifier
	else
		call parser%match(identifier_token, identifier_)
	end if

	!print *, "identifier_ = ", identifier_%text

	! Inside a method body, is a bare name a member or a sibling method of the
	! struct being parsed?  Then the call is an implicit-self call, c.f. the
	! fallbacks below, and a std-only fn of the same name must not take it
	self_name = .false.
	if (.not. present(module_prefix) .and. parser%in_method) then
		call parser%method_struct%vars%search(identifier_%text, &
			field_id, field_io, field_val)
		method_slot = parser%fns%find( &
			"0" // unqualified_name(parser%method_struct_name) // "::" // identifier_%text)
		self_name = field_io == exit_success .or. method_slot /= 0
	end if

	! After `use std::*;`, a bare name that isn't a user fn or legacy intrinsic
	! but is a std-only fn is parsed exactly like the explicit `std::name()`
	! call.  User fns and legacy intrinsics take precedence over the std-only fn,
	! and a variable or a member of that name is left to the handling below
	if (.not. present(module_prefix) .and. .not. self_name .and. &
		parser%std_imported(identifier_%text)) then
		if (parser%fns%find(identifier_%text) == 0 .and. &
			parser%fns%find("std::" // identifier_%text) /= 0) then
			call parser%search(identifier_%text, var_id_index, var_io, var_val, &
				var_is_loc)
			if (var_io /= exit_success) then
				call parser%parse_fn_call("std", identifier_, fn_call)
				return
			end if
		end if
	end if

	args     = new_syntax_node_vector()
	pos_args = new_integer_vector()
	is_ref   = new_logical_vector()

	call parser%match(lparen_token, lparen)

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		call pos_args%push(parser%current_pos())

		arg_is_ref = .false.
		if (parser%current_kind() == amp_token) then
			call parser%match(amp_token, amp)
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
			call parser%match(comma_token, comma)
		end if

		! break infinite loop
		if (parser%pos == pos0) call parser%next(dummy)

	end do
	call pos_args%push(parser%current_pos() + 1)
	!print *, "args%len_ = ", args%len_

	call parser%match(rparen_token, rparen)

	fn_call%kind = fn_call_expr
	fn_call%identifier = identifier_

	! Set module prefix if provided (for qualified calls like std::println)
	if (present(module_prefix)) then
		fn_call%module_prefix = module_prefix
	end if

	call resolve_overload(args, fn_call, has_rank, has_arr_type, arr_type_result, arr_type_src)
	if (has_rank) rank = fn_call%val%array%rank

	! If any argument has unknown_type, return early to prevent cascading errors.
	! The actual error (e.g. undefined function) was already pushed earlier.
	do i = 1, args%len_
		if (args%v(i)%val%type == unknown_type) then
			fn_call%val%type = unknown_type
			return
		end if
	end do

	! The rank of std::reshape()'s result is the length of its shape argument,
	! which is only known at parse time for an array literal
	if (parser%ipass /= 0 .and. present(module_prefix) .and. args%len_ >= 2) then
		if (module_prefix == "std" .and. identifier_%text == "reshape" .and. &
			.not. is_expl_array_lit(args%v(2))) then
			span = new_span(pos_args%v(2), pos_args%v(3) - pos_args%v(2) - 1)
			call parser%diagnostics%push( &
				err_reshape_shape_lit(parser%context(), span))
		end if
	end if

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
			slot = parser%fns%find(lookup_name)
			if (slot == 0) then
				! Fall back to regular intrinsic lookup without prefix (legacy intrinsics)
				lookup_name = fn_call%identifier%text
				slot = parser%fns%find(lookup_name)
			end if
		else
			! For user modules, look up with qualified name
			lookup_name = module_prefix // "::" // fn_call%identifier%text
			slot = parser%fns%find(lookup_name)
		end if
		display_name = module_prefix // "::" // identifier_%text
	else
		lookup_name = fn_call%identifier%text
		display_name = identifier_%text
		slot = parser%fns%find(lookup_name)
	end if

	if (slot == 0) then
		io = exit_failure
	else
		io = exit_success
		fn => parser%fns%get(slot)
		id_index = parser%fns%id_at(slot)
	end if

	!print *, "fn id_index = ", id_index
	if (io /= exit_success) then

		! Check if this is an std-only function that requires the std:: prefix.
		! Do this before the ipass==0 early return so the type is set correctly
		! in both passes, preventing cascading errors.  A member or a sibling
		! method of the same name is called through self instead
		if (.not. present(module_prefix) .and. .not. self_name) then
			slot = parser%fns%find("std::" // fn_call%identifier%text)
			if (slot == 0) then
				io_std = exit_failure
			else
				io_std = exit_success
				fn => parser%fns%get(slot)
				id_index = parser%fns%id_at(slot)
			end if
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

		! Not a known fn (this is the final pass, so no forward-reference
		! concern remains): check if this identifier is instead a local/global
		! variable of fn_type -- an indirect call through a fn pointer, e.g.
		! `let f = dbl; f(21);`.  Restricted to plain (unqualified) names in v1
		if (.not. present(module_prefix)) then

			call parser%search(identifier_%text, var_id_index, var_io, var_val, &
				var_is_loc)

			if (var_io == exit_success) then

				if (var_val%type /= fn_type) then
					span = new_span(identifier_%pos, len(identifier_%text))
					call parser%diagnostics%push( &
						err_not_callable(parser%context(), &
						span, identifier_%text, type_name(var_val)))
					fn_call%val%type = unknown_type
					return
				end if

				! Indirect call: dispatch target is resolved at runtime from the
				! callee variable value (fn_index), not from a parse-time id_index
				! into a specific fn.  id_index/is_loc here identify the callee
				! *variable* slot instead -- eval/compile distinguish this via
				! node%kind == fn_call_ptr_expr.  build_fn_ptr_call_node
				! (parse_expr.f90, shared with parse_dot's `s.f(x)` fn-typed-
				! struct-member branch) validates args and builds everything
				! else on fn_call
				call build_fn_ptr_call_node(parser, fn_call, var_val, &
					identifier_%text, args, is_ref, pos_args, &
					lparen%pos, rparen%pos)
				fn_call%id_index = var_id_index
				fn_call%is_loc = var_is_loc

				return

			end if

		end if

		! Bare-name fn-typed-member call: inside a method body, `f(args)` where
		! `f` is a member of the struct being parsed (and not a free fn or
		! fn-pointer variable, both checked above) is an indirect call through
		! that member on the implicit self, i.e. `self.f(args)`.  Mirrors
		! parse_dot's `s.f(x)` branch (parse_expr.f90), and compiles through
		! the same fn_call_ptr_expr + node%left member-chain path
		if (.not. present(module_prefix) .and. parser%in_method) then

			call parser%method_struct%vars%search(identifier_%text, &
				field_id, field_io, field_val)

			if (field_io == exit_success) then

				if (field_val%type /= fn_type) then
					span = new_span(identifier_%pos, len(identifier_%text))
					call parser%diagnostics%push( &
						err_not_callable(parser%context(), &
						span, identifier_%text, type_name(field_val)))
					fn_call%val%type = unknown_type
					return
				end if

				! Receiver: the member read `0self.<field>`, same shape as the
				! implicit-self read in parse_name_expr (parse_expr.f90)
				member_receiver%kind       = dot_expr
				member_receiver%id_index   = parser%self_loc_id
				member_receiver%is_loc     = .true.
				member_receiver%identifier = identifier_
				allocate(member_receiver%member)
				member_receiver%member%id_index   = field_id
				member_receiver%member%identifier = identifier_
				call value_copy(member_receiver%member%val, field_val)
				call value_copy(member_receiver%val, member_receiver%member%val)

				call build_fn_ptr_call_node(parser, fn_call, field_val, &
					identifier_%text, args, is_ref, pos_args, &
					lparen%pos, rparen%pos)
				if (fn_call%val%type == unknown_type) return
				call syntax_node_move(member_receiver, fn_call%left)

				return

			end if

		end if

		! Bare-name self-method fallback: inside a method body, a bare call
		! `foo(args)` that isn't a free fn or fn-pointer var may still be a
		! sibling method of the struct whose method is being parsed, called
		! implicitly on self, e.g. `push(x)` from inside another method
		! resolves to `self.push(x)`.  Free fns/fn-pointer vars take
		! precedence (checked above); this is only a fallback.  Mirrors the
		! mangled-name lookup in parse_dot's method-call branch
		! (parse_expr.f90): methods are registered under "0StructName::method"
		if (.not. present(module_prefix) .and. parser%in_method) then

			method_slot = parser%fns%find( &
				"0" // unqualified_name(parser%method_struct_name) // "::" // identifier_%text)

			if (method_slot /= 0) then
				method_fn    => parser%fns%get(method_slot)
				method_fn_id = parser%fns%id_at(method_slot)

				! Const receiver enforcement: a const method may not call a
				! mutable sibling method.  Self is always a bound local (never
				! a temporary fn-return value), so only this check -- not the
				! mutable-method-on-temp check in parse_dot -- applies here
				if (.not. method_fn%is_const_method .and. parser%in_const_method) then
					span = new_span(identifier_%pos, len(identifier_%text))
					call parser%diagnostics%push(err_const_assign( &
						parser%context(), span, "self"))
				end if

				! Synthesize the implicit self receiver: a name_expr bound to
				! the "0self" local inserted in parse_method_declaration
				self_token         = identifier_
				self_token%text    = "0self"
				self_val%type      = struct_type
				self_val%struct_name = parser%method_struct_name
				call new_name_expr(self_token, self_val, self_receiver)
				self_receiver%id_index = parser%self_loc_id
				self_receiver%is_loc   = .true.

				call build_method_call_node(parser, fn_call, self_receiver, &
					method_fn, method_fn_id, identifier_, args, is_ref, &
					pos_args, lparen%pos, rparen%pos)
				return
			end if

		end if

		span = new_span(identifier_%pos, len(identifier_%text))
		if (present(module_prefix)) then
			if (module_prefix /= "std") then
				call parser%diagnostics%push( &
					err_undeclare_fn(parser%context(), &
					span, display_name, &
					parser%fns%closest(display_name), &
					module_prefix = module_prefix))
			else
				call parser%diagnostics%push( &
					err_undeclare_fn(parser%context(), &
					span, display_name, &
					parser%fns%closest(display_name)))
			end if
		else
			call parser%diagnostics%push( &
				err_undeclare_fn(parser%context(), &
				span, display_name, &
				parser%fns%closest(display_name)))
		end if

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
		! i32() and i64().  Covered by nd-i32/logo-lowres.syntran (a low-res
		! port of samples/logo.syntran), which assigns a 2-D array slice from
		! i32() applied to an f32/f64 array expression
		if (.not. allocated(fn_call%val%array)) allocate(fn_call%val%array)
		fn_call%val%array%rank = rank

		! A reduction with a `dim` arg (e.g. sum(v, 0)) over a rank-1 array
		! resolves to rank 0, i.e. a scalar of the array's element type
		if (rank == 0) then
			fn_call%val%type = fn_call%val%array%type
			deallocate(fn_call%val%array)
		end if

		! For functions like std::reshape whose element type depends on their
		! arguments, restore the element type that resolve_overload determined.
		! fn_call%val = fn%type above would otherwise overwrite it with any_type.
		if (has_arr_type .and. .not. allocated(fn_call%val%array)) then
			! The rank-0 case above already deallocated the array info
			fn_call%val%type = arr_type_result
		else if (has_arr_type) then
			fn_call%val%array%type = arr_type_result

			! %array%type alone doesn't fully describe a struct/enum element
			! type: member access needs struct_name to look the struct up, and
			! type-equality checks need struct_cookie/enum_cookie. Copy those
			! from whichever argument the element type came from.
			if (arr_type_src > 0) then
				call copy_composite_id(fn_call%val, args%v(arr_type_src)%val)
			end if
		end if

	end if
	!print *, "rank = ", fn_call%val%array%rank

	! Intrinsic fns don't have a syntax node: they are implemented
	! in Fortran, not syntran

	!if (allocated(fn%node)) then
	if (.not. fn%is_intr) then

		!print *, 'assigning fn node'

		! fn_call%body is *not* set here.  The body is not copied per call
		! site: fn_call carries id_index, and compile_node() looks the body
		! up via cs%fns%fns(id_index)%node%body instead.  Params and
		! num_locs are still needed directly on fn_call, since compile_node()
		! reads them before it has a state to index into
		fn_call%params = fn%node%params

		fn_call%num_locs = fn%node%num_locs

		!print *, 'fn params size = ', size(fn%params)
		!print *, 'fn call params size = ', size(fn_call%params)

	end if

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

	allocate(alias_mut(args%len_), alias_spans(args%len_))

	allocate(param_val%array)
	do i = 1, args%len_

		! For variadic fns, check each variadic argument's type against
		! variadic_type.  writeln(file) writes a blank line, and
		! writeln(file, string1, 1, 2.0, ...) is fine too since writeln's
		! variadic_type is any_type; min/max instead set variadic_type to
		! their own numeric type, so e.g. min(1, 2, "a") is rejected

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
		else if (allocated(fn%ref_params)) then
			! Intrinsic with by-ref params, e.g. std::lu_factor(&A)
			if (i <= size(fn%ref_params)) then
				param_is_ref       = fn%ref_params(i)
				param_is_const_ref = fn%const_ref_params(i)
			end if
		end if

		span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
		call check_call_arg(parser, args%v(i), is_ref%v(i), span, &
			identifier_%text, i - 1, param_val, param_name, &
			param_is_ref, param_is_const_ref, eff_is_ref)
		fn_call%is_ref(i) = eff_is_ref
		alias_mut  (i) = param_is_ref .and. .not. param_is_const_ref
		alias_spans(i) = span

		! A bare enum name is a special form, only allowed as a value to a
		! small allowlist of intrinsics that consume the array and return a
		! non-array (size/str/std::print/println/std::write/writeln) -- everywhere else, including
		! user fns and any other intrinsic (e.g. std::reshape, which would
		! just hand the variant array back out), reject it as E99
		if (args%v(i)%is_enum_name) then
			if (.not. (fn%is_intr .and. ( &
				identifier_%text == "size"    .or. &
				identifier_%text == "str"     .or. &
				identifier_%text == "println" .or. &
				identifier_%text == "print"   .or. &
				identifier_%text == "writeln" .or. &
				identifier_%text == "write"))) then

				call parser%check_enum_name_value(args%v(i))
			end if
		end if

	end do

	call check_ref_aliases(parser, args%v(1: args%len_), fn_call%is_ref, &
		alias_mut, alias_spans, identifier_%text)

	! The i32(), i64(), f32() and f64() casts accept any_type params so that they
	! can be overloaded on scalar vs array, so check the arg type here.  The float
	! casts take only numeric types, while the int casts also take a str (a single
	! char, checked at runtime) or an enum.  Array args are checked by their
	! element type
	if (fn%is_intr .and. any(identifier_%text == ["i32", "i64", "f32", "f64"]) &
		.and. args%len_ == 1) then

		cast_type = args%v(1)%val%type
		if (cast_type == array_type) cast_type = args%v(1)%val%array%type

		is_ok = is_num_type(cast_type) .or. cast_type == unknown_type
		if (any(identifier_%text == ["i32", "i64"])) then
			is_ok = is_ok .or. cast_type == str_type .or. cast_type == enum_type
			exp_type = "numeric, str, or enum"
		else
			exp_type = "any numeric type"
		end if

		if (.not. is_ok) then
			call parser%diagnostics%push(err_bad_arg_type( &
				parser%context(), alias_spans(1), identifier_%text, 0, "a", &
				exp_type, type_name(args%v(1)%val)))
		end if
	end if

	if (fn%is_intr .and. any(identifier_%text == ["format", "printf", "writef"])) then
		! The format string is the first arg, except for writef(), where the file
		! handle comes first
		ifmt = 1
		if (identifier_%text == "writef") ifmt = 2
		span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
		call check_format_call(parser, identifier_%text, args%v(ifmt: args%len_), &
			alias_spans(ifmt:), span)
	end if

	fn_call%id_index = id_index

	! Move args from vector (avoids deep copy)
	allocate(fn_call%args(args%len_))
	do i = 1, args%len_
		call syntax_node_move_into(args%v(i), fn_call%args(i))
	end do

	if (fn%is_intr .and. identifier_%text == "printf") then
		call desugar_printf(parser, fn_call, "print", 1)
	else if (fn%is_intr .and. identifier_%text == "writef") then
		call desugar_printf(parser, fn_call, "write", 2)
	end if

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
	call parser%match(identifier_token, mod_identifier)
	module_name = mod_identifier%text

	! Consume ::
	call parser%match(double_colon_token, double_colon)

	! Get the next identifier (could be another namespace or the fn/var name)
	call parser%match(identifier_token, fn_identifier)
	fn_name = fn_identifier%text

	! Handle nested namespaces: math::vectors::fn()
	! Keep consuming ::identifier pairs while we see more :: tokens
	do while (parser%current_kind() == double_colon_token)
		! The current fn_name is actually part of the module path
		module_name = module_name // "::" // fn_name
		call parser%match(double_colon_token, double_colon)
		call parser%match(identifier_token, fn_identifier)
		fn_name = fn_identifier%text
	end do

	lookup_name = module_name // "::" // fn_name

	if (parser%enums%exists(lookup_name)) then
		! The qualified name is a registered enum type name.  Checked first
		! and unconditionally (rather than folded into the current_kind()-
		! driven branches below), mirroring the restructured unqualified
		! dispatch in parse_primary_expr() -- e.g. `for s in mod::Suit {
		! ... }` has `{` right after `Suit`, which would otherwise be caught
		! by the struct-instance branch below and misread as an undeclared
		! variable
		if (parser%current_kind() == lparen_token) then
			! Qualified reverse cast: mod::Suit(2)
			call parser%parse_enum_cast(expr, lookup_name)

		else if (parser%current_kind() == dot_token) then
			! Qualified enum variant access: mod::EnumName.Variant
			call parser%parse_enum_access(expr, lookup_name)

		else
			! Qualified bare enum name: mod::Suit (array of all variants).
			! A live variable of the same qualified name always wins,
			! mirroring the unqualified dispatch in parse_primary_expr()
			call parser%vars%search(lookup_name, id_index, iostat, var_val)
			if (iostat == exit_success) then
				call new_name_expr(fn_identifier, var_val, expr)
				expr%id_index = id_index
				expr%module_prefix = module_name
				call parser%parse_subscripts(expr)
				call parser%parse_dot(expr)
			else
				call parser%parse_enum_name_expr(expr, lookup_name)
				! parse_enum_name_expr() doesn't set expr%identifier when
				! enum_name is present (the bare name token was already
				! consumed by the caller here), so set it ourselves -- it's
				! needed as the span anchor for check_enum_name_value()
				expr%identifier = fn_identifier
				if (parser%current_kind() == lbracket_token) then
					call parser%diagnostics%push(err_enum_index( &
						parser%context(), &
						new_span(fn_identifier%pos, len(fn_identifier%text)), &
						expr%val%enum_name))
					! Error recovery: consume the subscripts so parsing
					! doesn't cascade, but don't attach them to expr
					block
						type(syntax_node_t) :: dummy_expr
						dummy_expr%val%type = unknown_type
						call parser%parse_subscripts(dummy_expr)
					end block
				end if
			end if
		end if

	else if (parser%current_kind() == lparen_token) then
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

module subroutine parse_fn_declaration(parser, decl, no_fn_kw)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl
	logical, intent(in), optional :: no_fn_kw

	!********

	character(len = :), allocatable :: type_text

	integer :: i, io, pos0, rank, fn_beg, fn_name_end

	! Captured alongside fn_beg below, before the body is parsed, so an
	! implicit-return instruction (which has no statement node of its own
	! to inherit a location from) maps back to the fn header instead of
	! whatever unit parsing happened to end in -- see emit_fn_epilogue()
	! in compile_ctrl.f90, which stamps prog%cur_id/cur_pos from
	! decl%src_id/src_pos before emitting the implicit return
	integer :: fn_src_id

	logical :: overwrite, const_param, in_fn_body0, no_fn_kw0

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

	no_fn_kw0 = .false.
	if (present(no_fn_kw)) no_fn_kw0 = no_fn_kw

	! Like a for statement, a fn declaration has its own scope (for its
	! parameters).  Its block body will have yet another scope
	call parser%vars%push_scope()
	call parser%locs%push_scope()
	parser%is_loc = .true.
	parser%num_locs = 0

	parser%returned = .false.
	fn_beg = parser%peek_pos(0)
	fn_src_id = parser%contexts%v(parser%current_unit())%src_id
	if (.not. no_fn_kw0) then
		call parser%match(fn_keyword, fn_kw)
	end if
	! When no_fn_kw0, the caller (parse_unit_pass() in parse_misc.f90) has
	! already pushed E106 for the missing `fn` and confirmed via
	! looks_like_fn_decl() that the token stream is unambiguously a fn
	! declaration -- so just carry on parsing from the name, without
	! consuming a `fn` token that isn't there

	call parser%match(identifier_token, identifier)
	fn_name_end = parser%peek_pos(0) - 1
	fn%is_intr = .false.

	!print *, "parsing fn ", identifier%text

	! c.f. the parser%pos (token index) vs current_pos()/peek_pos() (char
	! index) convention documented at their definitions in parse.f90 --
	! audited the new_span() call sites throughout src and none of them
	! build a span from parser%pos

	!print *, 'matching lparen'

	! This is a bit of a hack, but parser can crash otherwise if parens are
	! missing.  Should parens be optional for fns without params?
	if (parser%current_kind() /= lparen_token) return

	call parser%match(lparen_token, lparen)

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
		call parser%match(identifier_token, name)
		call parser%check_type_clash(name%text, name%pos)
		!print *, 'matching colon'
		call parser%match(colon_token, colon)

		! Should this be part of parse_type()?  I think not, as refs can appear
		! in fn decls but not struct decls.  Fn calls do not even call
		! parse_type, they call parse_expr instead
		if (parser%current_kind() == amp_token) then
			call parser%match(amp_token, amp)
			call is_ref%push(.true.)
			! &const means the callee cannot modify through this reference
			if (parser%current_kind() == const_keyword) then
				call parser%next(dummy)
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
			call parser%match(comma_token, comma)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) call parser%next(dummy)

	end do
	call pos_args%push(parser%current_pos() + 1)

	!print *, "names len = ", names%len_
	!print *, 'matching rparen'
	call parser%match(rparen_token, rparen)

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
		call parser%match(colon_token, colon)
		call parser%parse_type(type_text, type)

		! `&` as a return type is rejected inside parse_type() (E105)

		fn%type = type
	end if
	!print *, 'fn%type = ', fn%type

	! Copy for later return type checking while parsing body
	parser%fn_name = identifier%text
	parser%fn_type = fn%type

	! A `return` inside this body is a fn-scope return, not a module-scope one
	! (nested fns aren't allowed, but save/restore defensively anyway)
	in_fn_body0 = parser%in_fn_body
	parser%in_fn_body = .true.
	call parser%parse_statement(body)
	parser%in_fn_body = in_fn_body0

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
	decl%src_id     = fn_src_id
	decl%src_pos    = fn_beg
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

recursive module subroutine parse_type(parser, type_text, type)

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	type(value_t), intent(out) :: type

	!********

	integer :: rank, itype, i
	integer :: pos0, pos1, pos2

	character(len = :), allocatable :: cookie, suggest, param_type_text, ret_type_text

	type(syntax_token_t) :: colon, ident, comma, lbracket, rbracket, semi, dummy, &
		double_colon, fn_kw, lparen, rparen, amp

	type(text_span_t) :: span

	type(value_t) :: param_type, ret_type
	type(value_vector_t) :: param_types

	pos1 = parser%current_pos()

	if (parser%current_kind() == amp_token) then

		! References are only valid on fn parameters, which consume their
		! own leading `&` before calling parse_type() (c.f. parse_fn_call
		! and parse_method_declaration above).  Any `&` reaching here is in
		! a position that doesn't support references: return types, struct
		! member types, fn-pointer param/return types
		call parser%match(amp_token, amp)
		if (parser%current_kind() == const_keyword) call parser%next(dummy)

		span = new_span(amp%pos, parser%current_pos() - amp%pos)
		call parser%diagnostics%push(err_ref_type(parser%context(), span))

		! Fall through and parse the type normally so this doesn't cascade
		! into further bogus diagnostics
		pos1 = parser%current_pos()

	end if

	if (parser%current_kind() == fn_keyword) then

		! Fn-pointer type: fn(paramtype, paramtype, ...): rettype
		! Return type defaults to void if the ": rettype" suffix is omitted
		call parser%match(fn_keyword, fn_kw)
		type_text = "fn"

		call parser%match(lparen_token, lparen)

		param_types = new_value_vector()
		do while ( &
			parser%current_kind() /= rparen_token .and. &
			parser%current_kind() /= eof_token)

			pos0 = parser%pos

			call parser%parse_type(param_type_text, param_type)
			call param_types%push(param_type)

			if (parser%current_kind() /= rparen_token) then
				call parser%match(comma_token, comma)
			end if

			! Break infinite loop
			if (parser%pos == pos0) call parser%next(dummy)

		end do
		call parser%match(rparen_token, rparen)

		type%type = fn_type
		allocate(type%fn_params( param_types%len_ ))
		do i = 1, param_types%len_
			type%fn_params(i) = param_types%v(i)
		end do

		allocate(type%fn_ret)
		type%fn_ret%type = void_type
		if (parser%current_kind() == colon_token) then
			call parser%match(colon_token, colon)
			call parser%parse_type(ret_type_text, ret_type)
			type%fn_ret = ret_type
		end if

		return

	end if

	if (parser%current_kind() == lbracket_token) then

		! Array param
		call parser%match(lbracket_token, lbracket)
		call parser%match(identifier_token, ident)
		type_text = ident%text
		do while (parser%current_kind() == double_colon_token)
			! Qualified element type, e.g. [mod::Struct; :]
			call parser%match(double_colon_token, double_colon)
			call parser%match(identifier_token, ident)
			type_text = type_text//"::"//ident%text
		end do
		call parser%match(semicolon_token, semi)

		rank  = 0
		do while ( &
			parser%current_kind() /= rbracket_token .and. &
			parser%current_kind() /= eof_token)

			pos0 = parser%pos

			rank = rank + 1
			call parser%match(colon_token, colon)
			if (parser%current_kind() /= rbracket_token) then
				call parser%match(comma_token, comma)
			end if

			! break infinite loop
			if (parser%pos == pos0) call parser%next(dummy)

		end do
		!print *, 'rank = ', rank

		call parser%match(rbracket_token, rbracket)

	else
		! Scalar param
		call parser%match(identifier_token, ident)
		type_text = ident%text
		do while (parser%current_kind() == double_colon_token)
			! Qualified type, e.g. mod::Struct
			call parser%match(double_colon_token, double_colon)
			call parser%match(identifier_token, ident)
			type_text = type_text//"::"//ident%text
		end do
		rank = -1
	end if
	pos2 = parser%current_pos()

	itype = lookup_type(type_text, parser%structs, parser%enums, cookie)

	if (itype == unknown_type) then
		span = new_span(pos1, pos2 - pos1)
		suggest = parser%structs%closest(type_text)
		if (len(suggest) == 0) suggest = parser%enums%closest(type_text)
		call parser%diagnostics%push(err_bad_type( &
			parser%context(), span, type_text, suggest))
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
		type%struct_cookie = cookie
	else if (itype == enum_type) then
		type%enum_name = type_text
		type%enum_cookie = cookie
	end if

end subroutine parse_type

!===============================================================================

!===============================================================================

recursive module function all_paths_return(node) result(returns)

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
	!   switch_statement  -> returns iff else_clause (the `default` arm) is
	!                        present AND it returns AND every case_clause
	!                        member's body returns on all paths.
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

	case (switch_statement)
		! Requires a `default` arm (else_clause) that returns, AND every
		! case_clause arm's body (members(i)%body) to return on all paths.
		! A switch with no default is never guaranteed to return, since no
		! arm may match
		if (allocated(node%else_clause)) then
			returns = all_paths_return(node%else_clause)
			if (returns .and. allocated(node%members)) then
				do i = 1, size(node%members)
					if (.not. all_paths_return(node%members(i)%body)) then
						returns = .false.
						exit
					end if
				end do
			end if
		end if

	! while_statement / for_statement: body may run zero times -> no guarantee.
	! All other kinds: not a return.

	end select

end function all_paths_return

!===============================================================================

module subroutine check_call_arg(parser, arg, call_is_ref_i, arg_span, &
		fn_name, i_0based, param_val, param_name, param_is_ref, param_is_const_ref, &
		eff_is_ref)

	class(parser_t), intent(inout) :: parser
	type(syntax_node_t), intent(in) :: arg
	logical(kind = 1), intent(in) :: call_is_ref_i
	type(text_span_t), intent(in) :: arg_span
	character(len = *), intent(in) :: fn_name, param_name
	integer, intent(in) :: i_0based
	type(value_t), intent(in) :: param_val
	logical, intent(in) :: param_is_ref, param_is_const_ref
	logical(kind = 1), intent(out) :: eff_is_ref

	!********

	logical :: is_const_var, is_ok
	character(len = :), allocatable :: exp_type, act_type

	! Passing a const variable to a mutable-ref param is an error
	if (param_is_ref .and. .not. param_is_const_ref .and. arg%kind == name_expr) then
		if (arg%is_loc) then
			is_const_var = parser%locs%is_const(arg%identifier%text)
		else
			is_const_var = parser%vars%is_const(arg%identifier%text)
		end if
		if (is_const_var) then
			call parser%diagnostics%push(err_const_assign( &
				parser%context(), arg_span, arg%identifier%text))
		end if
	end if

	! Effective ref-ness of this argument.  Normally this is just whatever the
	! caller wrote (`&arg` or not).  But a `&const` param is a read-only
	! borrow: assigning to it inside the fn body is itself a parse error
	! (const_param, checked where params are declared), so the callee can
	! never write back through it, which lets a bare-name argument
	! transparently auto-borrow (no copy) even without an explicit `&` at
	! the call site.  Non-name args (literals, temporaries, subscripts,
	! etc.) stay by-value -- there is no caller slot to borrow.
	eff_is_ref = call_is_ref_i
	if (param_is_ref .and. param_is_const_ref .and. .not. call_is_ref_i .and. &
			arg%kind == name_expr .and. .not. allocated(arg%lsubscripts)) then
		eff_is_ref = .true.
	end if

	! Ref/val mismatch.  Only mutable `&` params require the caller to write
	! `&arg` explicitly; `&const` params accept plain value syntax (handled by
	! the auto-borrow above, so no mismatch here).
	if (param_is_ref .and. .not. param_is_const_ref .and. .not. call_is_ref_i) then
		call parser%diagnostics%push(err_bad_arg_val( &
			parser%context(), arg_span, fn_name, i_0based, param_name))
	else if (.not. param_is_ref .and. call_is_ref_i) then
		call parser%diagnostics%push(err_bad_arg_ref( &
			parser%context(), arg_span, fn_name, i_0based, param_name))
	end if

	! Void argument (no return value) -- reject before generic type-mismatch
	! check so it takes precedence and gives a clearer message
	if (arg%val%type == void_type) then
		call parser%diagnostics%push(err_void_arg( &
			parser%context(), arg_span, fn_name, i_0based, param_name))
		return
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

module subroutine check_ref_aliases(parser, args, is_ref, is_mut, spans, fn_name)

	! Called after every arg of a call has been through check_call_arg().
	! A by-ref arg is moved out of its variable for the duration of the call
	! and written back on return, so two by-ref args which share a root variable
	! can't both be live.  Passing the same variable by a mutable `&` ref and
	! by another ref is an error.  Several `&const` refs to one variable are
	! fine, and all but the first are passed by value (as a copy) instead
	!
	! The receiver of a method call is arg 1, and is by-ref even though it has
	! no `&` marker.  Only a bare name or a member/subscript chain from one has
	! a root variable.  A receiver or arg that is a fn call result is a
	! temporary and can't alias anything

	class(parser_t), intent(inout) :: parser
	type(syntax_node_t), intent(in) :: args(:)
	logical, intent(inout) :: is_ref(:)
	logical, intent(in) :: is_mut(:)
	type(text_span_t), intent(in) :: spans(:)
	character(len = *), intent(in) :: fn_name

	!********

	integer :: i, j

	do j = 2, size(args)
		if (.not. has_ref_root(args(j), is_ref(j))) cycle

		do i = 1, j - 1
			if (.not. has_ref_root(args(i), is_ref(i))) cycle
			if (args(i)%id_index /= args(j)%id_index) cycle
			if (args(i)%is_loc .neqv. args(j)%is_loc) cycle

			if (is_mut(i) .or. is_mut(j)) then
				call parser%diagnostics%push(err_ref_alias( &
					parser%context(), spans(j), args(j)%identifier%text, fn_name))
			else
				! All `&const`.  Reading the same variable through two const
				! refs is harmless, but it can only be moved onto the call stack
				! once, so pass this one as a copy
				is_ref(j) = .false.
			end if

			! One diagnostic per arg is enough
			exit
		end do
	end do

contains

logical function has_ref_root(arg, arg_is_ref)

	type(syntax_node_t), intent(in) :: arg
	logical, intent(in) :: arg_is_ref

	has_ref_root = .false.
	if (.not. arg_is_ref) return
	if (arg%root_kind /= 0) return
	has_ref_root = arg%kind == name_expr .or. arg%kind == dot_expr

end function has_ref_root

end subroutine check_ref_aliases

!===============================================================================

subroutine desugar_printf(parser, fn_call, new_name, ifmt)

	! Rewrite printf(fmt, values...) as print(format(fmt, values...)), and
	! writef(f, fmt, values...) as write(f, format(fmt, values...)).  ifmt is the
	! index of the format string arg, which is 1 for printf and 2 for writef.
	! The format has already been checked, and everything after parsing only
	! sees print() and write()

	class(parser_t), intent(in) :: parser
	type(syntax_node_t), intent(inout) :: fn_call
	character(len = *), intent(in) :: new_name
	integer, intent(in) :: ifmt

	!********

	integer :: i, nargs
	type(syntax_node_t) :: fmt_call
	type(syntax_node_t), allocatable :: args(:)

	nargs = size(fn_call%args)

	fmt_call%kind       = fn_call_intr_expr
	fmt_call%identifier = fn_call%identifier
	fmt_call%identifier%text = "format"
	fmt_call%val%type   = str_type
	fmt_call%id_index   = parser%fns%id_at(parser%fns%find("std::format"))

	allocate(fmt_call%args(nargs - ifmt + 1))
	allocate(fmt_call%is_ref(nargs - ifmt + 1))
	do i = ifmt, nargs
		call syntax_node_move_into(fn_call%args(i), fmt_call%args(i - ifmt + 1))
	end do
	fmt_call%is_ref = .false.

	! The file handle of writef() stays where it is, and the format() call is
	! the last arg
	allocate(args(ifmt))
	do i = 1, ifmt - 1
		call syntax_node_move_into(fn_call%args(i), args(i))
	end do
	call syntax_node_move_into(fmt_call, args(ifmt))

	call move_alloc(args, fn_call%args)

	fn_call%identifier%text = new_name
	fn_call%id_index = parser%fns%id_at(parser%fns%find("std::"//new_name))

	if (ifmt > 1) then
		fn_call%is_ref = [fn_call%is_ref(1: ifmt - 1), .false.]
	else
		fn_call%is_ref = [.false.]
	end if

end subroutine desugar_printf

!===============================================================================

module subroutine check_format_call(parser, fn_name, args, spans, call_span)

	! The format string of format() is checked against the rest of its arguments
	! when the call is parsed, so a literal is required.  Its placeholders are
	! `{}` or `{:spec}`, with one argument each.  The spec has to make sense for
	! the type of its argument, e.g. `x` is for integers.  The interpreter and
	! the Fortran backend then never see a bad format

	class(parser_t), intent(inout) :: parser
	character(len = *), intent(in) :: fn_name
	type(syntax_node_t), intent(in) :: args(:)
	type(text_span_t), intent(in) :: spans(:), call_span

	!********

	character(len = :), allocatable :: fmt, spec, why, tname

	integer :: i, ierr, nspecs, nargs, cat

	type(rt_fmt_spec_t) :: sp
	type(rt_str_t), allocatable :: pieces(:), specs(:)

	if (size(args) < 1) return

	! A format that isn't a string has already been reported as a type error
	if (args(1)%val%type /= str_type) return

	if (args(1)%kind /= literal_expr) then
		call parser%diagnostics%push(err_format_not_literal(parser%context(), spans(1), fn_name))
		return
	end if

	fmt = args(1)%val%str%s
	call rt_fmt_split(fmt, pieces, specs, ierr)
	if (ierr /= 0) then
		if (fmt(ierr: ierr) == '}') then
			why = 'unmatched `}` at position '//str(ierr)//' of the format string'
		else
			why = 'the `{` at position '//str(ierr)//' of the format string is ' &
				//'not closed, or is not `{}` or `{:spec}`'
		end if
		call parser%diagnostics%push(err_bad_format(parser%context(), spans(1), why))
		return
	end if

	nspecs = size(specs)
	nargs  = size(args) - 1
	if (nspecs /= nargs) then
		call parser%diagnostics%push(err_format_count(parser%context(), call_span, &
			nspecs, nargs))
		return
	end if

	do i = 1, nspecs

		spec = '{:'//specs(i)%s//'}'
		if (len(specs(i)%s) == 0) spec = '{}'

		call rt_fmt_parse(specs(i)%s, sp, ierr)
		if (ierr /= 0) then
			if (ierr <= len(specs(i)%s)) then
				why = 'unexpected `'//specs(i)%s(ierr: ierr)//'` at position '//str(ierr) &
					//' of the spec `'//spec//'`'
			else
				why = 'the spec `'//spec//'` ends where a digit is expected'
			end if
			call parser%diagnostics%push(err_bad_format(parser%context(), &
				spans(1), why))
			cycle
		end if

		associate (val => args(i+1)%val)

			tname = type_name(val)
			cat = format_category(val)

			why = ''
			select case (cat)
			case (0)
				why = 'only numbers, bools, strings, structs, enums, and arrays of ' &
					//'numbers, bools, and strings can be formatted'

			case (1)
				! Integers
				if (sp%ty == 'e' .or. sp%ty == 'E') then
					why = '`e` is for floats.  Integers can use `x`, `X`, `o`, or `b`'
				else if (sp%prec >= 0) then
					why = 'an integer has no precision'
				else if (sp%alt .and. sp%ty == ' ') then
					why = '`#` adds a prefix to `x`, `X`, `o`, or `b`'
				end if

			case (2)
				! Floats
				if (sp%ty /= ' ' .and. sp%ty /= 'e' .and. sp%ty /= 'E') then
					why = '`x`, `X`, `o`, and `b` are for integers.  Floats can use `e` or `E`'
				else if (sp%alt) then
					why = '`#` adds a prefix to integers in `x`, `X`, `o`, or `b`'
				end if

			case default
				! Bools, strings, enums, and structs are padded, and only strings
				! can be truncated by a precision
				if (sp%ty /= ' ') then
					why = '`'//sp%ty//'` is for numbers'
				else if (sp%plus .or. sp%alt .or. sp%zero) then
					why = '`+`, `#`, and `0` are for numbers'
				else if (sp%prec >= 0 .and. cat /= 3) then
					why = 'only a string has a precision, to truncate it'
				end if
			end select

			if (len(why) > 0) then
				call parser%diagnostics%push(err_format_type(parser%context(), &
					spans(i+1), spec, tname, why))
			end if

		end associate
	end do

contains

	function format_category(val) result(cat_)

		! 0  can not be formatted
		! 1  integer
		! 2  float
		! 3  string
		! 4  bool, enum, or struct

		type(value_t), intent(in) :: val
		integer :: cat_

		integer :: t

		t = val%type
		if (t == array_type) then
			if (.not. allocated(val%array)) then
				cat_ = 0
				return
			end if
			t = val%array%type
			if (t == struct_type .or. t == enum_type) then
				cat_ = 0
				return
			end if
		end if

		select case (t)
		case (i32_type, i64_type)
			cat_ = 1
		case (f32_type, f64_type)
			cat_ = 2
		case (str_type)
			cat_ = 3
		case (bool_type, enum_type, struct_type)
			cat_ = 4
		case default
			cat_ = 0
		end select

	end function format_category

end subroutine check_format_call

!===============================================================================

end submodule syntran__parse_fn

!===============================================================================

