
!===============================================================================

submodule (syntran__parse_m) syntran__parse_expr

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

recursive module subroutine parse_expr_statement(parser, expr)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: expr

	!********

	logical :: is_op_allowed, overwrite, is_const_var

	integer :: io, ltype, rtype, pos0, lrank, rrank, larrtype, &
		rarrtype, search_io, ndiag0, field_id, field_io

	type(value_t) :: field_val, self_val

	type(syntax_node_t) :: right
	type(syntax_token_t) :: let, identifier, op

	type(text_span_t) :: span

	!print *, 'starting parse_expr_statement()'

	! TODO: provide a way to declare variable types without initializing them?
	! Rust discourages mutability, instead preferring patterns like this:
	!
	!      let x = if condition
	!      {
	!          y
	!      }
	!      else
	!      {
	!          z
	!      };
	!
	! The above might be hard to do, as it would require checking that the types
	! of both condition branches match the LHS type

	if (parser%peek_kind(0) == const_keyword     .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		let        = parser%next()
		identifier = parser%next()
		op         = parser%next()

		call parser%parse_expr_statement(right)

		if (right%val%type ==  void_type) then
			span = new_span(let%pos, parser%current_pos() - let%pos)
			call parser%diagnostics%push( &
				err_void_assign(parser%context(), &
				span, identifier%text))
		end if

		call new_declaration_expr(identifier, op, right, expr)

		if (parser%is_loc) then
			parser%num_locs = parser%num_locs + 1
			expr%id_index   = parser%num_locs
			expr%is_loc = .true.
		else
			parser%num_vars = parser%num_vars + 1
			expr%id_index   = parser%num_vars
			expr%is_loc = .false.
		end if

		overwrite = .true.
		if (parser%ipass == 0) overwrite = .false.

		if (parser%is_loc) then
			call parser%locs%insert(identifier%text, expr%val, &
				expr%id_index, io, overwrite = overwrite, is_const = .true.)
		else
			call parser%vars%insert(identifier%text, expr%val, &
				expr%id_index, io, overwrite = overwrite, is_const = .true.)

			if (parser%ipass == 0) call parser%var_names%push(identifier%text)
		end if

		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(parser%context(), &
				span, identifier%text))
		end if

		return

	end if

	if (parser%peek_kind(0) == let_keyword      .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		! TODO: refactor this as parse_let_expr()

		!print *, 'let expr'

		! The if-statement above already verifies tokens, so we can use next()
		! instead of match() here

		let        = parser%next()
		identifier = parser%next()
		!print *, 'let ident = ', identifier%text

		op         = parser%next()

		call parser%parse_expr_statement(right)
		!right      = parser%parse_expr()

		if (right%val%type ==  void_type) then
			span = new_span(let%pos, parser%current_pos() - let%pos)
			call parser%diagnostics%push( &
				err_void_assign(parser%context(), &
				span, identifier%text))
		end if

		!! I think the way to get conditional initialization like rust is
		!! something like this.  May need to peek current and check if it's
		!! if_keyword or not
		!right      = parser%parse_statement()
		!!semi       = parser%match(semicolon_token)

		call new_declaration_expr(identifier, op, right, expr)

		!print *, "right type = ", kind_name(right%val%type)
		!print *, "expr  type = ", kind_name(expr %val%type)
		!print *, "right struct = ", right%val%struct_name
		!print *, "expr  struct = ", expr %val%struct_name

		! Increment the variable array index and save it in the expr node.
		! TODO: make this a push_var fn?  parse_for_statement uses it too
		if (parser%is_loc) then
			parser%num_locs = parser%num_locs + 1
			expr%id_index   = parser%num_locs
			expr%is_loc = .true.
		else
			parser%num_vars = parser%num_vars + 1
			expr%id_index   = parser%num_vars
			expr%is_loc = .false.
		end if

		!if (expr%val%type == array_type) then
		!	print *, 'array_type'
		!	print *, 'rank = ', expr%val%array%rank
		!end if

		overwrite = .true.
		if (parser%ipass == 0) overwrite = .false.

		! Insert the identifier's type into the dict and check that it
		! hasn't already been declared
		!print *, "inserting var"
		!print *, "parser is_loc = ", parser%is_loc
		if (parser%is_loc) then
			call parser%locs%insert(identifier%text, expr%val, &
				expr%id_index, io, overwrite = overwrite)
		else
			call parser%vars%insert(identifier%text, expr%val, &
				expr%id_index, io, overwrite = overwrite)

			! Track module-level variable names (like fn_names for functions)
			if (parser%ipass == 0) call parser%var_names%push(identifier%text)
		end if

		!print *, 'io = ', io
		if (io /= exit_success) then
			!print *, "expr redeclare"
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(parser%context(), &
				span, identifier%text))
		end if

		return

	end if

	! Handle qualified assignment: mod::var = value, mod::arr[i] = value,
	! or mod::struct.field = value
	if (parser%peek_kind(0) == identifier_token .and. &
	    parser%peek_kind(1) == double_colon_token) then

		pos0 = parser%pos
		ndiag0 = parser%diagnostics%len_

		! Parse the qualified name (mod::var or mod1::mod2::var)
		identifier = parser%match(identifier_token)
		expr%module_prefix = identifier%text

		op = parser%match(double_colon_token)

		identifier = parser%match(identifier_token)

		! Handle nested namespaces: mod1::mod2::var
		do while (parser%current_kind() == double_colon_token)
			expr%module_prefix = expr%module_prefix // "::" // identifier%text
			op = parser%match(double_colon_token)
			identifier = parser%match(identifier_token)
		end do

		! Look up the qualified variable
		expr%identifier = identifier
		is_const_var = .false.
		call parser%vars%search(expr%module_prefix // "::" // identifier%text, &
			expr%id_index, search_io, expr%val, is_const = is_const_var)

		! Parse subscripts and dot access for qualified names
		call parser%parse_subscripts(expr)

		if (parser%peek_kind(0) == dot_token) then
			if (search_io /= exit_success) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(parser%context(), span, identifier%text, &
					parser%vars%closest(identifier%text)))
			end if

			call parser%parse_dot(expr)
			if (.not. allocated(expr%member)) then
				return
			end if
		end if

		if (.not. is_assignment_op(parser%current_kind())) then
			! Not an assignment, rewind and let parse_expr handle it.  Also
			! discard any diagnostics pushed during this speculative parse
			! (e.g. from parse_subscripts/parse_dot) since parse_expr will
			! re-parse the same tokens and re-push them, causing duplicates
			parser%pos = pos0
			parser%diagnostics%len_ = ndiag0
		else
			! It's a qualified assignment

			if (expr%module_prefix == "std") then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_immutable_var(parser%context(), span, &
					expr%module_prefix // "::" // identifier%text))
			else if (is_const_var .and. search_io == exit_success) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_const_assign(parser%context(), span, &
					expr%module_prefix // "::" // identifier%text))
			end if

			if (search_io /= exit_success .and. .not. allocated(expr%member)) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(parser%context(), span, identifier%text, &
					parser%vars%closest(identifier%text)))
			end if

			expr%is_loc = .false.

			op    = parser%next()
			call parser%parse_expr_statement(right)

			expr%kind = assignment_expr
			expr%op   = op
			call syntax_node_move(right, expr%right)

			ltype = expr%val%type
			rtype = expr%right%val%type

			larrtype = unknown_type
			rarrtype = unknown_type
			if (ltype == array_type) larrtype = expr%val%array%type
			if (rtype == array_type) rarrtype = expr%right%val%array%type

			is_op_allowed = is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)

			if (.not. is_op_allowed) then
				span = new_span(op%pos, len(op%text))
				call parser%diagnostics%push( &
					err_binary_types(parser%context(), &
					span, op%text, &
					type_name(expr%val), &
					type_name(expr%right%val)))
			end if

			if (ltype == array_type .and. rtype == array_type) then
				lrank = expr%val%array%rank
				rrank = expr%right%val%array%rank

				if (lrank /= rrank) then
					span = new_span(op%pos, len(op%text))
					call parser%diagnostics%push( &
						err_binary_ranks(parser%context(), &
						span, op%text, lrank, rrank))
				end if
			end if

			return
		end if
	end if

	if (parser%peek_kind(0) == identifier_token) then

		! There may or may not be a subscript expression after an identifier, so
		! we can't know how many spaces ahead an equals_token might be without
		! looking ahead

		! %pos is the lexer token index, %current_pos() is the character index!
		pos0 = parser%pos
		ndiag0 = parser%diagnostics%len_

		!print *, "assign expr"

		identifier = parser%match(identifier_token)

		! this makes `identifier` a redundant copy, although a convenient
		! shorthand. we need expr%identifier for error handling inside
		! parse_subscripts()
		expr%identifier = identifier;

		!print *, "ident = ", identifier%text

		! Parse array subscript indices if present

		! Subscript can appear in assignment expr but not let expr, because let
		! must initialize the whole array.  Similarly for dot member access

		! Delay the error-handling on search_io because we might end up rewinding

		! TODO: make this a parser%search() fn to wrap loc and vars (global)
		! searches?

		!print *, "searching identifier ", identifier%text

		is_const_var = .false.
		if (parser%is_loc) then
			call parser%locs%search(identifier%text, expr%id_index, search_io, expr%val, &
				is_const = is_const_var)
		end if

		if (parser%is_loc .and. search_io == 0) then
			expr%is_loc = .true.
		else
			call parser%vars%search(identifier%text, expr%id_index, search_io, expr%val, &
				is_const = is_const_var)
		end if

		! Check if this is an implicit field access inside a method body
		if (parser%in_method .and. search_io /= exit_success) then
			call parser%method_struct%vars%search(identifier%text, field_id, field_io, field_val)
			if (field_io == exit_success) then
				! Transform to dot_expr("0self", field) for assignment LHS
				call parser%locs%search("0self", expr%id_index, search_io, self_val)
				expr%is_loc = .true.
				expr%val    = self_val

				! Reject writes in const fn
				if (parser%in_const_method) then
					span = new_span(identifier%pos, len(identifier%text))
					call parser%diagnostics%push( &
						err_const_assign(parser%context(), span, identifier%text))
				end if

				allocate(expr%member)
				expr%member%id_index   = field_id
				expr%member%val        = field_val
				expr%member%identifier = identifier
				expr%val = field_val
				call parser%parse_subscripts(expr%member)
				expr%val = expr%member%val
				if (parser%current_kind() == dot_token) then
					expr%member%identifier = identifier
					call parser%parse_dot(expr%member)
					expr%val = expr%member%val
				end if
				is_const_var = .false.
			end if
		end if

		call parser%parse_subscripts(expr)

		if (parser%peek_kind(0) == dot_token) then
			!print *, "dot token"

			if (search_io /= exit_success .and. .not. allocated(expr%member)) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(parser%context(), &
					span, identifier%text, &
					parser%vars%closest(identifier%text)))
			end if

			if (.not. allocated(expr%member)) then
				call parser%parse_dot(expr)
				! Return early only on actual error (no member, not a method call)
				if (.not. allocated(expr%member) .and. &
				    expr%kind /= method_call_expr) then
					!print *, "RETURNING ******"
					return
				end if
			end if

		end if

		if (.not. is_assignment_op(parser%current_kind())) then
			! Rewind and do the default case (same as outside the assignment if
			! block).  Could use goto or probably refactor somehow
			parser%pos = pos0
			!print *, "rewinding ********"
			!print *, 'pos0 = ', pos0
			! Discard diagnostics pushed during this speculative parse (e.g.
			! from parse_subscripts/parse_dot above): parse_expr below
			! re-parses the same tokens and would otherwise re-push them,
			! causing duplicates
			parser%diagnostics%len_ = ndiag0
			call parser%parse_expr(expr=expr)
			return
		end if
		!print *, 'parsing assignment'

		! Block assignment to a const variable (covers plain, compound,
		! subscript, and member cases — all share this code path)
		if (is_const_var .and. search_io == exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_const_assign(parser%context(), span, identifier%text))
		end if

		op    = parser%next()
		call parser%parse_expr_statement(right)
		!print *, "1a right index = ", right%right%id_index

		! regular vs compound assignment exprs are denoted by the op.  all of
		! them are the same kind
		expr%kind = assignment_expr

		expr%identifier = identifier
		expr%op         = op
		call syntax_node_move(right, expr%right)

		!print *, 'expr ident text = ', expr%identifier%text
		!print *, 'op = ', op%text

		! Get the identifier's type and index from the dict and check that it
		! has been declared, unless it is a struct which has already been looked
		! up above
		if (.not. allocated(expr%member)) then
			if (search_io /= exit_success) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(parser%context(), &
					span, identifier%text, &
					parser%vars%closest(identifier%text)))
				!print *, "undeclared var 2"
				!print *, "identifier = ", identifier%text
			end if
		end if

		!print *, 'type = ', kind_name(expr%val%type)
		!print *, 'allocated(expr%val%array) = ', allocated(expr%val%array)

		ltype = expr%val%type
		rtype = expr%right%val%type

		larrtype = unknown_type
		rarrtype = unknown_type
		if (ltype == array_type) larrtype = expr%val%array%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type

		!print *, "larrtype = ", kind_name(larrtype)
		!print *, "rarrtype = ", kind_name(rarrtype)
		!print *, "ltype    = ", kind_name(ltype)
		!print *, "rtype    = ", kind_name(rtype)

		is_op_allowed = is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)
		if (ltype == struct_type .and. is_op_allowed) then
			! Prefer the alias-independent struct_cookie (set at struct
			! declaration time) so the same struct reached via two different
			! module aliases/import paths is still recognized as the same
			! type.  Fall back to struct_name if either side lacks a cookie
			if (allocated(expr%val%struct_cookie) .and. &
				allocated(expr%right%val%struct_cookie)) then
				if (expr%val%struct_cookie /= expr%right%val%struct_cookie) &
					is_op_allowed = .false.
			else if (expr%val%struct_name /= expr%right%val%struct_name) then
				! TODO: this is a one-off check for assignment of one struct to
				! another. It should really be inside of is_binary_op_allowed(),
				! but I should change is_binary_op_allowed() to take 2 value_t
				! args, instead of a bunch of int args as-is
				is_op_allowed = .false.
			end if
		end if

		! This check could be moved inside of is_binary_op_allowed, but we would
		! need to pass parser to it to push diagnostics
		if (.not. is_op_allowed) then
			!print *, 'bin not allowed in parse_expr_statement'
			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context(), &
				span, op%text, &
				type_name(expr%val), &
				type_name(expr%right%val)))
		end if

		if (ltype == array_type .and. rtype == array_type) then

			lrank = expr%val%array%rank
			rrank = expr%right%val%array%rank

			if (lrank /= rrank) then
				span = new_span(op%pos, len(op%text))
				call parser%diagnostics%push( &
					err_binary_ranks(parser%context(), &
					span, op%text, &
					lrank, &
					rrank))
			end if
		end if

		return

	end if

	call parser%parse_expr(expr=expr)
	!semi       = parser%match(semicolon_token)

end subroutine parse_expr_statement

!===============================================================================

recursive module subroutine parse_expr(parser, parent_prec, expr)

	! In episode 3, Immo renamed this fn to "ParseBinaryExpression()", but
	! I consider that confusing because the result could be either unary or
	! binary

	class(parser_t) :: parser

	integer, optional, intent(in) :: parent_prec
	type(syntax_node_t), intent(out) :: expr

	!********

	integer :: parent_precl, prec, ltype, rtype, larrtype, rarrtype, &
		lrank, rrank

	type(syntax_node_t) :: right, bin_tmp
	type(syntax_token_t) :: op
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_expr'
	if (debug > 1) print *, 'pos = ', parser%pos

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		call parser%parse_expr(prec, right)
		call new_unary_expr(op, right, expr)

		rtype = right%val%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type

		if (.not. is_unary_op_allowed(op%kind, rtype, rarrtype)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_unary_types(parser%context(), span, op%text, &
				kind_name(expr%right%val%type)))

		end if

	else
		call parser%parse_primary_expr(expr)
	end if

	do
		prec = get_binary_op_prec(parser%current_kind())
		if (prec == 0 .or. prec <= parent_precl) exit

		op    = parser%next()
		call parser%parse_expr(prec, right)
		call new_binary_expr(expr, op, right, bin_tmp)
		call syntax_node_move_into(bin_tmp, expr)

		ltype = expr%left %val%type
		rtype = expr%right%val%type

		larrtype = unknown_type
		rarrtype = unknown_type
		if (ltype == array_type) larrtype = expr%left %val%array%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type

		!print *, 'larrtype = ', kind_name(larrtype)
		!print *, 'rarrtype = ', kind_name(rarrtype)
		!print *, 'ltype = ', kind_name(ltype)
		!print *, 'rtype = ', kind_name(rtype)

		if (.not. is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)) then

			!print *, 'bin not allowed in parse_expr'

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context(), &
				span, op%text, &
				kind_name(ltype), &
				kind_name(rtype)))

		end if

		if (ltype == array_type .and. rtype == array_type &
				.and. op%kind /= matmul_token) then
			!print *, 'double array operation'

			lrank = expr%left %val%array%rank
			rrank = expr%right%val%array%rank

			!print *, 'left  rank = ', lrank
			!print *, 'right rank = ', rrank

			if (lrank /= rrank) then
				span = new_span(op%pos, len(op%text))
				call parser%diagnostics%push( &
					err_binary_ranks(parser%context(), &
					span, op%text, &
					lrank, &
					rrank))
			end if
		end if

	end do

end subroutine parse_expr

!===============================================================================

recursive module subroutine parse_primary_expr(parser, expr)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: expr

	!********

	logical :: bool, exists

	type(syntax_token_t) :: left, right, keyword, token

	if (debug > 1) print *, 'parse_primary_expr'

	select case (parser%current_kind())

		case (lparen_token)

			! Left and right parens are not explicitly included as nodes in the
			! parse tree, they just change the connectivity of the tree

			left  = parser%next()

			! These two lines are the difference between allowing statement
			! "a = (b = 1)" or not.  Note that "a = b = 1" is allowed either way

			!expr  = parser%parse_expr()
			call parser%parse_expr_statement(expr)

			right = parser%match(rparen_token)

		case (lbracket_token)

			! Brackets are matched within parse_array_expr
			call parser%parse_array_expr(expr)

			!print *, '2 expr%val%type = ', expr%val%type
			!print *, '2 expr%val%array%type = ', expr%val%array%type

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			call new_bool(bool, expr)

			!print *, 'expr%val%sca%bool = ', expr%val%sca%bool

		case (identifier_token)

			!print *, "parser%peek_kind(1) = ", kind_name(parser%peek_kind(1))

			if (parser%peek_kind(1) == double_colon_token) then
				! Qualified name like `std::println()` or `mod::fn()`
				call parser%parse_qualified_expr(expr)
			else if (parser%peek_kind(1) == lparen_token) then
				call parser%parse_fn_call(fn_call=expr)
				if (parser%current_kind() == dot_token .and. &
						expr%val%type == struct_type) then
					call parser%parse_dot(expr)
				end if
			else if (parser%peek_kind(1) == lbrace_token) then

				! There is an ambiguity here because struct instantiators and
				! block statements both look similar, using braces{}.  Compare
				! an if statement:
				!
				!     if my_bool
				!     { ...
				!     }
				!
				!
				! To a struct instantiator:
				!
				!     let my_struct = Struct
				!     { ...
				!     };
				!
				! We resolve this by looking up the identifier ("my_bool" vs
				! "Struct") in the structs dict.  Alternatively, I could change
				! syntran to use a different token for struct instantiators,
				! e.g. `.{`, but I prefer this solution.

				! The exists() method is not strictly needed.  Search could work
				! and simplify the code.  I was experimenting while debugging
				! memory issue, but exists is not necessary.  On the other hand,
				! it might be more optimal to check existence w/o copying an
				! output val (which could containt big nested dict types)

				!print *, "text = ", parser%current_text()
				!dummy = parser%structs%search(parser%current_text(), dummy_id, io)
				exists = parser%structs%exists(parser%current_text())
				!deallocate(dummy%members)
				!deallocate(dummy%vars)
				!print *, "io = ", io

				!if (io == 0) then
				if (exists) then
					call parser%parse_struct_instance(expr)
					!print *, "back in parse_expr.f90"
				else
					! Same as default case below
					call parser%parse_name_expr(expr)
				end if

			else
				call parser%parse_name_expr(expr)
			end if

		case (f32_token)

			token = parser%match(f32_token)
			call new_f32(token%val%sca%f32, expr)

		case (f64_token)

			token = parser%match(f64_token)
			call new_f64(token%val%sca%f64, expr)

		case (str_token)

			token = parser%match(str_token)
			call new_str(token%val%str%s, expr)

		case (i64_token)

			token = parser%match(i64_token)
			call new_i64(token%val%sca%i64, expr)

		case default

			token = parser%match(i32_token)
			call new_i32(token%val%sca%i32, expr)

			if (debug > 1) print *, 'token = ', expr%val%to_str()

	end select

end subroutine parse_primary_expr

!===============================================================================

recursive module subroutine parse_name_expr(parser, expr)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: expr

	!********

	integer :: io, id_index, field_id, field_io

	type(syntax_token_t) :: identifier
	type(text_span_t) :: span

	type(value_t) :: var, field_val

	! Variable name expression

	identifier = parser%match(identifier_token)

	!print *, "RHS identifier = ", identifier%text
	!print *, "parser%is_loc = ", parser%is_loc
	!print *, '%current_kind() = ', kind_name(parser%current_kind())

	!print *, 'searching'

	if (parser%is_loc) then
		call parser%locs%search(identifier%text, id_index, io, var)
	end if

	if (parser%is_loc .and. io == 0) then
		call new_name_expr(identifier, var, expr)
		expr%id_index = id_index
		expr%is_loc = .true.
	else
		call parser%vars%search(identifier%text, id_index, io, var)
		call new_name_expr(identifier, var, expr)
		expr%id_index = id_index
		expr%is_loc = .false.

		if (io /= 0) then

			! Check if this is a struct field name in scope via implicit self
			if (parser%in_method) then
				call parser%method_struct%vars%search(identifier%text, field_id, field_io, field_val)
				if (field_io == exit_success) then
					! Build dot_expr("0self", field) inline
					expr%kind       = dot_expr
					expr%id_index   = parser%self_loc_id
					expr%is_loc     = .true.
					expr%identifier = identifier
					allocate(expr%member)
					expr%member%id_index   = field_id
					expr%member%val        = field_val
					expr%member%identifier = identifier
					expr%val = field_val
					call parser%parse_subscripts(expr%member)
					expr%val = expr%member%val
					if (parser%current_kind() == dot_token) then
						call parser%parse_dot(expr%member)
						expr%val = expr%member%val
					end if
					return
				end if
			end if

			!print *, "undeclared var 3"
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context(), &
				span, identifier%text, &
				parser%vars%closest(identifier%text)))
		end if
	end if

	!print *, 'type = ', kind_name(expr%val%type)
	!print *, 'allocated(expr%val%array) = ', &
	!	allocated(expr%val%array)

	!print *, '%current_kind() = ', kind_name(parser%current_kind())
	call parser%parse_subscripts(expr)

	!print *, "expr%val%type = ", kind_name(expr%val%type)

	!print *, "tail parse_dot"
	call parser%parse_dot(expr)

end subroutine parse_name_expr

!===============================================================================

recursive module subroutine parse_dot(parser, expr)

	class(parser_t) :: parser

	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: io, struct_id, member_id, pos0, pos1, method_i, method_fn_id, method_io, &
		i, id_index_tmp, io_tmp

	logical :: call_arg_is_ref, is_ok, param_is_ref, param_is_const_ref, &
		is_const_var, is_const_receiver

	character(len = :), allocatable :: exp_type, act_type, param_name

	type(fn_t) :: method_fn

	type(struct_t) :: struct

	type(syntax_node_t) :: receiver_save, arg_, receiver_cand, method_cand

	type(syntax_node_vector_t) :: call_args

	type(logical_vector_t) :: call_is_ref

	type(integer_vector_t) :: pos_args

	type(syntax_token_t) :: dot, identifier, lparen_, rparen_, comma_, amp_

	type(text_span_t) :: span

	type(value_t) :: member, param_val, const_check_val

	if (parser%current_kind() /= dot_token) return

	!print *, "parsing dot"
	!print *, "expr type = ", type_name(expr%val)

	dot  = parser%match(dot_token)

	identifier = parser%match(identifier_token)

	!print *, "dot identifier = ", identifier%text
	!print *, "type = ", kind_name(expr%val%type)

	if (expr%val%type /= struct_type) then
		! Don't cascade errors for undeclared vars
		if (expr%val%type /= unknown_type) then
			! Does expr%identifier always exist to create a span?  May need to just
			! underline dot itself.  I've tested this with arrays of structs
			! `struct_array[0].member` and nested dot exprs `a.b.c.z`
			span = new_span(expr%identifier%pos, dot%pos - expr%identifier%pos + 1)
			call parser%diagnostics%push(err_non_struct_dot( &
				parser%context(), &
				span, &
				expr%identifier%text))
		end if
		return
	end if

	!print *, "struct_name = """, expr%val%struct_name, """"

	! Is there a better way than looking up every struct by name again?
	call parser%structs%search(expr%val%struct_name, struct_id, io, struct)
	if (io /= 0) then
		! Type is already confirmed as struct_type above, so I'm fairly sure
		! this is unreachable
		write(*,*) err_int(IC_UNREACHABLE_STRUCT_LOOKUP, "unreachable struct lookup failure")
		call internal_error()
	end if

	! Check if the identifier is a method name on this struct
	method_fn = parser%fns%search( &
		"0" // expr%val%struct_name // "::" // identifier%text, &
		method_fn_id, method_io)

	if (method_io == exit_success) then

		! Const receiver enforcement: mutable methods may not be called on const instances
		if (.not. method_fn%is_const_method) then
			if (expr%kind == fn_call_expr .or. expr%kind == method_call_expr .or. &
					expr%kind == fn_call_intr_expr) then
				! Mutable method on a temporary fn-return value: mutation would be silently lost
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push(err_mutable_method_on_temp( &
					parser%context(), span, identifier%text))
				! Consume argument list for error recovery
				lparen_ = parser%match(lparen_token)
				do while (parser%current_kind() /= rparen_token .and. &
				          parser%current_kind() /= eof_token)
					call parser%parse_expr(expr = arg_)
					if (parser%current_kind() /= rparen_token) comma_ = parser%match(comma_token)
				end do
				rparen_ = parser%match(rparen_token)
				expr%val%type = unknown_type
				return
			end if
			if (expr%kind == name_expr) then
				is_const_receiver = .false.
				if (expr%is_loc) then
					call parser%locs%search(expr%identifier%text, &
						id_index_tmp, io_tmp, const_check_val, is_const = is_const_receiver)
				else
					call parser%vars%search(expr%identifier%text, &
						id_index_tmp, io_tmp, const_check_val, is_const = is_const_receiver)
				end if
				if (is_const_receiver) then
					span = new_span(identifier%pos, len(identifier%text))
					call parser%diagnostics%push(err_const_assign( &
						parser%context(), span, expr%identifier%text))
				end if
			end if
		end if

		! Save receiver before overwriting expr
		receiver_save = expr

		! Parse explicit argument list
		call_args   = new_syntax_node_vector()
		call_is_ref = new_logical_vector()
		pos_args    = new_integer_vector()

		lparen_ = parser%match(lparen_token)

		do while (parser%current_kind() /= rparen_token .and. &
		          parser%current_kind() /= eof_token)
			pos0 = parser%pos

			call pos_args%push(parser%current_pos())

			call_arg_is_ref = .false.
			if (parser%current_kind() == amp_token) then
				amp_ = parser%match(amp_token)
				call_arg_is_ref = .true.
			end if
			call call_is_ref%push(call_arg_is_ref)

			call parser%parse_expr(expr = arg_)
			call call_args%push(arg_)

			if (parser%current_kind() /= rparen_token) then
				comma_ = parser%match(comma_token)
			end if

			if (parser%pos == pos0) amp_ = parser%next()
		end do
		call pos_args%push(parser%current_pos() + 1)

		rparen_ = parser%match(rparen_token)

		! Validate explicit arg count.
		! method_fn%params holds only explicit params (self is NOT included there).
		! method_fn%node%params / is_ref / is_const_ref include self at index 1.
		if (allocated(method_fn%node)) then
			if (allocated(method_fn%params)) then
				if (size(method_fn%params) /= call_args%len_) then
					span = new_span(lparen_%pos, rparen_%pos - lparen_%pos + 1)
					call parser%diagnostics%push( &
						err_bad_arg_count(parser%context(), &
						span, identifier%text, size(method_fn%params), call_args%len_))
					expr%val%type = unknown_type
					return
				end if
			else if (call_args%len_ /= 0) then
				span = new_span(lparen_%pos, rparen_%pos - lparen_%pos + 1)
				call parser%diagnostics%push( &
					err_bad_arg_count(parser%context(), &
					span, identifier%text, 0, call_args%len_))
				expr%val%type = unknown_type
				return
			end if

			! Validate each explicit arg.
			! node%is_ref/is_const_ref index i+1 because self is at index 1.
			! fn%params and fn%param_names index i (only explicit params).
			do i = 1, call_args%len_
				param_val  = method_fn%params(i)
				param_name = method_fn%param_names%v(i)%s

				param_is_ref       = .false.
				param_is_const_ref = .false.
				param_is_ref = method_fn%node%is_ref(i + 1)
				if (allocated(method_fn%node%is_const_ref)) &
					param_is_const_ref = method_fn%node%is_const_ref(i + 1)

				span = new_span(pos_args%v(i), pos_args%v(i+1) - pos_args%v(i) - 1)
				call check_call_arg(parser, call_args%v(i), call_is_ref%v(i), span, &
					identifier%text, i - 1, param_val, param_name, &
					param_is_ref, param_is_const_ref)
			end do
		end if

		! Build method_call_expr node in expr
		expr%kind       = method_call_expr
		expr%identifier = identifier
		expr%id_index   = method_fn_id
		expr%val        = method_fn%type

		! args: receiver first, then explicit args
		if (allocated(expr%args))   deallocate(expr%args)
		if (allocated(expr%is_ref)) deallocate(expr%is_ref)
		if (allocated(expr%body))   deallocate(expr%body)
		allocate(expr%args(1 + call_args%len_))
		expr%args(1) = receiver_save
		do method_i = 1, call_args%len_
			expr%args(1 + method_i) = call_args%v(method_i)
		end do

		! is_ref: self always by-ref, explicit args by call-site marker
		allocate(expr%is_ref(1 + call_args%len_))
		expr%is_ref(1) = .true.
		do method_i = 1, call_args%len_
			expr%is_ref(1 + method_i) = call_is_ref%v(method_i)
		end do

		! Copy fn body and params from the method's fn node
		if (allocated(method_fn%node)) then
			allocate(expr%body)
			expr%body     = method_fn%node%body
			expr%params   = method_fn%node%params
			expr%num_locs = method_fn%node%num_locs
		end if

		return
	end if

	! For RHS dots, this will stick.  For LHS dots, this will be shortly
	! overwritten as assignment_expr in the caller
	expr%kind = dot_expr

	! Save dot info in member syntax node
	allocate(expr%member)

	call struct%vars%search(identifier%text, member_id, io, member)
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_bad_member_name( &
			parser%context(), &
			span, &
			identifier%text, &
			expr%identifier%text, &
			expr%val%struct_name))
		expr%val%type = unknown_type  ! this prevents cascades later
		return
	end if
	!print *, "member id = ", member_id
	!print *, "mem type  = ", kind_name(member%type)

	expr%member%id_index = member_id
	expr%val = member

	! I think this is the right place to parse subscripts. Or should it be after
	! the recursive parse_dot()?
	expr%member%val = member
	pos0 = parser%current_pos()
	call parser%parse_subscripts(expr%member)
	pos1 = parser%current_pos()
	if (allocated(expr%member%lsubscripts)) then
		expr%val = expr%member%val
	end if

	! I think this needs a recursive call to `parse_dot()` right here to handle
	! things like `a.b.c`
	if (parser%peek_kind(0) == dot_token) then
		expr%member%val = expr%val
		expr%member%identifier = identifier  ! set for diags in recursed parse_dot()

		! Save the current expr (e.g. y.b) before the recursive call can turn
		! expr%member into a method_call_expr.  Used to build the proper receiver
		! if a method is found deeper in the chain.
		receiver_cand = expr

		call parser%parse_dot(expr%member)
		expr%val = expr%member%val

		! If the recursive call resolved a method call, restructure so the
		! full chain (receiver_cand) is the method's receiver rather than just
		! the innermost sub-node.
		if (expr%member%kind == method_call_expr) then
			! receiver_cand is the outer dot_expr (e.g. y.b).  Its member
			! should be the inner partial chain produced by the recursive fix
			! (rather than the b-subnode copy from the save above).
			receiver_cand%member = expr%member%args(1)
			method_cand          = expr%member        ! deep-copy the method node
			method_cand%args(1)  = receiver_cand      ! replace receiver with full chain
			expr                 = method_cand         ! promote to top-level method call
		end if
	end if

end subroutine parse_dot

!===============================================================================

end submodule syntran__parse_expr

!===============================================================================

