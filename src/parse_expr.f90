
!===============================================================================

submodule (syntran__parse_m) syntran__parse_expr

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

recursive module function parse_expr_statement(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	logical :: is_op_allowed, overwrite

	integer :: io, ltype, rtype, pos0, lrank, rrank, larrtype, &
		rarrtype, search_io

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

	if (parser%peek_kind(0) == let_keyword      .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		!print *, 'let expr'

		! The if-statement above already verifies tokens, so we can use next()
		! instead of match() here

		let        = parser%next()
		identifier = parser%next()
		!print *, 'let ident = ', identifier%text

		op         = parser%next()

		right      = parser%parse_expr_statement()
		!right      = parser%parse_expr()

		!! I think the way to get conditional initialization like rust is
		!! something like this.  May need to peek current and check if it's
		!! if_keyword or not
		!right      = parser%parse_statement()
		!!semi       = parser%match(semicolon_token)

		expr = new_declaration_expr(identifier, op, right)

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
		end if

		!print *, 'io = ', io
		if (io /= exit_success) then
			!print *, "expr redeclare"
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(parser%context(), &
				span, identifier%text))
			!stop
		end if

		return

	end if

	if (parser%peek_kind(0) == identifier_token) then

		! There may or may not be a subscript expression after an identifier, so
		! we can't know how many spaces ahead an equals_token might be without
		! looking ahead

		! %pos is the lexer token index, %current_pos() is the character index!
		pos0 = parser%pos

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

		if (parser%is_loc) then
			call parser%locs%search(identifier%text, expr%id_index, search_io, expr%val)
			!print *, "locs io = ", search_io
		end if

		if (parser%is_loc .and. search_io == 0) then
			expr%is_loc = .true.
			!print *, "loc type = ", kind_name(expr%val%type)
		else
			call parser%vars%search(identifier%text, expr%id_index, search_io, expr%val)
		end if

		call parser%parse_subscripts(expr)

		if (parser%peek_kind(0) == dot_token) then
			!print *, "dot token"

			if (search_io /= exit_success) then
				span = new_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(parser%context(), &
					span, identifier%text))
			end if

			call parser%parse_dot(expr)
			if (.not. allocated(expr%member)) then
				!print *, "RETURNING ******"
				return
			end if

		end if

		if (.not. is_assignment_op(parser%current_kind())) then
			! Rewind and do the default case (same as outside the assignment if
			! block).  Could use goto or probably refactor somehow
			parser%pos = pos0
			!print *, "rewinding ********"
			!print *, 'pos0 = ', pos0
			expr = parser%parse_expr()
			return
		end if
		!print *, 'parsing assignment'

		op    = parser%next()
		right = parser%parse_expr_statement()
		!print *, "1a right index = ", right%right%id_index

		! regular vs compound assignment exprs are denoted by the op.  all of
		! them are the same kind
		expr%kind = assignment_expr

		if (.not. allocated(expr%right)) allocate(expr%right)

		expr%identifier = identifier

		expr%op    = op
		expr%right = right

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
					span, identifier%text))
				!print *, "undeclared var 2"
				!print *, "identifier = ", identifier%text
				!stop
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
			if (expr%val%struct_name /= expr%right%val%struct_name) then
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

	expr = parser%parse_expr()
	!semi       = parser%match(semicolon_token)

end function parse_expr_statement

!===============================================================================

recursive module function parse_expr(parser, parent_prec) result(expr)

	! In episode 3, Immo renamed this fn to "ParseBinaryExpression()", but
	! I consider that confusing because the result could be either unary or
	! binary

	class(parser_t) :: parser

	integer, optional, intent(in) :: parent_prec

	type(syntax_node_t) :: expr

	!********

	integer :: parent_precl, prec, ltype, rtype, larrtype, rarrtype, &
		lrank, rrank

	type(syntax_node_t) :: right
	type(syntax_token_t) :: op
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_expr'
	if (debug > 1) print *, 'pos = ', parser%pos

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

		rtype = right%val%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type

		if (.not. is_unary_op_allowed(op%kind, rtype, rarrtype)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_unary_types(parser%context(), span, op%text, &
				kind_name(expr%right%val%type)))

		end if

	else
		expr = parser%parse_primary_expr()
	end if

	do
		prec = get_binary_op_prec(parser%current_kind())
		if (prec == 0 .or. prec <= parent_precl) exit

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_binary_expr(expr, op, right)

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

		if (ltype == array_type .and. rtype == array_type) then
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

end function parse_expr

!===============================================================================

recursive module function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

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
			expr  = parser%parse_expr_statement()

			right = parser%match(rparen_token)

		case (lbracket_token)

			! Brackets are matched within parse_array_expr
			expr = parser%parse_array_expr()

			!print *, '2 expr%val%type = ', expr%val%type
			!print *, '2 expr%val%array%type = ', expr%val%array%type

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			expr = new_bool(bool)

			!print *, 'expr%val%sca%bool = ', expr%val%sca%bool

		case (identifier_token)

			!print *, "parser%peek_kind(1) = ", kind_name(parser%peek_kind(1))

			if (parser%peek_kind(1) == lparen_token) then
				expr = parser%parse_fn_call()
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
					expr = parser%parse_struct_instance()
					!print *, "back in parse_expr.f90"
				else
					! Same as default case below
					expr = parser%parse_name_expr()
				end if

			else
				expr = parser%parse_name_expr()
			end if

		case (f32_token)

			token = parser%match(f32_token)
			expr  = new_f32(token%val%sca%f32)

		case (f64_token)

			token = parser%match(f64_token)
			expr  = new_f64(token%val%sca%f64)

		case (str_token)

			token = parser%match(str_token)
			expr  = new_str(token%val%sca%str%s)

		case (i64_token)

			token = parser%match(i64_token)
			expr  = new_i64(token%val%sca%i64)

		case default

			token = parser%match(i32_token)
			expr  = new_i32(token%val%sca%i32)

			if (debug > 1) print *, 'token = ', expr%val%to_str()

	end select

end function parse_primary_expr

!===============================================================================

recursive module function parse_name_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, id_index

	type(syntax_token_t) :: identifier
	type(text_span_t) :: span

	type(value_t) :: var

	! Variable name expression

	identifier = parser%match(identifier_token)

	!print *, "RHS identifier = ", identifier%text
	!print *, "parser%is_loc = ", parser%is_loc
	!print *, '%current_kind() = ', kind_name(parser%current_kind())

	!print *, 'searching'

	if (parser%is_loc) then
		call parser%locs%search(identifier%text, id_index, io, var)
		!print *, "locs io = ", io
	end if

	if (parser%is_loc .and. io == 0) then
	
		expr = new_name_expr(identifier, var)
		expr%id_index = id_index
		expr%is_loc = .true.

	else
		call parser%vars%search(identifier%text, id_index, io, var)
		!print *, "vars io = ", io

		expr = new_name_expr(identifier, var)
		expr%id_index = id_index
		expr%is_loc = .false.

		if (io /= 0) then
			!print *, "undeclared var 3"
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context(), &
				span, identifier%text))
			!stop
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

end function parse_name_expr

!===============================================================================

recursive module subroutine parse_dot(parser, expr)

	class(parser_t) :: parser

	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: io, struct_id, member_id, pos0, pos1

	type(struct_t) :: struct

	type(syntax_token_t) :: dot, identifier

	type(text_span_t) :: span

	type(value_t) :: member

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

	! For RHS dots, this will stick.  For LHS dots, this will be shortly
	! overwritten as assignment_expr in the caller
	expr%kind = dot_expr

	! Save dot info in member syntax node
	allocate(expr%member)

	!print *, "struct_name = """, expr%val%struct_name, """"

	! Is there a better way than looking up every struct by name again?
	call parser%structs%search(expr%val%struct_name, struct_id, io, struct)
	if (io /= 0) then
		! Type is already confirmed as struct_type above, so I'm fairly sure
		! this is unreachable
		write(*,*) err_int_prefix//"unreachable struct lookup failure"//color_reset
		call internal_error()
	end if

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

		if (.not. all(expr%member%lsubscripts%sub_kind == scalar_sub)) then
			span = new_span(pos0, pos1 - pos0)
			call parser%diagnostics%push(err_struct_array_slice( &
				parser%context(), &
				span))
		end if

	end if

	! I think this needs a recursive call to `parse_dot()` right here to handle
	! things like `a.b.c`
	if (parser%peek_kind(0) == dot_token) then
		expr%member%val = expr%val
		expr%member%identifier = identifier  ! set for diags in recursed parse_dot()
		call parser%parse_dot(expr%member)
		expr%val = expr%member%val
	end if

end subroutine parse_dot

!===============================================================================

end submodule syntran__parse_expr

!===============================================================================

