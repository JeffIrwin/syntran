
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

	integer :: io, ltype, rtype, pos0, span0, span1, lrank, rrank, larrtype, &
		rarrtype, id_index

	logical :: is_dot

	type(syntax_node_t) :: right, member
	type(syntax_token_t) :: let, identifier, op

	type(text_span_t) :: span

	type(value_t) :: var

	!print *, 'starting parse_expr_statement()'

	is_dot = .false.

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
		op         = parser%next()

		right      = parser%parse_expr_statement()
		!right      = parser%parse_expr()

		!! I think the way to get conditional initialization like rust is
		!! something like this.  May need to peek current and check if it's
		!! if_keyword or not
		!right      = parser%parse_statement()
		!!semi       = parser%match(semicolon_token)

		expr = new_declaration_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		! Increment the variable array index and save it in the expr node.
		! TODO: make this a push_var fn?  parse_for_statement uses it too
		parser%num_vars = parser%num_vars + 1
		expr%id_index   = parser%num_vars

		!if (expr%val%type == array_type) then
		!	print *, 'array_type'
		!	print *, 'rank = ', expr%val%array%rank
		!end if

		! Insert the identifier's type into the dict and check that it
		! hasn't already been declared
		call parser%vars%insert(identifier%text, expr%val, &
			expr%id_index, io, overwrite = .false.)

		!print *, 'io = ', io
		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(parser%context(), &
				span, identifier%text))
		end if

		! TODO: this should be unnecessary.  Store the struct name in the value
		! instead
		!print *, "right type = ", kind_name(right%val%type)
		if (right%val%type == struct_type) then
			!print *, "struct_name = ", right%struct_name
			expr%struct_name = right%val%struct_name
		end if

		return

	end if

	if (parser%peek_kind(0) == identifier_token) then

		! There may or may not be a subscript expression after an identifier, so
		! we can't know how many spaces ahead an equals_token might be without
		! looking ahead

		! %pos is the lexer token index, %current_pos() is the character index!
		pos0 = parser%pos

		print *, "assign expr"

		identifier = parser%match(identifier_token)

		print *, "ident = ", identifier%text

		! Parse array subscript indices if present

		! Subscript can appear in assignment expr but not let expr, because let
		! must initialize the whole array.  Similarly for dot member access
		span0 = parser%current_pos()
		call parser%parse_subscripts(expr)

		if (size(expr%lsubscripts) <= 0) deallocate(expr%lsubscripts)
		span1 = parser%current_pos() - 1

		if (parser%peek_kind(0) == dot_token) then
			print *, "dot token"
			is_dot = .true.

			!call parser%vars%search(identifier%text, id_index, io, var)
			call parser%vars%search(identifier%text, id_index, io, var)

			!deallocate(expr%val)
			expr%val = var
			!expr%val%type = var%type

			call parser%parse_dot(expr)
			member = expr%right  ! swap because this will be re-used as RHS of whole expr

			allocate(expr%member)
			expr%member = member ! TODO: could get rid of local member var

			print *, "index = ", expr%right%id_index
			print *, "mndex = ", member%id_index

		end if

		if (.not. is_assignment_op(parser%current_kind())) then
			! Rewind and do the default case (same as outside the assignment if
			! block).  Could use goto or probably refactor somehow
			print *, "rewinding"
			parser%pos = pos0
			!print *, 'pos0 = ', pos0
			expr = parser%parse_expr()
			return
		end if
		print *, 'parsing assignment'

		op    = parser%next()
		right = parser%parse_expr_statement()

		! regular vs compound assignment exprs are denoted by the op.  all of
		! them are the same kind
		expr%kind = assignment_expr

		if (.not. allocated(expr%right)) allocate(expr%right)

		expr%identifier = identifier

		expr%op    = op
		expr%right = right

		print *, 'expr ident text = ', expr%identifier%text
		!print *, 'op = ', op%text

		! Get the identifier's type and index from the dict and check that it
		! has been declared
		call parser%vars%search(identifier%text, expr%id_index, io, expr%val)

		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context(), &
				span, identifier%text))
		end if

		!print *, 'type = ', kind_name(expr%val%type)

		!print *, 'allocated(expr%val%array) = ', allocated(expr%val%array)

		if (size(expr%lsubscripts) > 0) then

			if (expr%val%type == str_type) then
				!print *, 'str type'
				! TODO: check rank == 1
			else if (expr%val%type /= array_type) then
				span = new_span(span0, span1 - span0 + 1)
				call parser%diagnostics%push( &
					err_scalar_subscript(parser%context(), &
					span, identifier%text))
				return

			end if

			!print *, 'type = ', expr%val%type

			if (expr%val%type /= str_type) then

				if (all(expr%lsubscripts%sub_kind == scalar_sub)) then
					! this is not necessarily true for strings
					expr%val%type = expr%val%array%type
				end if

				!print *, 'rank = ', expr%val%array%rank
				!print *, 'subs = ', size(expr%lsubscripts)

				if (expr%val%array%rank /= size(expr%lsubscripts)) then
					span = new_span(span0, span1 - span0 + 1)
					call parser%diagnostics%push( &
						err_bad_sub_count(parser%context(), span, identifier%text, &
						expr%val%array%rank, size(expr%lsubscripts)))
				end if

				!print *, 'rank in  = ', expr%val%array%rank
				expr%val%array%rank = count(expr%lsubscripts%sub_kind /= scalar_sub)
				!print *, 'rank out = ', expr%val%array%rank

			end if

		end if

		ltype = expr%val%type
		rtype = expr%right%val%type

		! TODO: rename as *subtype instead of *arrtype
		larrtype = unknown_type
		rarrtype = unknown_type
		if (ltype == array_type) larrtype = expr%val%array%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type

		! !if (ltype == struct_type) larrtype = expr%val%struct(1)%type
		! !if (ltype == struct_type) larrtype = expr%val%struct( expr%right%id_index )%type
		! !if (ltype == struct_type) larrtype = expr%val%struct( member%id_index )%type

		!if (is_dot) larrtype = expr%val%struct( member%id_index )%type
		if (is_dot) ltype = expr%val%struct( member%id_index )%type

		! Descend similarly for rarrtype if dot expr
		if (expr%right%kind == dot_expr) then
			!rarrtype = expr%right%val%type
			!rarrtype = expr%right%val%struct(expr%right%id_index)%type
			rtype = expr%right%val%struct(expr%right%id_index)%type
		end if

		print *, "larrtype = ", kind_name(larrtype)
		print *, "rarrtype = ", kind_name(rarrtype)
		print *, "ltype    = ", kind_name(ltype)
		print *, "rtype    = ", kind_name(rtype)

		! This check could be moved inside of is_binary_op_allowed, but we would
		! need to pass parser to it to push diagnostics
		if (.not. is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)) then

			print *, 'bin not allowed in parse_expr_statement'

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context(), &
				span, op%text, &
				kind_name(ltype), &
				kind_name(rtype)))

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

		if (.not. is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)) then

			print *, 'bin not allowed in parse_expr'

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

module function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, dummy_id

	logical :: bool, exists

	!type(struct_t) :: dummy

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

				! TODO: is the exists() method needed?  Search will probably
				! work and simplify the code.  I was experimenting while
				! debugging memory issue, but exists might not be necessary.  On
				! the other hand, it might be more optimal to check existence
				! w/o copying an output val (which could containt big nested dict
				! types)
				!print *, "text = ", parser%current_text()
				!dummy = parser%structs%search(parser%current_text(), dummy_id, io)
				exists = parser%structs%exists(parser%current_text(), dummy_id, io)
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

module function parse_name_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, id_index, span0, span1, expect_rank

	type(syntax_token_t) :: identifier
	type(text_span_t) :: span

	type(value_t) :: var

	! Variable name expression

	identifier = parser%match(identifier_token)

	!print *, 'RHS identifier = ', identifier%text
	!print *, '%current_kind() = ', kind_name(parser%current_kind())

	!print *, 'searching'

	call parser%vars%search(identifier%text, id_index, io, var)
	expr = new_name_expr(identifier, var)
	expr%id_index = id_index

	if (io /= exit_success) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push( &
			err_undeclare_var(parser%context(), &
			span, identifier%text))
	end if

	!print *, 'type = ', kind_name(expr%val%type)
	!print *, 'allocated(expr%val%array) = ', &
	!	allocated(expr%val%array)

	!print *, '%current_kind() = ', kind_name(parser%current_kind())
	span0 = parser%current_pos()
	call parser%parse_subscripts(expr)

	span1 = parser%current_pos() - 1
	if (size(expr%lsubscripts) <= 0) then
		deallocate(expr%lsubscripts)
	else if (expr%val%type == array_type) then

		!print *, 'sub kind = ', kind_name(expr%lsubscripts(1)%sub_kind)

		if (all(expr%lsubscripts%sub_kind == scalar_sub)) then
			! this is not necessarily true for strings
			expr%val%type = expr%val%array%type
		end if

		! TODO: allow rank+1 for str arrays
		if (expr%val%array%rank /= size(expr%lsubscripts)) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				identifier%text, &
				expr%val%array%rank, size(expr%lsubscripts)))
		end if

		! A slice operation can change the result rank

		!print *, 'rank in  = ', expr%val%array%rank
		expr%val%array%rank = count(expr%lsubscripts%sub_kind /= scalar_sub)
		!print *, 'rank out = ', expr%val%array%rank

	else if (expr%val%type == str_type) then
		!print *, 'string type'

		expect_rank = 1
		if (size(expr%lsubscripts) /= expect_rank) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				identifier%text, &
				expect_rank, size(expr%lsubscripts)))
		end if
	else
		span = new_span(span0, span1 - span0 + 1)
		call parser%diagnostics%push( &
			err_scalar_subscript(parser%context(), &
			span, identifier%text))
	end if

	call parser%parse_dot(expr)

end function parse_name_expr

!===============================================================================

module subroutine parse_dot(parser, expr)

	class(parser_t) :: parser

	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: io, struct_id, member_id

	type(struct_t) :: struct

	type(syntax_token_t) :: dot, identifier

	type(value_t) :: member

	if (parser%current_kind() /= dot_token) then

		!! The function has to return something.  Caller deallocates
		!allocate( expr%lsubscripts(0))
		return

	end if

	print *, "parsing dot"

	dot  = parser%match(dot_token)

	! TODO: can this handle recursion?  `a.b.c`

	identifier = parser%match(identifier_token)

	!print *, "dot identifier = ", identifier%text
	!print *, "type = ", kind_name(expr%val%type)

	if (expr%val%type /= struct_type) then
		! TODO: diag.  Skip if unknown_type?  Probably already threw a diag in caller
		print *, err_prefix//"variable in dot expr is not a struct"//color_reset
		print *, "type = ", kind_name(expr%val%type)
		return
	end if

	!print *, "struct name = ", expr%val%struct_name

	!expr%kind = name_expr
	expr%kind = dot_expr

	! Save dot info in syntax node
	allocate(expr%right)

	!parser%vars%search(identifier%text, id_index, io)
	!dummy = parser%structs%search(parser%current_text(), dummy_id, io)

	! Is there a better way than looking up every struct by name again?

	!struct = parser%structs%search(expr%val%struct_name, struct_id, io)
	call parser%structs%search(expr%val%struct_name, struct_id, io, struct)

	if (io /= 0) then
		! TODO: diag
		print *, err_prefix//"unreachable struct lookup failure"//color_reset
		stop
	end if

	!member = struct%vars%search(name%text, member_id, io)
	call struct%vars%search(identifier%text, member_id, io, member)
	if (io /= 0) then
		! TODO: diag
		print *, err_prefix//"struct dot member does not exist"//color_reset
		stop
	end if
	print *, "member id = ", member_id
	print *, "mem type  = ", kind_name(member%type)

	expr%right%id_index = member_id
	print *, "index = ", expr%right%id_index

end subroutine parse_dot

!===============================================================================

end submodule syntran__parse_expr

!===============================================================================

