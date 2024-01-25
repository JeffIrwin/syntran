
!===============================================================================

module syntran__parser_m

	!use syntran__errors_m
	use syntran__lexer_m

	!! types and utils are used indirectly through lexer, so it doesn't matter
	!! much if they're explicitly included here
	!use syntran__types_m
	!use syntran__utils_m

	implicit none

	type parser_t

		! The parser takes a string of tokens (technically an array) and
		! constructs higher-level structures such as terms and expressions, like
		! constructing a phrase or sentence from words

		type(syntax_token_t), allocatable :: tokens(:)
		integer :: pos  ! token index position

		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		type(string_vector_t) :: diagnostics

		! Context for all src files (including include files).  Could convert to
		! standard array instead after size is known but I don't expect a
		! performance diff
		type(text_context_vector_t) :: contexts

		type(vars_t) :: vars
		integer :: num_vars = 0

		type(fns_t) :: fns
		integer :: num_fns = 0

		contains
			procedure :: &
				context => current_context, &
				current => current_token, &
				current_kind, &
				current_pos, &
				current_unit, &
				match, &
				match_pre, &
				next => next_token, &
				parse_array_expr, &
				parse_block_statement, &
				parse_expr, &
				parse_expr_statement, &
				parse_fn_call, &
				parse_fn_declaration, &
				parse_for_statement, &
				parse_if_statement, &
				parse_name_expr, &
				parse_primary_expr, &
				parse_size, &
				parse_statement, &
				parse_subscripts, &
				parse_type, &
				parse_unit, &
				parse_while_statement, &
				peek => peek_token, &
				peek_kind, &
				peek_pos, &
				peek_unit, &
				preprocess, &
				text => parser_text, &
				tokens_str

	end type parser_t

	!********

	interface
		! Implemented in parse_fn.f90

		module function parse_fn_call(parser) result(fn_call)
			class(parser_t) :: parser
			type(syntax_node_t) :: fn_call
		end function parse_fn_call

		module function parse_fn_declaration(parser) result(decl)
			class(parser_t) :: parser
			type(syntax_node_t) :: decl
		end function parse_fn_declaration

		module subroutine parse_type(parser, type_text, rank)
			class(parser_t) :: parser
			character(len = :), intent(out), allocatable :: type_text
			integer, intent(out) :: rank
		end subroutine parse_type

	end interface

	!********

	interface
		! Implemented in parse_array.f90

		module function parse_array_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function

		module function parse_size(parser) result(size)
			class(parser_t) :: parser
			type(syntax_node_vector_t) :: size
		end function parse_size

		module subroutine parse_subscripts(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(inout) :: expr
		end subroutine parse_subscripts

	end interface


!===============================================================================

contains

!===============================================================================

function tokens_str(parser) result(str)

	class(parser_t) :: parser

	character(len = :), allocatable :: str

	!********

	integer :: i

	! This will crash for very long token lists, but it should suffice for basic
	! debugging

	str = 'tokens = '//line_feed//'['//line_feed
	do i = 1, size(parser%tokens)
		str = str//tab &
				//'<'//           parser%tokens(i)%text  //'> ' &
				//'<'//kind_name( parser%tokens(i)%kind )//'>'  &
				//line_feed
	end do
	str = str//']'//line_feed

end function tokens_str

!===============================================================================

! You can't chain together member fn calls and their children like
! parser%current()%kind in Fortran, so use this helper fn instead

integer function current_kind(parser)
	class(parser_t) :: parser
	current_kind = parser%peek_kind(0)
end function current_kind

integer function peek_kind(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_kind = peek%kind
end function peek_kind

!===============================================================================

function match(parser, kind) result(token)

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t) :: token

	!********

	integer :: len_text

	type(syntax_token_t) :: current
	type(text_span_t) :: span

	! If current_text() and current_pos() helper fns are added, this local var
	! current can be eliminated
	current = parser%current()

	if (parser%current_kind() == kind) then
		token = parser%next()
		!print *, 'returning parser expecting false'
		return
	end if

	!! A continued expression can commonly have several unmatched tokens.  The
	!! last one is usually a semicolon, or it could be a right brace.  The first
	!! one is more helpful for the user to know
	!print *, 'unmatched '//kind_name(kind)
	!print *, 'unmatched '//kind_token(kind)

	if (.not. parser%first_expecting) then
		parser%first_expected  = kind_token(kind)
		parser%first_expecting = .true.
	end if

	!print *, 'pushing match diag'
	len_text = max(len(current%text), 1)

	span = new_span(parser%current_pos(), len_text)
	!span = new_span(current%pos, len_text)

	!call parser%diagnostics%push( &
	!	err_unexpected_token(parser%context(), span, current%text, &
	!	kind_name(parser%current_kind()), kind_name(kind)))

	!print *, 'current%unit_ = ', current%unit_
	!print *, 'current%text  = ', quote(current%text)

	call parser%diagnostics%push( &
		err_unexpected_token(parser%context(), span, current%text, &
		!err_unexpected_token(parser%contexts%v(current%unit_), span, current%text, &
		kind_name(parser%current_kind()), kind_name(kind)))

	! An unmatched char in the middle of the input is an error and should log
	! a diagnostic.  An unmatched char at the end means the interactive
	! interpreter should expect more lines
	if (parser%pos >= size(parser%tokens)) then
		parser%expecting = .true.
	end if

	token = new_token(kind, current%pos, null_char)
	!token = new_token(bad_token, current%pos, null_char)
	!token = new_token(kind, current%pos, "")

	token%unit_ = current%unit_
	!print *, 'setting token%unit_ = ', token%unit_

end function match

!===============================================================================

function current_token(parser)
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token
	current_token = parser%peek(0)
end function current_token

function peek_token(parser, offset) result(token)

	class(parser_t) :: parser

	type(syntax_token_t) :: token

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = parser%pos + offset

	if (debug > 2) print *, 'token pos ', pos

	if (pos > size(parser%tokens)) then
		token = parser%tokens( size(parser%tokens) )
		return
	end if

	token = parser%tokens(pos)

end function peek_token

!===============================================================================

function next_token(parser) result(next)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next_token

!===============================================================================

function parse_statement(parser) result(statement)

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

		case default
			statement = parser%parse_expr_statement()
			semi      = parser%match(semicolon_token)

	end select

end function parse_statement

!===============================================================================

function parse_for_statement(parser) result(statement)

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

function parse_while_statement(parser) result(statement)

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

function parse_if_statement(parser) result(statement)

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

function parse_block_statement(parser) result(block)

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

recursive function parse_expr_statement(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, ltype, rtype, pos0, span0, span1, lrank, rrank, larrtype, &
		rarrtype

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

		return

	end if

	if (parser%peek_kind(0) == identifier_token) then

		! There may or may not be a subscript expression after an identifier, so
		! we can't know how many spaces ahead an equals_token might be without
		! looking ahead

		! %pos is the lexer token index, %current_pos() is the character index!
		pos0 = parser%pos

		!print *, 'assign expr'

		identifier = parser%match(identifier_token)

		! Parse array subscript indices if present

		! Subscript can appear in assignment expr but not let expr, because let
		! must initialize the whole array
		span0 = parser%current_pos()
		call parser%parse_subscripts(expr)

		if (size(expr%lsubscripts) <= 0) deallocate(expr%lsubscripts)
		span1 = parser%current_pos() - 1

		if (.not. is_assignment_op(parser%current_kind())) then
			! Rewind and do the default case (same as outside the assignment if
			! block).  Could use goto or probably refactor somehow
			parser%pos = pos0
			!print *, 'pos0 = ', pos0
			expr = parser%parse_expr()
			return
		end if
		!print *, 'parsing assignment'

		op    = parser%next()
		right = parser%parse_expr_statement()

		! regular vs compound assignment exprs are denoted by the op.  all of
		! them are the same kind
		expr%kind = assignment_expr

		allocate(expr%right)

		expr%identifier = identifier

		expr%op    = op
		expr%right = right

		!print *, 'expr ident text = ', expr%identifier%text
		!print *, 'op = ', op%text

		! Get the identifier's type and index from the dict and check that it
		! has been declared
		expr%val = parser%vars%search(identifier%text, expr%id_index, io)

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

			!else if (any(expr%lsubscripts%sub_kind /= scalar_sub) .and. &
			!	expr%val%array%rank > 1) then
			else if (any(expr%lsubscripts%sub_kind /= scalar_sub)) then

				! TODO: allow LHS slices

				span = new_span(span0, span1 - span0 + 1)
				call parser%diagnostics%push( &
					err_bad_sub_rank(parser%context(), span, &
					identifier%text, expr%val%array%rank))

			end if

			!print *, 'type = ', expr%val%type

			if (expr%val%type /= str_type) then

				expr%val%type = expr%val%array%type

				!print *, 'rank = ', expr%val%array%rank
				!print *, 'subs = ', size(expr%lsubscripts)

				if (expr%val%array%rank /= size(expr%lsubscripts)) then
					span = new_span(span0, span1 - span0 + 1)
					call parser%diagnostics%push( &
						err_bad_sub_count(parser%context(), span, identifier%text, &
						expr%val%array%rank, size(expr%lsubscripts)))
				end if
			end if

		end if

		ltype = expr%val%type
		rtype = expr%right%val%type

		!if (rtype == array_type) rtype = expr%right%val%array%type
		!if (rtype == array_type) rtype = right%val%array%type

		larrtype = unknown_type
		rarrtype = unknown_type
		if (ltype == array_type) larrtype = expr%val%array%type
		if (rtype == array_type) rarrtype = expr%right%val%array%type
		!print *, 'larrtype = ', kind_name(larrtype)
		!print *, 'rarrtype = ', kind_name(rarrtype)

		! This check could be moved inside of is_binary_op_allowed, but we would
		! need to pass parser to it to push diagnostics
		if (.not. is_binary_op_allowed(ltype, op%kind, rtype, larrtype, rarrtype)) then

			!print *, 'bin not allowed in parse_expr_statement'

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

recursive function parse_expr(parser, parent_prec) result(expr)

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

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	logical :: bool

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

			if (parser%peek_kind(1) /= lparen_token) then
				expr = parser%parse_name_expr()
			else
				expr = parser%parse_fn_call()
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

function parse_name_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, id_index, span0, span1, exp_rank

	type(syntax_token_t) :: identifier
	type(text_span_t) :: span

	! Variable name expression

	identifier = parser%match(identifier_token)

	!print *, 'RHS identifier = ', identifier%text
	!print *, '%current_kind() = ', kind_name(parser%current_kind())

	!print *, 'searching'
	expr = new_name_expr(identifier, &
		parser%vars%search(identifier%text, id_index, io))
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

		exp_rank = 1
		if (size(expr%lsubscripts) /= exp_rank) then
			span = new_span(span0, span1 - span0 + 1)
			call parser%diagnostics%push( &
				err_bad_sub_count(parser%context(), span, &
				identifier%text, &
				exp_rank, size(expr%lsubscripts)))
		end if
	else
		span = new_span(span0, span1 - span0 + 1)
		call parser%diagnostics%push( &
			err_scalar_subscript(parser%context(), &
			span, identifier%text))
	end if

end function parse_name_expr

!===============================================================================

integer function current_pos(parser)

	! Get the current character index.  If you want the token index, use
	! parser%pos instead

	class(parser_t) :: parser

	current_pos = parser%peek_pos(0)

end function current_pos

!********

integer function peek_pos(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_pos = peek%pos
end function peek_pos

!********

integer function current_unit(parser)
	class(parser_t) :: parser
	current_unit = parser%peek_unit(0)
end function current_unit

integer function peek_unit(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_unit = peek%unit_
end function peek_unit

!********

function current_context(parser) result(context)
	class(parser_t) :: parser
	type(text_context_t) :: context

	!parser%contexts%v(parser%current_unit()), span, type_text))
	context = parser%contexts%v( parser%current_unit() )

end function current_context

!********

function parser_text(parser, beg_, end_) result(text)
	class(parser_t) :: parser
	integer, intent(in) :: beg_, end_
	type(text_context_t) :: context
	character(len = :), allocatable :: text

	!parser%context(), span, parser%context%text(cond_beg: cond_end), &
	context = parser%context()
	text = context%text(beg_: end_)

end function parser_text

!===============================================================================

subroutine preprocess(parser, tokens_in, src_file, contexts, unit_)

	! src_file is the filename of the current file being processed, i.e. the
	! *includer*, not the includee

	class(parser_t) :: parser
	type(syntax_token_t), intent(in) :: tokens_in(:)
	character(len = *), intent(in) :: src_file

	type(syntax_token_vector_t) :: tokens_out
	type(text_context_vector_t), intent(inout) :: contexts

	integer, intent(inout) :: unit_

	!********

	character(len = :), allocatable :: inc_text, filename

	integer :: i, j, iostat, unit_0

	type(parser_t) :: inc_parser

	type(text_span_t) :: span

	type(syntax_token_t) :: token, token_peek, lparen, rparen, semicolon

	unit_0 = unit_

	tokens_out = new_syntax_token_vector()
	i = 0
	do while (i < size(tokens_in))

		! TODO: make a variation of parser%next() instead of manually increment i/pos?
		i = i + 1
		token = tokens_in(i)

		! Whitespace has already been skipped in previous loop
		if (token%kind /= hash_token) then
			call tokens_out%push(token)
			cycle
		end if

		i = i + 1
		token_peek = tokens_in(i)

		select case (token_peek%kind)
		case (include_keyword)

			! This block could possibly be refactored as a general
			! "parse_directive_fn_call" for re-use as we add more directives,
			! but it may be difficult since parser is not fully constructed yet.
			! See comments on match_pre() vs match().

			! Parens are kind of a pain to match() since the parser isn't
			! constructed yet.  I can see why C works the way it does
			!
			! Note that matched tokens are not pushed to tokens_out here.  They
			! are consumed by the preprocessor, so the later actual parser does
			! not see them.
			lparen = parser%match_pre(lparen_token, tokens_in, i, contexts%v(unit_0))

			! Prepend with path to src_file
			!
			! TODO: maybe later add `-I` arg for include dirs, or an env var, or
			! a global installed syntran "std" lib dir?  See also the
			! fullpath/realpath fn interfaces in utils.f90

			! TODO: if filename is already absolute, do not prepend with path

			!print *, 'get_dir(src_file) = ', get_dir(src_file)

			i = i + 1
			filename = get_dir(src_file)//tokens_in(i)%val%sca%str%s  ! relative to src file
			!filename = tokens_in(i)%val%sca%str%s                    ! relative to runtime pwd

			!print *, 'include filename = ', quote(filename)

			if (.not. exists(filename)) then
				span = new_span(tokens_in(i)%pos, len(tokens_in(i)%text))
				call parser%diagnostics%push( &
					err_inc_404(contexts%v(unit_0), span, tokens_in(i)%text))

				! Could probably be refactored
				rparen    = parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0))
				semicolon = parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0))
				cycle
			end if

			inc_text = read_file(filename, iostat)
			if (iostat /= exit_success) then
				! For example, `#include(".");` exists but cannot be read.
				! AFAIK there is no portable way to differentiate files from
				! dirs in fortran
				span = new_span(tokens_in(i)%pos, len(tokens_in(i)%text))
				call parser%diagnostics%push( &
					err_inc_read(contexts%v(unit_0), span, tokens_in(i)%text))
				rparen    = parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0))
				semicolon = parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0))
				cycle
			end if

			!print *, 'len(inc_text) = ', len(inc_text)
			!print *, 'inc_text = '
			!print *, inc_text

			! Any nested includes are handled in this new_parser() call
			inc_parser = new_parser(inc_text, filename, contexts, unit_)

			! Add includee tokens to includer.  Minus 1 because included eof_token
			do j = 1, size(inc_parser%tokens) - 1
				call tokens_out%push( inc_parser%tokens(j) )
			end do

			! Push included diagnostics (from lexing) into parent parser
			!
			! TODO: append errors with extra context, like "in file included
			! here (show includer line number and context)
			call parser%diagnostics%push_all( inc_parser%diagnostics )

			rparen    = parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0))
			semicolon = parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0))

		!case (tree_keyword)
		!! TODO: maybe do #tree work at eval time

		! TODO: #pragma once or at least #ifndef/#def-style include guards

		case default

			! This will defer any diagnostic logging to the parser.  Should
			! there be a special-case diagnostic here for bad directives?
			call tokens_out%push(token)
			call tokens_out%push(token_peek)

		end select  ! case (token_peek%kind)

	end do  ! while (i < size(tokens_in))

	! Convert to standard member array
	parser%tokens = tokens_out%v( 1: tokens_out%len_ )

end subroutine preprocess

!===============================================================================

function match_pre(parser, kind, tokens, token_index, context) result(token)

	! This is like match(), but it can run during preprocessing before the
	! parser is fully constructed, at the cost of having a bunch of arguments.
	!
	! Things could probably be refactored by adding the temp syntax vector into
	! a new parser member and deleting it after preprocessing is done.  Then
	! this fn could work with parser members instead of taking so many args

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t) :: token
	type(syntax_token_t), intent(in) :: tokens(:)

	integer, intent(inout) :: token_index

	type(text_context_t) :: context

	!********

	integer :: len_text

	type(syntax_token_t) :: current
	type(text_span_t) :: span

	token_index = token_index + 1
	!current = parser%current()
	current = tokens(token_index)

	!if (parser%current_kind() == kind) then
	if (current%kind == kind) then
		!token = parser%next()
		token = current
		!print *, 'returning parser pre expecting false'
		!print *, ''
		return
	end if
	token_index = token_index - 1

	!print *, 'ERROR: unmatched token'
	!print *, ''

	!! A continued expression can commonly have several unmatched tokens.  The
	!! last one is usually a semicolon, or it could be a right brace.  The first
	!! one is more helpful for the user to know
	!print *, 'unmatched '//kind_name(kind)
	!print *, 'unmatched '//kind_token(kind)

	if (.not. parser%first_expecting) then
		parser%first_expected  = kind_token(kind)
		parser%first_expecting = .true.
	end if

	!print *, 'pushing match diag'
	len_text = max(len(current%text), 1)

	!span = new_span(parser%current_pos(), len_text)
	span = new_span(current%pos, len_text)

	!print *, 'current%unit_ = ', current%unit_
	!print *, 'current%text  = ', quote(current%text)

	!print *, 'pushing diag'
	call parser%diagnostics%push( &
		!err_unexpected_token(parser%context(), span, current%text, &
		!err_unexpected_token(parser%contexts%v(1), span, current%text, &
		err_unexpected_token(context, span, current%text, &
		kind_name(current%kind), kind_name(kind)))
	!print *, 'done'

	! An unmatched char in the middle of the input is an error and should log
	! a diagnostic.  An unmatched char at the end means the interactive
	! interpreter should expect more lines
	!if (parser%pos >= size(parser%tokens)) then
	if (token_index >= size(tokens)) then
		parser%expecting = .true.
	end if

	token = new_token(kind, current%pos, null_char)
	token%unit_ = current%unit_
	!print *, 'setting token%unit_ = ', token%unit_

end function match_pre

!===============================================================================

function parse_unit(parser) result(unit)

	class(parser_t) :: parser

	type(syntax_node_t) :: unit

	!********

	type(syntax_node_vector_t) :: members
	type(syntax_token_t) :: dummy

	integer :: i, pos0

	!print *, 'starting parse_unit()'

	members = new_syntax_node_vector()
	i = 0

	!left  = parser%match(lbrace_token)

	!! Pushing scope breaks interactive interpretation, but we may want it later
	!! for interpetting multiple files.  Another alternative would be chaining
	!! interpreted statements like Immo does

	!call parser%vars%push_scope()

	do while (parser%current_kind() /= eof_token)

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		if (parser%current_kind() == fn_keyword) then
			call members%push(parser%parse_fn_declaration())
		else
			call members%push(parser%parse_statement())
		end if

		! Break infinite loops
		if (parser%pos == pos0) dummy = parser%next()

	end do

	!call parser%vars%pop_scope()

	!right = parser%match(rbrace_token)

	unit%kind = translation_unit

	! Convert to standard array
	call syntax_nodes_copy(unit%members, members%v( 1: members%len_ ))

	! Eof is matched in the caller syntax_parse() to deal with broken stdin
	! lines with interactive interpretation

end function parse_unit

!===============================================================================

recursive function new_parser(str, src_file, contexts, unit_) result(parser)

	character(len = *), intent(in) :: str, src_file

	type(parser_t) :: parser

	integer, intent(inout) :: unit_

	!********

	type(text_context_vector_t) :: contexts

	type(lexer_t) :: lexer

	type(syntax_token_t) :: token
	type(syntax_token_vector_t) :: tokens

	! Lex and get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str, src_file, unit_)
	do
		token = lexer%lex()
		!print *, 'token%unit_ = ', token%unit_

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then
			call tokens%push(token)
		end if

		if (token%kind == eof_token) exit
	end do

	! Preprocess then convert to standard array (and parser class member)

	! For correct ordering wrt token%unit_, the current parser context is pushed
	! first, before preprocessing.
	call contexts%push( lexer%context )

	parser%diagnostics = new_string_vector()
	call parser%diagnostics%push_all( lexer%diagnostics )

	call parser%preprocess(tokens%v(1:tokens%len_), src_file, contexts, unit_)

	! Set other parser members

	parser%pos = 1

	parser%contexts = contexts  ! copy.  could convert to standard array if needed

	!print *, 'tokens%len_ = ', tokens%len_
	if (debug > 1) print *, parser%tokens_str()

end function new_parser

!===============================================================================

end module syntran__parser_m

!===============================================================================

