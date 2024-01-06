
!===============================================================================

module syntran__parser_m

	use syntran__errors_m
	use syntran__lexer_m
	use syntran__types_m
	use syntran__utils_m

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
			procedure :: match, tokens_str, current_kind, &
				current => current_token, next => next_parser_token, &
				peek => parser_peek_token, peek_kind, &
				parse_expr, parse_primary_expr, parse_expr_statement, &
				parse_statement, parse_block_statement, parse_if_statement, &
				current_pos, peek_pos, parse_for_statement, &
				parse_while_statement, parse_array_expr, parse_unit, &
				parse_fn_declaration, parse_subscripts, parse_type, &
				parse_size, peek_unit, current_unit, &
				context => parser_current_context, &
				text => parser_text, preprocess, match_pre

	end type parser_t

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

function parser_peek_token(parser, offset) result(token)

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

end function parser_peek_token

!===============================================================================

function next_parser_token(parser) result(next)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next_parser_token

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

	!integer :: bound_beg, bound_end

	type(syntax_node_t)  :: array, body
	type(syntax_token_t) :: for_token, in_token, identifier

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

	array      = parser%parse_array_expr()

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

	if (allocated(array%lbound)) then
		! Pathological code like `for <EOF>` can crash the parser :(
		call parser%vars%insert(identifier%text, array%lbound%val, &
			statement%id_index)
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

	integer :: io, ltype, rtype, pos0, span0, span1, lrank, rrank

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

		! This check could be moved inside of is_binary_op_allowed, but we would
		! need to pass parser to it to push diagnostics
		if (.not. is_binary_op_allowed(ltype, op%kind, rtype)) then

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

		!! segfault
		!if (rtype == array_type) rtype = right%val%array%type

		if (.not. is_unary_op_allowed(op%kind, rtype)) then

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

		!if (ltype == array_type) ltype = expr%left %val%array%type
		!if (rtype == array_type) rtype = expr%right%val%array%type

		larrtype = 0
		rarrtype = 0
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

subroutine parse_subscripts(parser, expr)

	! Parse array subscripts, if present

	class(parser_t) :: parser
	type(syntax_node_t), intent(inout) :: expr

	!********

	integer :: pos0, span0

	type(syntax_node_t) :: lsubscript, usubscript
	type(syntax_node_vector_t) :: lsubscripts_vec, usubscripts_vec
	type(syntax_token_t) :: lbracket, rbracket, comma, &
		dummy, colon

	type(text_span_t) :: span

	if (parser%current_kind() /= lbracket_token) then

		!! The function has to return something.  Caller deallocates
		allocate( expr%lsubscripts(0))
		return

	end if

	!print *, 'parsing subscripts'

	lsubscripts_vec = new_syntax_node_vector()  ! lower-bounds
	usubscripts_vec = new_syntax_node_vector()  ! upper-bounds

	lbracket  = parser%match(lbracket_token)

	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0  = parser%pos
		span0 = parser%current_pos()

		if (parser%current_kind() == colon_token) then
			lsubscript%sub_kind = all_sub
		else

			lsubscript = parser%parse_expr()

			!print *, 'lsubscript = ', lsubscript%str()
			!print *, 'lsubscript = ', parser%text(span0, parser%current_pos()-1)

			if (.not. any(lsubscript%val%type == [i32_type, i64_type])) then
				span = new_span(span0, parser%current_pos() - span0)
				call parser%diagnostics%push( &
					err_non_int_subscript(parser%context(), span, &
					parser%text(span0, parser%current_pos()-1)))
			end if

			! TODO: set step_sub case for non-unit range step
			if (parser%current_kind() == colon_token) then
				colon = parser%match(colon_token)
				lsubscript%sub_kind = range_sub

				usubscript = parser%parse_expr()
				! TODO: type check i32

			else
				lsubscript%sub_kind = scalar_sub

			end if
			!print *, kind_name(subscript%sub_kind)

		end if

		! Parallel arrays subscripts and usubscripts should be same size? Not
		! sure if this is ideal for multi-rank ranges
		call lsubscripts_vec%push(lsubscript)
		call usubscripts_vec%push(usubscript)

		! Break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

	!print *, 'parsing rbracket'
	rbracket  = parser%match(rbracket_token)
	!print *, 'done'

	! TODO: check that num of subscripts matches array rank, both LHS and
	! RHS parsing.  May need to pass identifier to this function.  LHS and RHS
	! cases are different in tricky ways.  RHS has already lookup up identifier
	! in vars dictionary when it calls parse_subscripts(), but LHS has not.
	! When LHS calls this, it does not yet know whether the identifier is an
	! array or a scalar or a function call in an expression statement.
	!
	! Check that the expr is actually an array (not a scalar), or do that next
	! to err_bad_sub_count() elsewhere
	!
	! So, only check rank match here if lsubscripts%len_ > 0

	call syntax_nodes_copy(expr%lsubscripts, &
		lsubscripts_vec%v( 1: lsubscripts_vec%len_ ))

	call syntax_nodes_copy(expr%usubscripts, &
		usubscripts_vec%v( 1: usubscripts_vec%len_ ))

end subroutine parse_subscripts

!===============================================================================

function parse_size(parser) result(size)

	class(parser_t) :: parser

	type(syntax_node_vector_t) :: size

	!********

	integer :: span_beg, span_end, pos0

	type(syntax_node_t)  :: len
	type(syntax_token_t) :: comma, dummy
	type(text_span_t) :: span

	size = new_syntax_node_vector()
	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		span_beg = parser%peek_pos(0)
		len      = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'len = ', parser%text(span_beg, span_end)

		if (.not. any(len%val%type == [i32_type, i64_type])) then
			span = new_span(span_beg, span_end - span_beg + 1)
			! TODO: different diag for each (or at least some) case
			call parser%diagnostics%push(err_non_int_range( &
				parser%context(), span, &
				parser%text(span_beg, span_end)))
		end if

		call size%push(len)

		! break infinite loop?
		if (parser%pos == pos0) dummy = parser%next()

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

end function parse_size

!===============================================================================

function parse_array_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: span_beg, span_end, pos0

	type(syntax_node_t)  :: lbound, step, ubound, len, elem
	type(syntax_node_vector_t) :: elems, size
	type(syntax_token_t) :: lbracket, rbracket, colon, semicolon, comma, dummy
	type(text_span_t) :: span

	!print *, 'starting parse_array_expr()'

	! This function parses arrays of the following forms:
	!
	!     // i32
	!     let a = [imin:        imax];      // current loop syntax
	!     let a = [imin: istep: imax];
	!     let a = [iconst           ; len]; // this one is like Rust
	!
	!     // f32
	!     let a = [fmin: fstep: fmax];      // consistent with i32
	!     let a = [fmin:        fmax; len]; // no default unit step like i32
	!     let a = [fconst           ; len];
	!
	!     // Rank-2, rank-3, etc.  No range variations, only all elements
	!     // the same value
	!     let a = [fconst           ; rows, cols];  // row-major like Fortran
	!     let a = [fconst           ; rows, cols, sheets];
	!
	!     // Explicit list for any rank-1 type
	!     [elem_0, elem_1, elem_2, ... ]
	!
	! A note on the term "rank-1":  Maybe there's an argument to be made that
	! for a language with 0-based arrays, we should call vectors "rank-0" and
	! matrices "rank-1".  However, I'm calling them "rank-1" and "rank-2"
	! respectively, as that's what Fortran calls them and I hadn't thought that
	! far ahead :).  Anyway, a "3D" vector is always like [x, y, z] -- C doesn't
	! call that a 4D vector despite being 0-based.
	!
	! NumPy uses the same convention for "rank-1" as us.  In fact, NumPy has
	! something below a vector called a "rank-0" array :exploding-head:

	lbracket = parser%match(lbracket_token)

	span_beg = parser%peek_pos(0)
	lbound   = parser%parse_expr()
	span_end = parser%peek_pos(0) - 1

	!print *, 'lbound = ', parser%text(span_beg, span_end)

	! TODO: should type checking be done by caller, or should we pass an
	! expected type arg for the RHS of this check?

	! TODO: check if lbound%val is allocated, e.g. for assigning one array to
	! a cat of another?  How would this work for rank-2+?
	!
	!     let a = [0: 3];
	!     let b = [a, 5, 6];
	!              ^ segfault

	!! TODO: there should still be *some* type checking.  At least, implicit
	!! ranges cannot use bool
	!if (lbound%val%type /= i32_type) then
	!	span = new_span(span_beg, span_end - span_beg + 1)
	!	call parser%diagnostics%push(err_non_int_range( &
	!		parser%context, span, parser%text(span_beg, span_end)))
	!end if

	if (parser%current_kind() == semicolon_token) then

		! Implicit constant-value array form [lbound; len]

		semicolon    = parser%match(semicolon_token)

		! rank-2+ arrays:
		!
		! [lbound; rows, cols]
		! [lbound; rows, cols, sheets, ...]

		size = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)
		allocate(expr%lbound)
		allocate(expr%len_)

		call syntax_nodes_copy(expr%size, size%v( 1: size%len_ ))

		expr%kind           = array_expr

		expr%val%type       = array_type

		expr%val%array%type = lbound%val%type
		expr%val%array%kind = impl_array
		expr%val%array%rank = size%len_

		!print *, 'expr%val%type       = ', expr%val%type
		!print *, 'expr%val%array%type = ', expr%val%array%type

		! Does this syntax node need to own these members, or can we just save
		! them in the array_t?  I think they do need to be duplicated, as they
		! may be an expression and not just a literal.  So, sizes have to be
		! allocated dynamically during evaluation, not during parsing

		expr%lbound = lbound
		expr%len_    = len

		return

	end if

	if (parser%current_kind() == colon_token) then

		! Implicit array form unit step [lbound: ubound] or [lbound: step: ubound]
		colon    = parser%match(colon_token)

		span_beg = parser%peek_pos(0)
		ubound   = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'lbound type = ', kind_name(lbound%val%type)
		!print *, 'ubound type = ', kind_name(ubound%val%type)

		!if (ubound%val%type /= lbound%val%type) then
		if (.not. all([ &
			is_num_type(lbound%val%type), &
			is_num_type(ubound%val%type)])) then

			!print *, 'HERE'
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_non_int_range( &
				parser%context(), span, parser%text(span_beg, span_end)))

		end if

		if (parser%current_kind() == colon_token) then

			! Implicit form [lbound: step: ubound]

			! Step has just been parsed as ubound above
			step = ubound

			colon    = parser%match(colon_token)

			span_beg = parser%peek_pos(0)
			ubound   = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			!if (ubound%val%type /= lbound%val%type) then
			if (.not. is_num_type(ubound%val%type)) then
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_non_int_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			! If [lbound: step: ubound] are all specified, then specifying the
			! len would be overconstrained!  Next token must be rbracket

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%step)
			allocate(expr%ubound)

			expr%kind           = array_expr
			expr%val%type       = array_type

			if (all(i32_type == &
				[lbound%val%type, step%val%type, ubound%val%type]) .or. &
				all(f32_type == &
				[lbound%val%type, step%val%type, ubound%val%type])) then

				expr%val%array%type = lbound%val%type

			! TODO: make is_int_type() elemental, then we can sugar up this syntax
			else if (all([ &
				is_int_type(lbound%val%type), &
				is_int_type(step  %val%type), &
				is_int_type(ubound%val%type)])) then

				expr%val%array%type = i64_type

			else
				! TODO: different message
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_non_int_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			expr%val%array%kind = impl_array
			expr%val%array%rank = 1

			expr%lbound = lbound
			expr%step   = step
			expr%ubound = ubound

			return

		end if

		if (parser%current_kind() == semicolon_token) then

			! Implicit form [lbound: ubound; len]

			semicolon    = parser%match(semicolon_token)

			span_beg = parser%peek_pos(0)
			len      = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			!print *, 'len = ', parser%text(span_beg, span_end)

			if (.not. any(len%val%type == [i32_type, i64_type])) then
				! Length is not an integer type
				span = new_span(span_beg, span_end - span_beg + 1)
				! TODO: different diag for each (or at least some) case
				call parser%diagnostics%push(err_non_int_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			! This used to be checked further up before i64 arrays
			if (ubound%val%type /= lbound%val%type) then
				! lbound type and ubound type do not match for length-based array
				span = new_span(span_beg, span_end - span_beg + 1)
				! TODO: different diag for each (or at least some) case.  Need
				! to save spans of different parts of the array text, because we
				! can't know if the lbound type needs to match the ubound type
				! until after we check for the semicolon_token in this block
				call parser%diagnostics%push(err_non_int_range( &
					parser%context(), span, &
					parser%text(span_beg, span_end)))
			end if

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%ubound)
			allocate(expr%len_)

			expr%kind           = array_expr

			!expr%val%type       = lbound%val%type
			expr%val%type       = array_type

			expr%val%array%type = lbound%val%type
			expr%val%array%kind = impl_array
			expr%val%array%rank = 1

			expr%lbound = lbound
			expr%ubound = ubound
			expr%len_    = len

			return

		end if

		! Implicit array form unit step [lbound: ubound]

		rbracket = parser%match(rbracket_token)

		!print *, 'lbound = ', lbound%str()
		!print *, 'ubound = ', ubound%str()

		allocate(expr%val%array)
		allocate(expr%lbound)
		allocate(expr%ubound)

		expr%kind = array_expr

		expr%val%type = array_type

		expr%val%array%kind = impl_array
		expr%val%array%rank = 1

		expr%lbound = lbound
		expr%ubound = ubound

		if (all(i32_type == &
			[lbound%val%type, ubound%val%type]) &! .or. &
			!all(f32_type == &  ! is f32 even allowed here?
			![lbound%val%type, ubound%val%type])) then
			) then

			expr%val%array%type = lbound%val%type

		! TODO: make is_int_type() elemental, then we can sugar up this syntax
		else if (all([ &
			is_int_type(lbound%val%type), &
			is_int_type(ubound%val%type)])) then

			expr%val%array%type = i64_type

		else
			! TODO: different message
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_non_int_range( &
				parser%context(), span, &
				parser%text(span_beg, span_end)))
		end if

		return

	end if

	! Explicit array form [elem_0, elem_1, elem_2, ... ].  elem_0 has already been
	! parsed as lbound above

	!print *, 'elem ', lbound%val%str()

	elems = new_syntax_node_vector()
	call elems%push(lbound)
	do while (&
		parser%current_kind() /= rbracket_token  .and. &
		parser%current_kind() /= semicolon_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		comma    = parser%match(comma_token)

		span_beg = parser%peek_pos(0)
		elem     = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'elem ', elem%val%str()

		if (elem%val%type /= lbound%val%type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_het_array( &
				parser%context(), span, parser%text(span_beg, span_end)))
		end if

		call elems%push(elem)

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	if (parser%current_kind() == semicolon_token) then

		! Rank-2+ array: [elem_0, elem_1, elem_2, ... ; size_0, size_1, ... ];
		semicolon = parser%match(semicolon_token)

		size = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)

		call syntax_nodes_copy(expr%size, size%v( 1: size%len_ ))

		expr%kind           = array_expr

		expr%val%type       = array_type

		expr%val%array%type = lbound%val%type
		expr%val%array%kind = expl_array
		expr%val%array%rank = size%len_

		call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len_ ))

		return

	end if

	! Rank-1 array (size is implicitly defined by number of elements)

	rbracket = parser%match(rbracket_token)

	allocate(expr%val%array)
	expr%kind           = array_expr

	!expr%val%type       = lbound%val%type
	expr%val%type       = array_type

	expr%val%array%type = lbound%val%type
	expr%val%array%kind = expl_array
	expr%val%array%rank = 1

	call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len_ ))

	! TODO: allow arbitrarily combinations catting expl and impl arrays, e.g.
	!
	!        [0: 3   ,   5, 6,   10: 13    ]
	!     // [0, 1, 2,   5, 6,   10, 11, 12]
	!
	! But how useful would that really be?

end function parse_array_expr

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	character(len = :), allocatable :: param_type, arg_type
	integer :: i, io, id_index, param_rank, arg_rank, span0, span1, &
		ptype, atype, exp_rank, pos0
	logical :: bool, types_match

	type(fn_t) :: fn
	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: left, right, keyword, identifier, &
		comma, lparen, rparen, token, dummy
	type(text_span_t) :: span

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

			! TODO: make fns for these long, deeply-nested cases

			if (parser%peek_kind(1) /= lparen_token) then

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

			else

				! Function call expression
				identifier = parser%match(identifier_token)

				!print *, 'parsing fn_call_expr'
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

				fn = parser%fns%search(identifier%text, id_index, io)
				if (io /= exit_success) then

					span = new_span(identifier%pos, len(identifier%text))
					call parser%diagnostics%push( &
						err_undeclare_fn(parser%context(), &
						span, identifier%text))

					! No more tokens are consumed below, so we can just return
					! to skip cascading fn arg count/type errors
					return

				end if

				expr%kind = fn_call_expr

				expr%identifier = identifier

				expr%val%type = fn%type
				if (fn%type == array_type) then
					allocate(expr%val%array)
					expr%val%array%type = fn%array_type
					expr%val%array%rank = fn%rank
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

					allocate(expr%body)
					expr%body = fn%node%body
					expr%params = fn%node%params

				end if

				! TODO: does fn need to be a syntax node member?  I think we can
				! just look it up later by identifier/id_index like we do for
				! variable value
				!expr%fn = fn

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

				expr%id_index = id_index

				call syntax_nodes_copy(expr%args, &
					args%v( 1: args%len_ ))

				!print *, 'done parsing fn_call_expr'

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

function parser_current_context(parser) result(context)
	class(parser_t) :: parser
	type(text_context_t) :: context

	!parser%contexts%v(parser%current_unit()), span, type_text))
	context = parser%contexts%v( parser%current_unit() )

end function parser_current_context

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

			print *, 'get_dir(src_file) = ', get_dir(src_file)

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

subroutine parse_type(parser, type_text, rank)

	! TODO: encapsulate out-args in struct if adding any more

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	integer, intent(out) :: rank

	!********

	type(syntax_token_t) :: colon, type, comma, lbracket, rbracket, semi

	if (parser%current_kind() == lbracket_token) then

		! Array param
		lbracket = parser%match(lbracket_token)
		type     = parser%match(identifier_token)
		semi     = parser%match(semicolon_token)

		rank  = 0
		do while ( &
			parser%current_kind() /= rbracket_token .and. &
			parser%current_kind() /= eof_token)

			rank = rank + 1
			colon = parser%match(colon_token)
			if (parser%current_kind() /= rbracket_token) then
				comma = parser%match(comma_token)
			end if

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

function parse_fn_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: i, pos0, pos1, pos2, rank, itype

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

	fn_kw = parser%match(fn_keyword)

	identifier = parser%match(identifier_token)

	!print *, 'parsing fn ', identifier%text

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
		!print *, 'name, type = ', names%v(i)%s, ', ', types%v(i)%s

		fn%params(i)%name = names%v(i)%s

		itype = lookup_type( types%v(i)%s )
		if (itype == unknown_type) then

			! TODO: make an array of pos's for each param to underline
			! individual param, not whole param list

			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context(), span, types%v(i)%s))
				!parser%contexts%v(name%unit_), span, types%v(i)%s))

		end if

		if (is_array%v(i)) then
			fn%params(i)%type = array_type
			fn%params(i)%array_type = itype
			fn%params(i)%rank = ranks%v(i)
		else
			fn%params(i)%type = itype
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
		end if

		call parser%vars%insert(fn%params(i)%name, val, parser%num_vars)

	end do

	! Rust uses "->" as a delimiter between the fn and its return type.  Here
	! I choose ":" instead as it seems more consistent, at least for normal
	! non-assignable fns.  There is some discussion on the Rust reasoning here:
	!
	!     https://stackoverflow.com/questions/35018919/whats-the-origin-of-in-rust-function-definition-return-types
	!

	fn%type = void_type
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

	body = parser%parse_statement()

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

