
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

	!********

	interface
		! Implemented in parse_control.f90

		module function parse_if_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_if_statement

		module function parse_for_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_for_statement

		module function parse_while_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_while_statement

		module function parse_block_statement(parser) result(block)
			class(parser_t) :: parser
			type(syntax_node_t) :: block
		end function parse_block_statement

		module function parse_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_statement

	end interface

	interface
		! Implemented in parse_expr.f90

		module recursive function parse_expr_statement(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_expr_statement

		module recursive function parse_expr(parser, parent_prec) result(expr)
			class(parser_t) :: parser
			integer, optional, intent(in) :: parent_prec
			type(syntax_node_t) :: expr
		end function parse_expr

		module function parse_primary_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_primary_expr

		module function parse_name_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_name_expr

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
			! fullpath/realpath fns in utils.f90

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

