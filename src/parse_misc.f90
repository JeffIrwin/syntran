
!===============================================================================

submodule (syntran__parse_m) syntran__parse_misc

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

module function tokens_str(parser) result(str)

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

module function match(parser, kind) result(token)

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

recursive module subroutine preprocess(parser, tokens_in, src_file, contexts, unit_)

	! src_file is the filename of the current file being processed, i.e. the
	! *includer*, not the includee

	class(parser_t) :: parser
	type(syntax_token_t), intent(in) :: tokens_in(:)
	character(len = *), intent(in) :: src_file

	type(text_context_vector_t), intent(inout) :: contexts

	integer, intent(inout) :: unit_

	!********

	character(len = :), allocatable :: inc_text, filename

	integer :: i, j, iostat, unit_0

	type(parser_t) :: inc_parser

	type(text_span_t) :: span

	type(syntax_token_t) :: token, token_peek, lparen, rparen, semicolon
	type(syntax_token_vector_t) :: tokens_out

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

module function match_pre(parser, kind, tokens, token_index, context) result(token)

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

module function parse_unit(parser) result(unit)

	class(parser_t) :: parser

	type(syntax_node_t) :: unit

	!********

	type(syntax_node_vector_t) :: members
	type(syntax_token_t) :: dummy

	integer :: i, pos0, num_vars0, num_fns0, num_structs0

	!print *, 'starting parse_unit()'

	!****************

	! First pass
	parser%ipass = 0

	members = new_syntax_node_vector()
	i = 0

	!! Pushing scope breaks interactive interpretation, but we may want it later
	!! for interpetting multiple files.  Another alternative would be chaining
	!! interpreted statements like Immo does

	!call parser%vars%push_scope()
	!call parser%locs%push_scope()

	!print *, "parser pos beg = ", parser%pos
	!print *, "num fns = ", parser%num_fns

	num_vars0 = parser%num_vars  ! not necessarily 0 for the REPL
	num_fns0 = parser%num_fns  ! includes intrinsic fns
	num_structs0 = parser%num_structs

	do while (parser%current_kind() /= eof_token)

		!print *, "    parser pos = ", parser%pos

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		select case (parser%current_kind())
		case (fn_keyword)
			call members%push(parser%parse_fn_declaration())
		case (struct_keyword)
			call members%push(parser%parse_struct_declaration())
		case default
			call members%push(parser%parse_statement())
		end select

		! Break infinite loops
		if (parser%pos == pos0) dummy = parser%next()

	end do
	!print *, "parser pos end = ", parser%pos
	!print *, "num fns = ", parser%num_fns

	!****************

	!print *, ""
	!print *, ""
	!print *, " ===========  PARSING PASS NUMBER 2 ========== "
	!print *, ""
	!print *, ""

	! TODO: if any errors, skip second pass.  Although be careful to still do
	! stuff at end of routine

	! Second pass
	parser%pos = 1
	parser%ipass = 1

	parser%num_vars = num_vars0
	parser%num_fns = num_fns0
	parser%num_structs = num_structs0

	! TODO: Double check struct resetting.  Does anything else need to be reset?  

	members = new_syntax_node_vector()
	i = 0

	!left  = parser%match(lbrace_token)

	!call parser%vars%push_scope()
	!call parser%locs%push_scope()

	! TODO: dry?  Two passes are almost the same, but also there are only two of
	! them

	!print *, "parser pos beg = ", parser%pos
	do while (parser%current_kind() /= eof_token)

		!print *, "    parser pos = ", parser%pos

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		select case (parser%current_kind())
		case (fn_keyword)
			call members%push(parser%parse_fn_declaration())
		case (struct_keyword)
			call members%push(parser%parse_struct_declaration())
		case default
			call members%push(parser%parse_statement())
		end select

		! Break infinite loops
		if (parser%pos == pos0) dummy = parser%next()

	end do
	!print *, "parser pos end = ", parser%pos

	!call parser%vars%pop_scope()
	!call parser%locs%pop_scope()

	!right = parser%match(rbrace_token)

	!****************

	unit%kind = translation_unit

	! Convert to standard array
	call syntax_nodes_copy(unit%members, members%v( 1: members%len_ ))

	! Eof is matched in the caller syntax_parse() to deal with broken stdin
	! lines with interactive interpretation

end function parse_unit

!===============================================================================

recursive module function new_parser(str, src_file, contexts, unit_) result(parser)

	character(len = *), intent(in) :: str, src_file

	type(text_context_vector_t) :: contexts

	integer, intent(inout) :: unit_

	type(parser_t) :: parser

	!********

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

	! Allocate scoped var dicts

	parser%vars%scope_cap = SCOPE_CAP_INIT
	allocate(parser%vars%dicts( parser%vars%scope_cap ))

	parser%locs%scope_cap = SCOPE_CAP_INIT
	allocate(parser%locs%dicts( parser%locs%scope_cap ))

	!print *, 'tokens%len_ = ', tokens%len_
	if (debug > 1) print *, parser%tokens_str()

end function new_parser

!===============================================================================

end submodule syntran__parse_misc

!===============================================================================

