
!===============================================================================

submodule (syntran__parse_m) syntran__parse_misc

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

module function tokens_str(parser) result(str_)

	class(parser_t) :: parser

	character(len = :), allocatable :: str_

	!********

	integer :: i

	! This will crash for very long token lists, but it should suffice for basic
	! debugging

	str_ = 'tokens = '//line_feed//'['//line_feed
	do i = 1, size(parser%tokens)
		str_ = str_//tab &
				//'<'//           parser%tokens(i)%text  //'> ' &
				//'<'//kind_name( parser%tokens(i)%kind )//'>'  &
				//line_feed
	end do
	str_ = str_//']'//line_feed

end function tokens_str

!===============================================================================

module subroutine check_type_clash(parser, name, pos)

	! At a variable-binding site (let/const/for-iterator/fn-param), check
	! whether `name` clashes with an already-declared enum or struct type
	! name.  A name can't be both (E26/E27/E92 already forbid that), so enum
	! vs struct here is just "which message to print", not an ambiguity
	!
	! Only push in pass 0.  parser%enums/%structs (unlike the *_names
	! bookkeeping vectors) are never cleared between passes, so by pass 1 they
	! already hold every type declared anywhere in the file -- including ones
	! that, in this pass's own source-order traversal, appear later than
	! `name`.  Pushing unconditionally would make this fire at both
	! declaration sites regardless of which one is textually first, instead
	! of just the second one.  parse_unit() (parse_misc.f90) falls back to
	! pass 0's diagnostics when pass 1 comes back clean, so gating here still
	! reports the single correct diagnostic

	class(parser_t) :: parser
	character(len = *), intent(in) :: name
	integer, intent(in) :: pos

	!********

	type(text_span_t) :: span

	if (parser%ipass /= 0) return

	if (parser%enums%exists(name)) then
		span = new_span(pos, len(name))
		call parser%diagnostics%push(err_var_type_clash( &
			parser%context(), span, name, "enum"))
	else if (parser%structs%exists(name)) then
		span = new_span(pos, len(name))
		call parser%diagnostics%push(err_var_type_clash( &
			parser%context(), span, name, "struct"))
	end if

end subroutine check_type_clash

!===============================================================================

module subroutine check_enum_name_value(parser, expr)

	! Push EC_ENUM_NAME_VALUE if `expr` is the special bare-enum-name form
	! (see parse_enum_name_expr()).  A bare enum name is only valid as a
	! `for` loop's iterable or as an argument to size()/str()/println()/
	! writeln() -- everywhere else that would bind or pass it as an ordinary
	! value, call this to reject it.  Called at every such site: let/const
	! init, assignment RHS, return, fn/method call args, struct member init,
	! and array literal elements

	class(parser_t) :: parser
	type(syntax_node_t), intent(in) :: expr

	!********

	type(text_span_t) :: span

	if (.not. expr%is_enum_name) return

	span = new_span(expr%identifier%pos, len(expr%identifier%text))
	call parser%diagnostics%push(err_enum_name_value( &
		parser%context(), span, expr%val%enum_name))

end subroutine check_enum_name_value

!===============================================================================

module subroutine check_var_clash(parser, name, pos, type_kind)

	! At a struct/enum declaration site, check whether `name` clashes with an
	! already-declared module-level variable.  Only `vars` is checked, not
	! `locs`: struct/enum declarations are top-level, and `locs` may still
	! hold stale entries from a previously parsed fn body
	!
	! Only push in pass 0 -- see the comment in check_type_clash() above;
	! `parser%vars` has the same cross-pass leakage problem

	class(parser_t) :: parser
	character(len = *), intent(in) :: name
	integer, intent(in) :: pos
	character(len = *), intent(in) :: type_kind

	!********

	integer :: id_index, io
	type(value_t) :: val
	type(text_span_t) :: span

	if (parser%ipass /= 0) return

	call parser%vars%search(name, id_index, io, val)
	if (io == 0) then
		span = new_span(pos, len(name))
		call parser%diagnostics%push(err_var_type_clash( &
			parser%context(), span, name, type_kind))
	end if

end subroutine check_var_clash

!===============================================================================

module subroutine match(parser, kind, token)

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t), intent(out) :: token

	!********

	integer :: len_text

	type(syntax_token_t) :: current
	type(text_span_t) :: span

	! If current_text() and current_pos() helper fns are added, this local var
	! current can be eliminated
	call parser%current(current)

	if (parser%current_kind() == kind) then
		call parser%next(token)
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

	call new_token(token, kind, current%pos, null_char)
	!token = new_token(bad_token, current%pos, null_char)
	!token = new_token(kind, current%pos, "")

	token%unit_ = current%unit_
	!print *, 'setting token%unit_ = ', token%unit_

end subroutine match

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
			call parser%match_pre(lparen_token, tokens_in, i, contexts%v(unit_0), lparen)

			! Prepend with path to src_file, unless already absolute

			!print *, 'get_dir(src_file) = ', get_dir(src_file)

			i = i + 1
			filename = resolve_path(get_dir(src_file), tokens_in(i)%val%str%s)

			!print *, 'include filename = ', quote(filename)

			if (.not. exists(filename)) then
				span = new_span(tokens_in(i)%pos, len(tokens_in(i)%text))
				call parser%diagnostics%push( &
					err_inc_404(contexts%v(unit_0), span, tokens_in(i)%text))

				! Could probably be refactored
				call parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0), rparen)
				call parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0), semicolon)
				cycle
			end if

			inc_text = read_file(filename, iostat)
			if (iostat /= exit_success) then
				! For example, `#include(".");` exists but cannot be read
				! (read_file() rejects directories portably, see is_dir())
				span = new_span(tokens_in(i)%pos, len(tokens_in(i)%text))
				call parser%diagnostics%push( &
					err_inc_read(contexts%v(unit_0), span, tokens_in(i)%text))
				call parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0), rparen)
				call parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0), semicolon)
				cycle
			end if

			!print *, 'len(inc_text) = ', len(inc_text)
			!print *, 'inc_text = '
			!print *, inc_text

			! Any nested includes are handled in this new_parser() call
			call new_parser(inc_parser, inc_text, filename, contexts, unit_)

			! Add includee tokens to includer.  Minus 1 because included eof_token
			do j = 1, size(inc_parser%tokens) - 1
				call tokens_out%push( inc_parser%tokens(j) )
			end do

			! Push included diagnostics (from lexing) into parent parser
			call parser%diagnostics%push_all( inc_parser%diagnostics )

			call parser%match_pre(rparen_token   , tokens_in, i, contexts%v(unit_0), rparen)
			call parser%match_pre(semicolon_token, tokens_in, i, contexts%v(unit_0), semicolon)

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

module subroutine match_pre(parser, kind, tokens, token_index, context, token)

	! This is like match(), but it can run during preprocessing before the
	! parser is fully constructed, at the cost of having a bunch of arguments.
	!
	! Things could probably be refactored by adding the temp syntax vector into
	! a new parser member and deleting it after preprocessing is done.  Then
	! this fn could work with parser members instead of taking so many args

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t), intent(out) :: token
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

	call new_token(token, kind, current%pos, null_char)
	token%unit_ = current%unit_
	!print *, 'setting token%unit_ = ', token%unit_

end subroutine match_pre

!===============================================================================

recursive module subroutine parse_unit(parser, unit)

	class(parser_t) :: parser

	type(syntax_node_t), intent(out) :: unit

	!********

	type(syntax_node_vector_t) :: members

	integer :: i, num_vars0, num_fns0, num_structs0, num_enums0
	integer :: ndiag_pre

	! Pass-0 diagnostics, saved so they can be restored if pass 1 emits none
	! (see the comment at the pass-1 fallback below)
	type(string_vector_t) :: diags0

	!print *, 'starting parse_unit()'

	!****************

	! First pass
	parser%ipass = 0

	! Diagnostics that exist before either pass: lexer errors and #include
	! preprocessor errors, both pushed by new_parser().  Pass 0's own output
	! is everything above this index, and gets discarded before pass 1
	ndiag_pre = parser%diagnostics%len_

	!! Pushing scope breaks interactive interpretation, but we may want it later
	!! for interpetting multiple files.  Another alternative would be chaining
	!! interpreted statements like Immo does

	!call parser%vars%push_scope()
	!call parser%locs%push_scope()

	!print *, "parser pos beg = ", parser%pos
	!print *, "num fns = ", parser%num_fns

	num_vars0 = parser%num_vars  ! not necessarily 0 for the REPL
	num_fns0 = parser%num_fns    ! includes intrinsic fns
	num_structs0 = parser%num_structs
	num_enums0 = parser%num_enums
	parser%fn_names = new_string_vector()
	parser%var_names = new_string_vector()
	parser%struct_names = new_string_vector()
	parser%enum_names = new_string_vector()

	call parse_unit_pass(parser, members)
	!print *, "parser pos end = ", parser%pos
	!print *, "num fns = ", parser%num_fns

	!****************

	! Pass 0 exists only to collect signatures, so its diagnostics are
	! throwaway: pass 1 re-parses the same tokens and re-pushes anything that
	! is still wrong, this time with fully resolved types.  Keeping both
	! copies would print every simple error twice, which is why this used to
	! skip pass 1 outright whenever pass 0 was non-empty -- but that let an
	! error late in the file pre-empt an earlier one that only pass 1 can
	! detect (e.g. E102 was invisible in a file with two ungated E40s later
	! on, because the E40s aborted the pass that finds the E102).
	!
	! So: always run pass 1, and report pass 1's diagnostics instead.  A few
	! diagnostic families (redeclaration, use-before-declaration) can only
	! ever be raised in pass 0 -- see the fallback after the second loop
	! below.
	!
	! Skip pass 1 only for an incomplete interactive line (a match() failure
	! at eof).  syntax_parse() rolls that parse back and re-parses from
	! scratch once the user types the rest, so a second pass buys nothing and
	! pass 1 has never run on partial input before
	diags0 = parser%diagnostics
	parser%diagnostics%len_ = ndiag_pre

	if (.not. parser%expecting) then

		!print *, ""
		!print *, ""
		!print *, " ===========  PARSING PASS NUMBER 2 ========== "
		!print *, ""
		!print *, ""

		! Second pass
		parser%pos = 1
		parser%ipass = 1

		parser%num_vars = num_vars0
		parser%num_fns = num_fns0
		parser%num_structs = num_structs0
		parser%num_enums = num_enums0

		! Resetting the counters (not the tables) is enough: pass 2 re-inserts
		! every struct/enum from num_structs0/num_enums0 with overwrite = .true.
		! (parser%ipass > 0, c.f. parse_struct_declaration()/
		! parse_enum_declaration() in parse_fn.f90), landing on the exact same
		! id_index as pass 1 assigned it

		! Pass 2 re-parses every `use` statement, so the duplicate-import
		! record has to start empty again or every import would look like a
		! duplicate (c.f. parse_use_statement() in parse_control.f90)
		call parser%imported_modules%destroy()
		call parser%imported_modules%init(16)

		!left  = parser%match(lbrace_token)

		!call parser%vars%push_scope()
		!call parser%locs%push_scope()

		!print *, "parser pos beg = ", parser%pos
		call parse_unit_pass(parser, members)
		!print *, "parser pos end = ", parser%pos

		!call parser%vars%pop_scope()
		!call parser%locs%pop_scope()

		!right = parser%match(rbrace_token)

	end if  ! not expecting more input

	! A few diagnostics are structurally pass-0-only and pass 1 physically
	! cannot re-emit them:
	!
	!   - the redeclaration family (E22/E24/E26/E92).  It is raised from the
	!     iostat of a dict insert, and `overwrite` is .false. only in pass 0
	!     -- pass 1 must overwrite, since the tables (unlike the counters)
	!     still hold everything pass 0 inserted
	!   - use-before-declaration, for the same reason: pass 0 already put the
	!     later `let` in the vars dict, so pass 1 resolves it happily
	!
	! Restoring pass 0's list when pass 1 came back clean keeps the old
	! behavior exactly for those, and guarantees a program pass 0 rejected is
	! never silently accepted and evaluated
	if (parser%diagnostics%len_ == ndiag_pre .and. diags0%len_ > ndiag_pre) then
		parser%diagnostics = diags0
	end if

	!****************

	unit%kind = translation_unit

	! Move members from vector directly (avoids deep copy)
	allocate(unit%members(members%len_))
	do i = 1, members%len_
		call syntax_node_move_into(members%v(i), unit%members(i))
	end do

	! Eof is matched in the caller syntax_parse() to deal with broken stdin
	! lines with interactive interpretation

end subroutine parse_unit

!===============================================================================

recursive subroutine parse_unit_pass(parser, members)

	! The statement-parsing loop shared by both passes of parse_unit().  Not
	! type-bound (no `module` prefix, no parse.f90 interface), since it is a
	! private implementation detail of parse_unit() alone

	class(parser_t) :: parser

	type(syntax_node_vector_t), intent(out) :: members

	!********

	type(syntax_node_t)  :: stmt_tmp
	type(syntax_token_t) :: dummy

	integer :: pos0

	members = new_syntax_node_vector()

	do while (parser%current_kind() /= eof_token)

		!print *, "    parser pos = ", parser%pos

		pos0 = parser%pos

		select case (parser%current_kind())
		case (fn_keyword)
			call parser%parse_fn_declaration(stmt_tmp)
			call members%push_move(stmt_tmp)
		case (struct_keyword)
			call parser%parse_struct_declaration(stmt_tmp)
			call members%push_move(stmt_tmp)
		case (enum_keyword)
			call parser%parse_enum_declaration(stmt_tmp)
			call members%push_move(stmt_tmp)
		case default
			call parser%parse_statement(stmt_tmp)
			call members%push_move(stmt_tmp)
		end select

		! Break infinite loops
		if (parser%pos == pos0) call parser%next(dummy)

	end do

end subroutine parse_unit_pass

!===============================================================================

recursive module subroutine new_parser(parser, str_, src_file, contexts, unit_)

	type(parser_t), intent(out) :: parser

	character(len = *), intent(in) :: str_, src_file

	type(text_context_vector_t) :: contexts

	integer, intent(inout) :: unit_

	!********

	type(lexer_t) :: lexer

	type(syntax_token_t) :: token
	type(syntax_token_vector_t) :: tokens

	! Lex and get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str_, src_file, unit_)
	do
		call lexer%lex(token)
		!print *, 'token%unit_ = ', token%unit_

		if (token%kind /= whitespace_token .and. &
		    token%kind /= comment_token   .and. &
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

	call parser%import_stack%init(16)
	call parser%imported_modules%init(16)

	!print *, 'tokens%len_ = ', tokens%len_
	if (debug > 1) print *, parser%tokens_str()

end subroutine new_parser

!===============================================================================

end submodule syntran__parse_misc

!===============================================================================

