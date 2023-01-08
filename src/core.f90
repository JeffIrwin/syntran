
!===============================================================================

module core_m

	! This module contains private members.  For the public API, see syntran.f90

	use iso_fortran_env

	use errors_m
	use utils

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	! Debug logging verbosity (0 == silent)
	integer, parameter :: debug = 0

	integer, parameter ::   &
		syntran_major =  0, &
		syntran_minor =  0, &
		syntran_patch =  5

	! TODO:
	!
	! Add:
	!  - for loop syntax:
	!
	!    for i = 1: 5
	!    {
	!       i
	!    }
	!    // 1, 2, 3, 4, 5
	!
	!    for i = 1: 2: 5
	!    {
	!       i
	!    }
	!    // 1,    3,    5
	!
	!    // And finally, after doing some array handling work, something like:
	!    for i = 1, 2, 4, 5
	!    {
	!       i
	!    }
	!    // 1, 2,   4, 5
	!
	!  - compound assignment: +=, -=, *=, etc.
	!    * Does any language have "**="? This will
	!  - ++, --
	!  - <, >, <=, >=
	!  - funcions
	!  - arrays
	!  - floats, characters, strings
	!  - structs
	!  - enums
	!  - % (mod/modulo (which? Fortran handles negatives differently in one))
	!  - xor
	!  - bitwise operators



	! TODO: optional braces for global compilation_unit statements? is
	! translation_unit unused?

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
			translation_unit    = 33, &
			block_statement     = 32, &
			expr_statement      = 31, &
			lbrace_token        = 30, &
			rbrace_token        = 29, &
			sstar_token         = 28, &
			let_keyword         = 27, &
			name_expr           = 26, &
			equals_token        = 25, & ! '='
			assignment_expr     = 24, &
			bang_equals_token   = 23, &
			eequals_token       = 22, & ! '=='
			and_keyword         = 21, &
			or_keyword          = 20, &
			not_keyword         = 19, &
			bool_expr           = 18, &
			literal_expr        = 17, &
			true_keyword        = 16, &
			false_keyword       = 15, &
			identifier_token    = 14, &
			unary_expr          = 13, &
			lparen_token        = 12, &
			rparen_token        = 11, &
			num_expr            = 10, &
			binary_expr         =  9, &
			star_token          =  8, &
			slash_token         =  7, &
			bad_token           =  6, &
			plus_token          =  5, &
			minus_token         =  4, &
			whitespace_token    =  3, &
			num_token           =  2, &
			eof_token           =  1

	! A note on naming: Immo calls '==' equals_equals_token, but I think that
	! invites tab-completion mistakes so I went with eequals_token.  Same for
	! sstar_token (and upcoming pplus_token, mminus_token, ...)

	!********

	type value_t
		! TODO: rename this kind to type?  Careful w/ search/replace
		integer :: kind
		logical :: bval
		integer :: ival
		contains
			procedure :: str => value_str
	end type value_t

	!********

	type syntax_token_t

		integer :: kind
		type(value_t) :: val
		integer :: pos
		character(len = :), allocatable :: text

	end type syntax_token_t

	!********

	type syntax_node_t

		integer :: kind

		! This structure could be more efficient.  For example, unary
		! expressions don't use left, name expressions don't use left or right,
		! binary expressions don't use an identifier, etc.

		type(syntax_node_t), allocatable :: left, right, statements(:)
		type(syntax_token_t) :: op, identifier
		type(value_t) :: val

		! TODO: add text_span_t member here?  Immo did this, may be needed for
		! line numbers in diagnostics
		type(string_vector_t) :: diagnostics

		! Only used to handle comment/whitespace lines
		logical :: is_empty = .false.

		! FIXME: when adding new members here, make sure to explicitly copy them
		! in syntax_node_copy, or else assignment will yield bugs

		contains
			procedure :: str => syntax_node_str, log_diagnostics

			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy

	end type syntax_node_t

	!********

	type lexer_t

		! The lexer takes a string of characters and divides it into into tokens
		! or words

		character(len = :), allocatable :: text
		integer :: pos

		type(string_vector_t) :: diagnostics

		! Both the lexer and the parser have current() and lex()/next() member
		! fns.  current_char() returns a char, while the others return syntax
		! tokens
		contains
			procedure :: lex, peek => peek_char, current => current_char, &
				lookahead => lookahead_char, read_single_line_comment

	end type lexer_t

	!********

	! Dependencies between types could make this module difficult to split into
	! separate files.  I think I like the monolithic design anyway

	type ternary_tree_node_t
		character :: split_char = ''
		type(ternary_tree_node_t), allocatable :: left, mid, right
		type(value_t), allocatable :: val
		!contains
		!	procedure :: print => ternary_node_print
	end type ternary_tree_node_t

	type variable_dictionary_t
		type(ternary_tree_node_t), allocatable :: root
		contains
			procedure :: &
				insert => variable_insert, &
				search => variable_search
	end type variable_dictionary_t

	!********

	type parser_t

		! The parser takes a string of tokens (technically an array) and
		! constructs higher-level structures such as terms and expressions, like
		! constructing a phrase or sentence from words

		type(syntax_token_t), allocatable :: tokens(:)
		integer :: pos

		type(string_vector_t) :: diagnostics

		type(variable_dictionary_t) :: variables

		contains
			procedure :: match, tokens_str, current_kind, &
				current => current_token, next => next_parser_token, &
				peek => parser_peek_token, peek_kind, &
				parse_expr, parse_primary_expr, parse_expr_statement, &
				parse_statement, parse_block_statement

	end type parser_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

	!********

	type syntax_node_vector_t
		type(syntax_node_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_node
	end type syntax_node_vector_t

!===============================================================================

contains

!===============================================================================

function variable_search(dictionary, key, iostat) result(val)

	class(variable_dictionary_t), intent(in) :: dictionary
	character(len = *), intent(in) :: key
	type(value_t) :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: io

	val = ternary_search(dictionary%root, key, io)
	if (present(iostat)) iostat = io

end function variable_search

!===============================================================================

recursive function ternary_search(node, key, iostat) result(val)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: iostat
	type(value_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key "', key, '"'

	iostat = exit_success

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		val = ternary_search(node%left , key, iostat)
		return
	else if (k > node%split_char) then
		val = ternary_search(node%right, key, iostat)
		return
	else if (len(ey) > 0) then
		val = ternary_search(node%mid  , ey, iostat)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		return
	end if

	val = node%val

	!print *, 'done ternary_search'
	!print *, ''

end function ternary_search

!===============================================================================

subroutine variable_insert(dictionary, key, val, iostat, overwrite)

	! There are a couple reasons for having this wrapper:
	!
	!   - dictionary is not allocatable, while dictionary%root is.  type-bound
	!     methods are not allowed for allocatable types
	!   - it's an abstraction away from the dictionary implementation.
	!     currently I'm using a ternary tree, but the dictionary could
	!     alternatively be implemented using another data structure like a trie
	!     or a radix tree
	!   - having an allocatable root is helpful both for the ternary
	!     insert/delete implementation and for moving the dictionary without
	!     copying in syntax_parse()

	class(variable_dictionary_t) :: dictionary
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer :: io
	logical :: overwritel

	!print *, 'inserting "', key, '"'

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	call ternary_insert(dictionary%root, key, val, io, overwritel)
	if (present(iostat)) iostat = io

end subroutine variable_insert

!===============================================================================

recursive subroutine ternary_insert(node, key, val, iostat, overwrite)

	type(ternary_tree_node_t), intent(inout), allocatable :: node
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val
	integer, intent(out) :: iostat
	logical, intent(in) :: overwrite

	!********

	character :: k
	character(len = :), allocatable :: ey

	iostat = exit_success

	!print *, 'inserting key "', key, '"'

	! key == k//ey.  Get it? :)
	k   = key(1:1)
	 ey = key(2:)

	if (.not. allocated(node)) then
		!print *, 'allocate'
		allocate(node)
		node%split_char = k
	else if (k < node%split_char) then
		!print *, 'left'
		call ternary_insert(node%left , key, val, iostat, overwrite)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call ternary_insert(node%right, key, val, iostat, overwrite)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call ternary_insert(node%mid  , ey, val, iostat, overwrite)
		return
	end if

	! node%val doesn't really need to be declared as allocatable (it's
	! a scalar anyway), but it's just a convenient way to check if
	! a duplicate key has already been inserted or not.  We could add
	! a separate logical member to node for this instead if needed

	! This is not necessarily a failure unless we don't want to overwrite.  In
	! the evaluator, we will insert values for variables which have already been
	! declared
	if (allocated(node%val) .and. .not. overwrite) then
		!print *, 'key already inserted'
		iostat = exit_failure
		return
	end if

	node%val = val

	!print *, 'done inserting'
	!print *, ''

end subroutine ternary_insert

!===============================================================================

function new_syntax_token_vector() result(vector)

	type(syntax_token_vector_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_token_vector

!===============================================================================

subroutine push_token(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(syntax_token_vector_t) :: vector
	type(syntax_token_t) :: val

	!********

	type(syntax_token_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len ) = val

end subroutine push_token

!===============================================================================

function new_syntax_node_vector() result(vector)

	type(syntax_node_vector_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_node_vector

!===============================================================================

subroutine push_node(vector, val)

	class(syntax_node_vector_t) :: vector
	type(syntax_node_t) :: val

	!********

	type(syntax_node_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector ====================================='

		tmp_cap = 2 * vector%len
		allocate(tmp( tmp_cap ))

		!print *, 'copy 1'
		!!tmp(1: vector%cap) = vector%v
		do i = 1, vector%cap
			tmp(i) = vector%v(i)
		end do

		!print *, 'move'
		!!call move_alloc(tmp, vector%v)

		deallocate(vector%v)
		allocate(vector%v( tmp_cap ))

		! Unfortunately we have to copy TO tmp AND back FROM tmp.  I guess the
		! fact that each node itself has allocatable members creates invalid
		! references otherwise.

		!print *, 'copy 2'
		!!vector%v(1: vector%cap) = tmp(1: vector%cap)
		do i = 1, vector%cap
			vector%v(i) = tmp(i)
		end do

		vector%cap = tmp_cap

	end if

	!print *, 'set val'
	vector%v( vector%len ) = val
	!print *, 'done push_node'

end subroutine push_node

!===============================================================================

function kind_name(kind)

	integer, intent(in) :: kind

	character(len = :), allocatable :: kind_name

	character(len = *), parameter :: names(*) = [ &
			"eof_token        ", & !  1
			"num_token        ", & !  2
			"whitespace_token ", & !  3
			"minus_token      ", & !  4
			"plus_token       ", & !  5
			"bad_token        ", & !  6
			"slash_token      ", & !  7
			"star_token       ", & !  8
			"binary_expr      ", & !  9
			"num_expr         ", & ! 10
			"rparen_token     ", & ! 11
			"lparen_token     ", & ! 12
			"unary_expr       ", & ! 13
			"identifier_token ", & ! 14
			"false_keyword    ", & ! 15
			"true_keyword     ", & ! 16
			"literal_expr     ", & ! 17
			"bool_expr        ", & ! 18
			"not_keyword      ", & ! 19
			"or_keyword       ", & ! 20
			"and_keyword      ", & ! 21
			"eequals_token    ", & ! 22
			"bang_equals_token", & ! 23
			"assignment_expr  ", & ! 24
			"equals_token     ", & ! 25
			"name_expr        ", & ! 26
			"let_keyword      ", & ! 27
			"sstar_token      ", & ! 28
			"rbrace_token     ", & ! 29
			"lbrace_token     ", & ! 30
			"expr_statement   ", & ! 31
			"block_statement  ", & ! 32
			"translation_unit "  & ! 33
		]

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end function

!===============================================================================

recursive function syntax_node_str(node, indent) result(str)

	! Convert tree to string in JSON-ish format

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str

	!********

	character(len = :), allocatable :: indentl, kind, left, op, right, val, &
		type, identifier, block

	integer :: i

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind  = '//kind_name(node%kind)//line_feed

	left  = ''
	op    = ''
	right = ''
	val   = ''
	block = ''

	identifier = ''

	type  = indentl//'    type  = '//kind_name(node%val%kind)//line_feed

	if      (node%kind == binary_expr) then

		left  = indentl//'    left  = '//node%left %str(indentl//'    ') &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == block_statement) then

		do i = 1, size(node%statements)
			block = block // node%statements(i)%str(indentl//'    ')
		end do

	else if (node%kind == assignment_expr) then

		identifier  = indentl//'    identifier = '//node%identifier%text &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == unary_expr) then

		op    = indentl//'    op    = '//node%op%text//line_feed
		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == literal_expr) then
		val   = indentl//'    val   = '//node%val%str()//line_feed
	end if

	str = line_feed// &
		indentl//'{'//line_feed// &
			kind       // &
			type       // &
			identifier // &
			left       // &
			op         // &
			right      // &
			val        // &
			block      // &
		indentl//'}'

end function syntax_node_str

!===============================================================================

recursive subroutine syntax_node_copy(dst, src)

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my node type, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading

	class(syntax_node_t), intent(inout) :: dst
	class(syntax_node_t), intent(in)    :: src

	!********

	integer :: i, n

	if (debug > 3) print *, 'starting syntax_node_copy()'

	dst%kind = src%kind
	dst%op   = src%op
	dst%val  = src%val

	dst%identifier = src%identifier

	dst%diagnostics = src%diagnostics

	dst%is_empty = src%is_empty

	if (allocated(src%left)) then
		!if (debug > 1) print *, 'copy() left'
		if (.not. allocated(dst%left)) allocate(dst%left)
		dst%left = src%left
	end if

	if (allocated(src%right)) then
		!if (debug > 1) print *, 'copy() right'
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

	if (allocated(src%statements)) then
		!print *, 'copying statements'

		n = size(src%statements)
		!print *, 'n = ', n

		!!if (allocated(dst%statements)) deallocate(dst%statements)
		!!print *, 'alloc'
		!!allocate(dst%statements( n ))
		!!print *, 'copy'
		!!dst%statements = src%statements
		!!print *, 'done 1'

		!print *, 'dealloc'
		if (allocated(dst%statements)) deallocate(dst%statements)
		!print *, 'alloc'
		allocate(dst%statements(n))

		!! This explicit loop is apparently required
		!print *, 'loop'
		do i = 1, n
			!print *, i
			dst%statements(i) = src%statements(i)
		end do
		!print *, 'done loop'

	end if

	if (debug > 3) print *, 'done syntax_node_copy()'

end subroutine syntax_node_copy

!===============================================================================

function new_token(kind, pos, text, val) result(token)

	integer :: kind, pos

	character(len = *) :: text

	type(value_t), optional :: val

	type(syntax_token_t) :: token

	token%kind = kind
	token%pos  = pos
	token%text = text

	if (present(val)) token%val  = val

end function new_token

!===============================================================================

character function peek_char(lexer, offset)

	class(lexer_t) :: lexer

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = lexer%pos + offset

	if (pos > len(lexer%text)) then
		peek_char = null_char
		return
	end if

	peek_char = lexer%text(pos: pos)

end function peek_char

!===============================================================================

character function current_char(lexer)
	class(lexer_t) :: lexer
	current_char = lexer%peek(0)
end function current_char

character function lookahead_char(lexer)
	class(lexer_t) :: lexer
	lookahead_char = lexer%peek(1)
end function lookahead_char

!===============================================================================

function lex(lexer) result(token)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: token

	!********

	integer :: kind
	integer :: start, io, ival

	character(len = :), allocatable :: text

	type(text_span_t) :: span
	type(value_t) :: val

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char)
		return
	end if

	start = lexer%pos

	if (is_digit(lexer%current())) then

		do while (is_digit(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		read(text, *, iostat = io) ival
		if (io /= exit_success) then
			span = new_text_span(start, len(text))
			call lexer%diagnostics%push(err_bad_int(span, text))
		end if

		val = new_value(num_expr, ival = ival)
		token = new_token(num_token, start, text, val)
		return

	end if

	if (is_whitespace(lexer%current())) then

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text)
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		do while (is_alphanum(lexer%current()) .or. lexer%current() == '_')
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		! This block handles booleans as well as identifiers, but note that it
		! does not set the value here like the is_digit() case for numbers
		! above.  The boolean value is not set until parse_primary_expr().

		kind = get_keyword_kind(text)
		token = new_token(kind, start, text)
		return

	end if

	select case (lexer%current())

		case ("+")
			token = new_token(plus_token  , lexer%pos, lexer%current())

			! TODO: prefix/postfix inc/dec operators (++, --)

		case ("-")
			token = new_token(minus_token , lexer%pos, lexer%current())

		case ("*")
			if (lexer%lookahead() == "*") then
				lexer%pos = lexer%pos + 1
				token = new_token(sstar_token  , lexer%pos, "**")
			else
				token = new_token(star_token  , lexer%pos, lexer%current())
			end if

		case ("/")
			if (lexer%lookahead() == "/") then

				call lexer%read_single_line_comment()

				! TODO: make "trivia" token types instead of overloading
				! whitespace_token for comments
				text = lexer%text(start: lexer%pos-1)
				token = new_token(whitespace_token, start, text)

			else
				token = new_token(slash_token , lexer%pos, lexer%current())
			end if

		case ("(")
			token = new_token(lparen_token, lexer%pos, lexer%current())

		case (")")
			token = new_token(rparen_token, lexer%pos, lexer%current())

		case ("{")
			token = new_token(lbrace_token, lexer%pos, lexer%current())

		case ("}")
			token = new_token(rbrace_token, lexer%pos, lexer%current())

		case ("=")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(eequals_token, lexer%pos, "==")
			else
				token = new_token(equals_token , lexer%pos, lexer%current())
			end if

		case ("!")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(bang_equals_token, lexer%pos, "!=")
			else

				! TODO: refactor w/ default case below since Fortran is weird
				! about breaking in select case
				token = new_token(bad_token, lexer%pos, lexer%current())

				span = new_text_span(lexer%pos, len(lexer%current()))
				call lexer%diagnostics%push( &
					err_unexpected_char(span, lexer%current()))

			end if

		case default

			token = new_token(bad_token, lexer%pos, lexer%current())

			span = new_text_span(lexer%pos, len(lexer%current()))
			call lexer%diagnostics%push( &
				err_unexpected_char(span, lexer%current()))

	end select
	lexer%pos = lexer%pos + 1

	! TODO: arrow keys create bad tokens in bash on Windows.  Fix that (better
	! yet, override up arrow to do what it does in bash.  c.f. rubik-js)

end function lex

!===============================================================================

! I am NOT planning on implementing multi-line comments.  Use block-insertion in
! your editor to comment-out multiple lines with "//"

subroutine read_single_line_comment(lexer)

	class(lexer_t) :: lexer

	lexer%pos = lexer%pos + 2

	loop: do

		!print *, 'char = ', lexer%current()
		select case (lexer%current())
			case (null_char, carriage_return, line_feed)
				exit loop
		end select

		lexer%pos = lexer%pos + 1
	end do loop
	!print *, 'done'

end subroutine read_single_line_comment

!===============================================================================

function new_value(kind, bval, ival) result(val)

	integer, intent(in) :: kind
	integer, intent(in), optional :: ival
	logical, intent(in), optional :: bval

	type(value_t) :: val

	val%kind = kind
	if (present(bval)) val%bval = bval
	if (present(ival)) val%ival = ival

end function new_value

!===============================================================================

integer function get_keyword_kind(text) result(kind)

	character(len = *), intent(in) :: text

	! Here we start to depart from Fortran syntax (true, not .true.)
	select case (text)

		case ("true")
			kind = true_keyword

		case ("false")
			kind = false_keyword

		case ("not")
			kind = not_keyword

		case ("and")
			kind = and_keyword

		case ("or")
			kind = or_keyword

		case ("let")
			kind = let_keyword

		case default
			kind = identifier_token

	end select

	!print *, 'get_keyword_kind = ', kind

end function get_keyword_kind

!===============================================================================

function new_lexer(text) result(lexer)

	character(len = *) :: text

	type(lexer_t) :: lexer

	lexer%text = text
	lexer%pos = 1

	lexer%diagnostics = new_string_vector()

end function new_lexer

!===============================================================================

function new_parser(str) result(parser)

	character(len = *) :: str

	type(parser_t) :: parser

	!********

	type(syntax_token_t)        :: token
	type(syntax_token_vector_t) :: tokens

	type(lexer_t) :: lexer

	! Get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str)
	do
		token = lexer%lex()

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then
			call tokens%push(token)
		end if

		if (token%kind == eof_token) exit
	end do

	! Convert to standard array (and class member)
	parser%tokens = tokens%v( 1: tokens%len )

	parser%pos = 1

	parser%diagnostics = lexer%diagnostics

	!print *, 'tokens%len = ', tokens%len
	if (debug > 1) print *, parser%tokens_str()

end function new_parser

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

function syntax_parse(str, variables) result(tree)

	character(len = *) :: str

	type(syntax_node_t) :: tree

	!********

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	type(variable_dictionary_t) :: variables

	if (debug > 0) print *, 'syntax_parse'
	if (debug > 0) print *, 'str = ', str

	parser = new_parser(str)

	! Do nothing for blank lines (or comments)
	if (parser%current_kind() == eof_token) then
		tree%is_empty = .true.
		tree%diagnostics = parser%diagnostics
		return
	end if

	! Point parser member to variables dictionary.  This could be done in the
	! constructor new_parser(), but it seems reasonable to do it here since it
	! has to be moved back later
	if (allocated(variables%root)) then
		!print *, 'moving'
		call move_alloc(variables%root, parser%variables%root)
		!print *, 'done'
	end if

	! Parse the tokens. Should this accept empty strings?  It says unexpected
	! token trying to match number in parse_primary_expr(), so currently the
	! interpreter driver skips empty lines
	tree = parser%parse_statement()

	if (debug > 1) print *, 'tree = ', tree%str()

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

	tree%diagnostics = parser%diagnostics

	! Move back.  It's possible that it was empty before this call but not
	! anymore

	!print *, 'checking alloc'
	if (allocated(parser%variables%root)) then
		!print *, 'moving back'
		call move_alloc(parser%variables%root, variables%root)
		!print *, 'done'
	end if

	if (debug > 0) print *, 'done syntax_parse'

end function syntax_parse

!===============================================================================

function parse_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	select case (parser%current_kind())! == let_keyword      .and. &

		case (lbrace_token)
			!print *, 'calling parse_block_statement'
			statement = parser%parse_block_statement()
			!print *, 'returned'
			!print *, '==========================='

		case default
			statement = parser%parse_expr_statement()

	end select

end function parse_statement

!===============================================================================

function parse_block_statement(parser) result(block)

	class(parser_t) :: parser

	type(syntax_node_t) :: block

	!********

	type(syntax_node_t)        :: statement
	type(syntax_node_vector_t) :: statements
	type(syntax_token_t) :: left, right

	integer :: i

	statements = new_syntax_node_vector()
	i = 0

	left  = parser%match(lbrace_token)

	do while ( &
		parser%current_kind() /= eof_token .and. &
		parser%current_kind() /= rbrace_token)

		i = i + 1
		!print *, '    statement ', i

		!statement = parser%parse_statement()
		!call statements%push(statement)
		call statements%push(parser%parse_statement())

	end do

	right = parser%match(rbrace_token)

	block%kind = block_statement

	! Convert to standard array.  TODO: make a subroutine for this explicit loop
	! copy, since apparently its required for memory correctness

	!print *, 'dealloc'
	if (allocated(block%statements)) deallocate(block%statements)
	!print *, 'alloc'
	allocate(block%statements( statements%len ))
	!print *, 'copy'
	!block%statements = statements%v( 1: statements%len )
	do i = 1, statements%len
		block%statements(i) = statements%v(i)
	end do
	!print *, 'done'

	!!if (allocated(block%statements)) deallocate(block%statements)
	!call move_alloc(statements%v, block%statements)

end function parse_block_statement

!===============================================================================

recursive function parse_expr_statement(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io

	type(syntax_node_t) :: right
	type(syntax_token_t) :: let, identifier, op
	type(text_span_t) :: span

	! TODO: provide a way to declare variable types without initializing them?
	! Rust discourages this, instead preferring patterns like this:
	!
	!      let x = if condition
	!      {
	!          y
	!      }
	!      else
	!      {
	!          z
	!      };

	if (parser%peek_kind(0) == let_keyword      .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		! TODO: I'm skipping ahead a bit here to what Immo does in episode 6.
		! He uses separate variable_declaration, expr_statement, and
		! assignment_expr kinds, all of which I'm handling here as
		! assignment_expr.  My code may need some refactoring to more closely
		! mirror Immo's.

		!print *, 'let expr'

		let        = parser%next()
		identifier = parser%next()
		op         = parser%next()
		right      = parser%parse_expr_statement()

		expr = new_declaration_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		! Insert the identifier's type into the dictionary
		call parser%variables%insert(identifier%text, expr%val, &
			io, overwrite = .false.)

		!print *, 'io = ', io
		if (io /= exit_success) then
			span = new_text_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(span, identifier%text))
		end if

		return

	end if

	if (parser%peek_kind(0) == identifier_token .and. &
	    parser%peek_kind(1) == equals_token) then

		!print *, 'assign expr'

		identifier = parser%next()
		op         = parser%next()
		right      = parser%parse_expr_statement()

		expr = new_assignment_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		expr%val = parser%variables%search(identifier%text, io)
		if (io /= exit_success) then
			span = new_text_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(span, identifier%text))
		end if

		! TODO: move this check inside of is_binary_op_allowed?  Need to pass
		! parser to it to push diagnostics
		if (.not. is_binary_op_allowed( &
			expr%val%kind, op%kind, expr%right%val%kind)) then

			span = new_text_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(span, op%text, &
				kind_name(expr%val%kind), &
				kind_name(expr%right%val%kind)))

		end if

		return

	end if

	expr = parser%parse_expr()

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

	integer :: parent_precl, prec

	type(syntax_node_t) :: right
	type(syntax_token_t) :: op
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_expr'

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

		if (.not. is_unary_op_allowed(op%kind, right%val%kind)) then

			span = new_text_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_unary_types(span, op%text, &
				kind_name(expr%right%val%kind)))

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

		if (.not. is_binary_op_allowed( &
			expr%left%val%kind, op%kind, expr%right%val%kind)) then

			span = new_text_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(span, op%text, &
				kind_name(expr%left %val%kind), &
				kind_name(expr%right%val%kind)))

		end if

	end do

end function parse_expr

!===============================================================================

logical function is_binary_op_allowed(left, op, right)

	! Is an operation allowed with the types of operator op and left/right
	! operands?

	integer, intent(in) :: left, op, right

	!print *, 'left, right = ', left, right

	!! This dynamic variable typing can be useful for testing
	!is_binary_op_allowed = .true.
	!return

	is_binary_op_allowed = .false.

	select case (op)

		case (plus_token, minus_token, sstar_token, star_token, slash_token)
			! FIXME: floats
			is_binary_op_allowed = left == num_expr  .and. right == num_expr

		case (and_keyword, or_keyword)
			is_binary_op_allowed = left == bool_expr .and. right == bool_expr

		case (equals_token, eequals_token, bang_equals_token)
			is_binary_op_allowed = left == right

	end select

end function is_binary_op_allowed

!===============================================================================

logical function is_unary_op_allowed(op, operand)

	! Is a unary operation allowed with kinds operator op and operand?

	integer, intent(in) :: op, operand

	is_unary_op_allowed = .false.

	select case (op)

		case (plus_token, minus_token)
			! FIXME: floats
			is_unary_op_allowed = operand == num_expr

		case (not_keyword)
			is_unary_op_allowed = operand == bool_expr

	end select

end function is_unary_op_allowed

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

integer function get_unary_op_prec(kind) result(prec)

	! Get unary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (plus_token, minus_token, not_keyword)
			prec = 7

		case default
			prec = 0

	end select

end function get_unary_op_prec

!===============================================================================

integer function get_binary_op_prec(kind) result(prec)

	! Get binary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		! FIXME: increment the unary operator precedence above after increasing
		! the max binary precedence

		case (sstar_token)
			prec = 6

		case (star_token, slash_token)
			prec = 5

		case (plus_token, minus_token)
			prec = 4

		case (eequals_token, bang_equals_token)
			prec = 3

		case (and_keyword)
			prec = 2

		case (or_keyword)
			prec = 1

		case default
			prec = 0

	end select

end function get_binary_op_prec

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io
	logical :: bool
	type(syntax_token_t) :: num, left, right, keyword, identifier
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

			return

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			expr = new_bool_expr(bool)

			!print *, 'expr%val%bval = ', expr%val%bval

		case (identifier_token)

			identifier = parser%next()
			!print *, 'identifier = ', identifier%text

			!print *, 'searching'
			expr = new_name_expr(identifier, &
				parser%variables%search(identifier%text, io))

			if (io /= exit_success) then
				span = new_text_span(identifier%pos, len(identifier%text))
				call parser%diagnostics%push( &
					err_undeclare_var(span, identifier%text))
			end if

		case default

			num = parser%match(num_token)
			expr = new_num_expr(num)

			if (debug > 1) print *, 'num = ', expr%val%str()

	end select

end function parse_primary_expr

!===============================================================================

function new_name_expr(identifier, val) result(expr)

	type(syntax_token_t), intent(in) :: identifier
	type(syntax_node_t) :: expr
	type(value_t) :: val

	expr%kind = name_expr
	expr%identifier = identifier
	expr%val = val

end function new_name_expr

!===============================================================================

function new_bool_expr(bool) result(expr)

	logical, intent(in) :: bool

	type(syntax_node_t) :: expr

	!print *, 'new_bool_expr'
	!print *, 'bool = ', bool

	! The expression node is a generic literal expression, while its child val
	! member indicates the specific type (e.g. bool_expr or num_expr)
	expr%kind = literal_expr

	expr%val = new_value(bool_expr, bval = bool)

	!print *, 'expr%val%bval = ', expr%val%bval

end function new_bool_expr

! TODO: combine new_bool_expr() and new_num_expr() into a general
! new_literal_exp() fn

function new_num_expr(num) result(expr)

	type(syntax_token_t), intent(in) :: num

	type(syntax_node_t) :: expr

	!********

	type(value_t) :: val

	val = num%val
	!val%kind = num_expr
	!val%ival = num

	expr%kind = literal_expr
	expr%val  = val

end function new_num_expr

!===============================================================================

function new_declaration_expr(identifier, op, right) result(expr)

	type(syntax_token_t), intent(in) :: identifier, op
	type(syntax_node_t) , intent(in) :: right

	type(syntax_node_t) :: expr

	!********

	expr%kind = assignment_expr

	allocate(expr%right)

	expr%identifier = identifier

	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent
	expr%val%kind = right%val%kind

end function new_declaration_expr

!===============================================================================

function new_assignment_expr(identifier, op, right) result(expr)

	type(syntax_token_t), intent(in) :: identifier, op
	type(syntax_node_t) , intent(in) :: right

	type(syntax_node_t) :: expr

	!********

	expr%kind = assignment_expr

	allocate(expr%right)

	expr%identifier = identifier

	expr%op    = op
	expr%right = right

	! The identifier has already been declared, so do not overwrite its type
	! here

end function new_assignment_expr

!===============================================================================

function new_binary_expr(left, op, right) result(expr)

	type(syntax_node_t) , intent(in) :: left, right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left  = ', left %str()
	if (debug > 1) print *, 'right = ', right%str()

	expr%kind = binary_expr

	allocate(expr%left)
	allocate(expr%right)

	expr%left  = left
	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent
	expr%val%kind = get_binary_op_kind(left%val%kind, op%kind, right%val%kind)
	!expr%val%kind = left%val%kind

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_binary_expr'

end function new_binary_expr

!===============================================================================

integer function get_binary_op_kind(left, op, right)

	! Is an operation allowed with kinds operator op and operand?

	integer, intent(in) :: left, op, right

	! Comparison operations can take 2 numbers, but always return a bool
	if (op == eequals_token .or. op == bang_equals_token) then
		!print *, 'bool_expr'
		get_binary_op_kind = bool_expr
		return
	end if
	!print *, 'default'

	! Other operations return the same type as their operands
	!
	! FIXME: floats, int casting.  1 + 0.5 and 0.5 + 1 should both cast to
	! float, not int.  That's why I'm passing right as an arg (but not using it
	! yet)

	get_binary_op_kind = left

end function get_binary_op_kind

!===============================================================================

function new_unary_expr(op, right) result(expr)

	type(syntax_node_t) , intent(in) :: right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_unary_expr'

	expr%kind = unary_expr

	allocate(expr%right)

	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent.  IIRC
	! all unary operators result in the same type as their operand, hence there
	! is a get_binary_op_kind() fn but no get_unary_op_kind() fn
	expr%val%kind = right%val%kind

	if (debug > 1) print *, 'new_unary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_unary_expr'

end function new_unary_expr

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
		return
	end if

	len_text = max(len(current%text), 1)
	span = new_text_span(current%pos, len_text)
	call parser%diagnostics%push( &
		err_unexpected_token(span, current%text, &
		kind_name(parser%current_kind()), kind_name(kind)))

	token = new_token(kind, current%pos, null_char)

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

recursive function syntax_eval(node, variables) result(res)

	type(syntax_node_t) :: node

	! I don't want to make this arg optional, because then it would require
	! copying a potentially large struct to a local var without fancy use of
	! move_alloc()
	type(variable_dictionary_t) :: variables

	type(value_t) :: res

	!********

	integer :: i
	type(value_t) :: left, right

	if (node%is_empty) then
		!print *, 'returning'
		return
	end if

	!********

	! I'm being a bit loose with consistency on select case indentation but
	! I don't want a gigantic diff

	select case (node%kind)
	case (literal_expr)
		! This handles both ints, bools, etc.
		res = node%val

	case (block_statement)

		! The final statement of a block returns the actual result.  Non-final
		! statements only change the state.
		do i = 1, size(node%statements)
			res = syntax_eval(node%statements(i), variables)
			!print *, i, ' res = ', res%str()
		end do

	case (assignment_expr)

		! Assign return value
		res = syntax_eval(node%right, variables)

		! Assign res to LHS identifier variable as well.  This inserts the
		! value, while the insert call in the parser inserts the type

		!print *, 'assigning identifier "', node%identifier%text, '"'
		call variables%insert(node%identifier%text, res)

	case (name_expr)
		!print *, 'searching identifier ', node%identifier%text
		res = variables%search(node%identifier%text)

	case (unary_expr)

		right = syntax_eval(node%right, variables)
		!print *, 'right = ', right

		res%kind = right%kind

		! TODO: add fallback type checking here?

		select case (node%op%kind)
		case (plus_token)
			res      =  right

		case (minus_token)
			res%ival = -right%ival

		case (not_keyword)
			res%bval = .not. right%bval

		case default

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected unary operator "' &
					//node%op%text//'"'//color_reset

			res%kind = 0

		end select

	case (binary_expr)

		left  = syntax_eval(node%left , variables)
		right = syntax_eval(node%right, variables)

		!print *, 'left  = ', left
		!print *, 'right = ', right

		! The parser should catch this, but do it here as a fallback (e.g. I'll
		! add floats later and forget this needs fixing)
		if (left%kind /= right%kind) then
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
				//fg_bold//': unexpected types for binary operator "' &
				//node%op%text//'"'//color_reset
		end if

		res%kind = get_binary_op_kind(left%kind, node%op%kind, right%kind)

		select case (node%op%kind)
		case (plus_token)
			res%ival = left%ival + right%ival

			! FIXME: floats

		case (minus_token)
			res%ival = left%ival - right%ival

		case (star_token)
			res%ival = left%ival * right%ival

		case (sstar_token)
			res%ival = left%ival ** right%ival

		case (slash_token)
			res%ival = left%ival / right%ival

		case (and_keyword)
			res%bval = left%bval .and. right%bval

		case (or_keyword)
			res%bval = left%bval .or.  right%bval

		case (eequals_token)

			if (left%kind == bool_expr) then
				res%bval = left%bval .eqv. right%bval
			else if (left%kind == num_expr) then
				res%bval = left%ival  ==   right%ival
			else
				write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected types for comparison "' &
					//node%op%text//'"'//color_reset
			end if

		case (bang_equals_token)

			if (left%kind == bool_expr) then
				res%bval = left%bval .neqv. right%bval
			else if (left%kind == num_expr) then
				res%bval = left%ival  /=   right%ival
			else
				! TODO: refactor w/ above case.  Add a routine to exit(-1) for
				! internal syntran error
				write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected types for comparison "' &
					//node%op%text//'"'//color_reset
			end if

		case default

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected binary operator "' &
					//node%op%text//'"'//color_reset

			!! This is catastrophic, but it kills the unit tests.  TODO:
			!internal syntran error
			!stop

			res%kind = 0

		end select

	case default

		res%kind = 0
		write(*,*) fg_bold_bright_red//'Error'//color_reset &
				//fg_bold//': unexpected node "'//kind_name(node%kind) &
				//'"'//color_reset

		!stop

	end select

end function syntax_eval

!===============================================================================

subroutine log_diagnostics(node, line, ou)

	! TODO: line numbers

	class(syntax_node_t), intent(in) :: node
	character(len = *)  , intent(in) :: line
	integer, optional   , intent(in) :: ou

	!********

	integer :: i, oul

	oul = output_unit
	if (present(ou)) oul = ou

	do i = 1, node%diagnostics%len
		! Check rustc conventions for style of line numbers in diagnostics
		write(oul,*)
		write(oul, '(a)') line
		write(oul, '(a)') node%diagnostics%v(i)%s
		write(oul,*)
	end do

end subroutine log_diagnostics

!===============================================================================

function value_str(val) result(str)

	class(value_t) :: val

	character(len = :), allocatable :: str

	!********

	character(len = 32) :: buffer

	select case (val%kind)

		case (num_expr)
			write(buffer, '(i0)') val%ival
			str = trim(buffer)

		case (bool_expr)
			! It might be helpful to have util fns for primitive str conversion
			if (val%bval) then
				str = "true"
			else
				str = "false"
			end if

		case default
			str = err_prefix//"<invalid_value>"//color_reset

	end select

end function

!===============================================================================

end module core_m

!===============================================================================

