
!===============================================================================

module core_m

	use iso_fortran_env
	use utils

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	integer, parameter :: debug = 0

	! TODO:
	!
	! Add:
	!  - **
	!  - compound assignment: +=, -=, *=, etc.
	!    * Does any language have "**="? This will
	!  - ++, --
	!  - <, >, <=, >=
	!  - floats, characters, strings
	!  - % (mod/modulo (which? Fortran handles negatives differently in one))
	!  - xor
	!  - bitwise operators

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
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

	type ternary_tree_node_t
		character :: split_char = ''
		type(ternary_tree_node_t), allocatable :: left, mid, right

		!integer, allocatable :: val
		type(value_t), allocatable :: val

		!contains
		!	procedure :: print => ternary_node_print
	end type ternary_tree_node_t

	type variable_dictionary_t
		type(ternary_tree_node_t), allocatable :: root
		contains
			! TODO
			procedure :: &
				insert => variable_insert, &
				search => variable_search
	end type variable_dictionary_t

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
		type(syntax_node_t), allocatable :: left, right
		type(syntax_token_t) :: op, identifier
		type(value_t) :: val

		type(string_vector_t) :: diagnostics

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
				lookahead => lookahead_char

	end type lexer_t

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
				parse_expr, parse_primary_expr, parse_assignment_expr

	end type parser_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

!===============================================================================

contains

!===============================================================================

function variable_search(dictionary, key) result(val)

	class(variable_dictionary_t), intent(in) :: dictionary
	character(len = *), intent(in) :: key
	type(value_t) :: val

	val = ternary_search(dictionary%root, key)

end function variable_search

!===============================================================================

recursive function ternary_search(node, key) result(val)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key
	type(value_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key "', key, '"'

	! TODO: len check should be unnecessary
	if (len(key) == 0 .or. .not. allocated(node)) then
		! Search key not found

		! TODO: return status code
		print *, "Error: variable doesn't exist"
		call exit(-1)

	end if

	! :)
	k  = key(1:1)
	ey = key(2:)

	if (k < node%split_char) then
		!print *, 'left'
		val = ternary_search(node%left , key)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		val = ternary_search(node%right, key)
		return
	else
		!print *, 'mid'

		if (len(ey) == 0) then
			val = node%val
			return
		end if

		val = ternary_search(node%mid  , ey)

		return
	end if

	!print *, 'done ternary_search'
	!print *, ''

end function ternary_search

!===============================================================================

subroutine variable_insert(dictionary, key, val, iostat)

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

	!********

	integer :: io

	!print *, 'inserting "', key, '"'
	call ternary_insert(dictionary%root, key, val, io)

	if (present(iostat)) iostat = io

end subroutine variable_insert

!===============================================================================

recursive subroutine ternary_insert(node, key, val, iostat)

	! TODO: can this be type bound? maybe make a wrapper dict type that
	! basically just contains a ternary_tree_node_t

	!class(ternary_tree_t), intent(inout) :: tree
	type(ternary_tree_node_t), intent(inout), allocatable :: node

	character(len = *), intent(in) :: key

	!integer, intent(in) :: val
	type(value_t), intent(in) :: val

	integer, intent(out) :: iostat

	!********

	character :: k
	character(len = :), allocatable :: ey

	iostat = 0

	if (len(key) == 0) then
		! TODO: refactor mid recursion so that this is unreachable
		return
		call exit(-1)
	end if

	!print *, 'inserting key "', key, '"'

	! :)
	k  = key(1:1)
	ey = key(2:)

	if (.not. allocated(node)) then
		!print *, 'allocate'

		allocate(node)
		node%split_char = k

		! Store val.  Node was not allocated, so no need to check val like below
		if (len(ey) == 0) node%val = val

		! Insert remainder of key
		call ternary_insert(node%mid, ey, val, iostat)
		return

	end if

	if (k < node%split_char) then
		!print *, 'left'
		call ternary_insert(node%left , key, val, iostat)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call ternary_insert(node%right, key, val, iostat)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call ternary_insert(node%mid  , ey, val, iostat)
		return
	end if

	! node%val doesn't really need to be declared as allocatable (it's
	! a scalar anyway), but it's just a convenient way to check if
	! a duplicate key has already been inserted or not.  We could add
	! a separate logical member to node for this instead if needed

	!! TODO: add an input arg to overwrite based on the presence of the
	!! "let" keyword.  Allow this:
	!!
	!!     let a = 1
	!!     a = 2
	!!
	!! But not this:
	!!
	!!     let a = 1
	!!     let a = 2
	!!

	! This is not necessarily a failure.  In the evaluator, we will insert
	! values for variables which have already been declared
	if (allocated(node%val)) then
		!write(*,*) 'Error: key already inserted'
		!call exit(-1)
		iostat = -1
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
			"let_keyword      "  & ! 27
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
		type, identifier

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind  = '//kind_name(node%kind)//line_feed

	left  = ''
	op    = ''
	right = ''
	val   = ''

	identifier = ''

	type  = indentl//'    type  = '//kind_name(node%val%kind)//line_feed

	if      (node%kind == binary_expr) then

		left  = indentl//'    left  = '//node%left %str(indentl//'    ') &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

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

	if (debug > 3) print *, 'starting syntax_node_copy()'

	dst%kind = src%kind
	dst%op   = src%op
	dst%val  = src%val

	dst%identifier = src%identifier

	dst%diagnostics = src%diagnostics

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

	type(value_t) :: val

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char)
		return
	end if

	if (is_digit(lexer%current())) then

		start = lexer%pos

		do while (is_digit(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		read(text, *, iostat = io) ival
		if (io /= 0) then
			! TODO: Refactor w/ underline fn, centralized style
			call lexer%diagnostics%push( &
					repeat(' ', start-1)//fg_bright_red &
					//repeat('^', len(text))//color_reset//line_feed &
					//fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': invalid int32 '//text &
					//color_reset &
					)
		end if

		val = new_value(num_expr, ival = ival)
		token = new_token(num_token, start, text, val)
		return

	end if

	if (is_whitespace(lexer%current())) then

		start = lexer%pos

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text)
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		start = lexer%pos

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
			token = new_token(star_token  , lexer%pos, lexer%current())

		case ("/")
			token = new_token(slash_token , lexer%pos, lexer%current())

		case ("(")
			token = new_token(lparen_token, lexer%pos, lexer%current())

		case (")")
			token = new_token(rparen_token, lexer%pos, lexer%current())

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

				call lexer%diagnostics%push( &
					repeat(' ', lexer%pos-1)//fg_bright_red &
					//repeat('^', 1)//color_reset//line_feed &
					//fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//": unexpected character '"//lexer%current() &
					//"'" &
					//color_reset &
					)

			end if

		case default

			token = new_token(bad_token, lexer%pos, lexer%current())

			call lexer%diagnostics%push( &
					repeat(' ', lexer%pos-1)//fg_bright_red &
					//repeat('^', 1)//color_reset//line_feed &
					//fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//": unexpected character '"//lexer%current() &
					//"'" &
					//color_reset &
					)

	end select
	lexer%pos = lexer%pos + 1

	! TODO: arrow keys create bad tokens in bash on Windows.  Fix that (better
	! yet, override up arrow to do what it does in bash.  c.f. rubik-js)

end function lex

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
	tree = parser%parse_assignment_expr()

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

recursive function parse_assignment_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io

	type(syntax_node_t) :: right
	type(syntax_token_t) :: let, identifier, op

	if (parser%peek_kind(0) == let_keyword      .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		! TODO: I'm skipping ahead a bit here to what Immo does in episode 6.
		! He uses separate variable_declaration, expr_statement, and
		! assignment_expr kinds, all of which I'm handling here as
		! assignment_expr.  My code my need some refactoring to more closely
		! mirror Immo's.

		!print *, 'let expr'

		let        = parser%next()
		identifier = parser%next()
		op         = parser%next()
		right      = parser%parse_assignment_expr()

		expr = new_assignment_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		! Insert the identifier's type into the dictionary
		call parser%variables%insert(identifier%text, expr%val, io)
		!call variables%insert(node%identifier%text, res)

		!print *, 'io = ', io
		if (io /= 0) then
			call parser%diagnostics%push('Error: variable "' &
				//identifier%text//'" has already been declared')
		end if

		return

	end if

	if (parser%peek_kind(0) == identifier_token .and. &
	    parser%peek_kind(1) == equals_token) then

		!print *, 'assign expr'

		identifier = parser%next()
		op         = parser%next()
		right      = parser%parse_assignment_expr()

		! TODO: pass on optional logical "let" arg to new_assignment_expr() so
		! it doesn't overwrite identifier's type, which should have already been
		! declared by an earlier let statement.  Also make a search call to
		! confirm it's been declared

		expr = new_assignment_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		return

	end if

	expr = parser%parse_expr()

end function parse_assignment_expr

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

	if (debug > 1) print *, 'parse_expr'

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

		if (.not. is_unary_op_allowed(op%kind, right%val%kind)) then
			! TODO: wording, styling
			call parser%diagnostics%push('Error: unary op not defined for type')
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

		!if (expr%left%val%kind /= expr%right%val%kind) then
		if (.not. is_binary_op_allowed( &
			expr%left%val%kind, op%kind, expr%right%val%kind)) then
			! TODO: wording, styling
			call parser%diagnostics%push('Error: bin op not defined for types')
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

	!! TODO: dynamic variable typing, probably testing only
	!if (left == 0 .or. right == 0) then
	!	is_binary_op_allowed = .true.
	!	return
	!end if

	select case (op)

		case (plus_token, minus_token, star_token, slash_token)
			! TODO: floats
			is_binary_op_allowed = left == num_expr  .and. right == num_expr

		case (and_keyword, or_keyword)
			is_binary_op_allowed = left == bool_expr .and. right == bool_expr

		case (equals_token, eequals_token, bang_equals_token)

			! TODO: I don't think this will need updated for equals_token, but
			! I'm commenting just in case.  Further work in
			! new_assignment_expr() should take care of it

			is_binary_op_allowed = left == right

			!is_binary_op_allowed = &
			!	(left == bool_expr .and. right == bool_expr) .or. &
			!	(left == num_expr  .and. right == num_expr)

	end select

end function is_binary_op_allowed

!===============================================================================

logical function is_unary_op_allowed(op, operand)

	! Is a unary operation allowed with kinds operator op and operand?

	integer, intent(in) :: op, operand

	is_unary_op_allowed = .false.

	select case (op)

		case (plus_token, minus_token)
			! TODO: floats
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

	!type(syntax_token_t) :: current
	!current = parser%current()
	!current_kind = current%kind

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
			prec = 6

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

	logical :: bool

	type(syntax_token_t) :: num, left, right, keyword, identifier

	if (debug > 1) print *, 'parse_primary_expr'

	select case (parser%current_kind())

		case (lparen_token)

			left  = parser%next()

			! These two lines are the difference between allowing statement
			! "a = (b = 1)" or not.  Note that "a = b = 1" is allowed either way

			!expr  = parser%parse_expr()
			expr  = parser%parse_assignment_expr()

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

			!expr = new_name_expr(identifier)
			expr = new_name_expr(identifier, &
				parser%variables%search(identifier%text))

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

	! TODO: search variables to get val? in caller? or just wait til eval?
	!
	! Statements like these all work:
	!
	!   a = b = 1
	!   c = (d = 1)
	!
	! Include those in tests?

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

function new_assignment_expr(identifier, op, right) result(expr)

	type(syntax_token_t), intent(in) :: identifier, op
	type(syntax_node_t) , intent(in) :: right

	type(syntax_node_t) :: expr

	!********

	!character(len = :), allocatable :: text

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'identifier  = ', identifier%text
	if (debug > 1) print *, 'right = ', right%str()

	expr%kind = assignment_expr

	!allocate(expr%left)
	allocate(expr%right)

	!expr%left  = identifier
	expr%identifier = identifier

	expr%identifier%text = identifier%text(:)

	!! Yet again burned debugging stuff here due to node updating
	!! syntax_node_copy()

	!text = identifier%text
	!!expr%identifier%text = text
	!allocate(expr%identifier
	!expr%identifier%text = text

	!print *, 'expr ident text = ', expr%identifier%text

	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent

	! TODO: if identifier has already been declared, do not overwrite its type
	! here

	expr%val%kind = right%val%kind

	!expr%val%kind = get_binary_op_kind(left%val%kind, op%kind, right%val%kind)
	!!expr%val%kind = left%val%kind

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_binary_expr'

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
	! TODO: floats, int casting.  1 + 0.5 and 0.5 + 1 should both cast to float,
	! not int.  That's why I'm passing right as an arg (but not using it yet)

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

	! If current_text() and current_pos() helper fns are added, this local var
	! current can be eliminated
	current = parser%current()

	if (parser%current_kind() == kind) then
		token = parser%next()
		return
	end if

	len_text = max(len(current%text), 1)

	call parser%diagnostics%push( &
			repeat(' ', current%pos-1)//fg_bright_red &
			//repeat('^', len_text)//color_reset//line_feed &
			//fg_bold_bright_red//'Error'//color_reset &
			//fg_bold//': unexpected token "'//current%text//'"' &
			//' (kind <'//kind_name(parser%current_kind())//'>)' &
			//', expected <'//kind_name(kind)//'>' &
			//color_reset &
			)

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

	type(value_t) :: left, right

	if (node%kind == literal_expr) then
		! This handles both ints, bools, etc.
		res = node%val
		return
	end if

	if (node%kind == assignment_expr) then

		! Assign return value
		res = syntax_eval(node%right, variables)

		! Assign res to LHS identifier variable as well.  TODO: this inserts the
		! value, but call insert in the parser as well to declare the type

		!print *, 'assigning identifier "', node%identifier%text, '"'
		call variables%insert(node%identifier%text, res)

		return
	end if

	if (node%kind == name_expr) then
		!print *, 'searching identifier ', node%identifier%text
		res = variables%search(node%identifier%text)
		return
	end if

	if (node%kind == unary_expr) then

		right = syntax_eval(node%right, variables)
		!print *, 'right = ', right

		res%kind = right%kind

		! TODO: add fallback type checking here?

		if      (node%op%kind == plus_token) then
			res      =  right

		else if (node%op%kind == minus_token) then
			res%ival = -right%ival

		else if (node%op%kind == not_keyword) then
			res%bval = .not. right%bval

		else

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected unary operator "' &
					//node%op%text//'"'//color_reset

			res%kind = 0

		end if

		return

	end if

	if (node%kind == binary_expr) then

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

		!res%kind = left%kind
		res%kind = get_binary_op_kind(left%kind, node%op%kind, right%kind)

		if      (node%op%kind == plus_token) then
			res%ival = left%ival + right%ival

			! TODO: floats

		else if (node%op%kind == minus_token) then
			res%ival = left%ival - right%ival

		else if (node%op%kind == star_token) then
			res%ival = left%ival * right%ival

		else if (node%op%kind == slash_token) then
			res%ival = left%ival / right%ival

		else if (node%op%kind == and_keyword) then
			res%bval = left%bval .and. right%bval

		else if (node%op%kind == or_keyword) then
			res%bval = left%bval .or.  right%bval

		else if (node%op%kind == eequals_token) then

			if (left%kind == bool_expr) then
				res%bval = left%bval .eqv. right%bval
			else if (left%kind == num_expr) then
				res%bval = left%ival  ==   right%ival
			else
				write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected types for comparison "' &
					//node%op%text//'"'//color_reset
			end if

		else if (node%op%kind == bang_equals_token) then

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

		else

			! Anything here should have been caught by a parser diagnostic
			write(*,*) fg_bold_bright_red//'Error'//color_reset &
					//fg_bold//': unexpected binary operator "' &
					//node%op%text//'"'//color_reset

			!! This is catastrophic, but it kills the unit tests.  TODO:
			!internal syntran error
			!stop

			res%kind = 0

		end if

		return

	end if

	res%kind = 0
	write(*,*) fg_bold_bright_red//'Error'//color_reset &
			//fg_bold//': unexpected node "'//kind_name(node%kind) &
			//'"'//color_reset

	!stop

end function syntax_eval

!===============================================================================

!subroutine interpret(str)
function interpret(str) result(res_str)

	! This is the interpreter shell
	!
	! Interpret stdin by default, or interpret the multi-line string str if it
	! is given.  The return value res_str is the result of the final expression
	! (like how Rust doesn't have a return statement, but fns just return the
	! final expression in their body)
	!
	! TODO: add quiet arg for bad syntax testing
	!
	! TODO: another optional arg for iu as stdin vs another file:
	!   - enable input echo for file input (not for stdin)
	!   - write file name and line num for diagnostics

	character(len = *), optional :: str
	character(len = :), allocatable :: res_str

	!********

	character(len = :), allocatable :: line
	character(len = *), parameter :: prompt = lang_name//'$ '

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	logical :: show_tree = .false.

	type(string_view_t) :: sv

	type(syntax_node_t) :: tree
	type(value_t) :: res
	type(variable_dictionary_t) :: variables

	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	if (present(str)) then
		! Append a trailing line feed in case it does not exist
		sv = new_string_view(str//line_feed)
	end if

	! Read-eval-print-loop
	do

		if (present(str)) then

			! TODO: I don't have an end-of-statement token yet (;), so interpret
			! multi-line strings one line at a time for now.  Whatever I end up
			! doing has to work with both strings and stdin, so I may need
			! a continue iostat for syntax_parse to continue parsing the same
			! tree through multiple input lines

			line = sv%get_line(iostat = io)

		else
			write(ou, '(a)', advance = 'no') prompt
			line = read_line(iu, iostat = io)
		end if

		!print *, 'line = <', line, '>'
		!print *, 'io = ', io

		!! Echo input?
		!write(ou, '(a)') line

		if (io == iostat_end) exit

		! Skip empty lines
		if (len(line) == 0) cycle

		if (line == '#tree') then
			show_tree = .not. show_tree
			cycle
		end if

		res_str = ''
		tree = syntax_parse(line, variables)

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0 .or. show_tree) print *, 'tree = ', tree%str()

		call tree%log_diagnostics(line, ou)

		! Don't try to evaluate with errors
		if (tree%diagnostics%len > 0) cycle

		res  = syntax_eval(tree, variables)

		! Consider MATLAB-style "ans = " log?
		res_str = res%str()
		if (.not. present(str)) write(ou, '(a)') res_str

	end do

end function interpret

!===============================================================================

subroutine log_diagnostics(node, line, ou)

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
			! TODO: log error
			str = "<invalid_value>"

	end select

end function

!===============================================================================

integer function eval_int(str)

	character(len = *), intent(in) :: str

	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(variable_dictionary_t) :: variables

	tree = syntax_parse(str, variables)
	call tree%log_diagnostics(str)

	if (tree%diagnostics%len > 0) then
		eval_int = 0
		return
	end if

	val = syntax_eval(tree, variables)
	eval_int = val%ival

end function eval_int

!===============================================================================

function eval(str, quiet) result(res)

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet

	!********

	logical :: quietl

	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(variable_dictionary_t) :: variables

	quietl = .false.
	if (present(quiet)) quietl = quiet

	!! One-liner, but no error handling.  This can crash unit tests without
	!! reporting failures
	!eval = syntax_eval(syntax_parse(str, variables), variables)

	! TODO: make a helper fn here that all the eval_* fns use

	tree = syntax_parse(str, variables)
	if (.not. quietl) call tree%log_diagnostics(str)

	if (tree%diagnostics%len > 0) then
		res = ''
		return
	end if

	val = syntax_eval(tree, variables)
	res = val%str()

end function eval

!===============================================================================

end module core_m

!===============================================================================

