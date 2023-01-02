
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

	! Token and syntax node kinds enum
	integer, parameter :: &
			num_expr         = 10, &
			binary_expr      =  9, &
			star_token       =  8, &
			slash_token      =  7, &
			bad_token        =  6, &
			plus_token       =  5, &
			minus_token      =  4, &
			whitespace_token =  3, &
			num_token        =  2, &
			eof_token        =  1

	!********

	type syntax_token_t

		! TODO: how to handle values of different types?
		integer :: val

		integer :: kind, pos
		character(len = :), allocatable :: text

	end type syntax_token_t

	!********

	type syntax_node_t
		integer :: kind
		type(syntax_node_t), pointer :: left => null(), right => null()
		type(syntax_token_t) :: op, num
		contains
			procedure :: str => syntax_node_str
	end type syntax_node_t

	!********

	!! Do we need a separate type for the tree, or can a node represent the whole
	!! tree?
	!type syntax_tree_t
	!	!integer :: dummy
	!end type syntax_tree_t

	!********

	type lexer_t

		character(len = :), allocatable :: text
		integer :: pos

		contains
			procedure next_token, current

	end type lexer_t

	!********

	type parser_t

		type(syntax_token_t), allocatable :: tokens(:)

		!character(len = :), allocatable :: text
		integer :: pos

		! TODO: consider renaming next_token/current vs current_token/next
		! (members of different types)
		contains
			procedure :: parse_term, match, current => current_token, next, &
				parse_factor, parse_primary_expr

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
			"eof_token       ", & !  1
			"num_token       ", & !  2
			"whitespace_token", & !  3
			"minus_token     ", & !  4
			"plus_token      ", & !  5
			"bad_token       ", & !  6
			"slash_token     ", & !  7
			"star_token      ", & !  8
			"binary_expr     ", & !  9
			"num_expr        "  & ! 10
		]

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end function

!===============================================================================

function new_token(kind, pos, text, val) result(token)

	integer :: val
	integer :: kind, pos

	character(len = *) :: text

	type(syntax_token_t) :: token

	token%kind = kind
	token%pos  = pos
	token%text = text
	token%val  = val

end function new_token

!===============================================================================

character function current(lexer)

	! Current character

	class(lexer_t) :: lexer

	if (lexer%pos > len(lexer%text)) then
		current = null_char
		return
	end if

	current = lexer%text( lexer%pos: lexer%pos )

end function current

!===============================================================================

function next_token(lexer) result(token)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: token

	!********

	integer :: val
	integer :: start, io

	character(len = :), allocatable :: text

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char, 0)
		return
	end if

	if (is_digit(lexer%current())) then

		start = lexer%pos

		do while (is_digit(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		read(text, *, iostat = io) val
		if (io /= 0) then
			! TODO: diagnostics
			print *, 'diag: invalid int32'
		end if

		token = new_token(num_token, start, text, val)
		return

	end if

	if (is_whitespace(lexer%current())) then

		start = lexer%pos

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text, 0)
		return

	end if

	select case (lexer%current())
		case ("+")
			token = new_token(plus_token , lexer%pos, lexer%current(), 0)
		case ("-")
			token = new_token(minus_token, lexer%pos, lexer%current(), 0)
		case ("*")
			token = new_token(star_token , lexer%pos, lexer%current(), 0)
		case ("/")
			token = new_token(slash_token, lexer%pos, lexer%current(), 0)
		case default
			! TODO: diagnostics
			token = new_token(bad_token, lexer%pos, lexer%current(), 0)
			print *, 'diag: bad token ', lexer%current()
	end select

	lexer%pos = lexer%pos + 1

end function next_token

!===============================================================================

function new_lexer(text) result(lexer)

	character(len = *) :: text

	type(lexer_t) :: lexer

	lexer%text = text
	lexer%pos = 1

end function new_lexer

!===============================================================================

function new_parser(str) result(parser)

	character(len = *) :: str

	type(parser_t) :: parser

	!********

	integer :: i

	type(syntax_token_t)        :: token
	type(syntax_token_vector_t) :: tokens

	type(lexer_t) :: lexer

	! Get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str)
	do
		token = lexer%next_token()

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then

			!print *, 'token = <', token%text, '> ', &
			!		'<'//kind_name(token%kind)//'>'

			call tokens%push(token)

		end if

		if (token%kind == eof_token) exit
	end do

	parser%tokens = tokens%v( 1: tokens%len )
	parser%pos = 1

	if (debug > 1) then
		! Make tokens to str fn?
		do i = 1, size(parser%tokens)
			print *, 'token = <',  parser%tokens(i)%text , '> ', &
			     '<'//kind_name( parser%tokens(i)%kind )//'>'
		end do
	end if

end function new_parser

!===============================================================================

function syntax_tree_parse(str) result(tree)

	character(len = *) :: str

	!type(syntax_tree_t) :: tree
	type(syntax_node_t) :: tree

	!********

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	if (debug > 1) print *, 'syntax_tree_parse'

	parser = new_parser(str)

	! Parse the tokens. TODO: handle totally empty lines? Here or higher level?
	tree = parser%parse_term()

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

end function syntax_tree_parse

!===============================================================================

function parse_term(parser) result(term)

	class(parser_t) :: parser

	type(syntax_node_t) :: term

	!********

	type(syntax_node_t) :: left, right, tmp
	type(syntax_token_t) :: current, op

	if (debug > 1) print *, 'parse_term'

	left = parser%parse_factor()

	current = parser%current()
	do while (current%kind == plus_token .or. &
	          current%kind == minus_token)

		op = parser%next()
		if (debug > 1) print *, 'op = ', op%text

		right = parser%parse_factor()
		tmp = new_binary_expr(left, op, right)
		left = tmp

		current = parser%current()
	end do

	term = left

	if (debug > 1) print *, 'done parse_term'

end function parse_term

!===============================================================================

function syntax_node_str(node) result(str)

	class(syntax_node_t) :: node

	character(len = :), allocatable :: str

	str =    line_feed// &
		'{'//line_feed// &
			tab//'kind = '//kind_name(node%kind)//line_feed// &
			!tab//'left = '//node%left//line_feed// &
			tab//'op   = '//node%op%text//line_feed// &
			tab//'num  = '//node%num%text//line_feed// &
		'}'//line_feed

end function syntax_node_str

!===============================================================================

recursive function parse_factor(parser) result(factor)

	class(parser_t) :: parser

	type(syntax_node_t) :: factor

	!********

	type(syntax_node_t) :: left, right, tmp
	type(syntax_token_t) :: current, op

	if (debug > 1) print *, 'parse_factor'

	left = parser%parse_primary_expr()

	current = parser%current()
	do while (current%kind == star_token .or. &
	          current%kind == slash_token)

		op = parser%next()
		right = parser%parse_primary_expr()
		tmp = new_binary_expr(left, op, right)
		left = tmp

		current = parser%current()
	end do

	factor = left

	if (debug > 1) print *, 'done parse_factor'

end function parse_factor

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	type(syntax_token_t) :: num

	if (debug > 1) print *, 'parse_primary_expr'

	! TODO: parens

	num = parser%match(num_token)
	expr = new_num_expr(num)

	if (debug > 1) print *, 'num = ', expr%num%val

end function parse_primary_expr

!===============================================================================

function new_num_expr(num) result(expr)

	type(syntax_token_t) :: num

	type(syntax_node_t) :: expr

	!********

	expr%kind = num_expr
	expr%num  = num

end function new_num_expr

!===============================================================================

function new_binary_expr(left, op, right) result(expr)

	type(syntax_node_t), target :: left, right
	type(syntax_token_t) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left = ', left%str()

	expr%kind = binary_expr

	! Note pointer targets (=>) vs regular vars (=)
	expr%left  => left
	expr%op    =  op
	expr%right => right

	if (debug > 1) print *, 'done new_binary_expr'

end function new_binary_expr

!===============================================================================

function match(parser, kind) result(token)

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t) :: token

	!********

	type(syntax_token_t) :: current

	current = parser%current()
	if (current%kind == kind) then
	!if (parser%current()%kind == kind) then
		token = parser%next()
		return
	end if

	! TODO: diags
	print *, 'Error: unexpected token'
	token = new_token(kind, current%pos, null_char, 0)

end function match

!===============================================================================

function current_token(parser)

	! Refactor in terms of peek(0) ?
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token

	if (debug > 1) print *, 'current_token pos ', parser%pos

	if (parser%pos > size(parser%tokens)) then
		current_token = parser%tokens( size(parser%tokens) )
		return
	end if

	current_token = parser%tokens( parser%pos )

end function current_token

!===============================================================================

function next(parser)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next

!===============================================================================

subroutine interpret()

	! This is the interpret shell
	!
	! TODO: arg for iu as stdin vs another file

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = lang_name//'$ '

	!type(syntax_tree_t) :: tree
	type(syntax_node_t) :: tree

	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	! Read-print-loop (eval TBD)
	do
		write(ou, '(a)', advance = 'no') prompt
		input = read_line(iu, iostat = io)

		!print *, 'input = <', input, '>'
		!print *, 'io = ', io

		!if (len(input) > 0) write(ou, '(a)') input
		write(ou, '(a)') input

		tree = syntax_tree_parse(input)

		if (io == iostat_end) exit

	end do

end subroutine interpret

!===============================================================================

end module core_m

!===============================================================================

program main

	use core_m

	write(*,*)
	write(*,*) lang_name//' v0.0.1'
	write(*,*)

	call interpret()

end program main

!===============================================================================

