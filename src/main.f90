
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
	integer, parameter ::          &
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

		! TODO: initiazing bad_token here shouldn't be required
		integer :: kind = bad_token, pos
		character(len = :), allocatable :: text

	end type syntax_token_t

	!********

	type syntax_node_t

		integer :: kind = bad_token
		type(syntax_node_t), allocatable :: left, right
		type(syntax_token_t) :: op, num

		contains
			procedure :: str => syntax_node_str

			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy

	end type syntax_node_t

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

recursive function syntax_node_str(node, indent) result(str)

	! Convert tree to string in JSON-ish format

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str

	!********

	character(len = :), allocatable :: indentl, kind, num, left, op, right

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind = '//kind_name(node%kind)//line_feed

	num = ''

	left = ''
	right = ''
	op = ''

	if      (node%kind == binary_expr) then
		left  = indentl//'    left  = '//node%left %str(indentl//'    ')//line_feed
		right = indentl//'    right = '//node%right%str(indentl//'    ')//line_feed
		op    = indentl//'    op    = '//node%op%text//line_feed
	else if (node%kind == num_expr) then
		num   = indentl//'    num   = '//node%num%text   //line_feed
	end if

	str =    line_feed// &
		indentl//'{'//line_feed// &
			kind // &
			left // &
			op   // &
			right// &
			num  // &
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
	dst%num  = src%num

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
			write(*,*) 'diag: invalid int32'
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
			write(*,*) 'diag: bad token ', lexer%current()
	end select

	! TODO: arrow keys create bad tokens.  Fix that (better yet, override up
	! arrow to do what it does in bash.  c.f. rubik-js)

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
		! TODO: make tokens to str fn?
		do i = 1, size(parser%tokens)
			print *, 'token = <',  parser%tokens(i)%text , '> ', &
			     '<'//kind_name( parser%tokens(i)%kind )//'>'
		end do
	end if

end function new_parser

!===============================================================================

function syntax_parse(str) result(tree)

	character(len = *) :: str

	type(syntax_node_t) :: tree

	!********

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	if (debug > 1) print *, 'syntax_parse'

	parser = new_parser(str)

	! Parse the tokens. Should this accept empty strings?  It says unexpected
	! token trying to match number in parse_primary_expr(), so currently the
	! interpreter driver skips empty lines
	!print *, 'parsing'
	tree = parser%parse_term()

	if (debug > 1) print *, 'tree = ', tree%str()

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

end function syntax_parse

!===============================================================================

function parse_term(parser) result(left)

	class(parser_t) :: parser

	type(syntax_node_t) :: left

	!********

	type(syntax_node_t) :: right
	type(syntax_token_t) :: current, op

	if (debug > 1) print *, 'parse_term'

	left = parser%parse_factor()

	!if (debug > 1) print *, 'left term = ', left %str()

	current = parser%current()
	do while (current%kind == plus_token .or. &
	          current%kind == minus_token)

		op = parser%next()
		if (debug > 1) print *, 'op = ', op%text

		right = parser%parse_factor()
		left  = new_binary_expr(left, op, right)

		if (debug > 1) print *, 'copied left = ', left%str()

		current = parser%current()
	end do

	if (debug > 1) print *, 'parse_term = ', left%str()
	if (debug > 1) print *, 'done parse_term'

end function parse_term

!===============================================================================

recursive function parse_factor(parser) result(left)

	class(parser_t) :: parser

	type(syntax_node_t) :: left

	!********

	type(syntax_node_t) :: right
	type(syntax_token_t) :: current, op

	if (debug > 1) print *, 'parse_factor'

	left = parser%parse_primary_expr()

	current = parser%current()
	do while (current%kind == star_token .or. &
	          current%kind == slash_token)

		op = parser%next()
		right = parser%parse_primary_expr()
		left  = new_binary_expr(left, op, right)

		current = parser%current()
	end do

	if (debug > 1) print *, 'done parse_factor'

end function parse_factor

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	type(syntax_token_t) :: num

	if (debug > 1) print *, 'parse_primary_expr'

	! TODO: parens, unary operators

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

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
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
		token = parser%next()
		return
	end if

	! TODO: diags
	write(*,*) 'Error: unexpected token'
	token = new_token(kind, current%pos, null_char, 0)

end function match

!===============================================================================

function current_token(parser)

	! Refactor in terms of peek(0) ?
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token

	if (debug > 2) print *, 'current_token pos ', parser%pos

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

recursive function syntax_eval(node) result(res)

	type(syntax_node_t) :: node

	integer :: res

	!********

	! TODO: polymorphic types
	integer :: left, right

	if (node%kind == num_expr) then
		res = node%num%val
		return
	end if

	if (node%kind == binary_expr) then

		left  = syntax_eval(node%left )
		right = syntax_eval(node%right)

		!print *, 'left  = ', left
		!print *, 'right = ', right

		if      (node%op%kind == plus_token) then
			res = left + right
		else if (node%op%kind == minus_token) then
			res = left - right
		else if (node%op%kind == star_token) then
			res = left * right
		else if (node%op%kind == slash_token) then
			res = left / right
		else
			! Anything here should have been caught by a parser diag
			write(*,*) 'Error: unexpected binary operator "', node%op%text, '"'
			stop
		end if

		return

	end if

	! TODO
	write(*,*) 'Error: unexpected node "', kind_name(node%kind), '"'
	res = 0

end function syntax_eval

!===============================================================================

subroutine interpret()

	! This is the interpret shell
	!
	! TODO: arg for iu as stdin vs another file

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io, res

	character(len = :), allocatable :: line
	character(len = *), parameter :: prompt = lang_name//'$ '

	type(syntax_node_t) :: tree

	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	! Read-eval-print-loop
	do
		write(ou, '(a)', advance = 'no') prompt
		line = read_line(iu, iostat = io)

		!print *, 'line = <', line, '>'
		!print *, 'io = ', io

		! Echo input
		write(ou, '(a)') line

		if (io == iostat_end) exit

		! Skip empty lines
		if (len(line) == 0) cycle

		tree = syntax_parse(line)

		if (debug > 0) print *, 'tree = ', tree%str()

		! TODO: catch diags
		res  = syntax_eval (tree)

		! Consider MATLAB-style "ans = " log?
		write(*, '(i0)') res

	end do

end subroutine interpret

!===============================================================================

integer function eval(str)

	character(len = *), intent(in) :: str

	eval = syntax_eval(syntax_parse(str))

end function eval

!===============================================================================

end module core_m

!===============================================================================

subroutine unit_test_bin_arith(npass, nfail)

	use core_m
	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'binary arithmetic'

	integer :: id(1)

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: subtraction, division, more in general

	tests = &
		[   &
			eval('1') == 1, &
			eval('69') == 69, &
			eval('420') == 420, &
			eval('1 + 2') == 1 + 2, &
			eval('1 + 2 + 34') == 1 + 2 + 34, &
			eval('1 + 2 * 3') == 1 + 2 * 3, &
			eval('1 * 2 * 3 * 4') == 1 * 2 * 3 * 4, &
			eval('73 - 48') == 73 - 48, &
			eval('73 - 48 - 21') == 73 - 48 - 21, &
			eval('24 / 6') == 24 / 6, &
			eval('24 / 6 / 2') == 24 / 6 / 2, &
			eval('343 - 87654345 / 27 + 76 * 234 - 65432 / 63') &
			   == 343 - 87654345 / 27 + 76 * 234 - 65432 / 63 &
		]

	if (.not. all(tests)) then

		write(*, '(a,i0,a)') '     Error: ', count(.not. tests), &
				' '//label//' test(s) failed'

		id = findlocl1(tests, .false.)
		write(*, '(a,i0,a)') '     Test ID ', id(1), ' was the first failure'

	end if

	npass = npass + count(tests)
	nfail = nfail + count(.not. tests)

end subroutine unit_test_bin_arith

!===============================================================================

subroutine unit_tests()

	implicit none

	integer :: npass, nfail

	! TODO: move to separate program

	write(*,*) repeat('=', 72)
	write(*,*) 'Running unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_bin_arith(npass, nfail)

	write(*,*)
	write(*,*) repeat('+', 42)
	write(*,*) '+', npass, ' total tests passed        +'
	write(*,*) '+', nfail, ' total tests failed        +'
	write(*,*) repeat('+', 42)
	write(*,*)
	write(*,*) repeat('=', 72)

end subroutine unit_tests

!===============================================================================

program main

	use core_m
	implicit none

	! TODO: print basic help/exiting message
	write(*,*)
	write(*,*) lang_name//' v0.0.1'
	write(*,*)

	call unit_tests()

	call interpret()

end program main

!===============================================================================

