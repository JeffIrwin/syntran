
!===============================================================================

module mfint

	use iso_fortran_env
	use utils

	implicit none

	character(len = *), parameter :: lang_name = 'fint'

	! Token kind enum
	integer, parameter :: &
			bad_token        = 6, &
			plus_token       = 5, &
			minus_token      = 4, &
			whitespace_token = 3, &
			number_token     = 2, &
			eof_token        = 1

	type tsyntax_token

		! TODO: how to handle values of different types?
		integer :: val

		integer :: kind, pos
		character(len = :), allocatable :: text
	end type tsyntax_token

	type tsyntax_tree

		! TODO: just so I can set something w/o uninitialize warnings
		integer :: dummy

	end type tsyntax_tree

	type tlexer

		character(len = :), allocatable :: text
		integer :: pos

		contains
			procedure next_token, current

	end type tlexer

!===============================================================================

contains

!===============================================================================

function kind_name(kind)

	integer, intent(in) :: kind

	character(len = :), allocatable :: kind_name

	! TODO: do this with a fixed-length char array
	select case (kind)
		case (bad_token)
			kind_name = "bad_token"
		case (plus_token)
			kind_name = "plus_token"
		case (minus_token)
			kind_name = "minus_token"
		case (whitespace_token)
			kind_name = "whitespace_token"
		case (number_token)
			kind_name = "number_token"
		case (eof_token)
			kind_name = "eof_token"
		case default
			kind_name = "unknown"
	end select

end function

!===============================================================================

function new_token(kind, pos, text, val) result(tok)

	integer :: val
	integer :: kind, pos

	character(len = *) :: text

	type(tsyntax_token) :: tok

	tok%kind  = kind
	tok%pos  = pos
	tok%text = text
	tok%val  = val

end function new_token

!===============================================================================

character function current(lexer)

	class(tlexer) :: lexer

	if (lexer%pos > len(lexer%text)) then
		current = null_char
		return
	end if

	current = lexer%text( lexer%pos: lexer%pos )

end function current

!===============================================================================

function next_token(lexer) result(tok)

	class(tlexer) :: lexer

	type(tsyntax_token) :: tok

	!********

	integer :: val
	integer :: start, io

	character(len = :), allocatable :: text

	if (lexer%pos > len(lexer%text)) then
		tok = new_token(eof_token, lexer%pos, null_char, 0)
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

		tok = new_token(number_token, start, text, val)
		return

	end if

	if (is_whitespace(lexer%current())) then

		start = lexer%pos

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		tok = new_token(whitespace_token, start, text, 0)
		return

	end if

	select case (lexer%current())
		case ("+")
			tok = new_token(plus_token , lexer%pos, lexer%current(), 0)
		case ("-")
			tok = new_token(minus_token, lexer%pos, lexer%current(), 0)
		case default
			! TODO: diagnostics
			tok = new_token(bad_token, lexer%pos, lexer%current(), 0)
			print *, 'diag: bad token ', lexer%current()
	end select

	lexer%pos = lexer%pos + 1

end function next_token

!===============================================================================

function new_lexer(text) result(lexer)

	character(len = *) :: text

	type(tlexer) :: lexer

	lexer%text = text
	lexer%pos = 1

end function new_lexer

!===============================================================================

function syntax_tree_parse(str) result(tree)

	character(len = *) :: str

	type(tsyntax_tree) :: tree

	!********

	integer :: i

	type(tsyntax_token) :: token

	type(tlexer) :: lexer

	! TODO: remove
	tree%dummy = 0

	lexer = new_lexer(str)

	i = 0
	do
		i = i + 1
		token = lexer%next_token()
		if (token%kind == eof_token) exit

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then

			print *, 'token%text = <', token%text, &
					'> <'//kind_name(token%kind)//'>'

		end if

		!if (i == 10) exit

	end do

end function syntax_tree_parse

!===============================================================================

subroutine fint

	! This is the interpreter shell
	!
	! TODO: arg for iu as stdin vs another file

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = lang_name//'$ '

	type(tsyntax_tree) :: tree

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

end subroutine fint

!===============================================================================

end module mfint

!===============================================================================

program main

	use mfint

	write(*,*) lang_name//' v0.0.1'
	write(*,*)

	call fint()

end program main

!===============================================================================

