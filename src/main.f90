
!===============================================================================

module core_m

	use iso_fortran_env
	use utils

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	! Token kind enum
	integer, parameter :: &
			bad_token        = 6, &
			plus_token       = 5, &
			minus_token      = 4, &
			whitespace_token = 3, &
			number_token     = 2, &
			eof_token        = 1

	type syntax_token_t

		! TODO: how to handle values of different types?
		integer :: val

		integer :: kind, pos
		character(len = :), allocatable :: text
	end type syntax_token_t

	type syntax_tree_t

		! TODO: just so I can set something w/o uninitialize warnings
		integer :: dummy

	end type syntax_tree_t

	type lexer_t

		character(len = :), allocatable :: text
		integer :: pos

		contains
			procedure next_token, current

	end type lexer_t

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

	type(syntax_token_t) :: tok

	tok%kind = kind
	tok%pos  = pos
	tok%text = text
	tok%val  = val

end function new_token

!===============================================================================

character function current(lexer)

	class(lexer_t) :: lexer

	if (lexer%pos > len(lexer%text)) then
		current = null_char
		return
	end if

	current = lexer%text( lexer%pos: lexer%pos )

end function current

!===============================================================================

function next_token(lexer) result(tok)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: tok

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

	type(lexer_t) :: lexer

	lexer%text = text
	lexer%pos = 1

end function new_lexer

!===============================================================================

function syntax_tree_parse(str) result(tree)

	character(len = *) :: str

	type(syntax_tree_t) :: tree

	!********

	integer :: i

	type(syntax_token_t) :: tok
	type(syntax_token_vector_t) :: toks

	type(lexer_t) :: lexer

	! TODO: remove
	tree%dummy = 0

	toks = new_syntax_token_vector()

	lexer = new_lexer(str)

	do
		tok = lexer%next_token()

		if (tok%kind /= whitespace_token .and. &
		    tok%kind /= bad_token) then

			!print *, 'tok = <', tok%text, '> ', &
			!		'<'//kind_name(tok%kind)//'>'

			call toks%push(tok)

		end if

		if (tok%kind == eof_token) exit
	end do

	! TODO: make toks to str fn

	!print *, 'len = ', toks%len
	do i = 1, toks%len
		print *, 'tok = <', toks%v(i)%text, '> ', &
				'<'//kind_name(toks%v(i)%kind)//'>'
	end do

end function syntax_tree_parse

!===============================================================================

subroutine interpret()

	! This is the interpret shell
	!
	! TODO: arg for iu as stdin vs another file

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = lang_name//'$ '

	type(syntax_tree_t) :: tree

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

	write(*,*) lang_name//' v0.0.1'
	write(*,*)

	call interpret()

end program main

!===============================================================================

