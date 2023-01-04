
!===============================================================================

module utils

	use iso_fortran_env
	implicit none

	character, parameter :: &
			null_char       = char( 0), &
			tab             = char( 9), &
			line_feed       = char(10), &
			vert_tab        = char(11), &
			carriage_return = char(13), &
			esc             = char(27)

	! ANSI escape color codes
	!
	!     https://stackoverflow.com/a/54062826/4347028
	!
	! TODO: more colors
	character(len = *), parameter :: &
			fg_bold            = esc//'[;1m', &
			fg_bright_red      = esc//'[91m', &
			fg_bold_bright_red = esc//'[91;1m', &
			fg_bright_green    = esc//'[92m', &
			fg_bright_white    = esc//'[97m', &
			color_reset        = esc//'[0m'

	!********

	type string_t
		character(len = :), allocatable :: s
	end type string_t

	!********

	type string_vector_t
		type(string_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push     => push_string
			procedure :: push_all => push_all_string
	end type string_vector_t

!===============================================================================

contains

!===============================================================================

function new_string_vector() result(vector)

	type(string_vector_t) :: vector

	vector%len = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_string_vector

!===============================================================================

subroutine push_string(vector, val)

	class(string_vector_t) :: vector

	character(len = *), intent(in) :: val

	!********

	type(string_t) :: val_str
	type(string_t), allocatable :: tmp(:)

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

	val_str%s = val
	vector%v( vector%len ) = val_str

end subroutine push_string

!===============================================================================

subroutine push_all_string(vector, add)

	class(string_vector_t) :: vector

	type(string_vector_t), intent(in) :: add

	!********

	integer :: i

	do i = 1, add%len
		call vector%push( add%v(i)%s )
	end do

end subroutine push_all_string

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c
	character(len = :), allocatable :: tmp

	integer :: i, io, str_cap, tmp_cap

	! Buffer string with some initial length
	str_cap = 64
	allocate(character(len = str_cap) :: str)

	! Read 1 character at a time until end
	i = 0
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit
		if (io == iostat_eor) exit
		!if (c == carriage_return) exit
		!if (c == line_feed) exit
		i = i + 1

		if (i > str_cap) then
			!print *, 'growing str'

			! Grow the buffer capacity.  What is the optimal growth factor?
			tmp_cap = 2 * str_cap
			allocate(character(len = tmp_cap) :: tmp)
			tmp(1: str_cap) = str

			call move_alloc(tmp, str)
			str_cap = tmp_cap

			!print *, 'str_cap  = ', str_cap
			!print *, 'len(str) = ', len(str)

		end if
		str(i:i) = c

	end do

	! Trim unused chars from buffer
	str = str(1:i)

	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

logical function is_digit(c)

	character, intent(in) :: c

	is_digit = '0' <= c .and. c <= '9'

end function is_digit

!===============================================================================

logical function is_whitespace(c)

	character, intent(in) :: c

	is_whitespace = any(c == [tab, line_feed, vert_tab, carriage_return, ' '])

end function is_whitespace

!===============================================================================

function findlocl1(arr, val) result(loc)

	! findloc() is standard in Fortran 2008, but gfortran 8.1.0 doesn't have it
	! yet :(.  Here I implement it for logical rank-1 arrays only without
	! optional args

	logical, intent(in) :: arr(:), val

	integer :: loc(1)

	if (size(arr) == 0) then
		loc = 0
		return
	end if

	loc = 1
	do while (arr(loc(1)) .neqv. val)
		loc(1) = loc(1) + 1

		if (loc(1) > size(arr, 1)) then
			! not found
			loc = 0
			return
		end if

	end do

end function findlocl1

!===============================================================================

! Colors work by default in bash and Windows terminal
!
! For color use in cmd or powershell, set:
!
!    [HKEY_CURRENT_USER\Console]
!    "VirtualTerminalLevel"=dword:00000001
!
! Ref:
!
!     https://superuser.com/a/1300251
!

subroutine console_color(color)
	character(len = *), intent(in) :: color
	write(*, '(a)', advance = 'no') color
end subroutine console_color

subroutine console_color_reset()
	write(*, '(a)', advance = 'no') color_reset
end subroutine console_color_reset

!===============================================================================

end module utils

!===============================================================================

