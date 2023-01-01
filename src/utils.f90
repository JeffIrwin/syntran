
!===============================================================================

module utils

	use iso_fortran_env
	implicit none

	character, parameter :: &
			null_char       = char( 0), &
			tab             = char( 9), &
			line_feed       = char(10), &
			vert_tab        = char(11), &
			carriage_return = char(13)

!===============================================================================

contains

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too
	!
	! TODO: move to utils

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

end module utils

!===============================================================================

