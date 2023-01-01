
!===============================================================================

module mfint

	use iso_fortran_env

	implicit none

!===============================================================================

contains

!===============================================================================

function read_line(iu) result(str)

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too
	!
	! TODO: move to utils

	integer, intent(in) :: iu

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

end function read_line

!===============================================================================

subroutine fint

	! This is the interpreter shell
	!
	! TODO: arg for iu as stdin vs another file

	integer, parameter :: iu = input_unit, ou = output_unit

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = 'fint$ '

	! Read-print-loop (eval TBD)
	do
		write(ou, '(a)', advance = 'no') prompt
		input = read_line(iu)

		if (len(input) > 0) write(ou, '(a)') input

	end do

end subroutine fint

!===============================================================================

end module mfint

!===============================================================================

program main

	use mfint

	write(*,*) 'Fint v0.0.1'
	write(*,*)

	call fint()

end program main

!===============================================================================

