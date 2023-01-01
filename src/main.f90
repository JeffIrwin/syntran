
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

	integer, intent(in) :: iu

	character(len = :), allocatable :: str

	!********

	character :: c, buffer*4
	integer :: io, str_len

	!read (iu, '(a)') buffer
	!str = trim(buffer)

	str = ''

	! Read 1 character at a time until end
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit
		if (io == iostat_eor) exit
		str = str//c
	end do

	!nchars = len_trim(buffer)

	!allocate(character(len = nchars) :: str)
	!!allocate(character(len = ns) :: s)

	!! Can't backspace on stdin: "illegal seek"
	!backspace(iu)
	!read(iu, '(a)') str

end function read_line

!===============================================================================

subroutine fint

	integer, parameter :: iu = input_unit, ou = output_unit

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = 'fint$ '

	! Read-print-loop
	do
		write(ou, '(a)', advance = 'no') prompt
		input = read_line(iu)

		if (len_trim(input) > 0) write(ou, '(a)') trim(input)

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

