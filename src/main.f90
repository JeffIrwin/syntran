
!===============================================================================

module mfint

	use iso_fortran_env
	use utils

	implicit none

!===============================================================================

contains

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

