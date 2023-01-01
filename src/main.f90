
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
	integer :: io

	character(len = :), allocatable :: input
	character(len = *), parameter :: prompt = 'fint$ '

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

		!var syntaxTree = SyntaxTree.Parse(line);

		if (io == iostat_end) exit

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

