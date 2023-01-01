
!===============================================================================

module mfint

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine fint

	!character(len = :), allocatable :: input
	character :: input*256

	do
		write(*, '(a)', advance = 'no') 'fint$ '

		read (*, '(a)') input

		if (len_trim(input) > 0) write(*, '(a)') trim(input)

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

