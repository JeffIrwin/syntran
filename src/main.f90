
!===============================================================================

program main

	use syntran
	implicit none

	character(len = :), allocatable :: res

	call  syntran_banner()
	res = syntran_interpret()

end program main

!===============================================================================

