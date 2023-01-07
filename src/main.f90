
!===============================================================================

program main

	use core_m
	implicit none

	character(len = :), allocatable :: res

	call syntran_banner()
	res = interpret()

end program main

!===============================================================================

