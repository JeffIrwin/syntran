
!===============================================================================

program main

	use syntran
	implicit none

	character(len = :), allocatable :: res

	! TODO: add syntran_parse_args() (return arg struct) for interpreter source
	! file, help, version info, etc.

	call  syntran_banner()
	res = syntran_interpret()

end program main

!===============================================================================

