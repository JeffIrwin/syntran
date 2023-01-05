
!===============================================================================

program main

	use core_m
	implicit none

	! TODO: move version to core

	! TODO: print basic help/exiting message
	write(*,*)
	write(*,*) lang_name//' 0.0.2'
	write(*,*) 'https://github.com/JeffIrwin/syntran'
	write(*,*)

	call interpret()

end program main

!===============================================================================

