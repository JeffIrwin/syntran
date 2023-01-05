
!===============================================================================

program main

	use core_m
	implicit none

	! TODO: move version to core

	! TODO: move to a new fn
	write(*,*)
	write(*,*) lang_name//' 0.0.2'
	write(*,*) 'https://github.com/JeffIrwin/syntran'
	write(*,*)
	write(*,*) 'Usage:'
	write(*,*) tab//'#tree to toggle tree display'
	write(*,*) tab//'Ctrl+C to exit'
	write(*,*)

	call interpret()

end program main

!===============================================================================

