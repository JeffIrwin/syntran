
!===============================================================================

program main

	use core_m
	implicit none

	character(len = :), allocatable :: res

	! TODO: move version to core

	! TODO: move to a new fn
	write(*,*)
	write(*,*) lang_name//' 0.0.3'
	write(*,*) 'https://github.com/JeffIrwin/syntran'
	write(*,*)
	write(*,*) 'Usage:'
	write(*,*) tab//'#tree to toggle tree display'
	write(*,*) tab//'Ctrl+C to exit'
	write(*,*)

	res = interpret()

end program main

!===============================================================================

