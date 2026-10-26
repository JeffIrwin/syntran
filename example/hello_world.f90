
!===============================================================================

program hello_world

	! The most basic example:  evaluate a string of syntran source from Fortran.
	! For more, see eval_basic.f90
	!
	! Run it from the project root with
	!
	!     fpm run --example hello_world

	use syntran

	implicit none

	character(len = :), allocatable :: res

	res = syntran_eval('println("hello world");')

end program hello_world

!===============================================================================
