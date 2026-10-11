
!===============================================================================

program interpret_file

	! Run a whole syntran script from a file.  Output from the script goes to
	! stdout, like it would with the `syntran` executable.
	!
	! Run it from the project root with
	!
	!     fpm run --example interpret_file

	use syntran

	implicit none

	character(len = :), allocatable :: res

	integer :: io

	res = syntran_interpret_file('example/hello.syntran', quiet = .true., io = io)

	if (io /= exit_success) error stop 'script failed'

end program interpret_file

!===============================================================================
