
!===============================================================================

program synup

	! The toolchain manager.  See src/synup.f90

	use syntran__core_m, only: syntran_major, syntran_minor, syntran_patch
	use syntran__synup_m
	use syntran__utils_m

	implicit none

	character(len = :), allocatable :: arg

	integer :: i, n, status

	type(string_vector_t) :: argv

	argv = new_string_vector()
	do i = 1, command_argument_count()
		call get_command_argument(i, length = n)
		allocate(character(len = n) :: arg)
		call get_command_argument(i, arg)
		call argv%push(arg)
		deallocate(arg)
	end do

	call synup_colors()

	status = synup_cli(argv, &
		str(syntran_major)//'.'//str(syntran_minor)//'.'//str(syntran_patch))

	call exit(status)

end program synup

!===============================================================================
