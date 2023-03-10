
!===============================================================================

program main

	use syntran
	implicit none

	character(len = :), allocatable :: res


	integer :: argc
	character(len = 256) :: argv
	character(len = :), allocatable :: file

	! TODO: add syntran_parse_args() (return arg struct) for interpreter source
	! file, help, version info, etc.

	! TODO: printing the #help directive is ok for syntran_interpret, but it
	! doesn't help for syntran_interpret_file.  Refactor banner
	call  syntran_banner()

	argc = command_argument_count()
	if (argc == 0) then
		res = syntran_interpret()

	else if (argc == 1) then

		! TODO: check truncation
		call get_command_argument(1, argv)
		file = trim(argv)

		res = syntran_interpret_file(file)
		write(*,*) '    '//res

	else
		! TODO
		write(*,*) 'Error: bad command line arguments'
		write(*,*)
	end if

end program main

!===============================================================================

