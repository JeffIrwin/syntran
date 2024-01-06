
!===============================================================================

program main

	use syntran__app_m
	use syntran__core_m
	use syntran

	implicit none

	character(len = :), allocatable :: res

	type(args_t) :: args

	!********

	args = parse_args()

	if (args%version .or. args%help) then
		call exit(EXIT_SUCCESS)
	end if

	! TODO: move into settings constructor?
	maxerr = args%maxerr

	if (args%syntran_file_arg) then
		res = syntran_interpret_file(args%syntran_file)
		write(*,*) '    '//res

	else if (args%command_arg) then
		res = syntran_eval(args%command)
		write(*,*) "ans = `", res, "`"  ! format subject to change

		! python -c command doesn't print anything unless you call print()
		! inside it

	else
		res = syntran_interpret()

	end if

	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

