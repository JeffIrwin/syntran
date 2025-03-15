
!===============================================================================

program main

	use syntran__app_m
	use syntran

	implicit none

	character(len = :), allocatable :: res

	type(args_t) :: args

	!********

	! What happens if there's an error while parsing args?  Here I initialize to
	! --color auto, but maybe off should be the initial default
	call set_ansi_colors()
	!call set_ansi_colors(.true.)

	args = parse_args()

	if (args%version .or. args%help) then
		call exit(EXIT_SUCCESS)
	end if

	! TODO: move into settings constructor?
	maxerr = args%maxerr

	! TODO: add a chdir cmd arg to pass to syntran_interpret_file(), for
	! convenience of debugging AOC solutions which need to chdir to load their
	! input files

	if (args%interactive) then
		! "Interactive" keeps running in the REPL with the same vars and fns
		! workspace after running a startup file
		!
		! TODO: add a test that covers a "-i" interactive run
		res = syntran_interpret(startup_file = args%syntran_file)

	else if (args%syntran_file_arg) then
		! Interpret a file and exit
		res = syntran_interpret_file(args%syntran_file, quiet_info = args%quiet)
		if (.not. args%quiet) write(*,*) '    '//res

	else if (args%command_arg) then
		! Interpret a cmd arg string
		res = syntran_eval(args%command)
		if (.not. args%quiet) write(*,*) '    '//res

		! python -c command doesn't print anything unless you call print()
		! inside it

	else
		! Start a clean interactive REPL shell (without any startup file)
		res = syntran_interpret()

	end if

	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

