
!===============================================================================

program main

	use syntran__app_m
	use syntran

	implicit none

	character(len = :), allocatable :: res

	integer :: io

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
	maxerr           = args%maxerr
	permissive_return = args%permissive_return

	io = EXIT_SUCCESS

	if (args%syntax_only) then
		! Parse and type check only, without evaluating.  Diagnostics (if any)
		! are printed by syntran_eval()/syntran_interpret_file(); nothing is
		! evaluated and no result is printed.  parse_args() has already
		! guaranteed one of these two input modes
		if (args%syntran_file_arg) then
			res = syntran_interpret_file(args%syntran_file, &
				chdir_ = args%chdir, script_args = args%script_args, &
				syntax_only = .true., io = io)
		else
			res = syntran_eval(args%command, script_args = args%script_args, &
				syntax_only = .true., io = io)
		end if

	else if (args%interactive) then
		! "Interactive" keeps running in the REPL with the same vars and fns
		! workspace after running a startup file
		!
		! TODO: add a test that covers a "-i" interactive run
		res = syntran_interpret(startup_file = args%syntran_file, &
			script_args = args%script_args)

	else if (args%syntran_file_arg) then
		! Interpret a file and exit
		res = syntran_interpret_file(args%syntran_file, quiet_info = args%quiet, &
			chdir_ = args%chdir, script_args = args%script_args, io = io)
		if (.not. args%quiet .and. io == EXIT_SUCCESS) write(*,*) '    '//res

	else if (args%command_arg) then
		! Interpret a cmd arg string
		res = syntran_eval(args%command, script_args = args%script_args, io = io)
		if (.not. args%quiet .and. io == EXIT_SUCCESS) write(*,*) '    '//res

		! python -c command doesn't print anything unless you call print()
		! inside it

	else
		! Start a clean interactive REPL shell (without any startup file)
		res = syntran_interpret(script_args = args%script_args)

	end if

	call exit(io)

end program main

!===============================================================================

