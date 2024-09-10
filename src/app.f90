
!===============================================================================

module syntran__app_m

	use syntran
	use syntran__core_m
	use syntran__errors_m
	use syntran__utils_m

	implicit none

	type args_t

		character(len = :), allocatable :: syntran_file, command

		integer :: maxerr

		logical :: &
			command_arg      = .false., &
			interactive      = .false., &
			syntran_file_arg = .false., &
			version          = .false., &
			help             = .false.

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!********
	character(len = :), allocatable, save :: argv0
	character(len = 1024) :: buffer
	integer, parameter :: STAT_TRUNC = -1
	integer :: io, argc
	logical, save :: first = .true.

	if (first) then
		first = .false.
		call get_command_argument(0, buffer)
		argv0 = trim(buffer)
	end if

	i = i + 1
	argc = command_argument_count()
	if (i > argc) then
		write(*,*) err_prefix//"missing required argument after """//argv0//""""
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		write(*,*) err_prefix//"command argument too long after """//argv0//""""
		call exit(EXIT_FAILURE)

	else if (io /= EXIT_SUCCESS) then
		write(*,*) err_prefix//"cannot get command argument after """//argv0//""""
		call exit(EXIT_FAILURE)

	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

	argv0 = argv

end subroutine get_next_arg

!===============================================================================

function parse_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, str_, url, version

	integer :: i, io, argc, ipos

	logical :: error = .false., interactive

	! Defaults
	args%maxerr = maxerr_def

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		call get_next_arg(i, argv)

		select case (argv)
		case ("-h", "--help", "-help")
			args%help    = .true.

		case ("--fmax-errors")
			call get_next_arg(i, str_)

			read(str_, *, iostat = io) args%maxerr
			if (io /= exit_success) then
				write(*,*) err_prefix//"--fmax-errors "//str_ &
					//" is not a valid integer"
				error = .true.
			end if

		case ("-c", "--command")
			args%command_arg = .true.
			call get_next_arg(i, args%command)

		case ("-i", "--interactive")
			args%interactive = .true.

		case ("--version")
			args%version = .true.

		case default

			! Positional arg
			ipos = ipos + 1

			if (ipos == 1) then
				args%syntran_file_arg = .true.
				args%syntran_file = argv

			!else if (ipos == 2) then
			!	args%lout_file = .true.
			!	args%out_file  = argv

			else
				write(*,*) err_prefix//"unknown argument `"//argv//"`"
				error = .true.

			end if

		end select

	end do

	!if (ipos < 1 .and. .not. (args%help .or. args%version)) then
	!	write(*,*) err_prefix//"syntran file not defined"
	!	error = .true.
	!end if

	url = 'https://github.com/JeffIrwin/syntran'

	version = &
		str(syntran_major)//'.'// &
		str(syntran_minor)//'.'// &
		str(syntran_patch)

	if (.not. error) then

		interactive = .not. &
			( &
				args%command_arg      .or. &
				args%version          .or. &
				args%syntran_file_arg .or. &
				args%help                  &
			)

		!call  syntran_banner(args%command_arg, interactive)

		! TODO: add a "--no-banner" cmd arg to turn off?

		if (.not. args%command_arg) then
			write(*,*)
			write(*,*) fg_bright_magenta//lang_name//' '//version//color_reset
			write(*,*) fg_bright_magenta//url//color_reset
			if (args%version) then
				write(*,*) fg_bright_magenta// &
				               "git commit = "//git_commit
				write(*,*)     "build date = "//build_date
				write(*,*)     "fortran compiler = "//fort_compiler//" "// &
					str(fort_vers)//color_reset
			end if
			write(*,*)
		end if

		if (interactive) then
			write(*,*) 'Usage:'
			write(*,*) tab//'#tree to toggle tree display'
			write(*,*) tab//'`exit(0);` or Ctrl+C to exit'
			write(*,*)
		end if

		! TODO: add an interactive #help directive for more in depth info.  -h help
		! cmd arg already exists
	end if

	if (error .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "    syntran <file.syntran> [--fmax-errors <n>] [-i | --interactive]"
		write(*,*) "    syntran"
		write(*,*) "    syntran -c <cmd> | --command <cmd>"
		write(*,*) "    syntran -h | --help"
		write(*,*) "    syntran --version"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    -h --help           Show this help"
		write(*,*) "    --version           Show version and build details"
		write(*,*) "    -c --command <cmd>  Run program passed in as string"
		write(*,*) "    -i --interactive    Interpret a file then start an interactive shell"
		write(*,*) "    --fmax-errors <n>   Limit max " &
			//"error messages to <n> [default: "//str(maxerr_def)//"]"
		write(*,*)

		if (.not. args%help) call exit(EXIT_FAILURE)
	end if

	!if (.not. args%lout_file) then
	!	if (args%waterfall) then
	!		args%out_file = "./build/waterfall-"//basename(args%ttf_file)//".ppm"
	!	else
	!		args%out_file = "./build/"//basename(args%ttf_file)//".ppm"
	!	end if
	!end if

end function parse_args

!===============================================================================

end module syntran__app_m

!===============================================================================

