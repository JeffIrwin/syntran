
!===============================================================================

#if defined(__GFORTRAN__)
module ifport
	! Unfortunately this is the only way i can get fpm to not complain about
	! ifport with gfortran
end module ifport
#endif

module syntran__app_m

	use syntran
	use syntran__core_m
	use syntran__errors_m
	use syntran__utils_m

	implicit none

	character(len = *), parameter :: &
		COLOR_AUTO = "auto", &
		COLOR_ON   = "on", &
		COLOR_OFF  = "off"

	type args_t

		character(len = :), allocatable :: syntran_file, command, color

		integer :: maxerr

		logical :: &
			command_arg      = .false., &
			interactive      = .false., &
			quiet            = .false., &
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

subroutine set_ansi_colors(is_color_in)

	! Should this be in error.f90 or utils.f90?  This is the only reason
	! test.f90 needs to use app.f90

#if defined(__INTEL_COMPILER)
	use ifport  ! isatty() (otherwise it's a gnu extension)
#endif

	logical, intent(in), optional :: is_color_in
	logical :: is_color

	!print *, "starting set_ansi_colors()"
	if (present(is_color_in)) then
		is_color = is_color_in
		!print *, "is_color_in = ", is_color_in
	else
		! If stdout is a TTY, default to color on.  If stdout is redirected to a
		! log (not TTY), default to color off because most text editors will not
		! render ANSI escape sequences
		is_color = isatty(output_unit)
		!print *, "is_color_in not present"
	end if

	if (is_color) then
		fg_bold            = FG_BOLD_
		fg_bright_red      = FG_BRIGHT_RED_
		fg_bold_bright_red = FG_BOLD_BRIGHT_RED_
		fg_bright_green    = FG_BRIGHT_GREEN_
		fg_green           = FG_GREEN_
		fg_bright_blue     = FG_BRIGHT_BLUE_
		fg_bright_magenta  = FG_BRIGHT_MAGENTA_
		fg_bright_cyan     = FG_BRIGHT_CYAN_
		fg_bright_white    = FG_BRIGHT_WHITE_
		color_reset        = COLOR_RESET_
	else
		fg_bold            = ""
		fg_bright_red      = ""
		fg_bold_bright_red = ""
		fg_bright_green    = ""
		fg_green           = ""
		fg_bright_blue     = ""
		fg_bright_magenta  = ""
		fg_bright_cyan     = ""
		fg_bright_white    = ""
		color_reset        = ""
	end if

	! These include fg_bold at the end, so the rest of the error message after
	! the prefix must concatenate color_reset at its end
	err_prefix     = fg_bold_bright_red//'Error'//fg_bold//': '
	err_int_prefix = fg_bold_bright_red//'Internal syntran error'//fg_bold//': '
	err_rt_prefix  = fg_bold_bright_red//'Runtime error'//fg_bold//': '

end subroutine set_ansi_colors

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
	args%color  = COLOR_AUTO

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

		case ("--color")
			call get_next_arg(i, str_)
			args%color = str_

			select case (args%color)
			case (COLOR_AUTO)
				! Note that set_ansii_colors() is also called in main() to
				! initially default to auto, in case of errors during
				! parse_args()
				call set_ansi_colors()

			case (COLOR_ON, COLOR_OFF)
				call set_ansi_colors(args%color == COLOR_ON)

			case default
				write(*,*) err_prefix//"bad --color argument"
				error = .true.
			end select

		case ("-c", "--command")
			args%command_arg = .true.
			call get_next_arg(i, args%command)

		case ("-q", "--quiet")
			args%quiet = .true.

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

	if (.not. error .and. .not. args%quiet) then

		interactive = .not. &
			( &
				args%command_arg      .or. &
				args%version          .or. &
				args%syntran_file_arg .or. &
				args%help                  &
			)

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

		! TODO: first <file> line is long enough that I should probbaly just say
		! `[options]`

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "    syntran <file.syntran> [--fmax-errors <n>] " &
			//"[-i | --interactive] [-q | --quiet] [--color (auto|off|on)]"
		write(*,*) "    syntran"
		write(*,*) "    syntran -c <cmd> | --command <cmd>"
		write(*,*) "    syntran -h | --help"
		write(*,*) "    syntran --version"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "    -h --help           Show this help"
		write(*,*) "    --version           Show version and build details"
		write(*,*) "    -c --command <cmd>  Run program passed in as string"
		write(*,*) "    --color (off|on)    Set ANSI text color [default: auto]"
		write(*,*) "    --fmax-errors <n>   Limit max " &
			//"error messages to <n> [default: "//str(maxerr_def)//"]"
		write(*,*) "    -i --interactive    Interpret a file then start an interactive shell"
		write(*,*) "    -q --quiet          Don't print the banner, only errors and println calls"
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

