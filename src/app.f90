
!===============================================================================

module app_m

	use core_m
	use errors_m
	use utils_m

	implicit none

	type args_t

		character(len = :), allocatable :: syntran_file

		integer :: maxerr

		logical :: &
			!any_arg          = .false., &
			syntran_file_arg = .false., &
			version          = .false., &
			help             = .false.

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!character(len = *), intent(in) :: argv0
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

	character(len = :), allocatable :: argv, str_

	integer :: i, io, argc, ipos

	logical :: lerror = .false.

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
			!args%any_arg = .true.
			args%help    = .true.

		case ("--fmax-errors")
			!args%any_arg = .true.
			call get_next_arg(i, str_)

			read(str_, *, iostat = io) args%maxerr
			if (io /= exit_success) then
				write(*,*) err_prefix//"--fmax-errors "//str_ &
					//" is not a valid integer"
				lerror = .true.
			end if

		!case ("-l", "--language")
		!	args%any_arg = .true.
		!	call get_next_arg(i, args%language)
		!	!if (.not. any(langs == args%language)) then
		!	!	write(*,*) err_prefix//"language """//args%language &
		!	!		//""" not supported or invalid ISO 639-1 language code"
		!	!	lerror = .true.
		!	!end if

		case ("--version")
			!args%any_arg = .true.
			args%version = .true.

		case default
			!args%any_arg = .true.

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
				lerror = .true.

			end if

		end select

	end do

	!if (ipos < 1 .and. .not. (args%help .or. args%version)) then
	!	write(*,*) err_prefix//"syntran file not defined"
	!	lerror = .true.
	!end if

	if (lerror .or. args%help) then

		write(*,*) fg_bold//"Usage:"//color_reset
		write(*,*) "	syntran <file.syntran> [--fmax-errors <n>]"
		write(*,*) "	syntran -h | --help"
		write(*,*) "	syntran --version"
		write(*,*)
		write(*,*) fg_bold//"Options:"//color_reset
		write(*,*) "	-h --help          Show this help"
		write(*,*) "	--version          Show version"
		write(*,*) "	--fmax-errors <n>  Limit max " &
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

end module app_m

!===============================================================================

