
!===============================================================================

program main

	use syntran__app_m
	use syntran__core_m
	use syntran

	implicit none

	character(len = :), allocatable :: res
	character(len = :), allocatable :: url, version

	type(args_t) :: args

	!********

	version = &
		str(syntran_major)//'.'// &
		str(syntran_minor)//'.'// &
		str(syntran_patch)

	url = 'https://github.com/JeffIrwin/syntran'

	args = parse_args()

	! TODO: add a "--no-banner" cmd arg to turn off this (and the banner() fn
	! call).  Add a twitch bot cmd to echo a str into syntran interpretter and
	! echo the evaluated answer back into twitch chat
	!
	! Maybe filter input and ban use of file "open" and anything unsafe
	if (.not. args%command_arg) then
		write(*,*)
		write(*,*) fg_bright_magenta//lang_name//' '//version//color_reset
		write(*,*) fg_bright_magenta//url//color_reset
		write(*,*)
	end if

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
		write(*,*) "ans = `", res, "`"

	else

		call  syntran_banner()
		res = syntran_interpret()

	end if

	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

