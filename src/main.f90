
!===============================================================================

program main

	use app_m
	use core_m
	use syntran

	implicit none

	character(len = :), allocatable :: res

	integer :: argc
	character(len = 256) :: argv
	character(len = :), allocatable :: file
	character(len = :), allocatable :: version

	type(args_t) :: args

	!********

	version = &
		str(syntran_major)//'.'// &
		str(syntran_minor)//'.'// &
		str(syntran_patch)

	write(*,*)
	write(*,*) fg_bright_magenta//lang_name//' '//version//color_reset
	write(*,*) fg_bright_magenta//'https://github.com/JeffIrwin/syntran' &
		//color_reset
	write(*,*)

	!  TODO: add syntran_parse_args() (return arg struct) for interpreter source
	!  file, help, version info, max error logging, etc.  Cali has a good arg
	!  parser that could be copied 

	args = parse_args()
	if (args%version .or. args%help) then
		!write(*,*) "Homepage: github.com/JeffIrwin/syntran"
		call exit(EXIT_SUCCESS)
	end if

	argc = command_argument_count()
	if (.not. args%syntran_file_arg) then

		call  syntran_banner()
		res = syntran_interpret()

	else !if (argc == 1) then

		!! TODO: check truncation
		!call get_command_argument(1, argv)
		!file = trim(argv)

		!res = syntran_interpret_file(file)
		res = syntran_interpret_file(args%syntran_file)
		write(*,*) '    '//res

	!else
	!	write(*,*) 'Error: bad command line arguments'
	!	write(*,*)

	end if

end program main

!===============================================================================

