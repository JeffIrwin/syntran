
module long_m

	use syntran__test_core_m

	implicit none

contains

!===============================================================================

subroutine unit_test_aoc_2023(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'aoc 2023'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: &
		!path = 'src/tests/long-src/aoc/2023/'
		path = 'src/tests/long/aoc/2023/'

	character(len = 1024) :: buffer
	character(len = :), allocatable :: cwd

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: pushd/popd framework?
	call getcwd(buffer)
	cwd = trim(buffer)
	print *, "cwd = ", cwd

	! TODO: add on option to interpret_file() to chdir within.  That's the
	! only way I can interpret more than one AOC solution in a single `tests`
	! array
	call chdir(path//'01')
	call system("pwd")
	!call system("ls")

	! TODO: verify return values.  Just sum parts 1 and 2?  Optimize if
	! necessary, or just exclude anything that takes more than ~30 s to run

	tests = &
		[   &
			!interpret_file(path//'01/main.syntran', quiet) == 'true', &
			!interpret_file(path//'01/main.syntran') == 'true', &
			interpret_file("main.syntran") == '207443', &
			!interpret_file(path//'test-02.syntran', quiet) == '2' , &
			.false.  & ! so I don't have to bother w/ trailing commas
		]
	call chdir(cwd)

	!! TODO
	!call chdir(path//'02')
	!call system("pwd")
	!tests = &
	!	[   &
	!		interpret_file("main.syntran") == 'true', &
	!		.false.  & ! so I don't have to bother w/ trailing commas
	!	]
	!call chdir(cwd)

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)
	print *, 'tests = ', tests

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_aoc_2023

!===============================================================================

subroutine unit_tests_long(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 72)
	write(*,*) 'Running syntran unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_aoc_2023(npass, nfail)

	call log_test_summary(npass, nfail)

	iostat = nfail

end subroutine unit_tests_long

!===============================================================================

end module long_m

!===============================================================================

program long

	! Run with `fpm test long`

	use long_m
	implicit none

	integer :: io

	call unit_tests_long(io)
	call exit(io)

end program long

!===============================================================================

