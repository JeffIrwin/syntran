
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

	! TODO: Add other aoc solutions.  Optimize if necessary, or just exclude
	! anything that takes more than ~30 s to run

	tests = &
		[   &
			interpret_file(path//"01/main.syntran", quiet = .true., chdir_ = .true.) == '107443', &
			interpret_file(path//"02/main.syntran", quiet = .true., chdir_ = .true.) == '76485', &
			interpret_file(path//"03/main.syntran", quiet = .true., chdir_ = .true.) == '88145909', &
			interpret_file(path//"04/main.syntran", quiet = .true., chdir_ = .true.) == '6311320', &
			interpret_file(path//"05/main.syntran", quiet = .true., chdir_ = .true.) == '261668924', &
			interpret_file(path//"06/main.syntran", quiet = .true., chdir_ = .true.) == '34675170', &
			interpret_file(path//"07/main.syntran", quiet = .true., chdir_ = .true.) == '499106636', &
			interpret_file(path//"08/main.syntran", quiet = .true., chdir_ = .true.) == '13289612829906', &
			interpret_file(path//"09/main.syntran", quiet = .true., chdir_ = .true.) == '1819127106', &
			interpret_file(path//"10/main.syntran", quiet = .true., chdir_ = .true.) == '7220', &
			interpret_file(path//"11/main.syntran", quiet = .true., chdir_ = .true.) == '827019473638', &
			interpret_file(path//"12/main.syntran", quiet = .true., chdir_ = .true.) == '21', &
			interpret_file(path//"13/main.syntran", quiet = .true., chdir_ = .true.) == '57259', &
			interpret_file(path//"14/main.syntran", quiet = .true., chdir_ = .true.) == '191157', &
			interpret_file(path//"15/main.syntran", quiet = .true., chdir_ = .true.) == '768500', &
			interpret_file(path//"16/main.syntran", quiet = .true., chdir_ = .true.) == '14553', &
			interpret_file(path//"17/main.syntran", quiet = .true., chdir_ = .true.) == '196', &
			interpret_file(path//"18/main.syntran", quiet = .true., chdir_ = .true.) == '111131797001390', &
			interpret_file(path//"19/main.syntran", quiet = .true., chdir_ = .true.) == '125317462035060', &
			interpret_file(path//"20/main.syntran", quiet = .true., chdir_ = .true.) == '818723272', &
			interpret_file(path//"21/main.syntran", quiet = .true., chdir_ = .true.) == '3770', &
			interpret_file(path//"22/main.syntran", quiet = .true., chdir_ = .true.) == '66981', &
			interpret_file(path//"23/main.syntran", quiet = .true., chdir_ = .true.) == '248', &
			interpret_file(path//"24/main.syntran", quiet = .true., chdir_ = .true.) == '25261', &
			interpret_file(path//"25/main.syntran", quiet = .true., chdir_ = .true.) == '555856', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_aoc_2023

!===============================================================================

subroutine unit_tests_long(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 72)
	write(*,*) 'Running long syntran unit tests ...'
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

