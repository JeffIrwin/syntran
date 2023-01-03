
module test_m

	implicit none

contains

!===============================================================================

subroutine unit_test_bin_arith(npass, nfail)

	use core_m
	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'binary arithmetic'

	integer :: id(1)

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('1') == 1, &
			eval('69') == 69, &
			eval('420') == 420, &
			eval('1337') == 1337, &
			eval('1 + 2') == 1 + 2, &
			eval('1 + 2 + 34') == 1 + 2 + 34, &
			eval('1 + 2 * 3') == 1 + 2 * 3, &
			eval('1 * 2 * 3 * 4') == 1 * 2 * 3 * 4, &
			eval('73 - 48') == 73 - 48, &
			eval('73 - 48 - 20') == 73 - 48 - 21, &
			eval('24 / 6') == 24 / 6, &
			eval('24 / 6 / 2') == 24 / 6 / 2, &
			eval('343 - 87654345 / 27 + 76 * 234 - 65432 / 63') &
			   == 343 - 87654345 / 27 + 76 * 234 - 65432 / 63 &
		]

	if (.not. all(tests)) then

		write(*, '(a,i0,a)') '     Error: ', count(.not. tests), &
				' '//label//' test(s) failed'

		id = findlocl1(tests, .false.)
		write(*, '(a,i0,a)') '     Test ID ', id(1), ' was the first failure'

	end if

	npass = npass + count(tests)
	nfail = nfail + count(.not. tests)

end subroutine unit_test_bin_arith

!===============================================================================

subroutine unit_tests(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	! TODO: move to separate program

	write(*,*) repeat('=', 72)
	write(*,*) 'Running unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_bin_arith(npass, nfail)

	write(*,*)
	write(*,*) repeat('+', 42)
	write(*,*) '+', npass, ' total tests passed        +'
	write(*,*) '+', nfail, ' total tests failed        +'
	write(*,*) repeat('+', 42)
	write(*,*)
	write(*,*) repeat('=', 72)

	iostat = nfail

end subroutine unit_tests

!===============================================================================

end module test_m

!===============================================================================

program test

	use core_m
	use test_m
	implicit none

	integer :: io

	call unit_tests(io)
	call exit(io)

end program test

!===============================================================================

