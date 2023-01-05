
module test_m

	use core_m
	use utils

	implicit none

contains

!===============================================================================

subroutine unit_test_bin_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'binary arithmetic'

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
			eval('73 - 48 - 21') == 73 - 48 - 21, &
			eval('24 / 6') == 24 / 6, &
			eval('24 / 6 / 2') == 24 / 6 / 2, &
			eval('343 - 87654345 / 27 + 76 * 234 - 65432 / 63') &
			   == 343 - 87654345 / 27 + 76 * 234 - 65432 / 63 &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bin_arith

!===============================================================================

subroutine unit_test_paren_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'parenthesized arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! In the first 3 tests, the parens don't change the result, but they should
	! at least be parsed
	tests = &
		[   &
			eval('(69)') == (69), &
			eval('(1) + 2') == (1) + 2, &
			eval('1 + (2 + 34)') == 1 + (2 + 34), &
			eval('(1 + 2) * 3') == (1 + 2) * 3, &
			eval('1 * (2 * 3 * 4)') == 1 * (2 * 3 * 4), &
			eval('73 - (48)') == 73 - (48), &
			eval('73 - (48 - 21)') == 73 - (48 - 21), &
			eval('24 / (6 / 2)') == 24 / (6 / 2), &
			eval('343 - (87654345 / 27 + 76 * (234 - 65432)) / 63') &
			   == 343 - (87654345 / 27 + 76 * (234 - 65432)) / 63,  &
			eval(  &
				'(1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9 ' &
				//' - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8) ' &
				//' - 9) * 8) + 1 - (2 * (3 + (4 - (5 * 6) / 2)  ' &
				//' + 7) - 8) * 9) + 1) * 42')   &
				== &
				 (1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9   &
				    - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8)   &
				    - 9) * 8) + 1 - (2 * (3 + (4 - (5 * 6) / 2)    &
				    + 7) - 8) * 9) + 1) * 42     &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_paren_arith

!===============================================================================

subroutine unit_test_unary_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'unary arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! I'm intentionally not testing things like '--69' or '-+-69' since those
	! will conflict with the prefix/postfix increment/decrement operators that
	! I'm planning.  Use parentheses if you want to write that bullshit.
	tests = &
		[   &
			eval('-(69)') == -(69), &
			eval('(-69)') == (-69), &
			eval('+(69)') == +(69), &
			eval('(+69)') == (+69), &
			eval('(-1) + 2') == (-1) + 2, &
			eval('1 + (+2 + 34)') == 1 + (+2 + 34), &
			eval('(1 + 2) * -3') == (1 + 2) * -3, &
			eval('-1 * (2 * -3 * -4)') == -1 * (2 * -3 * -4), &
			eval('-73 - (+48)') == -73 - (+48), &
			eval('24 / (-6 / 2)') == 24 / (-6 / 2), &
			eval('343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63') &
			   == 343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_unary_arith

!===============================================================================

subroutine unit_test_coda(tests, label, npass, nfail)

	logical, intent(in) :: tests(:)
	character(len = *), intent(in) :: label
	integer, intent(inout) :: npass, nfail

	!********

	integer :: id(1)

	if (.not. all(tests)) then

		call console_color(fg_bright_red)
		write(*, '(a,i0,a)') '     Error: ', count(.not. tests), &
				' '//label//' test(s) failed'

		id = findlocl1(tests, .false.)
		write(*, '(a,i0,a)') '     Test ID ', id(1), ' was the first failure'
		call console_color_reset()

	end if

	npass = npass + count(tests)
	nfail = nfail + count(.not. tests)

end subroutine unit_test_coda

!===============================================================================

subroutine unit_tests(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 72)
	write(*,*) 'Running unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_bin_arith  (npass, nfail)
	call unit_test_paren_arith(npass, nfail)
	call unit_test_unary_arith(npass, nfail)

	write(*,*)
	write(*,*) repeat('+', 42)

	write(*, '(a)', advance = 'no') ' +'
	if (npass > 0) call console_color(fg_bright_green)
	write(*, '(i12,a)', advance = 'no') npass, ' total tests passed'
	call console_color_reset()
	write(*,*) '        +'

	write(*, '(a)', advance = 'no') ' +'
	if (nfail > 0) call console_color(fg_bright_red)
	write(*, '(i12,a)', advance = 'no') nfail, ' total tests failed'
	call console_color_reset()
	write(*,*) '        +'

	write(*,*) repeat('+', 42)
	write(*,*)
	write(*,*) repeat('=', 72)

	iostat = nfail

end subroutine unit_tests

!===============================================================================

end module test_m

!===============================================================================

program test

	use test_m
	implicit none

	integer :: io

	call unit_tests(io)
	call exit(io)

end program test

!===============================================================================

