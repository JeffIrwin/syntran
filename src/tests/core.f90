
!===============================================================================

module syntran__test_core_m

	! Use short names for convenience
	use syntran, &
		interpret      => syntran_interpret, &
		interpret_file => syntran_interpret_file, &
		eval      => syntran_eval     , &
		eval_i32  => syntran_eval_i32, &
		eval_f32  => syntran_eval_f32

	use syntran__utils_m, only: fg_bright_red, fg_bright_green, line_feed, &
		findlocl1, console_color, console_color_reset

	implicit none

contains

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

subroutine log_test_summary(npass, nfail)

	implicit none

	integer, intent(in) :: npass, nfail

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

end subroutine log_test_summary

!===============================================================================

end module syntran__test_core_m

!===============================================================================

