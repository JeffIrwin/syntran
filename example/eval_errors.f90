
!===============================================================================

program eval_errors

	! What to do when the syntran source is bad.  Pass quiet = .true. and
	! check `io`, or collect the messages in `diags`.
	!
	! Without quiet = .true., a runtime error in the syntran source prints a
	! message and calls exit(), which ends the *host* program too.
	!
	! Run it from the project root with
	!
	!     fpm run --example eval_errors

	use syntran

	implicit none

	character(len = :), allocatable :: res

	integer :: io, n

	type(string_vector_t) :: diags

	! A parse (or type) error:  `1 + true` has no meaning
	res = syntran_eval('1 + true;', quiet = .true., diags = diags, io = io)
	call report('parse error', io, diags)
	if (io /= exit_failure) error stop 'expected a parse error'

	! A runtime error:  this parses fine, but "abc" is not an integer
	res = syntran_eval('parse_i32("abc");', quiet = .true., diags = diags, io = io)
	call report('runtime error', io, diags)
	if (io /= exit_failure) error stop 'expected a runtime error'

	! A type mismatch.  The program is fine, but it's an f32 and not an i32.
	! The result is 0 and `io` tells you that it is not a real zero
	n = syntran_eval_i32('1.5;', quiet = .true., io = io)
	print *, 'wrong type: io = ', io, ', n = ', n
	if (io /= exit_failure) error stop 'expected a type mismatch'

	! The host program is still alive, and syntran keeps no state between
	! calls, so the next one is unaffected by the failures above
	n = syntran_eval_i32('6 * 7;', quiet = .true., io = io)
	print *, 'still works: ', n
	if (io /= exit_success .or. n /= 42) error stop 'recovery failed'

contains

subroutine report(label, io, diags)

	character(len = *), intent(in) :: label
	integer, intent(in) :: io
	type(string_vector_t), intent(in) :: diags

	integer :: i

	print *, label, ': io = ', io, ', ', diags%len_, ' diagnostic(s)'
	do i = 1, diags%len_
		print *, '    ', diags%v(i)%s
	end do

end subroutine report

end program eval_errors

!===============================================================================
