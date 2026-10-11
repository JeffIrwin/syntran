
!===============================================================================

program eval_basic

	! The simplest way to use syntran as a library:  hand a string of syntran
	! source to one of the syntran_eval*() functions and get the result back as
	! a Fortran value.
	!
	! Run it from the project root with
	!
	!     fpm run --example eval_basic

	use syntran

	implicit none

	character(len = 64) :: buf
	character(len = :), allocatable :: arr

	integer :: io
	integer :: i
	integer(kind = 8) :: big

	real(kind = 8) :: x, y

	! Typed wrappers exist for i32, i64, f32, f64, bool, and str.  They return
	! the value directly.  Each call is independent, so state is not kept
	! between calls
	i = syntran_eval_i32('1 + 2 * 3;')
	print *, 'i32 : ', i
	if (i /= 7) error stop 'wrong i32'

	big = syntran_eval_i64("2'i64 ** 40;")
	print *, 'i64 : ', big
	if (big /= 1099511627776_8) error stop 'wrong i64'

	x = syntran_eval_f64('sqrt(2.0);')
	print *, 'f64 : ', x
	if (abs(x - sqrt(2.d0)) > 1.d-12) error stop 'wrong f64'

	if (.not. syntran_eval_bool('3 < 4 and 5 > 2;')) error stop 'wrong bool'
	print *, 'bool: ', syntran_eval_bool('3 < 4 and 5 > 2;')

	print *, 'str : ', syntran_eval_str('"hello " + "world";')

	! There is no way to pass a Fortran variable into syntran yet, so build the
	! source string instead.  Mind the literal format, e.g. a real needs a
	! decimal point to be an f64 in syntran
	y = 3.d0
	write(buf, '(a,es24.16,a)') '', y, ' ** 2;'
	x = syntran_eval_f64(trim(adjustl(buf)))
	print *, 'x**2: ', x
	if (abs(x - 9.d0) > 1.d-12) error stop 'wrong interpolated f64'

	! syntran_eval() works for any result type.  It returns the result
	! formatted as a string, just like the REPL would print it
	arr = syntran_eval('[1: 6] * 2;', io = io)
	print *, 'array: ', arr
	if (io /= exit_success) error stop 'array eval failed'

	! Multi-statement programs work too, including function declarations.  The
	! value of the last expression is the result
	i = syntran_eval_i32( &
		'fn sum_sq(n: i32): i32 {' // &
		'    let s = 0;' // &
		'    for k in [1: n + 1] { s += k * k; }' // &
		'    return s;' // &
		'}' // &
		'sum_sq(10);')
	print *, 'sum of squares 1..10: ', i
	if (i /= 385) error stop 'wrong sum of squares'

end program eval_basic

!===============================================================================
