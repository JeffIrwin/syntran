
!===============================================================================

module syntran__math_left_shift_m

	! Unlike math_bin_*.f90, this source was manually generated, but by copying
	! from math_bin_subtract.f90 and removing all the f32/f64 cases
	!
	! Shifting is defined *only* for integers, not floats.  However, an i32 can
	! be shifted by i64 and vice-versa.  This will not be the case for bitwise
	! and, or, etc.

	use syntran__value_m

	implicit none

	interface left_shift
		module procedure left_shift_value_t
	end interface left_shift

	interface shl_safe
		module procedure shl_safe_i32, shl_safe_i64
	end interface shl_safe

!===============================================================================

contains

!===============================================================================

elemental function shl_safe_i32(a, n) result(r)

	! Logical left shift of an i32 by n bits, which is an i64 so that a count of
	! either width can be passed.  The shift is in the width of the left
	! operand, so n is first truncated to 32 bits.  A count of 32 or more
	! gives 0, and a negative count is masked to 0:31.  The shiftl() intrinsic
	! would abort with -fcheck=all for any count outside of 0:32, and is
	! processor-dependent without it.  This is the same as the native backend

	integer(kind = 4), intent(in) :: a
	integer(kind = 8), intent(in) :: n
	integer(kind = 4) :: r

	integer(kind = 8) :: lo
	integer(kind = 4) :: c

	! Sign-extend the low 32 bits without overflowing an i32
	lo = iand(n, 4294967295_8)
	if (lo >= 2147483648_8) lo = lo - 4294967296_8
	c = int(lo, 4)

	if (c >= 32) then
		r = 0
	else
		r = shiftl(a, iand(c, 31))
	end if

end function shl_safe_i32

!===============================================================================

elemental function shl_safe_i64(a, n) result(r)

	! Like shl_safe_i32(), but in the width of an i64

	integer(kind = 8), intent(in) :: a
	integer(kind = 8), intent(in) :: n
	integer(kind = 8) :: r

	if (n >= 64) then
		r = 0
	else
		r = shiftl(a, int(iand(n, 63_8), 4))
	end if

end function shl_safe_i64

!===============================================================================
subroutine left_shift_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)
	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = shl_safe(left%array%i32, int(right%sca%i32, 8))
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = shl_safe(left%array%i64, int(right%sca%i32, 8))
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = shl_safe(left%array%i32, int(right%sca%i64, 8))
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = shl_safe(left%array%i64, int(right%sca%i64, 8))
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i32_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = shl_safe(left%sca%i32, int(right%array%i32, 8))
		case (i64_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = shl_safe(left%sca%i32, int(right%array%i64, 8))
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shl_safe(left%sca%i64, int(right%array%i32, 8))
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shl_safe(left%sca%i64, int(right%array%i64, 8))
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = shl_safe(left%array%i32, int(right%array%i32, 8))

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shl_safe(left%array%i64, int(right%array%i64, 8))

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = shl_safe(left%array%i32, int(right%array%i64, 8))

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shl_safe(left%array%i64, int(right%array%i32, 8))

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = shl_safe(left%sca%i32, int(right%sca%i32, 8))

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = shl_safe(left%sca%i64, int(right%sca%i64, 8))

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = shl_safe(left%sca%i64, int(right%sca%i32, 8))

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = shl_safe(left%sca%i32, int(right%sca%i64, 8))

	case        (magic**2 * i32_type + magic * i32_type + i64_type)
		res%sca%i32 = shl_safe(left%sca%i32, int(right%sca%i64, 8))

	!****
	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine left_shift_value_t

!===============================================================================

end module syntran__math_left_shift_m

!===============================================================================

