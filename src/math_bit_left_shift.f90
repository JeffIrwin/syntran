
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

!===============================================================================

contains

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
			res%array%i32 = shiftl(left%array%i32, right%sca%i32)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = shiftl(left%array%i64, right%sca%i32)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = shiftl(left%array%i32, right%sca%i64)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = shiftl(left%array%i64, right%sca%i64)
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
			res%array%i32 = shiftl(left%sca%i32, right%array%i32)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shiftl(left%sca%i32, right%array%i64)
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
			res%array%i64 = shiftl(left%sca%i64, right%array%i32)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shiftl(left%sca%i64, right%array%i64)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = shiftl(left%array%i32, right%array%i32)

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shiftl(left%array%i64, right%array%i64)

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shiftl(left%array%i32, right%array%i64)

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = shiftl(left%array%i64, right%array%i32)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = shiftl(left%sca%i32, right%sca%i32)

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = shiftl(left%sca%i64, right%sca%i64)

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = shiftl(left%sca%i64, right%sca%i32)

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = shiftl(left%sca%i32, right%sca%i64)

	case        (magic**2 * i32_type + magic * i32_type + i64_type)
		res%sca%i32 = shiftl(left%sca%i32, right%sca%i64)

	!****
	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine left_shift_value_t

!===============================================================================

end module syntran__math_left_shift_m

!===============================================================================

