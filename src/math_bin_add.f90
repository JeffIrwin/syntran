
!===============================================================================

module syntran__math_add_m

	use syntran__value_m

	implicit none

	interface add
		module procedure add_value_t
	end interface add

!===============================================================================

contains

!===============================================================================

subroutine add_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	! TODO: consider different operators for element-wise multiplication vs
	! matrix multiplication, e.g. `.*` vs `*` in MATLAB.  Similar for division
	!
	! Although I'm not sure I like MATLAB's choice.  Matrix-multiplication is
	! the special-purpose operator resticted to rank-2 only, while element-wise
	! multiplication is probably the more common operation

	!****
	case        (magic**2 * array_type + magic * array_type + f32_type)
		!print *, 'array_type + f32_type'

		select case (left%array%type)
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 + right%sca%f32
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i32 + right%sca%f32
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = real(left%array%i64) + right%sca%f32
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + f64_type)
		!print *, 'array_type + f64_type'

		select case (left%array%type)
		case (f64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = left%array%f64 + right%sca%f64
		case (i32_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = left%array%i32 + right%sca%f64
		case (i64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = real(left%array%i64, 8) + right%sca%f64
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = left%array%i32 + right%sca%i32
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 + right%sca%i32
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 + right%sca%i32
		case (f64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = left%array%f64 + right%sca%i32
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
			res%array%i64 = left%array%i32 + right%sca%i64
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 + right%sca%i64
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 + real(right%sca%i64)
		case (f64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = left%array%f64 + real(right%sca%i64, 8)
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
			res%array%i32 = left%sca%i32 + right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i32 + right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i32 + right%array%f32
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%sca%i32 + right%array%f64
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
			res%array%i64 = left%sca%i64 + right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 + right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = real(left%sca%i64) + right%array%f32
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = real(left%sca%i64, 8) + right%array%f64
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f32_type + array_type)
		!print *, 'f32_type + array_type'

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 + right%array%f32
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 + right%array%i32
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 + real(right%array%i64)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f64_type + array_type)
		!print *, 'f64_type + array_type'

		select case (right%array%type)
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%sca%f64 + right%array%f64
		case (i32_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%sca%f64 + right%array%i32
		case (i64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%sca%f64 + real(right%array%i64, 8)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%array%i32 + right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 + right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 + right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i32 + right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 + right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = real(left%array%i64) + right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 + real(right%array%i64)

		case (magic * f64_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%array%f64 + right%array%f64

		case (magic * i32_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%array%i32 + right%array%f64

		case (magic * f64_type + i32_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%array%f64 + right%array%i32

		case (magic * i64_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = real(left%array%i64, 8) + right%array%f64

		case (magic * f64_type + i64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = left%array%f64 + real(right%array%i64, 8)

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 + right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 + right%array%i32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 + right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 + right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 + right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 + right%sca%i64

	case        (magic**2 * i32_type + magic * i32_type + i64_type)
		res%sca%i32 = left%sca%i32 + right%sca%i64

	! TODO: i64/f32 casting

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 + right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 + right%sca%f32)

	case        (magic**2 * i32_type + magic * f64_type + i32_type)
		res%sca%i32 = int(left%sca%f64 + right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f64_type)
		res%sca%i32 = int(left%sca%i32 + right%sca%f64)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 + right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 + right%sca%i32

	case        (magic**2 * f32_type + magic * f32_type + i64_type)
		res%sca%f32 = left%sca%f32 + real(right%sca%i64)

	case        (magic**2 * f32_type + magic * i64_type + f32_type)
		res%sca%f32 = real(left%sca%i64) + right%sca%f32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 + right%sca%f32

	!****
	case        (magic**2 * f64_type + magic * f64_type + f64_type)
		res%sca%f64 = left%sca%f64 + right%sca%f64

	case        (magic**2 * f64_type + magic * f64_type + i32_type)
		res%sca%f64 = left%sca%f64 + right%sca%i32

	case        (magic**2 * f64_type + magic * f64_type + i64_type)
		res%sca%f64 = left%sca%f64 + real(right%sca%i64, 8)

	case        (magic**2 * f64_type + magic * i64_type + f64_type)
		res%sca%f64 = real(left%sca%i64, 8) + right%sca%f64

	case        (magic**2 * f64_type + magic * i32_type + f64_type)
		res%sca%f64 = left%sca%i32 + right%sca%f64

	!****
	! Mixed f32/f64 cases
	case        (magic**2 * f64_type + magic * f64_type + f32_type)
		res%sca%f64 = left%sca%f64 + right%sca%f32
	case        (magic**2 * f64_type + magic * f32_type + f64_type)
		res%sca%f64 = left%sca%f32 + right%sca%f64

	! TODO: array cases above too

	!! TODO?
	!case        (magic**2 * f32_type + magic * f64_type + f32_type)
	!	res%sca%f32 = left%sca%f64 + right%sca%f32
	!case        (magic**2 * f32_type + magic * f32_type + f64_type)
	!	res%sca%f32 = left%sca%f32 + right%sca%f64

	!****


	case        (magic**2 * str_type + magic * str_type + str_type)
		res%sca%str%s = left%sca%str%s // right%sca%str%s


	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine add_value_t

!===============================================================================

end module syntran__math_add_m

!===============================================================================

