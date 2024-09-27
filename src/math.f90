
!===============================================================================

module syntran__math_m

	use syntran__value_m

	use syntran__math_add_m
	use syntran__math_subtract_m
	use syntran__math_mul_m
	use syntran__math_div_m
	use syntran__math_pow_m

	implicit none

	interface assign_
		module procedure assign_value_t
	end interface assign_

	interface mod_
		module procedure modulo_value_t
	end interface mod_

	interface negate
		module procedure negate_value_t
	end interface negate

!===============================================================================

contains

!===============================================================================

subroutine assign_value_t(left, right, op_text)

	! This is arguably not math

	type(value_t), intent(in)  :: right

	! The type is set before calling this routine, so left is inout
	type(value_t), intent(inout) :: left

	character(len = *), intent(in) :: op_text

	!********

	!print *, 'starting assign_value_t()'
	!print *, 'left %type = ', kind_name(left%type)
	!print *, 'right%type = ', kind_name(right%type)

	if (right%type == array_type) then
		! TODO: check for subtle bugs where this might cast an f32 array to an
		! f64 array (or i32 to/from i64)
		left = right  ! simply overwrite, for any type
		return
	end if

	if (left%type /= array_type) then

		! For numeric types, cast the rhs if necessary without changing the type
		! of the lhs.  Bools and strs are straight forward

		select case (left%type)
		case (bool_type)
			left%sca%bool = right%sca%bool

		case (f32_type)
			left%sca%f32 = right%to_f32()
		case (f64_type)
			left%sca%f64 = right%to_f64()

		case (file_type)
			left = right

		case (i32_type)
			left%sca%i32 = right%to_i32()
		case (i64_type)
			left%sca%i64 = right%to_i64()

		case (str_type)
			left%sca%str = right%sca%str

		case (struct_type)
			left = right

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

		return
	end if

	! TODO: looks like this fails on arrays of structs
	select case (left%array%type)
	case (bool_type)
		left%array%bool = right%sca%bool
	case (f32_type)
		left%array%f32 = right%sca%f32
	case (f64_type)
		left%array%f64 = right%sca%f64
	case (i32_type)
		left%array%i32 = right%sca%i32
	case (i64_type)
		left%array%i64 = right%sca%i64
	case (str_type)
		left%array%str = right%sca%str
	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine assign_value_t

!===============================================================================

subroutine modulo_value_t(left, right, res, op_text)

	! The Fortran mod() fn is consistent with the C operator `%`, while modulo()
	! works differently for negative args (it's actually a remainder function,
	! not a true modulo)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * array_type + magic * array_type + f32_type)
		!print *, 'array_type + f32_type'

		select case (left%array%type)
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(left%array%f32, right%sca%f32)
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(real(left%array%i32), right%sca%f32)
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(real(left%array%i64, 4), right%sca%f32)
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
			res%array%f64 = mod(left%array%f64, right%sca%f64)
		case (i32_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = mod(real(left%array%i32, 8), right%sca%f64)
		case (i64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = mod(real(left%array%i64, 8), right%sca%f64)
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
			res%array%i32 = mod(left%array%i32, right%sca%i32)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = mod(left%array%i64, int(right%sca%i32, 8))
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%sca%i32))
		case (f64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = mod(left%array%f64, real(right%sca%i32, 8))
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
			res%array%i64 = mod(int(left%array%i32, 8), right%sca%i64)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = mod(left%array%i64, right%sca%i64)
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%sca%i64, 4))
		case (f64_type)
			res%array = mold(left%array, f64_type)
			res%array%f64 = mod(left%array%f64, real(right%sca%i64, 8))
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
			res%array%i32 = mod(left%sca%i32, right%array%i32)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(int(left%sca%i32, 8), right%array%i64)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%sca%i32), right%array%f32)
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(real(left%sca%i32, 8), right%array%f64)
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
			res%array%i64 = mod(left%sca%i64, int(right%array%i32, 8))
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%sca%i64, right%array%i64)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%sca%i64, 4), right%array%f32)
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(real(left%sca%i64, 8), right%array%f64)
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
			res%array%f32 = mod(left%sca%f32, right%array%f32)
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(left%sca%f32, real(right%array%i32))
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(left%sca%f32, real(right%array%i64, 4))
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
			res%array%f64 = mod(left%sca%f64, right%array%f64)
		case (i32_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(left%sca%f64, real(right%array%i32, 8))
		case (i64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(left%sca%f64, real(right%array%i64, 8))
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = mod(left%array%i32, right%array%i32)

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%array%i64, right%array%i64)

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(left%array%f32, right%array%f32)

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%array%i32), right%array%f32)

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%array%i32))

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%array%i64, 4), right%array%f32)

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%array%i64, 4))

		case (magic * f64_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(left%array%f64, right%array%f64)

		case (magic * i32_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(real(left%array%i32, 8), right%array%f64)

		case (magic * f64_type + i32_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(left%array%f64, real(right%array%i32, 8))

		case (magic * i64_type + f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(real(left%array%i64, 8), right%array%f64)

		case (magic * f64_type + i64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = mod(left%array%f64, real(right%array%i64, 8))

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(int(left%array%i32, 8), right%array%i64)

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%array%i64, int(right%array%i32, 8))

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = mod(left%sca%i32, right%sca%i32)

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = mod(left%sca%i64, right%sca%i64)

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = mod(left%sca%i64, int(right%sca%i32, 8))

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = mod(int(left%sca%i32, 8), right%sca%i64)

	case        (magic**2 * i32_type + magic * i32_type + i64_type)
		res%sca%i32 = mod(int(left%sca%i32, 8), right%sca%i64)

	! TODO: i64/f32 casting

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = mod(int(left%sca%f32), right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = mod(left%sca%i32, int(right%sca%f32))

	case        (magic**2 * i32_type + magic * f64_type + i32_type)
		res%sca%i32 = mod(int(left%sca%f64), right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f64_type)
		res%sca%i32 = mod(left%sca%i32, int(right%sca%f64))

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = mod(left%sca%f32, right%sca%f32)

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = mod(left%sca%f32, real(right%sca%i32))

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = mod(real(left%sca%i32), right%sca%f32)

	!****
	case        (magic**2 * f64_type + magic * f64_type + f64_type)
		res%sca%f64 = mod(left%sca%f64, right%sca%f64)

	case        (magic**2 * f64_type + magic * f64_type + i32_type)
		res%sca%f64 = mod(left%sca%f64, real(right%sca%i32, 8))

	case        (magic**2 * f64_type + magic * i32_type + f64_type)
		res%sca%f64 = mod(real(left%sca%i32, 8), right%sca%f64)

	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine modulo_value_t

!===============================================================================

subroutine negate_value_t(right, res, op_text)

	! Unary `-` operator

	type(value_t), intent(in)  :: right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	select case (right%type)

	case (i32_type)
		res%sca%i32 = -right%sca%i32
	case (i64_type)
		res%sca%i64 = -right%sca%i64
	case (f32_type)
		res%sca%f32 = -right%sca%f32
	case (f64_type)
		res%sca%f64 = -right%sca%f64

	case        (array_type)

		select case (right%array%type)

		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = -right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = -right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = -right%array%f32
		case (f64_type)
			res%array = mold(right%array, f64_type)
			res%array%f64 = -right%array%f64

		case default
			write(*,*) err_eval_unary_type(op_text)
			call internal_error()
		end select

	case default
		write(*,*) err_eval_unary_type(op_text)
		call internal_error()
	end select

end subroutine negate_value_t

!===============================================================================

end module syntran__math_m

!===============================================================================

