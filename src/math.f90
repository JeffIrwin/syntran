
!===============================================================================

module syntran__math_m

	use syntran__value_m

	implicit none

	interface add
		module procedure add_value_t
	end interface add

	interface subtract
		module procedure subtract_value_t
	end interface subtract

	interface mul
		module procedure mul_value_t
	end interface mul

	interface div
		module procedure div_value_t
	end interface div

	interface pow
		module procedure pow_value_t
	end interface pow

	interface mod_
		module procedure modulo_value_t
	end interface mod_

!===============================================================================

contains

!===============================================================================

subroutine add_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!********

	!print *, 'starting add_value_t()'
	!print *, 'res  %type = ', kind_name(res%type)
	!print *, 'left %type = ', kind_name(left%type)
	!print *, 'right%type = ', kind_name(right%type)

	! Case selector must be a scalar expression, so use this nasty hack.
	! This will break if magic is smaller than the largest type enum
	! parameter
	select case (magic**2 * res%type + magic * left%type + right%type)

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
			res%array%f32 = left%array%i64 + right%sca%f32
		case default
			! FIXME: other numeric types (f64, etc.)
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
		case default
			! FIXME: other numeric types (f64, etc.)
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
			res%array%f32 = left%array%f32 + right%sca%i64
		case default
			! FIXME: other numeric types (f64, etc.)
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
		case default
			! FIXME: other numeric types (f64, etc.)
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
			res%array%f32 = left%sca%i64 + right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
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
			res%array%f32 = left%sca%f32 + right%array%i64
		case default
			! FIXME: other numeric types (f64, etc.)
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
			res%array%f32 = left%array%i64 + right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 + right%array%i64

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 + right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 + right%array%i32

		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		!print *, 'scalar i32 addition'
		res%sca%i32 = left%sca%i32 + right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 + right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 + right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 + right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	! Usually, adding f32 to i32 casts to an i32 result.  But for compound
	! assignment we may want to make it an i32, e.g. i += 3.1;
	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 + right%sca%i32)
	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 + right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 + right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 + right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 + right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i64_type)
		res%sca%f32 = left%sca%f32 + real(right%sca%i64)

	case        (magic**2 * f32_type + magic * i64_type + f32_type)
		res%sca%f32 = real(left%sca%i64) + right%sca%f32

	case        (magic**2 * str_type + magic * str_type + str_type)
		res%sca%str%s = left%sca%str%s // right%sca%str%s

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()

	end select
	!print *, 'done add_value_t()'

end subroutine add_value_t

!===============================================================================

subroutine mul_value_t(left, right, res, op_text)

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
			res%array%f32 = left%array%f32 * right%sca%f32
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i32 * right%sca%f32
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i64 * right%sca%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = left%array%i32 * right%sca%i32
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 * right%sca%i32
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 * right%sca%i32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i32 * right%sca%i64
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 * right%sca%i64
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 * right%sca%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i32_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%sca%i32 * right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i32 * right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i32 * right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 * right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 * right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i64 * right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f32_type + array_type)
		!print *, 'f32_type + array_type'

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 * right%array%f32
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 * right%array%i32
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 * right%array%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%array%i32 * right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 * right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 * right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i32 * right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 * right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i64 * right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 * right%array%i64

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 * right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 * right%array%i32

		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 * right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 * right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 * right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 * right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 * right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 * right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 * right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 * right%sca%i32

	case        (magic**2 * f32_type + magic * f32_type + i64_type)
		res%sca%f32 = left%sca%f32 * real(right%sca%i64)

	case        (magic**2 * f32_type + magic * i64_type + f32_type)
		res%sca%f32 = real(left%sca%i64) * right%sca%f32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 * right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine mul_value_t

!===============================================================================

subroutine div_value_t(left, right, res, op_text)

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
			res%array%f32 = left%array%f32 / right%sca%f32
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i32 / right%sca%f32
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i64 / right%sca%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = left%array%i32 / right%sca%i32
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 / right%sca%i32
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 / right%sca%i32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i32 / right%sca%i64
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 / right%sca%i64
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 / right%sca%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i32_type + array_type)
		!print *, 'i32_type + array_type'

		! Divide scalars by arrays:  `12 / [3, 4, 6] == [4, 3, 2]`.  This might
		! not make a lot of sense mathematically, but Fortran and numpy also
		! allow it

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%sca%i32 / right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i32 / right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i32 / right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 / right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 / right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i64 / right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f32_type + array_type)
		!print *, 'f32_type + array_type'

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 / right%array%f32
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 / right%array%i32
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 / right%array%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%array%i32 / right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 / right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 / right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i32 / right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 / right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i64 / right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 / right%array%i64

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 / right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 / right%array%i32

		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 / right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 / right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 / right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 / right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 / right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 / right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 / right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 / right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 / right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine div_value_t

!===============================================================================

subroutine subtract_value_t(left, right, res, op_text)

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
			res%array%f32 = left%array%f32 - right%sca%f32
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i32 - right%sca%f32
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i64 - right%sca%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = left%array%i32 - right%sca%i32
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 - right%sca%i32
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 - right%sca%i32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i32 - right%sca%i64
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 - right%sca%i64
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 - right%sca%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i32_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%sca%i32 - right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i32 - right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i32 - right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 - right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 - right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i64 - right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f32_type + array_type)
		!print *, 'f32_type + array_type'

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 - right%array%f32
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 - right%array%i32
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 - right%array%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%array%i32 - right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 - right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 - right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i32 - right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 - right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i64 - right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 - right%array%i64

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 - right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 - right%array%i32

		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 - right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 - right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 - right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 - right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 - right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 - right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 - right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 - right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 - right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine subtract_value_t

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
			! FIXME: other numeric types (f64, etc.)
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
			res%array%i64 = mod(left%array%i64, right%sca%i32)
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%sca%i32))
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = mod(left%array%i32, right%sca%i64)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = mod(left%array%i64, right%sca%i64)
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = mod(left%array%f32, real(right%sca%i64, 4))
		case default
			! FIXME: other numeric types (f64, etc.)
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
			res%array%i64 = mod(left%sca%i32, right%array%i64)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%sca%i32), right%array%f32)
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%sca%i64, right%array%i32)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%sca%i64, right%array%i64)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = mod(real(left%sca%i64, 4), right%array%f32)
		case default
			! FIXME: other numeric types (f64, etc.)
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
			! FIXME: other numeric types (f64, etc.)
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

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%array%i32, right%array%i64)

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = mod(left%array%i64, right%array%i32)

		case default
			! FIXME: other numeric types (f64, etc.)
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

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = mod(int(left%sca%f32), right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = mod(left%sca%i32, int(right%sca%f32))

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = mod(left%sca%f32, right%sca%f32)

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = mod(left%sca%f32, real(right%sca%i32))

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = mod(real(left%sca%i32), right%sca%f32)

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine modulo_value_t

!===============================================================================

subroutine pow_value_t(left, right, res, op_text)

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
			res%array%f32 = left%array%f32 ** right%sca%f32
		case (i32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i32 ** right%sca%f32
		case (i64_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%i64 ** right%sca%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i32_type)
		!print *, 'array_type + i32_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i32_type)
			res%array%i32 = left%array%i32 ** right%sca%i32
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 ** right%sca%i32
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 ** right%sca%i32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i32 ** right%sca%i64
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = left%array%i64 ** right%sca%i64
		case (f32_type)
			res%array = mold(left%array, f32_type)
			res%array%f32 = left%array%f32 ** right%sca%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i32_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%sca%i32 ** right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i32 ** right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i32 ** right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)
		!print *, 'i32_type + array_type'

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 ** right%array%i32
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%sca%i64 ** right%array%i64
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%i64 ** right%array%f32
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * f32_type + array_type)
		!print *, 'f32_type + array_type'

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 ** right%array%f32
		case (i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 ** right%array%i32
		case (i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%sca%f32 ** right%array%i64
		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = left%array%i32 ** right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 ** right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 ** right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i32 ** right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 ** right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%i64 ** right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, f32_type)
			res%array%f32 = left%array%f32 ** right%array%i64

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i32 ** right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = left%array%i64 ** right%array%i32

		case default
			! FIXME: other numeric types (f64, etc.)
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 ** right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 ** right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 ** right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 ** right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 ** right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 ** right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 ** right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 ** right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 ** right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine pow_value_t

!===============================================================================

end module syntran__math_m

!===============================================================================

