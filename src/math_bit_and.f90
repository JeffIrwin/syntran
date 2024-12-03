
!===============================================================================

module syntran__math_bit_and_m

	use syntran__value_m

	implicit none

	interface bit_and
		module procedure bit_and_value_t
	end interface bit_and

!===============================================================================

contains

!===============================================================================

subroutine bit_and_value_t(left, right, res, op_text)

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
			res%array%i32 = iand(left%array%i32, right%sca%i32)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + i64_type)
		!print *, 'array_type + i64_type'

		select case (left%array%type)
		case (i64_type)
			res%array = mold(left%array, i64_type)
			res%array%i64 = iand(left%array%i64, right%sca%i64)
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
			res%array%i32 = iand(left%sca%i32, right%array%i32)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * i64_type + array_type)

		select case (right%array%type)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = iand(left%sca%i64, right%array%i64)
		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	!****
	case        (magic**2 * array_type + magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = iand(left%array%i32, right%array%i32)

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = iand(left%array%i64, right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()

		end select

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = iand(left%sca%i32, right%sca%i32)

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = iand(left%sca%i64, right%sca%i64)

	!****
	case default
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine bit_and_value_t

!===============================================================================

end module syntran__math_bit_and_m

!===============================================================================

