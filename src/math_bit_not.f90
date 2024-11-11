
!===============================================================================

module syntran__math_bit_not_m

	use syntran__value_m

	implicit none

	interface bit_not
		module procedure bit_not_value_t
	end interface bit_not

!===============================================================================

contains

!===============================================================================

subroutine bit_not_value_t(right, res, op_text)

	! Unary `-` operator

	type(value_t), intent(in)  :: right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	select case (right%type)

	case (i32_type)
		res%sca%i32 = not(right%sca%i32)
	case (i64_type)
		res%sca%i64 = not(right%sca%i64)

	case        (array_type)

		select case (right%array%type)

		case (i32_type)
			res%array = mold(right%array, i32_type)
			res%array%i32 = not(right%array%i32)
		case (i64_type)
			res%array = mold(right%array, i64_type)
			res%array%i64 = not(right%array%i64)

		case default
			write(*,*) err_eval_unary_type(op_text)
			call internal_error()
		end select

	case default
		write(*,*) err_eval_unary_type(op_text)
		call internal_error()
	end select

end subroutine bit_not_value_t

!===============================================================================

end module syntran__math_bit_not_m

!===============================================================================

