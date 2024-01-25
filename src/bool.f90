
!===============================================================================

module syntran__bool_m

	use syntran__value_m

	implicit none

	interface is_eq
		module procedure is_eq_value_t
	end interface is_eq

	interface is_ne
		module procedure is_ne_value_t
	end interface is_ne

	interface is_lt
		module procedure is_lt_value_t
	end interface is_lt

	interface is_ge
		module procedure is_ge_value_t
	end interface is_ge

	interface is_le
		module procedure is_le_value_t
	end interface is_le

	interface is_gt
		module procedure is_gt_value_t
	end interface is_gt

!===============================================================================

contains

!===============================================================================

subroutine is_eq_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	integer(kind = 8) :: i8

	select case (magic * left%type + right%type)
	case        (magic * i32_type + i32_type)
		res%sca%bool = left%sca%i32 == right%sca%i32

	case        (magic * i64_type + i64_type)
		res%sca%bool = left%sca%i64 == right%sca%i64

	case        (magic * i32_type + i64_type)
		res%sca%bool = left%sca%i32 == right%sca%i64

	case        (magic * i64_type + i32_type)
		res%sca%bool = left%sca%i64 == right%sca%i32

	case        (magic * f32_type + f32_type)
		res%sca%bool = left%sca%f32 == right%sca%f32

	case        (magic * f32_type + i32_type)
		res%sca%bool = left%sca%f32 == right%sca%i32
		! TODO: is this even possible or should I ban comparing ints and
		! floats?  Similarly for other comparisons
		!
		! GNU says Warning: Equality comparison for REAL(4) at (1)
		! [-Wcompare-reals]

	case        (magic * f32_type + i64_type)
		res%sca%bool = left%sca%f32 == real(right%sca%i64)

	case        (magic * i32_type + f32_type)
		res%sca%bool = left%sca%i32 == right%sca%f32

	case        (magic * i64_type + f32_type)
		res%sca%bool = real(left%sca%i64) == right%sca%f32

	case        (magic * bool_type + bool_type)
		res%sca%bool = left%sca%bool .eqv. right%sca%bool
	case        (magic * str_type + str_type)
		res%sca%bool = left%sca%str%s == right%sca%str%s

	case        (magic * array_type + i32_type)

		!print *, 'left%type       = ', kind_name(left%type)
		!print *, 'left array type = ', kind_name(left%array%type)

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 == right%sca%i32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 == right%sca%i32

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 == right%sca%i32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + i64_type)

		select case (left%array%type)
		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 == right%sca%i64

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 == right%sca%i64

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 == real(right%sca%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + f32_type)

		select case (left%array%type)
		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 == right%sca%f32

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 == right%sca%f32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = real(left%array%i64) == right%sca%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + bool_type)

		select case (left%array%type)
		case (bool_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%bool .eqv. right%sca%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + str_type)

		select case (left%array%type)
		case (str_type)
			res%array = mold(left%array, bool_type)

			!! Fortran is weird about string arrays
			!res%array%bool = left%array%str%s == right%sca%str%s
			allocate(res%array%bool( res%array%len_ ))
			do i8 = 1, res%array%len_
				res%array%bool(i8) = left%array%str(i8)%s == right%sca%str%s
			end do

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i32_type + array_type)

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 == right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 == right%array%i64

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 == right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i64_type + array_type)

		select case (right%array%type)
		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 == right%array%i64

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 == right%array%i32

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%sca%i64) == right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * f32_type + array_type)

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 == right%array%f32

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 == right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 == real(right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * bool_type + array_type)

		select case (right%array%type)
		case (bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%bool .eqv. right%array%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * str_type + array_type)

		select case (right%array%type)
		case (str_type)
			res%array = mold(right%array, bool_type)

			! Fortran is weird about string arrays
			allocate(res%array%bool( res%array%len_ ))
			do i8 = 1, res%array%len_
				res%array%bool(i8) = left%sca%str%s == right%array%str(i8)%s
			end do

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + array_type)

		!print *, 'array == array'

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 == right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 == right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 == right%array%i32

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 == right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 == right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 == right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 == right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%array%i64) == right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 == real(right%array%i64)

		case (magic * bool_type + bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%bool .eqv. right%array%bool

		case (magic * str_type + str_type)
			res%array = mold(right%array, bool_type)

			! Fortran is weird about string arrays
			allocate(res%array%bool( res%array%len_ ))
			do i8 = 1, res%array%len_
				res%array%bool(i8) = left%array%str(i8)%s == right%array%str(i8)%s
			end do

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine is_eq_value_t

!===============================================================================

subroutine is_ne_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	! It seems inefficient to invert a whole array, but is_eq() is 300
	! LOC and I'm eager to make this tradeoff
	call is_eq(left, right, res, op_text)
	if (res%type == array_type) then
		res%array%bool = .not. res%array%bool
	else
		res%sca%bool = .not. res%sca%bool
	end if

end subroutine is_ne_value_t

!===============================================================================

subroutine is_lt_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	integer(kind = 8) :: i8

	select case (magic * left%type + right%type)
	case        (magic * i32_type + i32_type)
		res%sca%bool = left%sca%i32 < right%sca%i32

	case        (magic * i64_type + i64_type)
		res%sca%bool = left%sca%i64 < right%sca%i64

	case        (magic * i32_type + i64_type)
		res%sca%bool = left%sca%i32 < right%sca%i64

	case        (magic * i64_type + i32_type)
		res%sca%bool = left%sca%i64 < right%sca%i32

	case        (magic * f32_type + f32_type)
		res%sca%bool = left%sca%f32 < right%sca%f32

	case        (magic * f32_type + i32_type)
		res%sca%bool = left%sca%f32 < right%sca%i32

	case        (magic * f32_type + i64_type)
		res%sca%bool = left%sca%f32 < real(right%sca%i64)

	case        (magic * i32_type + f32_type)
		res%sca%bool = left%sca%i32 < right%sca%f32

	case        (magic * i64_type + f32_type)
		res%sca%bool = real(left%sca%i64) < right%sca%f32

	case        (magic * array_type + i32_type)

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 < right%sca%i32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 < right%sca%i32

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 < right%sca%i32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + i64_type)

		select case (left%array%type)
		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 < right%sca%i64

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 < right%sca%i64

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 < real(right%sca%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + f32_type)

		select case (left%array%type)
		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 < right%sca%f32

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 < right%sca%f32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = real(left%array%i64) < right%sca%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i32_type + array_type)

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 < right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 < right%array%i64

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 < right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i64_type + array_type)

		select case (right%array%type)
		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 < right%array%i64

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 < right%array%i32

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%sca%i64) < right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * f32_type + array_type)

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 < right%array%f32

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 < right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 < real(right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 < right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 < right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 < right%array%i32

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 < right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 < right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 < right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 < right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%array%i64) < right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 < real(right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine is_lt_value_t

!===============================================================================

subroutine is_ge_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	! Efficiency vs LOC tradeoff again
	!
	! Actually, all other comparison operations could be implemented in terms of
	! just is_lt() and is_gt().  For example, `is_eq(x,y) = is_lt(x,y) .or.
	! is_gt(x,y)`, etc., but that means some comparisons would require *two* base
	! comparisons, and I'm not going to sacrifice that much efficiency.  A
	! simple inversion seems more worthwhile

	call is_lt(left, right, res, op_text)
	if (res%type == array_type) then
		res%array%bool = .not. res%array%bool
	else
		res%sca%bool = .not. res%sca%bool
	end if

end subroutine is_ge_value_t

!===============================================================================

subroutine is_le_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	integer(kind = 8) :: i8

	select case (magic * left%type + right%type)
	case        (magic * i32_type + i32_type)
		res%sca%bool = left%sca%i32 <= right%sca%i32

	case        (magic * i64_type + i64_type)
		res%sca%bool = left%sca%i64 <= right%sca%i64

	case        (magic * i32_type + i64_type)
		res%sca%bool = left%sca%i32 <= right%sca%i64

	case        (magic * i64_type + i32_type)
		res%sca%bool = left%sca%i64 <= right%sca%i32

	case        (magic * f32_type + f32_type)
		res%sca%bool = left%sca%f32 <= right%sca%f32

	case        (magic * f32_type + i32_type)
		res%sca%bool = left%sca%f32 <= right%sca%i32

	case        (magic * f32_type + i64_type)
		res%sca%bool = left%sca%f32 <= real(right%sca%i64)

	case        (magic * i32_type + f32_type)
		res%sca%bool = left%sca%i32 <= right%sca%f32

	case        (magic * i64_type + f32_type)
		res%sca%bool = real(left%sca%i64) <= right%sca%f32

	case        (magic * array_type + i32_type)

		select case (left%array%type)
		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 <= right%sca%i32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 <= right%sca%i32

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 <= right%sca%i32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + i64_type)

		select case (left%array%type)
		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i64 <= right%sca%i64

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 <= right%sca%i64

		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 <= real(right%sca%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + f32_type)

		select case (left%array%type)
		case (f32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%f32 <= right%sca%f32

		case (i32_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%i32 <= right%sca%f32

		case (i64_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = real(left%array%i64) <= right%sca%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i32_type + array_type)

		select case (right%array%type)
		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 <= right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 <= right%array%i64

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i32 <= right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * i64_type + array_type)

		select case (right%array%type)
		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 <= right%array%i64

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%i64 <= right%array%i32

		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%sca%i64) <= right%array%f32

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * f32_type + array_type)

		select case (right%array%type)
		case (f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 <= right%array%f32

		case (i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 <= right%array%i32

		case (i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%f32 <= real(right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * i32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 <= right%array%i32

		case (magic * i64_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 <= right%array%i64

		case (magic * i64_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i64 <= right%array%i32

		case (magic * i32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 <= right%array%i64

		case (magic * f32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 <= right%array%f32

		case (magic * i32_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%i32 <= right%array%f32

		case (magic * f32_type + i32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 <= right%array%i32

		case (magic * i64_type + f32_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = real(left%array%i64) <= right%array%f32

		case (magic * f32_type + i64_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%f32 <= real(right%array%i64)

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine is_le_value_t

!===============================================================================

subroutine is_gt_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	! Efficiency vs LOC tradeoff again
	call is_le(left, right, res, op_text)
	if (res%type == array_type) then
		res%array%bool = .not. res%array%bool
	else
		res%sca%bool = .not. res%sca%bool
	end if

end subroutine is_gt_value_t

!===============================================================================

subroutine and_(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	integer(kind = 8) :: i8

	select case (magic * left%type + right%type)
	case        (magic * bool_type + bool_type)
		res%sca%bool = left%sca%bool .and. right%sca%bool

	case        (magic * array_type + bool_type)

		select case (left%array%type)
		case (bool_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%bool .and. right%sca%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * bool_type + array_type)

		select case (right%array%type)
		case (bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%bool .and. right%array%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * bool_type + bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%bool .and. right%array%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine and_

!===============================================================================

subroutine or_(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!****

	integer(kind = 8) :: i8

	select case (magic * left%type + right%type)
	case        (magic * bool_type + bool_type)
		res%sca%bool = left%sca%bool .or. right%sca%bool

	case        (magic * array_type + bool_type)

		select case (left%array%type)
		case (bool_type)
			res%array = mold(left%array, bool_type)
			res%array%bool = left%array%bool .or. right%sca%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * bool_type + array_type)

		select case (right%array%type)
		case (bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%sca%bool .or. right%array%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case        (magic * array_type + array_type)

		select case (magic * left%array%type + right%array%type)
		case (magic * bool_type + bool_type)
			res%array = mold(right%array, bool_type)
			res%array%bool = left%array%bool .or. right%array%bool

		case default
			write(*,*) err_eval_binary_types(op_text)
			call internal_error()
		end select

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine or_

!===============================================================================

end module syntran__bool_m

!===============================================================================

