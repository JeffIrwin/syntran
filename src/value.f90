
!===============================================================================

module syntran__value_m

	use syntran__consts_m
	use syntran__errors_m
	!use syntran__types_m
	!use syntran__utils_m

	implicit none

	!********

	type file_t
		character(len = :), allocatable :: name_
		integer :: unit_  ! fortran file unit
		logical :: eof = .false.
		! Do we need a separate iostat beyond eof?
	end type file_t

	!********

	type scalar_t

		! Scalar value type.  Cannot be an array!

		type(file_t)      :: file_
		type(string_t)    :: str

		logical           :: bool
		integer(kind = 4) :: i32
		integer(kind = 8) :: i64
		real   (kind = 4) :: f32

		contains
			procedure :: to_str => scalar_to_str

	end type scalar_t

	!********

	type array_t

		! The array type is i32_type, f32_type, etc. while the kind is
		! impl_array (bound-based) or expl_array (CSV list)
		integer :: type, kind
		!type(value_t), allocatable :: lbound, step, ubound
		type(scalar_t), allocatable :: lbound, step, ubound

		! Note that these are arrays of primitive Fortran types, instead of
		! arrays of generic value_t.  This performs better since we can put
		! a type select/case outside of loops for processing arrays, as opposed
		! to inside of a loop for type selection of every element
		logical(kind = 1), allocatable :: bool(:)

		integer(kind = 4), allocatable ::  i32(:)
		integer(kind = 8), allocatable ::  i64(:)

		real   (kind = 4), allocatable ::  f32(:)

		type(string_t   ), allocatable ::  str(:)

		! TODO: file arrays

		integer :: rank
		integer(kind = 8) :: len_, cap
		integer(kind = 8), allocatable :: size(:)

		contains
			procedure :: push => push_array

	end type array_t

	type value_t
		integer :: type

		type(scalar_t) :: sca

		! Back when array_t could contain value_t's, gfortran would use up infinite
		! RAM trying to parse the circular type dependencies unless this was a
		! pointer.  But pointers lead to nasty memory leaks (e.g. aoc 2023 day
		! 07)
		!
		! Now arrays can contain a scalar_t instead of a value_t, so there
		! are no longer any circular type dependencies
		!
		! Note that a type containing itself is fine (e.g. ternary_tree_node_t),
		! but two types containing each other is bad
		type(array_t), allocatable :: array

		contains
			procedure :: to_str => value_to_str
			procedure :: to_f32 => value_to_f32
			procedure :: to_i32 => value_to_i32
			procedure :: to_i64 => value_to_i64

	end type value_t

	!********

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

	interface is_eq
		module procedure is_eq_value_t
	end interface is_eq

	interface is_ne
		module procedure is_ne_value_t
	end interface is_ne

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
	!print *, 'res%type = ', kind_name(res%type)

	! Case selector must be a scalar expression, so use this nasty hack.
	! This will break if magic is smaller than the largest type enum
	! parameter
	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
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

end subroutine add_value_t

!===============================================================================

subroutine mul_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

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

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	! The Fortran mod() fn is consistent with the C operator `%`, while modulo()
	! works differently for negative args (it's actually a remainder function,
	! not a true modulo)

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

function mold(mold_, type_) result(array)

	! Construct array meta-data, such as type, rank, and size, based on a given
	! mold
	!
	! The actual allocation of array%i32 or array%bool (appropriately depending
	! on the type) and setting of its values is done outside of here in the
	! calling fn

	type(array_t), intent(in) :: mold_
	!type(value_t), intent(in) :: mold_

	integer, intent(in) :: type_

	type(array_t), allocatable :: array

	allocate(array)

	array%type = type_

	!array%kind = expl_array
	array%rank = mold_%rank

	array%len_ = mold_%len_
	array%cap  = mold_%cap
	array%size = mold_%size

end function mold

!===============================================================================

subroutine push_array(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(array_t) :: vector
	type(value_t)  :: val

	!********

	integer(kind = 4), allocatable :: tmp_i32 (:)
	integer(kind = 8), allocatable :: tmp_i64 (:)

	real   (kind = 4), allocatable :: tmp_f32 (:)

	logical(kind = 1), allocatable :: tmp_bool(:)

	type(string_t   ), allocatable :: tmp_str (:)

	integer(kind = 8) :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_

		if (vector%type == i32_type) then

			allocate(tmp_i32 ( tmp_cap ))
			tmp_i32(1: vector%cap) = vector%i32
			call move_alloc(tmp_i32, vector%i32)

		else if (vector%type == i64_type) then

			allocate(tmp_i64 ( tmp_cap ))
			tmp_i64(1: vector%cap) = vector%i64
			call move_alloc(tmp_i64, vector%i64)

		else if (vector%type == f32_type) then

			allocate(tmp_f32 ( tmp_cap ))
			tmp_f32(1: vector%cap) = vector%f32
			call move_alloc(tmp_f32, vector%f32)

		else if (vector%type == bool_type) then

			allocate(tmp_bool( tmp_cap ))
			tmp_bool(1: vector%cap) = vector%bool
			call move_alloc(tmp_bool, vector%bool)

		else if (vector%type == str_type) then

			allocate(tmp_str ( tmp_cap ))
			tmp_str (1: vector%cap) = vector%str
			call move_alloc(tmp_str, vector%str)

		else
			! FIXME: when adding new types, implement it below too to set the
			! last val
			write(*,*) 'Error: push_array type not implemented'
			call internal_error()
		end if

		vector%cap = tmp_cap

	end if

	if      (vector%type == i32_type) then
		vector%i32 ( vector%len_ ) = val%sca%i32
	else if (vector%type == i64_type) then
		vector%i64 ( vector%len_ ) = val%sca%i64
	else if (vector%type == f32_type) then
		vector%f32 ( vector%len_ ) = val%sca%f32
	else if (vector%type == bool_type) then
		vector%bool( vector%len_ ) = val%sca%bool
	else if (vector%type == str_type) then
		vector%str ( vector%len_ ) = val%sca%str
	else
		write(*,*) 'Error: push_array type not implemented'
		call internal_error()
	end if

end subroutine push_array

!===============================================================================

function value_to_f32(val) result(ans)

	class(value_t) :: val

	real(kind = 4) :: ans

	select case (val%type)

		case (f32_type)
			ans = val%sca%f32

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = real(val%sca%i64)

		case (str_type)

			if (len(val%sca%str%s) == 1) then
				ans = iachar(val%sca%str%s)
			else
				write(*,*) err_int_prefix//'cannot convert from type `' &
					//kind_name(val%type)//'` to f32 '//color_reset
				call internal_error()
			end if

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to f32 '//color_reset
			call internal_error()

	end select

end function value_to_f32

!===============================================================================

function value_to_i32(val) result(ans)

	class(value_t) :: val

	integer(kind = 4) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 4)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = int(val%sca%i64, 4)

		case (str_type)

			if (len(val%sca%str%s) == 1) then
				ans = iachar(val%sca%str%s)
			else
				write(*,*) err_int_prefix//'cannot convert from type `' &
					//kind_name(val%type)//'` to i32 '//color_reset
				call internal_error()
			end if

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 '//color_reset
			call internal_error()

	end select

end function value_to_i32

!===============================================================================

function value_to_i64(val) result(ans)

	class(value_t) :: val

	integer(kind = 8) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 8)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			!write(buffer, '(i0)') val%sca%i64
			!ans = trim(buffer)
			ans = val%sca%i64

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i64 '//color_reset
			call internal_error()

	end select

end function value_to_i64

!===============================================================================

recursive function value_to_str(val) result(ans)

	class(value_t) :: val

	character(len = :), allocatable :: ans

	!********

	!character(len = 16) :: buf16

	integer :: j
	integer(kind = 8) :: i8, prod

	!type(string_vector_t) :: str_vec
	type(char_vector_t) :: str_vec

	select case (val%type)

		case (array_type)

			! This whole case could be an array_to_str() fn

			if (val%array%kind == impl_array) then
				ans = '['//val%array%lbound%to_str(val%array%type)//': ' &
				         //val%array%ubound%to_str(val%array%type)//']'
				return
			end if

			!print *, 'array type = ', val%array%type

			!! You would think that this would help
			!if (val%array%type == i32_type) then
			!	str_vec = new_char_vector( 12 * val%array%len_ )
			!else if (val%array%type == f32_type) then
			!	str_vec = new_char_vector( 16 * val%array%len_ )
			!end if
			str_vec = new_char_vector()

			call str_vec%push('[')
			if (val%array%rank > 1) call str_vec%push(line_feed)

			!! Debug w/o recursive io
			!call str_vec%push( kind_name(val%array%type) )
			!call str_vec%push(str(int(val%array%len_)))

			if (val%array%type == i32_type) then

				!! Recursive IO stalls execution
				!print *, 'size = ', val%array%size

				do i8 = 1, int(val%array%len_)

					call str_vec%push(str(val%array%i32(i8)))
					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == i64_type) then

				!! Recursive IO stalls execution
				!print *, 'size = ', val%array%size

				do i8 = 1, val%array%len_

					call str_vec%push(str(val%array%i64(i8)))
					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == f32_type) then

				do i8 = 1, val%array%len_

					!! Nice alignment, but breaks tests
					!write(buf16, '(es16.6)') val%array%f32(i8)
					!call str_vec%push(buf16)

					! Trimmed string (not aligned)
					call str_vec%push(str(val%array%f32(i8)))

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == bool_type) then

				do i8 = 1, val%array%len_

					call str_vec%push(str(val%array%bool(i8)))

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == str_type) then

				do i8 = 1, val%array%len_

					call str_vec%push(val%array%str(i8)%s)

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else
				write(*,*) 'Error: array ans conversion not implemented' &
					//' for this type'
				call internal_error()
			end if

			if (val%array%rank > 1) call str_vec%push(line_feed)
			call str_vec%push(']')

			ans = str_vec%v( 1: str_vec%len_ )

		case default
			ans = val%sca%to_str(val%type)

	end select

end function value_to_str

!===============================================================================

recursive function scalar_to_str(val, type) result(ans)

	class(scalar_t) :: val

	integer, intent(in) :: type

	character(len = :), allocatable :: ans

	!********

	character(len = 16) :: buf16
	character(len = 32) :: buffer

	select case (type)

		case (void_type)
			ans = ''

		case (bool_type)
			! TODO: use bool1_str() and other primitive converters
			if (val%bool) then
				ans = "true"
			else
				ans = "false"
			end if

		case (f32_type)
			write(buf16, '(es16.6)') val%f32
			!ans = trim(buf16)
			ans = buf16  ! no trim for alignment

		case (i32_type)
			write(buffer, '(i0)') val%i32
			ans = trim(buffer)

		case (i64_type)
			write(buffer, '(i0)') val%i64
			ans = trim(buffer)

		case (str_type)
			! TODO: wrap str in quotes for clarity, both scalars and str array
			! elements.  Update tests.
			ans = val%str%s

		case (file_type)
			ans = "{file_unit: "//str(val%file_%unit_)//", filename: """// &
				val%file_%name_//"""}"

		case default
			ans = err_prefix//"<invalid_value>"//color_reset

	end select

end function scalar_to_str

!===============================================================================

end module syntran__value_m

!===============================================================================

