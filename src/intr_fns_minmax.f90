
!===============================================================================

module syntran__intr_fns_minmax_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_minmax_fns(fns, id_index, fn_array)

	! Declare min/max intrinsic functions

	type(fns_t), intent(inout) :: fns
	integer, intent(inout) :: id_index
	type(fn_t), allocatable, intent(out) :: fn_array(:)

	!********

	type(fn_t) :: min_i32_fn, min_i64_fn, min_f32_fn, min_f64_fn, &
		max_i32_fn, max_i64_fn, max_f32_fn, max_f64_fn

	!********

	! We could make max() and min() work with just 1 argument too.  I'm not sure
	! why you would want to be able to take the max of 1 number, but it seems
	! like an arbitrary limitation.  Anyway we follow the Fortran convention
	! here

	! In Fortran, min() is polymorphic and variadic, but all args must be the
	! same type.  For example, min(1, 2) and min(1.1, 2.1) are allowed, but
	! min(1, 2.1) does not compile.  I think that's a reasonable restriction

	min_i32_fn%type%type = i32_type
	allocate(min_i32_fn%params(2))
	allocate(min_i32_fn%param_names%v(2))

	min_i32_fn%params(1)%type = i32_type
	min_i32_fn%param_names%v(1)%s = "a0"

	min_i32_fn%params(2)%type = i32_type
	min_i32_fn%param_names%v(2)%s = "a1"

	min_i32_fn%variadic_min  = 0
	min_i32_fn%variadic_type = i32_type
	min_i32_fn%variadic_name = "ai"

	! Internal overloaded name starts with a "0" because this would be illegal
	! for user-defined fn's, so there can never be a clash
	call fns%insert("0min_i32", min_i32_fn, id_index)

	!********

	min_i64_fn%type%type = i64_type
	allocate(min_i64_fn%params(2))
	allocate(min_i64_fn%param_names%v(2))

	min_i64_fn%params(1)%type = i64_type
	min_i64_fn%param_names%v(1)%s = "a0"

	min_i64_fn%params(2)%type = i64_type
	min_i64_fn%param_names%v(2)%s = "a1"

	min_i64_fn%variadic_min  = 0
	min_i64_fn%variadic_type = i64_type
	min_i64_fn%variadic_name = "ai"

	call fns%insert("0min_i64", min_i64_fn, id_index)

	!********

	min_f32_fn%type%type = f32_type
	allocate(min_f32_fn%params(2))
	allocate(min_f32_fn%param_names%v(2))

	min_f32_fn%params(1)%type = f32_type
	min_f32_fn%param_names%v(1)%s = "a0"

	min_f32_fn%params(2)%type = f32_type
	min_f32_fn%param_names%v(2)%s = "a1"

	min_f32_fn%variadic_min  = 0
	min_f32_fn%variadic_type = f32_type
	min_f32_fn%variadic_name = "ai"

	call fns%insert("0min_f32", min_f32_fn, id_index)

	!********

	min_f64_fn%type%type = f64_type
	allocate(min_f64_fn%params(2))
	allocate(min_f64_fn%param_names%v(2))

	min_f64_fn%params(1)%type = f64_type
	min_f64_fn%param_names%v(1)%s = "a0"

	min_f64_fn%params(2)%type = f64_type
	min_f64_fn%param_names%v(2)%s = "a1"

	min_f64_fn%variadic_min  = 0
	min_f64_fn%variadic_type = f64_type
	min_f64_fn%variadic_name = "ai"

	call fns%insert("0min_f64", min_f64_fn, id_index)

	!********

	max_i32_fn%type%type = i32_type
	allocate(max_i32_fn%params(2))
	allocate(max_i32_fn%param_names%v(2))

	max_i32_fn%params(1)%type = i32_type
	max_i32_fn%param_names%v(1)%s = "a0"

	max_i32_fn%params(2)%type = i32_type
	max_i32_fn%param_names%v(2)%s = "a1"

	max_i32_fn%variadic_min  = 0
	max_i32_fn%variadic_type = i32_type
	max_i32_fn%variadic_name = "ai"

	call fns%insert("0max_i32", max_i32_fn, id_index)

	!********

	max_i64_fn%type%type = i64_type
	allocate(max_i64_fn%params(2))
	allocate(max_i64_fn%param_names%v(2))

	max_i64_fn%params(1)%type = i64_type
	max_i64_fn%param_names%v(1)%s = "a0"

	max_i64_fn%params(2)%type = i64_type
	max_i64_fn%param_names%v(2)%s = "a1"

	max_i64_fn%variadic_min  = 0
	max_i64_fn%variadic_type = i64_type
	max_i64_fn%variadic_name = "ai"

	call fns%insert("0max_i64", max_i64_fn, id_index)

	!********

	max_f32_fn%type%type = f32_type
	allocate(max_f32_fn%params(2))
	allocate(max_f32_fn%param_names%v(2))

	max_f32_fn%params(1)%type = f32_type
	max_f32_fn%param_names%v(1)%s = "a0"

	max_f32_fn%params(2)%type = f32_type
	max_f32_fn%param_names%v(2)%s = "a1"

	max_f32_fn%variadic_min  = 0
	max_f32_fn%variadic_type = f32_type
	max_f32_fn%variadic_name = "ai"

	call fns%insert("0max_f32", max_f32_fn, id_index)

	!********

	max_f64_fn%type%type = f64_type
	allocate(max_f64_fn%params(2))
	allocate(max_f64_fn%param_names%v(2))

	max_f64_fn%params(1)%type = f64_type
	max_f64_fn%param_names%v(1)%s = "a0"

	max_f64_fn%params(2)%type = f64_type
	max_f64_fn%param_names%v(2)%s = "a1"

	max_f64_fn%variadic_min  = 0
	max_f64_fn%variadic_type = f64_type
	max_f64_fn%variadic_name = "ai"

	call fns%insert("0max_f64", max_f64_fn, id_index)

	!********

	! Return array of all functions declared in this module
	fn_array = &
		[ &
			min_i32_fn, min_i64_fn, min_f32_fn, min_f64_fn, &
			max_i32_fn, max_i64_fn, max_f32_fn, max_f64_fn  &
		]

end subroutine declare_minmax_fns

!===============================================================================

end module syntran__intr_fns_minmax_m

!===============================================================================
