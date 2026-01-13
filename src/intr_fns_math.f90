
!===============================================================================

module syntran__intr_fns_math_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_math_fns(fns, id_index, fn_array)

	! Declare mathematical intrinsic functions (exp, log, sqrt, abs)

	type(fns_t), intent(inout) :: fns
	integer, intent(inout) :: id_index
	type(fn_t), allocatable, intent(out) :: fn_array(:)

	!********

	type(fn_t) :: &
		abs_i32_fn, abs_i64_fn, abs_i32_arr_fn, abs_i64_arr_fn, &
		abs_f32_fn, abs_f64_fn, abs_f32_arr_fn, abs_f64_arr_fn, &
		exp_f32_fn, exp_f64_fn, exp_f32_arr_fn, exp_f64_arr_fn, &
		log_f32_fn, log_f64_fn, log_f32_arr_fn, log_f64_arr_fn, &
		log10_f32_fn, log10_f64_fn, log10_f32_arr_fn, log10_f64_arr_fn, &
		log2_f32_fn, log2_f64_fn, log2_f32_arr_fn, log2_f64_arr_fn, &
		sqrt_f32_fn, sqrt_f64_fn, sqrt_f32_arr_fn, sqrt_f64_arr_fn

	!********

	! Should exp be overloaded for ints?  No, fortran only allows exp on real or
	! complex types

	exp_f32_fn%type%type = f32_type
	allocate(exp_f32_fn%params(1))
	allocate(exp_f32_fn%param_names%v(1))
	exp_f32_fn%params(1)%type = f32_type
	exp_f32_fn%param_names%v(1)%s = "x"

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

	call fns%insert("0exp_f32", exp_f32_fn, id_index)

	!********

	exp_f64_fn%type%type = f64_type
	allocate(exp_f64_fn%params(1))
	allocate(exp_f64_fn%param_names%v(1))
	exp_f64_fn%params(1)%type = f64_type
	exp_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0exp_f64", exp_f64_fn, id_index)

	!********

	exp_f32_arr_fn%type%type = array_type
	allocate(exp_f32_arr_fn%type%array)
	exp_f32_arr_fn%type%array%type = f32_type
	exp_f32_arr_fn%type%array%rank = -1

	allocate(exp_f32_arr_fn%params(1))
	allocate(exp_f32_arr_fn%param_names%v(1))

	exp_f32_arr_fn%params(1)%type = any_type

	exp_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0exp_f32_arr", exp_f32_arr_fn, id_index)

	!********

	exp_f64_arr_fn%type%type = array_type
	allocate(exp_f64_arr_fn%type%array)
	exp_f64_arr_fn%type%array%type = f64_type
	exp_f64_arr_fn%type%array%rank = -1

	allocate(exp_f64_arr_fn%params(1))
	allocate(exp_f64_arr_fn%param_names%v(1))

	exp_f64_arr_fn%params(1)%type = any_type

	exp_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0exp_f64_arr", exp_f64_arr_fn, id_index)

	!********

	log_f32_fn%type%type = f32_type
	allocate(log_f32_fn%params(1))
	allocate(log_f32_fn%param_names%v(1))
	log_f32_fn%params(1)%type = f32_type
	log_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0log_f32", log_f32_fn, id_index)

	!********

	log_f64_fn%type%type = f64_type
	allocate(log_f64_fn%params(1))
	allocate(log_f64_fn%param_names%v(1))
	log_f64_fn%params(1)%type = f64_type
	log_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0log_f64", log_f64_fn, id_index)

	!********

	log_f32_arr_fn%type%type = array_type
	allocate(log_f32_arr_fn%type%array)
	log_f32_arr_fn%type%array%type = f32_type
	log_f32_arr_fn%type%array%rank = -1

	allocate(log_f32_arr_fn%params(1))
	allocate(log_f32_arr_fn%param_names%v(1))

	log_f32_arr_fn%params(1)%type = any_type

	log_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log_f32_arr", log_f32_arr_fn, id_index)

	!********

	log_f64_arr_fn%type%type = array_type
	allocate(log_f64_arr_fn%type%array)
	log_f64_arr_fn%type%array%type = f64_type
	log_f64_arr_fn%type%array%rank = -1

	allocate(log_f64_arr_fn%params(1))
	allocate(log_f64_arr_fn%param_names%v(1))

	log_f64_arr_fn%params(1)%type = any_type

	log_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log_f64_arr", log_f64_arr_fn, id_index)

	!********

	log10_f32_fn%type%type = f32_type
	allocate(log10_f32_fn%params(1))
	allocate(log10_f32_fn%param_names%v(1))
	log10_f32_fn%params(1)%type = f32_type
	log10_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0log10_f32", log10_f32_fn, id_index)

	!********

	log10_f64_fn%type%type = f64_type
	allocate(log10_f64_fn%params(1))
	allocate(log10_f64_fn%param_names%v(1))
	log10_f64_fn%params(1)%type = f64_type
	log10_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0log10_f64", log10_f64_fn, id_index)

	!********

	log10_f32_arr_fn%type%type = array_type
	allocate(log10_f32_arr_fn%type%array)
	log10_f32_arr_fn%type%array%type = f32_type
	log10_f32_arr_fn%type%array%rank = -1

	allocate(log10_f32_arr_fn%params(1))
	allocate(log10_f32_arr_fn%param_names%v(1))

	log10_f32_arr_fn%params(1)%type = any_type

	log10_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log10_f32_arr", log10_f32_arr_fn, id_index)

	!********

	log10_f64_arr_fn%type%type = array_type
	allocate(log10_f64_arr_fn%type%array)
	log10_f64_arr_fn%type%array%type = f64_type
	log10_f64_arr_fn%type%array%rank = -1

	allocate(log10_f64_arr_fn%params(1))
	allocate(log10_f64_arr_fn%param_names%v(1))

	log10_f64_arr_fn%params(1)%type = any_type

	log10_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log10_f64_arr", log10_f64_arr_fn, id_index)

	!********

	log2_f32_fn%type%type = f32_type
	allocate(log2_f32_fn%params(1))
	allocate(log2_f32_fn%param_names%v(1))
	log2_f32_fn%params(1)%type = f32_type
	log2_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0log2_f32", log2_f32_fn, id_index)

	!********

	log2_f64_fn%type%type = f64_type
	allocate(log2_f64_fn%params(1))
	allocate(log2_f64_fn%param_names%v(1))
	log2_f64_fn%params(1)%type = f64_type
	log2_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0log2_f64", log2_f64_fn, id_index)

	!********

	log2_f32_arr_fn%type%type = array_type
	allocate(log2_f32_arr_fn%type%array)
	log2_f32_arr_fn%type%array%type = f32_type
	log2_f32_arr_fn%type%array%rank = -1

	allocate(log2_f32_arr_fn%params(1))
	allocate(log2_f32_arr_fn%param_names%v(1))

	log2_f32_arr_fn%params(1)%type = any_type

	log2_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log2_f32_arr", log2_f32_arr_fn, id_index)

	!********

	log2_f64_arr_fn%type%type = array_type
	allocate(log2_f64_arr_fn%type%array)
	log2_f64_arr_fn%type%array%type = f64_type
	log2_f64_arr_fn%type%array%rank = -1

	allocate(log2_f64_arr_fn%params(1))
	allocate(log2_f64_arr_fn%param_names%v(1))

	log2_f64_arr_fn%params(1)%type = any_type

	log2_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0log2_f64_arr", log2_f64_arr_fn, id_index)

	!********

	sqrt_f32_fn%type%type = f32_type
	allocate(sqrt_f32_fn%params(1))
	allocate(sqrt_f32_fn%param_names%v(1))
	sqrt_f32_fn%params(1)%type = f32_type
	sqrt_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0sqrt_f32", sqrt_f32_fn, id_index)

	!********

	sqrt_f64_fn%type%type = f64_type
	allocate(sqrt_f64_fn%params(1))
	allocate(sqrt_f64_fn%param_names%v(1))
	sqrt_f64_fn%params(1)%type = f64_type
	sqrt_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0sqrt_f64", sqrt_f64_fn, id_index)

	!********

	sqrt_f32_arr_fn%type%type = array_type
	allocate(sqrt_f32_arr_fn%type%array)
	sqrt_f32_arr_fn%type%array%type = f32_type
	sqrt_f32_arr_fn%type%array%rank = -1

	allocate(sqrt_f32_arr_fn%params(1))
	allocate(sqrt_f32_arr_fn%param_names%v(1))

	sqrt_f32_arr_fn%params(1)%type = any_type

	sqrt_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sqrt_f32_arr", sqrt_f32_arr_fn, id_index)

	!********

	sqrt_f64_arr_fn%type%type = array_type
	allocate(sqrt_f64_arr_fn%type%array)
	sqrt_f64_arr_fn%type%array%type = f64_type
	sqrt_f64_arr_fn%type%array%rank = -1

	allocate(sqrt_f64_arr_fn%params(1))
	allocate(sqrt_f64_arr_fn%param_names%v(1))

	sqrt_f64_arr_fn%params(1)%type = any_type

	sqrt_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sqrt_f64_arr", sqrt_f64_arr_fn, id_index)

	!********

	abs_f32_fn%type%type = f32_type
	allocate(abs_f32_fn%params(1))
	allocate(abs_f32_fn%param_names%v(1))
	abs_f32_fn%params(1)%type = f32_type
	abs_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_f32", abs_f32_fn, id_index)

	!********

	abs_f64_fn%type%type = f64_type
	allocate(abs_f64_fn%params(1))
	allocate(abs_f64_fn%param_names%v(1))
	abs_f64_fn%params(1)%type = f64_type
	abs_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_f64", abs_f64_fn, id_index)

	!********

	abs_f32_arr_fn%type%type = array_type
	allocate(abs_f32_arr_fn%type%array)
	abs_f32_arr_fn%type%array%type = f32_type
	abs_f32_arr_fn%type%array%rank = -1

	allocate(abs_f32_arr_fn%params(1))
	allocate(abs_f32_arr_fn%param_names%v(1))

	abs_f32_arr_fn%params(1)%type = any_type

	abs_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_f32_arr", abs_f32_arr_fn, id_index)

	!********

	abs_f64_arr_fn%type%type = array_type
	allocate(abs_f64_arr_fn%type%array)
	abs_f64_arr_fn%type%array%type = f64_type
	abs_f64_arr_fn%type%array%rank = -1

	allocate(abs_f64_arr_fn%params(1))
	allocate(abs_f64_arr_fn%param_names%v(1))

	abs_f64_arr_fn%params(1)%type = any_type

	abs_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_f64_arr", abs_f64_arr_fn, id_index)

	!********

	abs_i32_fn%type%type = i32_type
	allocate(abs_i32_fn%params(1))
	allocate(abs_i32_fn%param_names%v(1))
	abs_i32_fn%params(1)%type = i32_type
	abs_i32_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_i32", abs_i32_fn, id_index)

	!********

	abs_i64_fn%type%type = i64_type
	allocate(abs_i64_fn%params(1))
	allocate(abs_i64_fn%param_names%v(1))
	abs_i64_fn%params(1)%type = i64_type
	abs_i64_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_i64", abs_i64_fn, id_index)

	!********

	abs_i32_arr_fn%type%type = array_type
	allocate(abs_i32_arr_fn%type%array)
	abs_i32_arr_fn%type%array%type = i32_type
	abs_i32_arr_fn%type%array%rank = -1

	allocate(abs_i32_arr_fn%params(1))
	allocate(abs_i32_arr_fn%param_names%v(1))

	abs_i32_arr_fn%params(1)%type = any_type

	abs_i32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_i32_arr", abs_i32_arr_fn, id_index)

	!********

	abs_i64_arr_fn%type%type = array_type
	allocate(abs_i64_arr_fn%type%array)
	abs_i64_arr_fn%type%array%type = i64_type
	abs_i64_arr_fn%type%array%rank = -1

	allocate(abs_i64_arr_fn%params(1))
	allocate(abs_i64_arr_fn%param_names%v(1))

	abs_i64_arr_fn%params(1)%type = any_type

	abs_i64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0abs_i64_arr", abs_i64_arr_fn, id_index)

	!********

	! Return array of all functions declared in this module
	fn_array = &
		[ &
			exp_f32_fn, exp_f64_fn, exp_f32_arr_fn, exp_f64_arr_fn, &
			log_f32_fn, log_f64_fn, log_f32_arr_fn, log_f64_arr_fn, &
			log10_f32_fn, log10_f64_fn, log10_f32_arr_fn, log10_f64_arr_fn, &
			log2_f32_fn, log2_f64_fn, log2_f32_arr_fn, log2_f64_arr_fn, &
			sqrt_f32_fn, sqrt_f64_fn, sqrt_f32_arr_fn, sqrt_f64_arr_fn, &
			abs_f32_fn, abs_f64_fn, abs_f32_arr_fn, abs_f64_arr_fn, &
			abs_i32_fn, abs_i64_fn, abs_i32_arr_fn, abs_i64_arr_fn &
		]

end subroutine declare_math_fns

!===============================================================================

end module syntran__intr_fns_math_m

!===============================================================================

