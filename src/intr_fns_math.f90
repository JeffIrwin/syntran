
!===============================================================================

module syntran__intr_fns_math_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_math_fns(fns, id_index, fn_array)

	! Declare mathematical intrinsic functions (exp, log, sqrt, abs, trig)

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
		sqrt_f32_fn, sqrt_f64_fn, sqrt_f32_arr_fn, sqrt_f64_arr_fn, &
		cos_f32_fn, cos_f64_fn, cos_f32_arr_fn, cos_f64_arr_fn, &
		sin_f32_fn, sin_f64_fn, sin_f32_arr_fn, sin_f64_arr_fn, &
		tan_f32_fn, tan_f64_fn, tan_f32_arr_fn, tan_f64_arr_fn, &
		cosd_f32_fn, cosd_f64_fn, cosd_f32_arr_fn, cosd_f64_arr_fn, &
		sind_f32_fn, sind_f64_fn, sind_f32_arr_fn, sind_f64_arr_fn, &
		tand_f32_fn, tand_f64_fn, tand_f32_arr_fn, tand_f64_arr_fn, &
		acos_f32_fn, acos_f64_fn, acos_f32_arr_fn, acos_f64_arr_fn, &
		asin_f32_fn, asin_f64_fn, asin_f32_arr_fn, asin_f64_arr_fn, &
		atan_f32_fn, atan_f64_fn, atan_f32_arr_fn, atan_f64_arr_fn, &
		acosd_f32_fn, acosd_f64_fn, acosd_f32_arr_fn, acosd_f64_arr_fn, &
		asind_f32_fn, asind_f64_fn, asind_f32_arr_fn, asind_f64_arr_fn, &
		atand_f32_fn, atand_f64_fn, atand_f32_arr_fn, atand_f64_arr_fn

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

	cos_f32_fn%type%type = f32_type
	allocate(cos_f32_fn%params(1))
	allocate(cos_f32_fn%param_names%v(1))
	cos_f32_fn%params(1)%type = f32_type
	cos_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0cos_f32", cos_f32_fn, id_index)

	!********

	cos_f64_fn%type%type = f64_type
	allocate(cos_f64_fn%params(1))
	allocate(cos_f64_fn%param_names%v(1))
	cos_f64_fn%params(1)%type = f64_type
	cos_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0cos_f64", cos_f64_fn, id_index)

	!********

	cos_f32_arr_fn%type%type = array_type
	allocate(cos_f32_arr_fn%type%array)
	cos_f32_arr_fn%type%array%type = f32_type
	cos_f32_arr_fn%type%array%rank = -1

	allocate(cos_f32_arr_fn%params(1))
	allocate(cos_f32_arr_fn%param_names%v(1))

	cos_f32_arr_fn%params(1)%type = any_type

	cos_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0cos_f32_arr", cos_f32_arr_fn, id_index)

	!********

	cos_f64_arr_fn%type%type = array_type
	allocate(cos_f64_arr_fn%type%array)
	cos_f64_arr_fn%type%array%type = f64_type
	cos_f64_arr_fn%type%array%rank = -1

	allocate(cos_f64_arr_fn%params(1))
	allocate(cos_f64_arr_fn%param_names%v(1))

	cos_f64_arr_fn%params(1)%type = any_type

	cos_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0cos_f64_arr", cos_f64_arr_fn, id_index)

	!********

	sin_f32_fn%type%type = f32_type
	allocate(sin_f32_fn%params(1))
	allocate(sin_f32_fn%param_names%v(1))
	sin_f32_fn%params(1)%type = f32_type
	sin_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0sin_f32", sin_f32_fn, id_index)

	!********

	sin_f64_fn%type%type = f64_type
	allocate(sin_f64_fn%params(1))
	allocate(sin_f64_fn%param_names%v(1))
	sin_f64_fn%params(1)%type = f64_type
	sin_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0sin_f64", sin_f64_fn, id_index)

	!********

	sin_f32_arr_fn%type%type = array_type
	allocate(sin_f32_arr_fn%type%array)
	sin_f32_arr_fn%type%array%type = f32_type
	sin_f32_arr_fn%type%array%rank = -1

	allocate(sin_f32_arr_fn%params(1))
	allocate(sin_f32_arr_fn%param_names%v(1))

	sin_f32_arr_fn%params(1)%type = any_type

	sin_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sin_f32_arr", sin_f32_arr_fn, id_index)

	!********

	sin_f64_arr_fn%type%type = array_type
	allocate(sin_f64_arr_fn%type%array)
	sin_f64_arr_fn%type%array%type = f64_type
	sin_f64_arr_fn%type%array%rank = -1

	allocate(sin_f64_arr_fn%params(1))
	allocate(sin_f64_arr_fn%param_names%v(1))

	sin_f64_arr_fn%params(1)%type = any_type

	sin_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sin_f64_arr", sin_f64_arr_fn, id_index)

	!********

	tan_f32_fn%type%type = f32_type
	allocate(tan_f32_fn%params(1))
	allocate(tan_f32_fn%param_names%v(1))
	tan_f32_fn%params(1)%type = f32_type
	tan_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0tan_f32", tan_f32_fn, id_index)

	!********

	tan_f64_fn%type%type = f64_type
	allocate(tan_f64_fn%params(1))
	allocate(tan_f64_fn%param_names%v(1))
	tan_f64_fn%params(1)%type = f64_type
	tan_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0tan_f64", tan_f64_fn, id_index)

	!********

	tan_f32_arr_fn%type%type = array_type
	allocate(tan_f32_arr_fn%type%array)
	tan_f32_arr_fn%type%array%type = f32_type
	tan_f32_arr_fn%type%array%rank = -1

	allocate(tan_f32_arr_fn%params(1))
	allocate(tan_f32_arr_fn%param_names%v(1))

	tan_f32_arr_fn%params(1)%type = any_type

	tan_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0tan_f32_arr", tan_f32_arr_fn, id_index)

	!********

	tan_f64_arr_fn%type%type = array_type
	allocate(tan_f64_arr_fn%type%array)
	tan_f64_arr_fn%type%array%type = f64_type
	tan_f64_arr_fn%type%array%rank = -1

	allocate(tan_f64_arr_fn%params(1))
	allocate(tan_f64_arr_fn%param_names%v(1))

	tan_f64_arr_fn%params(1)%type = any_type

	tan_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0tan_f64_arr", tan_f64_arr_fn, id_index)

	!********

	cosd_f32_fn%type%type = f32_type
	allocate(cosd_f32_fn%params(1))
	allocate(cosd_f32_fn%param_names%v(1))
	cosd_f32_fn%params(1)%type = f32_type
	cosd_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0cosd_f32", cosd_f32_fn, id_index)

	!********

	cosd_f64_fn%type%type = f64_type
	allocate(cosd_f64_fn%params(1))
	allocate(cosd_f64_fn%param_names%v(1))
	cosd_f64_fn%params(1)%type = f64_type
	cosd_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0cosd_f64", cosd_f64_fn, id_index)

	!********

	cosd_f32_arr_fn%type%type = array_type
	allocate(cosd_f32_arr_fn%type%array)
	cosd_f32_arr_fn%type%array%type = f32_type
	cosd_f32_arr_fn%type%array%rank = -1

	allocate(cosd_f32_arr_fn%params(1))
	allocate(cosd_f32_arr_fn%param_names%v(1))

	cosd_f32_arr_fn%params(1)%type = any_type

	cosd_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0cosd_f32_arr", cosd_f32_arr_fn, id_index)

	!********

	cosd_f64_arr_fn%type%type = array_type
	allocate(cosd_f64_arr_fn%type%array)
	cosd_f64_arr_fn%type%array%type = f64_type
	cosd_f64_arr_fn%type%array%rank = -1

	allocate(cosd_f64_arr_fn%params(1))
	allocate(cosd_f64_arr_fn%param_names%v(1))

	cosd_f64_arr_fn%params(1)%type = any_type

	cosd_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0cosd_f64_arr", cosd_f64_arr_fn, id_index)

	!********

	sind_f32_fn%type%type = f32_type
	allocate(sind_f32_fn%params(1))
	allocate(sind_f32_fn%param_names%v(1))
	sind_f32_fn%params(1)%type = f32_type
	sind_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0sind_f32", sind_f32_fn, id_index)

	!********

	sind_f64_fn%type%type = f64_type
	allocate(sind_f64_fn%params(1))
	allocate(sind_f64_fn%param_names%v(1))
	sind_f64_fn%params(1)%type = f64_type
	sind_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0sind_f64", sind_f64_fn, id_index)

	!********

	sind_f32_arr_fn%type%type = array_type
	allocate(sind_f32_arr_fn%type%array)
	sind_f32_arr_fn%type%array%type = f32_type
	sind_f32_arr_fn%type%array%rank = -1

	allocate(sind_f32_arr_fn%params(1))
	allocate(sind_f32_arr_fn%param_names%v(1))

	sind_f32_arr_fn%params(1)%type = any_type

	sind_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sind_f32_arr", sind_f32_arr_fn, id_index)

	!********

	sind_f64_arr_fn%type%type = array_type
	allocate(sind_f64_arr_fn%type%array)
	sind_f64_arr_fn%type%array%type = f64_type
	sind_f64_arr_fn%type%array%rank = -1

	allocate(sind_f64_arr_fn%params(1))
	allocate(sind_f64_arr_fn%param_names%v(1))

	sind_f64_arr_fn%params(1)%type = any_type

	sind_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0sind_f64_arr", sind_f64_arr_fn, id_index)

	!********

	tand_f32_fn%type%type = f32_type
	allocate(tand_f32_fn%params(1))
	allocate(tand_f32_fn%param_names%v(1))
	tand_f32_fn%params(1)%type = f32_type
	tand_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0tand_f32", tand_f32_fn, id_index)

	!********

	tand_f64_fn%type%type = f64_type
	allocate(tand_f64_fn%params(1))
	allocate(tand_f64_fn%param_names%v(1))
	tand_f64_fn%params(1)%type = f64_type
	tand_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0tand_f64", tand_f64_fn, id_index)

	!********

	tand_f32_arr_fn%type%type = array_type
	allocate(tand_f32_arr_fn%type%array)
	tand_f32_arr_fn%type%array%type = f32_type
	tand_f32_arr_fn%type%array%rank = -1

	allocate(tand_f32_arr_fn%params(1))
	allocate(tand_f32_arr_fn%param_names%v(1))

	tand_f32_arr_fn%params(1)%type = any_type

	tand_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0tand_f32_arr", tand_f32_arr_fn, id_index)

	!********

	tand_f64_arr_fn%type%type = array_type
	allocate(tand_f64_arr_fn%type%array)
	tand_f64_arr_fn%type%array%type = f64_type
	tand_f64_arr_fn%type%array%rank = -1

	allocate(tand_f64_arr_fn%params(1))
	allocate(tand_f64_arr_fn%param_names%v(1))

	tand_f64_arr_fn%params(1)%type = any_type

	tand_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0tand_f64_arr", tand_f64_arr_fn, id_index)

	!********

	acos_f32_fn%type%type = f32_type
	allocate(acos_f32_fn%params(1))
	allocate(acos_f32_fn%param_names%v(1))
	acos_f32_fn%params(1)%type = f32_type
	acos_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0acos_f32", acos_f32_fn, id_index)

	!********

	acos_f64_fn%type%type = f64_type
	allocate(acos_f64_fn%params(1))
	allocate(acos_f64_fn%param_names%v(1))
	acos_f64_fn%params(1)%type = f64_type
	acos_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0acos_f64", acos_f64_fn, id_index)

	!********

	acos_f32_arr_fn%type%type = array_type
	allocate(acos_f32_arr_fn%type%array)
	acos_f32_arr_fn%type%array%type = f32_type
	acos_f32_arr_fn%type%array%rank = -1

	allocate(acos_f32_arr_fn%params(1))
	allocate(acos_f32_arr_fn%param_names%v(1))

	acos_f32_arr_fn%params(1)%type = any_type

	acos_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0acos_f32_arr", acos_f32_arr_fn, id_index)

	!********

	acos_f64_arr_fn%type%type = array_type
	allocate(acos_f64_arr_fn%type%array)
	acos_f64_arr_fn%type%array%type = f64_type
	acos_f64_arr_fn%type%array%rank = -1

	allocate(acos_f64_arr_fn%params(1))
	allocate(acos_f64_arr_fn%param_names%v(1))

	acos_f64_arr_fn%params(1)%type = any_type

	acos_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0acos_f64_arr", acos_f64_arr_fn, id_index)

	!********

	asin_f32_fn%type%type = f32_type
	allocate(asin_f32_fn%params(1))
	allocate(asin_f32_fn%param_names%v(1))
	asin_f32_fn%params(1)%type = f32_type
	asin_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0asin_f32", asin_f32_fn, id_index)

	!********

	asin_f64_fn%type%type = f64_type
	allocate(asin_f64_fn%params(1))
	allocate(asin_f64_fn%param_names%v(1))
	asin_f64_fn%params(1)%type = f64_type
	asin_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0asin_f64", asin_f64_fn, id_index)

	!********

	asin_f32_arr_fn%type%type = array_type
	allocate(asin_f32_arr_fn%type%array)
	asin_f32_arr_fn%type%array%type = f32_type
	asin_f32_arr_fn%type%array%rank = -1

	allocate(asin_f32_arr_fn%params(1))
	allocate(asin_f32_arr_fn%param_names%v(1))

	asin_f32_arr_fn%params(1)%type = any_type

	asin_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0asin_f32_arr", asin_f32_arr_fn, id_index)

	!********

	asin_f64_arr_fn%type%type = array_type
	allocate(asin_f64_arr_fn%type%array)
	asin_f64_arr_fn%type%array%type = f64_type
	asin_f64_arr_fn%type%array%rank = -1

	allocate(asin_f64_arr_fn%params(1))
	allocate(asin_f64_arr_fn%param_names%v(1))

	asin_f64_arr_fn%params(1)%type = any_type

	asin_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0asin_f64_arr", asin_f64_arr_fn, id_index)

	!********

	atan_f32_fn%type%type = f32_type
	allocate(atan_f32_fn%params(1))
	allocate(atan_f32_fn%param_names%v(1))
	atan_f32_fn%params(1)%type = f32_type
	atan_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0atan_f32", atan_f32_fn, id_index)

	!********

	atan_f64_fn%type%type = f64_type
	allocate(atan_f64_fn%params(1))
	allocate(atan_f64_fn%param_names%v(1))
	atan_f64_fn%params(1)%type = f64_type
	atan_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0atan_f64", atan_f64_fn, id_index)

	!********

	atan_f32_arr_fn%type%type = array_type
	allocate(atan_f32_arr_fn%type%array)
	atan_f32_arr_fn%type%array%type = f32_type
	atan_f32_arr_fn%type%array%rank = -1

	allocate(atan_f32_arr_fn%params(1))
	allocate(atan_f32_arr_fn%param_names%v(1))

	atan_f32_arr_fn%params(1)%type = any_type

	atan_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0atan_f32_arr", atan_f32_arr_fn, id_index)

	!********

	atan_f64_arr_fn%type%type = array_type
	allocate(atan_f64_arr_fn%type%array)
	atan_f64_arr_fn%type%array%type = f64_type
	atan_f64_arr_fn%type%array%rank = -1

	allocate(atan_f64_arr_fn%params(1))
	allocate(atan_f64_arr_fn%param_names%v(1))

	atan_f64_arr_fn%params(1)%type = any_type

	atan_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0atan_f64_arr", atan_f64_arr_fn, id_index)

	!********

	acosd_f32_fn%type%type = f32_type
	allocate(acosd_f32_fn%params(1))
	allocate(acosd_f32_fn%param_names%v(1))
	acosd_f32_fn%params(1)%type = f32_type
	acosd_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0acosd_f32", acosd_f32_fn, id_index)

	!********

	acosd_f64_fn%type%type = f64_type
	allocate(acosd_f64_fn%params(1))
	allocate(acosd_f64_fn%param_names%v(1))
	acosd_f64_fn%params(1)%type = f64_type
	acosd_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0acosd_f64", acosd_f64_fn, id_index)

	!********

	acosd_f32_arr_fn%type%type = array_type
	allocate(acosd_f32_arr_fn%type%array)
	acosd_f32_arr_fn%type%array%type = f32_type
	acosd_f32_arr_fn%type%array%rank = -1

	allocate(acosd_f32_arr_fn%params(1))
	allocate(acosd_f32_arr_fn%param_names%v(1))

	acosd_f32_arr_fn%params(1)%type = any_type

	acosd_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0acosd_f32_arr", acosd_f32_arr_fn, id_index)

	!********

	acosd_f64_arr_fn%type%type = array_type
	allocate(acosd_f64_arr_fn%type%array)
	acosd_f64_arr_fn%type%array%type = f64_type
	acosd_f64_arr_fn%type%array%rank = -1

	allocate(acosd_f64_arr_fn%params(1))
	allocate(acosd_f64_arr_fn%param_names%v(1))

	acosd_f64_arr_fn%params(1)%type = any_type

	acosd_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0acosd_f64_arr", acosd_f64_arr_fn, id_index)

	!********

	asind_f32_fn%type%type = f32_type
	allocate(asind_f32_fn%params(1))
	allocate(asind_f32_fn%param_names%v(1))
	asind_f32_fn%params(1)%type = f32_type
	asind_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0asind_f32", asind_f32_fn, id_index)

	!********

	asind_f64_fn%type%type = f64_type
	allocate(asind_f64_fn%params(1))
	allocate(asind_f64_fn%param_names%v(1))
	asind_f64_fn%params(1)%type = f64_type
	asind_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0asind_f64", asind_f64_fn, id_index)

	!********

	asind_f32_arr_fn%type%type = array_type
	allocate(asind_f32_arr_fn%type%array)
	asind_f32_arr_fn%type%array%type = f32_type
	asind_f32_arr_fn%type%array%rank = -1

	allocate(asind_f32_arr_fn%params(1))
	allocate(asind_f32_arr_fn%param_names%v(1))

	asind_f32_arr_fn%params(1)%type = any_type

	asind_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0asind_f32_arr", asind_f32_arr_fn, id_index)

	!********

	asind_f64_arr_fn%type%type = array_type
	allocate(asind_f64_arr_fn%type%array)
	asind_f64_arr_fn%type%array%type = f64_type
	asind_f64_arr_fn%type%array%rank = -1

	allocate(asind_f64_arr_fn%params(1))
	allocate(asind_f64_arr_fn%param_names%v(1))

	asind_f64_arr_fn%params(1)%type = any_type

	asind_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0asind_f64_arr", asind_f64_arr_fn, id_index)

	!********

	atand_f32_fn%type%type = f32_type
	allocate(atand_f32_fn%params(1))
	allocate(atand_f32_fn%param_names%v(1))
	atand_f32_fn%params(1)%type = f32_type
	atand_f32_fn%param_names%v(1)%s = "x"

	call fns%insert("0atand_f32", atand_f32_fn, id_index)

	!********

	atand_f64_fn%type%type = f64_type
	allocate(atand_f64_fn%params(1))
	allocate(atand_f64_fn%param_names%v(1))
	atand_f64_fn%params(1)%type = f64_type
	atand_f64_fn%param_names%v(1)%s = "x"

	call fns%insert("0atand_f64", atand_f64_fn, id_index)

	!********

	atand_f32_arr_fn%type%type = array_type
	allocate(atand_f32_arr_fn%type%array)
	atand_f32_arr_fn%type%array%type = f32_type
	atand_f32_arr_fn%type%array%rank = -1

	allocate(atand_f32_arr_fn%params(1))
	allocate(atand_f32_arr_fn%param_names%v(1))

	atand_f32_arr_fn%params(1)%type = any_type

	atand_f32_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0atand_f32_arr", atand_f32_arr_fn, id_index)

	!********

	atand_f64_arr_fn%type%type = array_type
	allocate(atand_f64_arr_fn%type%array)
	atand_f64_arr_fn%type%array%type = f64_type
	atand_f64_arr_fn%type%array%rank = -1

	allocate(atand_f64_arr_fn%params(1))
	allocate(atand_f64_arr_fn%param_names%v(1))

	atand_f64_arr_fn%params(1)%type = any_type

	atand_f64_arr_fn%param_names%v(1)%s = "x"

	call fns%insert("0atand_f64_arr", atand_f64_arr_fn, id_index)

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
			abs_i32_fn, abs_i64_fn, abs_i32_arr_fn, abs_i64_arr_fn, &
			cos_f32_fn, cos_f64_fn, cos_f32_arr_fn, cos_f64_arr_fn, &
			sin_f32_fn, sin_f64_fn, sin_f32_arr_fn, sin_f64_arr_fn, &
			tan_f32_fn, tan_f64_fn, tan_f32_arr_fn, tan_f64_arr_fn, &
			cosd_f32_fn, cosd_f64_fn, cosd_f32_arr_fn, cosd_f64_arr_fn, &
			sind_f32_fn, sind_f64_fn, sind_f32_arr_fn, sind_f64_arr_fn, &
			tand_f32_fn, tand_f64_fn, tand_f32_arr_fn, tand_f64_arr_fn, &
			acos_f32_fn, acos_f64_fn, acos_f32_arr_fn, acos_f64_arr_fn, &
			asin_f32_fn, asin_f64_fn, asin_f32_arr_fn, asin_f64_arr_fn, &
			atan_f32_fn, atan_f64_fn, atan_f32_arr_fn, atan_f64_arr_fn, &
			acosd_f32_fn, acosd_f64_fn, acosd_f32_arr_fn, acosd_f64_arr_fn, &
			asind_f32_fn, asind_f64_fn, asind_f32_arr_fn, asind_f64_arr_fn, &
			atand_f32_fn, atand_f64_fn, atand_f32_arr_fn, atand_f64_arr_fn &
		]

end subroutine declare_math_fns

!===============================================================================

end module syntran__intr_fns_math_m

!===============================================================================

