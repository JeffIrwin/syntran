
!===============================================================================

module syntran__intr_fns_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_intr_fns(fns)

	type(fns_t), intent(out) :: fns

	!********

	integer :: id_index, num_fns

	type(fn_t) :: min_i32_fn, max_i32_fn, println_fn, size_fn, open_fn, &
		close_fn, readln_fn, writeln_fn, str_fn, eof_fn, parse_i32_fn, len_fn, &
		i64_sca_fn, parse_i64_fn, i32_sca_fn, exit_fn, any_fn, all_fn, count_fn, &
		min_i64_fn, max_i64_fn, i32_arr_fn, i64_arr_fn, sum_i32_fn, &
		sum_f32_fn, sum_i64_fn, parse_f32_fn, min_f32_fn, max_f32_fn, &
		char_fn, sum_f64_fn, parse_f64_fn, min_f64_fn, max_f64_fn, &
		abs_f32_fn, abs_f64_fn, abs_f32_arr_fn, abs_f64_arr_fn, &
		exp_f32_fn, exp_f64_fn, exp_f32_arr_fn, exp_f64_arr_fn, &
		log_f32_fn, log_f64_fn, log_f32_arr_fn, log_f64_arr_fn, &
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

	! Increment index for each fn and then set num_fns
	id_index = 0

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	abs_f32_fn%type%type = f32_type
	allocate(abs_f32_fn%params(1))
	allocate(abs_f32_fn%param_names%v(1))
	abs_f32_fn%params(1)%type = f32_type
	abs_f32_fn%param_names%v(1)%s = "x"

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	cos_f32_fn%type%type = f32_type
	allocate(cos_f32_fn%params(1))
	allocate(cos_f32_fn%param_names%v(1))
	cos_f32_fn%params(1)%type = f32_type
	cos_f32_fn%param_names%v(1)%s = "x"

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

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

	! We could make max() and min() work with just 1 argument too.  I'm not sure
	! why you would want to be able to take the max of 1 number, but it seems
	! like an arbitrary limitation.  Anyway we follow the Fortran convention
	! here

	! TODO: add an array version min(array) as opposed to Fortran's minval()
	!
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

	! Internal overloaded name starts with a "0" because this would be illegal
	! for user-defined fn's, so there can never be a clash
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

	call fns%insert("0max_f64", max_f64_fn, id_index)

	!********

	! TODO: update docs to use println() instead of old holyc implicit prints

	println_fn%type%type = void_type ! TODO?

	allocate(println_fn%params(0))
	allocate(println_fn%param_names%v(0))
	!println_fn%param_names%v(1)%s = "a"

	println_fn%variadic_min  = 0
	println_fn%variadic_type = any_type

	call fns%insert("println", println_fn, id_index)

	!********

	str_fn%type%type = str_type

	allocate(str_fn%params(0))
	allocate(str_fn%param_names%v(0))
	!str_fn%param_names%v(1)%s = "a"

	str_fn%variadic_min  = 0
	str_fn%variadic_type = any_type

	call fns%insert("str", str_fn, id_index)

	!********

	len_fn%type%type = i64_type
	allocate(len_fn%params(1))
	allocate(len_fn%param_names%v(1))
	len_fn%params(1)%type = str_type
	len_fn%param_names%v(1)%s = "str"

	call fns%insert("len", len_fn, id_index)

	!********

	! TODO: add fns for parsing str to other types (bool, etc.)

	! Should this accept any type?  f32 can be converted implicitly so there
	! shouldn't be a need for other types

	parse_i32_fn%type%type = i32_type
	allocate(parse_i32_fn%params(1))
	allocate(parse_i32_fn%param_names%v(1))
	parse_i32_fn%params(1)%type = str_type
	parse_i32_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_i32", parse_i32_fn, id_index)

	!********

	parse_i64_fn%type%type = i64_type
	allocate(parse_i64_fn%params(1))
	allocate(parse_i64_fn%param_names%v(1))
	parse_i64_fn%params(1)%type = str_type
	parse_i64_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_i64", parse_i64_fn, id_index)

	!********

	parse_f32_fn%type%type = f32_type
	allocate(parse_f32_fn%params(1))
	allocate(parse_f32_fn%param_names%v(1))
	parse_f32_fn%params(1)%type = str_type
	parse_f32_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_f32", parse_f32_fn, id_index)

	!********

	parse_f64_fn%type%type = f64_type
	allocate(parse_f64_fn%params(1))
	allocate(parse_f64_fn%param_names%v(1))
	parse_f64_fn%params(1)%type = str_type
	parse_f64_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_f64", parse_f64_fn, id_index)

	!********

	char_fn%type%type = str_type
	allocate(char_fn%params(1))
	allocate(char_fn%param_names%v(1))

	char_fn%params(1)%type = i32_type

	char_fn%param_names%v(1)%s = "i"

	call fns%insert("char", char_fn, id_index)

	!********

	i32_sca_fn%type%type = i32_type
	allocate(i32_sca_fn%params(1))
	allocate(i32_sca_fn%param_names%v(1))

	i32_sca_fn%params(1)%type = any_type

	i32_sca_fn%param_names%v(1)%s = "a"

	call fns%insert("0i32_sca", i32_sca_fn, id_index)

	!********

	i32_arr_fn%type%type = array_type
	allocate(i32_arr_fn%type%array)
	i32_arr_fn%type%array%type = i32_type
	i32_arr_fn%type%array%rank = -1

	allocate(i32_arr_fn%params(1))
	allocate(i32_arr_fn%param_names%v(1))

	i32_arr_fn%params(1)%type = any_type

	i32_arr_fn%param_names%v(1)%s = "a"

	call fns%insert("0i32_arr", i32_arr_fn, id_index)

	!********

	! TODO: to f32 casting

	i64_sca_fn%type%type = i64_type
	allocate(i64_sca_fn%params(1))
	allocate(i64_sca_fn%param_names%v(1))

	i64_sca_fn%params(1)%type = any_type

	i64_sca_fn%param_names%v(1)%s = "a"

	call fns%insert("0i64_sca", i64_sca_fn, id_index)

	!********

	i64_arr_fn%type%type = array_type
	allocate(i64_arr_fn%type%array)
	i64_arr_fn%type%array%type = i64_type
	i64_arr_fn%type%array%rank = -1

	allocate(i64_arr_fn%params(1))
	allocate(i64_arr_fn%param_names%v(1))

	i64_arr_fn%params(1)%type = any_type

	i64_arr_fn%param_names%v(1)%s = "a"

	call fns%insert("0i64_arr", i64_arr_fn, id_index)

	!********

	open_fn%type%type = file_type
	allocate(open_fn%params(1))
	allocate(open_fn%param_names%v(1))
	open_fn%params(1)%type = str_type
	open_fn%param_names%v(1)%s = "filename"

	call fns%insert("open", open_fn, id_index)

	!********

	readln_fn%type%type = str_type
	allocate(readln_fn%params(1))
	allocate(readln_fn%param_names%v(1))
	readln_fn%params(1)%type = file_type
	readln_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("readln", readln_fn, id_index)

	!********

	writeln_fn%type%type = void_type
	allocate(writeln_fn%params(1))
	allocate(writeln_fn%param_names%v(1))
	writeln_fn%params(1)%type = file_type
	writeln_fn%param_names%v(1)%s = "file_handle"

	writeln_fn%variadic_min  = 0
	!writeln_fn%variadic_min = 1
	writeln_fn%variadic_type = any_type

	call fns%insert("writeln", writeln_fn, id_index)

	!********

	eof_fn%type%type = bool_type
	allocate(eof_fn%params(1))
	allocate(eof_fn%param_names%v(1))
	eof_fn%params(1)%type = file_type
	eof_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("eof", eof_fn, id_index)

	!********

	close_fn%type%type = void_type
	allocate(close_fn%params(1))
	allocate(close_fn%param_names%v(1))
	close_fn%params(1)%type = file_type
	close_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("close", close_fn, id_index)

	!********

	exit_fn%type%type = void_type
	allocate(exit_fn%params(1))
	allocate(exit_fn%param_names%v(1))
	exit_fn%params(1)%type = i32_type
	exit_fn%param_names%v(1)%s = "exit_status"

	call fns%insert("exit", exit_fn, id_index)

	!********

	size_fn%type%type = i64_type
	allocate(size_fn%params(2))
	allocate(size_fn%param_names%v(2))

	size_fn%params(1)%type = array_type

	allocate(size_fn%params(1)%array)
	size_fn%params(1)%array%type = any_type
	size_fn%params(1)%array%rank = -1  ! negative means any rank

	size_fn%param_names%v(1)%s = "array"

	size_fn%params(2)%type = i32_type
	size_fn%param_names%v(2)%s = "dim"

	call fns%insert("size", size_fn, id_index)

	! It might also be useful to make size() variadic and have size(array)
	! return the product of each dimension's size.  It should just have a single
	! optional param though, not unlimited arity like min/max.

	!********

	count_fn%type%type = i64_type
	allocate(count_fn%params(1))
	allocate(count_fn%param_names%v(1))

	count_fn%params(1)%type = array_type

	allocate(count_fn%params(1)%array)
	count_fn%params(1)%array%type = bool_type
	count_fn%params(1)%array%rank = -1  ! negative means any rank

	count_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to count() like Fortran
	!count_fn%params(2)%type = i32_type
	!count_fn%param_names%v(2)%s = "dim"

	call fns%insert("count", count_fn, id_index)

	!********

	sum_i32_fn%type%type = i32_type
	allocate(sum_i32_fn%params(1))
	allocate(sum_i32_fn%param_names%v(1))

	sum_i32_fn%params(1)%type = array_type

	allocate(sum_i32_fn%params(1)%array)
	sum_i32_fn%params(1)%array%type = i32_type
	sum_i32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i32_fn%param_names%v(1)%s =  "array"

	!! TODO: add mask and dim args to sum() like Fortran.  Maybe overload
	!! several distinct internal fn's like 0min_i32 vs 0min_i64?  The return
	!! value is still the same so maybe there's an easier way
	!sum_i32_fn%params(2)%type = i32_type
	!sum_i32_fn%param_names%v(2)%s = "dim"

	call fns%insert("0sum_i32", sum_i32_fn, id_index)

	!********

	sum_i64_fn%type%type = i64_type
	allocate(sum_i64_fn%params(1))
	allocate(sum_i64_fn%param_names%v(1))

	sum_i64_fn%params(1)%type = array_type

	allocate(sum_i64_fn%params(1)%array)
	sum_i64_fn%params(1)%array%type = i64_type
	sum_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_i64", sum_i64_fn, id_index)

	!********

	sum_f32_fn%type%type = f32_type
	allocate(sum_f32_fn%params(1))
	allocate(sum_f32_fn%param_names%v(1))

	sum_f32_fn%params(1)%type = array_type

	allocate(sum_f32_fn%params(1)%array)
	sum_f32_fn%params(1)%array%type = f32_type
	sum_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_f32", sum_f32_fn, id_index)

	!********

	sum_f64_fn%type%type = f64_type
	allocate(sum_f64_fn%params(1))
	allocate(sum_f64_fn%param_names%v(1))

	sum_f64_fn%params(1)%type = array_type

	allocate(sum_f64_fn%params(1)%array)
	sum_f64_fn%params(1)%array%type = f64_type
	sum_f64_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_f64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_f64", sum_f64_fn, id_index)

	!********

	all_fn%type%type = bool_type
	allocate(all_fn%params(1))
	allocate(all_fn%param_names%v(1))

	all_fn%params(1)%type = array_type

	allocate(all_fn%params(1)%array)
	all_fn%params(1)%array%type = bool_type
	all_fn%params(1)%array%rank = -1  ! negative means any rank

	all_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to all() like Fortran
	!all_fn%params(2)%type = i32_type
	!all_fn%param_names%v(2)%s = "dim"

	call fns%insert("all", all_fn, id_index)

	!********

	any_fn%type%type = bool_type
	allocate(any_fn%params(1))
	allocate(any_fn%param_names%v(1))

	any_fn%params(1)%type = array_type

	allocate(any_fn%params(1)%array)
	any_fn%params(1)%array%type = bool_type
	any_fn%params(1)%array%rank = -1  ! negative means any rank

	any_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to any() like Fortran
	!any_fn%params(2)%type = i32_type
	!any_fn%param_names%v(2)%s = "dim"

	call fns%insert("any", any_fn, id_index)

	!********

	! FIXME: when adding new functions, remember to copy them into the
	! fns%fns(:) array below

	num_fns = id_index
	allocate(fns%fns(num_fns))

	fns%fns = &
		[ &
			abs_f32_fn, abs_f64_fn, abs_f32_arr_fn, abs_f64_arr_fn, &
			acos_f32_fn, acos_f64_fn, acos_f32_arr_fn, acos_f64_arr_fn, &
			asin_f32_fn, asin_f64_fn, asin_f32_arr_fn, asin_f64_arr_fn, &
			atan_f32_fn, atan_f64_fn, atan_f32_arr_fn, atan_f64_arr_fn, &
			acosd_f32_fn, acosd_f64_fn, acosd_f32_arr_fn, acosd_f64_arr_fn, &
			asind_f32_fn, asind_f64_fn, asind_f32_arr_fn, asind_f64_arr_fn, &
			atand_f32_fn, atand_f64_fn, atand_f32_arr_fn, atand_f64_arr_fn, &
			cos_f32_fn, cos_f64_fn, cos_f32_arr_fn, cos_f64_arr_fn, &
			cosd_f32_fn, cosd_f64_fn, cosd_f32_arr_fn, cosd_f64_arr_fn, &
			exp_f32_fn, exp_f64_fn, exp_f32_arr_fn, exp_f64_arr_fn, &
			log_f32_fn, log_f64_fn, log_f32_arr_fn, log_f64_arr_fn, &
			max_i32_fn, max_i64_fn, max_f32_fn, max_f64_fn, &
			min_i32_fn, min_i64_fn, min_f32_fn, min_f64_fn, &
			parse_i32_fn, parse_i64_fn, parse_f32_fn, parse_f64_fn, &
			sin_f32_fn, sin_f64_fn, sin_f32_arr_fn, sin_f64_arr_fn, &
			sind_f32_fn, sind_f64_fn, sind_f32_arr_fn, sind_f64_arr_fn, &
			sum_i32_fn, sum_i64_fn, sum_f32_fn, sum_f64_fn, &
			tan_f32_fn, tan_f64_fn, tan_f32_arr_fn, tan_f64_arr_fn, &
			tand_f32_fn, tand_f64_fn, tand_f32_arr_fn, tand_f64_arr_fn, &
			i32_sca_fn, i32_arr_fn, &
			i64_sca_fn, i64_arr_fn, &
			all_fn        , &
			any_fn        , &
			char_fn       , &
			close_fn      , &
			count_fn      , &
			eof_fn        , &
			exit_fn       , &
			len_fn        , &
			open_fn       , &
			println_fn    , &
			readln_fn     , &
			size_fn       , &
			str_fn        , &
			writeln_fn      &
		]

end subroutine declare_intr_fns

!===============================================================================

recursive subroutine resolve_overload(args, fn_call, has_rank)

	! Resolve special overloaded intrinsic fns

	type(syntax_node_vector_t), intent(in) :: args

	type(syntax_node_t), intent(inout) :: fn_call

	! It might be possible to eliminate the has_rank variable and instead check
	! if fn_call%val%array is allocated, but it might be cleaner this way
	logical, intent(out) :: has_rank

	!********

	integer :: type_, arr_type

	has_rank = .false.
	select case (fn_call%identifier%text)
	case ("exp")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0exp_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0exp_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0exp_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0exp_f32"
		case default
			fn_call%identifier%text = "0exp_f64"
		end select

	case ("log")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0log_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0log_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0log_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0log_f32"
		case default
			fn_call%identifier%text = "0log_f64"
		end select

	case ("abs")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0abs_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0abs_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0abs_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0abs_f32"
		case default
			fn_call%identifier%text = "0abs_f64"
		end select

	case ("cos")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0cos_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0cos_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0cos_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0cos_f32"
		case default
			fn_call%identifier%text = "0cos_f64"
		end select

	case ("sin")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0sin_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0sin_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0sin_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0sin_f32"
		case default
			fn_call%identifier%text = "0sin_f64"
		end select

	case ("tan")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0tan_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0tan_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0tan_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0tan_f32"
		case default
			fn_call%identifier%text = "0tan_f64"
		end select

	case ("cosd")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0cosd_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0cosd_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0cosd_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0cosd_f32"
		case default
			fn_call%identifier%text = "0cosd_f64"
		end select

	case ("sind")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0sind_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0sind_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0sind_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0sind_f32"
		case default
			fn_call%identifier%text = "0sind_f64"
		end select

	case ("tand")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0tand_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0tand_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0tand_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0tand_f32"
		case default
			fn_call%identifier%text = "0tand_f64"
		end select

	case ("acos")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0acos_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0acos_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0acos_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0acos_f32"
		case default
			fn_call%identifier%text = "0acos_f64"
		end select

	case ("asin")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0asin_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0asin_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0asin_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0asin_f32"
		case default
			fn_call%identifier%text = "0asin_f64"
		end select

	case ("atan")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0atan_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0atan_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0atan_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0atan_f32"
		case default
			fn_call%identifier%text = "0atan_f64"
		end select

	case ("acosd")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0acosd_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0acosd_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0acosd_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0acosd_f32"
		case default
			fn_call%identifier%text = "0acosd_f64"
		end select

	case ("asind")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0asind_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0asind_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0asind_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0asind_f32"
		case default
			fn_call%identifier%text = "0asind_f64"
		end select

	case ("atand")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0atand_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0atand_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0atand_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0atand_f32"
		case default
			fn_call%identifier%text = "0atand_f64"
		end select

	case ("min")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (i64_type)
			fn_call%identifier%text = "0min_i64"
		case (f32_type)
			fn_call%identifier%text = "0min_f32"
		case (f64_type)
			fn_call%identifier%text = "0min_f64"
		case default
			fn_call%identifier%text = "0min_i32"
		end select

	case ("max")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (i64_type)
			fn_call%identifier%text = "0max_i64"
		case (f32_type)
			fn_call%identifier%text = "0max_f32"
		case (f64_type)
			fn_call%identifier%text = "0max_f64"
		case default
			fn_call%identifier%text = "0max_i32"
		end select

	case ("i32")

		type_ = i32_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)
			!print *, "resolving 0i32_arr"
			fn_call%identifier%text = "0i32_arr"

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case default
			fn_call%identifier%text = "0i32_sca"
		end select

	case ("i64")

		type_ = i64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)
			!print *, "resolving 0i64_arr"
			fn_call%identifier%text = "0i64_arr"

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case default
			!print *, "resolving 0i64_sca"
			fn_call%identifier%text = "0i64_sca"
		end select

	case ("sum")

		type_ = i32_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0sum_f32"
		case (f64_type)
			fn_call%identifier%text = "0sum_f64"
		case (i64_type)
			fn_call%identifier%text = "0sum_i64"
		case default
			fn_call%identifier%text = "0sum_i32"
		end select

	end select

end subroutine resolve_overload

!===============================================================================

end module syntran__intr_fns_m

!===============================================================================

