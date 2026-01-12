
!===============================================================================

module syntran__intr_fns_trig_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_trig_fns(fns, id_index, fn_array)

	! Declare trigonometric intrinsic functions (sin, cos, tan, and inverse/degree variants)

	type(fns_t), intent(inout) :: fns
	integer, intent(inout) :: id_index
	type(fn_t), allocatable, intent(out) :: fn_array(:)

	!********

	type(fn_t) :: &
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

end subroutine declare_trig_fns

!===============================================================================

end module syntran__intr_fns_trig_m

!===============================================================================

