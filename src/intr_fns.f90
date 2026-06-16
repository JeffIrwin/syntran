
!===============================================================================

module syntran__intr_fns_m

	use syntran__types_m
	use syntran__intr_fns_math_m
	use syntran__intr_fns_trig_m
	use syntran__intr_fns_minmax_m
	use syntran__intr_fns_array_m
	use syntran__intr_fns_io_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_intr_fns(fns)

	type(fns_t), intent(out) :: fns

	!********

	integer :: id_index, num_fns

	type(fn_t), allocatable :: math_fns(:), trig_fns(:), minmax_fns(:), array_fns(:), io_fns(:)

	! This used to get incremented automatically inside of fns%insert, but I
	! changed it.  I don't think it matters for intrinsic fns anyway, because
	! their index is not used during evaluation (there's just a big fortran
	! select/case on the fn name)
	id_index = 0

	! Call each category's declare function to get arrays of fn_t
	call declare_math_fns(fns, id_index, math_fns)
	call declare_trig_fns(fns, id_index, trig_fns)
	call declare_minmax_fns(fns, id_index, minmax_fns)
	call declare_array_fns(fns, id_index, array_fns)
	call declare_io_fns(fns, id_index, io_fns)

	! Concatenate all function arrays together
	fns%fns = [math_fns, trig_fns, minmax_fns, array_fns, io_fns]

	num_fns = size(fns%fns)
	fns%num_intr_fns = num_fns
	!print *, "setting num_intr_fns = ", fns%num_intr_fns

end subroutine declare_intr_fns

!===============================================================================

logical function is_overloaded_intr(fn_name)

	! Check if a function name is an overloaded intrinsic function.
	! These are intrinsics that have type-specific implementations
	! (e.g., "dot" -> "0dot_i32", "0dot_f32", etc.)
	!
	! This list must be kept in sync with the cases in resolve_overload() below.
	! If a new overloaded intrinsic is added there, add it here too.

	character(len = *), intent(in) :: fn_name

	select case (fn_name)
	case ("exp", "log", "log10", "log2", "sqrt", "abs", &
		"cos", "sin", "tan", "cosd", "sind", "tand", &
		"acos", "asin", "atan", "acosd", "asind", "atand", &
		"min", "max", "i32", "i64", &
		"sum", "minval", "maxval", "product", "norm2", "dot")
		is_overloaded_intr = .true.
	case default
		is_overloaded_intr = .false.
	end select

end function is_overloaded_intr

!===============================================================================

recursive subroutine resolve_overload(args, fn_call, has_rank, has_arr_type, arr_type_result)

	! Resolve special overloaded intrinsic fns

	type(syntax_node_vector_t), intent(in) :: args

	type(syntax_node_t), intent(inout) :: fn_call

	! It might be possible to eliminate the has_rank variable and instead check
	! if fn_call%val%array is allocated, but it might be cleaner this way
	logical, intent(out) :: has_rank

	! Optional outputs for functions that also need to propagate element type
	! (e.g. std::reshape, whose result type depends on the source argument).
	! parse_fn_call restores arr_type_result into fn_call%val%array%type after
	! fn_call%val = fn%type overwrites it.
	logical, intent(out), optional :: has_arr_type
	integer, intent(out), optional :: arr_type_result

	!********

	integer :: type_, arr_type

	has_rank = .false.
	if (present(has_arr_type)) has_arr_type = .false.
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

	case ("log10")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0log10_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0log10_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0log10_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0log10_f32"
		case default
			fn_call%identifier%text = "0log10_f64"
		end select

	case ("log2")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0log2_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0log2_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0log2_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0log2_f32"
		case default
			fn_call%identifier%text = "0log2_f64"
		end select

	case ("sqrt")

		type_ = f64_type
		if (args%len_ >= 1) type_ = args%v(1)%val%type

		select case (type_)
		case (array_type)

			arr_type = args%v(1)%val%array%type
			!print *, "type = ", kind_name(arr_type)

			select case (arr_type)
			case (f32_type)
				fn_call%identifier%text = "0sqrt_f32_arr"
			case (f64_type)
				fn_call%identifier%text = "0sqrt_f64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0sqrt_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (f32_type)
			fn_call%identifier%text = "0sqrt_f32"
		case default
			fn_call%identifier%text = "0sqrt_f64"
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
			case (i32_type)
				fn_call%identifier%text = "0abs_i32_arr"
			case (i64_type)
				fn_call%identifier%text = "0abs_i64_arr"
			case default
				! Fall-back on scalar to throw a parser error later
				fn_call%identifier%text = "0abs_f64"
			end select

			if (args%len_ >= 1) then
				has_rank = .true.
				allocate(fn_call%val%array)
				fn_call%val%array%rank = args%v(1)%val%array%rank
			end if

		case (i32_type)
			fn_call%identifier%text = "0abs_i32"
		case (i64_type)
			fn_call%identifier%text = "0abs_i64"
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

	case ("minval")

		type_ = i32_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0minval_f32"
		case (f64_type)
			fn_call%identifier%text = "0minval_f64"
		case (i64_type)
			fn_call%identifier%text = "0minval_i64"
		case default
			fn_call%identifier%text = "0minval_i32"
		end select

	case ("maxval")

		type_ = i32_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0maxval_f32"
		case (f64_type)
			fn_call%identifier%text = "0maxval_f64"
		case (i64_type)
			fn_call%identifier%text = "0maxval_i64"
		case default
			fn_call%identifier%text = "0maxval_i32"
		end select

	case ("product")

		type_ = i32_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0product_f32"
		case (f64_type)
			fn_call%identifier%text = "0product_f64"
		case (i64_type)
			fn_call%identifier%text = "0product_i64"
		case default
			fn_call%identifier%text = "0product_i32"
		end select

	case ("norm2")
		! I might change the name norm2 to norm later but I'm not ready to lock
		! in.  `norm2` might have a second arg for `dim`, while `norm` might
		! have a second arg for 1-norm vs infty-norm, etc.

		type_ = f64_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		select case (type_)
		case (f32_type)
			fn_call%identifier%text = "0norm2_f32"
		case default
			fn_call%identifier%text = "0norm2_f64"
		end select

	case ("dot")

		type_ = f64_type
		if (args%len_ >= 1) then
			if (args%v(1)%val%type == array_type) type_ = args%v(1)%val%array%type
		end if

		! Fortran also offers dot_product for logical args.  Should syntran
		! support that?  Could be useful
		select case (type_)
		case (i32_type)
			fn_call%identifier%text = "0dot_i32"
		case (i64_type)
			fn_call%identifier%text = "0dot_i64"
		case (f32_type)
			fn_call%identifier%text = "0dot_f32"
		case default
			fn_call%identifier%text = "0dot_f64"
		end select

	case ("reshape")

		! std::reshape(source, shape) -- only fires for std::reshape, not a
		! user-defined reshape(), because module_prefix is set before this call.
		if (allocated(fn_call%module_prefix)) then
			if (fn_call%module_prefix == "std") then
				has_rank = .true.
				if (.not. allocated(fn_call%val%array)) allocate(fn_call%val%array)

				! Result rank = number of elements in the shape array (second argument)
				if (args%len_ >= 2 .and. args%v(2)%val%type == array_type) then
					fn_call%val%array%rank = int(args%v(2)%val%array%len_)
				else
					fn_call%val%array%rank = -1  ! unknown at parse time
				end if

				! Element type follows the source array (first argument).
				! This is passed back via has_arr_type / arr_type_result so that
				! parse_fn_call can restore it after `fn_call%val = fn%type` overwrites it.
				if (present(has_arr_type) .and. present(arr_type_result)) then
					if (args%len_ >= 1 .and. args%v(1)%val%type == array_type) then
						has_arr_type = .true.
						arr_type_result = args%v(1)%val%array%type
					end if
				end if
			end if
		end if

	end select

end subroutine resolve_overload

!===============================================================================

end module syntran__intr_fns_m

!===============================================================================

