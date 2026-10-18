
!===============================================================================

submodule (syntran__vm_m) syntran__vm_intr

	! M6: integer-id intrinsic dispatch for the bytecode VM.
	!
	! vm_call_intr is called by vm_exec when OP_CALL_INTR fires in native mode
	! (b >= 0).  Args have already been popped from the operand stack in reverse
	! order and collected into args(1:nargs).
	!
	! CALL_INTR_NODE fallback cases (readln(file_handle), close) are NOT routed
	! here; they are handled inline in vm_exec with slot writeback.  The no-arg
	! readln() (stdin) has no slot to write back to, so it IS routed here

	use syntran__linalg_m
	use syntran_rt, only: rt_str_t, rt_fmt_split, rt_fmt_join, rt_fmt_bool, &
		rt_fmt_i32, rt_fmt_i64, rt_fmt_f32, rt_fmt_f64, rt_fmt_str

	implicit none

	! Which numeric reduction reduce_dim_numeric() should do
	integer, parameter :: RED_SUM = 1, RED_MIN = 2, RED_MAX = 3, RED_PRODUCT = 4

!===============================================================================

contains

!===============================================================================

module subroutine vm_call_intr(intr_id, nargs, args, state, res)

	integer, intent(in) :: intr_id, nargs
	type(value_t), intent(in) :: args(:)
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!*******

	integer :: i, io
	integer :: env_len, env_stat
	integer :: red_inner, red_n, red_outer, red_op
	integer :: solve_n, solve_nrhs, solve_info
	integer(kind = 8) :: ir, ic

	logical :: exists_, red_ok, ok_

	character(len = :), allocatable :: env_val

	type(char_vector_t) :: str_

	type(rt_str_t), allocatable :: fmt_pieces(:), fmt_specs(:)

	double precision, parameter :: LOG_E_2  = log(2.d0)
	real,             parameter :: LOG_E_2F = log(2.0)

	res%type = unknown_type		! default for void/sentinel results

	select case (intr_id)

	!==== Math ==================================================================

	case (INTR_EXP_F32)
		res%type = f32_type; res%sca%f32 = exp(args(1)%sca%f32)

	case (INTR_EXP_F64)
		res%type = f64_type; res%sca%f64 = exp(args(1)%sca%f64)

	case (INTR_EXP_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = exp(args(1)%array%f32)

	case (INTR_EXP_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = exp(args(1)%array%f64)

	!****
	case (INTR_LOG_F32)
		res%type = f32_type; res%sca%f32 = log(args(1)%sca%f32)

	case (INTR_LOG_F64)
		res%type = f64_type; res%sca%f64 = log(args(1)%sca%f64)

	case (INTR_LOG_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = log(args(1)%array%f32)

	case (INTR_LOG_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = log(args(1)%array%f64)

	!****
	case (INTR_LOG10_F32)
		res%type = f32_type; res%sca%f32 = log10(args(1)%sca%f32)

	case (INTR_LOG10_F64)
		res%type = f64_type; res%sca%f64 = log10(args(1)%sca%f64)

	case (INTR_LOG10_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = log10(args(1)%array%f32)

	case (INTR_LOG10_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = log10(args(1)%array%f64)

	!****
	case (INTR_LOG2_F32)
		res%type = f32_type; res%sca%f32 = log(args(1)%sca%f32) / LOG_E_2F

	case (INTR_LOG2_F64)
		res%type = f64_type; res%sca%f64 = log(args(1)%sca%f64) / LOG_E_2

	case (INTR_LOG2_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = log(args(1)%array%f32) / LOG_E_2F

	case (INTR_LOG2_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = log(args(1)%array%f64) / LOG_E_2

	!****
	case (INTR_SQRT_F32)
		res%type = f32_type; res%sca%f32 = sqrt(args(1)%sca%f32)

	case (INTR_SQRT_F64)
		res%type = f64_type; res%sca%f64 = sqrt(args(1)%sca%f64)

	case (INTR_SQRT_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = sqrt(args(1)%array%f32)

	case (INTR_SQRT_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = sqrt(args(1)%array%f64)

	!****
	case (INTR_ABS_F32)
		res%type = f32_type; res%sca%f32 = abs(args(1)%sca%f32)

	case (INTR_ABS_F64)
		res%type = f64_type; res%sca%f64 = abs(args(1)%sca%f64)

	case (INTR_ABS_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = abs(args(1)%array%f32)

	case (INTR_ABS_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = abs(args(1)%array%f64)

	case (INTR_ABS_I32)
		res%type = i32_type; res%sca%i32 = abs(args(1)%sca%i32)

	case (INTR_ABS_I64)
		res%type = i64_type; res%sca%i64 = abs(args(1)%sca%i64)

	case (INTR_ABS_I32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, i32_type)
		res%array%i32 = abs(args(1)%array%i32)

	case (INTR_ABS_I64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, i64_type)
		res%array%i64 = abs(args(1)%array%i64)

	!==== Trig ==================================================================

	case (INTR_COS_F32)
		res%type = f32_type; res%sca%f32 = cos(args(1)%sca%f32)

	case (INTR_COS_F64)
		res%type = f64_type; res%sca%f64 = cos(args(1)%sca%f64)

	case (INTR_COS_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = cos(args(1)%array%f32)

	case (INTR_COS_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = cos(args(1)%array%f64)

	!****
	case (INTR_SIN_F32)
		res%type = f32_type; res%sca%f32 = sin(args(1)%sca%f32)

	case (INTR_SIN_F64)
		res%type = f64_type; res%sca%f64 = sin(args(1)%sca%f64)

	case (INTR_SIN_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = sin(args(1)%array%f32)

	case (INTR_SIN_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = sin(args(1)%array%f64)

	!****
	case (INTR_TAN_F32)
		res%type = f32_type; res%sca%f32 = tan(args(1)%sca%f32)

	case (INTR_TAN_F64)
		res%type = f64_type; res%sca%f64 = tan(args(1)%sca%f64)

	case (INTR_TAN_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = tan(args(1)%array%f32)

	case (INTR_TAN_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = tan(args(1)%array%f64)

	!****
	case (INTR_COSD_F32)
		res%type = f32_type; res%sca%f32 = cosd(args(1)%sca%f32)

	case (INTR_COSD_F64)
		res%type = f64_type; res%sca%f64 = cosd(args(1)%sca%f64)

	case (INTR_COSD_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = cosd(args(1)%array%f32)

	case (INTR_COSD_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = cosd(args(1)%array%f64)

	!****
	case (INTR_SIND_F32)
		res%type = f32_type; res%sca%f32 = sind(args(1)%sca%f32)

	case (INTR_SIND_F64)
		res%type = f64_type; res%sca%f64 = sind(args(1)%sca%f64)

	case (INTR_SIND_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = sind(args(1)%array%f32)

	case (INTR_SIND_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = sind(args(1)%array%f64)

	!****
	case (INTR_TAND_F32)
		res%type = f32_type; res%sca%f32 = tand(args(1)%sca%f32)

	case (INTR_TAND_F64)
		res%type = f64_type; res%sca%f64 = tand(args(1)%sca%f64)

	case (INTR_TAND_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = tand(args(1)%array%f32)

	case (INTR_TAND_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = tand(args(1)%array%f64)

	!****
	case (INTR_ACOS_F32)
		res%type = f32_type; res%sca%f32 = acos(args(1)%sca%f32)

	case (INTR_ACOS_F64)
		res%type = f64_type; res%sca%f64 = acos(args(1)%sca%f64)

	case (INTR_ACOS_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = acos(args(1)%array%f32)

	case (INTR_ACOS_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = acos(args(1)%array%f64)

	!****
	case (INTR_ASIN_F32)
		res%type = f32_type; res%sca%f32 = asin(args(1)%sca%f32)

	case (INTR_ASIN_F64)
		res%type = f64_type; res%sca%f64 = asin(args(1)%sca%f64)

	case (INTR_ASIN_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = asin(args(1)%array%f32)

	case (INTR_ASIN_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = asin(args(1)%array%f64)

	!****
	case (INTR_ATAN_F32)
		res%type = f32_type; res%sca%f32 = atan(args(1)%sca%f32)

	case (INTR_ATAN_F64)
		res%type = f64_type; res%sca%f64 = atan(args(1)%sca%f64)

	case (INTR_ATAN_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = atan(args(1)%array%f32)

	case (INTR_ATAN_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = atan(args(1)%array%f64)

	!****
	case (INTR_ACOSD_F32)
		res%type = f32_type; res%sca%f32 = acosd(args(1)%sca%f32)

	case (INTR_ACOSD_F64)
		res%type = f64_type; res%sca%f64 = acosd(args(1)%sca%f64)

	case (INTR_ACOSD_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = acosd(args(1)%array%f32)

	case (INTR_ACOSD_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = acosd(args(1)%array%f64)

	!****
	case (INTR_ASIND_F32)
		res%type = f32_type; res%sca%f32 = asind(args(1)%sca%f32)

	case (INTR_ASIND_F64)
		res%type = f64_type; res%sca%f64 = asind(args(1)%sca%f64)

	case (INTR_ASIND_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = asind(args(1)%array%f32)

	case (INTR_ASIND_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = asind(args(1)%array%f64)

	!****
	case (INTR_ATAND_F32)
		res%type = f32_type; res%sca%f32 = atand(args(1)%sca%f32)

	case (INTR_ATAND_F64)
		res%type = f64_type; res%sca%f64 = atand(args(1)%sca%f64)

	case (INTR_ATAND_F32_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f32_type)
		res%array%f32 = atand(args(1)%array%f32)

	case (INTR_ATAND_F64_ARR)
		res%type = array_type
		res%array = mold(args(1)%array, f64_type)
		res%array%f64 = atand(args(1)%array%f64)

	!==== Minmax (variadic) =====================================================

	case (INTR_MIN_I32)
		res%type = i32_type
		res%sca%i32 = args(1)%sca%i32
		do i = 2, nargs
			res%sca%i32 = min(res%sca%i32, args(i)%sca%i32)
		end do

	case (INTR_MIN_I64)
		res%type = i64_type
		res%sca%i64 = args(1)%sca%i64
		do i = 2, nargs
			res%sca%i64 = min(res%sca%i64, args(i)%sca%i64)
		end do

	case (INTR_MIN_F32)
		res%type = f32_type
		res%sca%f32 = args(1)%sca%f32
		do i = 2, nargs
			res%sca%f32 = min(res%sca%f32, args(i)%sca%f32)
		end do

	case (INTR_MIN_F64)
		res%type = f64_type
		res%sca%f64 = args(1)%sca%f64
		do i = 2, nargs
			res%sca%f64 = min(res%sca%f64, args(i)%sca%f64)
		end do

	!****
	case (INTR_MAX_I32)
		res%type = i32_type
		res%sca%i32 = args(1)%sca%i32
		do i = 2, nargs
			res%sca%i32 = max(res%sca%i32, args(i)%sca%i32)
		end do

	case (INTR_MAX_I64)
		res%type = i64_type
		res%sca%i64 = args(1)%sca%i64
		do i = 2, nargs
			res%sca%i64 = max(res%sca%i64, args(i)%sca%i64)
		end do

	case (INTR_MAX_F32)
		res%type = f32_type
		res%sca%f32 = args(1)%sca%f32
		do i = 2, nargs
			res%sca%f32 = max(res%sca%f32, args(i)%sca%f32)
		end do

	case (INTR_MAX_F64)
		res%type = f64_type
		res%sca%f64 = args(1)%sca%f64
		do i = 2, nargs
			res%sca%f64 = max(res%sca%f64, args(i)%sca%f64)
		end do

	!==== String / conversion ===================================================

	case (INTR_PRINTLN)
		res%type = void_type
		do i = 1, nargs
			write(output_unit, '(a)', advance = 'no') args(i)%to_str()
		end do
		write(output_unit, *)

	case (INTR_PRINT)
		! Same as println(), without the newline.  Flush so that a prompt shows
		! up before a following readln() blocks
		res%type = void_type
		do i = 1, nargs
			write(output_unit, '(a)', advance = 'no') args(i)%to_str()
		end do
		flush(output_unit)

	case (INTR_FORMAT)
		! The format string was checked against the args by check_format_call()
		! when the call was parsed, so splitting it can't fail
		res%type = str_type
		call rt_fmt_split(args(1)%str%s, fmt_pieces, fmt_specs, io)
		str_ = new_char_vector()
		do i = 1, size(fmt_specs)
			call str_%push(fmt_pieces(i)%s)
			call str_%push(format_value(args(i+1), fmt_specs(i)%s))
		end do
		call str_%push(fmt_pieces(size(fmt_pieces))%s)
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = str_%trim()

	case (INTR_STR)
		res%type = str_type
		str_ = new_char_vector()
		do i = 1, nargs
			call str_%push(args(i)%to_str())
		end do
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = str_%trim()

	case (INTR_LEN)
		res%type = i64_type
		res%sca%i64 = len(args(1)%str%s, 8)

	case (INTR_REPEAT)
		res%type = str_type
		if (args(2)%sca%i32 < 0) then
			call rt_throw(state, err_rt_repeat_negative(args(2)%sca%i32))
			return
		end if
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = repeat(args(1)%str%s, args(2)%sca%i32)

	case (INTR_PARSE_I32)
		res%type = i32_type
		read(args(1)%str%s, *, iostat = io) res%sca%i32
		if (io /= 0) then
			call rt_throw(state, err_rt(RC_PARSE_I32, " cannot parse_i32() for argument `"// &
				args(1)%str%s//"`"))
			return
		end if

	case (INTR_PARSE_I64)
		res%type = i64_type
		read(args(1)%str%s, *, iostat = io) res%sca%i64
		if (io /= 0) then
			call rt_throw(state, err_rt(RC_PARSE_I64, " cannot parse_i64() for argument `"// &
				args(1)%str%s//"`"))
			return
		end if

	case (INTR_PARSE_F32)
		res%type = f32_type
		read(args(1)%str%s, *, iostat = io) res%sca%f32
		if (io /= 0) then
			call rt_throw(state, err_rt(RC_PARSE_F32, " cannot parse_f32() for argument `"// &
				args(1)%str%s//"`"))
			return
		end if

	case (INTR_PARSE_F64)
		res%type = f64_type
		read(args(1)%str%s, *, iostat = io) res%sca%f64
		if (io /= 0) then
			call rt_throw(state, err_rt(RC_PARSE_F64, " cannot parse_f64() for argument `"// &
				args(1)%str%s//"`"))
			return
		end if

	case (INTR_CHAR)
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = achar(args(1)%sca%i32)

	case (INTR_I32_SCA)
		res%type = i32_type
		res%sca%i32 = args(1)%to_i32()

	case (INTR_I32_ARR)
		res%type = array_type
		res%array = args(1)%to_i32_array()

	case (INTR_I64_SCA)
		res%type = i64_type
		res%sca%i64 = args(1)%to_i64()

	case (INTR_I64_ARR)
		res%type = array_type
		res%array = args(1)%to_i64_array()

	!==== I/O (native-able) =====================================================

	case (INTR_OPEN)
		! args(1) = filename (str), args(2) = mode (str)
		res%type = file_type
		if (.not. allocated(res%file_)) allocate(res%file_)
		call open_file_impl(state, args(1)%str%s, args(2)%str%s, .true., res%file_)

	case (INTR_TRY_OPEN)
		! std::try_open(filename, mode) -- non-throwing open().  Returns a
		! closed handle (f.is_open == false) instead of raising R8 if the
		! underlying open() fails.  A malformed mode is still a runtime
		! error (R6/R7)
		res%type = file_type
		if (.not. allocated(res%file_)) allocate(res%file_)
		call open_file_impl(state, args(1)%str%s, args(2)%str%s, .false., res%file_)

	case (INTR_WRITELN)
		! args(1) = file handle, args(2:) = values to write
		res%type = void_type
		if (.not. args(1)%file_%is_open) then
			call rt_throw(state, err_rt(RC_WRITELN_NOT_OPEN, "writeln() was called for file """// &
				args(1)%file_%name_//""" which is not open"))
			return
		end if
		if (.not. args(1)%file_%mode_write) then
			call rt_throw(state, err_rt(RC_WRITELN_NOT_WRITE_MODE, "writeln() was called for file """// &
				args(1)%file_%name_//""" which was not opened in write mode ""w"""))
			return
		end if
		if (args(1)%file_%binary) then
			call rt_throw(state, err_rt(RC_BINARY_TEXT_OP, "writeln() was called for file """// &
				args(1)%file_%name_//""" which was opened in binary mode ""b"".  Use write() instead"))
			return
		end if
		do i = 2, nargs
			write(args(1)%file_%unit_, '(a)', advance = 'no', iostat = io) args(i)%to_str()
			if (io /= 0) then
				call rt_throw(state, err_rt(RC_WRITELN_FAIL, "cannot writeln() to file """// &
					args(1)%file_%name_//""" (iostat = "//str(io)//")"))
				return
			end if
		end do
		write(args(1)%file_%unit_, *, iostat = io)
		if (io /= 0) then
			call rt_throw(state, err_rt(RC_WRITELN_FAIL, "cannot writeln() to file """// &
				args(1)%file_%name_//""" (iostat = "//str(io)//")"))
			return
		end if

	case (INTR_WRITE)
		! args(1) = file handle, args(2:) = values to write.  Same as writeln(),
		! without the newline
		res%type = void_type
		if (.not. args(1)%file_%is_open) then
			call rt_throw(state, err_rt(RC_WRITELN_NOT_OPEN, "write() was called for file """// &
				args(1)%file_%name_//""" which is not open"))
			return
		end if
		if (.not. args(1)%file_%mode_write) then
			call rt_throw(state, err_rt(RC_WRITELN_NOT_WRITE_MODE, "write() was called for file """// &
				args(1)%file_%name_//""" which was not opened in write mode ""w"""))
			return
		end if
		if (args(1)%file_%binary) then
			do i = 2, nargs
				call write_binary(args(1)%file_%unit_, args(i), io, ok_)
				if (.not. ok_) then
					call rt_throw(state, err_rt(RC_BINARY_WRITE_TYPE, "write() can not write " &
						//"a value of type `"//type_name(args(i))//"` to file """// &
						args(1)%file_%name_//""" which was opened in binary mode ""b"""))
					return
				end if
				if (io /= 0) then
					call rt_throw(state, err_rt(RC_WRITELN_FAIL, "cannot write() to file """// &
						args(1)%file_%name_//""" (iostat = "//str(io)//")"))
					return
				end if
			end do
			return
		end if
		do i = 2, nargs
			write(args(1)%file_%unit_, '(a)', advance = 'no', iostat = io) args(i)%to_str()
			if (io /= 0) then
				call rt_throw(state, err_rt(RC_WRITELN_FAIL, "cannot write() to file """// &
					args(1)%file_%name_//""" (iostat = "//str(io)//")"))
				return
			end if
		end do

	case (INTR_READLN)
		! No-arg readln(): read a line from stdin.  (readln(file_handle) needs
		! slot writeback for the eof flag, so it is handled inline in vm_exec
		! instead of here; this case is only reached with nargs == 0)
		res%type = str_type
		if (state%stdin_eof) then
			! Match file readln(): reading again past EOF is an error
			call rt_throw(state, err_rt(RC_READLN_FAIL, &
				"cannot readln() from stdin past end of input"))
			return
		end if
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = read_line(input_unit, io)
		if (io == iostat_end) then
			state%stdin_eof = .true.
		else if (io /= 0 .and. io /= iostat_eor) then
			call rt_throw(state, err_rt(RC_READLN_FAIL, &
				"cannot readln() from stdin (iostat = "//str(io)//")"))
			return
		end if

	case (INTR_EOF)
		! args(1) = file handle (read only — no writeback needed)
		res%type = bool_type
		if (nargs == 0) then
			! No-arg form: check the stdin eof flag
			res%sca%bool = state%stdin_eof
			return
		end if
		if (.not. args(1)%file_%is_open) then
			call rt_throw(state, err_rt(RC_EOF_NOT_OPEN, "eof() was called for file """// &
				args(1)%file_%name_//""" which is not open"))
			return
		end if
		if (.not. args(1)%file_%mode_read) then
			call rt_throw(state, err_rt(RC_EOF_NOT_READ_MODE, "eof() was called for file """// &
				args(1)%file_%name_//""" which was not opened in read mode ""r"""))
			return
		end if
		if (args(1)%file_%binary) then
			call rt_throw(state, err_rt(RC_BINARY_TEXT_OP, "eof() was called for file """// &
				args(1)%file_%name_//""" which was opened in binary mode ""b"""))
			return
		end if
		if (args(1)%file_%is_std) then
			! stdin's eof lives on state%stdin_eof, kept in sync with the
			! no-arg eof()/readln() forms, not on the (never-set) file handle
			res%sca%bool = state%stdin_eof
		else
			res%sca%bool = args(1)%file_%eof
		end if

	case (INTR_READ_I32, INTR_READ_I64, INTR_READ_F32, INTR_READ_F64)
		call read_values(intr_id, nargs, args, state, res)

	!==== Misc ==================================================================

	case (INTR_EXIT)
		io = args(1)%sca%i32
		if (io == 0) then
			write(*,*) fg_bright_green//'Exiting syntran with status '// &
				str(io)//color_reset
		else
			write(*,*) fg_bold_bright_red//'Exiting syntran with status '// &
				str(io)//color_reset
		end if
		call exit(io)

	case (INTR_SIZE)
		res%type = i64_type
		if (nargs == 2) then
			if (args(2)%sca%i32 < 0 .or. args(2)%sca%i32 >= args(1)%array%rank) then
				call rt_throw(state, err_rt(RC_SIZE_RANK_MISMATCH, "rank mismatch in size() call"))
				return
			end if
			res%sca%i64 = int(args(1)%array%size(args(2)%sca%i32 + 1))
		else
			res%sca%i64 = int(args(1)%array%len_)
		end if

	case (INTR_COUNT)
		res%type = i64_type
		res%sca%i64 = count(args(1)%array%bool)

	!==== Reductions ============================================================

	case (INTR_MINVAL_I32, INTR_MINVAL_I64, INTR_MINVAL_F32, INTR_MINVAL_F64, &
			INTR_MAXVAL_I32, INTR_MAXVAL_I64, INTR_MAXVAL_F32, INTR_MAXVAL_F64)

		! There is no sensible min/max of nothing, and Fortran's huge() would
		! look like real data
		if (args(1)%array%len_ == 0) then
			call rt_throw(state, err_rt(RC_MINMAX_EMPTY, &
				"minval/maxval of an empty array"))
			return
		end if

		select case (intr_id)
		case (INTR_MINVAL_I32)
			res%type = i32_type; res%sca%i32 = minval(args(1)%array%i32)
		case (INTR_MINVAL_I64)
			res%type = i64_type; res%sca%i64 = minval(args(1)%array%i64)
		case (INTR_MINVAL_F32)
			res%type = f32_type; res%sca%f32 = minval(args(1)%array%f32)
		case (INTR_MINVAL_F64)
			res%type = f64_type; res%sca%f64 = minval(args(1)%array%f64)
		case (INTR_MAXVAL_I32)
			res%type = i32_type; res%sca%i32 = maxval(args(1)%array%i32)
		case (INTR_MAXVAL_I64)
			res%type = i64_type; res%sca%i64 = maxval(args(1)%array%i64)
		case (INTR_MAXVAL_F32)
			res%type = f32_type; res%sca%f32 = maxval(args(1)%array%f32)
		case (INTR_MAXVAL_F64)
			res%type = f64_type; res%sca%f64 = maxval(args(1)%array%f64)
		end select

	case (INTR_SUM_I32)
		res%type = i32_type; res%sca%i32 = sum(args(1)%array%i32)

	case (INTR_SUM_I64)
		res%type = i64_type; res%sca%i64 = sum(args(1)%array%i64)

	case (INTR_SUM_F32)
		res%type = f32_type; res%sca%f32 = sum(args(1)%array%f32)

	case (INTR_SUM_F64)
		res%type = f64_type; res%sca%f64 = sum(args(1)%array%f64)

	case (INTR_PRODUCT_I32)
		res%type = i32_type; res%sca%i32 = product(args(1)%array%i32)

	case (INTR_PRODUCT_I64)
		res%type = i64_type; res%sca%i64 = product(args(1)%array%i64)

	case (INTR_PRODUCT_F32)
		res%type = f32_type; res%sca%f32 = product(args(1)%array%f32)

	case (INTR_PRODUCT_F64)
		res%type = f64_type; res%sca%f64 = product(args(1)%array%f64)

	case (INTR_NORM2_F32)
		res%type = f32_type; res%sca%f32 = norm2(args(1)%array%f32)

	case (INTR_NORM2_F64)
		res%type = f64_type; res%sca%f64 = norm2(args(1)%array%f64)

	case (INTR_DOT_F32, INTR_DOT_F64, INTR_DOT_I32, INTR_DOT_I64)
		! dot() only takes rank-1 arrays, so the extent is size(1)
		if (args(1)%array%size(1) /= args(2)%array%size(1)) then
			call rt_throw(state, err_rt_dot_size( &
				args(1)%array%size(1), args(2)%array%size(1)))
			return
		end if

		select case (intr_id)
		case (INTR_DOT_F32)
			res%type = f32_type
			res%sca%f32 = dot_product(args(1)%array%f32, args(2)%array%f32)

		case (INTR_DOT_F64)
			res%type = f64_type
			res%sca%f64 = dot_product(args(1)%array%f64, args(2)%array%f64)

		case (INTR_DOT_I32)
			res%type = i32_type
			res%sca%i32 = dot_product(args(1)%array%i32, args(2)%array%i32)

		case (INTR_DOT_I64)
			res%type = i64_type
			res%sca%i64 = dot_product(args(1)%array%i64, args(2)%array%i64)
		end select

	case (INTR_SOLVE)
		! std::solve(A, b) -- A is n x n, b is a length-n vector or n x nrhs
		! matrix, both f32 or both f64 (checked at parse time).  Returns x
		! without modifying A or b
		if (args(1)%array%size(1) /= args(1)%array%size(2) .or. &
			args(1)%array%size(1) /= args(2)%array%size(1)) then
			call rt_throw(state, err_rt_solve_shape("solve", &
				args(1)%array%size(1), args(1)%array%size(2), &
				args(2)%array%size(1)))
			return
		end if

		solve_n = int(args(1)%array%size(1))
		solve_nrhs = 1
		if (args(2)%array%rank == 2) solve_nrhs = int(args(2)%array%size(2))

		! Result has the shape of b.  mold() copies the metadata but not the
		! buffers, and the solution overwrites a copy of b in place
		res%type  = array_type
		res%array = mold(args(2)%array, args(2)%array%type)

		if (args(1)%array%type == f32_type) then
			res%array%f32 = args(2)%array%f32(1: args(2)%array%len_)
			call gesv_f32(solve_n, solve_nrhs, args(1)%array%f32, &
				res%array%f32, solve_info)
		else
			res%array%f64 = args(2)%array%f64(1: args(2)%array%len_)
			call gesv_f64(solve_n, solve_nrhs, args(1)%array%f64, &
				res%array%f64, solve_info)
		end if

		if (solve_info > 0) then
			call rt_throw(state, err_rt_solve_singular("solve", solve_info))
			return
		end if

	case (INTR_ALL)
		res%type = bool_type; res%sca%bool = all(args(1)%array%bool)

	case (INTR_ANY)
		res%type = bool_type; res%sca%bool = any(args(1)%array%bool)

	!==== Reductions with a dim and/or mask arg ==================================

	case (INTR_COUNT_DIM, INTR_ALL_DIM, INTR_ANY_DIM)
		! count/all/any(mask, dim)
		call reduce_dim_setup(state, args(1)%array, args(2)%sca%i32, &
			merge(i64_type, bool_type, intr_id == INTR_COUNT_DIM), &
			red_inner, red_n, red_outer, res, red_ok)
		if (.not. red_ok) return

		select case (intr_id)
		case (INTR_COUNT_DIM)
			res%array%i64 = reshape(count(reshape( &
				args(1)%array%bool(1: args(1)%array%len_), &
				[red_inner, red_n, red_outer]), dim = 2, kind = 8), &
				[red_inner * red_outer])
		case (INTR_ALL_DIM)
			res%array%bool = reshape(all(reshape( &
				args(1)%array%bool(1: args(1)%array%len_), &
				[red_inner, red_n, red_outer]), dim = 2), &
				[red_inner * red_outer])
		case (INTR_ANY_DIM)
			res%array%bool = reshape(any(reshape( &
				args(1)%array%bool(1: args(1)%array%len_), &
				[red_inner, red_n, red_outer]), dim = 2), &
				[red_inner * red_outer])
		end select
		call reduce_dim_finish(res)

	case (INTR_SUM_EXT, INTR_MINVAL_EXT, INTR_MAXVAL_EXT, INTR_PRODUCT_EXT)
		! sum/minval/maxval/product(array, dim), (array, mask), or
		! (array, dim, mask)

		select case (intr_id)
		case (INTR_SUM_EXT)
			red_op = RED_SUM
		case (INTR_MINVAL_EXT)
			red_op = RED_MIN
		case (INTR_MAXVAL_EXT)
			red_op = RED_MAX
		case default
			red_op = RED_PRODUCT
		end select

		if (nargs == 2 .and. args(2)%type == array_type) then
			! f(array, mask): scalar reduction of the elements where mask is true
			if (.not. same_shape(args(1)%array, args(2)%array)) then
				call rt_throw(state, err_rt(RC_MASK_SHAPE_MISMATCH, &
					"mask shape does not match array shape in "// &
					reduce_name(red_op)//"() call"))
				return
			end if

			call reduce_mask_scalar(state, red_op, args(1)%array, &
				args(2)%array, res, red_ok)
			if (.not. red_ok) return

		else
			call reduce_dim_setup(state, args(1)%array, args(2)%sca%i32, &
				args(1)%array%type, red_inner, red_n, red_outer, res, red_ok)
			if (.not. red_ok) return

			if (nargs == 3) then
				! f(array, dim, mask)
				if (.not. same_shape(args(1)%array, args(3)%array)) then
					call rt_throw(state, err_rt(RC_MASK_SHAPE_MISMATCH, &
						"mask shape does not match array shape in "// &
						reduce_name(red_op)//"() call"))
					return
				end if
				call reduce_dim_numeric(state, red_op, args(1)%array, &
					[red_inner, red_n, red_outer], res, red_ok, args(3)%array)
			else
				call reduce_dim_numeric(state, red_op, args(1)%array, &
					[red_inner, red_n, red_outer], res, red_ok)
			end if
			if (.not. red_ok) return

			call reduce_dim_finish(res)
		end if

	case (INTR_ARGS)
		res%type = array_type
		allocate(res%array)
		res%array%type = str_type
		res%array%rank = 1
		res%array%len_ = state%script_args%len_
		allocate(res%array%size(1))
		res%array%size(1) = res%array%len_
		allocate(res%array%str(res%array%len_))
		do i = 1, state%script_args%len_
			res%array%str(i) = state%script_args%v(i)
		end do

	case (INTR_TRANSPOSE)
		! std::transpose(source) -- transpose a rank-2 array.
		! Physically permutes the column-major flat buffer; result is C x R.
		! args(1) = source array (must be rank-2)

		! Runtime guard: source must be rank-2
		if (args(1)%array%rank /= 2) then
			call rt_throw(state, err_rt(RC_TRANSPOSE_RANK, "transpose requires a rank-2 array"))
			return
		end if

		! Build result metadata without copying any buffers.
		res%type   = array_type
		res%array  = mold(args(1)%array, args(1)%array%type)

		! mold() doesn't carry struct/enum identity (struct_name/struct_cookie/
		! enum_name/enum_cookie); copy it explicitly so member access and
		! type-equality checks on the result still work
		call copy_composite_id(res, args(1))

		! Swap extents: R x C -> C x R (mold copied them as-is)
		res%array%size(1) = args(1)%array%size(2)
		res%array%size(2) = args(1)%array%size(1)

		! Allocate and fill only the relevant buffer via assign-on-allocate.
		select case (args(1)%array%type)
		case (i32_type)
			res%array%i32 = reshape(transpose(reshape( &
				args(1)%array%i32(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (i64_type)
			res%array%i64 = reshape(transpose(reshape( &
				args(1)%array%i64(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (f32_type)
			res%array%f32 = reshape(transpose(reshape( &
				args(1)%array%f32(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (f64_type)
			res%array%f64 = reshape(transpose(reshape( &
				args(1)%array%f64(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (bool_type)
			res%array%bool = reshape(transpose(reshape( &
				args(1)%array%bool(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (str_type)
			res%array%str = reshape(transpose(reshape( &
				args(1)%array%str(1:args(1)%array%len_), &
				[int(args(1)%array%size(1)), int(args(1)%array%size(2))])), &
				[int(res%array%len_)])
		case (struct_type, enum_type)
			! Composite elements live in %struct(:), not in an array_t buffer,
			! so permute the column-major flat index by hand.  Source is R x C
			! with element (r,c) at r + (c-1)*R; result is C x R with (c,r) at
			! c + (r-1)*C.
			allocate(res%struct( res%array%len_ ))
			do ic = 1, args(1)%array%size(2)
			do ir = 1, args(1)%array%size(1)
				res%struct(ic + (ir-1)*args(1)%array%size(2)) = &
					args(1)%struct(ir + (ic-1)*args(1)%array%size(1))
			end do
			end do
		case default
			write(*,*) err_int(IC_TRANSPOSE_ARRAY_TYPE, 'cannot transpose array of type `' &
				//type_name_primitive(args(1)%array%type)//'`')
			call internal_error()
		end select

	case (INTR_RESHAPE)
		! std::reshape(source, shape) -- copy flat buffer, overwrite shape metadata.
		! args(1) = source array, args(2) = shape (i32, rank-1)

		! Runtime guard: product of new shape must equal source element count
		if (product(int(args(2)%array%i32(1:args(2)%array%len_), 8)) /= args(1)%array%len_) then
			call rt_throw(state, err_rt(RC_RESHAPE_MISMATCH, "reshape size mismatch"))
			return
		end if

		! Copy source (deep copies flat buffer, type, kind, len_)
		res = args(1)

		! Overwrite shape metadata
		res%array%rank = int(args(2)%array%len_)
		if (allocated(res%array%size)) deallocate(res%array%size)
		allocate(res%array%size(res%array%rank))
		do i = 1, res%array%rank
			res%array%size(i) = int(args(2)%array%i32(i), 8)
		end do

	case (INTR_SHAPE)
		! std::shape(source) -- extents of source as a rank-1 i64 array.
		! Result length == source rank; result[i] is the size along dimension i.
		res%type = array_type
		allocate(res%array)
		res%array%type = i64_type
		res%array%rank = 1
		res%array%len_ = args(1)%array%rank
		allocate(res%array%size(1))
		res%array%size(1) = res%array%len_
		! %array%size is already i64, so direct assignment works.
		allocate(res%array%i64(res%array%len_))
		res%array%i64 = args(1)%array%size(1:args(1)%array%rank)

	case (INTR_GETENV)
		! std::getenv(name) -- value of env var `name`.  Runtime error if unset
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		call get_environment_variable(args(1)%str%s, length = env_len, status = env_stat)
		if (env_stat /= 0) then
			call rt_throw(state, err_rt(RC_GETENV_UNSET, "getenv() environment variable """// &
				args(1)%str%s//""" is not set"))
			return
		end if
		allocate(character(len = env_len) :: env_val)
		call get_environment_variable(args(1)%str%s, value = env_val)
		res%str%s = env_val
		deallocate(env_val)

	case (INTR_HASENV)
		! std::hasenv(name) -- whether env var `name` is set
		res%type = bool_type
		call get_environment_variable(args(1)%str%s, status = env_stat)
		res%sca%bool = env_stat == 0

	case (INTR_EXISTS)
		! std::exists(path) -- whether a file exists at `path`
		res%type = bool_type
		if (len(args(1)%str%s) == 0) then
			! inquire(file = "") is not standard-conforming
			res%sca%bool = .false.
		else
			inquire(file = resolve_path(state%src_dir, args(1)%str%s), exist = exists_)
			res%sca%bool = exists_
		end if

	case default
		write(*,*) 'VM: unknown intr_id in vm_call_intr: ', intr_id
		call internal_error()

	end select

end subroutine vm_call_intr

!===============================================================================

module subroutine vm_call_intr_ref(intr_id, nargs, args, state, res)

	! Dispatch for intrinsics with by-ref args, i.e. fn_t%ref_params.  Unlike
	! vm_call_intr(), this may modify `args` in place.  The VM moves the by-ref
	! ones back into their variables afterwards (OP_CALL_INTR_REF), even after
	! a runtime error here

	integer, intent(in) :: intr_id, nargs
	type(value_t), intent(inout) :: args(:)
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!*******

	integer :: i, n, m, nrhs, info
	integer, allocatable :: ip1(:)

	logical :: is_f32

	res%type = unknown_type

	select case (intr_id)

	case (INTR_LU_FACTOR)
		! std::lu_factor(&A[, &info]) -- A is m x n, overwritten with its L and
		! U factors.  Returns the min(m,n) pivots, 0-based.  Any shape is fine,
		! like LAPACK getrf.  A zero pivot is an error, unless the `info` arg is
		! there to receive it
		m = int(args(1)%array%size(1))
		n = int(args(1)%array%size(2))
		is_f32 = args(1)%array%type == f32_type

		res%type = array_type
		allocate(res%array)
		res%array%type = i32_type
		res%array%rank = 1
		res%array%len_ = min(m, n)
		allocate(res%array%size(1))
		res%array%size(1) = res%array%len_
		allocate(res%array%i32(res%array%len_))

		if (is_f32) then
			call getrf_f32(m, n, args(1)%array%f32, res%array%i32, info)
		else
			call getrf_f64(m, n, args(1)%array%f64, res%array%i32, info)
		end if

		! LAPACK pivots are 1-based, but syntran's are not
		res%array%i32 = res%array%i32 - 1

		if (nargs >= 2) then
			args(2)%sca%i32 = info
		else if (info > 0) then
			call rt_throw(state, err_rt_solve_singular("lu_factor", info))
			return
		end if

	case (INTR_LU_SOLVE)
		! std::lu_solve(A, ipiv, &b) -- A and ipiv are the outputs of
		! lu_factor() and must be n x n.  b is a length-n vector or n x nrhs
		! matrix, and is overwritten with x
		m = int(args(1)%array%size(1))
		n = int(args(1)%array%size(2))

		if (m /= n .or. m /= args(3)%array%size(1)) then
			call rt_throw(state, err_rt_solve_shape("lu_solve", &
				args(1)%array%size(1), args(1)%array%size(2), &
				args(3)%array%size(1)))
			return
		end if

		! ipiv comes from user code, so validate it before LAPACK reads rows
		! with it
		if (args(2)%array%len_ /= n) then
			call rt_throw(state, err_rt_lu_bad_pivots(int(n, 8), &
				args(2)%array%len_, -1_8, 0))
			return
		end if

		allocate(ip1(n))
		do i = 1, n
			if (args(2)%array%i32(i) < 0 .or. args(2)%array%i32(i) >= n) then
				call rt_throw(state, err_rt_lu_bad_pivots(int(n, 8), &
					args(2)%array%len_, int(i - 1, 8), args(2)%array%i32(i)))
				return
			end if
			ip1(i) = args(2)%array%i32(i) + 1
		end do

		nrhs = 1
		if (args(3)%array%rank == 2) nrhs = int(args(3)%array%size(2))

		! An exactly zero U diagonal means that the factors came from a
		! singular matrix.  lu_factor() raises an error for that unless its
		! caller asked for `info` instead, so check again here instead of
		! dividing by zero
		is_f32 = args(1)%array%type == f32_type
		info = 0
		do i = 1, n
			if (is_f32) then
				if (args(1)%array%f32((i - 1) * n + i) == 0) info = i
			else
				if (args(1)%array%f64((i - 1) * n + i) == 0) info = i
			end if
			if (info > 0) exit
		end do
		if (info > 0) then
			call rt_throw(state, err_rt_solve_singular("lu_solve", info))
			return
		end if

		if (is_f32) then
			call getrs_f32(n, nrhs, args(1)%array%f32, ip1, args(3)%array%f32)
		else
			call getrs_f64(n, nrhs, args(1)%array%f64, ip1, args(3)%array%f64)
		end if
		res%type = void_type

	case default
		write(*,*) 'vm_call_intr_ref: intrinsic id ', intr_id, ' has no by-ref dispatch'
		call internal_error()

	end select

end subroutine vm_call_intr_ref

!===============================================================================

subroutine reduce_dim_setup(state, src, dim_, elem_type, inner, n, outer, res, ok)

	! Common setup for reductions along a dimension.  Checks `dim_` (0-based, like
	! size()) against the rank of `src`, and views the column-major buffer of
	! `src` as a rank-3 array of shape [inner, n, outer] where `n` is the extent
	! being reduced.  Then allocates the metadata of the result in `res`.  The
	! caller fills the buffer `res%array%<type>`, then calls reduce_dim_finish()

	type(state_t), intent(inout) :: state
	type(array_t), intent(in) :: src
	integer, intent(in) :: dim_, elem_type
	integer, intent(out) :: inner, n, outer
	type(value_t), intent(inout) :: res
	logical, intent(out) :: ok

	!********

	inner = 0
	n     = 0
	outer = 0

	ok = .false.
	if (dim_ < 0 .or. dim_ >= src%rank) then
		call rt_throw(state, err_rt(RC_REDUCE_DIM_RANGE, &
			"dim "//str(dim_)//" is out of range for a rank-"//str(src%rank)// &
			" array in reduction call"))
		return
	end if

	inner = int(product(src%size(1: dim_)))
	n     = int(src%size(dim_ + 1))
	outer = int(product(src%size(dim_ + 2: src%rank)))

	res%type = array_type
	allocate(res%array)
	res%array%type = elem_type
	res%array%rank = src%rank - 1
	res%array%len_ = int(inner, 8) * outer
	res%array%cap  = res%array%len_
	res%array%size = [src%size(1: dim_), src%size(dim_ + 2: src%rank)]

	ok = .true.

end subroutine reduce_dim_setup

!===============================================================================

subroutine reduce_dim_finish(res)

	! Reducing a rank-1 array along its only dimension gives a rank-0 result,
	! i.e. a scalar, so unwrap the 1-element array that reduce_dim_setup()
	! allocated

	type(value_t), intent(inout) :: res

	if (res%array%rank /= 0) return

	res%type = res%array%type
	select case (res%type)
	case (i32_type)
		res%sca%i32 = res%array%i32(1)
	case (i64_type)
		res%sca%i64 = res%array%i64(1)
	case (f32_type)
		res%sca%f32 = res%array%f32(1)
	case (f64_type)
		res%sca%f64 = res%array%f64(1)
	case (bool_type)
		res%sca%bool = res%array%bool(1)
	end select
	deallocate(res%array)

end subroutine reduce_dim_finish

!===============================================================================

function reduce_name(op) result(name)

	! Name of a numeric reduction, for error messages

	integer, intent(in) :: op
	character(len = :), allocatable :: name

	select case (op)
	case (RED_SUM)
		name = "sum"
	case (RED_MIN)
		name = "minval"
	case (RED_MAX)
		name = "maxval"
	case default
		name = "product"
	end select

end function reduce_name

!===============================================================================

subroutine throw_minmax_empty(state, op)

	type(state_t), intent(inout) :: state
	integer, intent(in) :: op

	call rt_throw(state, err_rt(RC_MINMAX_EMPTY, &
		reduce_name(op)//"() of an empty array or mask selection"))

end subroutine throw_minmax_empty

!===============================================================================

subroutine reduce_dim_numeric(state, op, src, shp, res, ok, msk)

	! Fill the numeric result buffer of a reduction along a dimension.  `shp` is
	! the [inner, n, outer] view from reduce_dim_setup().  `msk` is only given
	! for a masked reduction
	!
	! There is no sensible minval/maxval of nothing, so those throw if any lane
	! is empty, i.e. if n == 0 or if the mask selects nothing in a lane.  The
	! result of sum/product of nothing is the identity, 0/1, like Fortran

	type(state_t), intent(inout) :: state
	integer, intent(in) :: op, shp(3)
	type(array_t), intent(in) :: src
	type(value_t), intent(inout) :: res
	logical, intent(out) :: ok
	type(array_t), intent(in), optional :: msk

	!********

	integer :: n_out
	logical :: empty
	logical(kind = 1), allocatable :: m3(:,:,:)

	ok = .true.
	n_out = shp(1) * shp(3)

	if (present(msk)) m3 = reshape(msk%bool(1: msk%len_), shp)

	if ((op == RED_MIN .or. op == RED_MAX) .and. n_out > 0) then
		if (present(msk)) then
			empty = any(count(m3, dim = 2) == 0)
		else
			empty = shp(2) == 0
		end if
		if (empty) then
			call throw_minmax_empty(state, op)
			ok = .false.
			return
		end if
	end if

	select case (src%type)
	case (i32_type)
		select case (op)
		case (RED_SUM)
			if (present(msk)) then
				res%array%i32 = reshape(sum(reshape(src%i32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i32 = reshape(sum(reshape(src%i32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MIN)
			if (present(msk)) then
				res%array%i32 = reshape(minval(reshape(src%i32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i32 = reshape(minval(reshape(src%i32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MAX)
			if (present(msk)) then
				res%array%i32 = reshape(maxval(reshape(src%i32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i32 = reshape(maxval(reshape(src%i32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_PRODUCT)
			if (present(msk)) then
				res%array%i32 = reshape(product(reshape(src%i32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i32 = reshape(product(reshape(src%i32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		end select

	case (i64_type)
		select case (op)
		case (RED_SUM)
			if (present(msk)) then
				res%array%i64 = reshape(sum(reshape(src%i64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i64 = reshape(sum(reshape(src%i64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MIN)
			if (present(msk)) then
				res%array%i64 = reshape(minval(reshape(src%i64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i64 = reshape(minval(reshape(src%i64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MAX)
			if (present(msk)) then
				res%array%i64 = reshape(maxval(reshape(src%i64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i64 = reshape(maxval(reshape(src%i64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_PRODUCT)
			if (present(msk)) then
				res%array%i64 = reshape(product(reshape(src%i64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%i64 = reshape(product(reshape(src%i64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		end select

	case (f32_type)
		select case (op)
		case (RED_SUM)
			if (present(msk)) then
				res%array%f32 = reshape(sum(reshape(src%f32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f32 = reshape(sum(reshape(src%f32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MIN)
			if (present(msk)) then
				res%array%f32 = reshape(minval(reshape(src%f32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f32 = reshape(minval(reshape(src%f32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MAX)
			if (present(msk)) then
				res%array%f32 = reshape(maxval(reshape(src%f32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f32 = reshape(maxval(reshape(src%f32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_PRODUCT)
			if (present(msk)) then
				res%array%f32 = reshape(product(reshape(src%f32(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f32 = reshape(product(reshape(src%f32(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		end select

	case (f64_type)
		select case (op)
		case (RED_SUM)
			if (present(msk)) then
				res%array%f64 = reshape(sum(reshape(src%f64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f64 = reshape(sum(reshape(src%f64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MIN)
			if (present(msk)) then
				res%array%f64 = reshape(minval(reshape(src%f64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f64 = reshape(minval(reshape(src%f64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_MAX)
			if (present(msk)) then
				res%array%f64 = reshape(maxval(reshape(src%f64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f64 = reshape(maxval(reshape(src%f64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		case (RED_PRODUCT)
			if (present(msk)) then
				res%array%f64 = reshape(product(reshape(src%f64(1: src%len_), shp), &
					dim = 2, mask = m3), [n_out])
			else
				res%array%f64 = reshape(product(reshape(src%f64(1: src%len_), shp), &
					dim = 2), [n_out])
			end if
		end select

	end select

end subroutine reduce_dim_numeric

!===============================================================================

subroutine reduce_mask_scalar(state, op, a, m, res, ok)

	! Scalar reduction of the elements of `a` where `m` is true.  The caller has
	! already checked that the shapes match.  minval/maxval throw if the mask
	! selects nothing (see reduce_dim_numeric())

	type(state_t), intent(inout) :: state
	integer, intent(in) :: op
	type(array_t), intent(in) :: a, m
	type(value_t), intent(inout) :: res
	logical, intent(out) :: ok

	ok = .true.
	if (op == RED_MIN .or. op == RED_MAX) then
		if (.not. any(m%bool(1: m%len_))) then
			call throw_minmax_empty(state, op)
			ok = .false.
			return
		end if
	end if

	res%type = a%type
	select case (a%type)
	case (i32_type)
		select case (op)
		case (RED_SUM)
			res%sca%i32 = sum(a%i32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MIN)
			res%sca%i32 = minval(a%i32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MAX)
			res%sca%i32 = maxval(a%i32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_PRODUCT)
			res%sca%i32 = product(a%i32(1: a%len_), mask = m%bool(1: m%len_))
		end select

	case (i64_type)
		select case (op)
		case (RED_SUM)
			res%sca%i64 = sum(a%i64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MIN)
			res%sca%i64 = minval(a%i64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MAX)
			res%sca%i64 = maxval(a%i64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_PRODUCT)
			res%sca%i64 = product(a%i64(1: a%len_), mask = m%bool(1: m%len_))
		end select

	case (f32_type)
		select case (op)
		case (RED_SUM)
			res%sca%f32 = sum(a%f32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MIN)
			res%sca%f32 = minval(a%f32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MAX)
			res%sca%f32 = maxval(a%f32(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_PRODUCT)
			res%sca%f32 = product(a%f32(1: a%len_), mask = m%bool(1: m%len_))
		end select

	case (f64_type)
		select case (op)
		case (RED_SUM)
			res%sca%f64 = sum(a%f64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MIN)
			res%sca%f64 = minval(a%f64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_MAX)
			res%sca%f64 = maxval(a%f64(1: a%len_), mask = m%bool(1: m%len_))
		case (RED_PRODUCT)
			res%sca%f64 = product(a%f64(1: a%len_), mask = m%bool(1: m%len_))
		end select

	end select

end subroutine reduce_mask_scalar

!===============================================================================

function format_value(val, spec) result(s)

	! One value of format(), with the spec of its placeholder.  An array is its
	! elements separated by spaces, with a line break after each row.  An enum or
	! a struct is formatted as the string that str() makes of it

	type(value_t), intent(in) :: val
	character(len = *), intent(in) :: spec
	character(len = :), allocatable :: s

	!********

	integer :: i
	type(rt_str_t), allocatable :: items(:)

	if (val%type == array_type) then

		allocate(items(val%array%len_))
		do i = 1, size(items)
			select case (val%array%type)
			case (i32_type)
				items(i)%s = rt_fmt_i32(val%array%i32(i), spec)
			case (i64_type)
				items(i)%s = rt_fmt_i64(val%array%i64(i), spec)
			case (f32_type)
				items(i)%s = rt_fmt_f32(val%array%f32(i), spec)
			case (f64_type)
				items(i)%s = rt_fmt_f64(val%array%f64(i), spec)
			case (bool_type)
				items(i)%s = rt_fmt_bool(logical(val%array%bool(i)), spec)
			case default
				items(i)%s = rt_fmt_str(val%array%str(i)%s, spec)
			end select
		end do
		s = rt_fmt_join(items, int(val%array%size(1)))
		return

	end if

	select case (val%type)
	case (i32_type)
		s = rt_fmt_i32(val%sca%i32, spec)
	case (i64_type)
		s = rt_fmt_i64(val%sca%i64, spec)
	case (f32_type)
		s = rt_fmt_f32(val%sca%f32, spec)
	case (f64_type)
		s = rt_fmt_f64(val%sca%f64, spec)
	case (bool_type)
		s = rt_fmt_bool(logical(val%sca%bool), spec)
	case (str_type)
		s = rt_fmt_str(val%str%s, spec)
	case default
		s = rt_fmt_str(val%to_str(), spec)
	end select

end function format_value

!===============================================================================

subroutine write_binary(unit_, val, io, ok)

	! Write the raw bytes of a value to a binary file.  A bool is 1 byte, a string
	! is its characters, and an array is its elements in storage order.  `ok` is
	! false for a value of any other type, which has no raw form

	integer, intent(in) :: unit_
	type(value_t), intent(in) :: val
	integer, intent(out) :: io
	logical, intent(out) :: ok

	!********

	integer(kind = 8) :: n

	ok = .true.
	io = 0

	if (val%type == array_type) then
		n = val%array%len_
		select case (val%array%type)
		case (i32_type)
			write(unit_, iostat = io) val%array%i32(1: n)
		case (i64_type)
			write(unit_, iostat = io) val%array%i64(1: n)
		case (f32_type)
			write(unit_, iostat = io) val%array%f32(1: n)
		case (f64_type)
			write(unit_, iostat = io) val%array%f64(1: n)
		case (bool_type)
			write(unit_, iostat = io) merge(1_1, 0_1, val%array%bool(1: n))
		case default
			ok = .false.
		end select
		return
	end if

	select case (val%type)
	case (i32_type)
		write(unit_, iostat = io) val%sca%i32
	case (i64_type)
		write(unit_, iostat = io) val%sca%i64
	case (f32_type)
		write(unit_, iostat = io) val%sca%f32
	case (f64_type)
		write(unit_, iostat = io) val%sca%f64
	case (bool_type)
		write(unit_, iostat = io) merge(1_1, 0_1, val%sca%bool)
	case (str_type)
		write(unit_, iostat = io) val%str%s
	case default
		ok = .false.
	end select

end subroutine write_binary

!===============================================================================

subroutine read_values(intr_id, nargs, args, state, res)

	! std::read_i32(), read_i64(), read_f32(), and read_f64().  With only a file
	! handle, one value is read, and with a count `n` an array of n values is.
	! A text file is read as list-directed input, so the values are separated by
	! blanks, commas, or line breaks, and the rest of the line that the last
	! value is on is skipped.  A binary file is read as raw bytes

	integer, intent(in) :: intr_id, nargs
	type(value_t), intent(in) :: args(:)
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!********

	character(len = :), allocatable :: fn_name, what
	integer :: io, elem
	integer(kind = 8) :: n

	res%type = unknown_type

	select case (intr_id)
	case (INTR_READ_I32)
		fn_name = "read_i32"
		elem = i32_type
	case (INTR_READ_I64)
		fn_name = "read_i64"
		elem = i64_type
	case (INTR_READ_F32)
		fn_name = "read_f32"
		elem = f32_type
	case default
		fn_name = "read_f64"
		elem = f64_type
	end select

	if (.not. args(1)%file_%is_open) then
		call rt_throw(state, err_rt(RC_READLN_NOT_OPEN, fn_name//"() was called for file """// &
			args(1)%file_%name_//""" which is not open"))
		return
	end if
	if (.not. args(1)%file_%mode_read) then
		call rt_throw(state, err_rt(RC_READLN_NOT_READ_MODE, fn_name// &
			"() was called for file """//args(1)%file_%name_// &
			""" which was not opened in read mode ""r"""))
		return
	end if

	n = 1
	if (nargs >= 2) then
		n = args(2)%sca%i32
		if (n < 0) then
			call rt_throw(state, err_rt(RC_READ_FAIL, "cannot "//fn_name// &
				"() a negative number of values (n = "//str(int(n))//")"))
			return
		end if
	end if

	if (nargs < 2) then
		! One value
		res%type = elem
		associate (unit_ => args(1)%file_%unit_)
			if (args(1)%file_%binary) then
				select case (elem)
				case (i32_type)
					read(unit_, iostat = io) res%sca%i32
				case (i64_type)
					read(unit_, iostat = io) res%sca%i64
				case (f32_type)
					read(unit_, iostat = io) res%sca%f32
				case default
					read(unit_, iostat = io) res%sca%f64
				end select
			else
				select case (elem)
				case (i32_type)
					read(unit_, *, iostat = io) res%sca%i32
				case (i64_type)
					read(unit_, *, iostat = io) res%sca%i64
				case (f32_type)
					read(unit_, *, iostat = io) res%sca%f32
				case default
					read(unit_, *, iostat = io) res%sca%f64
				end select
			end if
		end associate

	else
		! An array of n values
		res%type = array_type
		allocate(res%array)
		res%array%type = elem
		res%array%rank = 1
		res%array%len_ = n
		allocate(res%array%size(1))
		res%array%size(1) = n

		select case (elem)
		case (i32_type)
			allocate(res%array%i32(n))
		case (i64_type)
			allocate(res%array%i64(n))
		case (f32_type)
			allocate(res%array%f32(n))
		case default
			allocate(res%array%f64(n))
		end select

		io = 0
		if (n > 0) then
			associate (unit_ => args(1)%file_%unit_)
				if (args(1)%file_%binary) then
					select case (elem)
					case (i32_type)
						read(unit_, iostat = io) res%array%i32
					case (i64_type)
						read(unit_, iostat = io) res%array%i64
					case (f32_type)
						read(unit_, iostat = io) res%array%f32
					case default
						read(unit_, iostat = io) res%array%f64
					end select
				else
					select case (elem)
					case (i32_type)
						read(unit_, *, iostat = io) res%array%i32
					case (i64_type)
						read(unit_, *, iostat = io) res%array%i64
					case (f32_type)
						read(unit_, *, iostat = io) res%array%f32
					case default
						read(unit_, *, iostat = io) res%array%f64
					end select
				end if
			end associate
		end if
	end if

	if (io /= 0) then
		if (io == iostat_end) then
			what = "past the end of the file"
		else
			what = "(iostat = "//str(io)//")"
		end if
		call rt_throw(state, err_rt(RC_READ_FAIL, "cannot "//fn_name//"() from file """// &
			args(1)%file_%name_//""" "//what))
		res%type = unknown_type
	end if

end subroutine read_values

!===============================================================================

end submodule syntran__vm_intr

!===============================================================================
