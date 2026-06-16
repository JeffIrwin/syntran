
!===============================================================================

submodule (syntran__vm_m) syntran__vm_intr

	! M6: integer-id intrinsic dispatch for the bytecode VM.
	!
	! vm_call_intr is called by vm_exec when OP_CALL_INTR fires in native mode
	! (b >= 0).  Args have already been popped from the operand stack in reverse
	! order and collected into args(1:nargs).
	!
	! CALL_INTR_NODE fallback cases (readln, close) are NOT routed here; they are
	! handled directly in vm_exec by calling eval_fn_call_intr with the stored node.

	implicit none

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

	character :: char_

	character(len = :), allocatable :: mode_, status_, resolved_path_

	type(char_vector_t) :: str_

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
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = repeat(args(1)%str%s, args(2)%sca%i32)

	case (INTR_PARSE_I32)
		res%type = i32_type
		read(args(1)%str%s, *, iostat = io) res%sca%i32
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i32() for argument `"// &
				args(1)%str%s//"`"//color_reset
			call internal_error()
		end if

	case (INTR_PARSE_I64)
		res%type = i64_type
		read(args(1)%str%s, *, iostat = io) res%sca%i64
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i64() for argument `"// &
				args(1)%str%s//"`"//color_reset
			call internal_error()
		end if

	case (INTR_PARSE_F32)
		res%type = f32_type
		read(args(1)%str%s, *, iostat = io) res%sca%f32
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_f32() for argument `"// &
				args(1)%str%s//"`"//color_reset
			call internal_error()
		end if

	case (INTR_PARSE_F64)
		res%type = f64_type
		read(args(1)%str%s, *, iostat = io) res%sca%f64
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_f64() for argument `"// &
				args(1)%str%s//"`"//color_reset
			call internal_error()
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
		res%file_%mode_read  = .false.
		res%file_%mode_write = .false.
		mode_ = args(2)%str%s
		do i = 1, len(mode_)
			char_ = mode_(i: i)
			select case (char_)
			case ("r")
				res%file_%mode_read = .true.
			case ("w")
				res%file_%mode_write = .true.
			case default
				write(*,*) err_rt_prefix//"bad file mode character """// &
					char_//""""//color_reset
				call internal_error()
			end select
		end do
		if (res%file_%mode_read .and. res%file_%mode_write) then
			write(*,*) err_rt_prefix//"cannot open file """//args(1)%str%s// &
				""" in combined read/write mode """//mode_//""""
			call internal_error()
		end if
		if (res%file_%mode_read) then
			status_ = "old"
		else
			status_ = "unknown"
		end if
		resolved_path_ = resolve_path(state%src_dir, args(1)%str%s)
		open(newunit = res%file_%unit_, file = resolved_path_, &
			status = status_, iostat = io)
		if (io /= 0) then
			write(*,*) err_rt_prefix//"cannot open file """//resolved_path_//""""
			write(*,*) "iostat = ", str(io)
			call internal_error()
		end if
		res%file_%name_ = args(1)%str%s
		res%file_%eof   = .false.
		res%file_%is_open = .true.

	case (INTR_WRITELN)
		! args(1) = file handle, args(2:) = values to write
		res%type = void_type
		if (.not. args(1)%file_%is_open) then
			write(*,*) err_rt_prefix//"writeln() was called for file """// &
				args(1)%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (.not. args(1)%file_%mode_write) then
			write(*,*) err_rt_prefix//"writeln() was called for file """// &
				args(1)%file_%name_//""" which was not opened in write mode ""w"""
			call internal_error()
		end if
		do i = 2, nargs
			write(args(1)%file_%unit_, '(a)', advance = 'no') args(i)%to_str()
		end do
		write(args(1)%file_%unit_, *)

	case (INTR_EOF)
		! args(1) = file handle (read only — no writeback needed)
		res%type = bool_type
		if (.not. args(1)%file_%is_open) then
			write(*,*) err_rt_prefix//"eof() was called for file """// &
				args(1)%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (.not. args(1)%file_%mode_read) then
			write(*,*) err_rt_prefix//"eof() was called for file """// &
				args(1)%file_%name_//""" which was not opened in read mode ""r"""
			call internal_error()
		end if
		res%sca%bool = args(1)%file_%eof

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
				write(*,*) err_rt_prefix//"rank mismatch in size() call"//color_reset
				call internal_error()
			end if
			res%sca%i64 = int(args(1)%array%size(args(2)%sca%i32 + 1))
		else
			res%sca%i64 = int(args(1)%array%len_)
		end if

	case (INTR_COUNT)
		res%type = i64_type
		res%sca%i64 = count(args(1)%array%bool)

	!==== Reductions ============================================================

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

	case (INTR_ALL)
		res%type = bool_type; res%sca%bool = all(args(1)%array%bool)

	case (INTR_ANY)
		res%type = bool_type; res%sca%bool = any(args(1)%array%bool)

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

	case (INTR_RESHAPE)
		! std::reshape(source, shape) -- copy flat buffer, overwrite shape metadata.
		! args(1) = source array, args(2) = shape (i32, rank-1)

		! Runtime guard: product of new shape must equal source element count
		if (product(int(args(2)%array%i32(1:args(2)%array%len_), 8)) /= args(1)%array%len_) then
			write(*,*) err_rt_prefix//"reshape size mismatch"//color_reset
			call internal_error()
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

	case default
		write(*,*) 'VM: unknown intr_id in vm_call_intr: ', intr_id
		call internal_error()

	end select

end subroutine vm_call_intr

!===============================================================================

end submodule syntran__vm_intr

!===============================================================================
