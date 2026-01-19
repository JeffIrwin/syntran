
!===============================================================================

submodule (syntran__eval_m) syntran__eval_fn

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine eval_fn_call(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	logical :: returned0

	type(value_t), allocatable :: params_tmp(:), locs0(:)

	!print *, ""
	!print *, 'eval fn_call_expr ', node%identifier%text
	!print *, 'fn id_index   = ', node%id_index

	!print *, "num_locs = ", node%num_locs

	if (.not. allocated(node%params)) then
		write(*,*) err_int_prefix//'unexpected user fn'//color_reset
		call internal_error()
	end if

	allocate(params_tmp( size(node%params) ))

	! i think this is technically not different than using an explicit array.
	! we're just using fortran's call stack and recursive calls to
	! eval_fn_call() to mock a whole array with just `returned0` and `returned`.
	returned0 = state%returned  ! push
	state%returned = .false.

	! i don't think we need a stack of "breaked" bools.  that's just loop local,
	! right?

	! TODO: for runtime error logging, push a stack of fn names, line numbers,
	! etc.

	res%type = node%val%type

	!print *, 'res type = ', res%type

	! User-defined function

	!print *, 'fn name = ', node%identifier%text
	!print *, 'fn idx  = ', node%id_index
	!print *, 'node type = ', kind_name(node%val%type)
	!print *, 'alloc params = ', allocated(params)
	!print *, 'size params = ', size(node%params)
	!print *, 'param ids = ', params

	! Pass by value by default.  Arguments are evaluated and their values are
	! copied to the fn parameters

	! Deeply-nested fn calls can crash without the tmp value for the
	! pass-by-value case.  idk why i can't just eval directly into the state var
	! like commented above :(.  probably state var type is getting cleared by
	! passing it to an intent(out) arg? more likely, nested fn calls basically
	! create a stack in which we store each nested arg in different copies of
	! tmp.  if you try to store them all in the same state var at multiple stack
	! levels it breaks?
	!
	! This also seems to have led to a dramatic perf improvement for intel
	! compilers in commit 324ad414, running full tests in ~25 minutes instead of
	! 50.  gfortran perf remains good and unchanged

	! Make two passes.  First eval values, then move references.  If a var shows
	! up in two different args, both as a ref and a val, moving it first will
	! crash in a single pass
	!
	! I think this method of pass-by-reference will not work with
	! multi-threading, not that I ever plan to multi-thread syntran.  The data
	! is "moved", so multiple concurrent fns cannot have refs to the same data.
	! Perhaps pointers could allow that but fortran pointers are risky

	! TODO: does this do what I think it does if you pass-by-ref for one arg and
	! then increment that same var in a later arg?  Seems like a bad idea from
	! the user's end no matter how I handle it.  For example:
	!
	!     let res = my_fn(&x, (x += 1));

	do i = 1, size(node%params)
		if (.not. node%is_ref(i)) then
			! Pass-by-value
			call syntax_eval(node%args(i), state, params_tmp(i))
		end if
	end do

	do i = 1, size(node%params)
		if (node%is_ref(i)) then

			! Move arg in for pass-by-reference
			if (node%args(i)%is_loc) then
				call value_move(state%locs%vals( node%args(i)%id_index ), params_tmp(i))
			else
				call value_move(state%vars%vals( node%args(i)%id_index ), params_tmp(i))
			end if

		end if
	end do

	! Push/pop a stack of local vars, similar to returned0 stack
	if (allocated(state%locs%vals)) call move_alloc(state%locs%vals, locs0)

	! Push local var stack after evaluating args.  Arg evaluation can involve
	! recursive fn calls, so a tmp params array is needed here
	allocate(state%locs%vals( node%num_locs ))
	do i = 1, size(node%params)
		call value_move(params_tmp(i), state%locs%vals( node%params(i) ))
	end do

	! Finally, evaluate the fn body
	call syntax_eval(state%fns%fns( node%id_index )%node%body, state, res)

	!print *, "res rank = ", res%array%rank
	!print *, 'res = ', res%to_str()

	! This is a runtime stopgap check that every fn returns, until (?) i can
	! figure out parse-time return branch checking.  Checking for unreachable
	! statements after returns also seems hard
	if (.not. state%returned) then
		write(*,*) err_int_prefix//"reached end of function `", &
			node%identifier%text, "` without a return statement"//color_reset
		call internal_error()
	end if

	! Move out pass-by-ref args/params into params_tmp
	do i = 1, size(node%params)
		if (.not. node%is_ref(i)) cycle
		call value_move(state%locs%vals( node%params(i) ), params_tmp(i))
	end do

	state%returned = returned0  ! pop

	!print *, "popping runtime state stack"
	if (allocated(locs0)) call move_alloc(locs0, state%locs%vals)

	do i = 1, size(node%params)
		if (.not. node%is_ref(i)) cycle

		if (node%args(i)%is_loc) then
			call value_move(params_tmp(i), state%locs%vals( node%args(i)%id_index ))
		else
			call value_move(params_tmp(i), state%vars%vals( node%args(i)%id_index ))
		end if

	end do

end subroutine eval_fn_call

!===============================================================================

recursive module subroutine eval_fn_call_intr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	character :: char_
	character(len = :), allocatable :: color, mode, status_, resolved_path

	double precision, parameter :: LOG_E_2 = log(2.d0)
	real, parameter :: LOG_E_2F = log(2.0)

	integer :: i, io

	type(char_vector_t) :: str_

	type(value_t) :: arg, arg1, arg2

	!print *, 'eval fn_call_intr_expr'
	!print *, 'fn identifier = ', node%identifier%text
	!print *, 'fn id_index   = ', node%id_index

	res%type = node%val%type

	!print *, 'res type = ', res%type

	! Intrinsic fns
	select case (node%identifier%text)
	!********
	case ("0exp_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = exp(arg1%sca%f32)

	case ("0exp_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = exp(arg1%sca%f64)

	case ("0exp_f32_arr")

		call syntax_eval(node%args(1), state, arg1)

		! This requires an explicit call to mold() to copy array meta-data.  The
		! similar fn 0i32_arr already calls mold() via to_i32_array()

		res%array = mold(arg1%array, f32_type)
		res%array%f32 = exp(arg1%array%f32)

	case ("0exp_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = exp(arg1%array%f64)

	!********
	case ("0log_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = log(arg1%sca%f32)

	case ("0log_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = log(arg1%sca%f64)

	case ("0log_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = log(arg1%array%f32)

	case ("0log_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = log(arg1%array%f64)

	!********
	case ("0log10_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = log10(arg1%sca%f32)

	case ("0log10_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = log10(arg1%sca%f64)

	case ("0log10_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = log10(arg1%array%f32)

	case ("0log10_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = log10(arg1%array%f64)

	!********
	case ("0log2_f32")

		! TODO: there is an extra division operation here compared to other base
		! log fns.  Is there a more efficient implementation? Shell out to c?
		! Implement log2 myself?
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = log(arg1%sca%f32) / LOG_E_2F

	case ("0log2_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = log(arg1%sca%f64) / LOG_E_2

	case ("0log2_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = log(arg1%array%f32) / LOG_E_2F

	case ("0log2_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = log(arg1%array%f64) / LOG_E_2

	!********
	case ("0sqrt_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = sqrt(arg1%sca%f32)

	case ("0sqrt_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = sqrt(arg1%sca%f64)

	case ("0sqrt_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = sqrt(arg1%array%f32)

	case ("0sqrt_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = sqrt(arg1%array%f64)

	!********
	case ("0abs_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = abs(arg1%sca%f32)

	case ("0abs_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = abs(arg1%sca%f64)

	case ("0abs_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = abs(arg1%array%f32)

	case ("0abs_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = abs(arg1%array%f64)

	!********
	case ("0abs_i32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%i32 = abs(arg1%sca%i32)

	case ("0abs_i64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = abs(arg1%sca%i64)

	case ("0abs_i32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, i32_type)
		res%array%i32 = abs(arg1%array%i32)

	case ("0abs_i64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, i64_type)
		res%array%i64 = abs(arg1%array%i64)

	!********
	case ("0cos_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = cos(arg1%sca%f32)

	case ("0cos_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = cos(arg1%sca%f64)

	case ("0cos_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = cos(arg1%array%f32)

	case ("0cos_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = cos(arg1%array%f64)

	!********
	case ("0sin_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = sin(arg1%sca%f32)

	case ("0sin_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = sin(arg1%sca%f64)

	case ("0sin_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = sin(arg1%array%f32)

	case ("0sin_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = sin(arg1%array%f64)

	!********
	case ("0tan_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = tan(arg1%sca%f32)

	case ("0tan_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = tan(arg1%sca%f64)

	case ("0tan_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = tan(arg1%array%f32)

	case ("0tan_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = tan(arg1%array%f64)

	!********
	case ("0cosd_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = cosd(arg1%sca%f32)

	case ("0cosd_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = cosd(arg1%sca%f64)

	case ("0cosd_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = cosd(arg1%array%f32)

	case ("0cosd_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = cosd(arg1%array%f64)

	!********
	case ("0sind_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = sind(arg1%sca%f32)

	case ("0sind_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = sind(arg1%sca%f64)

	case ("0sind_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = sind(arg1%array%f32)

	case ("0sind_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = sind(arg1%array%f64)

	!********
	case ("0tand_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = tand(arg1%sca%f32)

	case ("0tand_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = tand(arg1%sca%f64)

	case ("0tand_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = tand(arg1%array%f32)

	case ("0tand_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = tand(arg1%array%f64)

	!********
	case ("0acos_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = acos(arg1%sca%f32)

	case ("0acos_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = acos(arg1%sca%f64)

	case ("0acos_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = acos(arg1%array%f32)

	case ("0acos_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = acos(arg1%array%f64)

	!********
	case ("0asin_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = asin(arg1%sca%f32)

	case ("0asin_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = asin(arg1%sca%f64)

	case ("0asin_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = asin(arg1%array%f32)

	case ("0asin_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = asin(arg1%array%f64)

	!********
	case ("0atan_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = atan(arg1%sca%f32)

	case ("0atan_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = atan(arg1%sca%f64)

	case ("0atan_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = atan(arg1%array%f32)

	case ("0atan_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = atan(arg1%array%f64)

	!********
	case ("0acosd_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = acosd(arg1%sca%f32)

	case ("0acosd_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = acosd(arg1%sca%f64)

	case ("0acosd_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = acosd(arg1%array%f32)

	case ("0acosd_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = acosd(arg1%array%f64)

	!********
	case ("0asind_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = asind(arg1%sca%f32)

	case ("0asind_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = asind(arg1%sca%f64)

	case ("0asind_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = asind(arg1%array%f32)

	case ("0asind_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = asind(arg1%array%f64)

	!********
	case ("0atand_f32")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = atand(arg1%sca%f32)

	case ("0atand_f64")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = atand(arg1%sca%f64)

	case ("0atand_f32_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f32_type)
		res%array%f32 = atand(arg1%array%f32)

	case ("0atand_f64_arr")

		call syntax_eval(node%args(1), state, arg1)
		res%array = mold(arg1%array, f64_type)
		res%array%f64 = atand(arg1%array%f64)

	!********
	case ("0min_i32")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i32 = arg%sca%i32

		! Note that min/max/println etc. are variadic, so we loop to
		! size(node%args) instead of size(node%params)
		!
		! TODO: add elemental array overloads for min/max (*not* minval/maxval)

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%i32 = min(res%sca%i32, arg%sca%i32)
		end do

	case ("0min_i64")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i64 = arg%sca%i64

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%i64 = min(res%sca%i64, arg%sca%i64)
		end do

	case ("0min_f32")

		call syntax_eval(node%args(1), state, arg)
		res%sca%f32 = arg%sca%f32

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%f32 = min(res%sca%f32, arg%sca%f32)
		end do

	case ("0min_f64")

		call syntax_eval(node%args(1), state, arg)
		res%sca%f64 = arg%sca%f64

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%f64 = min(res%sca%f64, arg%sca%f64)
		end do

	!********
	case ("0max_i32")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i32 = arg%sca%i32

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%i32 = max(res%sca%i32, arg%sca%i32)
		end do

	case ("0max_i64")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i64 = arg%sca%i64

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%i64 = max(res%sca%i64, arg%sca%i64)
		end do

	case ("0max_f32")

		call syntax_eval(node%args(1), state, arg)
		res%sca%f32 = arg%sca%f32

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%f32 = max(res%sca%f32, arg%sca%f32)
		end do

	case ("0max_f64")

		call syntax_eval(node%args(1), state, arg)
		res%sca%f64 = arg%sca%f64

		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			res%sca%f64 = max(res%sca%f64, arg%sca%f64)
		end do

	!********
	case ("println")

		! TODO: if struct, pass a struct_t as opt arg to to_str(), which
		! contains member names that can then be printed
		!
		! Actually it's a huge pain to pass structs dict from parser to evaler.
		! I tried for a bit but stashed it.  I will probably need to do this
		! eventually anyway for interactive runs with structs.  I can see why
		! rust requires #derive[debug] to allow printing a whole struct

		! Build the output string first, then write it all at once
		str_ = new_char_vector()
		do i = 1, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			call str_%push(arg%to_str())
		end do

		!$omp critical(stdout_write)
		write(output_unit, '(a)') str_%trim()
		!$omp end critical(stdout_write)

		!res%sca%i32 = 0

	case ("str")

		str_ = new_char_vector()
		do i = 1, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			call str_%push(arg%to_str())
		end do
		res%sca%str%s = str_%trim()

	case ("len")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i64 = len(arg%sca%str%s, 8)

	case ("repeat")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)
		res%sca%str%s = repeat(arg1%sca%str%s, arg2%sca%i32)

	case ("parse_i32")

		call syntax_eval(node%args(1), state, arg)
		call parse_i32_dec(arg%sca%str%s, res%sca%i32, io)
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i32() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_i64")

		call syntax_eval(node%args(1), state, arg)
		call parse_i64_dec(arg%sca%str%s, res%sca%i64, io)
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i64() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_f32")

		! TODO: trim "f" literal suffix if present

		call syntax_eval(node%args(1), state, arg)
		call parse_f32(arg%sca%str%s, res%sca%f32, io)
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_f32() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_f64")

		call syntax_eval(node%args(1), state, arg)
		call parse_f64(arg%sca%str%s, res%sca%f64, io)
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_f64() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("char")

		! The `i32()` intrinsic uses iachar(), so this should use achar(), not
		! char().  While achar() is guaranteed to be ASCII, char() could be some
		! other character set

		call syntax_eval(node%args(1), state, arg)
		!res%sca%str%s = char(arg%sca%i32)
		res%sca%str%s = achar(arg%sca%i32)

	case ("0i32_sca")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i32 = arg%to_i32()

	case ("0i32_arr")

		call syntax_eval(node%args(1), state, arg)
		res%array = arg%to_i32_array()

	case ("0i64_sca")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i64 = arg%to_i64()

	case ("0i64_arr")

		call syntax_eval(node%args(1), state, arg)
		res%array = arg%to_i64_array()

	case ("open")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)

		mode = arg2%sca%str%s
		!print *, "mode = ", mode

		do i = 1, len(mode)
			char_ = mode(i: i)
			select case (char_)
			case ("r")
				res%sca%file_%mode_read = .true.

			case ("w")
				res%sca%file_%mode_write = .true.

			case default
				write(*,*) err_rt_prefix//"bad file mode character """// &
					char_//""""//color_reset
				call internal_error()

			end select
		end do

		if (res%sca%file_%mode_read .and. res%sca%file_%mode_write) then
			! Maybe "rw" mode could be allowed in the future, but i'm not sure
			! what a useful application would be.  Perhaps if I exposed a
			! rewind() or seek() fn
			write(*,*) err_rt_prefix//"cannot open file """//arg1%sca%str%s &
				//""" in combined read/write mode """//mode//""""
			call internal_error()
		end if

		if (res%sca%file_%mode_read) then
			status_ = "old"
		else
			status_ = "unknown"
		end if

		! Resolve relative paths using src_dir from state
		! This is the key change for thread-safety
		resolved_path = resolve_path(state%src_dir, arg1%sca%str%s)

		!$omp critical(file_open)
		open(newunit = res%sca%file_%unit_, file = resolved_path, &
			status = status_, iostat = io)
		!$omp end critical(file_open)
		!print *, "io = ", io

		if (io /= 0) then
			! Decode fortran iostat codes in message?  I just looked up the docs
			! and there's not much about open iostat other than 0 is success.
			! Read iostats are more descriptive
			write(*,*) err_rt_prefix//"cannot open file """//resolved_path//""""
			write(*,*) "iostat = ", str(io)
			call internal_error()
		end if

		!print *, 'opened unit ', res%sca%file_%unit_
		res%sca%file_%name_ = arg1%sca%str%s  ! Keep original name for error messages
		res%sca%file_%eof = .false.
		res%sca%file_%is_open = .true.

	case ("readln")

		call syntax_eval(node%args(1), state, arg1)

		if (.not. arg1%sca%file_%is_open) then
			write(*,*) err_rt_prefix//"readln() was called for file """ &
				//arg1%sca%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (.not. arg1%sca%file_%mode_read) then
			write(*,*) err_rt_prefix//"readln() was called for file """ &
				//arg1%sca%file_%name_//""" which was not opened in read mode ""r"""
			call internal_error()
		end if

		!print *, "reading from unit", arg1%sca%file_%unit_
		res%sca%str%s = read_line(arg1%sca%file_%unit_, io)
		!print *, 'done reading'

		! This could be a very dangerous side effect!  The file argument of
		! readln() acts as an out-arg:  it's eof flag can be toggled on.  I
		! don't have out-args anywhere else so I may want to rethink this
		! :exploding-head:
		!
		! writeln() does not need to mess with the vars struct like this

		!!print *, 'ident = ', node%args(1)%identifier%text
		!!state%vars%vals(node%id_index) = res

		if (io == iostat_end) then
			!arg1%sca%file_%eof = .true.
			!id = node%id_index

			!print *, "node is_loc = ", node%is_loc
			!print *, "arg  is_loc = ", node%args(1)%is_loc

			if (node%args(1)%is_loc) then
				state%locs%vals(node%args(1)%id_index)%sca%file_%eof = .true.
			else
				state%vars%vals(node%args(1)%id_index)%sca%file_%eof = .true.
			end if

		else if (io == iostat_eor) then
			! Do nothing

		else if (io /= 0) then
			! This can get thrown if you attempt to read past EOF.  Maybe add a
			! more specific message ahead of read attempt in this case?
			write(*,*) err_rt_prefix//"cannot readln() from file """ &
				//arg1%sca%file_%name_//""""
			write(*,*) "iostat = ", str(io)
			call internal_error()

		end if
		!print *, 'eof   = ', arg1%sca%file_%eof

	case ("writeln")

		call syntax_eval(node%args(1), state, arg1)

		if (.not. arg1%sca%file_%is_open) then
			write(*,*) err_rt_prefix//"writeln() was called for file """ &
				//arg1%sca%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (.not. arg1%sca%file_%mode_write) then
			write(*,*) err_rt_prefix//"writeln() was called for file """ &
				//arg1%sca%file_%name_//""" which was not opened in write mode ""w"""
			call internal_error()
		end if

		!print *, 'writing to unit ', arg1%sca%file_%unit_
		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			write(arg1%sca%file_%unit_, '(a)', advance = 'no') arg%to_str()
		end do
		write(arg1%sca%file_%unit_, *)

	case ("eof")

		call syntax_eval(node%args(1), state, arg1)

		if (.not. arg1%sca%file_%is_open) then
			write(*,*) err_rt_prefix//"eof() was called for file """ &
				//arg1%sca%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (.not. arg1%sca%file_%mode_read) then
			write(*,*) err_rt_prefix//"eof() was called for file """ &
				//arg1%sca%file_%name_//""" which was not opened in read mode ""r"""
			call internal_error()
		end if

		!print *, "checking eof for unit", arg1%sca%file_%unit_
		res%sca%bool = arg1%sca%file_%eof

		!print *, 'eof fn = ', arg1%sca%file_%eof

	case ("close")
		call syntax_eval(node%args(1), state, arg1)

		if (.not. arg1%sca%file_%is_open) then
			write(*,*) err_rt_prefix//"close() was called for file """ &
				//arg1%sca%file_%name_//""" which is not open"
			call internal_error()
		end if
		if (node%args(1)%is_loc) then
			state%locs%vals(node%args(1)%id_index)%sca%file_%is_open = .false.
		else
			state%vars%vals(node%args(1)%id_index)%sca%file_%is_open = .false.
		end if

		!print *, 'closing unit ', arg1%sca%file_%unit_
		close(arg1%sca%file_%unit_)

	case ("exit")

		call syntax_eval(node%args(1), state, arg)

		io = arg%sca%i32
		if (io == 0) then
			color = fg_bright_green
		else
			color = fg_bold_bright_red
		end if

		write(*,*) color//'Exiting syntran with status '// &
			str(io)//color_reset

		call exit(io)

	case ("size")

		!print *, "evaluating size fn"
		call syntax_eval(node%args(1), state, arg1)

		! Is the `dim` arg present?
		if (size(node%args) == 2) then
			call syntax_eval(node%args(2), state, arg2)

			!print *, "arg 1 type = ", kind_name(node%args(1)%kind)
			!print *, "allocated = ", allocated(arg1%array)
			!print *, "arg2 = ", arg2%sca%i32
			!print *, "arg1 type = ", kind_name(arg1%type)

			if (arg2%sca%i32 < 0 .or. arg2%sca%i32 >= arg1%array%rank) then
				! TODO: re-think runtime errors.  A different prefix here
				! besides err_int_prefix helps, but context should be given if
				! possible like for parser/lexer error diagnostics
				write(*,*) err_rt_prefix//"rank mismatch in size() call"//color_reset
				!print *, "rank     = ", arg1%array%rank
				!print *, "size arg = ", arg2%sca%i32
				call internal_error()
			end if

			!print *, "allocated(size) = ", allocated(arg1%array%size)
			res%sca%i64 = int(arg1%array%size( arg2%sca%i32 + 1 ))
		else
			res%sca%i64 = int(arg1%array%len_)
		end if

	case ("count")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = count(arg1%array%bool)

	case ("0minval_i32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i32 = minval(arg1%array%i32)

	case ("0minval_i64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = minval(arg1%array%i64)

	case ("0minval_f32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = minval(arg1%array%f32)

	case ("0minval_f64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = minval(arg1%array%f64)

	case ("0maxval_i32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i32 = maxval(arg1%array%i32)

	case ("0maxval_i64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = maxval(arg1%array%i64)

	case ("0maxval_f32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = maxval(arg1%array%f32)

	case ("0maxval_f64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = maxval(arg1%array%f64)

	case ("0sum_i32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i32 = sum(arg1%array%i32)

	case ("0sum_i64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = sum(arg1%array%i64)

	case ("0sum_f32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = sum(arg1%array%f32)

	case ("0sum_f64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = sum(arg1%array%f64)

	case ("0product_i32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i32 = product(arg1%array%i32)

	case ("0product_i64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = product(arg1%array%i64)

	case ("0product_f32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = product(arg1%array%f32)

	case ("0product_f64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = product(arg1%array%f64)

	case ("0norm2_f32")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f32 = norm2(arg1%array%f32)

	case ("0norm2_f64")
		call syntax_eval(node%args(1), state, arg1)
		res%sca%f64 = norm2(arg1%array%f64)

	case ("0dot_f32")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)
		res%sca%f32 = dot_product(arg1%array%f32, arg2%array%f32)

	case ("0dot_f64")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)
		res%sca%f64 = dot_product(arg1%array%f64, arg2%array%f64)

	case ("0dot_i32")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)
		res%sca%i32 = dot_product(arg1%array%i32, arg2%array%i32)

	case ("0dot_i64")

		call syntax_eval(node%args(1), state, arg1)
		call syntax_eval(node%args(2), state, arg2)
		res%sca%i64 = dot_product(arg1%array%i64, arg2%array%i64)

	case ("all")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%bool = all(arg1%array%bool)

		! Might not be strictly necessary now that %array is allocatable
		! instead of pointable
		!deallocate(arg1%array)

	case ("any")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%bool = any(arg1%array%bool)

	case ("args")

		! Return script arguments passed after `--` as a string array
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

	case default

		!print *, 'fn name = ', node%identifier%text
		write(*,*) err_int_prefix//'unexpected intr fn'//color_reset
		call internal_error()

		!print *, 'fn idx  = ', node%id_index
		!print *, 'node type = ', node%val%type
		!print *, 'size params = ', size(node%params)
		!print *, 'param ids = ', node%params

	end select

end subroutine eval_fn_call_intr

!===============================================================================

end submodule syntran__eval_fn

!===============================================================================
