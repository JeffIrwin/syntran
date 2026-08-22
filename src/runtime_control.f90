
!===============================================================================

submodule (syntran__runtime_m) syntran__runtime_control

	! Slice-LHS/compound assignment and array-literal construction.
	! Formerly part of the AST walker (eval_control.f90); eval_for_statement/
	! eval_while_statement/eval_if_statement/eval_return_statement/
	! eval_block_statement/eval_translation_unit/eval_use_statement/
	! eval_enum_cast_expr were dropped here since the VM has its own native
	! OP_JUMP/OP_FOR_*/OP_CALL/OP_RET control flow and a const-pool-based
	! OP_ENUM_CAST (compile_ctrl.f90) -- runtime_m's module docstring has the
	! full picture

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine eval_assignment_expr(node, state, res, rhs_in, slots)

	! eval_assignment_expr's only caller is the VM's OP_STORE_SLICE handler
	! (vm_exec.f90), which always compiles and pushes node%right and, when
	! node%lsubscripts is allocated, node%lsubscripts(:)'s bound
	! sub-expressions -- so rhs_in and slots are always supplied (slots may
	! be a zero-length window when there's nothing to pop)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	type(value_t), intent(in) :: rhs_in
	type(value_t), intent(in) :: slots(:)

	!********

	integer :: rank_slice, id, type_, nelem
	integer(kind = 8) :: i8, j8, index_, len8, size_i, il, iu, sstep
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:), &
		size_tmp(:)

	logical :: has_char_sub

	type(i64_vector_t), allocatable :: asubs(:)
	type(value_t) :: array_val, tmp, tmp_array

	!print *, "eval assignment_expr"
	!print *, "node identifier = ", node%identifier%text
	!id = node%id_index
	!print *, 'lhs type = ', kind_name( state%vars%vals(id)%type )
	!if (state%vars%vals(id)%type == struct_type) then
	!if (allocated( node%member )) then
	!	print *, "mem index = ", node%member%id_index
	!end if

	if (allocated( node%member )) then
		! Unreachable: dot-member assignment (`a.b = x`, `a.b += x`) compiles
		! to OP_STORE_MEMBER (compile_ctrl.f90's assignment_expr case),
		! which calls get_val/set_val directly and never reaches
		! eval_assignment_expr at all
		write(*,*) err_int(IC_UNEXPECTED_ARRAY_KIND, &
			'unreachable: dot-member assignment reached eval_assignment_expr')
		call internal_error()

	else if (.not. allocated(node%lsubscripts)) then

		id = node%id_index

		!! This deallocation will cause a crash when an array appears on both
		!! the LHS and RHS of fn_call assignment, e.g. `dv = diff_(dv, i)` in
		!! AOC 2023/09
		!if (allocated(state%vars%vals)) then
		!if (allocated(state%vars%vals(id)%array)) then
		!	!print *, "deallocating lhs array"
		!	deallocate(state%vars%vals(id)%array)
		!end if
		!end if

		!print *, 'scalar apply_assign_op'

		! Eval the RHS
		res = rhs_in

		! TODO: test int/float casting.  It should be an error during
		! parsing

		!print *, 'lhs type = ', kind_name( state%vars%vals(id)%type )

		!print *, "apply_assign_op is_loc = ", node%is_loc
		if (node%is_loc) then
			!print *, "val type = ", kind_name( state%locs%vals(id)%type )
			call apply_assign_op(state%locs%vals(id), res, node%op)
			res = state%locs%vals(id)
		else
			call apply_assign_op(state%vars%vals(id), res, node%op)

			! For compound assignment, ensure that the LHS is returned
			!print *, 'setting res again'
			res = state%vars%vals(id)
			!print *, 'done'
		end if

		!print *, "node identifier = ", node%identifier%text

		! The difference between let and assign is inserting into the
		! current scope (let) vs possibly searching parent scopes (assign).
		! During evaluation we don't need any extra logic for scoping.  The
		! parser has already assigned a separate id_index for each
		! identifier at each scope level

	else
		id = node%id_index
		if (node%is_loc) then
			type_ = state%locs%vals(id)%type
		else
			type_ = state%vars%vals(id)%type
		end if

		!print *, 'LHS array subscript assignment'
		!print *, 'LHS type = ', kind_name(type_)

		! Detect optional char-rank subscript on a string array.
		! Use nested ifs to guard %array%type (no short-circuit guarantee).
		nelem = 0
		has_char_sub = .false.
		if (allocated(node%lsubscripts) .and. type_ == array_type) then
			if (node%is_loc) then
				if (state%locs%vals(id)%array%type == str_type) then
					nelem = state%locs%vals(id)%array%rank
					has_char_sub = size(node%lsubscripts) == nelem + 1
				end if
			else
				if (state%vars%vals(id)%array%type == str_type) then
					nelem = state%vars%vals(id)%array%rank
					has_char_sub = size(node%lsubscripts) == nelem + 1
				end if
			end if
		end if

		! Eval the RHS.  I should probably rename `res` to `rhs` here like I did
		! with get_val() for dot exprs above, because it's not really the result
		! yet in cases of compound assignment
		res = rhs_in

		!print *, 'RHS = ', res%to_str()

		if (type_ == str_type) then
			!print *, 'str_type'

			! TODO: ban compound character substring assignment

			! str_slice_bounds() handles scalar_sub/range_sub/step_sub/all_sub
			! uniformly, so stepped/reversed slice assignment (e.g.
			! s[:-1:] = "olleh") works the same as it does for arrays.
			if (node%is_loc) then
				call str_slice_bounds(node, 1, int(len(state%locs%vals(id)%str%s), 8), &
					state, il, iu, sstep, slots)
			else
				call str_slice_bounds(node, 1, int(len(state%vars%vals(id)%str%s), 8), &
					state, il, iu, sstep, slots)
			end if
			if (state%rt_halt) return

			i8 = il
			j8 = 1
			do while ((sstep > 0 .and. i8 < iu) .or. (sstep < 0 .and. i8 > iu))
				if (node%is_loc) then
					state%locs%vals(id)%str%s(i8+1: i8+1) = res%str%s(j8:j8)
				else
					state%vars%vals(id)%str%s(i8+1: i8+1) = res%str%s(j8:j8)
				end if
				i8 = i8 + sstep
				j8 = j8 + 1
			end do

		else if (has_char_sub) then

			! String array with char subscript assignment.  Apply the char sub
			! to every element selected by the element subscripts.
			if (all(node%lsubscripts(1:nelem)%sub_kind == scalar_sub)) then

				! All element subs scalar: single element
				i8 = subscript_eval(node, state, slots)   ! element flat index
				if (state%rt_halt) return
				call str_arr_char_assign(node, state, res, id, i8, nelem, slots)
				if (state%rt_halt) return

			else

				! Slice element selection: iterate over selected elements
				call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_slice, slots)
				if (state%rt_halt) return
				len8 = 1_8
				do j8 = 1, nelem
					if (allocated(asubs(j8)%v)) then
						size_i = size(asubs(j8)%v)
					else
						size_i = divceil(usubs(j8) - lsubs(j8), ssubs(j8))
						if (lsubs(j8) > usubs(j8) .and. ssubs(j8) > 0) size_i = 0
						if (lsubs(j8) < usubs(j8) .and. ssubs(j8) < 0) size_i = 0
					end if
					len8 = len8 * size_i
				end do

				subs = lsubs
				do j8 = 0, len8 - 1
					if (node%is_loc) then
						index_ = subscript_i32_eval(subs, state%locs%vals(id)%array)
					else
						index_ = subscript_i32_eval(subs, state%vars%vals(id)%array)
					end if
					call str_arr_char_assign(node, state, res, id, index_, nelem, slots)
					if (state%rt_halt) return
					call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
				end do

			end if

		else if (all(node%lsubscripts%sub_kind == scalar_sub)) then

			! Unreachable: an all-scalar subscript assignment (`arr[i][j] = x`,
			! `arr[i][j] += x`) is exactly compile_ctrl.f90's assignment_expr
			! `first` condition, which routes to OP_STORE_IDX/OP_STORE_IDX_NAT/
			! OP_COMPOUND_IDX_NAT -- never OP_STORE_SLICE -- so this never
			! reaches eval_assignment_expr
			write(*,*) err_int(IC_UNEXPECTED_ARRAY_KIND, &
				'unreachable: all-scalar subscript assignment reached eval_assignment_expr')
			call internal_error()

		else

			!print *, "lhs slice assignment"

			if (size(node%lsubscripts) == 1 .and. &
			    node%lsubscripts(1)%sub_kind /= arr_sub) then
				! Rank-1 slice fast path: avoids allocating lsubs/ssubs/usubs/asubs.
				call eval_assign_slice_rank1(node, state, id, res, slots)
				if (state%rt_halt) return
			else

			call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_slice, slots)
			if (state%rt_halt) return
			allocate(size_tmp(rank_slice))

			!print *, "rank     = ", state%vars%vals(id)%array%rank
			!print *, "rank_slice = ", rank_slice

			len8 = 1
			j8 = 1
			do i8 = 1, size(lsubs)
				if (allocated(asubs(i8)%v)) then
					size_i = size(asubs(i8)%v)
				else
					size_i = divceil(usubs(i8) - lsubs(i8), ssubs(i8))

					! Empty step slice?
					!
					! TODO: c.f. step_array cases (literals and for loops) for
					! ways to do this without branching (or at least, without
					! obvious branching)
					if (lsubs(i8) > usubs(i8) .and. ssubs(i8) > 0) size_i = 0
					if (lsubs(i8) < usubs(i8) .and. ssubs(i8) < 0) size_i = 0

				end if

				len8 = len8 * size_i
				if (node%lsubscripts(i8)%sub_kind /= scalar_sub) then
					size_tmp(j8) = size_i
					j8 = j8 + 1
				end if

			end do
			!print *, "len8 = ", len8

			! TODO: some size/shape checking might be needed here between
			! LHS and RHS

			! Scalar rhs
			if (res%type /= array_type) array_val = res

			allocate(tmp_array%array)
			tmp_array%type = array_type
			tmp_array%array%len_ = len8
			tmp_array%array%rank = rank_slice
			tmp_array%array%kind = expl_array
			tmp_array%array%size = size_tmp
			if (node%is_loc) then
				tmp_array%array%type = state%locs%vals(id)%array%type
			else
				tmp_array%array%type = state%vars%vals(id)%array%type
			end if

			! We cannot use mold here because the return value could be a lower
			! rank than the LHS array being sliced, and the RHS may be a scalar.
			! All meta-data usually set by mold is set above on tmp_array
			call allocate_array(tmp_array, len8)

			! Iterate through all subscripts in range and copy to result
			! array
			subs = lsubs
			do i8 = 0, len8 - 1

				!print *, 'subs = ', int(subs, 4)

				! This is confusing.  Maybe rename array_val -> rhs_val and
				! tmp -> lhs_val or something

				if (res%type == array_type) then
					call get_array_val(res%array, i8, array_val)
				end if

				if (node%is_loc) then
					index_ = subscript_i32_eval(subs, state%locs%vals(id)%array)
					call get_array_val(state%locs%vals(id)%array, index_, tmp)
					call apply_assign_op(tmp, array_val, node%op)
					call set_array_val(state%locs%vals(id)%array, index_, tmp)
				else
					index_ = subscript_i32_eval(subs, state%vars%vals(id)%array)
					call get_array_val(state%vars%vals(id)%array, index_, tmp)
					call apply_assign_op(tmp, array_val, node%op)
					call set_array_val(state%vars%vals(id)%array, index_, tmp)
				end if

				! Set the return val too
				call set_array_val(tmp_array%array, i8, tmp)

				!! move conditions out of loop for perf?
				!if (res%type == array_type) then
				!	call set_array_val(res%array, i8, tmp)
				!else
				!
				!	! this makes the res return value a scalar.  Maybe
				!	! not correct for fn return values or paren exprs, at
				!	! least it's not consistent with the way that array rhs
				!	! vals work
				!	res = tmp
				!
				!	! This is illegal in python numpy:
				!	!
				!	! >>> import numpy as np
				!	! >>> a = np.arange(1, 6)
				!	! >>> b = (a[1:4] := 3)
				!	!   File "<stdin>", line 1
				!	!     b = (a[1:4] := 3)
				!	!          ^^^^^^
				!	! SyntaxError: cannot use assignment expressions with subscript
				!	! >>>
				!	!
				!	! Of course, such an assignment is legal as its own
				!	! statement without the "walrus" operator `:=` :
				!	!
				!	! >>> a[1:4] = 3
				!	! >>> a
				!	!     # [1, 3, 3, 3, 5]
				!	!
				!	! I believe it is illegal in python because of the
				!	! ambiguity of what should `b` be if it is assigned.
				!	! Should `b` be the whole `a` array, or just the slice
				!	! `a[1:4]`, or just the scalar `3`?
				!	!
				!	! I think there's a good case to be made that it should
				!	! be the slice `a[1:4]`, which is what syntran does.
				!end if

				call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
			end do

			! Not setting res to anything here leaves the return value as the
			! RHS
			!
			! I think there are valid arguments for all 3 options here
			!
			! - setting to the whole array can trigger a big array copy and it's
			!   not consistent with non-nested assignment
			! - setting to the RHS performs well but it probably isn't the
			!   resulting rank that most users would expect for the scalar case
			! - setting the return value to only the modified slice makes the
			!   most sense imo, but i avoided it before because the code is more
			!   complex, requiring the tmp_array and getting all the size/rank
			!   array meta-data

			!res = state%vars%vals(id)  ! big copy for returing the whole array
			res = tmp_array  ! only return the modified slice

			end if   ! rank-1 fast path / general path

		end if
	end if

contains

	subroutine str_arr_char_assign(node, state, rhs, id, elem_idx, nelem_, slots)

		! Apply the char-rank subscript at lsubscripts(nelem_+1) to the
		! element string at state%{locs|vars}%vals(id)%array%str(elem_idx+1)%s.
		!
		! str_slice_bounds() handles scalar_sub/range_sub/step_sub/all_sub
		! uniformly, so stepped/reversed slice assignment works the same as it
		! does for arrays.  On step == 0, state%rt_halt is set; callers must
		! check it on return.
		!
		! `slots` is forwarded to str_slice_bounds -- see its docstring

		type(syntax_node_t), intent(in)    :: node
		type(state_t),       intent(inout) :: state
		type(value_t),       intent(in)    :: rhs
		integer,             intent(in)    :: id, nelem_
		integer(kind = 8),   intent(in)    :: elem_idx
		type(value_t),       intent(in)    :: slots(:)

		!********

		integer :: isub
		integer(kind = 8) :: il, iu, step, i8, j8

		isub = nelem_ + 1

		if (node%is_loc) then
			call str_slice_bounds(node, isub, &
				int(len(state%locs%vals(id)%array%str(elem_idx+1)%s), 8), &
				state, il, iu, step, slots)
		else
			call str_slice_bounds(node, isub, &
				int(len(state%vars%vals(id)%array%str(elem_idx+1)%s), 8), &
				state, il, iu, step, slots)
		end if
		if (state%rt_halt) return

		i8 = il
		j8 = 1
		do while ((step > 0 .and. i8 < iu) .or. (step < 0 .and. i8 > iu))
			if (node%is_loc) then
				state%locs%vals(id)%array%str(elem_idx+1)%s(i8+1 : i8+1) = rhs%str%s(j8:j8)
			else
				state%vars%vals(id)%array%str(elem_idx+1)%s(i8+1 : i8+1) = rhs%str%s(j8:j8)
			end if
			i8 = i8 + step
			j8 = j8 + 1
		end do

	end subroutine str_arr_char_assign

end subroutine eval_assignment_expr

!===============================================================================

recursive module subroutine eval_array_expr(node, state, res, slots)

	! eval_array_expr's only caller is the VM's OP_NEW_ARRAY handler
	! (vm_exec.f90), which always compiles and pushes node's sub-expressions
	! first (compile_array_expr_slots) -- so slots is always supplied
	! (array_expr_nslots(node)-sized); consumed via the monotonic cursor `k`
	! below in the same order -- see array_expr_nslots' docstring
	! (bytecode.f90) for the per-kind consumption order

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	type(value_t), intent(in) :: slots(:)

	!********

	integer :: i, j, k
	integer(kind = 8) :: i8, j8

	logical :: is_cat

	real(kind = 4) :: f, fstep
	real(kind = 8) :: f64, fstep64

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, elem, &
		step, len_, tmp

	!print *, "starting eval_array_expr()"
	!print *, 'identifier = ', node%identifier%text

	k = 0

	if (node%val%array%kind == step_array) then

		k = k + 1; lbound_ = slots(k)
		k = k + 1; step    = slots(k)
		k = k + 1; ubound_ = slots(k)

		array%type = node%val%array%type

		! If any bound or step is i64, cast the others up to match
		if (any(i64_type == [lbound_%type, step%type, ubound_%type])) then

			!! this happens during parsing
			!array%type = i64_type

			call promote_i32_i64(lbound_)
			call promote_i32_i64(step)
			call promote_i32_i64(ubound_)
		end if

		!print *, 'lbound_ = ', lbound_%sca%i64
		!print *, 'step32 = ', step  %sca%i32
		!print *, 'step64 = ', step  %sca%i64
		!print *, 'ubound_ = ', ubound_%sca%i64

		if (array%type == i32_type) then

			if (step%sca%i32 == 0) then
				call rt_throw(state, err_rt(RC_ARRAY_STEP_ZERO, 'array step is 0'))
				return
			end if

			array%cap = (ubound_%sca%i32 - lbound_%sca%i32 &
				+ step%sca%i32 - sign(1,step%sca%i32)) / step%sca%i32

			!print *, 'cap = ', array%cap
			allocate(array%i32( array%cap ))

			j = 1
			i = lbound_%sca%i32
			if (lbound_%sca%i32 < ubound_%sca%i32 .neqv. 0 < step%sca%i32) i = ubound_%sca%i32

			! Step may be negative
			do while ((i  < ubound_%sca%i32 .eqv. lbound_%sca%i32 < ubound_%sca%i32) &
			     .and. i /= ubound_%sca%i32)
				array%i32(j) = i
				i = i + step%sca%i32
				j = j + 1
			end do
			array%len_ = j - 1

		else if (array%type == i64_type) then

			if (step%sca%i64 == 0) then
				call rt_throw(state, err_rt(RC_ARRAY_STEP_ZERO, 'array step is 0'))
				return
			end if

			array%cap = (ubound_%sca%i64 - lbound_%sca%i64 &
				+ step%sca%i64 - sign(int(1,8), step%sca%i64)) / step%sca%i64

			allocate(array%i64( array%cap ))

			j = 1
			i8 = lbound_%sca%i64
			if (lbound_%sca%i64 < ubound_%sca%i64 .neqv. 0 < step%sca%i64) then
				i8 = ubound_%sca%i64
			end if

			! Step may be negative
			do while ((i8  < ubound_%sca%i64 .eqv. lbound_%sca%i64 < ubound_%sca%i64) &
			     .and. i8 /= ubound_%sca%i64)
				array%i64(j) = i8
				i8 = i8 + step%sca%i64
				j = j + 1
			end do
			array%len_ = j - 1

		else if (array%type == f32_type) then

			!print *, 'lbound_, ubound_ = ', lbound_%sca%f32, ubound_%sca%f32
			!print *, 'step = ', step%sca%f32

			if (step%sca%f32 == 0.0) then
				call rt_throw(state, err_rt(RC_ARRAY_STEP_ZERO_F, 'array step is 0.0'))
				return
			end if

			array%cap = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32)
			allocate(array%f32( array%cap ))

			j = 1
			f = lbound_%sca%f32
			if (lbound_%sca%f32 < ubound_%sca%f32 .neqv. 0 < step%sca%f32) f = ubound_%sca%f32

			do while ((f  < ubound_%sca%f32 .eqv. lbound_%sca%f32 < ubound_%sca%f32) &
			     .and. f /= ubound_%sca%f32)
				array%f32(j) = f

				! Using only addition here seems more efficient, but
				! rounding errors accumulate differently.  Compare
				! `[0.0: 0.1: 0.9];` with both methods.  First ends in
				! 0.800001, while second method (with multiplication) ends
				! in 0.8

				!f = f + step%sca%f32
				f = lbound_%sca%f32 + j * step%sca%f32

				j = j + 1
			end do
			array%len_ = j - 1
			!array%len_ = array%cap

		else if (array%type == f64_type) then

			!print *, 'lbound_, ubound_ = ', lbound_%sca%f64, ubound_%sca%f64
			!print *, 'step = ', step%sca%f64

			if (step%sca%f64 == 0.0) then
				call rt_throw(state, err_rt(RC_ARRAY_STEP_ZERO_F, 'array step is 0.0'))
				return
			end if

			array%cap = ceiling((ubound_%sca%f64 - lbound_%sca%f64) / step%sca%f64)
			allocate(array%f64( array%cap ))

			j = 1
			f64 = lbound_%sca%f64
			if (lbound_%sca%f64 < ubound_%sca%f64 .neqv. 0 < step%sca%f64) f64 = ubound_%sca%f64

			do while ((f64  < ubound_%sca%f64 .eqv. lbound_%sca%f64 < ubound_%sca%f64) &
			     .and. f64 /= ubound_%sca%f64)
				array%f64(j) = f64

				! Using only addition here seems more efficient, but
				! rounding errors accumulate differently.  Compare
				! `[0.0: 0.1: 0.9];` with both methods.  First ends in
				! 0.800001, while second method (with multiplication) ends
				! in 0.8

				!f64 = f64 + step%sca%f64
				f64 = lbound_%sca%f64 + j * step%sca%f64

				j = j + 1
			end do
			array%len_ = j - 1
			!array%len_ = array%cap

		else
			write(*,*) err_int(IC_STEP_ARRAY_TYPE, 'step array type eval not implemented')
			call internal_error()
		end if

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		allocate(res%array)
		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == len_array) then

		!print *, 'len array'
		k = k + 1; lbound_ = slots(k)
		k = k + 1; ubound_ = slots(k)
		k = k + 1; len_    = slots(k)

		array%type = node%val%array%type
		array%len_  = len_%to_i64()
		array%cap  = array%len_

		if (array%type == f32_type) then

			allocate(array%f32( array%cap ))
			fstep = (ubound_%sca%f32 - lbound_%sca%f32) &
				/ real((len_%to_i64() - 1))

			do i = 0, len_%to_i32() - 1
				array%f32(i+1) = lbound_%sca%f32 + i * fstep
			end do

		else if (array%type == f64_type) then

			allocate(array%f64( array%cap ))
			fstep64 = (ubound_%sca%f64 - lbound_%sca%f64) &
				/ real((len_%to_i64() - 1), 8)

			do i = 0, len_%to_i32() - 1
				array%f64(i+1) = lbound_%sca%f64 + i * fstep64
			end do

		else
			write(*,*) err_int(IC_BOUND_LEN_TYPE, 'bound/len array type eval not implemented')
			call internal_error()
		end if

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		allocate(res%array)

		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == unif_array) then

		allocate(res%array)
		res%array%rank = size( node%size )
		!print *, "rank = ", res%array%rank
		allocate(res%array%size( res%array%rank ))

		do i = 1, res%array%rank
			!print *, "i = ", i
			k = k + 1; len_ = slots(k)
			!print *, "len_%type = ", kind_name(len_%type)
			!print *, "len_      = ", len_%to_i64()
			res%array%size(i) = len_%to_i64()
			!print *, 'size['//str(i)//'] = ', res%array%size(i)
		end do

		! Uniform-value impl arrays (every element has the same value at
		! initialization, and you could say "constant" but they are of
		! course mutable)

		!print *, 'len array'
		k = k + 1; lbound_ = slots(k)

		! Allocate in one shot without growing

		res%array%type = node%val%array%type
		res%array%len_  = product(res%array%size)
		!print *, 'res%array%len_ = ', res%array%len_

		call allocate_array(res, res%array%len_)
		select case (res%array%type)
		case (i32_type)
			res%array%i32 = lbound_%sca%i32

		case (i64_type)
			res%array%i64 = lbound_%sca%i64

		case (f32_type)
			res%array%f32 = lbound_%sca%f32

		case (f64_type)
			res%array%f64 = lbound_%sca%f64

		case (bool_type)
			res%array%bool = lbound_%sca%bool

		case (str_type)
			! Don't rely on a scalar-to-array broadcast `res%array%str =
			! lbound_%str` here — string_t has its own allocatable %s
			! component, and gfortran's broadcast-assignment codegen for a
			! derived type with a nested allocatable does not give each
			! broadcast-target element its own independent deep copy (see
			! the identical str_type fix in value_copy() in value.f90).
			! Assign each element's %s individually instead
			do i8 = 1, res%array%len_
				res%array%str(i8)%s = lbound_%str%s
			end do

		case (struct_type)

			!print *, "lbound_ size = ", size(lbound_%struct)

			! Don't rely on a whole-array `res%struct(i8)%struct =
			! lbound_%struct` here — value_t's assignment(=) binding
			! (value_copy) is a plain (non-elemental) scalar subroutine, so
			! it cannot be dispatched for this array-to-array assignment;
			! gfortran silently falls back to raw intrinsic array
			! assignment instead, which doesn't deep-copy struct's nested
			! allocatable fields correctly.  Copy each field individually
			! via value_copy(), matching the pattern value_copy() itself
			! uses for its own struct(:) component
			do i8 = 1, res%array%len_
				if (allocated(res%struct(i8)%struct)) deallocate(res%struct(i8)%struct)
				allocate(res%struct(i8)%struct( size(lbound_%struct) ))
				do j = 1, size(lbound_%struct)
					call value_copy(res%struct(i8)%struct(j), lbound_%struct(j))
				end do

				! Each element needs its own type/name/cookie set too (not
				! just the outer array value), or value_to_str() falls
				! through to the scalar default arm and prints
				! "<invalid_value>" when the whole array is printed --
				! indexing (a[0]) still worked before this fix because that
				! path reads type from the outer value instead
				res%struct(i8)%type = struct_type
				res%struct(i8)%struct_name = lbound_%struct_name
				if (allocated(lbound_%struct_cookie)) &
					res%struct(i8)%struct_cookie = lbound_%struct_cookie
				res%struct(i8)%struct_reg_idx = lbound_%struct_reg_idx
			end do

			! Arrays are homogeneous, so every element shares one struct_name
			! for efficiency
			res%struct_name = lbound_%struct_name
			if (allocated(lbound_%struct_cookie)) res%struct_cookie = lbound_%struct_cookie
			res%struct_reg_idx = lbound_%struct_reg_idx

		case (enum_type)

			! Unlike struct_type, an enum variant has no nested allocatable
			! members of its own, so a plain value_copy() per element is
			! enough (no need to deep-copy a struct(:) sub-array)
			do i8 = 1, res%array%len_
				call value_copy(res%struct(i8), lbound_)
			end do

			! Arrays are homogeneous, so every element shares one enum_name
			! for efficiency
			res%enum_name = lbound_%enum_name
			if (allocated(lbound_%enum_cookie)) res%enum_cookie = lbound_%enum_cookie

		case default
			write(*,*) err_eval_len_array(kind_name(res%array%type))
			call internal_error()
		end select

		res%type  = array_type

	else if (node%val%array%kind == bound_array) then
		!print *, 'impl_array'

		! Expand implicit array kinds here on evaluation.  Consider
		! something like this:
		!
		!     let a = [0: 5];
		!     a[2] = -3;
		!
		! Even though a is initialized to an implicit array, the second
		! statement requires it to be explicit, so we might as well expand
		! at initialization

		k = k + 1; lbound_ = slots(k)
		k = k + 1; ubound_ = slots(k)

		allocate(res%array)
		res%array%type = node%val%array%type

		if (any(i64_type == [lbound_%type, ubound_%type])) then
			call promote_i32_i64(lbound_)
			call promote_i32_i64(ubound_)
		end if

		if (.not. any(res%array%type == [i32_type, i64_type])) then
			write(*,*) err_int(IC_UNIT_STEP_TYPE, 'unit step array type eval not implemented')
			call internal_error()
		end if

		if (res%array%type == i32_type) then
			res%array%len_ = max(0_8, int(ubound_%sca%i32 - lbound_%sca%i32, 8))
		else !if (res%array%type == i64_type) then
			res%array%len_ = max(0_8, ubound_%sca%i64 - lbound_%sca%i64)
		end if

		call allocate_array(res, res%array%len_)

		!print *, 'bounds in [', lbound_%str(), ': ', ubound_%str(), ']'
		!print *, 'node%val%array%type = ', node%val%array%type

		if (res%array%type == i32_type) then
			do i = lbound_%sca%i32, ubound_%sca%i32 - 1
				res%array%i32(i - lbound_%sca%i32 + 1) = i
			end do
		else !if (res%array%type == i64_type) then
			do i8 = lbound_%sca%i64, ubound_%sca%i64 - 1
				res%array%i64(i8 - lbound_%sca%i64 + 1) = i8
			end do
		end if

		res%array%rank = 1
		allocate(res%array%size( res%array%rank ))
		res%array%size = res%array%len_

		res%type  = array_type

	else if (node%val%array%kind == size_array) then

		! Explicit array with size

		array = new_array(node%val%array%type, size(node%elems))

		do i = 1, size(node%elems)
			k = k + 1; elem = slots(k)
			if (state%rt_halt) return
			!print *, 'elem['//str(i)//'] = ', elem%str()
			call array%push(elem)
		end do

		array%rank = size( node%size )
		allocate(array%size( array%rank ))
		do i = 1, array%rank
			k = k + 1; len_ = slots(k)
			if (state%rt_halt) return
			array%size(i) = len_%to_i64()
		end do

		if (size(node%elems) /= product(array%size)) then
			call rt_throw(state, err_rt_expl_array_size(size(node%elems), array%size))
			return
		end if

		!print *, 'copying array'
		allocate(res%array)
		res%type  = array_type
		res%array = array
		!print *, 'done'

		!print *, "size_array"
		!print *, "size = ", array%size

	else if (node%val%array%kind == expl_array) then
		!print *, 'expl_array'

		! Explicit rank-1 arrays

		! Allow empty arrays?  Sub type of empty array?  Empty arrays can
		! currently be created like [0: -1] or [0; 0].  They need to have an inferrable
		! type, so I don't think `[]` makes sense in syntran, but alternatives
		! like `[0; 0]` are fine and currently allowed

		allocate(res%array)
		res%array%type = node%val%array%type

		!print *, "elem 1 type = ", kind_name(node%elems(1)%val%type)

		call allocate_array(res, size(node%elems, kind = 8))

		res%array%len_ = 0
		is_cat = .false.

		do i = 1, size(node%elems)
			k = k + 1; elem = slots(k)
			if (state%rt_halt) return
			!print *, 'elem['//str(i)//'] = ', elem%str()

			if (any(res%array%type == [struct_type, enum_type])) then
				res%struct(i) = elem

			else if (elem%type == array_type) then
				is_cat = .true.
				do j8 = 0, elem%array%len_ - 1
					call get_array_val(elem%array, j8, tmp)
					call res%array%push(tmp)
				end do

			else
				call res%array%push(elem)
			end if

		end do

		! Trim catted array.  There is a perf overhead here, but you will get
		! the wrong answer for minval, sum, etc. on the untrimmed array unless
		! you do extra work when calling those builtin fns
		if (is_cat) then
			call res%array%trim()
		end if

		if (any(res%array%type == [struct_type, enum_type])) then
			res%array%len_ = size(node%elems)
		end if

		res%array%rank = 1
		allocate(res%array%size( res%array%rank ))
		res%array%size = res%array%len_

		res%type  = array_type

		if (allocated(node%val%struct_name)) then
			res%struct_name = node%val%struct_name
		end if
		if (allocated(node%val%struct_cookie)) then
			res%struct_cookie = node%val%struct_cookie
		end if
		res%struct_reg_idx = node%val%struct_reg_idx
		if (allocated(node%val%enum_name)) then
			res%enum_name = node%val%enum_name
		end if
		if (allocated(node%val%enum_cookie)) then
			res%enum_cookie = node%val%enum_cookie
		end if

		!print *, "struct_name = ", res%struct_name

	else
		write(*,*) err_int(IC_UNEXPECTED_ARRAY_KIND, 'unexpected array kind')
		call internal_error()
	end if

end subroutine eval_array_expr

!===============================================================================

end submodule syntran__runtime_control

!===============================================================================
