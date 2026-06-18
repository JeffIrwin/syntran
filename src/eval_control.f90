
!===============================================================================

submodule (syntran__eval_m) syntran__eval_control

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine eval_for_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i, rank, for_kind
	integer(kind = 8) :: i8, len8

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, itr, step, len_, tmp, str_

	! Evaluate all of these ahead of loop, but only if they are allocated!
	if (allocated(node%array%lbound)) call syntax_eval(node%array%lbound, state, lbound_)
	if (allocated(node%array%step  )) call syntax_eval(node%array%step  , state, step   )
	if (allocated(node%array%ubound)) call syntax_eval(node%array%ubound, state, ubound_)
	if (allocated(node%array%len_  )) call syntax_eval(node%array%len_  , state, len_   )

	!print *, 'lbound_ = ', lbound_%to_i64()
	!print *, 'ubound_ = ', ubound_%to_i64()
	!print *, 'lbound type = ', kind_name(lbound_%type)
	!print *, 'ubound type = ', kind_name(ubound_%type)
	!print *, 'node%array%type = ', kind_name(node%array%val%array%type)
	!print *, 'node type = ', kind_name(node%array%val%type)

	!print *, 'node array kind = ', kind_name(node%array%kind)
	!print *, 'array kind      = ', kind_name(node%array%val%array%kind)

	select case (node%array%kind)
	case (array_expr)

		! Primary array exprs are evaluated lazily without wasting memory
		for_kind = node%array%val%array%kind

		select case (node%array%val%array%kind)
		case (bound_array)

			! Do promotion once before loop
			if (any(i64_type == [lbound_%type, ubound_%type])) then
				!print *, 'promoting'
				call promote_i32_i64(lbound_)
				call promote_i32_i64(ubound_)
				itr%type = i64_type
			else
				itr%type = i32_type
			end if

			if (.not. any(itr%type == [i32_type, i64_type])) then
				write(*,*) err_int_prefix//'unit step array type eval not implemented'//color_reset
				call internal_error()
			end if

			len8 = ubound_%to_i64() - lbound_%to_i64()

		case (step_array)

			! If any bound or step is i64, cast the others up to match
			if (any(i64_type == [lbound_%type, step%type, ubound_%type])) then
				call promote_i32_i64(lbound_)
				call promote_i32_i64(step)
				call promote_i32_i64(ubound_)
				itr%type = i64_type
			else
				itr%type = lbound_%type
			end if

			select case (itr%type)
			case (i32_type)

				if (step%sca%i32 == 0) then
					write(*,*) err_int_prefix//'for loop step is 0'//color_reset
					call internal_error()
				end if
				len8 = (ubound_%sca%i32 - lbound_%sca%i32 &
					+ step%sca%i32 - sign(1,step%sca%i32)) / step%sca%i32

			case (i64_type)

				if (step%sca%i64 == 0) then
					write(*,*) err_int_prefix//'for loop step is 0'//color_reset
					call internal_error()
				end if
				len8 = (ubound_%sca%i64 - lbound_%sca%i64 &
					+ step%sca%i64 - sign(int(1,8),step%sca%i64)) / step%sca%i64

			case (f32_type)

				if (step%sca%f32 == 0.0) then
					write(*,*) err_int_prefix//'for loop step is 0.0'//color_reset
					call internal_error()
				end if
				len8 = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32)

			case (f64_type)

				if (step%sca%f64 == 0.0) then
					write(*,*) err_int_prefix//'for loop step is 0.0'//color_reset
					call internal_error()
				end if
				len8 = ceiling((ubound_%sca%f64 - lbound_%sca%f64) / step%sca%f64)

			case default
				write(*,*) err_int_prefix//'step array type eval not implemented'//color_reset
				call internal_error()
			end select

		case (len_array)

			itr%type = node%array%val%array%type

			select case (itr%type)
			case (f32_type)
				len8 = len_%to_i64()
			case (f64_type)
				len8 = len_%to_i64()
			case default
				write(*,*) err_int_prefix//'bound/len array type eval not implemented'//color_reset
				call internal_error()
			end select

		case (expl_array)
			! TODO: array catting in for statements
			len8 = node%array%val%array%len_

		case (size_array)

			rank = size( node%array%size )
			len8 = 1
			do i = 1, rank
				call syntax_eval(node%array%size(i), state, len_)
				len8 = len8 * len_%to_i64()
			end do

			if (size(node%array%elems) /= len8) then
				write(*,*) err_rt_prefix//"size of explicit array "// &
					"does not match number of elements"//color_reset
				call internal_error()
			end if

		case (unif_array)

			rank = size(node%array%size)
			!print *, 'rank = ', rank
			len8 = 1
			do i = 1, rank
				call syntax_eval(node%array%size(i), state, len_)
				len8 = len8 * len_%to_i64()
			end do
			!print *, 'len8 = ', len8

		case default
			write(*,*) err_int_prefix//'for loop not implemented for this array kind'//color_reset
			call internal_error()
		end select

	case default
		!print *, 'non-primary array expression'

		if (node%array%val%type == str_type) then

			! Allow iterating on chars in a str

			for_kind = str_type
			call syntax_eval(node%array, state, tmp)
			call value_move(tmp, str_)
			len8 = len(str_%str%s, 8)
			itr%type = str_type

			!print *, "str_ = ", str_%str%s

		else

			! Any non-primitive array needs to be evaluated before iterating
			! over it.  Parser guarantees that this is an array
			!
			! Unlike step_array, itr%type does not need to be set here because
			! it is set in array_at() (via get_array_val())
			for_kind = array_expr

			call syntax_eval(node%array, state, tmp)
			array = tmp%array  ! TODO: move_alloc() (or value_move) instead of copy?

			len8 = array%len_
			!print *, 'len8 = ', len8

		end if

	end select

	!print *, 'itr%type = ', kind_name(itr%type)

	! Push scope to make the loop iterator local
	call state%vars%push_scope()
	call state%locs%push_scope()

	state%breaked = .false.
	do i8 = 1, len8

		! `breaked` is set once per loop instance, while `continued` is reset on
		! every iteration
		state%continued = .false.

		call array_at(itr, for_kind, i8, lbound_, step, ubound_, &
			len_, array, node%array%elems, str_, state)

		!print *, 'itr = ', itr%to_str()

		! During evaluation, insert variables by array id_index instead of
		! dict lookup.  This is much faster and can be done during
		! evaluation now that we know all of the variable identifiers.
		! Parsing still needs to rely on dictionary lookups because it does
		! not know the entire list of variable identifiers ahead of time
		if (node%is_loc) then
			state%locs%vals(node%id_index) = itr
		else
			state%vars%vals(node%id_index) = itr
		end if

		call syntax_eval(node%body, state, res)

		if (state%returned) exit
		if (state%breaked ) exit

	end do

	! Reset for nested loops
	state%breaked   = .false.
	state%continued = .false.

	call state%vars%pop_scope()
	call state%locs%pop_scope()

end subroutine eval_for_statement

!===============================================================================

recursive module subroutine eval_assignment_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: rank_res, id, type_, nelem
	integer(kind = 8) :: i8, j8, index_, len8, size_i
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:), &
		size_tmp(:)

	logical :: has_char_sub

	type(i64_vector_t), allocatable :: asubs(:)
	type(value_t) :: array_val, rhs, tmp, tmp_array

	!print *, "eval assignment_expr"
	!print *, "node identifier = ", node%identifier%text
	!id = node%id_index
	!print *, 'lhs type = ', kind_name( state%vars%vals(id)%type )
	!if (state%vars%vals(id)%type == struct_type) then
	!if (allocated( node%member )) then
	!	print *, "mem index = ", node%member%id_index
	!end if

	if (allocated( node%member )) then
		!print *, "assign LHS dot member"

		! This is similar to what I do below with get_array_val() and
		! set_array_val(), but I've renamed some of the variables

		! Evaluate the RHS
		call syntax_eval(node%right, state, rhs)

		id = node%id_index
		if (node%is_loc) then
			call get_val(node, state%locs%vals(id), state, res)
			call compound_assign(res, rhs, node%op)
			call set_val(node, state%locs%vals(id), state, res)

		else
			! Get the initial value from the LHS, which could be nested like `a.b.c.d`
			call get_val(node, state%vars%vals(id), state, res)

			! Do the assignment or += or whatever and set res
			call compound_assign(res, rhs, node%op)

			! Save it back into the LHS var
			call set_val(node, state%vars%vals(id), state, res)

		end if

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

		!print *, 'scalar compound_assign'

		! Eval the RHS
		call syntax_eval(node%right, state, res)

		! TODO: test int/float casting.  It should be an error during
		! parsing

		!print *, 'lhs type = ', kind_name( state%vars%vals(id)%type )

		!print *, "compound_assign is_loc = ", node%is_loc
		if (node%is_loc) then
			!print *, "val type = ", kind_name( state%locs%vals(id)%type )
			call compound_assign(state%locs%vals(id), res, node%op)
			res = state%locs%vals(id)
		else
			call compound_assign(state%vars%vals(id), res, node%op)

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
		call syntax_eval(node%right, state, res)

		!print *, 'RHS = ', res%to_str()

		if (type_ == str_type) then
			!print *, 'str_type'

			! TODO: ban compound character substring assignment
			i8 = subscript_eval(node, state)
			if (node%is_loc) then
				state%locs%vals(id)%str%s(i8+1: i8+1) = res%str%s
			else
				state%vars%vals(id)%str%s(i8+1: i8+1) = res%str%s
			end if

		else if (has_char_sub) then

			! String array with char subscript assignment.  Apply the char sub
			! to every element selected by the element subscripts.
			if (all(node%lsubscripts(1:nelem)%sub_kind == scalar_sub)) then

				! All element subs scalar: single element
				i8 = subscript_eval(node, state)   ! element flat index
				call str_arr_char_assign(node, state, res, id, i8, nelem)

			else

				! Slice element selection: iterate over selected elements
				call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)
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
					call str_arr_char_assign(node, state, res, id, index_, nelem)
					call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
				end do

			end if

		else if (all(node%lsubscripts%sub_kind == scalar_sub)) then

			!print *, 'non str_type scalar subscript'
			!print *, 'LHS array type = ', &
			!	state%vars%vals(id)%array%type  ! this debug will break for is_loc
			!print *, 'LHS array = ', state%vars%vals(id)%array%i32

			!print *, "get_array_val a"

			! It is important to only eval the subscript once, in case it is an
			! expression which changes the state!  For example, `array[(index +=
			! 1)];`.  Maybe I should ban expression statements as indices, but
			! src/tests/test-src/fns/test-19.syntran at least will need updated
			i8 = subscript_eval(node, state)

			if (node%is_loc) then
				call get_val(node, state%locs%vals(id), state, array_val, index_ = i8)
				call compound_assign(array_val, res, node%op)
				call set_val(node, state%locs%vals(id), state, array_val, index_ = i8)
			else
				call get_val(node, state%vars%vals(id), state, array_val, index_ = i8)
				call compound_assign(array_val, res, node%op)
				call set_val(node, state%vars%vals(id), state, array_val, index_ = i8)
			end if

			res = array_val

		else

			!print *, "lhs slice assignment"

			if (size(node%lsubscripts) == 1 .and. &
			    node%lsubscripts(1)%sub_kind /= arr_sub) then
				! Rank-1 slice fast path: avoids allocating lsubs/ssubs/usubs/asubs.
				call eval_assign_slice_rank1(node, state, id, res)
			else

			call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)
			allocate(size_tmp(rank_res))

			!print *, "rank     = ", state%vars%vals(id)%array%rank
			!print *, "rank_res = ", rank_res

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
			tmp_array%array%rank = rank_res
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
					call compound_assign(tmp, array_val, node%op)
					call set_array_val(state%locs%vals(id)%array, index_, tmp)
				else
					index_ = subscript_i32_eval(subs, state%vars%vals(id)%array)
					call get_array_val(state%vars%vals(id)%array, index_, tmp)
					call compound_assign(tmp, array_val, node%op)
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
				!	! Should `b` be the whole `a` array as in syntran, or
				!	! just the slice `a[1:4]`, or just the scalar `3`?
				!	!
				!	! I think there's a good case to be made that it should
				!	! be the slice `a[1:4]`, although the implementation was more
				!	! simple by setting `b` to the whole array `a`.
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


			!! TODO: update readme for this change.  Search "contrast" or "This
			!! behaviour is in contrast to non-nested assignment:"
			!res = state%vars%vals(id)  ! big copy for returing the whole array
			res = tmp_array  ! only return the modified slice

			end if   ! rank-1 fast path / general path

		end if
	end if

contains

	subroutine str_arr_char_assign(node, state, rhs, id, elem_idx, nelem_)

		! Apply the char-rank subscript at lsubscripts(nelem_+1) to the
		! element string at state%{locs|vars}%vals(id)%array%str(elem_idx+1)%s.
		! Handles both scalar_sub (single char) and range_sub (substring).

		type(syntax_node_t), intent(in)    :: node
		type(state_t),       intent(inout) :: state
		type(value_t),       intent(in)    :: rhs
		integer,             intent(in)    :: id, nelem_
		integer(kind = 8),   intent(in)    :: elem_idx

		!********

		integer :: isub
		integer(kind = 8) :: il, iu, char_pos
		type(value_t) :: tmp_

		isub = nelem_ + 1

		select case (node%lsubscripts(isub)%sub_kind)
		case (scalar_sub)
			call syntax_eval(node%lsubscripts(isub), state, tmp_)
			char_pos = tmp_%to_i64()
			if (node%is_loc) then
				state%locs%vals(id)%array%str(elem_idx+1)%s( &
					char_pos+1 : char_pos+1) = rhs%str%s
			else
				state%vars%vals(id)%array%str(elem_idx+1)%s( &
					char_pos+1 : char_pos+1) = rhs%str%s
			end if

		case (range_sub)
			if (node%lsubscripts(isub)%lsub_omit) then
				il = 1
			else
				call syntax_eval(node%lsubscripts(isub), state, tmp_)
				il = tmp_%to_i64() + 1
			end if
			if (node%lsubscripts(isub)%usub_omit) then
				if (node%is_loc) then
					iu = len(state%locs%vals(id)%array%str(elem_idx+1)%s) + 1
				else
					iu = len(state%vars%vals(id)%array%str(elem_idx+1)%s) + 1
				end if
			else
				call syntax_eval(node%usubscripts(isub), state, tmp_)
				iu = tmp_%to_i64() + 1
			end if
			if (node%is_loc) then
				state%locs%vals(id)%array%str(elem_idx+1)%s(il : iu-1) = rhs%str%s
			else
				state%vars%vals(id)%array%str(elem_idx+1)%s(il : iu-1) = rhs%str%s
			end if

		case default
			write(*,*) err_int_prefix//'unexpected str char subscript kind'//color_reset
			call internal_error()

		end select

	end subroutine str_arr_char_assign

end subroutine eval_assignment_expr

!===============================================================================

module subroutine eval_translation_unit(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	! The final statement of a unit returns the actual result.  Non-final
	! members only change the (vars) state or define fns
	do i = 1, size(node%members)

		! Only eval statements, not fn or struct declarations
		if (node%members(i)%kind == fn_declaration    ) cycle
		if (node%members(i)%kind == struct_declaration) cycle

		call syntax_eval(node%members(i), state, res)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' res = ', res%to_str()
		!print *, ''

		if (state%returned) exit

	end do

end subroutine eval_translation_unit

!===============================================================================

module subroutine eval_use_statement(node, state, res)

	! Evaluate a module's translation unit. This ensures that module-level
	! statements (like `let a = [0: 10];`) are evaluated, not just parsed.

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	if (allocated(node%member)) then
		call eval_translation_unit(node%member, state, res)
	end if

end subroutine eval_use_statement

!===============================================================================

recursive module subroutine eval_array_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i, j
	integer(kind = 8) :: i8, j8

	logical :: is_cat

	real(kind = 4) :: f, fstep
	real(kind = 8) :: f64, fstep64

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, elem, &
		step, len_, tmp

	!print *, "starting eval_array_expr()"
	!print *, 'identifier = ', node%identifier%text

	if (node%val%array%kind == step_array) then

		call syntax_eval(node%lbound, state, lbound_)
		call syntax_eval(node%step  , state, step   )
		call syntax_eval(node%ubound, state, ubound_)

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
				write(*,*) err_int_prefix//'array step is 0'//color_reset
				call internal_error()
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
				write(*,*) err_int_prefix//'array step is 0'//color_reset
				call internal_error()
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
				write(*,*) err_int_prefix//'array step is 0.0'//color_reset
				call internal_error()
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
				write(*,*) err_int_prefix//'array step is 0.0'//color_reset
				call internal_error()
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
			write(*,*) err_int_prefix//'step array type eval not implemented'//color_reset
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
		call syntax_eval(node%lbound, state, lbound_)
		call syntax_eval(node%ubound, state, ubound_)
		call syntax_eval(node%len_  , state, len_   )

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
			write(*,*) err_int_prefix//'bound/len array type eval not implemented'//color_reset
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
			call syntax_eval(node%size(i), state, len_)
			!print *, "len_%type = ", kind_name(len_%type)
			!print *, "len_      = ", len_%to_i64()
			res%array%size(i) = len_%to_i64()
			!print *, 'size['//str(i)//'] = ', res%array%size(i)
		end do

		! Uniform-value impl arrays (every element has the same value at
		! initialization, and you could say "constant" but they are of
		! course mutable)

		!print *, 'len array'
		call syntax_eval(node%lbound, state, lbound_)

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
			res%array%str = lbound_%str

		case (struct_type)

			!print *, "lbound_ size = ", size(lbound_%struct)

			do i8 = 1, res%array%len_
				res%struct(i8)%struct = lbound_%struct
			end do

			! Arrays are homogeneous, so every element shares one struct_name
			! for efficiency
			res%struct_name = lbound_%struct_name
			res%struct_cookie = lbound_%struct_cookie

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

		call syntax_eval(node%lbound, state, lbound_)
		call syntax_eval(node%ubound, state, ubound_)

		!array = new_array(node%val%array%type)

		allocate(res%array)
		res%array%type = node%val%array%type

		if (any(i64_type == [lbound_%type, ubound_%type])) then
			call promote_i32_i64(lbound_)
			call promote_i32_i64(ubound_)
		end if

		if (.not. any(res%array%type == [i32_type, i64_type])) then
			write(*,*) err_int_prefix//'unit step array type eval not implemented'//color_reset
			call internal_error()
		end if

		if (res%array%type == i32_type) then
			res%array%len_ = ubound_%sca%i32 - lbound_%sca%i32
		else !if (res%array%type == i64_type) then
			res%array%len_ = ubound_%sca%i64 - lbound_%sca%i64
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
			call syntax_eval(node%elems(i), state, elem)
			!print *, 'elem['//str(i)//'] = ', elem%str()
			call array%push(elem)
		end do

		array%rank = size( node%size )
		allocate(array%size( array%rank ))
		do i = 1, array%rank
			call syntax_eval(node%size(i), state, len_)
			array%size(i) = len_%to_i64()
		end do

		if (size(node%elems) /= product(array%size)) then
			write(*,*) err_rt_prefix//"size of explicit array "// &
				"does not match number of elements"//color_reset
			call internal_error()
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
			call syntax_eval(node%elems(i), state, elem)
			!print *, 'elem['//str(i)//'] = ', elem%str()

			if (res%array%type == struct_type) then
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

		if (res%array%type == struct_type) then
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

		!print *, "struct_name = ", res%struct_name

	else
		write(*,*) err_int_prefix//'unexpected array kind'//color_reset
		call internal_error()
	end if

end subroutine eval_array_expr

!===============================================================================

recursive module subroutine eval_while_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	type(value_t) :: condition

	call syntax_eval(node%condition, state, condition)
	state%breaked = .false.
	do while (condition%sca%bool)
		state%continued = .false.

		call syntax_eval(node%body, state, res)
		call syntax_eval(node%condition, state, condition)

		if (state%returned) exit
		if (state%breaked ) exit

	end do
	state%breaked   = .false.
	state%continued = .false.

end subroutine eval_while_statement

!===============================================================================

recursive module subroutine eval_if_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	type(value_t) :: condition

	call syntax_eval(node%condition, state, condition)
	!print *, 'condition = ', condition%str()

	if (condition%sca%bool) then
		!print *, 'if'
		call syntax_eval(node%if_clause, state, res)

	else if (allocated(node%else_clause)) then
		!print *, 'else'
		call syntax_eval(node%else_clause, state, res)

	end if

end subroutine eval_if_statement

!===============================================================================

recursive module subroutine eval_return_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	!print *, "starting eval_return_statement"

	state%returned = .true.

	if (node%right%val%type == void_type) then
		!res%type = unknown_type
		return
	end if

	call syntax_eval(node%right, state, res)

	!print *, "ending eval_return_statement"

end subroutine eval_return_statement

!===============================================================================

recursive module subroutine eval_block_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	type(value_t) :: tmp

	call state%vars%push_scope()
	call state%locs%push_scope()

	! The final statement of a block returns the actual result.  Non-final
	! members only change the (vars) state.
	do i = 1, size(node%members)
		call syntax_eval(node%members(i), state, tmp)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' tmp = ', tmp%to_str()
		!print *, 'type = ', tmp%type, kind_name(tmp%type)
		!print *, ''

		! In case of no-op if statements and while loops
		if (tmp%type /= unknown_type) res = tmp

		if (state%returned ) exit
		if (state%breaked  ) exit
		if (state%continued) exit  ! exit (break) the block but not the enclosing loop

	end do

	call state%vars%pop_scope()
	call state%locs%pop_scope()

end subroutine eval_block_statement

!===============================================================================

end submodule syntran__eval_control

!===============================================================================
