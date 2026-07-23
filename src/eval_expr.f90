
!===============================================================================

submodule (syntran__eval_m) syntran__eval_expr

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine eval_binary_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: larrtype, rarrtype

	character(len = :), allocatable :: rt_err

	type(value_t) :: left, right

	call syntax_eval(node%left , state, left )
	if (state%rt_halt) return
	call syntax_eval(node%right, state, right)
	if (state%rt_halt) return

	!print *, 'left  type = ', kind_name(left%type)
	!print *, 'right type = ', kind_name(right%type)
	!print *, "op kind    = ", kind_name(node%op%kind)

	larrtype = unknown_type
	rarrtype = unknown_type
	if (left %type == array_type) larrtype = left %array%type
	if (right%type == array_type) rarrtype = right%array%type

	res%type = get_binary_op_kind(left%type, node%op%kind, right%type, &
		larrtype, rarrtype)
	select case (res%type)
	case (bool_array_type, f32_array_type, f64_array_type, &
		i32_array_type, i64_array_type, str_array_type)

		res%type = array_type
	end select

	if (res%type == unknown_type) then
		write(*,*) err_eval_binary_types(node%op%text)
		call internal_error()
	end if

	!print *, 'op = ', node%op%text

	select case (node%op%kind)
	case (plus_token)
		call add(left, right, res, node%op%text)

	case (minus_token)
		call subtract(left, right, res, node%op%text)

	case (star_token)
		call mul(left, right, res, node%op%text)

	case (matmul_token)
		call matmul_(left, right, res, node%op%text, rt_err)
		if (allocated(rt_err)) then
			call rt_throw(state, rt_err)
			return
		end if

	case (sstar_token)
		call pow(left, right, res, node%op%text)

	case (slash_token)
		call div(left, right, res, node%op%text)

	case (percent_token)
		call mod_(left, right, res, node%op%text)

	case (and_keyword)
		call and_(left, right, res, node%op%text)

	case (or_keyword)
		call or_(left, right, res, node%op%text)

	case (eequals_token)
		call is_eq(left, right, res, node%op%text)

	case (bang_equals_token)
		call is_ne(left, right, res, node%op%text)

	case (less_token)
		call is_lt(left, right, res, node%op%text)

	case (less_equals_token)
		call is_le(left, right, res, node%op%text)

	case (greater_token)
		call is_gt(left, right, res, node%op%text)

	case (greater_equals_token)
		call is_ge(left, right, res, node%op%text)

	case (lless_token)
		call left_shift(left, right, res, node%op%text)

	case (ggreater_token)
		call right_shift(left, right, res, node%op%text)

	case (caret_token)
		call bit_xor(left, right, res, node%op%text)

	case (pipe_token)
		call bit_or(left, right, res, node%op%text)

	case (amp_token)
		call bit_and(left, right, res, node%op%text)

	case default
		write(*,*) err_eval_binary_op(node%op%text)
		call internal_error()

	end select

end subroutine eval_binary_expr

!===============================================================================

recursive module subroutine eval_name_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: id, rank_res, idim_, idim_res, sub_kind, type_, nelem
	integer(kind = 8) :: i8, index_, diff
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:)

	logical :: has_char_sub

	type(i64_vector_t), allocatable :: asubs(:)
	type(value_t) :: right, tmp

	!print *, ""
	!print *, "starting eval_name_expr()"
	!print *, 'searching identifier ', node%identifier%text
	!print *, "node is_loc = ", node%is_loc

	id = node%id_index
	if (node%is_loc) then
		type_ = state%locs%vals(id)%type
	else
		type_ = state%vars%vals(id)%type
	end if
	!print *, "id = ", id
	!print *, "type_ = ", kind_name(type_)

	if (type_ == unknown_type) then
		write(*,*) err_int(IC_UNKNOWN_NAME_EXPR_TYPE, "unknown name expr type")
		call internal_error()
	end if

	! Detect optional char-rank subscript on a string array.  Both
	! subscript_eval() and get_subscript_range() loop to rank_ (the element
	! rank), so the trailing char sub is naturally ignored by them.
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

	if (allocated(node%lsubscripts) .and. type_ == str_type) then
		!print *, 'string subscript RHS name expr'

		! Use str_char_slice() helper (isub=1 for scalar strings) which
		! handles scalar_sub and range_sub with omitted bounds.
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		if (node%is_loc) then
			res%str%s = str_char_slice(state%locs%vals(id)%str%s, node, state, 1)
		else
			res%str%s = str_char_slice(state%vars%vals(id)%str%s, node, state, 1)
		end if
		if (state%rt_halt) return

	else if (has_char_sub) then

		! String array with optional char subscript at index nelem+1.
		! The first nelem subscripts select element(s); the last one indexes
		! into the characters of each selected element.

		if (all(node%lsubscripts(1:nelem)%sub_kind == scalar_sub)) then

			! All element subscripts scalar → scalar string result
			i8 = subscript_eval(node, state)   ! element flat index (char sub ignored)
			res%type = str_type
			if (.not. allocated(res%str)) allocate(res%str)
			if (node%is_loc) then
				res%str%s = str_char_slice( &
					state%locs%vals(id)%array%str(i8+1)%s, node, state, nelem+1)
			else
				res%str%s = str_char_slice( &
					state%vars%vals(id)%array%str(i8+1)%s, node, state, nelem+1)
			end if
			if (state%rt_halt) return

		else

			! Element range/slice → string array result.  Reuse the standard
			! slice machinery; get_subscript_range ignores the trailing char sub.
			call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)
			if (state%rt_halt) return

			allocate(res%array)
			res%type = array_type
			res%array%kind = expl_array
			res%array%type = str_type
			res%array%rank = rank_res

			allocate(res%array%size(rank_res))
			idim_res = 1
			do idim_ = 1, nelem
				sub_kind = node%lsubscripts(idim_)%sub_kind
				select case (sub_kind)
				case (step_sub, range_sub, all_sub)
					diff = usubs(idim_) - lsubs(idim_)
					res%array%size(idim_res) = divceil(diff, ssubs(idim_))
					idim_res = idim_res + 1
				case (arr_sub)
					res%array%size(idim_res) = size(asubs(idim_)%v)
					idim_res = idim_res + 1
				case (scalar_sub)
					! noop
				case default
					call rt_throw(state, err_rt(RC_BAD_SUBSCRIPT_KIND, "bad subscript kind `"// &
						kind_name(sub_kind)//"`"))
					return
				end select
			end do
			res%array%len_ = product(res%array%size)
			call allocate_array(res, res%array%len_)

			! Iterate through all element subscripts; for each selected element
			! apply the char subscript and store the result string.
			subs = lsubs
			do i8 = 0, res%array%len_ - 1
				if (node%is_loc) then
					index_ = subscript_i32_eval(subs, state%locs%vals(id)%array)
					res%array%str(i8+1)%s = str_char_slice( &
						state%locs%vals(id)%array%str(index_+1)%s, node, state, nelem+1)
				else
					index_ = subscript_i32_eval(subs, state%vars%vals(id)%array)
					res%array%str(i8+1)%s = str_char_slice( &
						state%vars%vals(id)%array%str(index_+1)%s, node, state, nelem+1)
				end if
				if (state%rt_halt) return
				call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
			end do

		end if

	else if (allocated(node%lsubscripts)) then

		if (type_ /= array_type) then
			write(*,*) err_int(IC_BAD_TYPE_EXPECT_ARRAY, 'bad type, expected array')
			call internal_error()
		end if

		!print *, ""
		!print *, "sub kind 1 = ", kind_name(node%lsubscripts(1)%sub_kind)
		!print *, "rank = ", node%val%array%rank

		if (all(node%lsubscripts%sub_kind == scalar_sub)) then
			i8 = subscript_eval(node, state)

			if (node%is_loc) then
				call get_val(node, state%locs%vals(id), state, res, index_ = i8)
			else
				call get_val(node, state%vars%vals(id), state, res, index_ = i8)
			end if

		else if (size(node%lsubscripts) == 1 .and. &
		         node%lsubscripts(1)%sub_kind /= arr_sub) then

			! Rank-1 slice fast path: avoids allocating lsubs/ssubs/usubs/asubs.
			call eval_slice_rank1(node, state, res)
			if (state%rt_halt) return

		else

			call get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)
			if (state%rt_halt) return

			!print *, "type = ", kind_name( node%val%array%type )

			!print *, "type  = ", node%val%array%type
			!print *, "rank  = ", node%val%array%rank
			!print *, "size  = ", node%val%array%size
			!print *, "len_  = ", node%val%array%len_
			!print *, "cap   = ", node%val%array%cap

			allocate(res%array)
			res%type = array_type
			res%array%kind = expl_array
			res%array%type = node%val%array%type
			res%array%rank = rank_res

			allocate(res%array%size( rank_res ))
			idim_res = 1
			do idim_ = 1, size(lsubs)
				sub_kind = node%lsubscripts(idim_)%sub_kind
				select case (sub_kind)
				case (step_sub, range_sub, all_sub)

					diff = usubs(idim_) - lsubs(idim_)
					res%array%size(idim_res) = divceil(diff, ssubs(idim_))
					idim_res = idim_res + 1

				case (arr_sub)
					res%array%size(idim_res) = size(asubs(idim_)%v)
					idim_res = idim_res + 1

				case (scalar_sub)
					! noop
				case default
					call rt_throw(state, err_rt(RC_BAD_SUBSCRIPT_KIND, "bad subscript kind `"// &
						kind_name(sub_kind)//"`"))
					return
				end select
			end do
			res%array%len_ = product(res%array%size)

			!print *, "res len = ", res%array%len_
			!print *, "lsubs = ", lsubs
			!print *, "ssubs = ", ssubs
			!print *, "usubs = ", usubs
			!print *, "size  = ", res%array%size

			call allocate_array(res, res%array%len_)

			! Iterate through all subscripts in range and copy to result
			! array
			subs = lsubs
			do i8 = 0, res%array%len_ - 1
				!print *, 'subs = ', int(subs, 4)

				if (node%is_loc) then
					index_ = subscript_i32_eval(subs, state%locs%vals(id)%array)
					call get_array_val(state%locs%vals(id)%array, index_, tmp)
				else
					index_ = subscript_i32_eval(subs, state%vars%vals(id)%array)
					call get_array_val(state%vars%vals(id)%array, index_, tmp)
				end if

				call set_array_val(res%array, i8, tmp)
				call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
			end do
		end if

	else
		!print *, "name expr without subscripts"
		!print *, "id = ", id
		!print *, "size(vals) = ", size(state%vars%vals)

		if (node%is_loc) then
			res = state%locs%vals(id)
		else
			res = state%vars%vals(id)
		end if

	end if

end subroutine eval_name_expr

!===============================================================================

module subroutine eval_dot_expr(node, state, res)

	! This is an RHS dot expr.  LHS dots are handled in eval_assignment_expr().

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: id

	if (node%root_kind /= 0) then
		! Root is a fn_call_expr/method_call_expr (e.g. `fn().field`).
		! Evaluate the fn call (incl. any subscripts) to get the root struct value,
		! then apply the member chain to it.
		block
			type(syntax_node_t) :: root_node, wrapper
			type(value_t) :: root_val
			root_node = node
			root_node%kind = node%root_kind
			if (allocated(root_node%member)) deallocate(root_node%member)
			call syntax_eval(root_node, state, root_val)
			if (state%rt_halt) return
			wrapper%kind = dot_expr
			allocate(wrapper%member)
			wrapper%member = node%member
			call get_val(wrapper, root_val, state, res)
		end block
		return
	end if

	! This won't work for struct literal member access.  It only works for
	! `identifier.member`

	id = node%id_index
	if (node%is_loc) then
		call get_val(node, state%locs%vals(id), state, res)
	else
		call get_val(node, state%vars%vals(id), state, res)
	end if

end subroutine eval_dot_expr

!===============================================================================

recursive module subroutine eval_unary_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	type(value_t) :: right

	call syntax_eval(node%right, state, right)
	!print *, 'right = ', right

	res%type = right%type

	select case (node%op%kind)
	case (plus_token)
		res = right

	case (minus_token)
		call negate(right, res, node%op%text)

	case (not_keyword)
		call not_(right, res, node%op%text)

	case (bang_token)
		call bit_not(right, res, node%op%text)

	case default
		write(*,*) err_eval_unary_op(node%op%text)
		call internal_error()
	end select

end subroutine eval_unary_expr

!===============================================================================

module subroutine promote_i32_i64(val)

	! If val is i32 type, change it to i64 and copy the values
	!
	! TODO: consider refactoring with to_i64()? at least add internal error.
	! to_i64() is a fn which returns a new val, whereas this is a subroutine
	! that changes the type of a given value

	type(value_t), intent(inout) :: val

	if (val%type == i32_type) then
		val%type = i64_type
		val%sca%i64 = val%sca%i32
	end if

end subroutine promote_i32_i64

!===============================================================================

module function str_char_slice(s, node, state, isub) result(out)

	! Extract a character or substring from Fortran character string `s` using
	! subscript index `isub` in `node%lsubscripts` / `node%usubscripts`.
	!
	! This factors out the inline logic that was previously duplicated for scalar
	! strings; it works at any subscript index so the array-char-rank path can
	! pass isub = nelem + 1 without touching subscript_eval (which loops to rank_).
	!
	! Syntran indexing is 0-based, upper bound exclusive; Fortran slicing is
	! 1-based inclusive — hence the +1 / -1 offsets below.

	character(len = *), intent(in) :: s
	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state
	integer, intent(in) :: isub

	character(len = :), allocatable :: out

	!********

	integer(kind = 8) :: il, iu, step, i8, j8, n_out

	! str_slice_bounds() handles scalar/range/step/all subscript kinds
	! uniformly (0-based, upper-exclusive in the direction of step), so a
	! stepped/reversed slice like s[:-1:] works the same as it does for
	! arrays.
	call str_slice_bounds(node, isub, int(len(s), 8), state, il, iu, step)
	if (state%rt_halt) then
		out = ""
		return
	end if

	! Allocate `out` once at its final length and fill by index.  Growing it
	! one character at a time with `out = out // s(i8+1:i8+1)` reallocates and
	! copies the whole string on every iteration, making a length-L slice
	! O(L^2) -- disastrous for string-heavy hot loops (e.g. repeated slicing
	! in a tight loop over a long string).
	if (step > 0) then
		n_out = max(0_8, (iu - il + step - 1) / step)
	else
		n_out = max(0_8, (il - iu - step - 1) / (-step))
	end if
	allocate(character(len = n_out) :: out)

	i8 = il
	j8 = 1
	do while ((step > 0 .and. i8 < iu) .or. (step < 0 .and. i8 > iu))
		out(j8:j8) = s(i8+1 : i8+1)
		i8 = i8 + step
		j8 = j8 + 1
	end do

end function str_char_slice

!===============================================================================

end submodule syntran__eval_expr

!===============================================================================
