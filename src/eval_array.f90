
!===============================================================================

submodule (syntran__eval_m) syntran__eval_array

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine set_val(node, var, state, val, index_)

	! Assign var.mem = val, or recurse if mem is also a dot expr

	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(inout) :: var
	type(state_t), intent(inout) :: state

	type(value_t), intent(in) :: val

	integer(kind = 8), optional, intent(in) :: index_

	!********

	integer :: id
	integer(kind = 8) :: i8, j8

	if (allocated(node%lsubscripts) .and. allocated(node%member)) then

		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state)
		end if
		id = node%member%id_index

		! Recursion could still be required.  Unfortunately, if an
		! identifier has a subscript *and* a dot, then so does its node.  I
		! think this might require a bunch of if() logic like this instead
		! of any possibility of clean recursion

		if (node%member%kind == dot_expr) then
			! Recurse
			call set_val(node%member, var%struct(i8+1)%struct(id), state, val)
			return
		end if

		if (.not. allocated(node%member%lsubscripts)) then
			var%struct(i8+1)%struct(id) = val
			return
		end if
		!print *, "array dot chain"

		! Arrays chained by a dot: `a[0].b[0]`
		j8 = sub_eval(node%member, var%struct(i8+1)%struct(id), state)
		call set_array_val(var%struct(i8+1)%struct(id)%array, j8, val)
		return

	else if (allocated(node%lsubscripts)) then

		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state)
		end if
		if (var%array%type /= struct_type) then
			call set_array_val(var%array, i8, val)
			return
		end if

		var%struct(i8+1) = val
		return

	end if

	! `id` tracks whether each member is the 1st, 2nd, etc. member in the struct
	! array of its parent.  A local variable isnt' really needed but I think it
	! helps readability
	id = node%member%id_index

	if (node%member%kind == dot_expr) then
		! Recurse
		call set_val(node%member, var%struct(id), state, val)
		return
	end if

	! Base case

	if (.not. allocated(node%member%lsubscripts)) then
		var%struct(id) = val
		return
	end if
	!print *, "lsubscripts allocated"

	if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
		! Already caught in parser
		call rt_throw(state, err_rt(RC_STRUCT_ARRAY_SLICE, "struct array slices are not implemented"))
		return
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state)
	end if

	if (var%struct(id)%type == str_type) then
		var%struct(id)%str%s(i8+1: i8+1) = val%str%s
		return
	end if

	if (var%struct(id)%array%type /= struct_type) then
		call set_array_val(var%struct(id)%array, i8, val)
		return
	end if

	var%struct(id)%struct(i8+1) = val

end subroutine set_val

!===============================================================================

recursive module subroutine get_val(node, var, state, res, index_)

	! In nested expressions, like `a.b.c.d`, var begins as the top-most
	! (left-most, outer-most) value `a`
	!
	! Now realize that the node var expression could be any permutation like
	! `a.b[1].c[2].d`, with the tail value `d` being either a primitive type,
	! array, or another struct.  That is what this routine abstracts
	!
	! FIXME: if you change something in the getter, change it in the setter too
	!
	! Should I rename this eval_*() for consistency?

	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(in) :: var
	type(state_t), intent(inout) :: state

	integer(kind = 8), optional, intent(in) :: index_

	type(value_t), intent(out) :: res

	!********

	integer :: id
	integer(kind = 8) :: i8, j8

	!print *, "get_val()"

	if (allocated(node%lsubscripts) .and. allocated(node%member)) then

		if (present(index_)) then
			i8 = index_
		else

			if (.not. all(node%lsubscripts%sub_kind == scalar_sub)) then
				!print *, "slice sub"
				call rt_throw(state, err_rt(RC_STRUCT_ARRAY_SLICE, "struct array slices are not implemented"))
				return
			end if

			i8 = sub_eval(node, var, state)
		end if

		!print *, "i8 = ", i8
		id = node%member%id_index

		! Recursion could still be required.  Unfortunately, if an
		! identifier has a subscript *and* a dot, then so does its node.  I
		! think this might require a bunch of if() logic like this instead
		! of any possibility of clean recursion

		if (node%member%kind == dot_expr) then
			! Recurse
			call get_val(node%member, var%struct(i8+1)%struct(id), state, res)
			return
		end if

		if (.not. allocated(node%member%lsubscripts)) then
			res = var%struct(i8+1)%struct(id)
			return
		end if
		!print *, "array dot chain"

		if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
			!print *, "slice sub"
			call rt_throw(state, err_rt(RC_STRUCT_ARRAY_SLICE, "struct array slices are not implemented"))
			return
		end if

		! Arrays chained by a dot: `a[0].b[0]`
		j8 = sub_eval(node%member, var%struct(i8+1)%struct(id), state)
		!print *, "get_array_val 1"
		call get_array_val(var%struct(i8+1)%struct(id)%array, j8, res)
		return

	else if (allocated(node%lsubscripts)) then

		! Prefer sub_eval() over subscript_eval() because it doesn't make any
		! assumptions about var's relation to node
		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state)
		end if

		if (var%array%type /= struct_type) then
			!print *, "get_array_val 2"
			call get_array_val(var%array, i8, res)
			return
		end if

		res = var%struct(i8+1)
		res%type = struct_type
		res%struct_name = var%struct_name
		if (allocated(var%struct_cookie)) res%struct_cookie = var%struct_cookie
		return

	end if

	! `id` tracks whether each member is the 1st, 2nd, etc. member in the struct
	! array of its parent.  A local variable isnt' really needed but I think it
	! helps readability
	id = node%member%id_index

	if (node%member%kind == dot_expr) then
		! Recurse.  This branch was incorrectly entering sometimes because
		! `kind` was uninitialized, only on Windows in release build
		!print *, "recursing"
		call get_val(node%member, var%struct(id), state, res)
		return
	end if

	! Base case

	if (.not. allocated(node%member%lsubscripts)) then
		!print *, "base"
		res = var%struct(id)
		return
	end if
	!print *, "lsubscripts allocated"

	if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
		!print *, "slice sub"
		call rt_throw(state, err_rt(RC_STRUCT_ARRAY_SLICE, "struct array slices are not implemented"))
		return
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state)
	end if

	if (var%struct(id)%type == str_type) then
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = var%struct(id)%str%s(i8+1: i8+1)
		res%type = str_type
		return
	end if

	if (var%struct(id)%array%type /= struct_type) then
		!print *, "get_array_val 3"
		call get_array_val(var%struct(id)%array, i8, res)
		return
	end if

	res = var%struct(id)%struct(i8+1)

end subroutine get_val

!===============================================================================

recursive module subroutine eval_struct_instance(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	!print *, 'eval struct_instance_expr'
	!print *, 'struct identifier = ', node%identifier%text
	!print *, 'struct id_index   = ', node%id_index

	res%type = node%val%type
	res%struct_name = node%struct_name
	if (allocated(node%val%struct_cookie)) res%struct_cookie = node%val%struct_cookie

	if (allocated(res%struct)) deallocate(res%struct)
	allocate(res%struct( size(node%members) ))

	!print *, 'res type = ', kind_name(res%type)
	!print *, "num members = ", size(node%members)
	!print *, "num members = ", size(res%struct)

	do i = 1, size(node%members)
		call syntax_eval(node%members(i), state, res%struct(i))
	end do

end subroutine eval_struct_instance

!===============================================================================

module subroutine allocate_array(val, cap)

	type(value_t), intent(inout) :: val
	integer(kind = 8), intent(in) :: cap

	!! always done in caller
	!if (.not. allocated(val%array)) allocate(val%array)

	val%array%cap = cap

	select case (val%array%type)
	case (i32_type)
		allocate(val%array%i32( cap ))

	case (i64_type)
		allocate(val%array%i64( cap ))

	case (f32_type)
		allocate(val%array%f32( cap ))

	case (f64_type)
		allocate(val%array%f64( cap ))

	case (bool_type)
		allocate(val%array%bool( cap ))

	case (str_type)
		allocate(val%array%str( cap ))

	case (struct_type)
		allocate(val%struct( cap ))

	case default
		write(*,*) err_int(IC_ALLOC_ARRAY_TYPE, 'cannot allocate array of type `' &
			//kind_name(val%array%type)//'`')
		call internal_error()
	end select

end subroutine allocate_array

!===============================================================================

module function new_array(type, cap) result(vector)

	! TODO: use or combine allocate_array()

	integer, intent(in) :: type
	integer, intent(in), optional :: cap
	type(array_t) :: vector

	vector%len_ = 0

	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2  ! I think a small default makes sense here
	end if

	if      (type == i32_type) then
		allocate(vector%i32 ( vector%cap ))
	else if (type == i64_type) then
		allocate(vector%i64 ( vector%cap ))
	else if (type == f32_type) then
		allocate(vector%f32 ( vector%cap ))
	else if (type == f64_type) then
		allocate(vector%f64 ( vector%cap ))
	else if (type == bool_type) then
		allocate(vector%bool( vector%cap ))
	else if (type == str_type) then
		allocate(vector%str ( vector%cap ))
	else
		write(*,*) err_int(IC_ARRAY_TYPE_NOT_IMPL, 'array type not implemented')
		call internal_error()
	end if

	vector%type = type

end function new_array

!===============================================================================

module subroutine compound_assign(lhs, rhs, op)
	! TODO: rename?  This also handles regular assignment

	! lhs += rhs;
	!   or
	! lhs *= rhs;
	!   etc.

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in) :: rhs

	type(syntax_token_t), intent(in) :: op

	!******

	type(value_t) :: tmp  ! necessary for arrays

	if (op%kind /= equals_token) tmp = lhs

	select case (op%kind)
	case (equals_token)
		!print *, 'assign'
		!lhs = rhs  ! simply overwrite
		call assign_(lhs, rhs, op%text)

	case (plus_equals_token)
		call add(tmp, rhs, lhs, op%text)

	case (minus_equals_token)
		call subtract(tmp, rhs, lhs, op%text)

	case (star_equals_token)
		call mul(tmp, rhs, lhs, op%text)

	case (slash_equals_token)
		call div(tmp, rhs, lhs, op%text)

	case (sstar_equals_token)
		call pow(tmp, rhs, lhs, op%text)

	case (percent_equals_token)
		call mod_(tmp, rhs, lhs, op%text)

	case (amp_equals_token)
		call bit_and(tmp, rhs, lhs, op%text)

	case (pipe_equals_token)
		call bit_or(tmp, rhs, lhs, op%text)

	case (caret_equals_token)
		call bit_xor(tmp, rhs, lhs, op%text)

	case (lless_equals_token)
		call left_shift(tmp, rhs, lhs, op%text)

	case (ggreater_equals_token)
		call right_shift(tmp, rhs, lhs, op%text)

	case default
		write(*,*) err_int(IC_UNEXPECTED_ASSIGN_OP, 'unexpected assignment operator '//quote(op%text))
		call internal_error()
	end select

end subroutine compound_assign

!===============================================================================

module subroutine eval_subscript_1d(node, state, i, lsub, ssub, usub, asub, contributes_rank)

	! Evaluate the lower bound, step, and upper bound for one dimension of a
	! subscripted array slice.  Extracted from get_subscript_range so that the
	! rank-1 fast paths (eval_slice_rank1 / eval_assign_slice_rank1) can obtain
	! scalar bounds without allocating the full lsubs/ssubs/usubs arrays.

	type(syntax_node_t), intent(in)    :: node
	type(state_t),       intent(inout) :: state
	integer,             intent(in)    :: i            ! 1-based dimension index
	integer(kind = 8),   intent(out)   :: lsub, ssub, usub
	type(i64_vector_t),  intent(inout) :: asub         ! populated for arr_sub only
	logical,             intent(out)   :: contributes_rank

	!********

	integer :: id
	integer(kind = 8) :: sz
	type(value_t) :: asubval, lsubval, usubval, ssubval

	id = node%id_index
	contributes_rank = .false.
	lsub = 0
	ssub = 1
	usub = 0

	select case (node%lsubscripts(i)%sub_kind)
	case (all_sub)
		lsub = 0
		ssub = 1
		if (node%is_loc) then
			usub = state%locs%vals(id)%array%size(i)
		else
			usub = state%vars%vals(id)%array%size(i)
		end if
		contributes_rank = .true.

	case (range_sub)
		ssub = 1

		if (node%lsubscripts(i)%lsub_omit) then
			lsub = 0
		else
			call syntax_eval(node%lsubscripts(i), state, lsubval)
			lsub = lsubval%to_i64()
		end if

		if (node%lsubscripts(i)%usub_omit) then
			if (node%is_loc) then
				usub = state%locs%vals(id)%array%size(i)
			else
				usub = state%vars%vals(id)%array%size(i)
			end if
		else
			call syntax_eval(node%usubscripts(i), state, usubval)
			usub = usubval%to_i64()
		end if

		contributes_rank = .true.

	case (step_sub)
		! Evaluate step FIRST so its sign determines default bounds
		call syntax_eval(node%ssubscripts(i), state, ssubval)
		ssub = ssubval%to_i64()

		if (ssub == 0) then
			write(*,*) err_int(IC_SUBSCRIPT_STEP_ZERO, 'subscript step is 0')
			call internal_error()
		end if

		if (node%lsubscripts(i)%lsub_omit .or. node%lsubscripts(i)%usub_omit) then
			if (node%is_loc) then
				sz = state%locs%vals(id)%array%size(i)
			else
				sz = state%vars%vals(id)%array%size(i)
			end if
		end if

		if (node%lsubscripts(i)%lsub_omit) then
			lsub = merge(sz - 1_8, 0_8, ssub < 0)
		else
			call syntax_eval(node%lsubscripts(i), state, lsubval)
			lsub = lsubval%to_i64()
		end if

		if (node%lsubscripts(i)%usub_omit) then
			usub = merge(-1_8, sz, ssub < 0)
		else
			call syntax_eval(node%usubscripts(i), state, usubval)
			usub = usubval%to_i64()
		end if

		contributes_rank = .true.

	case (scalar_sub)
		! Scalar subs are converted to a range-1 sub so we can
		! iterate later without further case logic
		call syntax_eval(node%lsubscripts(i), state, lsubval)
		lsub = lsubval%to_i64()
		usub = lsub + 1
		ssub = 1
		! contributes_rank stays .false.

	case (arr_sub)
		call syntax_eval(node%lsubscripts(i), state, asubval)

		! TODO: refactor `if` to select/case. There is a fn
		! value_to_i64_array() but it returns an array_t

		if      (asubval%array%type == i32_type) then
			asub%v = asubval%array%i32
		else if (asubval%array%type == i64_type) then
			asub%v = asubval%array%i64
		else
			write(*,*) err_int(IC_BAD_ARRAY_SUBSCRIPT_TYPE, 'bad array subscript type')
			call internal_error()
		end if

		lsub = asub%v(1)
		usub = 1
		contributes_rank = .true.

	case default
		write(*,*) err_int(IC_EVAL_SUBSCRIPT_KIND, 'cannot evaluate subscript kind')
		call internal_error()

	end select

end subroutine eval_subscript_1d

!===============================================================================

module subroutine get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)

	! Evaluate the lower- and upper-bounds of each range of a subscripted array
	! slice
	!
	! TODO: `rank_res` is a misnomer.  For LHS slicing it's the rank of the LHS
	! *after* being sliced

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(i64_vector_t), allocatable, intent(out) :: asubs(:)
	integer(kind = 8), allocatable, intent(out) :: lsubs(:), ssubs(:), usubs(:)
	integer, intent(out) :: rank_res

	!********

	integer :: i, id, rank_
	logical :: cr

	id = node%id_index
	if (node%is_loc) then
		rank_ = state%locs%vals(id)%array%rank
	else
		rank_ = state%vars%vals(id)%array%rank
	end if

	allocate(asubs(rank_), lsubs(rank_), ssubs(rank_), usubs(rank_))
	rank_res = 0
	do i = 1, rank_
		call eval_subscript_1d(node, state, i, lsubs(i), ssubs(i), usubs(i), asubs(i), cr)
		if (cr) rank_res = rank_res + 1
	end do
	!print *, 'lsubs = ', lsubs
	!print *, 'ssubs = ', ssubs
	!print *, 'usubs = ', usubs
	!print *, 'rank_res = ', rank_res

end subroutine get_subscript_range

!===============================================================================

module subroutine get_next_subscript(asubs, lsubs, ssubs, usubs, subs)

	! This is like a bignum += 1 algorithm but in an arbitrary mixed radix.  It
	! was a bit more straightforward before I added index arrays (asub)

	type(i64_vector_t), intent(in), allocatable :: asubs(:)
	integer(kind = 8) , intent(in) :: lsubs(:), ssubs(:)
	integer(kind = 8) , intent(inout) :: usubs(:), subs(:)

	!********

	logical :: carry
	integer :: j, n

	j = 1
	if (allocated(asubs(j)%v)) then
		n = size(asubs(j)%v)
		carry = j < size(subs) .and. subs(j) == asubs(j)%v(n)
	else
		carry = j < size(subs) .and. subs(j) >= usubs(j) - 1
	end if

	do while (carry)
		subs(j) = lsubs(j)
		if (allocated(asubs(j)%v)) then
			usubs(j) = 1
		end if
		j = j + 1

		if (allocated(asubs(j)%v)) then
			n = size(asubs(j)%v)
			carry = j < size(subs) .and. subs(j) == asubs(j)%v(n)
		else
			carry = j < size(subs) .and. subs(j) >= usubs(j) - 1
		end if
	end do

	if (allocated(asubs(j)%v)) then

		! usubs is overloaded as an index for array subscripts.  This is why it
		! is inout while most other args are in
		usubs(j) = usubs(j) + 1
		if (usubs(j) <= size(asubs(j)%v)) then
			subs(j) = asubs(j)%v( usubs(j) )
		end if

	else
		subs(j) = subs(j) + ssubs(j)
	end if

	!print *, "next sub = ", subs

end subroutine get_next_subscript

!===============================================================================

module function subscript_i32_eval(subs, array) result(index_)

	! subscript_eval() but with a primitive subs int array
	!
	! Is there a way to copy a slice without doing so much math?
	!
	! TODO: bound checking if enabled.  unlike subscript_eval(),
	! we can do it here outside the i8 loop

	integer(kind = 8), intent(in) :: subs(:)
	type(array_t) :: array

	integer(kind = 8) :: index_

	!********

	integer :: j
	integer(kind = 8) :: prod

	prod  = 1
	index_ = 0
	do j = 1, array%rank
		!print *, 'j = ', j
		index_ = index_ + prod * subs(j)
		prod = prod * array%size(j)
	end do
	!print *, 'index_ = ', index_

end function subscript_i32_eval

!===============================================================================

module function sub_eval(node, var, state) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_
	!
	! Can this be dried up with subscript_eval()?

	type(syntax_node_t) :: node
	type(value_t) :: var
	type(state_t), intent(inout) :: state

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting sub_eval()'

	if (var%type == str_type) then
		call syntax_eval(node%lsubscripts(1), state, subscript)
		index_ = subscript%to_i64()
		return
	end if

	prod  = 1
	index_ = 0
	do i = 1, var%array%rank
		!print *, 'i = ', i

		call syntax_eval(node%lsubscripts(i), state, subscript)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%to_i64()
		prod   = prod * var%array%size(i)

	end do
	!print *, "index_ = ", index_

end function sub_eval

!===============================================================================

recursive module function subscript_eval(node, state) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_

	type(syntax_node_t) :: node
	type(state_t), intent(inout) :: state

	integer(kind = 8) :: index_

	!******

	integer :: i, id, type_, rank_
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	!print *, "node is_loc = ", node%is_loc
	id = node%id_index
	if (node%is_loc) then
		type_ = state%locs%vals(id)%type
	else
		type_ = state%vars%vals(id)%type
	end if

	! str scalar with single char subscript
	if (type_ == str_type) then
		call syntax_eval(node%lsubscripts(1), state, subscript)
		index_ = subscript%to_i64()
		return
	end if

	!if (type_ /= array_type) then
	!	! internal_error?
	!end if

	if (node%is_loc) then
		rank_ = state%locs%vals(id)%array%rank
	else
		rank_ = state%vars%vals(id)%array%rank
	end if

	! This could be refactored to run syntax_eval() on each subscript first, and
	! then call subscript_i32_eval() after the loop.  There would be a small
	! memory overhead to save i32 sub array but probably no significant time or
	! space perf difference
	prod  = 1
	index_ = 0
	do i = 1, rank_
		!print *, 'i = ', i

		call syntax_eval(node%lsubscripts(i), state, subscript)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%to_i64()

		if (node%is_loc) then
			prod  = prod * state%locs%vals(id)%array%size(i)
		else
			prod  = prod * state%vars%vals(id)%array%size(i)
		end if

	end do

end function subscript_eval

!===============================================================================

module subroutine array_at(val, kind_, i, lbound_, step, ubound_, len_, array, &
		elems, str_, state)

	! This lazily gets an array value at an index i without expanding the whole
	! implicit array in memory.  Used for for loops
	!
	! TODO: way too many args.  Bundle lbound_, step, ubound_, len_, array, and
	! elems into a new struct named `array_parts`.
	!
	! It's also worth considering whether the existence of an array_at() fn is
	! the right abstraction at all.  It only gets called in one place.  Is the
	! memory saving worthwhile?

	type(value_t), intent(inout) :: val

	integer, intent(in) :: kind_

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(in) :: lbound_, step, ubound_, len_

	type(array_t), intent(in) :: array

	type(syntax_node_t), allocatable :: elems(:)

	type(value_t), intent(in) :: str_

	type(state_t), intent(inout) :: state

	!*********

	select case (kind_)
	case (bound_array)

		if (val%type == i32_type) then
			val%sca%i32 = lbound_%sca%i32 + int(i) - 1
		else !if (val%type == i64_type) then
			val%sca%i64 = lbound_%sca%i64 + i - 1
		end if

	case (step_array)

		select case (val%type)
		case (i32_type)
			val%sca%i32 = lbound_%sca%i32 + int(i - 1) * step%sca%i32

		case (i64_type)
			val%sca%i64 = lbound_%sca%i64 + (i - 1) * step%sca%i64

		case (f32_type)
			val%sca%f32 = lbound_%sca%f32 + real(i - 1) * step%sca%f32

		case (f64_type)
			val%sca%f64 = lbound_%sca%f64 + real(i - 1, 8) * step%sca%f64

		end select

	case (len_array)

		select case (val%type)
		case (f32_type)
			val%sca%f32 = lbound_%sca%f32 + real(i - 1) * &
				(ubound_%sca%f32 - lbound_%sca%f32) / real((len_%to_i64() - 1))

		case (f64_type)
			val%sca%f64 = lbound_%sca%f64 + real(i - 1, 8) * &
				(ubound_%sca%f64 - lbound_%sca%f64) / real((len_%to_i64() - 1), 8)

		end select

	case (expl_array, size_array)
		call syntax_eval(elems(i), state, val)

	case (unif_array)
		val = lbound_

	case (array_expr)
		! Non-primary array expr
		call get_array_val(array, i - 1, val)

	case (str_type)
		!val%type = str_type
		if (.not. allocated(val%str)) allocate(val%str)
		val%str%s = str_%str%s(i:i)
		!print *, "val s = ", val%str%s

	case default
		write(*,*) err_int(IC_FOR_ARRAY_KIND, 'for loop not implemented for this array kind')
		call internal_error()
	end select

end subroutine array_at

!===============================================================================

module subroutine get_array_val(array, i, val)

	type(array_t), intent(in) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(out) :: val

	!print *, 'starting get_array_val()'
	!print *, 'array%type = ', kind_name(array%type)

	val%type = array%type
	select case (array%type)
		case (bool_type)
			val%sca%bool = array%bool(i + 1)

		case (i32_type)
			val%sca%i32 = array%i32(i + 1)

		case (i64_type)
			val%sca%i64 = array%i64(i + 1)

		case (f32_type)
			val%sca%f32 = array%f32(i + 1)

		case (f64_type)
			val%sca%f64 = array%f64(i + 1)

		case (str_type)
			val%str = array%str(i + 1)

		case default
			write(*,*) err_int(IC_BAD_ARRAY_VAL_TYPE, "bad type in get_array_val")
			call internal_error()

	end select

end subroutine get_array_val

!===============================================================================

module subroutine set_array_val(array, i, val)

	type(array_t), intent(inout) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(in) :: val

	!print *, 'starting set_array_val()'
	!print *, 'array%type = ', kind_name(array%type)
	!print *, 'val%type   = ', kind_name(val%type)

	! array%type is already set
	select case (array%type)
		case (bool_type)
			array%bool(i + 1) = val%sca%bool

		case (i32_type)
			array%i32(i + 1) = val%to_i32()

		case (i64_type)
			array%i64(i + 1) = val%to_i64()

		case (f32_type)
			array%f32(i + 1) = val%to_f32()

		case (f64_type)
			array%f64(i + 1) = val%to_f64()

		case (str_type)
			array%str(i + 1) = val%str

	end select

end subroutine set_array_val

!===============================================================================

module subroutine eval_slice_rank1(node, state, res)

	! Allocation-free fast path for rank-1 array read slices:
	!   a[i:j], a[:j], a[i:], a[i:j:k], a[:k:], a[:]
	! where a is a 1-D array and the single subscript is NOT arr_sub.
	!
	! Uses scalar lsub/ssub/usub on the stack instead of allocating
	! lsubs/ssubs/usubs/asubs/subs arrays as get_subscript_range does.

	type(syntax_node_t), intent(in)  :: node
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(out) :: res

	!********

	integer :: id
	integer(kind = 8) :: lsub, ssub, usub, len_, idx, i8
	type(i64_vector_t) :: asub_unused
	logical :: cr_unused
	type(value_t) :: tmp

	id = node%id_index

	call eval_subscript_1d(node, state, 1, lsub, ssub, usub, asub_unused, cr_unused)

	len_ = divceil(usub - lsub, ssub)
	if (lsub > usub .and. ssub > 0) len_ = 0_8
	if (lsub < usub .and. ssub < 0) len_ = 0_8

	allocate(res%array)
	res%type = array_type
	res%array%kind = expl_array
	if (node%is_loc) then
		res%array%type = state%locs%vals(id)%array%type
	else
		res%array%type = state%vars%vals(id)%array%type
	end if
	res%array%rank = 1
	allocate(res%array%size(1))
	res%array%size(1) = len_
	res%array%len_ = len_

	call allocate_array(res, len_)

	idx = lsub
	if (node%is_loc) then
		do i8 = 0, len_ - 1
			call get_array_val(state%locs%vals(id)%array, idx, tmp)
			call set_array_val(res%array, i8, tmp)
			idx = idx + ssub
		end do
	else
		do i8 = 0, len_ - 1
			call get_array_val(state%vars%vals(id)%array, idx, tmp)
			call set_array_val(res%array, i8, tmp)
			idx = idx + ssub
		end do
	end if

end subroutine eval_slice_rank1

!===============================================================================

module subroutine eval_assign_slice_rank1(node, state, id, res)

	! Fast path for rank-1 array write slices: a[i:j] = rhs, a[i:j] += rhs, etc.
	! On entry res holds the already-evaluated RHS.
	! On return res holds the modified slice (matching the old tmp_array return).

	type(syntax_node_t), intent(in)    :: node
	type(state_t),       intent(inout) :: state
	integer,             intent(in)    :: id    ! node%id_index, already extracted
	type(value_t),       intent(inout) :: res   ! RHS in, modified slice out

	!********

	integer(kind = 8) :: lsub, ssub, usub, len_, idx, i8
	integer :: arr_type
	type(i64_vector_t) :: asub_unused
	logical :: cr_unused
	type(value_t) :: rhs_elem, elem_val, result_val

	call eval_subscript_1d(node, state, 1, lsub, ssub, usub, asub_unused, cr_unused)

	len_ = divceil(usub - lsub, ssub)
	if (lsub > usub .and. ssub > 0) len_ = 0_8
	if (lsub < usub .and. ssub < 0) len_ = 0_8

	! For scalar RHS, capture it now before building result_val.
	if (res%type /= array_type) rhs_elem = res

	if (node%is_loc) then
		arr_type = state%locs%vals(id)%array%type
	else
		arr_type = state%vars%vals(id)%array%type
	end if

	! Build the result array (the modified slice) into a separate local so we
	! don't conflict with res%array, which may already be allocated (array RHS).
	allocate(result_val%array)
	result_val%type = array_type
	result_val%array%kind = expl_array
	result_val%array%type = arr_type
	result_val%array%rank = 1
	allocate(result_val%array%size(1))
	result_val%array%size(1) = len_
	result_val%array%len_ = len_
	call allocate_array(result_val, len_)

	idx = lsub
	if (node%is_loc) then
		do i8 = 0, len_ - 1
			if (res%type == array_type) call get_array_val(res%array, i8, rhs_elem)
			call get_array_val(state%locs%vals(id)%array, idx, elem_val)
			call compound_assign(elem_val, rhs_elem, node%op)
			call set_array_val(state%locs%vals(id)%array, idx, elem_val)
			call set_array_val(result_val%array, i8, elem_val)
			idx = idx + ssub
		end do
	else
		do i8 = 0, len_ - 1
			if (res%type == array_type) call get_array_val(res%array, i8, rhs_elem)
			call get_array_val(state%vars%vals(id)%array, idx, elem_val)
			call compound_assign(elem_val, rhs_elem, node%op)
			call set_array_val(state%vars%vals(id)%array, idx, elem_val)
			call set_array_val(result_val%array, i8, elem_val)
			idx = idx + ssub
		end do
	end if

	res = result_val   ! return the modified slice, as the general path does

end subroutine eval_assign_slice_rank1

!===============================================================================

end submodule syntran__eval_array

!===============================================================================
