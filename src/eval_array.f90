
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
		write(*,*) err_rt_prefix//"struct array slices are not implemented"//color_reset
		call internal_error()
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state)
	end if

	if (var%struct(id)%type == str_type) then
		!$omp critical(str_access)
		var%struct(id)%sca%str%s(i8+1: i8+1) = val%sca%str%s
		!$omp end critical(str_access)
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
				write(*,*) err_rt_prefix//"struct array slices are not implemented"//color_reset
				call internal_error()
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
			write(*,*) err_rt_prefix//"struct array slices are not implemented"//color_reset
			call internal_error()
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
		write(*,*) err_rt_prefix//"struct array slices are not implemented"//color_reset
		call internal_error()
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state)
	end if

	if (var%struct(id)%type == str_type) then
		!$omp critical(str_access)
		res%sca%str%s = var%struct(id)%sca%str%s(i8+1: i8+1)
		!$omp end critical(str_access)
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
		write(*,*) err_int_prefix//'cannot allocate array of type `' &
			//kind_name(val%array%type)//'`'//color_reset
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
		write(*,*) err_int_prefix//'array type not implemented'//color_reset
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
		write(*,*) err_int_prefix//'unexpected assignment operator ', quote(op%text)//color_reset
		call internal_error()
	end select

end subroutine compound_assign

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

	type(value_t) :: asubval, lsubval, usubval, ssubval

	id = node%id_index
	if (node%is_loc) then
		rank_ = state%locs%vals(id)%array%rank
	else
		rank_ = state%vars%vals(id)%array%rank
	end if

	allocate(asubs(rank_), lsubs(rank_), ssubs(rank_), usubs(rank_))
	rank_res = 0
	do i = 1, rank_

		if (node%lsubscripts(i)%sub_kind == all_sub) then
			lsubs(i) = 0
			!print *, 'lsubs(i) = ', lsubs(i)
		else if (node%lsubscripts(i)%sub_kind == arr_sub) then

			!print *, "arr_sub"

			call syntax_eval(node%lsubscripts(i), state, asubval)

			! TODO: refactor `if` to select/case. There is a fn
			! value_to_i64_array() but it returns an array_t

			if      (asubval%array%type == i32_type) then
				asubs(i)%v = asubval%array%i32
			else if (asubval%array%type == i64_type) then
				asubs(i)%v = asubval%array%i64
			else
				write(*,*) err_int_prefix//'bad array subscript type'//color_reset
				call internal_error()
			end if

			!print *, "asubs = ", asubs(i)%v

		else
			call syntax_eval(node%lsubscripts(i), state, lsubval)
			lsubs(i) = lsubval%to_i64()
		end if

		!********

		select case (node%lsubscripts(i)%sub_kind)
		case (all_sub)
			ssubs(i) = 1
			if (node%is_loc) then
				usubs(i) = state%locs%vals(id)%array%size(i)
			else
				usubs(i) = state%vars%vals(id)%array%size(i)
			end if
			!print *, 'usubs(i) = ', usubs(i)

			rank_res = rank_res + 1

		case (range_sub)
			! Range subs are basically handled as a step sub with step == 1
			ssubs(i) = 1

			call syntax_eval(node%usubscripts(i), state, usubval)
			usubs(i) = usubval%to_i64()

			rank_res = rank_res + 1

		case (step_sub)
			call syntax_eval(node%ssubscripts(i), state, ssubval)
			call syntax_eval(node%usubscripts(i), state, usubval)
			ssubs(i) = ssubval%to_i64()
			usubs(i) = usubval%to_i64()

			if (ssubs(i) == 0) then
				write(*,*) err_int_prefix//'subscript step is 0'//color_reset
				call internal_error()
			end if

			rank_res = rank_res + 1

		case (scalar_sub)
			! Scalar subs are converted to a range-1 sub so we can
			! iterate later without further case logic
			usubs(i) = lsubs(i) + 1
			ssubs(i) = 1

		case (arr_sub)
			lsubs(i) = asubs(i)%v(1)  ! reset to this after carrying
			usubs(i) = 1              ! use this as an index to increment and get the next asub
			rank_res = rank_res + 1

		case default
			write(*,*) err_int_prefix//'cannot evaluate subscript kind'//color_reset
			call internal_error()

		end select

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
		!$omp critical(str_access)
		val%sca%str%s = str_%sca%str%s(i:i)
		!$omp end critical(str_access)
		!print *, "val s = ", val%sca%str%s

	case default
		write(*,*) err_int_prefix//'for loop not implemented for this array kind'//color_reset
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
			val%sca%str = array%str(i + 1)

		case default
			write(*,*) err_int_prefix//"bad type in get_array_val"//color_reset
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
			array%str(i + 1) = val%sca%str

	end select

end subroutine set_array_val

!===============================================================================

end submodule syntran__eval_array

!===============================================================================
