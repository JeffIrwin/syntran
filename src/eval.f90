
!===============================================================================

module syntran__eval_m

	use iso_fortran_env

	use syntran__bool_m
	use syntran__math_m
	use syntran__types_m

	implicit none

	!********

	type state_t
		! Run time (eval time) state

		logical :: quiet

		type(fns_t) :: fns

		!type(structs_t) :: structs

		type(vars_t) :: vars

		logical :: returned

	end type state_t

!===============================================================================

contains

!===============================================================================

recursive subroutine syntax_eval(node, state, res)

	! TODO: add diagnostics to state for runtime errors (bounds overflow, rank
	! mismatch, etc.)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	! I experimented with making res intent(inout) in commit 993345ad, but it
	! had a negative imact on perf, making gfortran about twice as slow on aoc
	! tests, likely due to the extra work of checking `if allocated(...)
	! deallocate` in lots of places
	type(value_t), intent(out) :: res

	!********

	!print *, "starting syntax_eval()"
	!print *, "node kind = ", kind_name(node%kind)

	if (node%is_empty) then
		!print *, 'returning'
		return
	end if

	!********

	! I'm being a bit loose with consistency on select case indentation but
	! I don't want a gigantic diff

	select case (node%kind)

	case (literal_expr)
		res = node%val  ! this handles ints, bools, etc.

	case (array_expr)
		call eval_array_expr(node, state, res)

	case (for_statement)
		call eval_for_statement(node, state, res)

	case (while_statement)
		call eval_while_statement(node, state, res)

	case (if_statement)
		call eval_if_statement(node, state, res)

	case (return_statement)
		call eval_return_statement(node, state, res)

	case (translation_unit)
		call eval_translation_unit(node, state, res)

	case (block_statement)
		call eval_block_statement(node, state, res)

	case (assignment_expr)
		call eval_assignment_expr(node, state, res)

	case (let_expr)

		! Assign return value
		call syntax_eval(node%right, state, res)

		!print *, 'assigning identifier ', quote(node%identifier%text)

		state%vars%vals(node%id_index) = res

		!print *, "res type = ", kind_name(res%type)
		!print *, "allocated(struct) = ", allocated(res%struct)
		!if (res%type == struct_type) then
		!	print *, "size struct = ", size(res%struct)
		!	print *, "size struct = ", size( state%vars%vals(node%id_index)%struct )
		!	do i = 1, size(res%struct)
		!		print *, "struct[", str(i), "] = ", res%struct(i)%to_str()
		!		print *, "struct[", str(i), "] = ", state%vars%vals(node%id_index)%struct(i)%to_str()
		!	end do
		!end if

	case (fn_call_expr)  ! user-defined
		call eval_fn_call(node, state, res)

	case (fn_call_intr_expr)
		call eval_fn_call_intr(node, state, res)

	case (struct_instance_expr)
		call eval_struct_instance(node, state, res)

	case (name_expr)
		!print *, "name_expr"
		call eval_name_expr(node, state, res)

	case (dot_expr)
		call eval_dot_expr(node, state, res)

	case (unary_expr)
		call eval_unary_expr(node, state, res)

	case (binary_expr)
		call eval_binary_expr(node, state, res)

	case default
		write(*,*) err_eval_node(kind_name(node%kind))
		call internal_error()

	end select

end subroutine syntax_eval

!===============================================================================

recursive subroutine eval_binary_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: larrtype, rarrtype

	type(value_t) :: left, right

	call syntax_eval(node%left , state, left )
	call syntax_eval(node%right, state, right)

	!print *, 'left  type = ', kind_name(left%type)
	!print *, 'right type = ', kind_name(right%type)

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

	case default
		write(*,*) err_eval_binary_op(node%op%text)
		call internal_error()

	end select

end subroutine eval_binary_expr

!===============================================================================

recursive subroutine eval_name_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: rank_res, idim_, idim_res
	integer(kind = 8) :: il, iu, i8, index_
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	type(value_t) :: right, tmp

	!print *, "starting eval_name_expr()"
	!print *, 'searching identifier ', node%identifier%text

	if (allocated(node%lsubscripts) .and. &
		state%vars%vals(node%id_index)%type == str_type) then
		!print *, 'string subscript RHS name expr'

		!print *, 'str type'
		res%type = state%vars%vals(node%id_index)%type

		select case (node%lsubscripts(1)%sub_kind)
		case (scalar_sub)
			i8 = subscript_eval(node, state)
			!print *, 'i8 = ', i8
			res%sca%str%s = state%vars%vals(node%id_index)%sca%str%s(i8+1: i8+1)

		case (range_sub)

			! TODO: str all_sub

			il = subscript_eval(node, state) + 1

			! This feels inconsistent and not easy to extend to higher ranks
			call syntax_eval(node%usubscripts(1), state, right)
			iu = right%to_i64() + 1

			!print *, ''
			!print *, 'identifier ', node%identifier%text
			!print *, 'il = ', il
			!print *, 'iu = ', iu
			!print *, 'str = ', state%vars%vals(node%id_index)%sca%str%s

			! Not inclusive of upper bound
			res%sca%str%s = state%vars%vals(node%id_index)%sca%str%s(il: iu-1)

		case default
			write(*,*) err_int_prefix//'unexpected subscript kind'//color_reset
			call internal_error()
		end select

	else if (allocated(node%lsubscripts)) then

		if (state%vars%vals(node%id_index)%type /= array_type) then
			write(*,*) err_int_prefix//'bad type, expected array'//color_reset
			call internal_error()
		end if

		!print *, 'sub kind 1 = ', kind_name(node%lsubscripts(1)%sub_kind)
		!print *, 'rank = ', node%val%array%rank

		if (all(node%lsubscripts%sub_kind == scalar_sub)) then
			i8 = subscript_eval(node, state)
			call get_val(node, state%vars%vals(node%id_index), state, res, index_ = i8)
		else

			call get_subscript_range(node, state, lsubs, usubs, rank_res)

			!print *, 'type = ', kind_name( node%val%array%type )

			!print *, 'type  = ', node%val%array%type
			!print *, 'rank  = ', node%val%array%rank
			!print *, 'size  = ', node%val%array%size
			!print *, 'len_  = ', node%val%array%len_
			!print *, 'cap   = ', node%val%array%cap

			allocate(res%array)
			res%type = array_type
			res%array%kind = expl_array
			res%array%type = node%val%array%type
			res%array%rank = rank_res

			allocate(res%array%size( rank_res ))
			idim_res = 1
			do idim_ = 1, size(lsubs)
				select case (node%lsubscripts(idim_)%sub_kind)
				case (range_sub, all_sub)

					res%array%size(idim_res) = usubs(idim_) - lsubs(idim_)

					idim_res = idim_res + 1
				end select
			end do
			!print *, 'res size = ', res%array%size

			res%array%len_ = product(res%array%size)
			!print *, 'res len = ', res%array%len_

			call allocate_array(res, res%array%len_)

			! Iterate through all subscripts in range and copy to result
			! array
			subs = lsubs
			do i8 = 0, res%array%len_ - 1
				!print *, 'subs = ', int(subs, 4)

				index_ = subscript_i32_eval(subs, state%vars%vals(node%id_index)%array)
				call get_array_val(state%vars%vals(node%id_index)%array, index_, tmp)
				call set_array_val(res%array, i8, tmp)
				call get_next_subscript(lsubs, usubs, subs)
			end do
		end if

	else
		!print *, "name expr without subscripts"
		!print *, "id_index = ", node%id_index
		!print *, "size(vals) = ", size(state%vars%vals)
		res = state%vars%vals(node%id_index)

	end if

end subroutine eval_name_expr

!===============================================================================

subroutine eval_dot_expr(node, state, res)

	! This is an RHS dot expr.  LHS dots are handled in eval_assignment_expr().

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	!print *, "eval dot_expr"
	!print *, "id_index = ", node%id_index
	!print *, "struct[", str(i), "] = ", state%vars%vals(node%id_index)%struct(i)%to_str()

	! This won't work for struct literal member access.  It only works for
	! `identifier.member`

	call get_val(node, state%vars%vals(node%id_index), state, res)

end subroutine eval_dot_expr

!===============================================================================

recursive subroutine set_val(node, var, state, val, index_)

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
		var%struct(id)%sca%str%s(i8+1: i8+1) = val%sca%str%s
		return
	end if

	if (var%struct(id)%array%type /= struct_type) then
		call set_array_val(var%struct(id)%array, i8, val)
		return
	end if

	var%struct(id)%struct(i8+1) = val

end subroutine set_val

!===============================================================================

recursive subroutine get_val(node, var, state, res, index_)

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
		res%sca%str%s = var%struct(id)%sca%str%s(i8+1: i8+1)
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

recursive subroutine eval_struct_instance(node, state, res)

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

		!print *, "mem[", str(i), "] = ", res%struct(i)%to_str()
		!res = node%val%struct( node%right%id_index )
		!node%members(i)%val = res

	end do

end subroutine eval_struct_instance

!===============================================================================

recursive subroutine eval_fn_call(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	character(len = :), allocatable :: color

	integer :: i, io

	logical :: returned0

	type(char_vector_t) :: str_

	type(value_t) :: arg, arg1, arg2, tmp

	!print *, 'eval fn_call_expr'
	!print *, 'fn identifier = ', node%identifier%text
	!print *, 'fn id_index   = ', node%id_index

	! i think this is technically not different than using an explicit array.
	! we're just using fortran's call stack and recursive calls to
	! eval_fn_call() to mock a whole array with just `returned0` and `returned`.
	returned0 = state%returned  ! push
	state%returned = .false.

	res%type = node%val%type

	!print *, 'res type = ', res%type

	! User-defined function

	if (.not. allocated(node%params)) then
		write(*,*) err_int_prefix//'unexpected fn'//color_reset
		call internal_error()
	end if

	!print *, 'fn name = ', node%identifier%text
	!print *, 'fn idx  = ', node%id_index
	!print *, 'node type = ', node%val%type
	!print *, 'size params = ', size(node%params)
	!print *, 'param ids = ', node%params

	! TODO: Shared param scope is ok at first, but eventually target
	! recursive fns with scoped stack frames

	! Pass by value (for now, at least).  Arguments are evaluated and
	! their values are copied to the fn parameters

	do i = 1, size(node%params)
		!print *, 'copying param ', i

		!call syntax_eval(node%args(i), state, &
		!	state%vars%vals( node%params(i) ))

		! deeply-nested fn calls can crash without the tmp value.  idk why i
		! can't just eval directly into the state var like commented above
		! :(.  probably state var type is getting cleared by passing it to
		! an intent(out) arg? more likely, nested fn calls basically create
		! a stack in which we store each nested arg in different copies of
		! tmp.  if you try to store them all in the same state var at
		! multiple stack levels it breaks?
		!
		! this also seems to have led to a dramatic perf improvement for
		! intel compilers in commit 324ad414, running full tests in ~25
		! minutes instead of 50.  gfortran perf remains good and unchanged
		!
		call syntax_eval(node%args(i), state, tmp)
		state%vars%vals( node%params(i) ) = tmp

		!print *, "param type = ", kind_name(state%vars%vals( node%params(i) )%type)
		!print *, "param rank = ", state%vars%vals( node%params(i) )%array%rank
		!print *, "param size = ", state%vars%vals( node%params(i) )%array%size

		!print *, 'done'
		!print *, ''
	end do

	call syntax_eval(node%body, state, res)
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

	state%returned = returned0  ! pop

end subroutine eval_fn_call

!===============================================================================

recursive subroutine eval_fn_call_intr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	character(len = :), allocatable :: color

	integer :: i, io

	type(char_vector_t) :: str_

	type(value_t) :: arg, arg1, arg2, tmp

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
	case ("0min_i32")

		call syntax_eval(node%args(1), state, arg)
		res%sca%i32 = arg%sca%i32

		! Note that min/max/println etc. are variadic, so we loop to
		! size(node%args) instead of size(node%params)

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

		do i = 1, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			write(output_unit, '(a)', advance = 'no') arg%to_str()
		end do
		write(output_unit, *)

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

	case ("parse_i32")

		call syntax_eval(node%args(1), state, arg)
		read(arg%sca%str%s, *, iostat = io) res%sca%i32
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i32() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_i64")

		call syntax_eval(node%args(1), state, arg)
		read(arg%sca%str%s, *, iostat = io) res%sca%i64
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_i64() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_f32")

		! TODO: trim "f" literal suffix if present

		call syntax_eval(node%args(1), state, arg)
		read(arg%sca%str%s, *, iostat = io) res%sca%f32
		if (io /= 0) then
			write(*,*) err_rt_prefix//" cannot parse_f32() for argument `"// &
				arg%sca%str%s//"`"//color_reset
			call internal_error()
		end if

	case ("parse_f64")

		call syntax_eval(node%args(1), state, arg)
		read(arg%sca%str%s, *, iostat = io) res%sca%f64
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

		call syntax_eval(node%args(1), state, arg)

		! TODO: catch iostat, e.g. same file opened twice, folder doesn't
		! exist, etc.
		open(newunit = res%sca%file_%unit_, file = arg%sca%str%s)

		!print *, 'opened unit ', res%sca%file_%unit_
		res%sca%file_%name_ = arg%sca%str%s
		res%sca%file_%eof = .false.

	case ("readln")

		call syntax_eval(node%args(1), state, arg1)

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

		! TODO:  set eof flag or crash for other non-zero io
		if (io == iostat_end) then
		!if (io /= 0) then
			!arg1%sca%file_%eof = .true.
			state%vars%vals(node%args(1)%id_index)%sca%file_%eof = .true.
		end if
		!print *, 'eof   = ', arg1%sca%file_%eof

	case ("writeln")

		call syntax_eval(node%args(1), state, arg1)

		!print *, 'writing to unit ', arg1%sca%file_%unit_
		do i = 2, size(node%args)
			call syntax_eval(node%args(i), state, arg)
			write(arg1%sca%file_%unit_, '(a)', advance = 'no') arg%to_str()
		end do
		write(arg1%sca%file_%unit_, *)

	case ("eof")

		call syntax_eval(node%args(1), state, arg1)

		!print *, "checking eof for unit", arg1%sca%file_%unit_
		res%sca%bool = arg1%sca%file_%eof

		!print *, 'eof fn = ', arg1%sca%file_%eof

	case ("close")
		call syntax_eval(node%args(1), state, arg)
		!print *, 'closing unit ', arg%sca%file_%unit_
		close(arg%sca%file_%unit_)

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

	case ("count")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%i64 = count(arg1%array%bool)

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

	case ("all")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%bool = all(arg1%array%bool)

		! Might not be strictly necessary now that %array is allocatable
		! instead of pointable
		!deallocate(arg1%array)

	case ("any")

		call syntax_eval(node%args(1), state, arg1)
		res%sca%bool = any(arg1%array%bool)

	case default
		write(*,*) err_int_prefix//'unexpected fn'//color_reset
		call internal_error()

		!print *, 'fn name = ', node%identifier%text
		!print *, 'fn idx  = ', node%id_index
		!print *, 'node type = ', node%val%type
		!print *, 'size params = ', size(node%params)
		!print *, 'param ids = ', node%params

	end select

end subroutine eval_fn_call_intr

!===============================================================================

recursive subroutine eval_for_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i, rank, for_kind
	integer(kind = 8) :: i8, len8

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, itr, &
		step, len_, tmp

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
				len8 = (ubound_%sca%i32 - lbound_%sca%i32 &
					+ step%sca%i32 - sign(1,step%sca%i32)) / step%sca%i32

			case (i64_type)
				len8 = (ubound_%sca%i64 - lbound_%sca%i64 &
					+ step%sca%i64 - sign(int(1,8),step%sca%i64)) / step%sca%i64

			case (f32_type)
				len8 = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32)

			case (f64_type)
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

		! Any non-primitive array needs to be evaluated before iterating
		! over it.  Parser guarantees that this is an array
		!
		! Unlike step_array, itr%type does not need to be set here because
		! it is set in array_at() (via get_array_val())
		for_kind = array_expr

		call syntax_eval(node%array, state, tmp)
		array = tmp%array

		len8 = array%len_
		!print *, 'len8 = ', len8

	end select

	!print *, 'itr%type = ', kind_name(itr%type)

	! Push scope to make the loop iterator local
	call state%vars%push_scope()
	do i8 = 1, len8

		call array_at(itr, for_kind, i8, lbound_, step, ubound_, &
			len_, array, node%array%elems, state)

		!print *, 'itr = ', itr%to_str()

		! During evaluation, insert variables by array id_index instead of
		! dict lookup.  This is much faster and can be done during
		! evaluation now that we know all of the variable identifiers.
		! Parsing still needs to rely on dictionary lookups because it does
		! not know the entire list of variable identifiers ahead of time
		state%vars%vals(node%id_index) = itr

		call syntax_eval(node%body, state, res)

		if (state%returned) exit

	end do
	call state%vars%pop_scope()

end subroutine eval_for_statement

!===============================================================================

recursive subroutine eval_assignment_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: rank_res, id
	integer(kind = 8) :: i8, index_, len8
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	type(value_t) :: array_val, rhs, tmp

	!print *, "eval assignment_expr"
	!print *, "node identifier = ", node%identifier%text
	!print *, 'lhs type = ', kind_name( state%vars%vals(node%id_index)%type )
	!if (state%vars%vals(node%id_index)%type == struct_type) then
	!if (allocated( node%member )) then
	!	print *, "mem index = ", node%member%id_index
	!end if

	if (allocated( node%member )) then
		!print *, "assign LHS dot member"

		! This is similar to what I do below with get_array_val() and
		! set_array_val(), but I've renamed some of the variables

		! Evaluate the RHS
		call syntax_eval(node%right, state, rhs)

		! Get the initial value from the LHS, which could be nested like `a.b.c.d`
		id = node%member%id_index
		call get_val(node, state%vars%vals(node%id_index), state, res)

		! Do the assignment or += or whatever and set res
		call compound_assign(res, rhs, node%op)

		! Save it back into the LHS var
		call set_val(node, state%vars%vals(node%id_index), state, res)

	else if (.not. allocated(node%lsubscripts)) then

		!! This deallocation will cause a crash when an array appears on both
		!! the LHS and RHS of fn_call assignment, e.g. `dv = diff_(dv, i)` in
		!! AOC 2023/09
		!if (allocated(state%vars%vals)) then
		!if (allocated(state%vars%vals(node%id_index)%array)) then
		!	!print *, "deallocating lhs array"
		!	deallocate(state%vars%vals(node%id_index)%array)
		!end if
		!end if

		! Eval the RHS
		!print *, 'eval and set res'
		call syntax_eval(node%right, state, res)

		! TODO: test int/float casting.  It should be an error during
		! parsing

		!print *, 'compound assign'
		!print *, 'lhs type = ', kind_name( state%vars%vals(node%id_index)%type )

		call compound_assign(state%vars%vals(node%id_index), res, node%op)

		! For compound assignment, ensure that the LHS is returned
		!print *, 'setting res again'
		res = state%vars%vals(node%id_index)
		!print *, 'done'

		!print *, "node identifier = ", node%identifier%text

		! The difference between let and assign is inserting into the
		! current scope (let) vs possibly searching parent scopes (assign).
		! During evaluation we don't need any extra logic for scoping.  The
		! parser has already assigned a separate id_index for each
		! identifier at each scope level

	else
		!print *, 'LHS array subscript assignment'
		!print *, 'LHS type = ', kind_name(state%vars%vals(node%id_index)%array%type)  ! not alloc for str

		! Eval the RHS.  I should probably rename `res` to `rhs` here like I did
		! with get_val() for dot exprs above, because it's not really the result
		! yet in cases of compound assignment
		call syntax_eval(node%right, state, res)

		!print *, 'RHS = ', res%to_str()

		if (state%vars%vals(node%id_index)%type == str_type) then
			!print *, 'str_type'

			! TODO: ban compound character substring assignment
			i8 = subscript_eval(node, state)
			state%vars%vals(node%id_index)%sca%str%s(i8+1: i8+1) = res%sca%str%s

		else if (all(node%lsubscripts%sub_kind == scalar_sub)) then

			!print *, 'non str_type scalar subscript'
			!print *, 'LHS array type = ', &
			!	state%vars%vals(node%id_index)%array%type
			!print *, 'LHS array = ', state%vars%vals(node%id_index)%array%i32

			!print *, "get_array_val a"

			! It is important to only eval the subscript once, in case it is an
			! expression which changes the state!  For example, `array[(index +=
			! 1)];`.  Maybe I should ban expression statements as indices, but
			! src/tests/test-src/fns/test-19.syntran at least will need updated
			i8 = subscript_eval(node, state)
			call get_val(node, state%vars%vals(node%id_index), state, array_val, index_ = i8)
			call compound_assign(array_val, res, node%op)
			call set_val(node, state%vars%vals(node%id_index), state, array_val, index_ = i8)
			res = array_val

		else

			!print *, 'lhs slice assignment'

			call get_subscript_range(node, state, lsubs, usubs, rank_res)
			len8 = product(usubs - lsubs)
			!print *, 'len8 = ', len8

			! TODO: some size/shape checking might be needed here between
			! LHS and RHS

			! Scalar rhs
			if (res%type /= array_type) array_val = res

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

				index_ = subscript_i32_eval(subs, state%vars%vals(node%id_index)%array)
				call get_array_val(state%vars%vals(node%id_index)%array, index_, tmp)
				call compound_assign(tmp, array_val, node%op)
				call set_array_val(state%vars%vals(node%id_index)%array, index_, tmp)

				!! move conditions out of loop for perf?
				!if (res%type == array_type) then
				!	call set_array_val(res%array, i8, tmp)
				!else

				!	! this makes the res return value a scalar.  Maybe
				!	! not correct for fn return values or paren exprs, at
				!	! least it's not consistent with the way that array rhs
				!	! vals work.  Maybe I will make a breaking change on the
				!	! return value here because copying res val can also
				!	! have a large perf overhead.
				!	res = tmp

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
				!	! be the slice `a[1:4]`, but the implementation is more
				!	! simple by setting `b` to the whole array `a`.

				!end if

				call get_next_subscript(lsubs, usubs, subs)
			end do

			! set res (whole array (slice?)) for return val in case of
			! compound assignment.  see note above re walrus operator
			res = state%vars%vals(node%id_index)

		end if
	end if

end subroutine eval_assignment_expr

!===============================================================================

subroutine eval_translation_unit(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	! TODO: do we want to globally push/pop scope for whole
	! translation_unit?  Will this have impacts on interpretting multiple
	! files, or allowing the user to override intrinsic fns?
	!call vars%push_scope()

	! The final statement of a unit returns the actual result.  Non-final
	! members only change the (vars) state or define fns
	do i = 1, size(node%members)

		! Only eval statements, not fn or struct declarations
		!
		! TODO: is this where we should copy fn dict to array?
		if (node%members(i)%kind == fn_declaration    ) cycle
		if (node%members(i)%kind == struct_declaration) cycle

		call syntax_eval(node%members(i), state, res)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' res = ', res%to_str()
		!print *, ''

		! HolyC feature: implicitly print name expression members.  I may
		! remove this after I implement an intrinsic print() fn.  May also
		! need to suppress this for void fn calls later
		if (node%members(i)%kind == name_expr .and. .not. state%quiet) then
			write(*,*) res%to_str()
		end if

		if (state%returned) exit

	end do

	!call state%vars%pop_scope()

end subroutine eval_translation_unit

!===============================================================================

recursive subroutine eval_array_expr(node, state, res)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i, j
	integer(kind = 8) :: i8

	real(kind = 4) :: f, fstep
	real(kind = 8) :: f64, fstep64

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, elem, &
		step, len_

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

			array%cap = (ubound_%sca%i64 - lbound_%sca%i64 &
				+ step%sca%i64 - sign(int(1,8),step%sca%i64)) / step%sca%i64

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
			res%array%str = lbound_%sca%str

		case (struct_type)

			!print *, "lbound_ size = ", size(lbound_%struct)

			do i8 = 1, res%array%len_
				res%struct(i8)%struct = lbound_%struct
			end do

			! Arrays are homogeneous, so every element shares one struct_name
			! for efficiency
			res%struct_name = lbound_%struct_name

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

		! TODO: allow empty arrays?  Sub type of empty array?  Empty arrays
		! can currently be created like [0: -1];

		allocate(res%array)
		res%array%type = node%val%array%type
		call allocate_array(res, size(node%elems, kind = 8))
		res%array%len_ = 0

		do i = 1, size(node%elems)
			call syntax_eval(node%elems(i), state, elem)
			!print *, 'elem['//str(i)//'] = ', elem%str()

			if (res%array%type == struct_type) then
				res%struct(i) = elem
			else
				call res%array%push(elem)
			end if

		end do

		if (res%array%type == struct_type) then
			res%array%len_ = size(node%elems)
		end if

		res%array%rank = 1
		allocate(res%array%size( res%array%rank ))
		res%array%size = res%array%len_

		res%type  = array_type

		res%struct_name = node%val%struct_name

		!print *, "struct_name = ", res%struct_name

	else
		write(*,*) err_int_prefix//'unexpected array kind'//color_reset
		call internal_error()
	end if

end subroutine eval_array_expr

!===============================================================================

recursive subroutine eval_while_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	type(value_t) :: condition

	call syntax_eval(node%condition, state, condition)
	do while (condition%sca%bool)
		call syntax_eval(node%body, state, res)
		call syntax_eval(node%condition, state, condition)
		if (state%returned) exit
	end do

end subroutine eval_while_statement

!===============================================================================

recursive subroutine eval_if_statement(node, state, res)

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

recursive subroutine eval_return_statement(node, state, res)

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

recursive subroutine eval_block_statement(node, state, res)

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(value_t), intent(out) :: res

	!********

	integer :: i

	type(value_t) :: tmp

	call state%vars%push_scope()

	! The final statement of a block returns the actual result.  Non-final
	! members only change the (vars) state.
	do i = 1, size(node%members)
		call syntax_eval(node%members(i), state, tmp)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' tmp = ', tmp%to_str()
		!print *, 'type = ', tmp%type, kind_name(tmp%type)
		!print *, ''

		! In case of no-op if statements and while loops
		if (tmp%type /= unknown_type) then
			res = tmp

			!if (tmp%type == array_type) then
			!	res%array = tmp%array
			!end if
		end if

		! HolyC feature: implicitly print name expression members.  I may
		! remove this after I implement an intrinsic print() fn.  May also
		! need to suppress this for void fn calls later
		if (node%members(i)%kind == name_expr .and. .not. state%quiet) then
			write(*,*) tmp%to_str()
		end if

		if (state%returned) exit

	end do

	call state%vars%pop_scope()

end subroutine eval_block_statement

!===============================================================================

recursive subroutine eval_unary_expr(node, state, res)

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

	case default
		write(*,*) err_eval_unary_op(node%op%text)
		call internal_error()
	end select

end subroutine eval_unary_expr

!===============================================================================

subroutine promote_i32_i64(val)

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

subroutine allocate_array(val, cap)

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

function new_array(type, cap) result(vector)

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

subroutine compound_assign(lhs, rhs, op)
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

	case default
		write(*,*) err_int_prefix//'unexpected assignment operator ', quote(op%text)//color_reset
		call internal_error()
	end select

end subroutine compound_assign

!===============================================================================

subroutine get_subscript_range(node, state, lsubs, usubs, rank_res)

	! Evaluate the lower- and upper-bounds of each range of a subscripted array
	! slice
	!
	! TODO: `rank_res` is a misnomer.  For LHS slicing it's the rank of the LHS
	! *after* being sliced

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	integer(kind = 8), allocatable, intent(out) :: lsubs(:), usubs(:)
	integer, intent(out) :: rank_res

	!********

	integer :: i, rank_

	type(value_t) :: lsubval, usubval

	rank_ = state%vars%vals(node%id_index)%array%rank
	allocate(lsubs(rank_), usubs(rank_))
	rank_res = 0
	do i = 1, rank_

		if (node%lsubscripts(i)%sub_kind == all_sub) then
			lsubs(i) = 0
			!print *, 'lsubs(i) = ', lsubs(i)
		else
			call syntax_eval(node%lsubscripts(i), state, lsubval)
			lsubs(i) = lsubval%sca%i32
		end if

		select case (node%lsubscripts(i)%sub_kind)
		case (all_sub)
			usubs(i) = state%vars%vals(node%id_index)%array%size(i)
			!print *, 'usubs(i) = ', usubs(i)

			rank_res = rank_res + 1

		case (range_sub)
			call syntax_eval(node%usubscripts(i), state, usubval)
			!usubs(i) = usubval%sca%i32
			usubs(i) = usubval%to_i64()

			rank_res = rank_res + 1

		case (scalar_sub)
			! Scalar subs are converted to a range-1 sub so we can
			! iterate later without further case logic
			usubs(i) = lsubs(i) + 1

		case default
			write(*,*) err_int_prefix//'cannot evaluate subscript kind'//color_reset
			call internal_error()

		end select

	end do
	!print *, 'lsubs = ', lsubs
	!print *, 'usubs = ', usubs
	!print *, 'rank_res = ', rank_res

end subroutine get_subscript_range

!===============================================================================

subroutine get_next_subscript(lsubs, usubs, subs)

	! This is like a bignum += 1 algorithm but in an arbitrary mixed radix

	integer(kind = 8), intent(in) :: lsubs(:), usubs(:)
	integer(kind = 8), intent(inout) :: subs(:)

	!********

	integer :: j

	j = 1
	do while (j < size(subs) .and. subs(j) == usubs(j) - 1)
		subs(j) = lsubs(j)
		j = j + 1
	end do
	subs(j) = subs(j) + 1

end subroutine get_next_subscript

!===============================================================================

function subscript_i32_eval(subs, array) result(index_)

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
		prod  = prod * array%size(j)
	end do
	!print *, 'index_ = ', index_

end function subscript_i32_eval

!===============================================================================

function sub_eval(node, var, state) result(index_)

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

	!if (state%vars%vals(node%id_index)%type == str_type) then
	if (var%type == str_type) then
		call syntax_eval(node%lsubscripts(1), state, subscript)
		index_ = subscript%to_i64()
		return
	end if

	!if (state%vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

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

recursive function subscript_eval(node, state) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_

	type(syntax_node_t) :: node
	type(state_t), intent(inout) :: state

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	! str scalar with single char subscript
	if (state%vars%vals(node%id_index)%type == str_type) then
		call syntax_eval(node%lsubscripts(1), state, subscript)
		index_ = subscript%to_i64()
		return
	end if

	!if (state%vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

	! This could be refactored to run syntax_eval() on each subscript first, and
	! then call subscript_i32_eval() after the loop.  There would be a small
	! memory overhead to save i32 sub array but probably no significant time or
	! space perf difference
	prod  = 1
	index_ = 0
	do i = 1, state%vars%vals(node%id_index)%array%rank
		!print *, 'i = ', i

		call syntax_eval(node%lsubscripts(i), state, subscript)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%to_i64()
		prod  = prod * state%vars%vals(node%id_index)%array%size(i)

	end do

end function subscript_eval

!===============================================================================

subroutine array_at(val, kind_, i, lbound_, step, ubound_, len_, array, &
		elems, state)

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

	case default
		write(*,*) err_int_prefix//'for loop not implemented for this array kind'//color_reset
		call internal_error()
	end select

end subroutine array_at

!===============================================================================

subroutine get_array_val(array, i, val)

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

subroutine set_array_val(array, i, val)

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

end module syntran__eval_m

!===============================================================================

