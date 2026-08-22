
!===============================================================================

submodule (syntran__runtime_m) syntran__runtime_array

	! Struct/array element get/set, subscript-bound evaluation, and array
	! iteration primitives.  Formerly part of the AST walker
	! (eval_array.f90); eval_struct_instance was dropped here since
	! OP_MAKE_STRUCT builds struct values directly from a const-pooled
	! prototype instead (compile_ctrl.f90) -- runtime_m's module docstring
	! has the full picture

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine set_val(node, var, state, val, index_, slots, pos)

	! Assign var.mem = val, or recurse if mem is also a dot expr
	!
	! `slots`/`pos`, when present, mirror get_val's -- see its docstring

	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(inout) :: var
	type(state_t), intent(inout) :: state

	type(value_t), intent(in) :: val

	integer(kind = 8), optional, intent(in) :: index_
	type(value_t), intent(in), optional :: slots(:)
	integer, intent(inout), optional :: pos

	!********

	integer :: id
	integer(kind = 8) :: i8, j8

	if (var%type == file_type) then
		! Unreachable: file handle members are rejected as assignment
		! targets at parse time (EC_READONLY_FILE_MEMBER).  Without this
		! guard, the struct base case below would index an unallocated
		! var%struct(:)
		write(*,*) err_int(IC_FILE_MEMBER, "assignment to a file handle member")
		call internal_error()
	end if

	if (allocated(node%lsubscripts) .and. allocated(node%member)) then

		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state, slots(pos+1 : pos+subscript_total_nslots(node)))
			pos = pos + subscript_total_nslots(node)
			if (state%rt_halt) return
		end if
		id = node%member%id_index

		! Recursion could still be required.  Unfortunately, if an
		! identifier has a subscript *and* a dot, then so does its node.  I
		! think this might require a bunch of if() logic like this instead
		! of any possibility of clean recursion

		if (node%member%kind == dot_expr) then
			! Recurse
			call set_val(node%member, var%struct(i8+1)%struct(id), state, val, slots=slots, pos=pos)
			return
		end if

		if (.not. allocated(node%member%lsubscripts)) then
			var%struct(i8+1)%struct(id) = val
			return
		end if
		!print *, "array dot chain"

		! A str member stores characters in %str, not %array, so neither
		! set_field_slice_val (reads field_val%array%size) nor set_array_val
		! below applies -- str_char_assign handles scalar/range/step/all
		! subscript kinds uniformly (also covers the [str;:] + trailing
		! char-sub case).
		if (member_str_sub(node%member, var%struct(i8+1)%struct(id))) then
			call set_str_member_val(node%member, var%struct(i8+1)%struct(id), state, val, &
				slots(pos+1 : pos+subscript_total_nslots(node%member)))
			pos = pos + subscript_total_nslots(node%member)
			return
		end if

		! Arrays chained by a dot: `a[0].b[0]`
		if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
			call set_field_slice_val(node%member, var%struct(i8+1)%struct(id), state, val, &
				slots(pos+1 : pos+subscript_total_nslots(node%member)))
			pos = pos + subscript_total_nslots(node%member)
			return
		end if
		j8 = sub_eval(node%member, var%struct(i8+1)%struct(id), state, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		if (state%rt_halt) return
		call set_array_val(var%struct(i8+1)%struct(id)%array, j8, val)
		return

	else if (allocated(node%lsubscripts)) then

		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state, slots(pos+1 : pos+subscript_total_nslots(node)))
			pos = pos + subscript_total_nslots(node)
			if (state%rt_halt) return
		end if
		if (.not. any(var%array%type == [struct_type, enum_type])) then
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
		call set_val(node%member, var%struct(id), state, val, slots=slots, pos=pos)
		return
	end if

	! Base case

	if (.not. allocated(node%member%lsubscripts)) then
		var%struct(id) = val
		return
	end if
	!print *, "lsubscripts allocated"

	! A str member stores characters in %str, not %array -- str_char_assign
	! handles scalar/range/step/all subscript kinds uniformly (also covers
	! the [str;:] + trailing char-sub case).  See member_str_sub's docstring.
	if (member_str_sub(node%member, var%struct(id))) then
		call set_str_member_val(node%member, var%struct(id), state, val, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		return
	end if

	if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
		call set_field_slice_val(node%member, var%struct(id), state, val, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		return
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		if (state%rt_halt) return
	end if

	if (.not. any(var%struct(id)%array%type == [struct_type, enum_type])) then
		call set_array_val(var%struct(id)%array, i8, val)
		return
	end if

	var%struct(id)%struct(i8+1) = val

end subroutine set_val

!===============================================================================

recursive module subroutine get_val(node, var, state, res, index_, slots, pos)

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
	!
	! `slots`/`pos`, when present, hold the whole chain's pre-evaluated
	! subscript bound values (compile_member_chain_slots, compile_ctrl.f90)
	! and a running consumption cursor; every consumption point below reads
	! its own subscript_total_nslots(...)-sized window and advances `pos` by
	! that amount instead of AST-walking via sub_eval/get_field_slice_val/
	! apply_subscripts_to_val -- see chain_total_nslots' docstring
	! (bytecode.f90) for why this exactly mirrors this routine's recursion

	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(in) :: var
	type(state_t), intent(inout) :: state

	integer(kind = 8), optional, intent(in) :: index_

	type(value_t), intent(out) :: res

	type(value_t), intent(in), optional :: slots(:)
	integer, intent(inout), optional :: pos

	!********

	integer :: id
	integer(kind = 8) :: i8, j8

	!print *, "get_val()"

	if (var%type == file_type) then
		! File handles have a fixed set of read-only members, reached only
		! via a member chain (OP_LOAD_MEMBER et al: a file variable can
		! never be reached through the index_ path, since files aren't
		! subscriptable), so slots is always present here.  A file value
		! has no %struct(:), so this must intercept before every other
		! branch below
		block
			type(value_t) :: member_val
			call get_file_member(node%member, var, state, member_val)
			if (allocated(node%member%lsubscripts)) then
				call apply_subscripts_to_val(node%member, member_val, state, res, &
					slots(pos+1 : pos+subscript_total_nslots(node%member)))
				pos = pos + subscript_total_nslots(node%member)
			else
				res = member_val
			end if
		end block
		return
	end if

	if (allocated(node%lsubscripts) .and. allocated(node%member)) then

		if (present(index_)) then
			i8 = index_
		else

			if (.not. all(node%lsubscripts%sub_kind == scalar_sub)) then
				!print *, "slice sub"
				call rt_throw(state, err_rt(RC_STRUCT_ARRAY_SLICE, "struct array slices are not implemented"))
				return
			end if

			i8 = sub_eval(node, var, state, slots(pos+1 : pos+subscript_total_nslots(node)))
			pos = pos + subscript_total_nslots(node)
			if (state%rt_halt) return
		end if

		!print *, "i8 = ", i8
		id = node%member%id_index

		! Recursion could still be required.  Unfortunately, if an
		! identifier has a subscript *and* a dot, then so does its node.  I
		! think this might require a bunch of if() logic like this instead
		! of any possibility of clean recursion

		if (node%member%kind == dot_expr) then
			! Recurse
			call get_val(node%member, var%struct(i8+1)%struct(id), state, res, slots=slots, pos=pos)
			return
		end if

		if (.not. allocated(node%member%lsubscripts)) then
			res = var%struct(i8+1)%struct(id)
			return
		end if
		!print *, "array dot chain"

		! A str member stores characters in %str, not %array, so neither
		! get_field_slice_val (reads field_val%array%size) nor get_array_val
		! below applies -- str_char_slice handles scalar/range/step/all
		! subscript kinds uniformly (also covers the [str;:] + trailing
		! char-sub case).
		if (member_str_sub(node%member, var%struct(i8+1)%struct(id))) then
			call get_str_member_val(node%member, var%struct(i8+1)%struct(id), state, res, &
				slots(pos+1 : pos+subscript_total_nslots(node%member)))
			pos = pos + subscript_total_nslots(node%member)
			return
		end if

		if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
			call get_field_slice_val(node%member, var%struct(i8+1)%struct(id), state, res, &
				slots(pos+1 : pos+subscript_total_nslots(node%member)))
			pos = pos + subscript_total_nslots(node%member)
			return
		end if

		! Arrays chained by a dot: `a[0].b[0]`
		j8 = sub_eval(node%member, var%struct(i8+1)%struct(id), state, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		if (state%rt_halt) return
		!print *, "get_array_val 1"
		call get_array_val(var%struct(i8+1)%struct(id)%array, j8, res)
		return

	else if (allocated(node%lsubscripts)) then

		! Prefer sub_eval() over subscript_eval() because it doesn't make any
		! assumptions about var's relation to node
		if (present(index_)) then
			i8 = index_
		else
			i8 = sub_eval(node, var, state, slots(pos+1 : pos+subscript_total_nslots(node)))
			pos = pos + subscript_total_nslots(node)
			if (state%rt_halt) return
		end if

		if (.not. any(var%array%type == [struct_type, enum_type])) then
			!print *, "get_array_val 2"
			call get_array_val(var%array, i8, res)
			return
		end if

		res = var%struct(i8+1)
		if (var%array%type == struct_type) then
			res%type = struct_type
			res%struct_name = var%struct_name
			if (allocated(var%struct_cookie)) res%struct_cookie = var%struct_cookie
			if (var%struct_reg_idx >= 1) res%struct_reg_idx = var%struct_reg_idx
		end if
		! For enum_type, each stored element is already a fully baked enum
		! value_t (type/enum_name/enum_variant/enum_cookie all set), so no
		! extra tagging is needed here
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
		call get_val(node%member, var%struct(id), state, res, slots=slots, pos=pos)
		return
	end if

	! Base case

	if (.not. allocated(node%member%lsubscripts)) then
		!print *, "base"
		res = var%struct(id)
		return
	end if
	!print *, "lsubscripts allocated"

	! A str member stores characters in %str, not %array -- str_char_slice
	! handles scalar/range/step/all subscript kinds uniformly (also covers
	! the [str;:] + trailing char-sub case).  See member_str_sub's docstring.
	if (member_str_sub(node%member, var%struct(id))) then
		call get_str_member_val(node%member, var%struct(id), state, res, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		return
	end if

	if (.not. all(node%member%lsubscripts%sub_kind == scalar_sub)) then
		call get_field_slice_val(node%member, var%struct(id), state, res, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		return
	end if
	!print *, "scalar_sub"

	if (present(index_)) then
		i8 = index_
	else
		i8 = sub_eval(node%member, var%struct(id), state, &
			slots(pos+1 : pos+subscript_total_nslots(node%member)))
		pos = pos + subscript_total_nslots(node%member)
		if (state%rt_halt) return
	end if

	if (.not. any(var%struct(id)%array%type == [struct_type, enum_type])) then
		!print *, "get_array_val 3"
		call get_array_val(var%struct(id)%array, i8, res)
		return
	end if

	res = var%struct(id)%struct(i8+1)

end subroutine get_val

!===============================================================================

subroutine get_file_member(member_node, var, state, res)

	! Read one of a file handle's fixed read-only members (c.f. FILE_MEM_*
	! in consts.f90 and parse_file_member() in parse_expr.f90)

	type(syntax_node_t), intent(in) :: member_node
	type(value_t), intent(in) :: var
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	if (.not. allocated(var%file_)) then
		! Defensive: every file_type value allocates %file_ (open(),
		! std::try_open(), populate_intr_vars())
		write(*,*) err_int(IC_FILE_MEMBER, "file member read on an unallocated file handle")
		call internal_error()
	end if

	select case (member_node%id_index)
	case (FILE_MEM_IS_OPEN)
		res%type = bool_type
		res%sca%bool = var%file_%is_open

	case (FILE_MEM_EOF)
		res%type = bool_type
		if (var%file_%unit_ == input_unit) then
			! stdin's eof lives on state%stdin_eof, kept in sync with the
			! no-arg eof()/readln() forms, not on the handle itself
			res%sca%bool = state%stdin_eof
		else
			res%sca%bool = var%file_%eof
		end if

	case (FILE_MEM_NAME)
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = var%file_%name_

	case default
		write(*,*) err_int(IC_FILE_MEMBER, "unknown file member id")
		call internal_error()
	end select

end subroutine get_file_member

!===============================================================================

subroutine alloc_array_prim(arr, cap, ok)

	! Shared per-type allocate() dispatch for allocate_array()/new_array()
	! below.  Only handles the primitive element types that both callers
	! deal with directly (struct_type/enum_type is allocate_array()-only,
	! via val%struct, since array_t itself has no value_t component).
	!
	! ok is returned .false. for an unhandled type so each caller can keep
	! emitting its own existing internal-error code/message

	type(array_t), intent(inout) :: arr
	integer(kind = 8), intent(in) :: cap
	logical, intent(out) :: ok

	ok = .true.
	select case (arr%type)
	case (i32_type)
		allocate(arr%i32( cap ))

	case (i64_type)
		allocate(arr%i64( cap ))

	case (f32_type)
		allocate(arr%f32( cap ))

	case (f64_type)
		allocate(arr%f64( cap ))

	case (bool_type)
		allocate(arr%bool( cap ))

	case (str_type)
		allocate(arr%str( cap ))

	case default
		ok = .false.
	end select

end subroutine alloc_array_prim

!===============================================================================

module subroutine allocate_array(val, cap)

	type(value_t), intent(inout) :: val
	integer(kind = 8), intent(in) :: cap

	!! always done in caller
	!if (.not. allocated(val%array)) allocate(val%array)

	logical :: ok

	val%array%cap = cap

	if (any(val%array%type == [struct_type, enum_type])) then
		allocate(val%struct( cap ))
		return
	end if

	call alloc_array_prim(val%array, cap, ok)
	if (.not. ok) then
		write(*,*) err_int(IC_ALLOC_ARRAY_TYPE, 'cannot allocate array of type `' &
			//kind_name(val%array%type)//'`')
		call internal_error()
	end if

end subroutine allocate_array

!===============================================================================

module function new_array(type, cap) result(vector)

	! Only caller is the size_array branch of eval_array_expr
	! (runtime_control.f90's new_array(node%val%array%type, size(node%elems))),
	! which then fills it via array%push() -- hence the small default cap and
	! len_ = 0 that allocate_array() (above) doesn't need

	integer, intent(in) :: type
	integer, intent(in), optional :: cap
	type(array_t) :: vector

	logical :: ok

	vector%len_ = 0

	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2  ! I think a small default makes sense here
	end if

	vector%type = type

	call alloc_array_prim(vector, vector%cap, ok)
	if (.not. ok) then
		write(*,*) err_int(IC_ARRAY_TYPE_NOT_IMPL, 'array type not implemented')
		call internal_error()
	end if

end function new_array

!===============================================================================

module subroutine apply_assign_op(lhs, rhs, op)

	! lhs += rhs;
	!   or
	! lhs *= rhs;
	!   etc.
	!
	! Also handles plain assignment (op%kind == equals_token), i.e. `lhs = rhs;`
	! -- there's no separate "assign" entry point, so every '='/'+='/etc.
	! assignment target funnels through here

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in) :: rhs

	type(syntax_token_t), intent(in) :: op

	!******

	type(value_t) :: tmp  ! necessary for arrays
	type(value_t) :: rhs_

	if (op%kind /= equals_token) tmp = lhs

	! For compound assignment to an array (e.g. `v += vmax / n`), cast rhs to
	! match the LHS's existing element type *before* the op, so the result
	! preserves the LHS's type instead of letting the generic binop promote
	! it (e.g. i32 array += i64 scalar must stay i32, matching how scalar
	! compound-assign already truncates back to the LHS's type).  Without
	! this, the LHS array's element type could silently change at runtime,
	! which the bytecode compiler's static type info doesn't expect.
	rhs_ = rhs
	if (op%kind /= equals_token .and. lhs%type == array_type) then
		select case (lhs%array%type)
		case (i32_type)
			if (rhs%type == array_type) then
				rhs_%array = rhs%to_i32_array()
			else
				rhs_%type     = i32_type
				rhs_%sca%i32  = rhs%to_i32()
			end if
		case (i64_type)
			if (rhs%type == array_type) then
				rhs_%array = rhs%to_i64_array()
			else
				rhs_%type     = i64_type
				rhs_%sca%i64  = rhs%to_i64()
			end if
		case (f32_type)
			if (rhs%type == array_type) then
				rhs_%array = rhs%to_f32_array()
			else
				rhs_%type     = f32_type
				rhs_%sca%f32  = rhs%to_f32()
			end if
		case (f64_type)
			if (rhs%type == array_type) then
				rhs_%array = rhs%to_f64_array()
			else
				rhs_%type     = f64_type
				rhs_%sca%f64  = rhs%to_f64()
			end if
		end select
	end if

	select case (op%kind)
	case (equals_token)
		!print *, 'assign'
		!lhs = rhs  ! simply overwrite
		call assign_(lhs, rhs_, op%text)

	case (plus_equals_token)
		call add(tmp, rhs_, lhs, op%text)

	case (minus_equals_token)
		call subtract(tmp, rhs_, lhs, op%text)

	case (star_equals_token)
		call mul(tmp, rhs_, lhs, op%text)

	case (slash_equals_token)
		call div(tmp, rhs_, lhs, op%text)

	case (sstar_equals_token)
		call pow(tmp, rhs_, lhs, op%text)

	case (percent_equals_token)
		call mod_(tmp, rhs_, lhs, op%text)

	case (amp_equals_token)
		call bit_and(tmp, rhs_, lhs, op%text)

	case (pipe_equals_token)
		call bit_or(tmp, rhs_, lhs, op%text)

	case (caret_equals_token)
		call bit_xor(tmp, rhs_, lhs, op%text)

	case (lless_equals_token)
		call left_shift(tmp, rhs_, lhs, op%text)

	case (ggreater_equals_token)
		call right_shift(tmp, rhs_, lhs, op%text)

	case default
		write(*,*) err_int(IC_UNEXPECTED_ASSIGN_OP, 'unexpected assignment operator '//quote(op%text))
		call internal_error()
	end select

end subroutine apply_assign_op

!===============================================================================

module subroutine eval_subscript_1d(node, state, i, lsub, ssub, usub, asub, contributes_rank, slots)

	! Evaluate the lower bound, step, and upper bound for one dimension of a
	! subscripted array slice.  Extracted from get_subscript_range so that the
	! rank-1 fast paths (eval_slice_rank1 / eval_assign_slice_rank1) can obtain
	! scalar bounds without allocating the full lsubs/ssubs/usubs arrays.
	!
	! node%lsubscripts(i)'s bound sub-expressions were already compiled to
	! bytecode (compile_subscript_slots, compile_ctrl.f90) and popped off
	! the operand stack by the VM; read the pre-evaluated values from
	! slots(subscript_slot_start(node,i)+1 : ...).  subscript_dim_nslots /
	! subscript_slot_start (bytecode.f90) are the single source of truth for
	! this layout, shared with the compiler

	type(syntax_node_t), intent(in)    :: node
	type(state_t),       intent(inout) :: state
	integer,             intent(in)    :: i            ! 1-based dimension index
	integer(kind = 8),   intent(out)   :: lsub, ssub, usub
	type(i64_vector_t),  intent(inout) :: asub         ! populated for arr_sub only
	logical,             intent(out)   :: contributes_rank
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: id, k
	integer(kind = 8) :: sz
	type(value_t) :: asubval, lsubval, usubval, ssubval

	id = node%id_index
	contributes_rank = .false.
	lsub = 0
	ssub = 1
	usub = 0

	! Running cursor into slots(), starting at dimension i's own window and
	! advanced below in the same step/lower/upper order the bounds are
	! listed in node%[l|u|s]subscripts(i)
	k = subscript_slot_start(node, i)

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
			k = k + 1; lsubval = slots(k)
			lsub = lsubval%to_i64()
		end if

		if (node%lsubscripts(i)%usub_omit) then
			if (node%is_loc) then
				usub = state%locs%vals(id)%array%size(i)
			else
				usub = state%vars%vals(id)%array%size(i)
			end if
		else
			k = k + 1; usubval = slots(k)
			usub = usubval%to_i64()
		end if

		contributes_rank = .true.

	case (step_sub)
		! Evaluate step FIRST so its sign determines default bounds
		k = k + 1; ssubval = slots(k)
		ssub = ssubval%to_i64()

		if (ssub == 0) then
			call rt_throw(state, err_rt(RC_SUBSCRIPT_STEP_ZERO, 'subscript step is 0'))
			return
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
			k = k + 1; lsubval = slots(k)
			lsub = lsubval%to_i64()
		end if

		if (node%lsubscripts(i)%usub_omit) then
			usub = merge(-1_8, sz, ssub < 0)
		else
			k = k + 1; usubval = slots(k)
			usub = usubval%to_i64()
		end if

		contributes_rank = .true.

	case (scalar_sub)
		! Scalar subs are converted to a range-1 sub so we can
		! iterate later without further case logic
		k = k + 1; lsubval = slots(k)
		lsub = lsubval%to_i64()
		usub = lsub + 1
		ssub = 1
		! contributes_rank stays .false.

	case (arr_sub)
		k = k + 1; asubval = slots(k)

		! value_to_i64_array() (value.f90) isn't used here: it returns an
		! array_t, but asub is an i64_vector_t, so routing through it would
		! add a whole extra array copy for no benefit

		select case (asubval%array%type)
		case (i32_type)
			asub%v = asubval%array%i32
		case (i64_type)
			asub%v = asubval%array%i64
		case default
			write(*,*) err_int(IC_BAD_ARRAY_SUBSCRIPT_TYPE, 'bad array subscript type')
			call internal_error()
		end select

		! lsub is only used in get_next_subscript iteration; when asub%v is empty
		! (e.g. x[ifree] where ifree=[]) the result has 0 elements and lsub is
		! never consumed.  Guard the access so empty index arrays don't crash.
		if (size(asub%v) > 0) lsub = asub%v(1)
		usub = 1
		contributes_rank = .true.

	case default
		write(*,*) err_int(IC_EVAL_SUBSCRIPT_KIND, 'cannot evaluate subscript kind')
		call internal_error()

	end select

end subroutine eval_subscript_1d

!===============================================================================

module subroutine get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_slice, slots)

	! Evaluate the lower- and upper-bounds of each range of a subscripted array
	! slice
	!
	! rank_slice is the rank of the *result* of subscripting node: for a
	! read (RHS) it's the sliced value's own rank; for an LHS slice-assign
	! target it's the rank of the target after being sliced
	!
	! `slots` is forwarded to eval_subscript_1d -- see its docstring

	type(syntax_node_t), intent(in) :: node
	type(state_t), intent(inout) :: state

	type(i64_vector_t), allocatable, intent(out) :: asubs(:)
	integer(kind = 8), allocatable, intent(out) :: lsubs(:), ssubs(:), usubs(:)
	integer, intent(out) :: rank_slice
	type(value_t), intent(in) :: slots(:)

	!********

	integer :: i, id, rank_
	integer(kind = 8) :: sz, len_
	logical :: cr

	id = node%id_index
	if (node%is_loc) then
		rank_ = state%locs%vals(id)%array%rank
	else
		rank_ = state%vars%vals(id)%array%rank
	end if

	allocate(asubs(rank_), lsubs(rank_), ssubs(rank_), usubs(rank_))
	rank_slice = 0
	do i = 1, rank_
		call eval_subscript_1d(node, state, i, lsubs(i), ssubs(i), usubs(i), asubs(i), cr, slots)
		if (state%rt_halt) return
		if (cr) rank_slice = rank_slice + 1

		if (bounds_check) then
			if (node%is_loc) then
				sz = state%locs%vals(id)%array%size(i)
			else
				sz = state%vars%vals(id)%array%size(i)
			end if
			if (node%lsubscripts(i)%sub_kind == arr_sub) then
				call check_arr_sub_bound(state, i, rank_, asubs(i), sz)
			else
				len_ = divceil(usubs(i) - lsubs(i), ssubs(i))
				if (lsubs(i) > usubs(i) .and. ssubs(i) > 0) len_ = 0_8
				if (lsubs(i) < usubs(i) .and. ssubs(i) < 0) len_ = 0_8
				call check_range_bound(state, i, rank_, lsubs(i), ssubs(i), len_, sz)
			end if
			if (state%rt_halt) return
		end if
	end do
	!print *, 'lsubs = ', lsubs
	!print *, 'ssubs = ', ssubs
	!print *, 'usubs = ', usubs
	!print *, 'rank_slice = ', rank_slice

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

subroutine check_bound(state, sub, idim, rank, sz)

	! Bounds-check one dimension's already-evaluated subscript value.  Every
	! call site wraps this in `if (bounds_check) then ... end if` (compiled
	! out entirely -- see compiler.F90 -- when the build wasn't configured
	! with -DSYNTRAN_BOUNDS_CHECK), and must check state%rt_halt immediately
	! after, since a caught out-of-bounds subscript leaves index_/subs
	! computed so far meaningless.

	type(state_t), intent(inout) :: state
	integer(kind = 8), intent(in) :: sub, sz
	integer, intent(in) :: idim, rank

	if (sub < 0 .or. sub >= sz) then
		call rt_throw(state, err_rt_subscript_oob(sub, idim, rank, sz))
	end if

end subroutine check_bound

!===============================================================================

subroutine check_str_bound(state, sub, len_)

	! String-index counterpart of check_bound() above, for a scalar
	! character subscript (str_char_slice/str_char_assign, and sub_eval's/
	! subscript_eval's str_type branches)

	type(state_t), intent(inout) :: state
	integer(kind = 8), intent(in) :: sub, len_

	if (sub < 0 .or. sub >= len_) then
		call rt_throw(state, err_rt_str_index_oob(sub, len_))
	end if

end subroutine check_str_bound

!===============================================================================

subroutine check_range_bound(state, idim, rank, lsub, ssub, len_, sz)

	! Bounds-check a linear-stride (all/range/step/scalar) subscript range:
	! the first and last actually-accessed index must both be within [0, sz).
	! Checking just the two endpoints is enough since every index in between
	! lies strictly between them (constant ssub stride).
	!
	! An empty range (len_ <= 0) is always valid and skips the check
	! entirely, matching existing behavior: e.g. `a[10:10]` on a size-5 array
	! has always returned `[]` rather than erroring, since the copy loop
	! never actually touches index 10 -- it would be a regression to start
	! rejecting that once bounds checking exists.

	type(state_t), intent(inout) :: state
	integer, intent(in) :: idim, rank
	integer(kind = 8), intent(in) :: lsub, ssub, len_, sz

	integer(kind = 8) :: last

	if (len_ <= 0) return

	call check_bound(state, lsub, idim, rank, sz)
	if (state%rt_halt) return

	last = lsub + (len_ - 1) * ssub
	call check_bound(state, last, idim, rank, sz)

end subroutine check_range_bound

!===============================================================================

subroutine check_arr_sub_bound(state, idim, rank, asub, sz)

	! Bounds-check every element of an arr_sub subscript (`a[[2, 4, 9]]`) --
	! unlike a linear-stride range, arbitrary values mean every element must
	! be checked individually, not just the endpoints

	type(state_t), intent(inout) :: state
	integer, intent(in) :: idim, rank
	type(i64_vector_t), intent(in) :: asub
	integer(kind = 8), intent(in) :: sz

	integer(kind = 8) :: k

	if (.not. allocated(asub%v)) return
	do k = 1, size(asub%v)
		call check_bound(state, asub%v(k), idim, rank, sz)
		if (state%rt_halt) return
	end do

end subroutine check_arr_sub_bound

!===============================================================================

module function subscript_i32_eval(subs, array) result(index_)

	! subscript_eval() but with a primitive subs int array
	!
	! Is there a way to copy a slice without doing so much math?
	!
	! No per-call bounds check here: this is called inside get_subscript_range/
	! field_slice_bounds's per-element copy loops (runtime_expr.f90,
	! runtime_control.f90, and this file), which already validate every
	! dimension's lsub/usub/asub once via check_bound before entering the
	! loop -- see get_subscript_range's docstring

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

module function sub_eval(node, var, state, slots) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_
	!
	! Can this be dried up with subscript_eval()?

	type(syntax_node_t) :: node
	type(value_t) :: var
	type(state_t), intent(inout) :: state
	type(value_t), intent(in) :: slots(:)

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting sub_eval()'

	if (var%type == str_type) then
		subscript = slots(1)
		index_ = subscript%to_i64()
		if (bounds_check) then
			call check_str_bound(state, index_, len(var%str%s, 8))
			if (state%rt_halt) return
		end if
		return
	end if

	prod  = 1
	index_ = 0
	do i = 1, var%array%rank
		!print *, 'i = ', i

		subscript = slots(i)

		if (bounds_check) then
			call check_bound(state, subscript%to_i64(), i, var%array%rank, var%array%size(i))
			if (state%rt_halt) return
		end if

		index_ = index_ + prod * subscript%to_i64()
		prod   = prod * var%array%size(i)

	end do
	!print *, "index_ = ", index_

end function sub_eval

!===============================================================================

recursive module function subscript_eval(node, state, slots) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_
	!
	! Only called where every node%lsubscripts(i) is scalar_sub (callers
	! guarantee this), so subscript_dim_nslots gives exactly 1 slot per
	! dimension: slots(i) is dimension i's value directly

	type(syntax_node_t) :: node
	type(state_t), intent(inout) :: state
	type(value_t), intent(in) :: slots(:)

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
		subscript = slots(1)
		index_ = subscript%to_i64()
		if (bounds_check) then
			if (node%is_loc) then
				call check_str_bound(state, index_, len(state%locs%vals(id)%str%s, 8))
			else
				call check_str_bound(state, index_, len(state%vars%vals(id)%str%s, 8))
			end if
			if (state%rt_halt) return
		end if
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

	prod  = 1
	index_ = 0
	do i = 1, rank_
		!print *, 'i = ', i

		subscript = slots(i)

		if (bounds_check) then
			if (node%is_loc) then
				call check_bound(state, subscript%to_i64(), i, rank_, state%locs%vals(id)%array%size(i))
			else
				call check_bound(state, subscript%to_i64(), i, rank_, state%vars%vals(id)%array%size(i))
			end if
			if (state%rt_halt) return
		end if

		index_ = index_ + prod * subscript%to_i64()

		if (node%is_loc) then
			prod  = prod * state%locs%vals(id)%array%size(i)
		else
			prod  = prod * state%vars%vals(id)%array%size(i)
		end if

	end do

end function subscript_eval

!===============================================================================

module subroutine array_at(val, iter)

	! This lazily gets an array value at index iter%counter without expanding
	! the whole implicit array in memory.  Used for for loops.
	!
	! iter bundles what used to be 9 separate args (kind_, i, lbound_, step,
	! ubound_, len_, array, str_, and the two optional struct/elem_vals) --
	! they're exactly for_iter_t's fields (runtime.f90), one entry per active
	! native for loop in vm_exec.f90's for_iters(:) stack, plus the loop
	! counter the caller advances before each call.
	!
	! Only called from OP_FOR_NEXT (vm_exec.f90); the memory saving over
	! materializing the whole iterated array up front is the point of having
	! this lazy per-kind step at all, keeping OP_FOR_NEXT itself readable.

	type(value_t), intent(inout) :: val
	type(for_iter_t), intent(in) :: iter

	!*********

	integer(kind = 8) :: i

	i = iter%counter

	select case (iter%for_kind)
	case (bound_array)

		if (val%type == i32_type) then
			val%sca%i32 = iter%lbound_%sca%i32 + int(i) - 1
		else !if (val%type == i64_type) then
			val%sca%i64 = iter%lbound_%sca%i64 + i - 1
		end if

	case (step_array)

		select case (val%type)
		case (i32_type)
			val%sca%i32 = iter%lbound_%sca%i32 + int(i - 1) * iter%step%sca%i32

		case (i64_type)
			val%sca%i64 = iter%lbound_%sca%i64 + (i - 1) * iter%step%sca%i64

		case (f32_type)
			val%sca%f32 = iter%lbound_%sca%f32 + real(i - 1) * iter%step%sca%f32

		case (f64_type)
			val%sca%f64 = iter%lbound_%sca%f64 + real(i - 1, 8) * iter%step%sca%f64

		end select

	case (len_array)

		select case (val%type)
		case (f32_type)
			val%sca%f32 = iter%lbound_%sca%f32 + real(i - 1) * &
				(iter%ubound_%sca%f32 - iter%lbound_%sca%f32) / real((iter%len_%to_i64() - 1))

		case (f64_type)
			val%sca%f64 = iter%lbound_%sca%f64 + real(i - 1, 8) * &
				(iter%ubound_%sca%f64 - iter%lbound_%sca%f64) / real((iter%len_%to_i64() - 1), 8)

		end select

	case (expl_array, size_array)
		! Pre-evaluated at OP_FOR_SETUP time (compile_array_expr_slots'
		! elem_vals) -- OP_FOR_SETUP always allocates iter%elem_vals for
		! these two kinds
		val = iter%elem_vals(i)

	case (unif_array)
		val = iter%lbound_

	case (array_expr)
		! Non-primary array expr
		if (allocated(iter%struct)) then
			! Enum/struct elements: array_t has no value_t component, so
			! they were threaded through separately (1-based, unlike
			! get_array_val's 0-based `array`)
			val = iter%struct(i)
		else
			call get_array_val(iter%array, i - 1, val)
		end if

	case (str_type)
		!val%type = str_type
		if (.not. allocated(val%str)) allocate(val%str)
		val%str%s = iter%str_%str%s(i:i)
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

module subroutine eval_slice_rank1(node, state, res, slots)

	! Allocation-free fast path for rank-1 array read slices:
	!   a[i:j], a[:j], a[i:], a[i:j:k], a[:k:], a[:]
	! where a is a 1-D array and the single subscript is NOT arr_sub.
	!
	! Uses scalar lsub/ssub/usub on the stack instead of allocating
	! lsubs/ssubs/usubs/asubs/subs arrays as get_subscript_range does.
	!
	! `slots` is forwarded to eval_subscript_1d -- see its docstring

	type(syntax_node_t), intent(in)  :: node
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(out) :: res
	type(value_t),       intent(in) :: slots(:)

	!********

	integer :: id
	integer(kind = 8) :: lsub, ssub, usub, len_, idx, i8
	type(i64_vector_t) :: asub_unused
	logical :: cr_unused
	type(value_t) :: tmp

	id = node%id_index

	call eval_subscript_1d(node, state, 1, lsub, ssub, usub, asub_unused, cr_unused, slots)
	if (state%rt_halt) return

	len_ = divceil(usub - lsub, ssub)
	if (lsub > usub .and. ssub > 0) len_ = 0_8
	if (lsub < usub .and. ssub < 0) len_ = 0_8

	if (bounds_check) then
		if (node%is_loc) then
			call check_range_bound(state, 1, 1, lsub, ssub, len_, state%locs%vals(id)%array%size(1))
		else
			call check_range_bound(state, 1, 1, lsub, ssub, len_, state%vars%vals(id)%array%size(1))
		end if
		if (state%rt_halt) return
	end if

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

module subroutine eval_assign_slice_rank1(node, state, id, res, slots)

	! Fast path for rank-1 array write slices: a[i:j] = rhs, a[i:j] += rhs, etc.
	! On entry res holds the already-evaluated RHS.
	! On return res holds the modified slice (matching the old tmp_array return).
	!
	! `slots` is forwarded to eval_subscript_1d -- see its docstring

	type(syntax_node_t), intent(in)    :: node
	type(state_t),       intent(inout) :: state
	integer,             intent(in)    :: id    ! node%id_index, already extracted
	type(value_t),       intent(inout) :: res   ! RHS in, modified slice out
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer(kind = 8) :: lsub, ssub, usub, len_, idx, i8
	integer :: arr_type
	type(i64_vector_t) :: asub_unused
	logical :: cr_unused
	type(value_t) :: rhs_elem, elem_val, result_val

	call eval_subscript_1d(node, state, 1, lsub, ssub, usub, asub_unused, cr_unused, slots)
	if (state%rt_halt) return

	len_ = divceil(usub - lsub, ssub)
	if (lsub > usub .and. ssub > 0) len_ = 0_8
	if (lsub < usub .and. ssub < 0) len_ = 0_8

	if (bounds_check) then
		if (node%is_loc) then
			call check_range_bound(state, 1, 1, lsub, ssub, len_, state%locs%vals(id)%array%size(1))
		else
			call check_range_bound(state, 1, 1, lsub, ssub, len_, state%vars%vals(id)%array%size(1))
		end if
		if (state%rt_halt) return
	end if

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
			call apply_assign_op(elem_val, rhs_elem, node%op)
			call set_array_val(state%locs%vals(id)%array, idx, elem_val)
			call set_array_val(result_val%array, i8, elem_val)
			idx = idx + ssub
		end do
	else
		do i8 = 0, len_ - 1
			if (res%type == array_type) call get_array_val(res%array, i8, rhs_elem)
			call get_array_val(state%vars%vals(id)%array, idx, elem_val)
			call apply_assign_op(elem_val, rhs_elem, node%op)
			call set_array_val(state%vars%vals(id)%array, idx, elem_val)
			call set_array_val(result_val%array, i8, elem_val)
			idx = idx + ssub
		end do
	end if

	call value_move(result_val, res)   ! return the modified slice, as the general path does

end subroutine eval_assign_slice_rank1

!===============================================================================

module subroutine field_slice_bounds(member_node, field_val, state, rank_slice, lsubs, ssubs, usubs, asubs, slots)

	! Compute subscript bounds (lsubs, ssubs, usubs) and result rank for a
	! non-scalar slice on a struct field array.  Shared by get_field_slice_val
	! and set_field_slice_val to avoid code duplication.
	! On step_sub with ssub==0, rt_throw is called and state%rt_halt is set;
	! callers must check state%rt_halt on return.
	!
	! member_node%lsubscripts(:)'s bound sub-expressions were already
	! compiled to bytecode and popped by the VM -- same convention as
	! eval_subscript_1d's `slots` argument

	type(syntax_node_t),            intent(in)    :: member_node
	type(value_t),                  intent(in)    :: field_val
	type(state_t),                  intent(inout) :: state
	integer,                        intent(out)   :: rank_slice
	integer(kind = 8), allocatable, intent(out)   :: lsubs(:), ssubs(:), usubs(:)
	type(i64_vector_t), allocatable, intent(out)  :: asubs(:)
	type(value_t),                  intent(in)    :: slots(:)

	!********

	integer :: i, rank_, k
	integer(kind = 8) :: lsub, ssub, usub, sz, len_
	type(value_t) :: lsubval, usubval, ssubval, asubval

	rank_ = field_val%array%rank
	allocate(lsubs(rank_), ssubs(rank_), usubs(rank_), asubs(rank_))
	rank_slice = 0

	do i = 1, rank_
		lsub = 0
		ssub = 1
		usub = 0
		sz = field_val%array%size(i)

		k = subscript_slot_start(member_node, i)

		select case (member_node%lsubscripts(i)%sub_kind)
		case (all_sub)
			lsub = 0
			ssub = 1
			usub = field_val%array%size(i)
			rank_slice = rank_slice + 1

		case (range_sub)
			ssub = 1
			if (member_node%lsubscripts(i)%lsub_omit) then
				lsub = 0
			else
				k = k + 1; lsubval = slots(k)
				lsub = lsubval%to_i64()
			end if
			if (member_node%lsubscripts(i)%usub_omit) then
				usub = field_val%array%size(i)
			else
				k = k + 1; usubval = slots(k)
				usub = usubval%to_i64()
			end if
			rank_slice = rank_slice + 1

		case (step_sub)
			k = k + 1; ssubval = slots(k)
			ssub = ssubval%to_i64()
			if (ssub == 0) then
				call rt_throw(state, err_rt(RC_SUBSCRIPT_STEP_ZERO, 'subscript step is 0'))
				return
			end if
			sz = field_val%array%size(i)
			if (member_node%lsubscripts(i)%lsub_omit) then
				lsub = merge(sz - 1_8, 0_8, ssub < 0)
			else
				k = k + 1; lsubval = slots(k)
				lsub = lsubval%to_i64()
			end if
			if (member_node%lsubscripts(i)%usub_omit) then
				usub = merge(-1_8, sz, ssub < 0)
			else
				k = k + 1; usubval = slots(k)
				usub = usubval%to_i64()
			end if
			rank_slice = rank_slice + 1

		case (scalar_sub)
			k = k + 1; lsubval = slots(k)
			lsub = lsubval%to_i64()
			usub = lsub + 1
			ssub = 1

		case (arr_sub)
			k = k + 1; asubval = slots(k)
			if (asubval%array%type == i32_type) then
				asubs(i)%v = asubval%array%i32
			else if (asubval%array%type == i64_type) then
				asubs(i)%v = asubval%array%i64
			else
				write(*,*) err_int(IC_BAD_ARRAY_SUBSCRIPT_TYPE, 'bad array subscript type')
				call internal_error()
			end if
			! lsub is only used in get_next_subscript iteration; when
			! asubs(i)%v is empty (e.g. x[ifree] where ifree=[]) the result
			! has 0 elements and lsub is never consumed.  Guard the access so
			! empty index arrays don't crash (mirrors eval_subscript_1d).
			if (size(asubs(i)%v) > 0) lsub = asubs(i)%v(1)
			usub = 1
			ssub = 1
			rank_slice = rank_slice + 1

		end select

		if (bounds_check) then
			if (member_node%lsubscripts(i)%sub_kind == arr_sub) then
				call check_arr_sub_bound(state, i, rank_, asubs(i), sz)
			else
				len_ = divceil(usub - lsub, ssub)
				if (lsub > usub .and. ssub > 0) len_ = 0_8
				if (lsub < usub .and. ssub < 0) len_ = 0_8
				call check_range_bound(state, i, rank_, lsub, ssub, len_, sz)
			end if
			if (state%rt_halt) return
		end if

		lsubs(i) = lsub
		ssubs(i) = ssub
		usubs(i) = usub
	end do

end subroutine field_slice_bounds

!===============================================================================

module subroutine str_slice_bounds(node, isub, sz, state, il, iu, step, slots)

	! Compute 0-based (il, iu, step) bounds for a character-string subscript
	! at node%lsubscripts(isub) (paired w/ usubscripts(isub)/ssubscripts(isub)),
	! given the string length sz.  Mirrors field_slice_bounds()'s step_sub
	! handling so strings support the same [lower:step:upper] slice forms as
	! arrays, including reversal (e.g. s[:-1:]).
	!
	! Iteration convention: characters at il, il+step, il+2*step, ... up to
	! (but not including) iu, same as field_slice_bounds().
	!
	! On step == 0, rt_throw() is called and state%rt_halt is set; callers
	! must check state%rt_halt on return.
	!
	! Dimension isub's bound sub-expressions were already compiled to
	! bytecode (compile_subscript_slots) and popped by the VM; read them
	! from slots(subscript_slot_start(node,isub)+1 : ...) -- same convention
	! as eval_subscript_1d's `slots` argument

	type(syntax_node_t), intent(in)    :: node
	integer,              intent(in)    :: isub
	integer(kind = 8),    intent(in)    :: sz
	type(state_t),        intent(inout) :: state
	integer(kind = 8),    intent(out)   :: il, iu, step
	type(value_t),        intent(in)    :: slots(:)

	!********

	integer :: k
	integer(kind = 8) :: n_out
	type(value_t) :: lval, uval, sval

	il   = 0
	step = 1
	iu   = sz

	k = subscript_slot_start(node, isub)

	select case (node%lsubscripts(isub)%sub_kind)
	case (all_sub)
		il   = 0
		step = 1
		iu   = sz

	case (scalar_sub)
		k = k + 1; lval = slots(k)
		il   = lval%to_i64()
		iu   = il + 1
		step = 1

	case (range_sub)
		step = 1
		if (node%lsubscripts(isub)%lsub_omit) then
			il = 0
		else
			k = k + 1; lval = slots(k)
			il = lval%to_i64()
		end if
		if (node%lsubscripts(isub)%usub_omit) then
			iu = sz
		else
			k = k + 1; uval = slots(k)
			iu = uval%to_i64()
		end if

	case (step_sub)
		k = k + 1; sval = slots(k)
		step = sval%to_i64()
		if (step == 0) then
			call rt_throw(state, err_rt(RC_SUBSCRIPT_STEP_ZERO, 'subscript step is 0'))
			return
		end if
		if (node%lsubscripts(isub)%lsub_omit) then
			il = merge(sz - 1_8, 0_8, step < 0)
		else
			k = k + 1; lval = slots(k)
			il = lval%to_i64()
		end if
		if (node%lsubscripts(isub)%usub_omit) then
			iu = merge(-1_8, sz, step < 0)
		else
			k = k + 1; uval = slots(k)
			iu = uval%to_i64()
		end if

	case default
		write(*,*) err_int(IC_STR_CHAR_SUBSCRIPT, 'unexpected str char subscript kind')
		call internal_error()

	end select

	if (bounds_check) then
		! Same endpoint-only check as check_range_bound(), but against a
		! string length (err_rt_str_index_oob's message) instead of an array
		! dimension -- and an empty selection (n_out <= 0) is always valid,
		! same exemption as check_range_bound()'s docstring explains
		if (step > 0) then
			n_out = max(0_8, (iu - il + step - 1) / step)
		else
			n_out = max(0_8, (il - iu - step - 1) / (-step))
		end if
		if (n_out > 0) then
			call check_str_bound(state, il, sz)
			if (state%rt_halt) return
			call check_str_bound(state, il + (n_out - 1) * step, sz)
			if (state%rt_halt) return
		end if
	end if

end subroutine str_slice_bounds

!===============================================================================

module subroutine get_field_slice_val(member_node, field_val, state, res, slots)

	! Evaluate a range/step/all subscript on a struct field array.
	!
	! `slots` is forwarded to field_slice_bounds -- see its docstring

	type(syntax_node_t), intent(in)    :: member_node
	type(value_t),       intent(in)    :: field_val
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(out)   :: res
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: rank_slice, idim_, idim_res
	integer(kind = 8) :: diff, i8, index_
	type(value_t) :: tmp
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:)
	type(i64_vector_t), allocatable :: asubs(:)

	call field_slice_bounds(member_node, field_val, state, rank_slice, lsubs, ssubs, usubs, asubs, slots)
	if (state%rt_halt) return

	allocate(res%array)
	res%type = array_type
	res%array%kind = expl_array
	res%array%type = field_val%array%type
	res%array%rank = rank_slice

	allocate(res%array%size(rank_slice))
	idim_res = 1
	do idim_ = 1, field_val%array%rank
		select case (member_node%lsubscripts(idim_)%sub_kind)
		case (step_sub, range_sub, all_sub)
			diff = usubs(idim_) - lsubs(idim_)
			! Clamp reversed/empty ranges to 0 (mirrors eval_slice_rank1 and
			! the sibling set_field_slice_val), otherwise divceil can return
			! a negative size, leading to a negative-size array allocation.
			res%array%size(idim_res) = max(0_8, divceil(diff, ssubs(idim_)))
			idim_res = idim_res + 1
		case (arr_sub)
			res%array%size(idim_res) = size(asubs(idim_)%v)
			idim_res = idim_res + 1
		end select
	end do
	res%array%len_ = product(res%array%size)

	call allocate_array(res, res%array%len_)

	subs = lsubs
	do i8 = 0, res%array%len_ - 1
		index_ = subscript_i32_eval(subs, field_val%array)
		call get_array_val(field_val%array, index_, tmp)
		call set_array_val(res%array, i8, tmp)
		call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
	end do

end subroutine get_field_slice_val

!===============================================================================

module subroutine set_field_slice_val(member_node, field_val, state, val, slots)

	! Write val's elements into field_val at the positions described by
	! member_node%lsubscripts.
	!
	! `slots` is forwarded to field_slice_bounds -- see its docstring

	type(syntax_node_t), intent(in)    :: member_node
	type(value_t),       intent(inout) :: field_val
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(in)    :: val
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: rank_slice, idim_
	integer(kind = 8) :: i8, index_, lhs_len
	type(value_t) :: tmp
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:)
	type(i64_vector_t), allocatable :: asubs(:)

	call field_slice_bounds(member_node, field_val, state, rank_slice, lsubs, ssubs, usubs, asubs, slots)
	if (state%rt_halt) return

	lhs_len = 1
	do idim_ = 1, field_val%array%rank
		select case (member_node%lsubscripts(idim_)%sub_kind)
		case (step_sub, range_sub, all_sub)
			lhs_len = lhs_len * max(0_8, divceil(usubs(idim_) - lsubs(idim_), ssubs(idim_)))
		case (arr_sub)
			lhs_len = lhs_len * size(asubs(idim_)%v, kind = 8)
		end select
	end do
	if (val%array%len_ /= lhs_len) then
		call rt_throw(state, err_rt(RC_ARRAY_SIZE_MISMATCH, &
			"size of RHS does not match size of LHS slice"))
		return
	end if

	subs = lsubs
	do i8 = 0, lhs_len - 1
		index_ = subscript_i32_eval(subs, field_val%array)
		call get_array_val(val%array, i8, tmp)
		call set_array_val(field_val%array, index_, tmp)
		call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
	end do

end subroutine set_field_slice_val

!===============================================================================

function member_str_sub(member_node, field_val) result(is_str_sub)

	! True when member_node's subscripts index characters of field_val rather
	! than array elements: either field_val is a scalar str, or it's a
	! [str; :] array with a trailing char-rank subscript (has_char_sub, c.f.
	! parse_subscripts in parse_array.f90 and eval_name_expr in
	! runtime_expr.f90).  get_val/set_val route to get_str_member_val/
	! set_str_member_val instead of get_field_slice_val/set_field_slice_val
	! when this is true, since a str-typed value has no %array to slice.

	type(syntax_node_t), intent(in) :: member_node
	type(value_t),       intent(in) :: field_val
	logical :: is_str_sub

	is_str_sub = .false.

	if (field_val%type == str_type) then
		is_str_sub = .true.
		return
	end if

	! Guard %array%type access with a nested if -- Fortran doesn't guarantee
	! short-circuit evaluation of .and. chains (c.f. vm_exec.f90's OP_INDEX
	! handler)
	if (field_val%type == array_type) then
		if (field_val%array%type == str_type) then
			is_str_sub = size(member_node%lsubscripts) == field_val%array%rank + 1
		end if
	end if

end function member_str_sub

!===============================================================================

subroutine get_str_member_val(member_node, field_val, state, res, slots)

	! Read a character subscript off a str-typed struct member, or a
	! [str; :] member with a trailing char-rank subscript.  Counterpart of
	! get_field_slice_val for str-holding fields (member_str_sub() decides
	! which one applies).

	type(syntax_node_t), intent(in)    :: member_node
	type(value_t),       intent(in)    :: field_val
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(out)   :: res
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: nelem
	integer(kind = 8) :: i8
	character(len = :), allocatable :: tmp_s

	if (field_val%type == str_type) then
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = str_char_slice(field_val%str%s, member_node, state, 1, slots)
		return
	end if

	! [str; :] with a trailing char-rank subscript
	nelem = field_val%array%rank

	if (all(member_node%lsubscripts(1:nelem)%sub_kind == scalar_sub)) then
		! Scalar element selection -> scalar string result
		i8 = sub_eval(member_node, field_val, state, slots)
		if (state%rt_halt) return
		res%type = str_type
		if (.not. allocated(res%str)) allocate(res%str)
		res%str%s = str_char_slice( &
			field_val%array%str(i8+1)%s, member_node, state, nelem+1, slots)
		return
	end if

	! Element range/slice -> string array result.  get_field_slice_val loops
	! to field_val%array%rank, so the trailing char sub is naturally ignored
	! by it (same property sub_eval/subscript_eval rely on); apply the char
	! sub to each selected element afterward.
	call get_field_slice_val(member_node, field_val, state, res, slots)
	if (state%rt_halt) return

	do i8 = 1, res%array%len_
		tmp_s = str_char_slice( &
			res%array%str(i8)%s, member_node, state, nelem+1, slots)
		if (state%rt_halt) return
		res%array%str(i8)%s = tmp_s
	end do

end subroutine get_str_member_val

!===============================================================================

subroutine set_str_member_val(member_node, field_val, state, val, slots)

	! Write a character subscript on a str-typed struct member, or a
	! [str; :] member with a trailing char-rank subscript.  Counterpart of
	! set_field_slice_val for str-holding fields (member_str_sub() decides
	! which one applies).

	type(syntax_node_t), intent(in)    :: member_node
	type(value_t),       intent(inout) :: field_val
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(in)    :: val
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: nelem, idim_
	integer(kind = 8) :: i8, index_, len8
	integer(kind = 8), allocatable :: lsubs(:), ssubs(:), usubs(:), subs(:)
	type(i64_vector_t), allocatable :: asubs(:)
	integer :: rank_slice

	if (field_val%type == str_type) then
		call str_char_assign(field_val%str%s, member_node, state, 1, val%str%s, slots)
		return
	end if

	! [str; :] with a trailing char-rank subscript
	nelem = field_val%array%rank

	if (all(member_node%lsubscripts(1:nelem)%sub_kind == scalar_sub)) then
		! Scalar element selection: one element's chars are assigned
		i8 = sub_eval(member_node, field_val, state, slots)
		if (state%rt_halt) return
		call str_char_assign( &
			field_val%array%str(i8+1)%s, member_node, state, nelem+1, val%str%s, slots)
		return
	end if

	! Element range/slice: val is the array-shaped result get_str_member_val
	! would have returned for this same node (element k's chars come from
	! val%array%str(k)), NOT a scalar string -- OP_STORE_MEMBER's generic
	! get_val/do_compound/set_val flow (vm_exec.f90) runs the RHS through
	! do_compound() against get_val's result before reaching here, and
	! assign_value_t (math.f90) broadcasts/conforms a scalar or array RHS to
	! left's array shape, so a scalar RHS like `s.n[:,1] = "X"` already
	! becomes an array of "X"s by this point.  field_slice_bounds loops to
	! field_val%array%rank, so the trailing char sub is naturally ignored by
	! it.
	call field_slice_bounds(member_node, field_val, state, rank_slice, lsubs, ssubs, usubs, asubs, slots)
	if (state%rt_halt) return

	! Total number of selected elements (mirrors set_field_slice_val's
	! lhs_len computation)
	len8 = 1
	do idim_ = 1, nelem
		select case (member_node%lsubscripts(idim_)%sub_kind)
		case (step_sub, range_sub, all_sub)
			len8 = len8 * max(0_8, divceil(usubs(idim_) - lsubs(idim_), ssubs(idim_)))
		case (arr_sub)
			len8 = len8 * size(asubs(idim_)%v, kind = 8)
		end select
	end do

	subs = lsubs
	do i8 = 0, len8 - 1
		index_ = subscript_i32_eval(subs, field_val%array)
		call str_char_assign( &
			field_val%array%str(index_+1)%s, member_node, state, nelem+1, &
			val%array%str(i8+1)%s, slots)
		if (state%rt_halt) return
		call get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
	end do

end subroutine set_str_member_val

!===============================================================================

module subroutine apply_subscripts_to_val(node, val, state, res, slots)

	! Apply lsubscripts from node to a pre-evaluated value_t (e.g. a fn return).
	! Mirrors the allocated(node%lsubscripts) branch of eval_name_expr but reads
	! array data from val directly instead of the variable store.
	!
	! node%lsubscripts(:)'s bound sub-expressions were already compiled to
	! bytecode (compile_subscript_slots) and popped by the VM's
	! OP_SUBSCRIPT_TOS handler.  In the all-scalar branch every dimension
	! consumes exactly 1 slot (subscript_dim_nslots), so slots(i) is
	! dimension i's value directly; the slice branch forwards `slots` to
	! get_field_slice_val/field_slice_bounds, which use the general
	! subscript_slot_start offset lookup.

	type(syntax_node_t), intent(in)    :: node
	type(value_t),       intent(in)    :: val
	type(state_t),       intent(inout) :: state
	type(value_t),       intent(out)   :: res
	type(value_t),       intent(in)    :: slots(:)

	!********

	integer :: i
	integer(kind = 8) :: i8, prod
	type(value_t) :: subscript

	if (val%type == str_type) then
		if (.not. allocated(res%str)) allocate(res%str)
		res%type = str_type
		res%str%s = str_char_slice(val%str%s, node, state, 1, slots)
		return
	end if

	if (val%type /= array_type) call internal_error()

	if (all(node%lsubscripts%sub_kind == scalar_sub)) then
		! Inline sub_eval logic using val directly (avoids intent mismatch copy).
		prod  = 1
		i8    = 0
		do i = 1, val%array%rank
			subscript = slots(i)
			i8   = i8 + prod * subscript%to_i64()
			prod = prod * val%array%size(i)
		end do
		if (val%array%type == struct_type) then
			res = val%struct(i8+1)
			res%type       = struct_type
			res%struct_name = val%struct_name
			if (allocated(val%struct_cookie)) res%struct_cookie = val%struct_cookie
			if (val%struct_reg_idx >= 1) res%struct_reg_idx = val%struct_reg_idx
		else if (val%array%type == enum_type) then
			! Each stored element is already a fully baked enum value_t, so
			! no extra tagging is needed here (c.f. get_val above)
			res = val%struct(i8+1)
		else
			call get_array_val(val%array, i8, res)
		end if
	else
		call get_field_slice_val(node, val, state, res, slots)
	end if

end subroutine apply_subscripts_to_val

!===============================================================================

end submodule syntran__runtime_array

!===============================================================================
