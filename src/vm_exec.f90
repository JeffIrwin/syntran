
!===============================================================================

submodule (syntran__vm_m) syntran__vm_exec

	! Stack-based bytecode VM execution loop.
	!
	! M1: native handlers for scalars, loads/stores, binop, unop.
	! M2: OP_JUMP / OP_JUMP_IF_FALSE for if/while/block/break/continue.
	! M3: OP_CALL / OP_RET / OP_LOAD_REF_* for user-defined function frames.
	! M8: OP_FOR_SETUP / OP_FOR_NEXT (for loops), OP_NEW_ARRAY, OP_STORE_SLICE,
	!     OP_HALT; readln/close handled inline in OP_CALL_INTR.

	implicit none

	!------------------------------------------------------------------------
	! Call frame: one entry per active function invocation.
	! caller_locs holds state%locs%vals saved via move_alloc at CALL time,
	! restored via move_alloc at RET time.
	!------------------------------------------------------------------------

	type :: frame_t
		integer :: return_ip  = 0
		integer :: node_idx   = 0   ! index into prog%nodes for the fn_call_expr node
		integer :: nfor_saved = 0   ! for-iter stack depth on function entry (restore at RET)
		type(value_t), allocatable :: caller_locs(:)
		! Locals pool buffer: retained across RET so the NEXT OP_CALL at this
		! frame depth can reuse the allocation.  Avoids allocate/deallocate on
		! every recursive call.  Reset with value_reset() before reuse.
		type(value_t), allocatable :: locs_buf(:)
		! By-ref args' receiver-chain subscript slots (compile_ctrl.f90's Pass
		! 3, popped off the operand stack at OP_CALL), read back at OP_RET
		! for set_val's writeback instead of AST-walking the receiver again.
		! Indexed via call_recv_slot_start(cn, i)+1 .. (bytecode.f90); sized
		! call_recv_total_nslots(cn), so unallocated/zero-length when no
		! by-ref arg has a subscripted/dot-expr receiver
		type(value_t), allocatable :: recv_slots(:)
	end type frame_t

	!------------------------------------------------------------------------
	! For-loop iterator frame: one entry per active native for loop.
	! for_kind uses array_t's kind constants: bound_array, step_array,
	! len_array, expl_array, size_array, unif_array, array_expr, or
	! str_type for string iteration.
	!------------------------------------------------------------------------

	type :: for_iter_t
		integer :: for_kind = 0           ! array kind or str_type
		integer :: itr_type = 0           ! element type (i32/i64/f32/f64/str)
		integer(kind = 8) :: len8  = 0    ! total iteration count
		integer(kind = 8) :: counter = 0  ! current 1-based counter (0 = before first)
		integer :: node_idx = 0           ! prog%nodes index of the for_statement node
		type(value_t) :: lbound_          ! loop lower bound / uniform value
		type(value_t) :: step             ! loop step
		type(value_t) :: ubound_          ! loop upper bound
		type(value_t) :: len_             ! loop length (len_array kind)
		type(array_t) :: array            ! materialized array (non-primary array exprs)
		type(value_t) :: str_             ! string to iterate over (str_type)
		! Enum/struct elements of `array` (array_t has no value_t component of
		! its own), set only when array%type is enum_type/struct_type.
		! c.f. array_at()'s optional `struct` arg in eval_array.f90
		type(value_t), allocatable :: struct(:)
		! expl_array/size_array elements, pre-evaluated at OP_FOR_SETUP time
		! (compile_array_expr_slots) instead of AST-walked per-iteration by
		! array_at.  1-based, same indexing as prog%nodes(node_idx)%array%elems.
		! Unallocated for every other for_kind
		type(value_t), allocatable :: elem_vals(:)
	end type for_iter_t

!===============================================================================

contains

!===============================================================================

! --- call-frame and for-iterator grow helpers ---------------------------------
!
! Both frames(:) and for_iters(:) start at a small initial capacity and double
! on demand (move-on-grow).  This gives the VM unbounded recursion / loop
! depth, bounded only by OS stack / memory like ordinary Fortran recursion.

subroutine grow_frames(frames)
	! Double the call-frame stack.  Allocatable components (caller_locs,
	! locs_buf) are moved cheaply; scalar fields are plain assigned.
	type(frame_t), allocatable, intent(inout) :: frames(:)
	type(frame_t), allocatable :: tmp(:)
	integer :: i, n
	n = size(frames)
	allocate(tmp(2 * n))
	do i = 1, n
		tmp(i)%return_ip  = frames(i)%return_ip
		tmp(i)%node_idx   = frames(i)%node_idx
		tmp(i)%nfor_saved = frames(i)%nfor_saved
		if (allocated(frames(i)%caller_locs)) &
			call move_alloc(frames(i)%caller_locs, tmp(i)%caller_locs)
		if (allocated(frames(i)%locs_buf)) &
			call move_alloc(frames(i)%locs_buf, tmp(i)%locs_buf)
	end do
	call move_alloc(tmp, frames)
end subroutine grow_frames

!===============================================================================

subroutine grow_fors(for_iters)
	! Double the for-iterator stack.  for_iter_t's members are either plain
	! derived types with allocatables inside (value_t/array_t) or an
	! allocatable array directly (struct(:)); either way plain assignment
	! handles (re)allocation of each element correctly, so assignment is used
	! for each element.  Copies into the doubled buffer directly to avoid the
	! extra round-trip through a same-sized tmp.
	type(for_iter_t), allocatable, intent(inout) :: for_iters(:)
	type(for_iter_t), allocatable :: tmp(:)
	integer :: i, n
	n = size(for_iters)
	allocate(tmp(2 * n))
	do i = 1, n
		tmp(i) = for_iters(i)
	end do
	call move_alloc(tmp, for_iters)
end subroutine grow_fors

!===============================================================================

! --- operand-stack helpers ----------------------------------------------------
!
! vm_push_copy: push a deep copy; source remains live (LOAD_CONST, LOAD_GLOBAL,
!   LOAD_LOCAL — the variable slot / constant pool entry must not be cleared).
! vm_push_move: push by move; source is a temporary that dies here (binop
!   result, intrinsic return value, etc.).  O(1) for array/struct values.
! vm_pop_copy:  pop TOS via move (the slot is dead after decrement; "copy" in
!   the name is historical).  Callers always pop into a local variable.

subroutine vm_push_copy(stack, val)
	! Push a deep copy of val.  Source must remain live after this call.
	type(value_vector_t), intent(inout) :: stack
	type(value_t), intent(in) :: val
	call stack%push(val)		! push_value: deep copy + growth
end subroutine vm_push_copy

!===============================================================================

subroutine vm_push_move(stack, val)
	! Push val by moving it (consuming val).  Val must be a temporary with no
	! other live references.  For array/struct values this avoids an O(n) copy.
	type(value_vector_t), intent(inout) :: stack
	type(value_t), intent(inout) :: val
	call stack%push_move(val)	! push_value_move: move + growth
end subroutine vm_push_move

!===============================================================================

subroutine vm_pop_copy(stack, val)
	! Pop TOS into val via move (slot becomes dead; name kept for stability).
	type(value_vector_t), intent(inout) :: stack
	type(value_t), intent(out) :: val
	call value_move(stack%v(stack%len_), val)
	stack%len_ = stack%len_ - 1
end subroutine vm_pop_copy

!===============================================================================

subroutine vm_pop_discard(stack)
	! Discard the top of stack (OP_POP).  Scalars have no allocatable members
	! so we just decrement len_ without freeing.  Strings/arrays/structs that
	! appear here are left in the dead slot until the vector is released.
	type(value_vector_t), intent(inout) :: stack
	if (stack%len_ > 0) stack%len_ = stack%len_ - 1
end subroutine vm_pop_discard

!===============================================================================

subroutine vm_stack_grow(stack)
	! Grow the operand stack when len_ > cap.  Called from the typed-load fast
	! path which increments len_ before checking capacity.
	type(value_vector_t), intent(inout) :: stack
	type(value_t), allocatable :: tmp(:)
	integer :: i
	stack%cap = 2 * stack%len_
	allocate(tmp(stack%cap))
	do i = 1, stack%len_ - 1
		call value_move(stack%v(i), tmp(i))
	end do

	! Slots from len_ up are DEAD BUT NOT EMPTY.  vm_pop_discard() leaves a
	! popped array/struct/str allocated in its slot rather than freeing it, and
	! the typed-load fast paths retag a recycled slot as a scalar (`%type =
	! i32_type` etc) without freeing what it still owns -- which also hides
	! them from value_reset(), whose scalar fast path assumes a scalar tag
	! means nothing is allocated.
	!
	! move_alloc() below deallocates the old stack%v, and letting that walk a
	! live, deeply-nested value_t tree is exactly the implicit deep
	! deallocation this codebase never relies on (see value_array_destroy() in
	! value.f90).  Free them explicitly first.
	!
	! Every slot, not just those from len_ up: value_move() above transfers
	! only the component matching %type, so a slot that was retagged as a
	! scalar while still owning an array keeps that array even after being
	! moved out.
	!
	! Note: size(stack%v), not stack%cap -- cap was already overwritten with
	! the NEW capacity above.
	do i = 1, size(stack%v)
		call value_destroy(stack%v(i))
	end do

	call move_alloc(tmp, stack%v)
end subroutine vm_stack_grow

!===============================================================================

subroutine do_compound(lhs, rhs, op_kind)

	! Call compound_assign with just an integer op_kind (no full token needed).
	! op%text is only used for error messages, so an empty string is fine.
	! Note: the VM dispatch loop is single-threaded; module-level compiler state
	! in compile_ctrl.f90 is similarly non-reentrant by design.

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in)    :: rhs
	integer, intent(in)          :: op_kind

	!*******

	type(syntax_token_t) :: op_tok

	op_tok%text = ''
	op_tok%kind = op_kind
	call compound_assign(lhs, rhs, op_tok)

end subroutine do_compound

!===============================================================================

subroutine do_binop(left, right, op_kind, restype, res, rt_err)

	! Compute a binary operation on two values.
	! restype: pre-computed result type from the compiler (node%val%type).
	!   When restype /= unknown_type the expensive get_binary_op_kind call is
	!   skipped.  The compiler always supplies this for OP_BINOP (instr%b).
	! The math routines (add, subtract, etc.) are available because
	! syntran__vm_m uses syntran__runtime_m which uses syntran__math_m and
	! syntran__bool_m.

	type(value_t), intent(in) :: left, right
	integer, intent(in) :: op_kind, restype
	type(value_t), intent(out) :: res

	! Set (allocated) on a reachable matmul dimension-mismatch runtime error.
	! See the comment on matmul_value_t() in math_bin_matmul.f90: that module
	! has no access to state_t, so the caller (vm_run's OP_BINOP handler,
	! which does have state) is responsible for translating an allocated
	! rt_err into call rt_throw(state, rt_err)
	character(len = :), allocatable, intent(out) :: rt_err

	!*******

	integer :: larrtype, rarrtype

	if (restype /= unknown_type) then
		res%type = restype
	else
		larrtype = unknown_type
		rarrtype = unknown_type
		if (left %type == array_type) larrtype = left %array%type
		if (right%type == array_type) rarrtype = right%array%type

		res%type = get_binary_op_kind(left%type, op_kind, right%type, &
			larrtype, rarrtype)

		select case (res%type)
		case (bool_array_type, f32_array_type, f64_array_type, &
			i32_array_type, i64_array_type, str_array_type)
			res%type = array_type
		end select
	end if

	select case (op_kind)
	case (plus_token)
		call add(left, right, res, '+')
	case (minus_token)
		call subtract(left, right, res, '-')
	case (star_token)
		call mul(left, right, res, '*')
	case (matmul_token)
		call matmul_(left, right, res, '@', rt_err)
	case (sstar_token)
		call pow(left, right, res, '**')
	case (slash_token)
		call div(left, right, res, '/')
	case (percent_token)
		call mod_(left, right, res, '%')
	case (and_keyword)
		call and_(left, right, res, 'and')
	case (or_keyword)
		call or_(left, right, res, 'or')
	case (eequals_token)
		call is_eq(left, right, res, '==')
	case (bang_equals_token)
		call is_ne(left, right, res, '!=')
	case (less_token)
		call is_lt(left, right, res, '<')
	case (less_equals_token)
		call is_le(left, right, res, '<=')
	case (greater_token)
		call is_gt(left, right, res, '>')
	case (greater_equals_token)
		call is_ge(left, right, res, '>=')
	case (lless_token)
		call left_shift(left, right, res, '<<')
	case (ggreater_token)
		call right_shift(left, right, res, '>>')
	case (caret_token)
		call bit_xor(left, right, res, '^')
	case (pipe_token)
		call bit_or(left, right, res, '|')
	case (amp_token)
		call bit_and(left, right, res, '&')
	case default
		write(*,*) 'VM: unknown binary op ', op_kind
		call internal_error()
	end select

end subroutine do_binop

!===============================================================================

subroutine do_array_binop_typed(left, right, op_kind, elem_type, res)

	! Fast-path for same-type numeric array ⊕ array operations emitted as
	! OP_ARR_BINOP.  Inlines the array kernels from the math/bool routines,
	! avoiding the magic-dispatch and subroutine-call overhead of do_binop.
	!
	! op_kind : token kind (plus_token, minus_token, …, bang_equals_token)
	! elem_type: i32_type | i64_type | f32_type | f64_type  (same for both operands)

	type(value_t), intent(inout) :: left, right
	integer, intent(in) :: op_kind, elem_type
	type(value_t), intent(out) :: res

	res%type = array_type

	select case (op_kind)

	! --- Arithmetic: result element type == operand element type ---
	case (plus_token)
		res%array = mold(left%array, elem_type)
		select case (elem_type)
		case (i32_type); res%array%i32 = left%array%i32 + right%array%i32
		case (i64_type); res%array%i64 = left%array%i64 + right%array%i64
		case (f32_type); res%array%f32 = left%array%f32 + right%array%f32
		case (f64_type); res%array%f64 = left%array%f64 + right%array%f64
		end select
	case (minus_token)
		res%array = mold(left%array, elem_type)
		select case (elem_type)
		case (i32_type); res%array%i32 = left%array%i32 - right%array%i32
		case (i64_type); res%array%i64 = left%array%i64 - right%array%i64
		case (f32_type); res%array%f32 = left%array%f32 - right%array%f32
		case (f64_type); res%array%f64 = left%array%f64 - right%array%f64
		end select
	case (star_token)
		res%array = mold(left%array, elem_type)
		select case (elem_type)
		case (i32_type); res%array%i32 = left%array%i32 * right%array%i32
		case (i64_type); res%array%i64 = left%array%i64 * right%array%i64
		case (f32_type); res%array%f32 = left%array%f32 * right%array%f32
		case (f64_type); res%array%f64 = left%array%f64 * right%array%f64
		end select
	case (slash_token)
		res%array = mold(left%array, elem_type)
		select case (elem_type)
		case (i32_type); res%array%i32 = left%array%i32 / right%array%i32
		case (i64_type); res%array%i64 = left%array%i64 / right%array%i64
		case (f32_type); res%array%f32 = left%array%f32 / right%array%f32
		case (f64_type); res%array%f64 = left%array%f64 / right%array%f64
		end select
	case (percent_token)
		res%array = mold(left%array, elem_type)
		select case (elem_type)
		case (i32_type); res%array%i32 = mod(left%array%i32, right%array%i32)
		case (i64_type); res%array%i64 = mod(left%array%i64, right%array%i64)
		case (f32_type); res%array%f32 = mod(left%array%f32, right%array%f32)
		case (f64_type); res%array%f64 = mod(left%array%f64, right%array%f64)
		end select

	! --- Comparisons: result is a bool array ---
	case (less_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type); res%array%bool = left%array%i32 < right%array%i32
		case (i64_type); res%array%bool = left%array%i64 < right%array%i64
		case (f32_type); res%array%bool = left%array%f32 < right%array%f32
		case (f64_type); res%array%bool = left%array%f64 < right%array%f64
		end select
	case (less_equals_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type); res%array%bool = left%array%i32 <= right%array%i32
		case (i64_type); res%array%bool = left%array%i64 <= right%array%i64
		case (f32_type); res%array%bool = left%array%f32 <= right%array%f32
		case (f64_type); res%array%bool = left%array%f64 <= right%array%f64
		end select
	case (greater_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type); res%array%bool = left%array%i32 > right%array%i32
		case (i64_type); res%array%bool = left%array%i64 > right%array%i64
		case (f32_type); res%array%bool = left%array%f32 > right%array%f32
		case (f64_type); res%array%bool = left%array%f64 > right%array%f64
		end select
	case (greater_equals_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type); res%array%bool = left%array%i32 >= right%array%i32
		case (i64_type); res%array%bool = left%array%i64 >= right%array%i64
		case (f32_type); res%array%bool = left%array%f32 >= right%array%f32
		case (f64_type); res%array%bool = left%array%f64 >= right%array%f64
		end select
	case (eequals_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type);  res%array%bool = left%array%i32  == right%array%i32
		case (i64_type);  res%array%bool = left%array%i64  == right%array%i64
		case (f32_type);  res%array%bool = left%array%f32  == right%array%f32
		case (f64_type);  res%array%bool = left%array%f64  == right%array%f64
		case (bool_type); res%array%bool = left%array%bool .eqv. right%array%bool
		end select
	case (bang_equals_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type);  res%array%bool = left%array%i32  /= right%array%i32
		case (i64_type);  res%array%bool = left%array%i64  /= right%array%i64
		case (f32_type);  res%array%bool = left%array%f32  /= right%array%f32
		case (f64_type);  res%array%bool = left%array%f64  /= right%array%f64
		case (bool_type); res%array%bool = left%array%bool .neqv. right%array%bool
		end select
	case (and_keyword)
		res%array = mold(left%array, bool_type)
		if (elem_type == bool_type) res%array%bool = left%array%bool .and. right%array%bool
	case (or_keyword)
		res%array = mold(left%array, bool_type)
		if (elem_type == bool_type) res%array%bool = left%array%bool .or. right%array%bool

	end select

end subroutine do_array_binop_typed

!===============================================================================

subroutine do_unop(right, op_kind, res)

	! Compute a unary operation.

	type(value_t), intent(in) :: right
	integer, intent(in) :: op_kind
	type(value_t), intent(out) :: res

	res%type = right%type

	select case (op_kind)
	case (plus_token)
		res = right
	case (minus_token)
		call negate(right, res, '-')
	case (not_keyword)
		call not_(right, res, 'not')
	case (bang_token)
		call bit_not(right, res, '~')
	case default
		write(*,*) 'VM: unknown unary op ', op_kind
		call internal_error()
	end select

end subroutine do_unop

!===============================================================================

module subroutine vm_run(prog, state, res)

	type(program_t), intent(in) :: prog
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!*******

	! Initial capacities for the growable stacks.  Both double on demand so
	! there is no hard recursion or loop-nesting limit.
	integer, parameter :: INIT_FRAMES_CAP = 64
	integer, parameter :: INIT_FORS_CAP   = 16

	type(value_vector_t) :: stack
	type(value_t) :: left, right, val

	! Set (allocated) by do_binop() on a reachable matmul dimension-mismatch
	! runtime error.  See the comment on do_binop()
	character(len = :), allocatable :: rt_err
	! params_pool: reusable buffer for fn-call / return parameter passing.
	! Grows on demand; avoids allocate/deallocate on every OP_CALL / OP_RET.
	type(value_t), allocatable :: params_pool(:)
	integer :: params_pool_cap
	integer :: ip, next_ip
	integer :: i, fn_id, nparams, node_idx_call
	integer :: id, type_, n_mem, nelem
	integer(kind = 8) :: i8

	! M6: reusable arg pool for OP_CALL_INTR native dispatch.
	! Mirrors the params_pool/params_pool_cap pattern for OP_CALL; avoids
	! allocate/deallocate on every intrinsic call.
	type(value_t), allocatable :: iargs_pool(:)
	integer :: iargs_pool_cap, nintr

	! Call-frame stack (growable; no hard recursion limit)
	type(frame_t), allocatable :: frames(:)
	integer :: nframes

	! For-loop iterator stack (growable; no hard loop-nesting limit)
	type(for_iter_t), allocatable :: for_iters(:)
	integer :: nfor

	!print *, "starting vm_run()"

	nframes         = 0
	nfor            = 0
	params_pool_cap = 0
	iargs_pool_cap  = 0
	stack           = new_value_vector()
	allocate(frames(INIT_FRAMES_CAP))
	allocate(for_iters(INIT_FORS_CAP))
	allocate(iargs_pool(0))   ! pre-allocate so iargs_pool(1:0) is valid for nullary intrinsics

	ip = prog%entry_main
	do while (ip <= prog%len_)

		! Default: advance to next instruction.  Jump handlers override this.
		next_ip = ip + 1

		associate(instr => prog%code(ip))

		select case (instr%op)

		! --- constants and variable loads ---
		case (OP_LOAD_CONST)
			call vm_push_copy(stack, prog%consts(instr%a))

		case (OP_LOAD_GLOBAL)
			call vm_push_copy(stack, state%vars%vals(instr%a))

		case (OP_LOAD_LOCAL)
			call vm_push_copy(stack, state%locs%vals(instr%a))

		! --- variable stores (keep TOS on stack, copy into slot) ---
		! Use assign_() when the slot already has a type (i.e. post-let assignment)
		! to preserve the lhs type and cast the rhs. Raw copy for let_expr init
		! (slot starts as unknown_type).
		case (OP_STORE_GLOBAL)
			if (state%vars%vals(instr%a)%type == unknown_type) then
				state%vars%vals(instr%a) = stack%v(stack%len_)
			else
				call assign_(state%vars%vals(instr%a), stack%v(stack%len_), '=')
			end if

		case (OP_STORE_LOCAL)
			if (state%locs%vals(instr%a)%type == unknown_type) then
				state%locs%vals(instr%a) = stack%v(stack%len_)
			else
				call assign_(state%locs%vals(instr%a), stack%v(stack%len_), '=')
			end if

		! --- binary operation ---
		case (OP_BINOP)
			call vm_pop_copy(stack, right)
			call vm_pop_copy(stack, left)
			call do_binop(left, right, instr%a, instr%b, val, rt_err)
			if (allocated(rt_err)) then
				call rt_throw(state, rt_err)
				exit
			end if
			call vm_push_move(stack, val)

		! --- unary operation ---
		case (OP_UNOP)
			call vm_pop_copy(stack, right)
			call do_unop(right, instr%a, val)
			call vm_push_move(stack, val)

		! --- discard ---
		case (OP_POP)
			call vm_pop_discard(stack)

		! --- control flow: unconditional jump ---
		case (OP_JUMP)
			next_ip = instr%a

		! --- control flow: conditional jump ---
		! Bool is a scalar (no allocatables), so read sca%bool directly and
		! decrement len_ without going through value_move.
		case (OP_JUMP_IF_FALSE)
			if (.not. stack%v(stack%len_)%sca%bool) next_ip = instr%a
			stack%len_ = stack%len_ - 1

		! --- by-ref arg loading: move value from variable slot onto stack -----
		! The original slot is left in a valid-but-empty state; the value is
		! written back from the callee's frame at OP_RET time.
		case (OP_LOAD_REF_GLOBAL)
			call value_move(state%vars%vals(instr%a), val)
			call vm_push_move(stack, val)

		case (OP_LOAD_REF_LOCAL)
			call value_move(state%locs%vals(instr%a), val)
			call vm_push_move(stack, val)

		! --- user function call -----------------------------------------------
		! Stack layout on entry (bottom to top):
		!   [by-value args][by-ref args][by-ref receiver-chain slots]
		! All three groups in ascending param-index order.  We pop the
		! receiver-chain slots first (they're on top), then by-ref (reverse
		! index order), then by-val.
		case (OP_CALL)
			fn_id        = instr%a
			node_idx_call = instr%b
			associate(cn => prog%nodes(node_idx_call))
			nparams = 0
			if (allocated(cn%params)) nparams = size(cn%params)
			! Grow params_pool if needed (amortised; avoids alloc/dealloc per call).
			if (nparams > params_pool_cap) then
				call value_array_destroy(params_pool)
				allocate(params_pool(nparams))
				params_pool_cap = nparams
			end if

			! Pop by-ref receiver-chain slots (compile_ctrl.f90's Pass 3),
			! stashed in a local buffer until the frame is pushed below --
			! they must outlive the callee's entire execution for OP_RET's
			! writeback, so they can't stay on the shared operand stack.
			block
			integer :: nrecv_, recv_base_
			type(value_t), allocatable :: recv_tmp_(:)
			nrecv_ = call_recv_total_nslots(cn)
			if (nrecv_ > 0) then
				recv_base_ = stack%len_ - nrecv_
				allocate(recv_tmp_(nrecv_))
				recv_tmp_ = stack%v(recv_base_+1 : recv_base_+nrecv_)
				stack%len_ = recv_base_
			end if

			! Pop by-ref args in reverse param-index order.
			do i = nparams, 1, -1
				if (cn%is_ref(i)) call vm_pop_copy(stack, params_pool(i))
			end do

			! Pop by-val args in reverse param-index order.
			do i = nparams, 1, -1
				if (.not. cn%is_ref(i)) call vm_pop_copy(stack, params_pool(i))
			end do

			! Push a new call frame; save caller's local vars and for-iter depth.
			! Grow the frame stack if needed (no hard recursion limit).
			if (nframes + 1 > size(frames)) call grow_frames(frames)
			nframes = nframes + 1
			frames(nframes)%return_ip  = ip + 1
			frames(nframes)%nfor_saved = nfor
			frames(nframes)%node_idx  = node_idx_call
			if (allocated(frames(nframes)%recv_slots)) deallocate(frames(nframes)%recv_slots)
			if (nrecv_ > 0) call move_alloc(recv_tmp_, frames(nframes)%recv_slots)
			if (allocated(state%locs%vals)) then
				call move_alloc(state%locs%vals, frames(nframes)%caller_locs)
			end if

			! Locals-pool: reuse the saved buffer from the previous call at this
			! frame depth if it is large enough; otherwise allocate fresh.
			if (allocated(frames(nframes)%locs_buf)) then
				if (size(frames(nframes)%locs_buf) >= cn%num_locs) then
					! Reuse: reset all used slots to unknown_type.
					do i = 1, cn%num_locs
						call value_reset(frames(nframes)%locs_buf(i))
					end do
					call move_alloc(frames(nframes)%locs_buf, state%locs%vals)
				else
					! Buffer too small (function changed num_locs? shouldn't happen).
					call value_array_destroy(frames(nframes)%locs_buf)
					allocate(state%locs%vals(cn%num_locs))
				end if
			else
				! First call at this depth: allocate fresh (default-init = unknown_type).
				allocate(state%locs%vals(cn%num_locs))
			end if

			! Move params into the callee's local slots.
			do i = 1, nparams
				call value_move(params_pool(i), state%locs%vals(cn%params(i)))
			end do

			end block
			end associate

			next_ip = prog%fn_entry(fn_id)

		! --- indirect call through a fn-pointer value --------------------------
		! Stack layout on entry (bottom to top): [by-value args][callee_fn_value]
		! (callee at TOS).  Unlike OP_CALL, the target fn is not known until this
		! instruction executes: pop the callee value first and read its fn_index
		! to resolve the entry point/num_locs.  v1 fn
		! pointers are by-value only, so cn%params/cn%is_ref are not used here
		! (cn%params is left unallocated on this node kind, which also makes
		! OP_RET's by-ref writeback loop below a no-op for this call kind, since
		! it sizes nparams from cn%params)
		case (OP_CALL_PTR)
			node_idx_call = instr%b
			associate(cn => prog%nodes(node_idx_call))
			nparams = 0
			if (allocated(cn%args)) nparams = size(cn%args)

			! Pop the callee fn-pointer value (pushed last, on top of the args).
			call vm_pop_copy(stack, val)
			fn_id = val%sca%fn_index

			! Grow params_pool if needed (amortised; avoids alloc/dealloc per call).
			if (nparams > params_pool_cap) then
				call value_array_destroy(params_pool)
				allocate(params_pool(nparams))
				params_pool_cap = nparams
			end if

			! All args are by-value in v1: pop in reverse order.
			do i = nparams, 1, -1
				call vm_pop_copy(stack, params_pool(i))
			end do

			! Push a new call frame; save caller's local vars and for-iter depth.
			if (nframes + 1 > size(frames)) call grow_frames(frames)
			nframes = nframes + 1
			frames(nframes)%return_ip  = ip + 1
			frames(nframes)%nfor_saved = nfor
			frames(nframes)%node_idx  = node_idx_call
			if (allocated(state%locs%vals)) then
				call move_alloc(state%locs%vals, frames(nframes)%caller_locs)
			end if

			! Locals-pool: reuse the saved buffer from the previous call at this
			! frame depth if it is large enough; otherwise allocate fresh.  Unlike
			! OP_CALL, num_locs comes from prog%fn_num_locs(fn_id) (looked up by
			! the runtime-resolved target), not from a parse-time-fixed cn%num_locs,
			! since different calls through the same call site can target
			! differently-sized fns.
			if (allocated(frames(nframes)%locs_buf)) then
				if (size(frames(nframes)%locs_buf) >= prog%fn_num_locs(fn_id)) then
					! Reuse: reset all used slots to unknown_type.
					do i = 1, prog%fn_num_locs(fn_id)
						call value_reset(frames(nframes)%locs_buf(i))
					end do
					call move_alloc(frames(nframes)%locs_buf, state%locs%vals)
				else
					! Buffer too small for this target: reallocate.
					call value_array_destroy(frames(nframes)%locs_buf)
					allocate(state%locs%vals(prog%fn_num_locs(fn_id)))
				end if
			else
				! First call at this depth: allocate fresh (default-init = unknown_type).
				allocate(state%locs%vals(prog%fn_num_locs(fn_id)))
			end if

			! Params occupy local slots 1..nparams (guaranteed by
			! parse_fn_declaration: params are always declared before any other
			! local, so they always get slots 1..nparams in order).
			do i = 1, nparams
				call value_move(params_pool(i), state%locs%vals(i))
			end do

			end associate

			next_ip = prog%fn_entry(fn_id)

		! --- return from user function ----------------------------------------
		! TOS is the return value (or an unknown sentinel for void functions).
		! Write back by-ref params, restore caller locs, jump to return_ip.
		case (OP_RET)
			call vm_pop_copy(stack, val)   ! pop return value

			associate(fr => frames(nframes), &
			          cn => prog%nodes(frames(nframes)%node_idx))
			nparams = 0
			if (allocated(cn%params)) nparams = size(cn%params)
			! Grow params_pool if needed (amortised; avoids alloc/dealloc per return).
			if (nparams > params_pool_cap) then
				call value_array_destroy(params_pool)
				allocate(params_pool(nparams))
				params_pool_cap = nparams
			end if

			! Move by-ref params from callee locs into params_pool for writeback.
			do i = 1, nparams
				if (cn%is_ref(i)) then
					call value_move(state%locs%vals(cn%params(i)), params_pool(i))
				end if
			end do

			! Save callee locs to pool for reuse by the next call at this depth.
			if (allocated(state%locs%vals)) then
				call move_alloc(state%locs%vals, frames(nframes)%locs_buf)
			end if

			! Restore caller's local variable array.
			if (allocated(fr%caller_locs)) then
				call move_alloc(fr%caller_locs, state%locs%vals)
			end if
			! (state%locs%vals stays unallocated if the caller had none — correct.)

			! Write by-ref modified values back to caller's variable slots.
			do i = 1, nparams
				if (.not. cn%is_ref(i)) cycle
				! Temporary receiver (fn-return value or fn-return-with-field): no writeback
				if (cn%args(i)%kind == fn_call_expr .or. &
				    cn%args(i)%kind == method_call_expr .or. &
				    cn%args(i)%kind == fn_call_intr_expr .or. &
				    cn%args(i)%root_kind /= 0) cycle
				if (allocated(cn%args(i)%lsubscripts) .or. &
				    cn%args(i)%kind == dot_expr) then
					! Subscripted or dot-expr receiver: write element back via
					! set_val, using this param's own window of the
					! call-time-compiled receiver-chain slots
					! (compile_ctrl.f90's Pass 3) instead of AST-walking the
					! receiver again.  fr%recv_slots is unallocated only when
					! NO by-ref receiver anywhere in this call has any
					! subscript to evaluate (call_recv_total_nslots(cn) == 0),
					! in which case this receiver's own chain_total_nslots is
					! also 0 and set_val never reaches a slots consumption
					! point -- safe to call without slots in that case.
					block
						integer :: recv_pos_, recv_n_
						recv_n_ = chain_total_nslots(cn%args(i))
						recv_pos_ = 0
						if (allocated(fr%recv_slots)) then
							if (cn%args(i)%is_loc) then
								call set_val(cn%args(i), state%locs%vals(cn%args(i)%id_index), &
									state, params_pool(i), &
									slots = fr%recv_slots( &
										call_recv_slot_start(cn, i)+1 : call_recv_slot_start(cn, i)+recv_n_), &
									pos = recv_pos_)
							else
								call set_val(cn%args(i), state%vars%vals(cn%args(i)%id_index), &
									state, params_pool(i), &
									slots = fr%recv_slots( &
										call_recv_slot_start(cn, i)+1 : call_recv_slot_start(cn, i)+recv_n_), &
									pos = recv_pos_)
							end if
						else if (cn%args(i)%is_loc) then
							call set_val(cn%args(i), state%locs%vals(cn%args(i)%id_index), state, params_pool(i))
						else
							call set_val(cn%args(i), state%vars%vals(cn%args(i)%id_index), state, params_pool(i))
						end if
					end block
				else if (cn%args(i)%is_loc) then
					call value_move(params_pool(i), &
						state%locs%vals(cn%args(i)%id_index))
				else
					call value_move(params_pool(i), &
						state%vars%vals(cn%args(i)%id_index))
				end if
			end do

			next_ip = fr%return_ip
			nfor    = fr%nfor_saved   ! clean up any for-iters the callee leaked (e.g. via return)
			end associate
			nframes = nframes - 1

			! Push the return value onto the caller's operand stack.
			call vm_push_move(stack, val)

		! --- scalar subscript read: a[i] or s[i] -----------------------------------
		! Every n%lsubscripts(:) is scalar_sub (compile-time guarantee); their
		! values were compiled to bytecode (compile_subscript_slots) and are on
		! TOS.  Pop them as a slot window and pass to subscript_eval/
		! str_char_slice instead of AST-walking, then read the element with
		! get_val.  For strings: extracts a single character.
		case (OP_INDEX)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, base_
			nslots_ = subscript_total_nslots(n)
			base_ = stack%len_ - nslots_

			id = n%id_index
			if (n%is_loc) then
				type_ = state%locs%vals(id)%type
			else
				type_ = state%vars%vals(id)%type
			end if

			! subscript_eval loops to the variable's element rank_, so a trailing
			! char subscript (index rank+1) is naturally ignored — it gives the
			! flat element index for both scalar-string and string-array cases.
			i8 = subscript_eval(n, state, stack%v(base_+1:base_+nslots_))

			if (type_ == str_type) then
				! Scalar string: use str_char_slice helper (handles scalar/range).
				val%type = str_type
				if (.not. allocated(val%str)) allocate(val%str)
				if (n%is_loc) then
					val%str%s = str_char_slice(state%locs%vals(id)%str%s, n, state, 1, &
						stack%v(base_+1:base_+nslots_))
				else
					val%str%s = str_char_slice(state%vars%vals(id)%str%s, n, state, 1, &
						stack%v(base_+1:base_+nslots_))
				end if
			else if (type_ == array_type) then
				! Check for string array with optional char subscript.
				! Use nested ifs to guard the %array%type access (Fortran does not
				! guarantee short-circuit evaluation of .and. chains).
				nelem = 0
				if (n%is_loc) then
					if (state%locs%vals(id)%array%type == str_type) then
						nelem = state%locs%vals(id)%array%rank
					end if
				else
					if (state%vars%vals(id)%array%type == str_type) then
						nelem = state%vars%vals(id)%array%rank
					end if
				end if

				if (nelem > 0 .and. size(n%lsubscripts) == nelem + 1) then
					! String array with scalar element + char subscript.
					! i8 is the flat element index (char sub was ignored by subscript_eval).
					val%type = str_type
					if (.not. allocated(val%str)) allocate(val%str)
					if (n%is_loc) then
						val%str%s = str_char_slice( &
							state%locs%vals(id)%array%str(i8+1)%s, n, state, nelem+1, &
							stack%v(base_+1:base_+nslots_))
					else
						val%str%s = str_char_slice( &
							state%vars%vals(id)%array%str(i8+1)%s, n, state, nelem+1, &
							stack%v(base_+1:base_+nslots_))
					end if
				else
					if (n%is_loc) then
						call get_val(n, state%locs%vals(id), state, val, index_ = i8)
					else
						call get_val(n, state%vars%vals(id), state, val, index_ = i8)
					end if
				end if
			else
				if (n%is_loc) then
					call get_val(n, state%locs%vals(id), state, val, index_ = i8)
				else
					call get_val(n, state%vars%vals(id), state, val, index_ = i8)
				end if
			end if

			stack%len_ = base_
			call vm_push_move(stack, val)
			end block
			end associate

		! --- slice / non-scalar subscript read: a[i:j], a[:], a[[0,2,4]] ---------
		! Delegates to eval_name_expr which handles all slice kinds, array
		! subscripts, step subscripts, and multi-rank combinations.  Bound
		! sub-expressions were compiled to bytecode (compile_subscript_slots)
		! and are on TOS; pop them as a slot window instead of AST-walking.
		! Can rt_throw() (R20, R22, R27), so check rt_halt before pushing a
		! possibly-unset result.
		case (OP_SLICE)
			block
			integer :: nslots_, base_
			nslots_ = subscript_total_nslots(prog%nodes(instr%a))
			base_ = stack%len_ - nslots_
			call eval_name_expr(prog%nodes(instr%a), state, val, stack%v(base_+1:base_+nslots_))
			stack%len_ = base_
			end block
			if (state%rt_halt) exit
			call vm_push_move(stack, val)

		! --- scalar subscript write: a[i] = x  or  a[i] += x -------------------
		! TOS holds the already-evaluated RHS; below it, n%lsubscripts(:)'s
		! (all scalar_sub) pre-evaluated values (compile_subscript_slots).
		! Uses subscript_eval to find the linear index, reads the current
		! element, applies the compound op, stores back.  For strings: direct
		! character replacement (only plain = is valid).
		case (OP_STORE_IDX)
			call vm_pop_copy(stack, right)   ! RHS (already evaluated by compiler)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, base_
			nslots_ = subscript_total_nslots(n)
			base_ = stack%len_ - nslots_

			id = n%id_index
			if (n%is_loc) then
				type_ = state%locs%vals(id)%type
			else
				type_ = state%vars%vals(id)%type
			end if

			i8 = subscript_eval(n, state, stack%v(base_+1:base_+nslots_))

			if (type_ == str_type) then
				! Scalar string character assignment: s[i] = char_expr
				if (n%is_loc) then
					state%locs%vals(id)%str%s(i8+1: i8+1) = right%str%s
				else
					state%vars%vals(id)%str%s(i8+1: i8+1) = right%str%s
				end if
				stack%len_ = base_
				call vm_push_move(stack, right)
			else if (type_ == array_type) then
				! Check for string array with trailing char subscript.
				! Use nested ifs to guard %array%type (no short-circuit guarantee).
				nelem = 0
				if (n%is_loc) then
					if (state%locs%vals(id)%array%type == str_type) then
						nelem = state%locs%vals(id)%array%rank
					end if
				else
					if (state%vars%vals(id)%array%type == str_type) then
						nelem = state%vars%vals(id)%array%rank
					end if
				end if

				if (nelem > 0 .and. size(n%lsubscripts) == nelem + 1) then
					! String array single-element char assignment: v[i,j] = char_expr
					! i8 is the flat element index; the char sub at lsubscripts(nelem+1)
					! is scalar_sub like every other dimension here, so its
					! pre-evaluated value is slot nelem+1 directly (subscript_slot_start
					! is i-1 uniformly when every dimension is scalar_sub)
					block
						integer(kind=8) :: char_pos
						char_pos = stack%v(base_+nelem+1)%to_i64()
						if (n%is_loc) then
							state%locs%vals(id)%array%str(i8+1)%s( &
								char_pos+1 : char_pos+1) = right%str%s
						else
							state%vars%vals(id)%array%str(i8+1)%s( &
								char_pos+1 : char_pos+1) = right%str%s
						end if
					end block
					stack%len_ = base_
					call vm_push_move(stack, right)
				else
					! Array element assignment (including compound ops)
					if (n%is_loc) then
						call get_val(n, state%locs%vals(id), state, val, index_ = i8)
						call do_compound(val, right, instr%b)
						call set_val(n, state%locs%vals(id), state, val, index_ = i8)
					else
						call get_val(n, state%vars%vals(id), state, val, index_ = i8)
						call do_compound(val, right, instr%b)
						call set_val(n, state%vars%vals(id), state, val, index_ = i8)
					end if
					stack%len_ = base_
					call vm_push_move(stack, val)
				end if
			else
				! Array element assignment (struct or other non-array non-str type)
				if (n%is_loc) then
					call get_val(n, state%locs%vals(id), state, val, index_ = i8)
					call do_compound(val, right, instr%b)
					call set_val(n, state%locs%vals(id), state, val, index_ = i8)
				else
					call get_val(n, state%vars%vals(id), state, val, index_ = i8)
					call do_compound(val, right, instr%b)
					call set_val(n, state%vars%vals(id), state, val, index_ = i8)
				end if
				stack%len_ = base_
				call vm_push_move(stack, val)
			end if
			end block
			end associate

		! --- struct instance construction -----------------------------------------
		! M5: pops nmembers (instr%b) values from stack in reverse order, builds
		! a struct value_t (identity from the const-pooled prototype), and
		! pushes the result.
		case (OP_MAKE_STRUCT)
			associate(cv => prog%consts(instr%a))
			n_mem = instr%b
			val%type = struct_type
			if (allocated(cv%struct_name)) val%struct_name = cv%struct_name

			! Needed so value_to_str() can look up member names by
			! %struct_reg_idx (c.f. struct_reg_set() in value.f90) -- without
			! it, structs built through OP_MAKE_STRUCT print unlabeled.
			! Deallocate/reset on the else branch so a reused `val` can't
			! carry stale identity from a prior struct type
			if (allocated(cv%struct_cookie)) then
				val%struct_cookie = cv%struct_cookie
			else if (allocated(val%struct_cookie)) then
				deallocate(val%struct_cookie)
			end if
			val%struct_reg_idx = cv%struct_reg_idx

			if (allocated(val%struct)) deallocate(val%struct)
			allocate(val%struct(n_mem))
			do i = n_mem, 1, -1
				call vm_pop_copy(stack, val%struct(i))
			end do
			call vm_push_move(stack, val)
			end associate

		! --- dot member read ------------------------------------------------------
		! M5: calls get_val with the stored dot_expr node to handle simple,
		! nested, and subscripted member access chains.  Chain subscript
		! slots (compile_member_chain_slots) are on TOS; pop them as a slot
		! window with a fresh consumption cursor instead of AST-walking.
		case (OP_LOAD_MEMBER)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, base_, pos_
			nslots_ = chain_total_nslots(n)
			base_ = stack%len_ - nslots_
			pos_ = 0
			id = n%id_index
			if (n%is_loc) then
				call get_val(n, state%locs%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
			else
				call get_val(n, state%vars%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
			end if
			stack%len_ = base_
			call vm_push_move(stack, val)
			end block
			end associate

		! --- dot member read from fn/method return value (root on TOS) -----------
		! M5: root struct value already on stack (pushed by compiled fn call),
		! with the wrapper's own chain subscript slots on top of it. Pop both
		! regions, applies member chain from wrapper node via get_val.
		case (OP_LOAD_MEMBER_TOS)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, value_idx_, pos_
			nslots_ = chain_total_nslots(n)
			value_idx_ = stack%len_ - nslots_
			pos_ = 0
			call value_move(stack%v(value_idx_), left)
			call get_val(n, left, state, val, &
				slots = stack%v(value_idx_+1:value_idx_+nslots_), pos = pos_)
			stack%len_ = value_idx_ - 1
			end block
			end associate
			if (state%rt_halt) exit
			call vm_push_move(stack, val)

		! --- dot member write -----------------------------------------------------
		! M5: pops RHS from stack, reads current member via get_val, applies the
		! compound op, writes back via set_val, and pushes the new member value.
		! Below the RHS: chain subscript slots (compile_member_chain_slots),
		! popped as a slot window with a fresh cursor instead of AST-walking --
		! get_val and set_val each get their own cursor since they walk the
		! chain independently (get_val's read happens before do_compound, then
		! set_val re-walks the same chain for the write).
		case (OP_STORE_MEMBER)
			call vm_pop_copy(stack, right)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, base_, pos_
			nslots_ = chain_total_nslots(n)
			base_ = stack%len_ - nslots_
			id = n%id_index
			pos_ = 0
			if (n%is_loc) then
				call get_val(n, state%locs%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
				call do_compound(val, right, instr%b)
				pos_ = 0
				call set_val(n, state%locs%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
			else
				call get_val(n, state%vars%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
				call do_compound(val, right, instr%b)
				pos_ = 0
				call set_val(n, state%vars%vals(id), state, val, &
					slots = stack%v(base_+1:base_+nslots_), pos = pos_)
			end if
			stack%len_ = base_
			if (state%rt_halt) exit
			call vm_push_move(stack, val)
			end block
			end associate

		! --- M6: intrinsic function call -----------------------------------------
		! Native mode: pop args from stack, dispatch by intr_id.
		! readln/close: inline handling with slot writeback via instr%c.
		case (OP_CALL_INTR)
			nintr = instr%b
			! Grow the reusable iargs pool if needed (amortised; avoids
			! allocate/deallocate on every intrinsic call).
			if (nintr > iargs_pool_cap) then
				call value_array_destroy(iargs_pool)
				allocate(iargs_pool(nintr))
				iargs_pool_cap = nintr
			end if
			do i = nintr, 1, -1
				call vm_pop_copy(stack, iargs_pool(i))
			end do

			if (instr%a == INTR_READLN .and. nintr >= 1) then
				! readln(file_handle): read a line from the file; set eof flag on
				! the orig slot.  (No-arg readln() reads stdin and has no slot to
				! write back to; it falls through to the native dispatch below)
				block
				integer :: slot_id_, io_
				logical :: is_loc_
				slot_id_ = int(instr%c / 2)
				is_loc_  = (mod(instr%c, 2_8) == 1_8)
				if (.not. iargs_pool(1)%file_%is_open) then
					call rt_throw(state, err_rt(RC_READLN_NOT_OPEN, 'readln() was called for file "' &
						//iargs_pool(1)%file_%name_//'" which is not open'))
					exit
				end if
				if (.not. iargs_pool(1)%file_%mode_read) then
					call rt_throw(state, err_rt(RC_READLN_NOT_READ_MODE, 'readln() was called for file "' &
						//iargs_pool(1)%file_%name_//'" which was not opened in read mode "r"'))
					exit
				end if
				if (iargs_pool(1)%file_%eof) then
					! Reading again after the eof flag was already set is
					! non-portable across compiler runtimes (some return a
					! generic error iostat, others just return iostat_end
					! again).  Throw deterministically instead of relying on
					! the runtime's iostat
					call rt_throw(state, err_rt(RC_READLN_FAIL, 'cannot readln() from file "' &
						//iargs_pool(1)%file_%name_//'" past end of file'))
					exit
				end if
				val%type = str_type
				if (.not. allocated(val%str)) allocate(val%str)
				val%str%s = read_line(iargs_pool(1)%file_%unit_, io_)
				if (io_ == iostat_end) then
					if (is_loc_) then
						state%locs%vals(slot_id_)%file_%eof = .true.
					else
						state%vars%vals(slot_id_)%file_%eof = .true.
					end if
					! Keep the no-arg readln()/eof() stdin state in sync with
					! the std::IN-argument forms, so mixing the two doesn't
					! desync
					if (iargs_pool(1)%file_%is_std) state%stdin_eof = .true.
				else if (io_ /= 0 .and. io_ /= iostat_eor) then
					call rt_throw(state, err_rt(RC_READLN_FAIL, 'cannot readln() from file "' &
						//iargs_pool(1)%file_%name_//'"'))
					exit
				end if
				end block

			else if (instr%a == INTR_CLOSE) then
				! close: close the file unit; set is_open=false on the orig slot.
				block
				integer :: slot_id_, io_
				logical :: is_loc_
				slot_id_ = int(instr%c / 2)
				is_loc_  = (mod(instr%c, 2_8) == 1_8)
				if (iargs_pool(1)%file_%is_std) then
					call rt_throw(state, err_rt(RC_CLOSE_STANDARD, 'close() cannot be called on ' &
						//'standard file handle "'//iargs_pool(1)%file_%name_//'"'))
					exit
				end if
				if (.not. iargs_pool(1)%file_%is_open) then
					call rt_throw(state, err_rt(RC_CLOSE_NOT_OPEN, 'close() was called for file "' &
						//iargs_pool(1)%file_%name_//'" which is not open'))
					exit
				end if
				if (is_loc_) then
					state%locs%vals(slot_id_)%file_%is_open = .false.
				else
					state%vars%vals(slot_id_)%file_%is_open = .false.
				end if
				close(iargs_pool(1)%file_%unit_, iostat = io_)
				if (io_ /= 0) then
					call rt_throw(state, err_rt(RC_CLOSE_FAIL, 'cannot close() file "' &
						//iargs_pool(1)%file_%name_//'" (iostat = '//str(io_)//')'))
					exit
				end if
				val%type = unknown_type
				end block

			else
				call vm_call_intr(instr%a, nintr, iargs_pool(1:nintr), state, val)
				if (state%rt_halt) exit
			end if

			call vm_push_move(stack, val)

		! --- M8: for-loop setup ---------------------------------------------------
		! Evaluates loop bounds / computes len8; pushes a for_iter_t onto the
		! for-iterator stack.
		case (OP_FOR_SETUP)
			block
			integer :: fi, rk_
			integer(kind = 8), allocatable :: sizes_(:)
			type(value_t) :: tmp_

			! Grow the for-iterator stack if needed (no hard loop-nesting limit).
			if (nfor + 1 > size(for_iters)) call grow_fors(for_iters)
			nfor = nfor + 1
			fi = nfor
			for_iters(fi)%node_idx = instr%a
			for_iters(fi)%counter  = 0
			for_iters(fi)%len8     = 0
			for_iters(fi)%itr_type = unknown_type

			associate(nd => prog%nodes(instr%a))

			! bound_array/step_array/len_array are always intercepted by
			! for_setup_native_ok before OP_FOR_SETUP is ever emitted for
			! them (see compile_ctrl.f90's for_statement case), so the
			! case(bound_array)/case(step_array)/case(len_array) arms below
			! are unreachable; only unif_array/size_array/expl_array
			! sub-expressions are ever compiled to bytecode here
			! (compile_array_expr_slots), hence the nslots_ guard below only
			! pops for those three.
			select case (nd%array%kind)
			case (array_expr)
				for_iters(fi)%for_kind = nd%array%val%array%kind

				block
				integer :: nslots_, base_, k_
				select case (nd%array%val%array%kind)
				case (unif_array, size_array, expl_array)
					nslots_ = array_expr_nslots(nd%array)
				case default
					nslots_ = 0
				end select
				base_ = stack%len_ - nslots_
				k_ = base_

				select case (nd%array%val%array%kind)
				case (bound_array)
					! Promote bounds to i64 if either is i64
					if (any(i64_type == [for_iters(fi)%lbound_%type, &
					                      for_iters(fi)%ubound_%type])) then
						call promote_i32_i64(for_iters(fi)%lbound_)
						call promote_i32_i64(for_iters(fi)%ubound_)
						for_iters(fi)%itr_type = i64_type
					else
						for_iters(fi)%itr_type = i32_type
					end if
					if (.not. any(for_iters(fi)%itr_type == [i32_type, i64_type])) then
						write(*,*) err_int(IC_UNIT_STEP_TYPE, 'unit step array type not implemented')
						call internal_error()
					end if
					for_iters(fi)%len8 = for_iters(fi)%ubound_%to_i64() &
					                   - for_iters(fi)%lbound_%to_i64()

				case (step_array)
					! Promote all to i64 if any is i64
					if (any(i64_type == [for_iters(fi)%lbound_%type, &
					                      for_iters(fi)%step%type, &
					                      for_iters(fi)%ubound_%type])) then
						call promote_i32_i64(for_iters(fi)%lbound_)
						call promote_i32_i64(for_iters(fi)%step)
						call promote_i32_i64(for_iters(fi)%ubound_)
						for_iters(fi)%itr_type = i64_type
					else
						for_iters(fi)%itr_type = for_iters(fi)%lbound_%type
					end if
					select case (for_iters(fi)%itr_type)
					case (i32_type)
						if (for_iters(fi)%step%sca%i32 == 0) then
							call rt_throw(state, err_rt(RC_FOR_STEP_ZERO, 'for loop step is 0'))
							exit
						end if
						for_iters(fi)%len8 = ( &
							for_iters(fi)%ubound_%sca%i32 - for_iters(fi)%lbound_%sca%i32 &
							+ for_iters(fi)%step%sca%i32  &
							- sign(1, for_iters(fi)%step%sca%i32) ) / for_iters(fi)%step%sca%i32
					case (i64_type)
						if (for_iters(fi)%step%sca%i64 == 0) then
							call rt_throw(state, err_rt(RC_FOR_STEP_ZERO, 'for loop step is 0'))
							exit
						end if
						for_iters(fi)%len8 = ( &
							for_iters(fi)%ubound_%sca%i64 - for_iters(fi)%lbound_%sca%i64 &
							+ for_iters(fi)%step%sca%i64  &
							- sign(int(1,8), for_iters(fi)%step%sca%i64) ) / for_iters(fi)%step%sca%i64
					case (f32_type)
						if (for_iters(fi)%step%sca%f32 == 0.0) then
							call rt_throw(state, err_rt(RC_FOR_STEP_ZERO_F, 'for loop step is 0.0'))
							exit
						end if
						for_iters(fi)%len8 = ceiling( &
							(for_iters(fi)%ubound_%sca%f32 - for_iters(fi)%lbound_%sca%f32) &
							/ for_iters(fi)%step%sca%f32)
					case (f64_type)
						if (for_iters(fi)%step%sca%f64 == 0.0d0) then
							call rt_throw(state, err_rt(RC_FOR_STEP_ZERO_F, 'for loop step is 0.0'))
							exit
						end if
						for_iters(fi)%len8 = ceiling( &
							(for_iters(fi)%ubound_%sca%f64 - for_iters(fi)%lbound_%sca%f64) &
							/ for_iters(fi)%step%sca%f64)
					case default
						write(*,*) err_int(IC_STEP_ARRAY_TYPE, 'step array type not implemented')
						call internal_error()
					end select

				case (len_array)
					for_iters(fi)%itr_type = nd%array%val%array%type
					select case (for_iters(fi)%itr_type)
					case (f32_type, f64_type)
						for_iters(fi)%len8 = for_iters(fi)%len_%to_i64()
					case default
						write(*,*) err_int(IC_BOUND_LEN_TYPE, 'bound/len array type not implemented')
						call internal_error()
					end select

				case (expl_array)
					for_iters(fi)%len8 = nd%array%val%array%len_

					! Materialize elements now (compile_array_expr_slots
					! pushed them) instead of AST-walking elems(i) per
					! iteration in array_at.  for_iters(fi) is a reused
					! stack slot, so a stale allocation from a prior
					! for-loop at this depth must be cleared first
					if (allocated(for_iters(fi)%elem_vals)) deallocate(for_iters(fi)%elem_vals)
					allocate(for_iters(fi)%elem_vals(nslots_))
					for_iters(fi)%elem_vals = stack%v(base_+1 : base_+nslots_)

				case (size_array)
					rk_ = size(nd%array%size)
					allocate(sizes_(rk_))

					! Slot window order (compile_array_expr_slots): elems(:)
					! first, then size(:).  Materialize elements the same
					! way expl_array does above.
					if (allocated(for_iters(fi)%elem_vals)) deallocate(for_iters(fi)%elem_vals)
					allocate(for_iters(fi)%elem_vals(size(nd%array%elems)))
					for_iters(fi)%elem_vals = stack%v(base_+1 : base_+size(nd%array%elems))
					k_ = base_ + size(nd%array%elems)

					for_iters(fi)%len8 = 1
					do i = 1, rk_
						! Note: state%rt_halt is checked once after this loop
						! (not inside it), since a bare exit here would only
						! break this inner do, not the outer VM dispatch loop
						k_ = k_ + 1
						sizes_(i) = stack%v(k_)%to_i64()
						for_iters(fi)%len8 = for_iters(fi)%len8 * sizes_(i)
					end do

					! Guards against a runtime-valued size mismatch -- without
					! it, OP_FOR_NEXT's array_at() reads past the end of
					! for_iters(fi)%elem_vals for a
					! mismatched runtime-valued size (crashing with a raw
					! Fortran bounds abort instead of R21, since the parser
					! can only catch this ahead of time when every size is a
					! literal (E102))
					if (.not. state%rt_halt .and. size(nd%array%elems) /= for_iters(fi)%len8) then
						call rt_throw(state, err_rt_expl_array_size( &
							size(nd%array%elems), sizes_))
					end if

				case (unif_array)
					! Slot window order (compile_array_expr_slots): size(:)
					! first, then lbound (fill value, consumed by array_at's
					! unif_array case).
					rk_ = size(nd%array%size)
					for_iters(fi)%len8 = 1
					do i = 1, rk_
						k_ = k_ + 1
						for_iters(fi)%len8 = for_iters(fi)%len8 * stack%v(k_)%to_i64()
					end do
					k_ = k_ + 1
					for_iters(fi)%lbound_ = stack%v(k_)

				case default
					write(*,*) err_int(IC_FOR_ARRAY_KIND, 'for loop: unknown array kind')
					call internal_error()
				end select

				stack%len_ = base_
				end block

			case default
				! Non-primary array expression: node%array%kind /= array_expr,
				! so the compiler already compiled it to bytecode
				! (compile_ctrl.f90's for_statement case) and its value is on
				! TOS -- pop it instead of AST-walking it here
				if (nd%array%val%type == str_type) then
					for_iters(fi)%for_kind = str_type
					for_iters(fi)%itr_type = str_type
					call vm_pop_copy(stack, tmp_)
					call value_move(tmp_, for_iters(fi)%str_)
					for_iters(fi)%len8 = len(for_iters(fi)%str_%str%s, 8)
				else
					for_iters(fi)%for_kind = array_expr
					call vm_pop_copy(stack, tmp_)
					for_iters(fi)%array = tmp_%array

					! Enum/struct elements live in %struct(:), not in
					! array_t (which has no value_t component) -- thread it
					! through separately.  array_at() falls back to its
					! array_t path when this isn't allocated.
					! for_iters(fi) is a reused slot on a stack, not a fresh
					! variable, so a stale allocation from a prior for-loop
					! that used this same slot must be cleared first --
					! otherwise a plain (non-enum) array iterated afterward
					! would inherit stale enum/struct elements
					if (allocated(for_iters(fi)%struct)) deallocate(for_iters(fi)%struct)
					if (allocated(tmp_%struct)) &
						call move_alloc(tmp_%struct, for_iters(fi)%struct)

					for_iters(fi)%len8  = for_iters(fi)%array%len_
				end if
			end select

			! Catches rt_halt set anywhere above: the size_array/unif_array
			! length loops (which can't safely exit the outer dispatch loop
			! from inside their own inner do), and the non-primary array
			! expression case just above
			if (state%rt_halt) exit

			end associate
			end block

		! --- P6: native for-loop setup ---------------------------------------------
		! Bounds are pre-compiled to bytecode; this handler pops them from the
		! operand stack to populate for_iters(fi), avoiding syntax_eval entirely.
		! a = node_idx   (OP_FOR_NEXT still needs nd%is_loc, nd%id_index)
		! b = for_kind   (bound_array / step_array / len_array)
		! c = itr_type   (static hint; promote logic below may override for int cases)
		case (OP_FOR_SETUP_NAT)
			block
			integer :: fi

			if (nfor + 1 > size(for_iters)) call grow_fors(for_iters)
			nfor = nfor + 1
			fi = nfor
			for_iters(fi)%node_idx = instr%a
			for_iters(fi)%counter  = 0
			for_iters(fi)%len8     = 0
			for_iters(fi)%for_kind = instr%b
			for_iters(fi)%itr_type = int(instr%c)

			select case (instr%b)

			case (bound_array)
				! Stack: [lb][ub], ub at TOS
				call value_move(stack%v(stack%len_  ), for_iters(fi)%ubound_)
				call value_move(stack%v(stack%len_-1), for_iters(fi)%lbound_)
				stack%len_ = stack%len_ - 2
				if (any(i64_type == [for_iters(fi)%lbound_%type, &
				                      for_iters(fi)%ubound_%type])) then
					call promote_i32_i64(for_iters(fi)%lbound_)
					call promote_i32_i64(for_iters(fi)%ubound_)
					for_iters(fi)%itr_type = i64_type
				else
					for_iters(fi)%itr_type = i32_type
				end if
				for_iters(fi)%len8 = for_iters(fi)%ubound_%to_i64() &
				                   - for_iters(fi)%lbound_%to_i64()

			case (step_array)
				! Stack: [lb][step][ub], ub at TOS
				call value_move(stack%v(stack%len_  ), for_iters(fi)%ubound_)
				call value_move(stack%v(stack%len_-1), for_iters(fi)%step  )
				call value_move(stack%v(stack%len_-2), for_iters(fi)%lbound_)
				stack%len_ = stack%len_ - 3
				if (any(i64_type == [for_iters(fi)%lbound_%type, &
				                      for_iters(fi)%step%type, &
				                      for_iters(fi)%ubound_%type])) then
					call promote_i32_i64(for_iters(fi)%lbound_)
					call promote_i32_i64(for_iters(fi)%step)
					call promote_i32_i64(for_iters(fi)%ubound_)
					for_iters(fi)%itr_type = i64_type
				else
					for_iters(fi)%itr_type = for_iters(fi)%lbound_%type
				end if
				select case (for_iters(fi)%itr_type)
				case (i32_type)
					if (for_iters(fi)%step%sca%i32 == 0) then
						call rt_throw(state, err_rt(RC_FOR_STEP_ZERO, 'for loop step is 0'))
						exit
					end if
					for_iters(fi)%len8 = ( &
						for_iters(fi)%ubound_%sca%i32 - for_iters(fi)%lbound_%sca%i32 &
						+ for_iters(fi)%step%sca%i32  &
						- sign(1, for_iters(fi)%step%sca%i32) ) / for_iters(fi)%step%sca%i32
				case (i64_type)
					if (for_iters(fi)%step%sca%i64 == 0) then
						call rt_throw(state, err_rt(RC_FOR_STEP_ZERO, 'for loop step is 0'))
						exit
					end if
					for_iters(fi)%len8 = ( &
						for_iters(fi)%ubound_%sca%i64 - for_iters(fi)%lbound_%sca%i64 &
						+ for_iters(fi)%step%sca%i64  &
						- sign(int(1,8), for_iters(fi)%step%sca%i64) ) / for_iters(fi)%step%sca%i64
				case (f32_type)
					if (for_iters(fi)%step%sca%f32 == 0.0) then
						call rt_throw(state, err_rt(RC_FOR_STEP_ZERO_F, 'for loop step is 0.0'))
						exit
					end if
					for_iters(fi)%len8 = ceiling( &
						(for_iters(fi)%ubound_%sca%f32 - for_iters(fi)%lbound_%sca%f32) &
						/ for_iters(fi)%step%sca%f32)
				case (f64_type)
					if (for_iters(fi)%step%sca%f64 == 0.0d0) then
						call rt_throw(state, err_rt(RC_FOR_STEP_ZERO_F, 'for loop step is 0.0'))
						exit
					end if
					for_iters(fi)%len8 = ceiling( &
						(for_iters(fi)%ubound_%sca%f64 - for_iters(fi)%lbound_%sca%f64) &
						/ for_iters(fi)%step%sca%f64)
				case default
					write(*,*) err_int(IC_STEP_ARRAY_TYPE, 'step array type not implemented')
					call internal_error()
				end select

			case (len_array)
				! Stack: [lb][ub][len], len at TOS
				call value_move(stack%v(stack%len_  ), for_iters(fi)%len_  )
				call value_move(stack%v(stack%len_-1), for_iters(fi)%ubound_)
				call value_move(stack%v(stack%len_-2), for_iters(fi)%lbound_)
				stack%len_ = stack%len_ - 3
				! itr_type comes from instr%c (f32_type or f64_type, set by compiler)
				select case (for_iters(fi)%itr_type)
				case (f32_type, f64_type)
					for_iters(fi)%len8 = for_iters(fi)%len_%to_i64()
				case default
					write(*,*) err_int(IC_BOUND_LEN_TYPE, 'bound/len array type not implemented')
					call internal_error()
				end select

			end select
			end block

		! --- M8: for-loop advance --------------------------------------------------
		! Increments counter; if exhausted: pop iter stack and jump to loop end.
		! Otherwise: call array_at to produce the next iterator value and write
		! it to the loop variable slot, then fall through to the body.
		case (OP_FOR_NEXT)
			! Advance counter; if exhausted: jump to L_pop (does NOT pop nfor here —
			! OP_FOR_POP does that for both exhaustion and break).
			block
			integer :: fi
			fi = nfor
			for_iters(fi)%counter = for_iters(fi)%counter + 1
			if (for_iters(fi)%counter > for_iters(fi)%len8) then
				next_ip = instr%a   ! jump to FOR_POP; no nfor decrement here
			else
				val%type = for_iters(fi)%itr_type
				! for_iters(fi)%elem_vals is unallocated except for
				! expl_array/size_array (populated at OP_FOR_SETUP by
				! compile_array_expr_slots' pre-evaluation); an unallocated
				! allocatable actual argument is treated as not-present for
				! array_at's optional elem_vals dummy, same as %struct below
				call array_at(val, for_iters(fi)%for_kind, for_iters(fi)%counter, &
					for_iters(fi)%lbound_, for_iters(fi)%step, for_iters(fi)%ubound_, &
					for_iters(fi)%len_, for_iters(fi)%array, &
					for_iters(fi)%str_, for_iters(fi)%struct, for_iters(fi)%elem_vals)
				associate(nd => prog%nodes(for_iters(fi)%node_idx))
				if (nd%is_loc) then
					call value_move(val, state%locs%vals(nd%id_index))
				else
					call value_move(val, state%vars%vals(nd%id_index))
				end if
				end associate
			end if
			end block

		! --- M8: for-loop exit (all paths: exhaustion and break) -------------------
		! Emitted once at L_pop, after the loop's back-edge JUMP.
		! Both FOR_NEXT's exhaustion jump and break-statement jumps target here.
		case (OP_FOR_POP)
			nfor = nfor - 1

		! --- M8: array construction -----------------------------------------------
		! Delegates to eval_array_expr for all array kinds (bound, step, len,
		! expl, size, unif).  Rank-1 native specialization is a future perf pass.
		! Sub-expressions were compiled to bytecode (compile_array_expr_slots)
		! and are on TOS; pop them as a slot window instead of AST-walking.
		! Can rt_throw (R21 size mismatch, R25/R26 step-zero), so check rt_halt
		! before pushing a possibly-unset result.
		case (OP_NEW_ARRAY)
			block
			integer :: nslots_, base_
			nslots_ = array_expr_nslots(prog%nodes(instr%a))
			base_ = stack%len_ - nslots_
			call eval_array_expr(prog%nodes(instr%a), state, val, stack%v(base_+1:base_+nslots_))
			stack%len_ = base_
			end block
			if (state%rt_halt) exit
			call vm_push_move(stack, val)

		! --- enum reverse cast, e.g. `Suit(2)` -------------------------------------
		! The ordinal sub-expression was compiled to bytecode (compile_ctrl.f90);
		! pop it and linear-scan the const-pooled %struct(:) variant list built
		! at parse time by parse_enum_cast().  No runtime enum registry is
		! needed.  rt_throw's R32 on no match.
		case (OP_ENUM_CAST)
			call vm_pop_copy(stack, right)
			associate(cv => prog%consts(instr%a))
			block
				integer(kind = 4) :: ord
				integer :: j
				logical :: found
				ord = right%to_i32()
				found = .false.
				do j = 1, size(cv%struct)
					if (cv%struct(j)%sca%i32 == ord) then
						val = cv%struct(j)
						found = .true.
						exit
					end if
				end do
				if (.not. found) then
					call rt_throw(state, err_rt(RC_ENUM_CAST_RANGE, &
						"no variant with value "//str(ord)//" in enum `"// &
						cv%enum_name//"`"))
					exit
				end if
			end block
			end associate
			call vm_push_move(stack, val)

		! --- M8: slice/complex LHS assignment ------------------------------------
		! Handles slice-range LHS (a[1:3] = x) and subscript-less compound
		! assignments by delegating to eval_assignment_expr.  The RHS was
		! compiled to bytecode (compile_ctrl.f90) and is on TOS -- pop it and
		! pass it in rather than letting eval_assignment_expr AST-walk
		! node%right itself.  Below the RHS: the LHS subscript bound
		! sub-expressions (compile_subscript_slots), only present when
		! n%lsubscripts is allocated (the slice-LHS case; the subscript-less
		! compound-assign case has none).  Can rt_throw (R27
		! subscript-step-zero), so check rt_halt before pushing a
		! possibly-unset result.
		case (OP_STORE_SLICE)
			call vm_pop_copy(stack, right)
			associate(n => prog%nodes(instr%a))
			block
			integer :: nslots_, base_
			nslots_ = 0
			if (allocated(n%lsubscripts)) nslots_ = subscript_total_nslots(n)
			base_ = stack%len_ - nslots_
			call eval_assignment_expr(n, state, val, rhs_in = right, &
				slots = stack%v(base_+1:base_+nslots_))
			stack%len_ = base_
			end block
			end associate
			if (state%rt_halt) exit
			call vm_push_move(stack, val)

		! --- M8: top-level return (halt) ------------------------------------------
		! Exits the VM loop immediately; TOS is the return value.
		case (OP_HALT)
			next_ip = prog%len_ + 1

		! --- typed scalar opcodes (Stage 2) --------------------------------------
		! Each operates on the raw sca fields of TOS / TOS-1 with no subroutine
		! calls, no value_move, and no get_binary_op_kind.  The compiler only
		! emits these for same-type scalar operands.

		! Arithmetic (result same type as operands, left = TOS-1, right = TOS)
		case (OP_ADD_I32)
			stack%v(stack%len_-1)%sca%i32 = stack%v(stack%len_-1)%sca%i32 &
			                              + stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              + stack%v(stack%len_  )%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_ADD_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 &
			                              + stack%v(stack%len_  )%sca%f32
			stack%len_ = stack%len_ - 1
		case (OP_ADD_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 &
			                              + stack%v(stack%len_  )%sca%f64
			stack%len_ = stack%len_ - 1

		case (OP_SUB_I32)
			stack%v(stack%len_-1)%sca%i32 = stack%v(stack%len_-1)%sca%i32 &
			                              - stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              - stack%v(stack%len_  )%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 &
			                              - stack%v(stack%len_  )%sca%f32
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 &
			                              - stack%v(stack%len_  )%sca%f64
			stack%len_ = stack%len_ - 1

		case (OP_MUL_I32)
			stack%v(stack%len_-1)%sca%i32 = stack%v(stack%len_-1)%sca%i32 &
			                              * stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              * stack%v(stack%len_  )%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 &
			                              * stack%v(stack%len_  )%sca%f32
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 &
			                              * stack%v(stack%len_  )%sca%f64
			stack%len_ = stack%len_ - 1

		case (OP_DIV_I32)
			stack%v(stack%len_-1)%sca%i32 = stack%v(stack%len_-1)%sca%i32 &
			                              / stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              / stack%v(stack%len_  )%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 &
			                              / stack%v(stack%len_  )%sca%f32
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 &
			                              / stack%v(stack%len_  )%sca%f64
			stack%len_ = stack%len_ - 1

		case (OP_MOD_I32)
			stack%v(stack%len_-1)%sca%i32 = mod(stack%v(stack%len_-1)%sca%i32, &
			                                     stack%v(stack%len_  )%sca%i32)
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I64)
			stack%v(stack%len_-1)%sca%i64 = mod(stack%v(stack%len_-1)%sca%i64, &
			                                     stack%v(stack%len_  )%sca%i64)
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F32)
			stack%v(stack%len_-1)%sca%f32 = mod(stack%v(stack%len_-1)%sca%f32, &
			                                     stack%v(stack%len_  )%sca%f32)
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F64)
			stack%v(stack%len_-1)%sca%f64 = mod(stack%v(stack%len_-1)%sca%f64, &
			                                     stack%v(stack%len_  )%sca%f64)
			stack%len_ = stack%len_ - 1

		! Power: same layout as arithmetic (result same type, left=TOS-1, right=TOS).
		! Semantics match math_bin_pow.f90 same-type scalar cases.
		case (OP_POW_I32)
			stack%v(stack%len_-1)%sca%i32 = stack%v(stack%len_-1)%sca%i32 &
			                              ** stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_POW_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              ** stack%v(stack%len_  )%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_POW_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 &
			                              ** stack%v(stack%len_  )%sca%f32
			stack%len_ = stack%len_ - 1
		case (OP_POW_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 &
			                              ** stack%v(stack%len_  )%sca%f64
			stack%len_ = stack%len_ - 1

		! Mixed i32/i64 arithmetic: result is i64. TOS-1 type updated to i64_type.
		! OP_<OP>_I32_I64: left=i32 (TOS-1), right=i64 (TOS).
		! OP_<OP>_I64_I32: left=i64 (TOS-1), right=i32 (TOS).
		case (OP_ADD_I32_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i32 &
			                              + stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = i64_type
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I64_I32)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              + stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I32_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i32 &
			                              - stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = i64_type
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I64_I32)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              - stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I32_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i32 &
			                              * stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = i64_type
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I64_I32)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              * stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I32_I64)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i32 &
			                              / stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = i64_type
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I64_I32)
			stack%v(stack%len_-1)%sca%i64 = stack%v(stack%len_-1)%sca%i64 &
			                              / stack%v(stack%len_  )%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I32_I64)
			stack%v(stack%len_-1)%sca%i64 = mod(int(stack%v(stack%len_-1)%sca%i32, 8), &
			                                     stack%v(stack%len_  )%sca%i64)
			stack%v(stack%len_-1)%type = i64_type
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I64_I32)
			stack%v(stack%len_-1)%sca%i64 = mod(stack%v(stack%len_-1)%sca%i64, &
			                                     int(stack%v(stack%len_  )%sca%i32, 8))
			stack%len_ = stack%len_ - 1

		! Mixed i32/i64 comparisons: result is bool. TOS-1 type updated to bool_type.
		case (OP_LT_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                               < stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                               < stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LE_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              <= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LE_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              <= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GT_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                               > stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GT_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                               > stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GE_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              >= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GE_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              >= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              == stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              == stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_I32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              /= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_I64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              /= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1

		! Same-type numeric array binop: a=op_kind (token), b=element type.
		! Dispatches to do_array_binop_typed (inlined array kernels).
		case (OP_ARR_BINOP)
			call vm_pop_copy(stack, right)
			call vm_pop_copy(stack, left)
			call do_array_binop_typed(left, right, instr%a, instr%b, val)
			call vm_push_move(stack, val)

		! --- M9: native scalar array element read ------------------------------------
		! Stack before: [sub_1][sub_2]...[sub_nsub]
		! Stack after:  [element_value]
		! a=id_index, b=nsub, c=is_local (0=global, 1=local).
		! Computes the linear index inline — no syntax_eval call per subscript.
		case (OP_INDEX_NAT)
			block
			integer :: nsub_, k_, base_
			integer(kind=8) :: lin_, prod_

			nsub_ = instr%b
			base_ = stack%len_ - nsub_   ! subs at base_+1..base_+nsub_; result → base_+1
			lin_  = 0_8
			prod_ = 1_8

			if (instr%c == 1_8) then
				associate(arr => state%locs%vals(instr%a)%array)
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * arr%size(k_)
				end do
				stack%v(base_+1)%type = arr%type
				select case (arr%type)
				case (bool_type); stack%v(base_+1)%sca%bool = arr%bool(lin_+1)
				case (i32_type);  stack%v(base_+1)%sca%i32  = arr%i32(lin_+1)
				case (i64_type);  stack%v(base_+1)%sca%i64  = arr%i64(lin_+1)
				case (f32_type);  stack%v(base_+1)%sca%f32  = arr%f32(lin_+1)
				case (f64_type);  stack%v(base_+1)%sca%f64  = arr%f64(lin_+1)
				end select
				end associate
			else
				associate(arr => state%vars%vals(instr%a)%array)
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * arr%size(k_)
				end do
				stack%v(base_+1)%type = arr%type
				select case (arr%type)
				case (bool_type); stack%v(base_+1)%sca%bool = arr%bool(lin_+1)
				case (i32_type);  stack%v(base_+1)%sca%i32  = arr%i32(lin_+1)
				case (i64_type);  stack%v(base_+1)%sca%i64  = arr%i64(lin_+1)
				case (f32_type);  stack%v(base_+1)%sca%f32  = arr%f32(lin_+1)
				case (f64_type);  stack%v(base_+1)%sca%f64  = arr%f64(lin_+1)
				end select
				end associate
			end if

			stack%len_ = base_ + 1
			end block

		! --- M9: native scalar array element write (plain '=' only) ------------------
		! Stack before: [sub_1]...[sub_nsub][rhs]
		! Stack after:  [rhs]   (leaves stored value on top, matching OP_STORE_IDX)
		! a=id_index, b=nsub, c=is_local (0=global, 1=local).
		! Only emitted for numeric/bool element types and op == '='.
		case (OP_STORE_IDX_NAT)
			block
			integer :: nsub_, k_, base_
			integer(kind=8) :: lin_, prod_

			nsub_ = instr%b
			! Stack layout: subs at len_-nsub_-1+1..len_-1, RHS at len_
			base_ = stack%len_ - nsub_ - 1   ! base_+1..base_+nsub_ are subs; base_+nsub_+1 is RHS
			lin_  = 0_8
			prod_ = 1_8

			if (instr%c == 1_8) then
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * state%locs%vals(instr%a)%array%size(k_)
				end do
				! Use to_*() for type-safe reads — handles RHS type != array element type
				! (e.g. h is i64 from size(), but s is an i32 array).
				select case (state%locs%vals(instr%a)%array%type)
				case (bool_type)
					state%locs%vals(instr%a)%array%bool(lin_+1) = stack%v(base_+nsub_+1)%sca%bool
				case (i32_type)
					state%locs%vals(instr%a)%array%i32(lin_+1) = stack%v(base_+nsub_+1)%to_i32()
				case (i64_type)
					state%locs%vals(instr%a)%array%i64(lin_+1) = stack%v(base_+nsub_+1)%to_i64()
				case (f32_type)
					state%locs%vals(instr%a)%array%f32(lin_+1) = stack%v(base_+nsub_+1)%to_f32()
				case (f64_type)
					state%locs%vals(instr%a)%array%f64(lin_+1) = stack%v(base_+nsub_+1)%to_f64()
				end select
			else
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * state%vars%vals(instr%a)%array%size(k_)
				end do
				select case (state%vars%vals(instr%a)%array%type)
				case (bool_type)
					state%vars%vals(instr%a)%array%bool(lin_+1) = stack%v(base_+nsub_+1)%sca%bool
				case (i32_type)
					state%vars%vals(instr%a)%array%i32(lin_+1) = stack%v(base_+nsub_+1)%to_i32()
				case (i64_type)
					state%vars%vals(instr%a)%array%i64(lin_+1) = stack%v(base_+nsub_+1)%to_i64()
				case (f32_type)
					state%vars%vals(instr%a)%array%f32(lin_+1) = stack%v(base_+nsub_+1)%to_f32()
				case (f64_type)
					state%vars%vals(instr%a)%array%f64(lin_+1) = stack%v(base_+nsub_+1)%to_f64()
				end select
			end if

			! Leave stored value (RHS) on top: copy POD fields from RHS slot → base_+1
			stack%v(base_+1)%type = stack%v(base_+nsub_+1)%type
			stack%v(base_+1)%sca  = stack%v(base_+nsub_+1)%sca
			stack%len_ = base_ + 1
			end block

		! --- M10: compound array element op without subscript_eval ----------------
		! Stack before: [sub_1]...[sub_nsub][rhs]
		! Stack after:  [rhs]
		! a=id_index, b=nsub, c=op_kind*2+is_local
		case (OP_COMPOUND_IDX_NAT)
			block
			integer :: nsub_, k_, base_, op_kind_
			integer(kind=8) :: lin_, prod_, c_

			c_     = instr%c
			nsub_  = instr%b
			op_kind_ = int(c_ / 2_8)
			base_  = stack%len_ - nsub_ - 1

			lin_  = 0_8
			prod_ = 1_8

			if (mod(c_, 2_8) == 1_8) then
				associate(arr => state%locs%vals(instr%a)%array)
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * arr%size(k_)
				end do
				! Read current element, apply compound op, write back
				call get_array_val(arr, lin_, left)
				call vm_pop_copy(stack, right)
				stack%len_ = base_
				call do_compound(left, right, op_kind_)
				call set_array_val(arr, lin_, left)
				end associate
			else
				associate(arr => state%vars%vals(instr%a)%array)
				do k_ = 1, nsub_
					select case (stack%v(base_+k_)%type)
					case (i32_type); lin_ = lin_ + prod_ * int(stack%v(base_+k_)%sca%i32, 8)
					case (i64_type); lin_ = lin_ + prod_ * stack%v(base_+k_)%sca%i64
					end select
					prod_ = prod_ * arr%size(k_)
				end do
				call get_array_val(arr, lin_, left)
				call vm_pop_copy(stack, right)
				stack%len_ = base_
				call do_compound(left, right, op_kind_)
				call set_array_val(arr, lin_, left)
				end associate
			end if
			call vm_push_move(stack, left)
			end block

		! --- M10: native string subscript read ------------------------------------
		! Handles two sub-cases at runtime:
		!   scalar_str[i]  → 1-char read: val%str%s(i+1:i+1)
		!   str_arr[i]     → full element read from rank-1 array: array%str(i+1)%s
		! Stack before: [subscript]
		! Stack after:  [result string]
		! a=id_index, c=is_local
		case (OP_STR_INDEX_NAT)
			block
			integer(kind=8) :: i8_

			i8_ = stack%v(stack%len_)%to_i64()
			stack%len_ = stack%len_ - 1
			val%type = str_type
			if (.not. allocated(val%str)) allocate(val%str)
			if (instr%c == 1_8) then
				associate(v => state%locs%vals(instr%a))
				if (v%type == str_type) then
					val%str%s = v%str%s(i8_+1 : i8_+1)
				else
					val%str%s = v%array%str(i8_+1)%s
				end if
				end associate
			else
				associate(v => state%vars%vals(instr%a))
				if (v%type == str_type) then
					val%str%s = v%str%s(i8_+1 : i8_+1)
				else
					val%str%s = v%array%str(i8_+1)%s
				end if
				end associate
			end if
			call vm_push_move(stack, val)
			end block

		! --- M10: native scalar string substring (bound_array range) read --------
		! Stack before: [lbound][ubound]
		! Stack after:  [substring]
		! a=id_index, c=is_local
		case (OP_STR_SLICE_NAT)
			block
			integer(kind=8) :: lb_, ub_

			ub_ = stack%v(stack%len_  )%to_i64()
			lb_ = stack%v(stack%len_-1)%to_i64()
			stack%len_ = stack%len_ - 2
			val%type = str_type
			if (.not. allocated(val%str)) allocate(val%str)
			if (instr%c == 1_8) then
				val%str%s = state%locs%vals(instr%a)%str%s(lb_+1 : ub_)
			else
				val%str%s = state%vars%vals(instr%a)%str%s(lb_+1 : ub_)
			end if
			call vm_push_move(stack, val)
			end block

		! OP_SLICE_NAT: native array range-slice read.  All subscripts are range_sub
		! with explicit bounds compiled onto the stack.
		! Stack before: [lb_1][ub_1][lb_2][ub_2]...[lb_n][ub_n]
		! Stack after:  [result_array]
		! a=id_index, b=ndim, c=is_local
		case (OP_SLICE_NAT)
			block
			integer :: ndim_, k_
			integer(kind=8) :: lb_(MAX_NAT_SLICE_RANK), ub_(MAX_NAT_SLICE_RANK), lens_(MAX_NAT_SLICE_RANK)
			integer(kind=8) :: len_tot_, outer_count_, outer_i_
			integer(kind=8) :: src_off_, src_prod_, dst_off_, ci_, tmp_i_

			ndim_ = instr%b

			! Bounds pushed [lb_1,ub_1,...,lb_n,ub_n]; pop in reverse so we get dim 1 first.
			do k_ = ndim_, 1, -1
				ub_(k_) = stack%v(stack%len_  )%to_i64()
				lb_(k_) = stack%v(stack%len_-1)%to_i64()
				stack%len_ = stack%len_ - 2
			end do

			do k_ = 1, ndim_
				lens_(k_) = max(0_8, ub_(k_) - lb_(k_))
			end do
			len_tot_ = product(lens_(1:ndim_))

			allocate(val%array)
			val%type        = array_type
			val%array%kind  = expl_array
			val%array%rank  = ndim_
			val%array%len_  = len_tot_
			allocate(val%array%size(ndim_))
			val%array%size(1:ndim_) = lens_(1:ndim_)

			if (instr%c == 1_8) then
				associate(src => state%locs%vals(instr%a)%array)
				val%array%type = src%type
				call allocate_array(val, len_tot_)
				if (ndim_ == 1) then
					select case (src%type)
					case (bool_type); val%array%bool(1:len_tot_) = src%bool(lb_(1)+1 : ub_(1))
					case (i32_type);  val%array%i32 (1:len_tot_) = src%i32 (lb_(1)+1 : ub_(1))
					case (i64_type);  val%array%i64 (1:len_tot_) = src%i64 (lb_(1)+1 : ub_(1))
					case (f32_type);  val%array%f32 (1:len_tot_) = src%f32 (lb_(1)+1 : ub_(1))
					case (f64_type);  val%array%f64 (1:len_tot_) = src%f64 (lb_(1)+1 : ub_(1))
					case (str_type);  val%array%str (1:len_tot_) = src%str (lb_(1)+1 : ub_(1))
					end select
				else
					outer_count_ = product(lens_(2:ndim_))
					do outer_i_ = 0, outer_count_ - 1
						! Decompose outer_i_ into per-dim coords; compute flat source offset.
						src_off_  = 0_8
						src_prod_ = src%size(1)
						tmp_i_    = outer_i_
						do k_ = 2, ndim_
							ci_       = mod(tmp_i_, lens_(k_))
							tmp_i_    = tmp_i_ / lens_(k_)
							src_off_  = src_off_ + (lb_(k_) + ci_) * src_prod_
							src_prod_ = src_prod_ * src%size(k_)
						end do
						dst_off_ = outer_i_ * lens_(1)
						select case (src%type)
						case (bool_type)
							val%array%bool(dst_off_+1 : dst_off_+lens_(1)) = &
								src%bool(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (i32_type)
							val%array%i32(dst_off_+1 : dst_off_+lens_(1)) = &
								src%i32(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (i64_type)
							val%array%i64(dst_off_+1 : dst_off_+lens_(1)) = &
								src%i64(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (f32_type)
							val%array%f32(dst_off_+1 : dst_off_+lens_(1)) = &
								src%f32(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (f64_type)
							val%array%f64(dst_off_+1 : dst_off_+lens_(1)) = &
								src%f64(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (str_type)
							val%array%str(dst_off_+1 : dst_off_+lens_(1)) = &
								src%str(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						end select
					end do
				end if
				end associate
			else
				associate(src => state%vars%vals(instr%a)%array)
				val%array%type = src%type
				call allocate_array(val, len_tot_)
				if (ndim_ == 1) then
					select case (src%type)
					case (bool_type); val%array%bool(1:len_tot_) = src%bool(lb_(1)+1 : ub_(1))
					case (i32_type);  val%array%i32 (1:len_tot_) = src%i32 (lb_(1)+1 : ub_(1))
					case (i64_type);  val%array%i64 (1:len_tot_) = src%i64 (lb_(1)+1 : ub_(1))
					case (f32_type);  val%array%f32 (1:len_tot_) = src%f32 (lb_(1)+1 : ub_(1))
					case (f64_type);  val%array%f64 (1:len_tot_) = src%f64 (lb_(1)+1 : ub_(1))
					case (str_type);  val%array%str (1:len_tot_) = src%str (lb_(1)+1 : ub_(1))
					end select
				else
					outer_count_ = product(lens_(2:ndim_))
					do outer_i_ = 0, outer_count_ - 1
						src_off_  = 0_8
						src_prod_ = src%size(1)
						tmp_i_    = outer_i_
						do k_ = 2, ndim_
							ci_       = mod(tmp_i_, lens_(k_))
							tmp_i_    = tmp_i_ / lens_(k_)
							src_off_  = src_off_ + (lb_(k_) + ci_) * src_prod_
							src_prod_ = src_prod_ * src%size(k_)
						end do
						dst_off_ = outer_i_ * lens_(1)
						select case (src%type)
						case (bool_type)
							val%array%bool(dst_off_+1 : dst_off_+lens_(1)) = &
								src%bool(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (i32_type)
							val%array%i32(dst_off_+1 : dst_off_+lens_(1)) = &
								src%i32(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (i64_type)
							val%array%i64(dst_off_+1 : dst_off_+lens_(1)) = &
								src%i64(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (f32_type)
							val%array%f32(dst_off_+1 : dst_off_+lens_(1)) = &
								src%f32(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (f64_type)
							val%array%f64(dst_off_+1 : dst_off_+lens_(1)) = &
								src%f64(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						case (str_type)
							val%array%str(dst_off_+1 : dst_off_+lens_(1)) = &
								src%str(lb_(1)+src_off_+1 : lb_(1)+src_off_+lens_(1))
						end select
					end do
				end if
				end associate
			end if

			call vm_push_move(stack, val)
			end block

		! OP_STORE_SLICE_NAT: native array range-slice write (plain '=' only).
		! Stack before: [lb_1][ub_1]...[lb_n][ub_n][rhs_array]
		! Stack after:  [rhs_array]   (RHS left on top, matching OP_STORE_SLICE convention)
		! a=id_index, b=ndim, c=is_local
		case (OP_STORE_SLICE_NAT)
			block
			integer :: ndim_, k_
			integer(kind=8) :: lb_(MAX_NAT_SLICE_RANK), ub_(MAX_NAT_SLICE_RANK), lens_(MAX_NAT_SLICE_RANK)
			integer(kind=8) :: outer_count_, outer_i_
			integer(kind=8) :: dst_off_, dst_prod_, rhs_off_, ci_, tmp_i_

			ndim_ = instr%b

			! RHS is on top; move it into `right`, then pop bounds.
			call vm_pop_copy(stack, right)

			do k_ = ndim_, 1, -1
				ub_(k_) = stack%v(stack%len_  )%to_i64()
				lb_(k_) = stack%v(stack%len_-1)%to_i64()
				stack%len_ = stack%len_ - 2
			end do

			do k_ = 1, ndim_
				lens_(k_) = max(0_8, ub_(k_) - lb_(k_))
			end do

			if (instr%c == 1_8) then
				associate(dst => state%locs%vals(instr%a)%array)
				if (ndim_ == 1) then
					select case (dst%type)
					case (bool_type); dst%bool(lb_(1)+1 : ub_(1)) = right%array%bool(1:lens_(1))
					case (i32_type);  dst%i32 (lb_(1)+1 : ub_(1)) = right%array%i32 (1:lens_(1))
					case (i64_type);  dst%i64 (lb_(1)+1 : ub_(1)) = right%array%i64 (1:lens_(1))
					case (f32_type);  dst%f32 (lb_(1)+1 : ub_(1)) = right%array%f32 (1:lens_(1))
					case (f64_type);  dst%f64 (lb_(1)+1 : ub_(1)) = right%array%f64 (1:lens_(1))
					case (str_type);  dst%str (lb_(1)+1 : ub_(1)) = right%array%str (1:lens_(1))
					end select
				else
					outer_count_ = product(lens_(2:ndim_))
					do outer_i_ = 0, outer_count_ - 1
						! Destination offset uses dst%size for strides; rhs is packed.
						dst_off_  = lb_(1)
						dst_prod_ = dst%size(1)
						tmp_i_    = outer_i_
						do k_ = 2, ndim_
							ci_       = mod(tmp_i_, lens_(k_))
							tmp_i_    = tmp_i_ / lens_(k_)
							dst_off_  = dst_off_ + (lb_(k_) + ci_) * dst_prod_
							dst_prod_ = dst_prod_ * dst%size(k_)
						end do
						rhs_off_ = outer_i_ * lens_(1)
						select case (dst%type)
						case (bool_type)
							dst%bool(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%bool(rhs_off_+1 : rhs_off_+lens_(1))
						case (i32_type)
							dst%i32(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%i32(rhs_off_+1 : rhs_off_+lens_(1))
						case (i64_type)
							dst%i64(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%i64(rhs_off_+1 : rhs_off_+lens_(1))
						case (f32_type)
							dst%f32(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%f32(rhs_off_+1 : rhs_off_+lens_(1))
						case (f64_type)
							dst%f64(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%f64(rhs_off_+1 : rhs_off_+lens_(1))
						case (str_type)
							dst%str(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%str(rhs_off_+1 : rhs_off_+lens_(1))
						end select
					end do
				end if
				end associate
			else
				associate(dst => state%vars%vals(instr%a)%array)
				if (ndim_ == 1) then
					select case (dst%type)
					case (bool_type); dst%bool(lb_(1)+1 : ub_(1)) = right%array%bool(1:lens_(1))
					case (i32_type);  dst%i32 (lb_(1)+1 : ub_(1)) = right%array%i32 (1:lens_(1))
					case (i64_type);  dst%i64 (lb_(1)+1 : ub_(1)) = right%array%i64 (1:lens_(1))
					case (f32_type);  dst%f32 (lb_(1)+1 : ub_(1)) = right%array%f32 (1:lens_(1))
					case (f64_type);  dst%f64 (lb_(1)+1 : ub_(1)) = right%array%f64 (1:lens_(1))
					case (str_type);  dst%str (lb_(1)+1 : ub_(1)) = right%array%str (1:lens_(1))
					end select
				else
					outer_count_ = product(lens_(2:ndim_))
					do outer_i_ = 0, outer_count_ - 1
						dst_off_  = lb_(1)
						dst_prod_ = dst%size(1)
						tmp_i_    = outer_i_
						do k_ = 2, ndim_
							ci_       = mod(tmp_i_, lens_(k_))
							tmp_i_    = tmp_i_ / lens_(k_)
							dst_off_  = dst_off_ + (lb_(k_) + ci_) * dst_prod_
							dst_prod_ = dst_prod_ * dst%size(k_)
						end do
						rhs_off_ = outer_i_ * lens_(1)
						select case (dst%type)
						case (bool_type)
							dst%bool(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%bool(rhs_off_+1 : rhs_off_+lens_(1))
						case (i32_type)
							dst%i32(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%i32(rhs_off_+1 : rhs_off_+lens_(1))
						case (i64_type)
							dst%i64(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%i64(rhs_off_+1 : rhs_off_+lens_(1))
						case (f32_type)
							dst%f32(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%f32(rhs_off_+1 : rhs_off_+lens_(1))
						case (f64_type)
							dst%f64(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%f64(rhs_off_+1 : rhs_off_+lens_(1))
						case (str_type)
							dst%str(dst_off_+1 : dst_off_+lens_(1)) = &
								right%array%str(rhs_off_+1 : rhs_off_+lens_(1))
						end select
					end do
				end if
				end associate
			end if

			call vm_push_move(stack, right)
			end block

		! OP_SIZE_NAT: read array%size(dim+1) or array%len_ directly from the variable
		! slot without deep-copying the array.  a=slot_id, b=dim (b<0 → total len_),
		! c=is_local.  Eliminates O(N) allocation from size(large_arr, dim) in loops.
		case (OP_SIZE_NAT)
			block
			integer :: rank_
			if (instr%c == 1_8) then
				rank_ = state%locs%vals(instr%a)%array%rank
			else
				rank_ = state%vars%vals(instr%a)%array%rank
			end if
			if (instr%b >= 0 .and. instr%b >= rank_) then
				call rt_throw(state, err_rt(RC_SIZE_RANK_MISMATCH, "rank mismatch in size() call"))
				exit
			end if
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type = i64_type
			if (instr%c == 1_8) then
				if (instr%b < 0) then
					stack%v(stack%len_)%sca%i64 = state%locs%vals(instr%a)%array%len_
				else
					stack%v(stack%len_)%sca%i64 = state%locs%vals(instr%a)%array%size(instr%b + 1)
				end if
			else
				if (instr%b < 0) then
					stack%v(stack%len_)%sca%i64 = state%vars%vals(instr%a)%array%len_
				else
					stack%v(stack%len_)%sca%i64 = state%vars%vals(instr%a)%array%size(instr%b + 1)
				end if
			end if
			end block

		! Comparisons: result type is bool; TOS-1 type updated to bool_type.
		case (OP_LT_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                               < stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                               < stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                               < stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                               < stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1

		case (OP_LE_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              <= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LE_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              <= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LE_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                              <= stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_LE_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                              <= stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1

		case (OP_GT_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                               > stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GT_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                               > stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GT_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                               > stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GT_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                               > stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1

		case (OP_GE_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              >= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GE_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              >= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GE_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                              >= stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_GE_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                              >= stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1

		case (OP_EQ_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              == stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              == stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                              == stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                              == stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_EQ_BOOL)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%bool &
			                             .eqv. stack%v(stack%len_  )%sca%bool
			stack%len_ = stack%len_ - 1

		case (OP_NE_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 &
			                              /= stack%v(stack%len_  )%sca%i32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 &
			                              /= stack%v(stack%len_  )%sca%i64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 &
			                              /= stack%v(stack%len_  )%sca%f32
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 &
			                              /= stack%v(stack%len_  )%sca%f64
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
		case (OP_NE_BOOL)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%bool &
			                             .neqv. stack%v(stack%len_  )%sca%bool
			stack%len_ = stack%len_ - 1

		! String equality: compare two str_type stack slots directly without
		! vm_pop_copy (avoids allocatable-string heap copies per comparison).
		! Uses is_str_eq for length-aware comparison (Fortran's == pads spaces).
		case (OP_EQ_STR)
			block
			logical :: b_
			b_ = is_str_eq(stack%v(stack%len_-1)%str%s, stack%v(stack%len_)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block
		case (OP_NE_STR)
			block
			logical :: b_
			b_ = .not. is_str_eq(stack%v(stack%len_-1)%str%s, stack%v(stack%len_)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block

		! String ordering: same shape as OP_EQ_STR / OP_NE_STR above, but using
		! is_str_lt() for lexicographic, length-aware comparison (see its
		! comment in utils.f90 for why raw Fortran `<` is unsafe for strings).
		case (OP_LT_STR)
			block
			logical :: b_
			b_ = is_str_lt(stack%v(stack%len_-1)%str%s, stack%v(stack%len_)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block
		case (OP_LE_STR)
			block
			logical :: b_
			b_ = .not. is_str_lt(stack%v(stack%len_)%str%s, stack%v(stack%len_-1)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block
		case (OP_GT_STR)
			block
			logical :: b_
			b_ = is_str_lt(stack%v(stack%len_)%str%s, stack%v(stack%len_-1)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block
		case (OP_GE_STR)
			block
			logical :: b_
			b_ = .not. is_str_lt(stack%v(stack%len_-1)%str%s, stack%v(stack%len_)%str%s)
			deallocate(stack%v(stack%len_-1)%str)
			deallocate(stack%v(stack%len_  )%str)
			stack%v(stack%len_-1)%sca%bool = b_
			stack%v(stack%len_-1)%type = bool_type
			stack%len_ = stack%len_ - 1
			end block

		! Bool binary
		case (OP_AND_BOOL)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%bool &
			                           .and. stack%v(stack%len_  )%sca%bool
			stack%len_ = stack%len_ - 1
		case (OP_OR_BOOL)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%bool &
			                            .or. stack%v(stack%len_  )%sca%bool
			stack%len_ = stack%len_ - 1

		! Unary: operand = TOS; result replaces TOS in-place.
		case (OP_NEG_I32)
			stack%v(stack%len_)%sca%i32 = -stack%v(stack%len_)%sca%i32
		case (OP_NEG_I64)
			stack%v(stack%len_)%sca%i64 = -stack%v(stack%len_)%sca%i64
		case (OP_NEG_F32)
			stack%v(stack%len_)%sca%f32 = -stack%v(stack%len_)%sca%f32
		case (OP_NEG_F64)
			stack%v(stack%len_)%sca%f64 = -stack%v(stack%len_)%sca%f64
		case (OP_NOT_BOOL)
			stack%v(stack%len_)%sca%bool = .not. stack%v(stack%len_)%sca%bool
		case (OP_BNOT_I32)
			stack%v(stack%len_)%sca%i32 = not(stack%v(stack%len_)%sca%i32)
		case (OP_BNOT_I64)
			stack%v(stack%len_)%sca%i64 = not(stack%v(stack%len_)%sca%i64)

		! Typed scalar loads: push without value_copy overhead.
		! For LOAD_CONST_*: value embedded in instruction (no const pool lookup).
		! For LOAD_LOCAL/GLOBAL_*: slot index in instr%a, direct field read.
		case (OP_LOAD_CONST_BOOL)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = bool_type
			stack%v(stack%len_)%sca%bool = (instr%a /= 0)
		case (OP_LOAD_CONST_I32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i32_type
			stack%v(stack%len_)%sca%i32 = instr%a
		case (OP_LOAD_CONST_I64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i64_type
			stack%v(stack%len_)%sca%i64 = instr%c
		case (OP_LOAD_CONST_F32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f32_type
			stack%v(stack%len_)%sca%f32 = transfer(instr%a, 0.0_4)
		case (OP_LOAD_CONST_F64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f64_type
			stack%v(stack%len_)%sca%f64 = transfer(instr%c, 0.0d0)

		case (OP_LOAD_LOCAL_BOOL)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type     = bool_type
			stack%v(stack%len_)%sca%bool = state%locs%vals(instr%a)%sca%bool
		case (OP_LOAD_LOCAL_I32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i32_type
			stack%v(stack%len_)%sca%i32 = state%locs%vals(instr%a)%sca%i32
		case (OP_LOAD_LOCAL_I64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i64_type
			stack%v(stack%len_)%sca%i64 = state%locs%vals(instr%a)%sca%i64
		case (OP_LOAD_LOCAL_F32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f32_type
			stack%v(stack%len_)%sca%f32 = state%locs%vals(instr%a)%sca%f32
		case (OP_LOAD_LOCAL_F64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f64_type
			stack%v(stack%len_)%sca%f64 = state%locs%vals(instr%a)%sca%f64

		case (OP_LOAD_GLOBAL_BOOL)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type     = bool_type
			stack%v(stack%len_)%sca%bool = state%vars%vals(instr%a)%sca%bool
		case (OP_LOAD_GLOBAL_I32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i32_type
			stack%v(stack%len_)%sca%i32 = state%vars%vals(instr%a)%sca%i32
		case (OP_LOAD_GLOBAL_I64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = i64_type
			stack%v(stack%len_)%sca%i64 = state%vars%vals(instr%a)%sca%i64
		case (OP_LOAD_GLOBAL_F32)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f32_type
			stack%v(stack%len_)%sca%f32 = state%vars%vals(instr%a)%sca%f32
		case (OP_LOAD_GLOBAL_F64)
			stack%len_ = stack%len_ + 1
			if (stack%len_ > stack%cap) call vm_stack_grow(stack)
			stack%v(stack%len_)%type    = f64_type
			stack%v(stack%len_)%sca%f64 = state%vars%vals(instr%a)%sca%f64

		! Typed scalar stores: write TOS into slot, keep TOS.
		! Always write %type too (slot may be freshly allocated = unknown_type).
		case (OP_STORE_LOCAL_BOOL)
			state%locs%vals(instr%a)%type     = bool_type
			state%locs%vals(instr%a)%sca%bool = stack%v(stack%len_)%sca%bool
		case (OP_STORE_LOCAL_I32)
			state%locs%vals(instr%a)%type    = i32_type
			state%locs%vals(instr%a)%sca%i32 = stack%v(stack%len_)%sca%i32
		case (OP_STORE_LOCAL_I64)
			state%locs%vals(instr%a)%type    = i64_type
			state%locs%vals(instr%a)%sca%i64 = stack%v(stack%len_)%sca%i64
		case (OP_STORE_LOCAL_F32)
			state%locs%vals(instr%a)%type    = f32_type
			state%locs%vals(instr%a)%sca%f32 = stack%v(stack%len_)%sca%f32
		case (OP_STORE_LOCAL_F64)
			state%locs%vals(instr%a)%type    = f64_type
			state%locs%vals(instr%a)%sca%f64 = stack%v(stack%len_)%sca%f64

		case (OP_STORE_GLOBAL_BOOL)
			state%vars%vals(instr%a)%type     = bool_type
			state%vars%vals(instr%a)%sca%bool = stack%v(stack%len_)%sca%bool
		case (OP_STORE_GLOBAL_I32)
			state%vars%vals(instr%a)%type    = i32_type
			state%vars%vals(instr%a)%sca%i32 = stack%v(stack%len_)%sca%i32
		case (OP_STORE_GLOBAL_I64)
			state%vars%vals(instr%a)%type    = i64_type
			state%vars%vals(instr%a)%sca%i64 = stack%v(stack%len_)%sca%i64
		case (OP_STORE_GLOBAL_F32)
			state%vars%vals(instr%a)%type    = f32_type
			state%vars%vals(instr%a)%sca%f32 = stack%v(stack%len_)%sca%f32
		case (OP_STORE_GLOBAL_F64)
			state%vars%vals(instr%a)%type    = f64_type
			state%vars%vals(instr%a)%sca%f64 = stack%v(stack%len_)%sca%f64

		case (OP_SUBSCRIPT_TOS)
			! Stack layout: [fn_return_value][slot_1]...[slot_N], N =
			! subscript_total_nslots -- the return value is pushed by
			! OP_CALL/OP_CALL_PTR/OP_CALL_INTR, then compile_subscript_slots
			! pushes the (possibly zero) pre-evaluated subscript bound values
			! on top of it.  Pop both regions, apply subscripts, push result.
			block
			integer :: nslots_, value_idx_
			nslots_ = subscript_total_nslots(prog%nodes(instr%a))
			value_idx_ = stack%len_ - nslots_
			call value_move(stack%v(value_idx_), left)
			call apply_subscripts_to_val(prog%nodes(instr%a), left, state, val, &
				stack%v(value_idx_+1 : value_idx_+nslots_))
			stack%len_ = value_idx_ - 1
			end block
			if (state%rt_halt) exit
			call vm_push_move(stack, val)

		! OP_UNIF_ARRAY_NAT: native uniform-fill array construction.
		! a = element type, b = rank.
		! Stack before: [size(1)][size(2)]...[size(rank)][fill_val]
		! Stack after:  [result_array]
		case (OP_UNIF_ARRAY_NAT)
			block
			integer :: rk_, k_
			integer(kind=8) :: dims_(MAX_NAT_UNIF_RANK), len_tot_
			type(value_t) :: fill_

			rk_     = instr%b
			call vm_pop_copy(stack, fill_)
			len_tot_ = 1
			do k_ = rk_, 1, -1
				dims_(k_) = stack%v(stack%len_)%to_i64()
				stack%len_ = stack%len_ - 1
				len_tot_   = len_tot_ * dims_(k_)
			end do

			allocate(val%array)
			val%type           = array_type
			val%array%type     = instr%a
			val%array%rank     = rk_
			val%array%len_     = len_tot_
			allocate(val%array%size(rk_))
			val%array%size(1:rk_) = dims_(1:rk_)
			call allocate_array(val, len_tot_)

			select case (instr%a)
			case (bool_type); val%array%bool = fill_%sca%bool
			case (i32_type);  val%array%i32  = fill_%sca%i32
			case (i64_type);  val%array%i64  = fill_%sca%i64
			case (f32_type);  val%array%f32  = fill_%sca%f32
			case (f64_type);  val%array%f64  = fill_%sca%f64
			end select

			call vm_push_move(stack, val)
			end block

		! OP_BOUND_ARRAY_NAT: native integer range array [lb:ub].
		! a = element type (i32_type or i64_type).
		! Stack before: [lb][ub]
		! Stack after:  [result_array]
		case (OP_BOUND_ARRAY_NAT)
			block
			integer(kind=8) :: lb_, ub_, len_, k8_

			ub_ = stack%v(stack%len_  )%to_i64()
			lb_ = stack%v(stack%len_-1)%to_i64()
			stack%len_ = stack%len_ - 2

			len_ = max(0_8, ub_ - lb_)

			allocate(val%array)
			val%type           = array_type
			val%array%type     = instr%a
			val%array%rank     = 1
			val%array%len_     = len_
			allocate(val%array%size(1))
			val%array%size(1)  = len_
			call allocate_array(val, len_)

			select case (instr%a)
			case (i32_type)
				do k8_ = 1, len_
					val%array%i32(k8_) = int(lb_ + k8_ - 1, 4)
				end do
			case (i64_type)
				do k8_ = 1, len_
					val%array%i64(k8_) = lb_ + k8_ - 1
				end do
			end select

			call vm_push_move(stack, val)
			end block

		! OP_EXPL_ARRAY_NAT: native explicit array literal [e1, e2, ..., en].
		! a = element type (bool/i32/i64/f32/f64_type)
		! b = n_elems (compile-time constant)
		! Stack before: [e1][e2]...[en]  (e1 at stack%len_-n+1, en at TOS)
		! Stack after:  [result_array]
		case (OP_EXPL_ARRAY_NAT)
			block
			integer :: n_, j_
			integer(kind=8) :: len_

			n_   = instr%b
			len_ = int(n_, 8)

			allocate(val%array)
			val%type          = array_type
			val%array%type    = instr%a
			val%array%kind    = expl_array
			val%array%rank    = 1
			val%array%len_    = len_
			allocate(val%array%size(1))
			val%array%size(1) = len_
			call allocate_array(val, len_)

			select case (instr%a)
			case (bool_type)
				do j_ = 1, n_
					val%array%bool(j_) = stack%v(stack%len_ - n_ + j_)%sca%bool
				end do
			case (i32_type)
				do j_ = 1, n_
					val%array%i32(j_) = stack%v(stack%len_ - n_ + j_)%sca%i32
				end do
			case (i64_type)
				do j_ = 1, n_
					val%array%i64(j_) = stack%v(stack%len_ - n_ + j_)%sca%i64
				end do
			case (f32_type)
				do j_ = 1, n_
					val%array%f32(j_) = stack%v(stack%len_ - n_ + j_)%sca%f32
				end do
			case (f64_type)
				do j_ = 1, n_
					val%array%f64(j_) = stack%v(stack%len_ - n_ + j_)%sca%f64
				end do
			end select
			stack%len_ = stack%len_ - n_

			call vm_push_move(stack, val)
			end block

		! Mixed f32/i32 scalar arithmetic (result f32) and comparisons (result bool).
		! When float is TOS-1: type unchanged. When int is TOS-1: type updated to f32_type.
		case (OP_ADD_F32_I32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 + stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I32_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i32 + stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F32_I32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 - stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I32_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i32 - stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F32_I32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 * stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I32_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i32 * stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F32_I32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 / stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I32_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i32 / stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F32_I32)
			stack%v(stack%len_-1)%sca%f32 = mod(stack%v(stack%len_-1)%sca%f32, real(stack%v(stack%len_)%sca%i32, 4))
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I32_F32)
			stack%v(stack%len_-1)%sca%f32 = mod(real(stack%v(stack%len_-1)%sca%i32, 4), stack%v(stack%len_)%sca%f32)
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 < stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LT_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 < stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 <= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 <= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 > stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 > stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 >= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 >= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 == stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 == stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_F32_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 /= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_I32_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 /= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1

		! Mixed f32/i64 scalar arithmetic and comparisons.
		case (OP_ADD_F32_I64)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 + stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I64_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i64 + stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F32_I64)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 - stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I64_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i64 - stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F32_I64)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 * stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I64_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i64 * stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F32_I64)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%f32 / stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I64_F32)
			stack%v(stack%len_-1)%sca%f32 = stack%v(stack%len_-1)%sca%i64 / stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F32_I64)
			stack%v(stack%len_-1)%sca%f32 = mod(stack%v(stack%len_-1)%sca%f32, real(stack%v(stack%len_)%sca%i64, 4))
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I64_F32)
			stack%v(stack%len_-1)%sca%f32 = mod(real(stack%v(stack%len_-1)%sca%i64, 4), stack%v(stack%len_)%sca%f32)
			stack%v(stack%len_-1)%type = f32_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 < stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LT_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 < stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 <= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 <= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 > stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 > stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 >= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 >= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 == stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 == stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_F32_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f32 /= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_I64_F32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 /= stack%v(stack%len_)%sca%f32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1

		! Mixed f64/i32 scalar arithmetic and comparisons.
		case (OP_ADD_F64_I32)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 + stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I32_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i32 + stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F64_I32)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 - stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I32_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i32 - stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F64_I32)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 * stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I32_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i32 * stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F64_I32)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 / stack%v(stack%len_)%sca%i32
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I32_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i32 / stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F64_I32)
			stack%v(stack%len_-1)%sca%f64 = mod(stack%v(stack%len_-1)%sca%f64, real(stack%v(stack%len_)%sca%i32, 8))
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I32_F64)
			stack%v(stack%len_-1)%sca%f64 = mod(real(stack%v(stack%len_-1)%sca%i32, 8), stack%v(stack%len_)%sca%f64)
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 < stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LT_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 < stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 <= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 <= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 > stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 > stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 >= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 >= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 == stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 == stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_F64_I32)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 /= stack%v(stack%len_)%sca%i32
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_I32_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i32 /= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1

		! Mixed f64/i64 scalar arithmetic and comparisons.
		case (OP_ADD_F64_I64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 + stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_ADD_I64_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i64 + stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_SUB_F64_I64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 - stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_SUB_I64_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i64 - stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_MUL_F64_I64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 * stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_MUL_I64_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i64 * stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_DIV_F64_I64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%f64 / stack%v(stack%len_)%sca%i64
			stack%len_ = stack%len_ - 1
		case (OP_DIV_I64_F64)
			stack%v(stack%len_-1)%sca%f64 = stack%v(stack%len_-1)%sca%i64 / stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_MOD_F64_I64)
			stack%v(stack%len_-1)%sca%f64 = mod(stack%v(stack%len_-1)%sca%f64, real(stack%v(stack%len_)%sca%i64, 8))
			stack%len_ = stack%len_ - 1
		case (OP_MOD_I64_F64)
			stack%v(stack%len_-1)%sca%f64 = mod(real(stack%v(stack%len_-1)%sca%i64, 8), stack%v(stack%len_)%sca%f64)
			stack%v(stack%len_-1)%type = f64_type
			stack%len_ = stack%len_ - 1
		case (OP_LT_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 < stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LT_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 < stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 <= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_LE_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 <= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 > stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GT_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 > stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 >= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_GE_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 >= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 == stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_EQ_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 == stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_F64_I64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%f64 /= stack%v(stack%len_)%sca%i64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1
		case (OP_NE_I64_F64)
			stack%v(stack%len_-1)%sca%bool = stack%v(stack%len_-1)%sca%i64 /= stack%v(stack%len_)%sca%f64
			stack%v(stack%len_-1)%type = bool_type; stack%len_ = stack%len_ - 1

		case default
			write(*,*) 'VM: unknown opcode ', instr%op
			call internal_error()

		end select

		end associate

		ip = next_ip

	end do

	! The final result is whatever is left on top of the stack.
	! Move rather than copy — the stack is local and discarded immediately.
	if (stack%len_ > 0) call value_move(stack%v(stack%len_), res)

	! Everything below is local to vm_run and about to fall out of scope:
	! the operand stack, the call frames (each holding two value_t pools), and
	! the two reusable arg pools.  All are arrays of value_t, and all of them
	! routinely hold live arrays/structs/strings in "dead" slots -- popped
	! values are not freed (vm_pop_discard) and recycled slots get retagged as
	! scalars without being freed (the typed-load fast paths).
	!
	! Letting them fall out of scope hands that whole nested tree to gfortran's
	! implicit deep deallocation, which this codebase does not rely on (see
	! value_array_destroy() in value.f90).  This runs on every vm_run() call --
	! i.e. every evaluated statement -- so it is the hottest instance of that
	! pattern in the interpreter.  Tear it all down explicitly.
	call value_array_destroy(stack%v)
	stack%len_ = 0
	stack%cap  = 0

	if (allocated(frames)) then
		do i = 1, size(frames)
			call value_array_destroy(frames(i)%caller_locs)
			call value_array_destroy(frames(i)%locs_buf)
		end do
		deallocate(frames)
	end if

	call value_array_destroy(params_pool)
	call value_array_destroy(iargs_pool)

end subroutine vm_run

!===============================================================================

end submodule syntran__vm_exec

!===============================================================================
