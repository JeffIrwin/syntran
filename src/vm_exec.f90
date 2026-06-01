
!===============================================================================

submodule (syntran__vm_m) syntran__vm_exec

	! Stack-based bytecode VM execution loop.
	!
	! M0: OP_EVAL_NODE fallback (delegates to syntax_eval; kept as debug opcode).
	! M1: native handlers for scalars, loads/stores, binop, unop.
	! M2: OP_JUMP / OP_JUMP_IF_FALSE for if/while/block/break/continue.
	! M3: OP_CALL / OP_RET / OP_LOAD_REF_* for user-defined function frames.
	! M8: OP_FOR_SETUP / OP_FOR_NEXT (for loops), OP_NEW_ARRAY, OP_STORE_SLICE,
	!     OP_HALT; readln/close handled inline in OP_CALL_INTR.

	implicit none

	!------------------------------------------------------------------------
	! Call frame: one entry per active function invocation.
	! caller_locs holds state%locs%vals saved via move_alloc at CALL time,
	! restored via move_alloc at RET time — mirrors eval_fn_call's locs0 pattern.
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
	end type frame_t

	!------------------------------------------------------------------------
	! For-loop iterator frame: one entry per active native for loop.
	! Mirrors the local variables in eval_for_statement (eval_control.f90:14).
	! for_kind uses the same constants as eval_for_statement: bound_array,
	! step_array, len_array, expl_array, size_array, unif_array, array_expr,
	! or str_type for string iteration.
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
	end type for_iter_t

!===============================================================================

contains

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
	call move_alloc(tmp, stack%v)
end subroutine vm_stack_grow

!===============================================================================

subroutine do_compound(lhs, rhs, op_kind)

	! Call compound_assign with just an integer op_kind (no full token needed).
	! op%text is only used for error messages, so an empty string is safe.

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in)    :: rhs
	integer, intent(in)          :: op_kind

	!*******

	type(syntax_token_t) :: op_tok

	op_tok%kind = op_kind
	op_tok%text = ''
	call compound_assign(lhs, rhs, op_tok)

end subroutine do_compound

!===============================================================================

subroutine do_binop(left, right, op_kind, restype, res)

	! Compute a binary operation on two values, mirroring eval_binary_expr.
	! restype: pre-computed result type from the compiler (node%val%type).
	!   When restype /= unknown_type the expensive get_binary_op_kind call is
	!   skipped.  The compiler always supplies this for OP_BINOP (instr%b).
	! The math routines (add, subtract, etc.) are available because
	! syntran__vm_m uses syntran__eval_m which uses syntran__math_m and
	! syntran__bool_m.

	type(value_t), intent(in) :: left, right
	integer, intent(in) :: op_kind, restype
	type(value_t), intent(out) :: res

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
		case (i32_type); res%array%bool = left%array%i32 == right%array%i32
		case (i64_type); res%array%bool = left%array%i64 == right%array%i64
		case (f32_type); res%array%bool = left%array%f32 == right%array%f32
		case (f64_type); res%array%bool = left%array%f64 == right%array%f64
		end select
	case (bang_equals_token)
		res%array = mold(left%array, bool_type)
		select case (elem_type)
		case (i32_type); res%array%bool = left%array%i32 /= right%array%i32
		case (i64_type); res%array%bool = left%array%i64 /= right%array%i64
		case (f32_type); res%array%bool = left%array%f32 /= right%array%f32
		case (f64_type); res%array%bool = left%array%f64 /= right%array%f64
		end select

	end select

end subroutine do_array_binop_typed

!===============================================================================

subroutine do_unop(right, op_kind, res)

	! Compute a unary operation, mirroring eval_unary_expr.

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

	integer, parameter :: MAX_FRAMES = 256
	integer, parameter :: MAX_FORS   = 64

	type(value_vector_t) :: stack
	type(value_t) :: left, right, val
	type(value_t), allocatable :: params_tmp(:)
	integer :: ip, next_ip
	integer :: i, fn_id, nparams, node_idx_call
	integer :: id, type_, n_mem
	integer(kind = 8) :: i8

	! M6: temporary arg array for OP_CALL_INTR native dispatch
	type(value_t), allocatable :: iargs(:)
	integer :: nintr

	! Call-frame stack
	type(frame_t) :: frames(MAX_FRAMES)
	integer :: nframes

	! For-loop iterator stack (M8)
	type(for_iter_t) :: for_iters(MAX_FORS)
	integer :: nfor

	!print *, "starting vm_run()"

	nframes = 0
	nfor    = 0
	stack   = new_value_vector()

	ip = prog%entry_main
	do while (ip <= prog%len_)

		! Default: advance to next instruction.  Jump handlers override this.
		next_ip = ip + 1

		associate(instr => prog%code(ip))

		select case (instr%op)

		! --- fallback: AST walker ---
		case (OP_EVAL_NODE)
			call syntax_eval(prog%nodes(instr%a), state, val)
			call vm_push_move(stack, val)
			if (state%returned) then
				if (nframes > 0) then
					! A return_statement executed inside an AST-walker fallback
					! (e.g. a for loop) while we're inside a compiled function
					! frame.  Perform the OP_RET cleanup inline so the frame is
					! correctly unwound.
					call vm_pop_copy(stack, val)   ! TOS = return value

					! Move by-ref params back and restore caller locs
					associate(fr => frames(nframes), &
					          cn => prog%nodes(frames(nframes)%node_idx))
					nparams = size(cn%params)
					allocate(params_tmp(nparams))
					do i = 1, nparams
						if (cn%is_ref(i)) then
							call value_move(state%locs%vals(cn%params(i)), params_tmp(i))
						end if
					end do
					! Save callee locs to pool, then restore caller locs.
					if (allocated(state%locs%vals)) then
						call move_alloc(state%locs%vals, frames(nframes)%locs_buf)
					end if
					if (allocated(fr%caller_locs)) then
						call move_alloc(fr%caller_locs, state%locs%vals)
					end if
					do i = 1, nparams
						if (.not. cn%is_ref(i)) cycle
						if (cn%args(i)%is_loc) then
							call value_move(params_tmp(i), &
								state%locs%vals(cn%args(i)%id_index))
						else
							call value_move(params_tmp(i), &
								state%vars%vals(cn%args(i)%id_index))
						end if
					end do
					next_ip = fr%return_ip
					end associate
					nframes = nframes - 1
					deallocate(params_tmp)
					state%returned = .false.
					call vm_push_move(stack, val)
				else
					! Top-level return: exit the main VM loop.
					next_ip = prog%len_ + 1
				end if
			end if

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
			call do_binop(left, right, instr%a, instr%b, val)
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
		!   [by-value args in param index order] [by-ref args in param index order]
		! Both groups in ascending param-index order.
		! We pop by-ref first (they're on top) in reverse index order, then by-val.
		case (OP_CALL)
			fn_id        = instr%a
			node_idx_call = instr%b
			associate(cn => prog%nodes(node_idx_call))
			nparams = 0
			if (allocated(cn%params)) nparams = size(cn%params)
			allocate(params_tmp(nparams))

			! Pop by-ref args in reverse param-index order.
			do i = nparams, 1, -1
				if (cn%is_ref(i)) call vm_pop_copy(stack, params_tmp(i))
			end do

			! Pop by-val args in reverse param-index order.
			do i = nparams, 1, -1
				if (.not. cn%is_ref(i)) call vm_pop_copy(stack, params_tmp(i))
			end do

			! Push a new call frame; save caller's local vars and for-iter depth.
			nframes = nframes + 1
			frames(nframes)%return_ip  = ip + 1
			frames(nframes)%nfor_saved = nfor
			frames(nframes)%node_idx  = node_idx_call
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
					deallocate(frames(nframes)%locs_buf)
					allocate(state%locs%vals(cn%num_locs))
				end if
			else
				! First call at this depth: allocate fresh (default-init = unknown_type).
				allocate(state%locs%vals(cn%num_locs))
			end if

			! Move params into the callee's local slots.
			do i = 1, nparams
				call value_move(params_tmp(i), state%locs%vals(cn%params(i)))
			end do

			deallocate(params_tmp)
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
			allocate(params_tmp(nparams))

			! Move by-ref params from callee locs into params_tmp for writeback.
			do i = 1, nparams
				if (cn%is_ref(i)) then
					call value_move(state%locs%vals(cn%params(i)), params_tmp(i))
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
				if (cn%args(i)%is_loc) then
					call value_move(params_tmp(i), &
						state%locs%vals(cn%args(i)%id_index))
				else
					call value_move(params_tmp(i), &
						state%vars%vals(cn%args(i)%id_index))
				end if
			end do

			next_ip = fr%return_ip
			nfor    = fr%nfor_saved   ! clean up any for-iters the callee leaked (e.g. via return)
			end associate
			nframes = nframes - 1
			deallocate(params_tmp)

			! Push the return value onto the caller's operand stack.
			call vm_push_move(stack, val)

		! --- scalar subscript read: a[i] or s[i] -----------------------------------
		! Evaluates subscript indices via subscript_eval (which calls syntax_eval
		! on each index expression), then reads the element with get_val.
		! For strings: extracts a single character.
		case (OP_INDEX)
			associate(n => prog%nodes(instr%a))
			id = n%id_index
			if (n%is_loc) then
				type_ = state%locs%vals(id)%type
			else
				type_ = state%vars%vals(id)%type
			end if

			i8 = subscript_eval(n, state)

			if (type_ == str_type) then
				val%type = str_type
				if (.not. allocated(val%str)) allocate(val%str)
				if (n%is_loc) then
					val%str%s = state%locs%vals(id)%str%s(i8+1: i8+1)
				else
					val%str%s = state%vars%vals(id)%str%s(i8+1: i8+1)
				end if
			else
				if (n%is_loc) then
					call get_val(n, state%locs%vals(id), state, val, index_ = i8)
				else
					call get_val(n, state%vars%vals(id), state, val, index_ = i8)
				end if
			end if

			call vm_push_move(stack, val)
			end associate

		! --- slice / non-scalar subscript read: a[i:j], a[:], a[[0,2,4]] ---------
		! Delegates to eval_name_expr which handles all slice kinds, array
		! subscripts, step subscripts, and multi-rank combinations.
		case (OP_SLICE)
			call eval_name_expr(prog%nodes(instr%a), state, val)
			call vm_push_move(stack, val)

		! --- scalar subscript write: a[i] = x  or  a[i] += x -------------------
		! TOS holds the already-evaluated RHS.  Uses subscript_eval to find the
		! linear index, reads the current element, applies the compound op, stores
		! back.  For strings: direct character replacement (only plain = is valid).
		case (OP_STORE_IDX)
			call vm_pop_copy(stack, right)   ! RHS (already evaluated by compiler)
			associate(n => prog%nodes(instr%a))
			id = n%id_index
			if (n%is_loc) then
				type_ = state%locs%vals(id)%type
			else
				type_ = state%vars%vals(id)%type
			end if

			i8 = subscript_eval(n, state)

			if (type_ == str_type) then
				! String character assignment: s[i] = char_expr
				if (n%is_loc) then
					state%locs%vals(id)%str%s(i8+1: i8+1) = right%str%s
				else
					state%vars%vals(id)%str%s(i8+1: i8+1) = right%str%s
				end if
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
				call vm_push_move(stack, val)
			end if
			end associate

		! --- struct instance construction -----------------------------------------
		! M5: pops nmembers values from stack in reverse order, builds a struct
		! value_t (struct_name from the stored node), and pushes the result.
		case (OP_MAKE_STRUCT)
			associate(sn => prog%nodes(instr%a))
			n_mem = size(sn%members)
			val%type = struct_type
			if (allocated(sn%struct_name)) val%struct_name = sn%struct_name
			if (allocated(val%struct)) deallocate(val%struct)
			allocate(val%struct(n_mem))
			do i = n_mem, 1, -1
				call vm_pop_copy(stack, val%struct(i))
			end do
			call vm_push_move(stack, val)
			end associate

		! --- dot member read ------------------------------------------------------
		! M5: calls get_val with the stored dot_expr node to handle simple,
		! nested, and subscripted member access chains.
		case (OP_LOAD_MEMBER)
			associate(n => prog%nodes(instr%a))
			id = n%id_index
			if (n%is_loc) then
				call get_val(n, state%locs%vals(id), state, val)
			else
				call get_val(n, state%vars%vals(id), state, val)
			end if
			call vm_push_move(stack, val)
			end associate

		! --- dot member write -----------------------------------------------------
		! M5: pops RHS from stack, reads current member via get_val, applies the
		! compound op, writes back via set_val, and pushes the new member value.
		case (OP_STORE_MEMBER)
			call vm_pop_copy(stack, right)
			associate(n => prog%nodes(instr%a))
			id = n%id_index
			if (n%is_loc) then
				call get_val(n, state%locs%vals(id), state, val)
				call do_compound(val, right, instr%b)
				call set_val(n, state%locs%vals(id), state, val)
			else
				call get_val(n, state%vars%vals(id), state, val)
				call do_compound(val, right, instr%b)
				call set_val(n, state%vars%vals(id), state, val)
			end if
			call vm_push_move(stack, val)
			end associate

		! --- M6: intrinsic function call -----------------------------------------
		! Native mode: pop args from stack, dispatch by intr_id.
		! readln/close: inline handling with slot writeback via instr%c.
		case (OP_CALL_INTR)
			nintr = instr%b
			allocate(iargs(nintr))
			do i = nintr, 1, -1
				call vm_pop_copy(stack, iargs(i))
			end do

			if (instr%a == INTR_READLN) then
				! readln: read a line from the file; set eof flag on the orig slot.
				block
				integer :: slot_id_, io_
				logical :: is_loc_
				slot_id_ = int(instr%c / 2)
				is_loc_  = (mod(instr%c, 2_8) == 1_8)
				if (.not. iargs(1)%file_%is_open) then
					write(*,*) err_rt_prefix//'readln() was called for file "' &
						//iargs(1)%file_%name_//'" which is not open'
					call internal_error()
				end if
				if (.not. iargs(1)%file_%mode_read) then
					write(*,*) err_rt_prefix//'readln() was called for file "' &
						//iargs(1)%file_%name_//'" which was not opened in read mode "r"'
					call internal_error()
				end if
				val%type = str_type
				if (.not. allocated(val%str)) allocate(val%str)
				val%str%s = read_line(iargs(1)%file_%unit_, io_)
				if (io_ == iostat_end) then
					if (is_loc_) then
						state%locs%vals(slot_id_)%file_%eof = .true.
					else
						state%vars%vals(slot_id_)%file_%eof = .true.
					end if
				else if (io_ /= 0 .and. io_ /= iostat_eor) then
					write(*,*) err_rt_prefix//'cannot readln() from file "' &
						//iargs(1)%file_%name_//'"'
					call internal_error()
				end if
				end block

			else if (instr%a == INTR_CLOSE) then
				! close: close the file unit; set is_open=false on the orig slot.
				block
				integer :: slot_id_
				logical :: is_loc_
				slot_id_ = int(instr%c / 2)
				is_loc_  = (mod(instr%c, 2_8) == 1_8)
				if (.not. iargs(1)%file_%is_open) then
					write(*,*) err_rt_prefix//'close() was called for file "' &
						//iargs(1)%file_%name_//'" which is not open'
					call internal_error()
				end if
				if (is_loc_) then
					state%locs%vals(slot_id_)%file_%is_open = .false.
				else
					state%vars%vals(slot_id_)%file_%is_open = .false.
				end if
				close(iargs(1)%file_%unit_)
				val%type = unknown_type
				end block

			else
				call vm_call_intr(instr%a, nintr, iargs, state, val)
			end if

			deallocate(iargs)
			call vm_push_move(stack, val)

		! --- M8: for-loop setup ---------------------------------------------------
		! Evaluates loop bounds / computes len8; pushes a for_iter_t onto the
		! for-iterator stack.  Mirrors eval_for_statement setup (eval_control.f90:31-205).
		case (OP_FOR_SETUP)
			block
			integer :: fi, rk_
			type(value_t) :: tmp_

			nfor = nfor + 1
			fi = nfor
			for_iters(fi)%node_idx = instr%a
			for_iters(fi)%counter  = 0
			for_iters(fi)%len8     = 0
			for_iters(fi)%itr_type = unknown_type

			associate(nd => prog%nodes(instr%a))

			if (allocated(nd%array%lbound)) &
				call syntax_eval(nd%array%lbound, state, for_iters(fi)%lbound_)
			if (allocated(nd%array%step  )) &
				call syntax_eval(nd%array%step,   state, for_iters(fi)%step   )
			if (allocated(nd%array%ubound)) &
				call syntax_eval(nd%array%ubound, state, for_iters(fi)%ubound_)
			if (allocated(nd%array%len_  )) &
				call syntax_eval(nd%array%len_,   state, for_iters(fi)%len_   )

			select case (nd%array%kind)
			case (array_expr)
				for_iters(fi)%for_kind = nd%array%val%array%kind

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
						write(*,*) err_int_prefix//'unit step array type not implemented'//color_reset
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
							write(*,*) err_rt_prefix//'for loop step is 0'//color_reset
							call internal_error()
						end if
						for_iters(fi)%len8 = ( &
							for_iters(fi)%ubound_%sca%i32 - for_iters(fi)%lbound_%sca%i32 &
							+ for_iters(fi)%step%sca%i32  &
							- sign(1, for_iters(fi)%step%sca%i32) ) / for_iters(fi)%step%sca%i32
					case (i64_type)
						if (for_iters(fi)%step%sca%i64 == 0) then
							write(*,*) err_rt_prefix//'for loop step is 0'//color_reset
							call internal_error()
						end if
						for_iters(fi)%len8 = ( &
							for_iters(fi)%ubound_%sca%i64 - for_iters(fi)%lbound_%sca%i64 &
							+ for_iters(fi)%step%sca%i64  &
							- sign(int(1,8), for_iters(fi)%step%sca%i64) ) / for_iters(fi)%step%sca%i64
					case (f32_type)
						if (for_iters(fi)%step%sca%f32 == 0.0) then
							write(*,*) err_rt_prefix//'for loop step is 0.0'//color_reset
							call internal_error()
						end if
						for_iters(fi)%len8 = ceiling( &
							(for_iters(fi)%ubound_%sca%f32 - for_iters(fi)%lbound_%sca%f32) &
							/ for_iters(fi)%step%sca%f32)
					case (f64_type)
						if (for_iters(fi)%step%sca%f64 == 0.0d0) then
							write(*,*) err_rt_prefix//'for loop step is 0.0'//color_reset
							call internal_error()
						end if
						for_iters(fi)%len8 = ceiling( &
							(for_iters(fi)%ubound_%sca%f64 - for_iters(fi)%lbound_%sca%f64) &
							/ for_iters(fi)%step%sca%f64)
					case default
						write(*,*) err_int_prefix//'step array type not implemented'//color_reset
						call internal_error()
					end select

				case (len_array)
					for_iters(fi)%itr_type = nd%array%val%array%type
					select case (for_iters(fi)%itr_type)
					case (f32_type, f64_type)
						for_iters(fi)%len8 = for_iters(fi)%len_%to_i64()
					case default
						write(*,*) err_int_prefix//'bound/len array type not implemented'//color_reset
						call internal_error()
					end select

				case (expl_array)
					for_iters(fi)%len8 = nd%array%val%array%len_

				case (size_array)
					rk_ = size(nd%array%size)
					for_iters(fi)%len8 = 1
					do i = 1, rk_
						call syntax_eval(nd%array%size(i), state, tmp_)
						for_iters(fi)%len8 = for_iters(fi)%len8 * tmp_%to_i64()
					end do

				case (unif_array)
					rk_ = size(nd%array%size)
					for_iters(fi)%len8 = 1
					do i = 1, rk_
						call syntax_eval(nd%array%size(i), state, tmp_)
						for_iters(fi)%len8 = for_iters(fi)%len8 * tmp_%to_i64()
					end do

				case default
					write(*,*) err_int_prefix//'for loop: unknown array kind'//color_reset
					call internal_error()
				end select

			case default
				! Non-primary array expression
				if (nd%array%val%type == str_type) then
					for_iters(fi)%for_kind = str_type
					for_iters(fi)%itr_type = str_type
					call syntax_eval(nd%array, state, tmp_)
					call value_move(tmp_, for_iters(fi)%str_)
					for_iters(fi)%len8 = len(for_iters(fi)%str_%str%s, 8)
				else
					for_iters(fi)%for_kind = array_expr
					call syntax_eval(nd%array, state, tmp_)
					for_iters(fi)%array = tmp_%array
					for_iters(fi)%len8  = for_iters(fi)%array%len_
				end if
			end select

			end associate
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
				call array_at(val, for_iters(fi)%for_kind, for_iters(fi)%counter, &
					for_iters(fi)%lbound_, for_iters(fi)%step, for_iters(fi)%ubound_, &
					for_iters(fi)%len_, for_iters(fi)%array, &
					prog%nodes(for_iters(fi)%node_idx)%array%elems, for_iters(fi)%str_, &
					state)
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
		case (OP_NEW_ARRAY)
			call eval_array_expr(prog%nodes(instr%a), state, val)
			call vm_push_move(stack, val)

		! --- M8: slice/complex LHS assignment ------------------------------------
		! Handles slice-range LHS (a[1:3] = x) and subscript-less compound
		! assignments by delegating to eval_assignment_expr.
		case (OP_STORE_SLICE)
			call eval_assignment_expr(prog%nodes(instr%a), state, val)
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

		case default
			write(*,*) 'VM: unknown opcode ', instr%op

		end select

		end associate

		ip = next_ip

	end do

	! The final result is whatever is left on top of the stack.
	if (stack%len_ > 0) res = stack%v(stack%len_)

end subroutine vm_run

!===============================================================================

end submodule syntran__vm_exec

!===============================================================================
