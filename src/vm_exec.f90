
!===============================================================================

submodule (syntran__vm_m) syntran__vm_exec

	! Stack-based bytecode VM execution loop.
	!
	! M0: OP_EVAL_NODE fallback (delegates to syntax_eval).
	! M1: native handlers for scalars, loads/stores, binop, unop.
	! M2: OP_JUMP / OP_JUMP_IF_FALSE for if/while/block/break/continue.
	! M3: OP_CALL / OP_RET / OP_LOAD_REF_* for user-defined function frames.

	implicit none

	!------------------------------------------------------------------------
	! Call frame: one entry per active function invocation.
	! caller_locs holds state%locs%vals saved via move_alloc at CALL time,
	! restored via move_alloc at RET time — mirrors eval_fn_call's locs0 pattern.
	!------------------------------------------------------------------------

	type :: frame_t
		integer :: return_ip = 0
		integer :: node_idx  = 0   ! index into prog%nodes for the fn_call_expr node
		type(value_t), allocatable :: caller_locs(:)
	end type frame_t

!===============================================================================

contains

!===============================================================================

! --- operand-stack helpers ----------------------------------------------------
!
! The operand stack is a value_vector_t.  For scalars (M1) both push_copy and
! the pop pattern are equivalent, but the names signal intent for future moves.

subroutine vm_push_copy(stack, val)
	! Push a deep copy of val onto the stack.  Used when the source must remain
	! live (LOAD_GLOBAL, LOAD_LOCAL, LOAD_CONST).
	type(value_vector_t), intent(inout) :: stack
	type(value_t), intent(in) :: val
	call stack%push(val)		! push_value does deep copy + growth
end subroutine vm_push_copy

!===============================================================================

subroutine vm_pop_copy(stack, val)
	! Pop the top of stack into val via deep copy, decrement len_.
	type(value_vector_t), intent(inout) :: stack
	type(value_t), intent(out) :: val
	val = stack%v(stack%len_)	! deep copy via value_copy
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

subroutine do_binop(left, right, op_kind, res)

	! Compute a binary operation on two values, mirroring eval_binary_expr.
	! The math routines (add, subtract, etc.) are available because
	! syntran__vm_m uses syntran__eval_m which uses syntran__math_m and
	! syntran__bool_m.

	type(value_t), intent(in) :: left, right
	integer, intent(in) :: op_kind
	type(value_t), intent(out) :: res

	!*******

	integer :: larrtype, rarrtype

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

	type(value_vector_t) :: stack
	type(value_t) :: left, right, val
	type(value_t), allocatable :: params_tmp(:)
	integer :: ip, next_ip
	integer :: i, fn_id, nparams, node_idx_call

	! Call-frame stack
	type(frame_t) :: frames(MAX_FRAMES)
	integer :: nframes

	nframes = 0
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
			call vm_push_copy(stack, val)
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
					if (allocated(fr%caller_locs)) then
						call move_alloc(fr%caller_locs, state%locs%vals)
					else if (allocated(state%locs%vals)) then
						deallocate(state%locs%vals)
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
					call vm_push_copy(stack, val)
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
		case (OP_STORE_GLOBAL)
			state%vars%vals(instr%a) = stack%v(stack%len_)

		case (OP_STORE_LOCAL)
			state%locs%vals(instr%a) = stack%v(stack%len_)

		! --- binary operation ---
		case (OP_BINOP)
			call vm_pop_copy(stack, right)
			call vm_pop_copy(stack, left)
			call do_binop(left, right, instr%a, val)
			call vm_push_copy(stack, val)

		! --- unary operation ---
		case (OP_UNOP)
			call vm_pop_copy(stack, right)
			call do_unop(right, instr%a, val)
			call vm_push_copy(stack, val)

		! --- discard ---
		case (OP_POP)
			call vm_pop_discard(stack)

		! --- control flow: unconditional jump ---
		case (OP_JUMP)
			next_ip = instr%a

		! --- control flow: conditional jump ---
		case (OP_JUMP_IF_FALSE)
			call vm_pop_copy(stack, val)
			if (.not. val%sca%bool) next_ip = instr%a

		! --- by-ref arg loading: move value from variable slot onto stack -----
		! The original slot is left in a valid-but-empty state; the value is
		! written back from the callee's frame at OP_RET time.
		case (OP_LOAD_REF_GLOBAL)
			call value_move(state%vars%vals(instr%a), val)
			call vm_push_copy(stack, val)

		case (OP_LOAD_REF_LOCAL)
			call value_move(state%locs%vals(instr%a), val)
			call vm_push_copy(stack, val)

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

			! Push a new call frame; save caller's local vars.
			nframes = nframes + 1
			frames(nframes)%return_ip = ip + 1
			frames(nframes)%node_idx  = node_idx_call
			if (allocated(state%locs%vals)) then
				call move_alloc(state%locs%vals, frames(nframes)%caller_locs)
			end if

			! Allocate the callee's local variable array.
			allocate(state%locs%vals(cn%num_locs))

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

			! Restore caller's local variable array.
			if (allocated(fr%caller_locs)) then
				call move_alloc(fr%caller_locs, state%locs%vals)
			else if (allocated(state%locs%vals)) then
				deallocate(state%locs%vals)
			end if

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
			end associate
			nframes = nframes - 1
			deallocate(params_tmp)

			! Push the return value onto the caller's operand stack.
			call vm_push_copy(stack, val)

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
