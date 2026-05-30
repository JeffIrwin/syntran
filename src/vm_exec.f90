
!===============================================================================

submodule (syntran__vm_m) syntran__vm_exec

	! Stack-based bytecode VM execution loop.
	!
	! M0: OP_EVAL_NODE fallback (delegates to syntax_eval).
	! M1: native handlers for scalars, loads/stores, binop, unop.

	implicit none

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

	type(value_vector_t) :: stack
	type(value_t) :: left, right, val
	integer :: ip

	stack = new_value_vector()

	do ip = prog%entry_main, prog%len_

		associate(instr => prog%code(ip))

		select case (instr%op)

		! --- fallback: AST walker ---
		case (OP_EVAL_NODE)
			call syntax_eval(prog%nodes(instr%a), state, val)
			call vm_push_copy(stack, val)

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

		case default
			write(*,*) 'VM: unknown opcode ', instr%op

		end select

		end associate

	end do

	! The final result is whatever is left on top of the stack.
	if (stack%len_ > 0) res = stack%v(stack%len_)

end subroutine vm_run

!===============================================================================

end submodule syntran__vm_exec

!===============================================================================
