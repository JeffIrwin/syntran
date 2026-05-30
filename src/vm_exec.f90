
!===============================================================================

submodule (syntran__vm_m) syntran__vm_exec

	! M0 VM execution loop.  For now the only opcode is OP_EVAL_NODE which
	! delegates to the AST-walking syntax_eval.  Subsequent milestones will add
	! real opcodes as each node kind is lowered in the compiler.

	implicit none

!===============================================================================

contains

!===============================================================================

module subroutine vm_run(prog, state, res)

	type(program_t), intent(in) :: prog
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!*******

	integer :: ip

	do ip = prog%entry_main, prog%len_

		select case (prog%code(ip)%op)

		case (OP_EVAL_NODE)

			! Delegate to the AST walker.  The result of the last OP_EVAL_NODE
			! becomes the program result, matching eval_translation_unit semantics.
			call syntax_eval(prog%nodes(prog%code(ip)%a), state, res)

		case default
			write(*,*) 'VM: unknown opcode ', prog%code(ip)%op

		end select

	end do

end subroutine vm_run

!===============================================================================

end submodule syntran__vm_exec

!===============================================================================
