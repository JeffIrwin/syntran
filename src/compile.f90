
!===============================================================================

module syntran__compile_m

	! Bytecode compiler: lowers an AST (syntax_node_t) into a program_t.
	!
	! Module + submodule layout mirrors eval.f90 + eval_*.f90.
	! Submodules:
	!   compile_ctrl.f90  - compile_tree entry point and control flow

	use syntran__bytecode_m
	use syntran__eval_m

	implicit none

!===============================================================================

	interface

		! Implemented in compile_ctrl.f90

		module subroutine compile_tree(tree, prog, fns)
			type(syntax_node_t), intent(in) :: tree
			type(program_t), intent(out) :: prog

			! REPL support: fns declared on an earlier REPL line (c.f.
			! compiler_state_t%fns in bytecode.f90).  Omit for a
			! whole-file/string compile, where every fn already has a
			! fn_declaration node in the tree being compiled
			type(fns_t), intent(in), target, optional :: fns
		end subroutine

	end interface

!===============================================================================

end module syntran__compile_m

!===============================================================================
