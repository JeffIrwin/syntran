
!===============================================================================

submodule (syntran__compile_m) syntran__compile_ctrl

	! M0 compiler: compiles the entire translation_unit as a single OP_EVAL_NODE.
	! Subsequent milestones (M1+) will replace OP_EVAL_NODE emissions with real
	! opcode sequences as each node kind is lowered.

	implicit none

!===============================================================================

contains

!===============================================================================

module subroutine compile_tree(tree, prog)

	type(syntax_node_t), intent(in) :: tree
	type(program_t), intent(out) :: prog

	!*******

	integer :: idx

	prog = new_program()

	! M0: store the whole tree in the node pool and emit one OP_EVAL_NODE.
	! The VM will call syntax_eval on it directly.
	idx = add_node(prog, tree)
	call emit(prog, OP_EVAL_NODE, a = idx)

end subroutine compile_tree

!===============================================================================

end submodule syntran__compile_ctrl

!===============================================================================
