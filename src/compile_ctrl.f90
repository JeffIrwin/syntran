
!===============================================================================

submodule (syntran__compile_m) syntran__compile_ctrl

	! Bytecode compiler: AST -> bytecode lowering.
	!
	! M0: single OP_EVAL_NODE fallback for any unsupported node.
	! M1: natively compiles literal_expr, simple name_expr, binary_expr,
	!     unary_expr, let_expr, simple assignment_expr, and translation_unit.
	!     Everything else still falls back to OP_EVAL_NODE.

	implicit none

!===============================================================================

contains

!===============================================================================

recursive subroutine compile_node(prog, node)

	! Lower one AST node to opcodes.  The contract is that this subroutine
	! always leaves exactly one value on the operand stack after the emitted
	! opcodes execute — even for the OP_EVAL_NODE fallback.

	type(program_t), intent(inout) :: prog
	type(syntax_node_t), intent(in) :: node

	!*******

	integer :: idx, i, const_idx
	logical :: first

	select case (node%kind)

	! ---- literals --------------------------------------------------------------
	case (literal_expr)
		const_idx = add_const(prog, node%val)
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- variable reads --------------------------------------------------------
	case (name_expr)
		if (allocated(node%lsubscripts)) then
			! Subscripted access — fall back to the AST walker for now
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
		else
			if (node%is_loc) then
				call emit(prog, OP_LOAD_LOCAL, a = node%id_index)
			else
				call emit(prog, OP_LOAD_GLOBAL, a = node%id_index)
			end if
		end if

	! ---- arithmetic / comparison / logic / bitwise ----------------------------
	case (binary_expr)
		call compile_node(prog, node%left )
		call compile_node(prog, node%right)
		call emit(prog, OP_BINOP, a = node%op%kind)

	case (unary_expr)
		call compile_node(prog, node%right)
		call emit(prog, OP_UNOP, a = node%op%kind)

	! ---- variable declarations ------------------------------------------------
	case (let_expr)
		call compile_node(prog, node%right)
		if (node%is_loc) then
			call emit(prog, OP_STORE_LOCAL,  a = node%id_index)
		else
			call emit(prog, OP_STORE_GLOBAL, a = node%id_index)
		end if

	! ---- simple scalar assignment (no subscripts, no dot members, plain =) ----
	case (assignment_expr)
		if (.not. allocated(node%member)      .and. &
		    .not. allocated(node%lsubscripts) .and. &
		    node%op%kind == equals_token) then

			call compile_node(prog, node%right)
			if (node%is_loc) then
				call emit(prog, OP_STORE_LOCAL,  a = node%id_index)
			else
				call emit(prog, OP_STORE_GLOBAL, a = node%id_index)
			end if

		else
			! Compound assignment or subscript / dot LHS — fall back
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
		end if

	! ---- top-level translation unit -------------------------------------------
	case (translation_unit)
		! Fn and struct declarations are registered at parse time; skip them here.
		! Each statement contributes to the result: the last one leaves its value
		! on the stack; preceding ones are discarded with OP_POP.
		! This matches eval_translation_unit which takes the last member's result.
		first = .true.
		do i = 1, size(node%members)
			if (node%members(i)%kind == fn_declaration    ) cycle
			if (node%members(i)%kind == struct_declaration) cycle
			if (.not. first) call emit(prog, OP_POP)
			first = .false.
			call compile_node(prog, node%members(i))
		end do

		! If all members were fn/struct declarations, nothing was emitted and
		! the stack is empty.  Emit a LOAD_CONST of an unknown_type sentinel so
		! vm_run always has one value to return (the REPL cycles on unknown_type).
		if (first) then
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

	! ---- fallback: delegate to the AST walker ---------------------------------
	case default
		idx = add_node(prog, node)
		call emit(prog, OP_EVAL_NODE, a = idx)

	end select

end subroutine compile_node

!===============================================================================

function unknown_val() result(v)
	! Return a value_t with type = unknown_type (used as an empty-result sentinel).
	type(value_t) :: v
	v%type = unknown_type
end function unknown_val

!===============================================================================

module subroutine compile_tree(tree, prog)

	type(syntax_node_t), intent(in) :: tree
	type(program_t), intent(out) :: prog

	prog = new_program()
	call compile_node(prog, tree)

end subroutine compile_tree

!===============================================================================

end submodule syntran__compile_ctrl

!===============================================================================
