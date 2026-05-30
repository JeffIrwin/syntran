
!===============================================================================

submodule (syntran__compile_m) syntran__compile_ctrl

	! Bytecode compiler: AST -> bytecode lowering.
	!
	! M0: single OP_EVAL_NODE fallback for any unsupported node.
	! M1: natively compiles literal_expr, simple name_expr, binary_expr,
	!     unary_expr, let_expr, simple assignment_expr, and translation_unit.
	!     Everything else still falls back to OP_EVAL_NODE.
	! M2: adds block_statement, if_statement, while_statement,
	!     break_statement, continue_statement via JUMP/JUMP_IF_FALSE opcodes.
	!     for_statement still falls back to OP_EVAL_NODE.

	implicit none

	! --- Loop context for break/continue backpatching ---
	!
	! loop_depth is incremented when entering a natively-compiled while/for loop
	! and decremented on exit.  continue_target(d) is the instruction index to
	! jump to for a `continue` inside loop depth d.  break fixups inside loops
	! are collected and backpatched after the loop body is fully compiled.

	integer, parameter :: MAX_LOOP_DEPTH   = 64
	integer, parameter :: MAX_BREAK_FIXUPS = 4096

	integer :: loop_depth = 0
	integer :: continue_target(MAX_LOOP_DEPTH) = 0

	integer :: break_fixup_ips(MAX_BREAK_FIXUPS)    = 0
	integer :: break_fixup_depths(MAX_BREAK_FIXUPS)  = 0
	integer :: nbreak_fixups = 0

	! --- Block-level break context ---
	!
	! In Syntran, `break` can exit a plain block statement (not just loops).
	! The AST walker propagates state%breaked through all enclosing blocks until
	! it reaches a for/while loop or the translation unit.
	!
	! In the VM, we replicate this by tracking whether we are inside the
	! outermost natively-compiled block (with no enclosing native loop).  If so,
	! `break` records a fixup that gets patched to the block's end when the
	! outermost block finishes compilation.  Inner blocks nested inside the
	! outermost block share the same target, so one JUMP exits all of them.

	logical :: in_block_break_ctx = .false.
	integer :: block_break_ips(MAX_BREAK_FIXUPS) = 0
	integer :: nblock_break_fixups = 0

	! Set to true while compiling a user function body; used to distinguish
	! a function-level return_statement (emit OP_RET) from a top-level one
	! (fall back to OP_EVAL_NODE).
	logical :: in_fn_body = .false.

!===============================================================================

contains

!===============================================================================

subroutine backpatch_breaks(prog, depth, target)

	! Patch all pending loop-break fixups at the given loop depth to target,
	! then remove them from the fixup list.

	type(program_t), intent(inout) :: prog
	integer, intent(in) :: depth, target

	!*******

	integer :: i, j

	j = 0
	do i = 1, nbreak_fixups
		if (break_fixup_depths(i) == depth) then
			call patch_jump(prog, break_fixup_ips(i), target)
		else
			j = j + 1
			break_fixup_ips(j)    = break_fixup_ips(i)
			break_fixup_depths(j) = break_fixup_depths(i)
		end if
	end do
	nbreak_fixups = j

end subroutine backpatch_breaks

!===============================================================================

recursive subroutine compile_node(prog, node)

	! Lower one AST node to opcodes.  The contract is that this subroutine
	! always leaves exactly one value on the operand stack after the emitted
	! opcodes execute — even for the OP_EVAL_NODE fallback.
	!
	! Exception: break_statement and continue_statement emit an unconditional
	! JUMP so the instructions after them are unreachable.  Stack discipline is
	! only required on live paths.

	type(program_t), intent(inout) :: prog
	type(syntax_node_t), intent(in) :: node

	!*******

	integer :: idx, i, const_idx
	integer :: jf_ip, j_ip, l_top, l_else, l_end, l_pad
	integer :: nblock_saved
	logical :: first, entering_ctx

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

	! ---- block statement -------------------------------------------------------
	!
	! Compiles all members sequentially; the last one leaves its result on the
	! stack.  When loop_depth == 0 and this is the outermost natively-compiled
	! block, `break` inside emits a JUMP that is patched to a pad at the block's
	! end.  The pad pushes unknown_type so the stack is D+1 on both the normal
	! and break-taken paths.
	!
	! Normal path:   [body, last result on stack] JUMP L_skip_pad
	!                L_pad: LOAD_CONST unknown    <- not reached on normal path
	!                L_skip_pad:
	!
	! Break path:    [body...] JUMP L_pad         <- from break_statement
	!                [unreachable body tail]
	!                JUMP L_skip_pad
	!                L_pad: LOAD_CONST unknown
	!                L_skip_pad:
	case (block_statement)
		! Enter the outermost block-break context if not already in one and
		! there is no enclosing native while/for loop.
		if (loop_depth == 0 .and. .not. in_block_break_ctx) then
			in_block_break_ctx = .true.
			entering_ctx = .true.
			nblock_saved = nblock_break_fixups
		else
			entering_ctx = .false.
			nblock_saved = 0		! unused, but avoids uninitialized warning
		end if

		! Compile members: pop between them, leave last result on stack
		first = .true.
		do i = 1, size(node%members)
			if (.not. first) call emit(prog, OP_POP)
			first = .false.
			call compile_node(prog, node%members(i))
		end do
		! Empty block: push unknown_type sentinel
		if (first) then
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

		! After the outermost block body: emit the break-taken sentinel pad
		! if any block-level break fixups were collected.
		if (entering_ctx) then
			if (nblock_break_fixups > nblock_saved) then
				! Normal path: skip the pad
				j_ip = prog%len_ + 1
				call emit(prog, OP_JUMP, a = 0)

				! Break path: push unknown_type as the block's result
				l_pad = prog%len_ + 1
				do i = nblock_saved + 1, nblock_break_fixups
					call patch_jump(prog, block_break_ips(i), l_pad)
				end do
				const_idx = add_const(prog, unknown_val())
				call emit(prog, OP_LOAD_CONST, a = const_idx)

				! Both paths converge here
				l_end = prog%len_ + 1
				call patch_jump(prog, j_ip, l_end)
			end if

			! Restore the block-break context for the enclosing scope
			nblock_break_fixups = nblock_saved
			in_block_break_ctx  = .false.
		end if

	! ---- if statement ----------------------------------------------------------
	! Bytecode pattern (no else):
	!   [condition]
	!   JUMP_IF_FALSE L_else
	!   [if-clause]          -> leaves result on stack
	!   JUMP L_end
	!   L_else: LOAD_CONST unknown_type
	!   L_end:
	!
	! Pattern (with else):
	!   [condition]
	!   JUMP_IF_FALSE L_else
	!   [if-clause]          -> leaves result on stack
	!   JUMP L_end
	!   L_else: [else-clause] -> leaves result on stack
	!   L_end:
	case (if_statement)
		! Compile condition; leaves bool on stack
		call compile_node(prog, node%condition)

		! JUMP_IF_FALSE — target patched below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_JUMP_IF_FALSE, a = 0)

		! Compile the if-clause
		call compile_node(prog, node%if_clause)

		if (allocated(node%else_clause)) then
			! JUMP past else — target patched below
			j_ip = prog%len_ + 1
			call emit(prog, OP_JUMP, a = 0)

			l_else = prog%len_ + 1
			call patch_jump(prog, jf_ip, l_else)

			! Compile the else-clause (may itself be another if_statement)
			call compile_node(prog, node%else_clause)

			l_end = prog%len_ + 1
			call patch_jump(prog, j_ip, l_end)
		else
			! No else: jump past the unknown_type push
			j_ip = prog%len_ + 1
			call emit(prog, OP_JUMP, a = 0)

			l_else = prog%len_ + 1
			call patch_jump(prog, jf_ip, l_else)

			! Condition was false and no else: push unknown_type
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)

			l_end = prog%len_ + 1
			call patch_jump(prog, j_ip, l_end)
		end if

	! ---- while statement -------------------------------------------------------
	! Bytecode pattern:
	!   L_top: [condition]
	!          JUMP_IF_FALSE L_end
	!          [body]         -> result discarded with POP
	!          POP
	!          JUMP L_top
	!   L_end: LOAD_CONST unknown_type  (while-statement result)
	!
	! break  -> JUMP L_end  (backpatched after loop)
	! continue -> JUMP L_top (target known when continue is emitted)
	case (while_statement)
		loop_depth = loop_depth + 1
		l_top = prog%len_ + 1
		continue_target(loop_depth) = l_top

		! Compile condition
		call compile_node(prog, node%condition)

		! JUMP_IF_FALSE — patched to L_end below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_JUMP_IF_FALSE, a = 0)

		! Compile body; its result is discarded before looping
		call compile_node(prog, node%body)
		call emit(prog, OP_POP)
		call emit(prog, OP_JUMP, a = l_top)

		! L_end: backpatch the conditional exit and all break fixups
		l_end = prog%len_ + 1
		call patch_jump(prog, jf_ip, l_end)
		call backpatch_breaks(prog, loop_depth, l_end)
		loop_depth = loop_depth - 1

		! Push unknown_type as the while-statement's result value
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- break -----------------------------------------------------------------
	case (break_statement)
		if (loop_depth > 0) then
			! Inside a native while/for: jump to the loop's end (backpatched)
			nbreak_fixups = nbreak_fixups + 1
			break_fixup_ips(nbreak_fixups)    = prog%len_ + 1
			break_fixup_depths(nbreak_fixups) = loop_depth
			call emit(prog, OP_JUMP, a = 0)

		else if (in_block_break_ctx) then
			! Inside the outermost native block (no enclosing loop): jump to
			! the block's end pad (backpatched when the block finishes)
			nblock_break_fixups = nblock_break_fixups + 1
			block_break_ips(nblock_break_fixups) = prog%len_ + 1
			call emit(prog, OP_JUMP, a = 0)

		else
			! No breakable context (top-level break): fall back to AST walker
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
		end if

	! ---- continue --------------------------------------------------------------
	case (continue_statement)
		if (loop_depth > 0) then
			call emit(prog, OP_JUMP, a = continue_target(loop_depth))
		else
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
		end if

	! ---- top-level translation unit -------------------------------------------
	case (translation_unit)
		! M3: compile all user fn bodies first into prog%code so that fn_entry
		! offsets are known before any call-site OP_CALL is emitted.  The VM
		! starts at prog%entry_main which is set after all fn bodies.

		! Pass 0: determine max fn id to size the entry/num_locs tables.
		do i = 1, size(node%members)
			if (node%members(i)%kind /= fn_declaration) cycle
			const_idx = node%members(i)%id_index   ! reusing const_idx as scratch
			if (.not. allocated(prog%fn_entry)) then
				allocate(prog%fn_entry(const_idx))
				allocate(prog%fn_num_locs(const_idx))
				prog%fn_entry    = 0
				prog%fn_num_locs = 0
			else if (const_idx > size(prog%fn_entry)) then
				! Grow tables (rare: fns are declared in order by the parser).
				block
					integer, allocatable :: tmp_entry(:), tmp_locs(:)
					call move_alloc(prog%fn_entry,    tmp_entry)
					call move_alloc(prog%fn_num_locs, tmp_locs)
					allocate(prog%fn_entry(const_idx))
					allocate(prog%fn_num_locs(const_idx))
					prog%fn_entry    = 0
					prog%fn_num_locs = 0
					prog%fn_entry(1: size(tmp_entry))    = tmp_entry
					prog%fn_num_locs(1: size(tmp_locs))  = tmp_locs
				end block
			end if
		end do

		! Pass 1: compile each fn declaration body.
		do i = 1, size(node%members)
			if (node%members(i)%kind /= fn_declaration) cycle
			l_top = node%members(i)%id_index   ! fn_id (reuse l_top as scratch)
			prog%fn_num_locs(l_top) = node%members(i)%num_locs
			prog%fn_entry(l_top)    = prog%len_ + 1

			in_fn_body = .true.
			call compile_node(prog, node%members(i)%body)
			in_fn_body = .false.
		end do

		! Top-level statements start here.
		prog%entry_main = prog%len_ + 1

		! Pass 2: compile top-level (non-fn, non-struct) statements.
		! Each contributes to the result: the last one leaves its value on the
		! stack; preceding ones are discarded with OP_POP.
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

	! ---- fn_declaration: bodies are compiled separately in translation_unit -----
	! Skip silently here; compilation of the body happens in the translation_unit
	! case before the top-level statements.
	case (fn_declaration)
		! Nothing to emit: fn bodies are pre-compiled by the translation_unit handler.

	! ---- user-defined function call -------------------------------------------
	! Only emit OP_CALL for functions compiled into this program_t (i.e. locally
	! declared functions whose fn_entry is known).  Module-imported functions and
	! any call whose fn_id lies outside the compiled range fall back to OP_EVAL_NODE
	! so the AST walker handles them via eval_fn_call.
	case (fn_call_expr)
		! Fortran .or./.and. are NOT short-circuit; use nested ifs to avoid
		! an out-of-bounds access on prog%fn_entry(node%id_index).
		first = .false.   ! reuse `first` as a "use_native" scratch bool
		if (allocated(prog%fn_entry)) then
			if (node%id_index <= size(prog%fn_entry)) then
				if (prog%fn_entry(node%id_index) /= 0) first = .true.
			end if
		end if

		if (.not. first) then
			! Not a locally-compiled fn: fall back to the AST walker.
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
		else
			! Locally-compiled fn: two-pass arg push + OP_CALL.
			! Store the call node so OP_CALL/OP_RET can access params/is_ref.
			idx = add_node(prog, node)

			! Pass 1: push by-value args onto stack.
			if (allocated(node%is_ref)) then
				do i = 1, size(node%is_ref)
					if (.not. node%is_ref(i)) call compile_node(prog, node%args(i))
				end do

				! Pass 2: move by-ref args from their variable slots onto stack.
				do i = 1, size(node%is_ref)
					if (node%is_ref(i)) then
						if (node%args(i)%is_loc) then
							call emit(prog, OP_LOAD_REF_LOCAL,  a = node%args(i)%id_index)
						else
							call emit(prog, OP_LOAD_REF_GLOBAL, a = node%args(i)%id_index)
						end if
					end if
				end do
			end if

			call emit(prog, OP_CALL, a = node%id_index, b = idx)
		end if

	! ---- intrinsic function call -----------------------------------------------
	! Delegate the entire call to the AST walker via OP_EVAL_NODE.  The walker
	! evaluates args using the current state (which has the callee's locs when
	! inside a compiled frame).  Migration to native integer dispatch is M6.
	case (fn_call_intr_expr)
		idx = add_node(prog, node)
		call emit(prog, OP_EVAL_NODE, a = idx)

	! ---- return statement ------------------------------------------------------
	! Inside a compiled function body: push return value (or unknown sentinel for
	! void) then emit OP_RET.  At top level: fall back to OP_EVAL_NODE (which
	! sets state%returned so the VM exits its main loop).
	case (return_statement)
		if (in_fn_body) then
			if (node%right%val%type == void_type) then
				! Void return: push unknown_type as a dummy return slot.
				const_idx = add_const(prog, unknown_val())
				call emit(prog, OP_LOAD_CONST, a = const_idx)
			else
				call compile_node(prog, node%right)
			end if
			call emit(prog, OP_RET)
		else
			! Top-level return: fall back to OP_EVAL_NODE which sets state%returned.
			idx = add_node(prog, node)
			call emit(prog, OP_EVAL_NODE, a = idx)
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

	! Reset all compiler state for each fresh compilation
	loop_depth          = 0
	nbreak_fixups       = 0
	in_block_break_ctx  = .false.
	nblock_break_fixups = 0
	in_fn_body          = .false.

	prog = new_program()
	call compile_node(prog, tree)

end subroutine compile_tree

!===============================================================================

end submodule syntran__compile_ctrl

!===============================================================================
