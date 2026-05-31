
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
	! M6: fn_call_intr_expr emits OP_CALL_INTR with integer intr_id dispatch.
	! M7: use_statement: module fn bodies compiled into prog before entry_main;
	!     module init code emitted inline at the use_statement site.
	! M8: for_statement (native FOR_SETUP/FOR_NEXT), array_expr (OP_NEW_ARRAY),
	!     slice/complex LHS assign (OP_STORE_SLICE), top-level return (OP_HALT),
	!     readln/close native dispatch with slot writeback; OP_EVAL_NODE fallback
	!     removed from compiler (kept as debug opcode in the VM handler).

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

subroutine ensure_fn_entry(prog, fn_id)

	! Grow prog%fn_entry / prog%fn_num_locs so that index fn_id is valid.
	! New slots are zeroed.  No-op if the tables are already large enough.

	type(program_t), intent(inout) :: prog
	integer, intent(in) :: fn_id

	!*******

	integer, allocatable :: tmp_entry(:), tmp_locs(:)

	if (.not. allocated(prog%fn_entry)) then
		allocate(prog%fn_entry(fn_id))
		allocate(prog%fn_num_locs(fn_id))
		prog%fn_entry    = 0
		prog%fn_num_locs = 0
	else if (fn_id > size(prog%fn_entry)) then
		call move_alloc(prog%fn_entry,    tmp_entry)
		call move_alloc(prog%fn_num_locs, tmp_locs)
		allocate(prog%fn_entry(fn_id))
		allocate(prog%fn_num_locs(fn_id))
		prog%fn_entry    = 0
		prog%fn_num_locs = 0
		prog%fn_entry(1: size(tmp_entry))    = tmp_entry
		prog%fn_num_locs(1: size(tmp_locs))  = tmp_locs
	end if

end subroutine ensure_fn_entry

!===============================================================================

recursive subroutine compile_module_fns(prog, module_node)

	! Compile all fn bodies from a module's translation_unit node into prog,
	! recording their entry points in prog%fn_entry.  Recursively handles any
	! nested use_statement members so that transitively-imported module fns are
	! also compiled.  Skips any fn whose fn_entry is already non-zero to guard
	! against diamond-dependency double-compilation.
	!
	! Two-sub-pass design so that forward references within the module resolve:
	!   Sub-pass A: register (ensure_fn_entry) ALL fns in this module first.
	!   Sub-pass B: compile each body; any call to a forward-declared sibling
	!               finds the id registered (in range) even if fn_entry == 0,
	!               and OP_CALL is emitted safely (entry will be filled before VM runs).

	type(program_t),     intent(inout) :: prog
	type(syntax_node_t), intent(in)    :: module_node   ! translation_unit

	!*******

	integer :: i, fn_id

	! Recurse into nested use_statements first so that their fns are available
	! to any bodies compiled below that call them.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind /= use_statement) cycle
		if (.not. allocated(module_node%members(i)%member)) cycle
		call compile_module_fns(prog, module_node%members(i)%member)
	end do

	! Sub-pass A: register all local fn ids before compiling any body.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind /= fn_declaration) cycle
		call ensure_fn_entry(prog, module_node%members(i)%id_index)
	end do

	! Sub-pass B: compile each fn_declaration body.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind /= fn_declaration) cycle
		fn_id = module_node%members(i)%id_index
		if (prog%fn_entry(fn_id) /= 0) cycle   ! already compiled (diamond dep)
		prog%fn_num_locs(fn_id) = module_node%members(i)%num_locs
		prog%fn_entry(fn_id)    = prog%len_ + 1
		in_fn_body = .true.
		call compile_node(prog, module_node%members(i)%body)
		in_fn_body = .false.
	end do

end subroutine compile_module_fns

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
			! Subscripted access: all-scalar → OP_INDEX, any range/step/all → OP_SLICE
			idx = add_node(prog, node)
			if (all(node%lsubscripts%sub_kind == scalar_sub)) then
				call emit(prog, OP_INDEX, a = idx)
			else
				call emit(prog, OP_SLICE, a = idx)
			end if
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

	! ---- assignment: dot member, scalar subscript, simple, or compound -------
	case (assignment_expr)
		if (allocated(node%member)) then
			! M5: dot member assignment: a.b = expr  or  a.b += expr
			call compile_node(prog, node%right)
			idx = add_node(prog, node)
			call emit(prog, OP_STORE_MEMBER, a = idx, b = node%op%kind)

		else if (.not. allocated(node%lsubscripts) .and. &
		         node%op%kind == equals_token) then
			! Simple plain assignment: a = expr
			call compile_node(prog, node%right)
			if (node%is_loc) then
				call emit(prog, OP_STORE_LOCAL,  a = node%id_index)
			else
				call emit(prog, OP_STORE_GLOBAL, a = node%id_index)
			end if

		else
			! Fortran .and. is NOT short-circuit; use nested ifs to guard the
			! all(lsubscripts%sub_kind) access from unallocated lsubscripts.
			first = .false.   ! reuse `first` as "use native subscript store"
			if (allocated(node%lsubscripts)) then
				if (all(node%lsubscripts%sub_kind == scalar_sub)) first = .true.
			end if

			if (first) then
				! Scalar subscript write (any op): a[i] = x  or  a[i] += x
				call compile_node(prog, node%right)
				idx = add_node(prog, node)
				call emit(prog, OP_STORE_IDX, a = idx, b = node%op%kind)
			else
				! Slice LHS or subscript-less compound: delegate to eval_assignment_expr
				idx = add_node(prog, node)
				call emit(prog, OP_STORE_SLICE, a = idx)
			end if
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
			! No breakable context: top-level break is a no-op in translation_unit
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

	! ---- continue --------------------------------------------------------------
	case (continue_statement)
		if (loop_depth > 0) then
			call emit(prog, OP_JUMP, a = continue_target(loop_depth))
		else
			! No loop context: top-level continue is a no-op in translation_unit
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

	! ---- top-level translation unit -------------------------------------------
	case (translation_unit)
		! All fn bodies (local + transitively-imported modules) are compiled
		! before entry_main so that call-site OP_CALL can reference fn_entry.
		! The VM starts at prog%entry_main.

		! Pass 0: grow fn_entry / fn_num_locs for locally-declared fns.
		do i = 1, size(node%members)
			if (node%members(i)%kind /= fn_declaration) cycle
			call ensure_fn_entry(prog, node%members(i)%id_index)
		end do

		! M7 pre-pass: compile fn bodies from all transitively-imported modules.
		! Must run before Pass 1 (local fns) so that cross-module calls resolve.
		do i = 1, size(node%members)
			if (node%members(i)%kind /= use_statement) cycle
			if (.not. allocated(node%members(i)%member)) cycle
			call compile_module_fns(prog, node%members(i)%member)
		end do

		! Pass 1: compile each locally-declared fn body.
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

	! ---- for statement --------------------------------------------------------
	! M8: for_statement lowering:
	!
	!   OP_FOR_SETUP a=node_idx    ! eval bounds once, push onto for-iter stack
	!   L_top:
	!   OP_FOR_NEXT  a=L_pop       ! counter++; if exhausted: jump L_pop; else write loop var
	!   [body]
	!   OP_POP                     ! discard body result before next iteration
	!   OP_JUMP a=L_top
	!   L_pop:
	!   OP_FOR_POP                 ! pop for-iter stack (all exits: exhaustion + break)
	!   LOAD_CONST unknown_type    ! for-statement result
	!
	! break    -> JUMP L_pop (backpatched; FOR_POP then LOAD_CONST execute)
	! continue -> JUMP L_top (continue_target; FOR_NEXT re-evaluates)
	case (for_statement)
		idx = add_node(prog, node)
		call emit(prog, OP_FOR_SETUP, a = idx)

		loop_depth = loop_depth + 1
		l_top = prog%len_ + 1
		continue_target(loop_depth) = l_top

		! FOR_NEXT — patched to L_pop below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_FOR_NEXT, a = 0)

		! Compile body; its result is discarded before the back edge
		call compile_node(prog, node%body)
		call emit(prog, OP_POP)
		call emit(prog, OP_JUMP, a = l_top)

		! L_pop: backpatch FOR_NEXT and all break fixups here, then pop + result
		l_end = prog%len_ + 1
		call patch_jump(prog, jf_ip, l_end)
		call backpatch_breaks(prog, loop_depth, l_end)
		loop_depth = loop_depth - 1
		call emit(prog, OP_FOR_POP)

		! Push unknown_type as the for-statement's result value
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- array expression construction ----------------------------------------
	! M8: Store the array_expr node in the pool and emit OP_NEW_ARRAY.
	! The VM handler calls eval_array_expr to build the value.
	case (array_expr)
		idx = add_node(prog, node)
		call emit(prog, OP_NEW_ARRAY, a = idx)

	! ---- struct instance construction -----------------------------------------
	! M5: Compile each member-initialiser expression in order, then emit
	! OP_MAKE_STRUCT.  The node is stored in the pool so the VM can recover
	! struct_name and nmembers; the member expressions themselves are bytecode.
	case (struct_instance_expr)
		do i = 1, size(node%members)
			call compile_node(prog, node%members(i))
		end do
		idx = add_node(prog, node)
		call emit(prog, OP_MAKE_STRUCT, a = idx)

	! ---- dot-expression (struct member read) -----------------------------------
	! M5: Store the dot_expr node in the pool so the VM can call get_val with
	! full chain information (nested dots, subscripted members, etc.).
	case (dot_expr)
		idx = add_node(prog, node)
		call emit(prog, OP_LOAD_MEMBER, a = idx)

	! ---- fn_declaration: bodies are compiled separately in translation_unit -----
	! Skip silently here; compilation of the body happens in the translation_unit
	! case before the top-level statements.
	case (fn_declaration)
		! Nothing to emit: fn bodies are pre-compiled by the translation_unit handler.

	! ---- use_statement (M7) ---------------------------------------------------
	! Module fn bodies were already compiled into prog%fn_entry by the
	! translate_unit pre-pass (compile_module_fns).  Here we only emit the
	! module's top-level initialisation code (e.g. `let count = 0;`).
	! Each init statement leaves one value on the stack; we discard all of them
	! with OP_POP, then push unknown_type as the use_statement's own result.
	case (use_statement)
		if (allocated(node%member)) then
			do i = 1, size(node%member%members)
				if (node%member%members(i)%kind == fn_declaration    ) cycle
				if (node%member%members(i)%kind == struct_declaration) cycle
				call compile_node(prog, node%member%members(i))
				call emit(prog, OP_POP)
			end do
		end if
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- user-defined function call -------------------------------------------
	! Only emit OP_CALL for functions compiled into this program_t (i.e. locally
	! declared functions whose fn_entry is known).  Module-imported functions and
	! any call whose fn_id lies outside the compiled range fall back to OP_EVAL_NODE
	! so the AST walker handles them via eval_fn_call.
	case (fn_call_expr)
		! Check registration only: fn_entry[id] may be 0 for a forward reference
		! (the fn is declared after this call site in the source).  That is fine —
		! fn_entry[id] will be non-zero by the time the VM executes because all
		! fn bodies are compiled before execution begins.  What matters at compile
		! time is whether the fn_id was registered (id is in range of fn_entry).
		! Fortran .or./.and. are NOT short-circuit; use nested ifs.
		first = .false.   ! reuse `first` as "fn_id is registered"
		if (allocated(prog%fn_entry)) then
			if (node%id_index <= size(prog%fn_entry)) first = .true.
		end if

		if (.not. first) then
			! fn_id not registered — neither the translation_unit pre-pass nor
			! compile_module_fns saw this fn.  Compiler bug.
			write(*,*) 'compile: fn_call_expr: fn_entry not registered for id', node%id_index
			call internal_error()
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
	! M6/M8: emit OP_CALL_INTR.  For most intrinsics: push all args, then emit
	! with b=argc for native dispatch in vm_call_intr.
	! For readln/close (slot writeback needed): push the arg, encode the file
	! variable's slot in c = (id_index*2 + is_loc), dispatch inline in vm_exec.
	case (fn_call_intr_expr)
		block
			integer :: intr_id_, nargs_
			integer(kind = 8) :: slot_c
			intr_id_ = intr_id_from_name(node%identifier%text)
			select case (intr_id_)
			case (INTR_READLN, INTR_CLOSE)
				! Push the file argument, encode its slot for writeback in c.
				call compile_node(prog, node%args(1))
				slot_c = int(node%args(1)%id_index, 8) * 2 + &
				         merge(1_8, 0_8, node%args(1)%is_loc)
				call emit(prog, OP_CALL_INTR, a = intr_id_, b = 1, c = slot_c)
			case default
				! Native dispatch: push all args, then emit OP_CALL_INTR.
				nargs_ = 0
				if (allocated(node%args)) then
					do i = 1, size(node%args)
						call compile_node(prog, node%args(i))
					end do
					nargs_ = size(node%args)
				end if
				call emit(prog, OP_CALL_INTR, a = intr_id_, b = nargs_)
			end select
		end block

	! ---- return statement ------------------------------------------------------
	! Inside a compiled function body: push return value (or unknown sentinel for
	! void) then emit OP_RET.  At top level: push return value then OP_HALT.
	case (return_statement)
		if (node%right%val%type == void_type) then
			! Void return: push unknown_type as a dummy return slot.
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		else
			call compile_node(prog, node%right)
		end if
		if (in_fn_body) then
			call emit(prog, OP_RET)
		else
			! Top-level return: OP_HALT exits the VM loop; TOS is the result.
			call emit(prog, OP_HALT)
		end if

	! ---- no unhandled node kinds should reach here ----------------------------
	case default
		write(*,*) 'compile: unhandled node kind ', node%kind
		call internal_error()

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

	!print *, 'starting compile_tree()'

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
