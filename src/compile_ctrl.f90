
!===============================================================================

submodule (syntran__compile_m) syntran__compile_ctrl

	! Bytecode compiler: AST -> bytecode lowering.
	!
	! M1: natively compiles literal_expr, simple name_expr, binary_expr,
	!     unary_expr, let_expr, simple assignment_expr, and translation_unit.
	! M2: adds block_statement, if_statement, while_statement,
	!     break_statement, continue_statement via JUMP/JUMP_IF_FALSE opcodes.
	! M6: fn_call_intr_expr emits OP_CALL_INTR with integer intr_id dispatch.
	! M7: use_statement: module fn bodies compiled into prog before entry_main;
	!     module init code emitted inline at the use_statement site.
	! M8: for_statement (native FOR_SETUP/FOR_NEXT), array_expr (OP_NEW_ARRAY),
	!     slice/complex LHS assign (OP_STORE_SLICE), top-level return (OP_HALT),
	!     readln/close native dispatch with slot writeback.
	! All node kinds are now natively compiled; the case default traps unhandled
	! nodes via internal_error().

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine grow_int(arr)

	! Double the capacity of an allocatable integer array, preserving contents.
	! Used to grow continue_target, break_fixup_*, and block_break_ips on demand.

	integer, allocatable, intent(inout) :: arr(:)
	integer, allocatable :: tmp(:)
	integer :: n

	n = size(arr)
	allocate(tmp(2 * n))
	tmp(1:n) = arr
	call move_alloc(tmp, arr)

end subroutine grow_int

!===============================================================================

recursive subroutine compile_subscript_slots(prog, cs, node)

	! Compile node%lsubscripts(:)'s bound sub-expressions (paired with
	! usubscripts(:)/ssubscripts(:)) to bytecode, in the fixed per-dimension
	! order subscript_dim_nslots/subscript_slot_start (bytecode.f90) assume:
	! step first (step_sub only), then lower (unless omitted), then upper
	! (unless omitted; range_sub/step_sub only).  Used by the
	! OP_INDEX/OP_SLICE/OP_STORE_IDX/OP_SUBSCRIPT_TOS fallback paths so the
	! VM pops pre-evaluated values instead of AST-walking node%lsubscripts(i)
	! via eval_subscript_1d/str_slice_bounds/field_slice_bounds at eval time.

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	type(syntax_node_t),    intent(in)    :: node

	!*******

	integer :: i

	do i = 1, size(node%lsubscripts)
		select case (node%lsubscripts(i)%sub_kind)
		case (all_sub)
			! nothing to compile

		case (scalar_sub, arr_sub)
			call compile_node(prog, cs, node%lsubscripts(i))

		case (range_sub)
			if (.not. node%lsubscripts(i)%lsub_omit) &
				call compile_node(prog, cs, node%lsubscripts(i))
			if (.not. node%lsubscripts(i)%usub_omit) &
				call compile_node(prog, cs, node%usubscripts(i))

		case (step_sub)
			call compile_node(prog, cs, node%ssubscripts(i))
			if (.not. node%lsubscripts(i)%lsub_omit) &
				call compile_node(prog, cs, node%lsubscripts(i))
			if (.not. node%lsubscripts(i)%usub_omit) &
				call compile_node(prog, cs, node%usubscripts(i))

		end select
	end do

end subroutine compile_subscript_slots

!===============================================================================

recursive subroutine compile_member_chain_slots(prog, cs, node)

	! Mirrors get_val/set_val's (eval_array.f90) recursive member-chain
	! walk: compiles node's own subscript bound sub-expressions (if any) via
	! compile_subscript_slots, then either recurses into node%member (when
	! it is itself a further dot_expr) or compiles node%member's
	! subscripts directly (when node%member is the chain's leaf segment).
	! Order and structure must match chain_total_nslots (bytecode.f90)
	! exactly, and get_val/set_val's own `pos` cursor consumption order.
	! Used by OP_LOAD_MEMBER/OP_LOAD_MEMBER_TOS/OP_STORE_MEMBER and the
	! OP_RET by-ref receiver writeback.

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	type(syntax_node_t),    intent(in)    :: node

	if (allocated(node%lsubscripts)) call compile_subscript_slots(prog, cs, node)

	if (allocated(node%member)) then
		if (node%member%kind == dot_expr) then
			call compile_member_chain_slots(prog, cs, node%member)
		else if (allocated(node%member%lsubscripts)) then
			call compile_subscript_slots(prog, cs, node%member)
		end if
	end if

end subroutine compile_member_chain_slots

!===============================================================================

recursive subroutine compile_array_expr_slots(prog, cs, node)

	! Compile an array_expr node's sub-expressions to bytecode, in the fixed
	! order array_expr_nslots (bytecode.f90) assumes -- see its docstring.
	! Used by OP_NEW_ARRAY's fallback sites (unif_array/expl_array not
	! covered by their native paths, plus step_array/len_array/size_array,
	! which have no native path at all) and by the for_statement fallback
	! (node%array%kind == array_expr but not for_setup_native_ok, i.e.
	! unif_array/size_array/expl_array) so the VM pops pre-evaluated values
	! instead of AST-walking via eval_array_expr / OP_FOR_SETUP's inline
	! handling at eval time.

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	type(syntax_node_t),    intent(in)    :: node

	!*******

	integer :: i

	select case (node%val%array%kind)
	case (step_array)
		call compile_node(prog, cs, node%lbound)
		call compile_node(prog, cs, node%step)
		call compile_node(prog, cs, node%ubound)

	case (len_array)
		call compile_node(prog, cs, node%lbound)
		call compile_node(prog, cs, node%ubound)
		call compile_node(prog, cs, node%len_)

	case (unif_array)
		do i = 1, size(node%size)
			call compile_node(prog, cs, node%size(i))
		end do
		call compile_node(prog, cs, node%lbound)

	case (bound_array)
		call compile_node(prog, cs, node%lbound)
		call compile_node(prog, cs, node%ubound)

	case (size_array)
		do i = 1, size(node%elems)
			call compile_node(prog, cs, node%elems(i))
		end do
		do i = 1, size(node%size)
			call compile_node(prog, cs, node%size(i))
		end do

	case (expl_array)
		do i = 1, size(node%elems)
			call compile_node(prog, cs, node%elems(i))
		end do

	end select

end subroutine compile_array_expr_slots

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

recursive subroutine compile_module_fns(prog, cs, module_node)

	! Compile all fn bodies from a module's translation_unit node into prog,
	! recording their entry points in prog%fn_entry.  Recursively handles any
	! nested use_statement members so that transitively-imported module fns are
	! also compiled.  Skips any fn whose fn_entry is already non-zero to guard
	! against diamond-dependency double-compilation.
	!
	! Two-sub-pass design so that forward references within the module resolve:
	!   Sub-pass A: register (ensure_fn_entry) ALL fns and methods first.
	!   Sub-pass B: compile each body; any call to a forward-declared sibling
	!               finds the id registered (in range) even if fn_entry == 0,
	!               and OP_CALL is emitted safely (entry will be filled before VM runs).

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	type(syntax_node_t),    intent(in)    :: module_node   ! translation_unit

	!*******

	integer :: i, j, fn_id, const_idx

	! Recurse into nested use_statements first so that their fns are available
	! to any bodies compiled below that call them.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind /= use_statement) cycle
		if (.not. allocated(module_node%members(i)%member)) cycle
		call compile_module_fns(prog, cs, module_node%members(i)%member)
	end do

	! Sub-pass A: register all local fn and method ids before compiling any body.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind == fn_declaration) then
			call ensure_fn_entry(prog, module_node%members(i)%id_index)
		else if (module_node%members(i)%kind == struct_declaration) then
			if (allocated(module_node%members(i)%members)) then
				do j = 1, size(module_node%members(i)%members)
					call ensure_fn_entry(prog, module_node%members(i)%members(j)%id_index)
				end do
			end if
		end if
	end do

	! Sub-pass B: compile each fn_declaration body and struct method bodies.
	do i = 1, size(module_node%members)
		if (module_node%members(i)%kind == fn_declaration) then
			fn_id = module_node%members(i)%id_index
			if (prog%fn_entry(fn_id) /= 0) cycle   ! already compiled (diamond dep)
			prog%fn_num_locs(fn_id) = module_node%members(i)%num_locs
			prog%fn_entry(fn_id)    = prog%len_ + 1
			cs%in_fn_body = .true.
			call compile_node(prog, cs, module_node%members(i)%body)
			cs%in_fn_body = .false.
			! Implicit void return for functions with no explicit return statement
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
			call emit(prog, OP_RET)
		else if (module_node%members(i)%kind == struct_declaration) then
			if (allocated(module_node%members(i)%members)) then
				do j = 1, size(module_node%members(i)%members)
					fn_id = module_node%members(i)%members(j)%id_index
					if (prog%fn_entry(fn_id) /= 0) cycle   ! already compiled
					prog%fn_num_locs(fn_id) = module_node%members(i)%members(j)%num_locs
					prog%fn_entry(fn_id)    = prog%len_ + 1
					cs%in_fn_body = .true.
					call compile_node(prog, cs, module_node%members(i)%members(j)%body)
					cs%in_fn_body = .false.
					const_idx = add_const(prog, unknown_val())
					call emit(prog, OP_LOAD_CONST, a = const_idx)
					call emit(prog, OP_RET)
				end do
			end if
		end if
	end do

end subroutine compile_module_fns

!===============================================================================

subroutine backpatch_breaks(prog, cs, depth, tgt)

	! Patch all pending loop-break fixups at the given loop depth to tgt,
	! then remove them from the fixup list.

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	integer,                intent(in)    :: depth, tgt

	!*******

	integer :: i, j

	j = 0
	do i = 1, cs%nbreak_fixups
		if (cs%break_fixup_depths(i) == depth) then
			call patch_jump(prog, cs%break_fixup_ips(i), tgt)
		else
			j = j + 1
			cs%break_fixup_ips(j)    = cs%break_fixup_ips(i)
			cs%break_fixup_depths(j) = cs%break_fixup_depths(i)
		end if
	end do
	cs%nbreak_fixups = j

end subroutine backpatch_breaks

!===============================================================================

recursive subroutine compile_node(prog, cs, node)

	! Lower one AST node to opcodes.  The contract is that this subroutine
	! always leaves exactly one value on the operand stack after the emitted
	! opcodes execute.
	!
	! Exception: break_statement and continue_statement emit an unconditional
	! JUMP so the instructions after them are unreachable.  Stack discipline is
	! only required on live paths.

	type(program_t),        intent(inout) :: prog
	type(compiler_state_t), intent(inout) :: cs
	type(syntax_node_t),    intent(in)    :: node

	!*******

	integer :: idx, i, j, const_idx, typed_op
	integer :: jf_ip, j_ip, l_top, l_else, l_end, l_pad
	integer :: nblock_saved
	logical :: first, entering_ctx

	select case (node%kind)

	! ---- literals --------------------------------------------------------------
	case (literal_expr)
		! Scalar types: embed value directly in the instruction (no const pool).
		! Non-scalar (strings, arrays): still use const pool.
		select case (node%val%type)
		case (bool_type)
			call emit(prog, OP_LOAD_CONST_BOOL, a = merge(1, 0, node%val%sca%bool))
		case (i32_type)
			call emit(prog, OP_LOAD_CONST_I32, a = node%val%sca%i32)
		case (i64_type)
			call emit(prog, OP_LOAD_CONST_I64, c = node%val%sca%i64)
		case (f32_type)
			call emit(prog, OP_LOAD_CONST_F32, a = transfer(node%val%sca%f32, 0))
		case (f64_type)
			call emit(prog, OP_LOAD_CONST_F64, c = transfer(node%val%sca%f64, 0_8))
		case default
			const_idx = add_const(prog, node%val)
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end select

	! ---- fn-pointer reference ---------------------------------------------------
	! A bare fn name: behaves like a literal.  node%val is the fully-built
	! fn_type value (fn_index/fn_params/fn_ret); it has allocatable components
	! so it always goes through the const pool, like the literal_expr non-scalar
	! case above
	case (fn_ref_expr)
		const_idx = add_const(prog, node%val)
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- enum variant access ----------------------------------------------------
	! An enum variant, e.g. `Dir.North`: like fn_ref_expr, node%val was fully
	! baked at parse time (parse_enum_access) and carries allocatable
	! components (enum_name/enum_variant/enum_cookie), so it goes through the
	! const pool rather than the scalar-immediate literal_expr cases above
	case (enum_access_expr)
		const_idx = add_const(prog, node%val)
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- variable reads --------------------------------------------------------
	case (name_expr)
		if (allocated(node%lsubscripts)) then
			! Subscripted access.
			! Fast path: all-scalar subscripts + numeric result → OP_INDEX_NAT.
			!   Compile each subscript expression onto the operand stack so the VM
			!   can compute the linear index inline without calling syntax_eval.
			! Fallback A: all-scalar but non-numeric (str char, struct) → OP_INDEX.
			! Fallback B: any range/step/all subscript → OP_SLICE.
			if (index_native_ok(node)) then
				do i = 1, size(node%lsubscripts)
					call compile_node(prog, cs, node%lsubscripts(i))
				end do
				call emit(prog, OP_INDEX_NAT, &
					a = node%id_index, &
					b = int(size(node%lsubscripts)), &
					c = merge(1_8, 0_8, node%is_loc))
			else if (str_index_native_ok(node)) then
				call compile_node(prog, cs, node%lsubscripts(1))
				call emit(prog, OP_STR_INDEX_NAT, &
					a = node%id_index, &
					c = merge(1_8, 0_8, node%is_loc))
			else if (all(node%lsubscripts%sub_kind == scalar_sub)) then
				! Fallback: str char or struct-element scalar index.  Every
				! subscript is scalar_sub (1 slot each, see
				! subscript_dim_nslots), so this just pushes them in order --
				! the VM computes the linear index itself instead of calling
				! subscript_eval, which would otherwise AST-walk them.
				call compile_subscript_slots(prog, cs, node)
				idx = add_node(prog, node)
				call emit(prog, OP_INDEX, a = idx)
			else if (str_slice_native_ok(node)) then
				call compile_node(prog, cs, node%lsubscripts(1))
				call compile_node(prog, cs, node%usubscripts(1))
				call emit(prog, OP_STR_SLICE_NAT, &
					a = node%id_index, &
					c = merge(1_8, 0_8, node%is_loc))
			else if (arr_slice_native_ok(node)) then
				do i = 1, size(node%lsubscripts)
					call compile_node(prog, cs, node%lsubscripts(i))
					call compile_node(prog, cs, node%usubscripts(i))
				end do
				call emit(prog, OP_SLICE_NAT, &
					a = node%id_index, &
					b = int(size(node%lsubscripts)), &
					c = merge(1_8, 0_8, node%is_loc))
			else
				! Fallback: any range/step/all/arr subscript combination not
				! covered by the native paths above.  Pre-compile the bound
				! sub-expressions so the VM pops them instead of AST-walking
				! node%lsubscripts(:) via eval_subscript_1d/str_slice_bounds.
				call compile_subscript_slots(prog, cs, node)
				idx = add_node(prog, node)
				call emit(prog, OP_SLICE, a = idx)
			end if
		else
			! Scalar type: emit typed load (avoids value_copy overhead).
			typed_op = typed_load_op(node%val%type, .false., node%is_loc)
			if (typed_op /= 0) then
				call emit(prog, typed_op, a = node%id_index)
			else if (node%is_loc) then
				call emit(prog, OP_LOAD_LOCAL, a = node%id_index)
			else
				call emit(prog, OP_LOAD_GLOBAL, a = node%id_index)
			end if
		end if

	! ---- arithmetic / comparison / logic / bitwise ----------------------------
	case (binary_expr)
		call compile_node(prog, cs, node%left )
		call compile_node(prog, cs, node%right)
		! Same-type scalar or mixed i32/i64: emit typed opcode.
		! Same-type numeric arrays: emit OP_ARR_BINOP (a=op_kind, b=elem_type).
		! Otherwise emit generic OP_BINOP with pre-computed result type in instr%b
		! so the VM can skip get_binary_op_kind at runtime.
		typed_op = binop_typed_opcode(node%op%kind, &
			node%left%val%type, node%right%val%type)
		if (typed_op /= 0) then
			call emit(prog, typed_op)
		else
			! Check for same-type numeric array ⊕ array.  Use nested ifs so the
			! %array%type access is guarded by the allocated() checks — gfortran
			! at -O0 does not short-circuit .and. chains in else-if conditions.
			if (node%left%val%type == array_type .and. &
			    node%right%val%type == array_type) then
				if (allocated(node%left%val%array) .and. &
				    allocated(node%right%val%array)) then
					if (node%left%val%array%type == node%right%val%array%type) then
						typed_op = arr_binop_typed_opcode(node%op%kind, &
							node%left%val%array%type)
						if (typed_op /= 0) &
							call emit(prog, typed_op, &
								a = node%op%kind, b = node%left%val%array%type)
					end if
				end if
			end if
			if (typed_op == 0) &
				call emit(prog, OP_BINOP, a = node%op%kind, b = node%val%type)
		end if

	case (unary_expr)
		call compile_node(prog, cs, node%right)
		typed_op = unop_typed_opcode(node%op%kind, node%right%val%type)
		if (typed_op /= 0) then
			call emit(prog, typed_op)
		else
			call emit(prog, OP_UNOP, a = node%op%kind)
		end if

	! ---- variable declarations ------------------------------------------------
	case (let_expr)
		call compile_node(prog, cs, node%right)
		! Scalar type: emit typed store (avoids assign_ overhead).
		typed_op = typed_store_op(node%val%type, node%is_loc)
		if (typed_op /= 0) then
			call emit(prog, typed_op, a = node%id_index)
		else if (node%is_loc) then
			call emit(prog, OP_STORE_LOCAL,  a = node%id_index)
		else
			call emit(prog, OP_STORE_GLOBAL, a = node%id_index)
		end if

	! ---- assignment: dot member, scalar subscript, simple, or compound -------
	case (assignment_expr)
		if (allocated(node%member)) then
			! M5: dot member assignment: a.b = expr  or  a.b += expr.
			! Chain subscript slots first so RHS ends up on TOS, matching
			! OP_STORE_IDX's [subscripts...][rhs] layout -- the VM's
			! vm_pop_copy grabs the RHS, leaving the slot window below it.
			call compile_member_chain_slots(prog, cs, node)
			call compile_node(prog, cs, node%right)
			idx = add_node(prog, node)
			call emit(prog, OP_STORE_MEMBER, a = idx, b = node%op%kind)

		else if (.not. allocated(node%lsubscripts) .and. &
		         node%op%kind == equals_token) then
			! Simple plain assignment: a = expr
			call compile_node(prog, cs, node%right)
			! Scalar type (LHS and RHS agree — parser guarantees): typed store.
			typed_op = 0
			if (node%val%type == node%right%val%type) &
				typed_op = typed_store_op(node%val%type, node%is_loc)
			if (typed_op /= 0) then
				call emit(prog, typed_op, a = node%id_index)
			else if (node%is_loc) then
				call emit(prog, OP_STORE_LOCAL,  a = node%id_index)
			else
				call emit(prog, OP_STORE_GLOBAL, a = node%id_index)
			end if

		else if (.not. allocated(node%lsubscripts) .and. &
		         .not. allocated(node%member) .and. &
		         compound_to_arith_token(node%op%kind) /= 0) then
			! Compound scalar assignment: a += expr, a -= expr, etc.
			! Native: load a, compile RHS, emit typed binop, store a.
			! Guard: a must be a numeric scalar with typed load/store/binop available.
			! Requires same LHS and RHS type so the binop result type == LHS type.
			block
			integer :: arith_tok_, binop_op_
			arith_tok_ = compound_to_arith_token(node%op%kind)
			typed_op = typed_load_op(node%val%type, .false., node%is_loc)
			binop_op_ = 0
			if (typed_op /= 0 .and. allocated(node%right)) then
				if (node%val%type == node%right%val%type) &
					binop_op_ = binop_typed_opcode(arith_tok_, node%val%type, node%right%val%type)
			end if
			if (binop_op_ /= 0) then
				! Load current value, compile RHS, apply op, store result.
				call emit(prog, typed_op, a = node%id_index)
				call compile_node(prog, cs, node%right)
				call emit(prog, binop_op_)
				typed_op = typed_store_op(node%val%type, node%is_loc)
				call emit(prog, typed_op, a = node%id_index)
			else
				! Fallback: mixed types, bitwise ops, etc.  eval_assignment_expr
				! handles these but no longer walks the RHS itself -- compile it
				! to bytecode so the VM pops the value instead of AST-walking it.
				call compile_node(prog, cs, node%right)
				idx = add_node(prog, node)
				call emit(prog, OP_STORE_SLICE, a = idx)
			end if
			end block

		else
			! Fortran .and. is NOT short-circuit; use nested ifs to guard the
			! all(lsubscripts%sub_kind) access from unallocated lsubscripts.
			first = .false.   ! reuse `first` as "use native subscript store"
			if (allocated(node%lsubscripts)) then
				if (all(node%lsubscripts%sub_kind == scalar_sub)) first = .true.
			end if

			if (first) then
				! Scalar subscript write.
				! Fast path A: plain '=' with numeric RHS → OP_STORE_IDX_NAT.
				! Fast path B: compound op with numeric RHS → OP_COMPOUND_IDX_NAT.
				! Fallback: str/struct elements, casts → OP_STORE_IDX.
				if (store_idx_native_ok(node)) then
					do i = 1, size(node%lsubscripts)
						call compile_node(prog, cs, node%lsubscripts(i))
					end do
					call compile_node(prog, cs, node%right)
					call emit(prog, OP_STORE_IDX_NAT, &
						a = node%id_index, &
						b = int(size(node%lsubscripts)), &
						c = merge(1_8, 0_8, node%is_loc))
				else if (compound_idx_native_ok(node)) then
					do i = 1, size(node%lsubscripts)
						call compile_node(prog, cs, node%lsubscripts(i))
					end do
					call compile_node(prog, cs, node%right)
					call emit(prog, OP_COMPOUND_IDX_NAT, &
						a = node%id_index, &
						b = int(size(node%lsubscripts)), &
						c = int(node%op%kind, 8) * 2_8 + merge(1_8, 0_8, node%is_loc))
				else
					! Fallback: str chars, struct elements, casts, bitwise compound.
					! Every subscript is scalar_sub here (guard above), so
					! compile_subscript_slots just pushes them in order (1 slot
					! each); RHS is pushed last so it stays on TOS for the VM's
					! vm_pop_copy, matching OP_STORE_IDX_NAT/OP_COMPOUND_IDX_NAT's
					! [subscripts...][rhs] stack layout.
					call compile_subscript_slots(prog, cs, node)
					call compile_node(prog, cs, node%right)
					idx = add_node(prog, node)
					call emit(prog, OP_STORE_IDX, a = idx, b = node%op%kind)
				end if
			else if (store_slice_nat_ok(node)) then
				do i = 1, size(node%lsubscripts)
					call compile_node(prog, cs, node%lsubscripts(i))
					call compile_node(prog, cs, node%usubscripts(i))
				end do
				call compile_node(prog, cs, node%right)
				call emit(prog, OP_STORE_SLICE_NAT, &
					a = node%id_index, &
					b = int(size(node%lsubscripts)), &
					c = merge(1_8, 0_8, node%is_loc))
			else
				! Slice LHS, OR a subscript-less whole-array/whole-struct
				! compound assign that isn't a plain arithmetic op (e.g. a
				! bitwise v &= x; compound_to_arith_token doesn't cover
				! bitwise tokens, so that case skips the dedicated
				! subscript-less branch above and lands here too, with
				! node%lsubscripts left unallocated).  Delegate to
				! eval_assignment_expr.  Compile the subscript bound
				! sub-expressions (compile_subscript_slots), when there are
				! any, then the RHS, so the VM pops both instead of
				! AST-walking node%lsubscripts(:)/node%right itself --
				! [subscripts...][rhs] layout, matching OP_STORE_IDX's
				! fallback above.
				if (allocated(node%lsubscripts)) call compile_subscript_slots(prog, cs, node)
				call compile_node(prog, cs, node%right)
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
		if (cs%loop_depth == 0 .and. .not. cs%in_block_break_ctx) then
			cs%in_block_break_ctx = .true.
			entering_ctx = .true.
			nblock_saved = cs%nblock_break_fixups
		else
			entering_ctx = .false.
			nblock_saved = 0		! unused, but avoids uninitialized warning
		end if

		! Compile members: pop between them, leave last result on stack
		first = .true.
		do i = 1, size(node%members)
			if (.not. first) call emit(prog, OP_POP)
			first = .false.
			call compile_node(prog, cs, node%members(i))
		end do
		! Empty block: push unknown_type sentinel
		if (first) then
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

		! After the outermost block body: emit the break-taken sentinel pad
		! if any block-level break fixups were collected.
		if (entering_ctx) then
			if (cs%nblock_break_fixups > nblock_saved) then
				! Normal path: skip the pad
				j_ip = prog%len_ + 1
				call emit(prog, OP_JUMP, a = 0)

				! Break path: push unknown_type as the block's result
				l_pad = prog%len_ + 1
				do i = nblock_saved + 1, cs%nblock_break_fixups
					call patch_jump(prog, cs%block_break_ips(i), l_pad)
				end do
				const_idx = add_const(prog, unknown_val())
				call emit(prog, OP_LOAD_CONST, a = const_idx)

				! Both paths converge here
				l_end = prog%len_ + 1
				call patch_jump(prog, j_ip, l_end)
			end if

			! Restore the block-break context for the enclosing scope
			cs%nblock_break_fixups = nblock_saved
			cs%in_block_break_ctx  = .false.
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
		call compile_node(prog, cs, node%condition)

		! JUMP_IF_FALSE — target patched below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_JUMP_IF_FALSE, a = 0)

		! Compile the if-clause
		call compile_node(prog, cs, node%if_clause)

		if (allocated(node%else_clause)) then
			! JUMP past else — target patched below
			j_ip = prog%len_ + 1
			call emit(prog, OP_JUMP, a = 0)

			l_else = prog%len_ + 1
			call patch_jump(prog, jf_ip, l_else)

			! Compile the else-clause (may itself be another if_statement)
			call compile_node(prog, cs, node%else_clause)

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
		if (cs%loop_depth + 1 > size(cs%continue_target)) call grow_int(cs%continue_target)
		cs%loop_depth = cs%loop_depth + 1
		l_top = prog%len_ + 1
		cs%continue_target(cs%loop_depth) = l_top

		! Compile condition
		call compile_node(prog, cs, node%condition)

		! JUMP_IF_FALSE — patched to L_end below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_JUMP_IF_FALSE, a = 0)

		! Compile body; its result is discarded before looping
		call compile_node(prog, cs, node%body)
		call emit(prog, OP_POP)
		call emit(prog, OP_JUMP, a = l_top)

		! L_end: backpatch the conditional exit and all break fixups
		l_end = prog%len_ + 1
		call patch_jump(prog, jf_ip, l_end)
		call backpatch_breaks(prog, cs, cs%loop_depth, l_end)
		cs%loop_depth = cs%loop_depth - 1

		! Push unknown_type as the while-statement's result value
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- break -----------------------------------------------------------------
	case (break_statement)
		if (cs%loop_depth > 0) then
			! Inside a native while/for: jump to the loop's end (backpatched)
			if (cs%nbreak_fixups + 1 > size(cs%break_fixup_ips)) then
				call grow_int(cs%break_fixup_ips)
				call grow_int(cs%break_fixup_depths)
			end if
			cs%nbreak_fixups = cs%nbreak_fixups + 1
			cs%break_fixup_ips(cs%nbreak_fixups)    = prog%len_ + 1
			cs%break_fixup_depths(cs%nbreak_fixups) = cs%loop_depth
			call emit(prog, OP_JUMP, a = 0)

		else if (cs%in_block_break_ctx) then
			! Inside the outermost native block (no enclosing loop): jump to
			! the block's end pad (backpatched when the block finishes)
			if (cs%nblock_break_fixups + 1 > size(cs%block_break_ips)) &
				call grow_int(cs%block_break_ips)
			cs%nblock_break_fixups = cs%nblock_break_fixups + 1
			cs%block_break_ips(cs%nblock_break_fixups) = prog%len_ + 1
			call emit(prog, OP_JUMP, a = 0)

		else
			! No breakable context: top-level break is a no-op in translation_unit
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		end if

	! ---- continue --------------------------------------------------------------
	case (continue_statement)
		if (cs%loop_depth > 0) then
			call emit(prog, OP_JUMP, a = cs%continue_target(cs%loop_depth))
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

		! Pass 0: grow fn_entry / fn_num_locs for locally-declared fns and methods.
		do i = 1, size(node%members)
			if (node%members(i)%kind == fn_declaration) then
				call ensure_fn_entry(prog, node%members(i)%id_index)
			else if (node%members(i)%kind == struct_declaration) then
				if (allocated(node%members(i)%members)) then
					do j = 1, size(node%members(i)%members)
						call ensure_fn_entry(prog, node%members(i)%members(j)%id_index)
					end do
				end if
			end if
		end do

		! M7 pre-pass: compile fn bodies from all transitively-imported modules.
		! Must run before Pass 1 (local fns) so that cross-module calls resolve.
		do i = 1, size(node%members)
			if (node%members(i)%kind /= use_statement) cycle
			if (.not. allocated(node%members(i)%member)) cycle
			call compile_module_fns(prog, cs, node%members(i)%member)
		end do

		! Pass 1: compile each locally-declared fn body (including struct methods).
		do i = 1, size(node%members)
			if (node%members(i)%kind == fn_declaration) then
				l_top = node%members(i)%id_index   ! fn_id (reuse l_top as scratch)
				prog%fn_num_locs(l_top) = node%members(i)%num_locs
				prog%fn_entry(l_top)    = prog%len_ + 1

				cs%in_fn_body = .true.
				call compile_node(prog, cs, node%members(i)%body)
				cs%in_fn_body = .false.
				! Implicit void return for functions with no explicit return statement
				const_idx = add_const(prog, unknown_val())
				call emit(prog, OP_LOAD_CONST, a = const_idx)
				call emit(prog, OP_RET)
			else if (node%members(i)%kind == struct_declaration) then
				if (allocated(node%members(i)%members)) then
					do j = 1, size(node%members(i)%members)
						l_top = node%members(i)%members(j)%id_index
						prog%fn_num_locs(l_top) = node%members(i)%members(j)%num_locs
						prog%fn_entry(l_top)    = prog%len_ + 1

						cs%in_fn_body = .true.
						call compile_node(prog, cs, node%members(i)%members(j)%body)
						cs%in_fn_body = .false.
						const_idx = add_const(prog, unknown_val())
						call emit(prog, OP_LOAD_CONST, a = const_idx)
						call emit(prog, OP_RET)
					end do
				end if
			end if
		end do

		! REPL pass: compile any fn declared on an earlier REPL line.  Each
		! REPL statement gets its own fresh program_t (c.f. eval_dispatch()
		! in syntran.f90), so such a fn has no fn_declaration node in *this*
		! tree -- its AST only survives in cs%fns%fns(:) (state%fns%fns(:)
		! at REPL time), carried forward from the line that declared it.
		! Skip ids already compiled above (this line's own fns) and
		! intrinsics (unallocated %node).  No-op outside the REPL, where
		! compile_tree() is called without the fns arg
		if (associated(cs%fns)) then
			do i = cs%fns%num_intr_fns + 1, size(cs%fns%fns)
				if (.not. allocated(cs%fns%fns(i)%node)) cycle

				call ensure_fn_entry(prog, i)
				if (prog%fn_entry(i) /= 0) cycle   ! already compiled above

				l_top = i   ! fn_id (reuse l_top as scratch)
				prog%fn_num_locs(l_top) = cs%fns%fns(i)%node%num_locs
				prog%fn_entry(l_top)    = prog%len_ + 1

				cs%in_fn_body = .true.
				call compile_node(prog, cs, cs%fns%fns(i)%node%body)
				cs%in_fn_body = .false.
				! Implicit void return for functions with no explicit return statement
				const_idx = add_const(prog, unknown_val())
				call emit(prog, OP_LOAD_CONST, a = const_idx)
				call emit(prog, OP_RET)
			end do
		end if

		! Top-level statements start here.
		prog%entry_main = prog%len_ + 1

		! Pass 2: compile top-level (non-fn, non-struct) statements.
		! Each contributes to the result: the last one leaves its value on the
		! stack; preceding ones are discarded with OP_POP.
		first = .true.
		do i = 1, size(node%members)
			if (node%members(i)%kind == fn_declaration    ) cycle
			if (node%members(i)%kind == struct_declaration) cycle
			if (node%members(i)%kind == enum_declaration   ) cycle
			if (.not. first) call emit(prog, OP_POP)
			first = .false.
			call compile_node(prog, cs, node%members(i))
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
		if (for_setup_native_ok(node)) then
			! Compile bound sub-expressions to bytecode; the VM pops them instead
			! of calling syntax_eval every time the surrounding loop re-enters.
			select case (node%array%val%array%kind)
			case (bound_array)
				call compile_node(prog, cs, node%array%lbound)
				call compile_node(prog, cs, node%array%ubound)
			case (step_array)
				call compile_node(prog, cs, node%array%lbound)
				call compile_node(prog, cs, node%array%step)
				call compile_node(prog, cs, node%array%ubound)
			case (len_array)
				call compile_node(prog, cs, node%array%lbound)
				call compile_node(prog, cs, node%array%ubound)
				call compile_node(prog, cs, node%array%len_)
			end select
			call emit(prog, OP_FOR_SETUP_NAT, a = idx, b = node%array%val%array%kind, &
				c = int(node%array%val%array%type, 8))
		else if (node%array%kind /= array_expr) then
			! Non-primary iterable: an arbitrary expression (string or array-
			! valued), not literal array syntax.  Compile it to bytecode so the
			! VM pops the value instead of calling syntax_eval on every loop
			! entry (OP_FOR_SETUP only runs once per for-statement, but this
			! keeps the fallback AST-free like the other for_statement paths).
			call compile_node(prog, cs, node%array)
			call emit(prog, OP_FOR_SETUP, a = idx)
		else
			! Literal array syntax whose kind isn't covered by
			! for_setup_native_ok above: unif_array, size_array, or
			! expl_array (bound_array/step_array/len_array are always
			! native, so never reach here).  Compile the sub-expressions
			! (compile_array_expr_slots -- the same helper OP_NEW_ARRAY's
			! fallback uses) so the VM pops them instead of AST-walking via
			! OP_FOR_SETUP's inline unif_array/size_array/expl_array
			! handling / array_at at eval time.
			call compile_array_expr_slots(prog, cs, node%array)
			call emit(prog, OP_FOR_SETUP, a = idx)
		end if

		if (cs%loop_depth + 1 > size(cs%continue_target)) call grow_int(cs%continue_target)
		cs%loop_depth = cs%loop_depth + 1
		l_top = prog%len_ + 1
		cs%continue_target(cs%loop_depth) = l_top

		! FOR_NEXT — patched to L_pop below
		jf_ip = prog%len_ + 1
		call emit(prog, OP_FOR_NEXT, a = 0)

		! Compile body; its result is discarded before the back edge
		call compile_node(prog, cs, node%body)
		call emit(prog, OP_POP)
		call emit(prog, OP_JUMP, a = l_top)

		! L_pop: backpatch FOR_NEXT and all break fixups here, then pop + result
		l_end = prog%len_ + 1
		call patch_jump(prog, jf_ip, l_end)
		call backpatch_breaks(prog, cs, cs%loop_depth, l_end)
		cs%loop_depth = cs%loop_depth - 1
		call emit(prog, OP_FOR_POP)

		! Push unknown_type as the for-statement's result value
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- array expression construction ----------------------------------------
	! Native paths for unif_array and bound_array compile sub-expressions to
	! bytecode so the VM avoids syntax_eval calls; other kinds fall back to
	! OP_NEW_ARRAY which delegates to eval_array_expr.
	case (array_expr)
		select case (node%val%array%kind)

		case (unif_array)
			! [fill_val; d1, d2, ...] — native when element type is numeric/bool.
			! Compile size expressions then the fill value onto the stack.
			! OP_UNIF_ARRAY_NAT's handler uses a fixed-size local dims_ buffer
			! sized MAX_NAT_UNIF_RANK; higher-rank arrays fall back to
			! OP_NEW_ARRAY, which has no rank limit.
			select case (node%val%array%type)
			case (bool_type, i32_type, i64_type, f32_type, f64_type)
				if (node%val%array%rank <= MAX_NAT_UNIF_RANK) then
					do i = 1, node%val%array%rank
						call compile_node(prog, cs, node%size(i))
					end do
					call compile_node(prog, cs, node%lbound)
					call emit(prog, OP_UNIF_ARRAY_NAT, &
						a = node%val%array%type, b = node%val%array%rank)
				else
					call compile_array_expr_slots(prog, cs, node)
					idx = add_node(prog, node)
					call emit(prog, OP_NEW_ARRAY, a = idx)
				end if
			case default
				call compile_array_expr_slots(prog, cs, node)
				idx = add_node(prog, node)
				call emit(prog, OP_NEW_ARRAY, a = idx)
			end select

		case (bound_array)
			! [lb:ub] integer range — always native (only i32/i64 supported).
			call compile_node(prog, cs, node%lbound)
			call compile_node(prog, cs, node%ubound)
			call emit(prog, OP_BOUND_ARRAY_NAT, a = node%val%array%type)

		case (expl_array)
			! [e1, e2, ..., en] — native when element type is scalar numeric/bool.
			if (expl_array_native_ok(node)) then
				do i = 1, int(node%val%array%len_)
					call compile_node(prog, cs, node%elems(i))
				end do
				call emit(prog, OP_EXPL_ARRAY_NAT, &
					a = node%val%array%type, b = int(node%val%array%len_))
			else
				call compile_array_expr_slots(prog, cs, node)
				idx = add_node(prog, node)
				call emit(prog, OP_NEW_ARRAY, a = idx)
			end if

		case default
			call compile_array_expr_slots(prog, cs, node)
			idx = add_node(prog, node)
			call emit(prog, OP_NEW_ARRAY, a = idx)

		end select

	! ---- enum reverse cast -----------------------------------------------------
	! `EnumName(ordinal)`: compile the ordinal sub-expr to bytecode (the VM pops
	! it), and const-pool node%val, which already carries the baked %struct(:)
	! variant list to match against (set at parse time by parse_enum_cast) plus
	! %enum_name for the R32 diagnostic
	case (enum_cast_expr)
		call compile_node(prog, cs, node%right)
		const_idx = add_const(prog, node%val)
		call emit(prog, OP_ENUM_CAST, a = const_idx)

	! ---- struct instance construction -----------------------------------------
	! M5: Compile each member-initialiser expression in order, then emit
	! OP_MAKE_STRUCT.  Only struct_name/struct_cookie/struct_reg_idx are needed
	! at runtime (to build the result's identity), so const-pool a prototype
	! value_t carrying just those instead of storing the whole node; nmembers
	! travels in instr%b since the member expressions are already bytecode.
	case (struct_instance_expr)
		do i = 1, size(node%members)
			call compile_node(prog, cs, node%members(i))
		end do
		block
			type(value_t) :: struct_proto
			struct_proto%type = struct_type
			if (allocated(node%struct_name)) struct_proto%struct_name = node%struct_name
			if (allocated(node%val%struct_cookie)) &
				struct_proto%struct_cookie = node%val%struct_cookie
			struct_proto%struct_reg_idx = node%val%struct_reg_idx
			const_idx = add_const(prog, struct_proto)
		end block
		call emit(prog, OP_MAKE_STRUCT, a = const_idx, b = int(size(node%members)))

	! ---- dot-expression (struct member read) -----------------------------------
	! M5: Store the dot_expr node in the pool so the VM can call get_val with
	! full chain information (nested dots, subscripted members, etc.).
	case (dot_expr)
		if (node%root_kind /= 0) then
			! Root is a fn_call_expr/method_call_expr (e.g. `fn().field`).
			! Compile the fn call (incl. any subscripts) to push root value on stack,
			! then compile the member chain's own subscript slots on top of it
			! (matching OP_SUBSCRIPT_TOS's [value][slot_1]...[slot_N] layout),
			! then emit OP_LOAD_MEMBER_TOS with a wrapper node holding just the
			! member chain.
			block
				type(syntax_node_t) :: root_node, wrapper
				root_node = node
				root_node%kind = node%root_kind
				if (allocated(root_node%member)) deallocate(root_node%member)
				call compile_node(prog, cs, root_node)
				wrapper%kind = dot_expr
				allocate(wrapper%member)
				wrapper%member = node%member
				call compile_member_chain_slots(prog, cs, wrapper)
				idx = add_node(prog, wrapper)
			end block
			call emit(prog, OP_LOAD_MEMBER_TOS, a = idx)
		else
			call compile_member_chain_slots(prog, cs, node)
			idx = add_node(prog, node)
			call emit(prog, OP_LOAD_MEMBER, a = idx)
		end if

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
				if (node%member%members(i)%kind == enum_declaration   ) cycle
				call compile_node(prog, cs, node%member%members(i))
				call emit(prog, OP_POP)
			end do
		end if
		const_idx = add_const(prog, unknown_val())
		call emit(prog, OP_LOAD_CONST, a = const_idx)

	! ---- user-defined function call -------------------------------------------
	! All user-defined functions (local and module-imported) are compiled into
	! this program_t by the translation_unit pre-pass / compile_module_fns (M7).
	! fn_entry[id] must be registered before execution; a missing registration is
	! a compiler bug, not a fallback.
	case (fn_call_expr, method_call_expr)
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
					if (.not. node%is_ref(i)) call compile_node(prog, cs, node%args(i))
				end do

				! Pass 2: move by-ref args from their variable slots onto stack.
				do i = 1, size(node%is_ref)
					if (node%is_ref(i)) then
						if (allocated(node%args(i)%lsubscripts) .or. &
						    node%args(i)%kind == dot_expr .or. &
						    node%args(i)%kind == fn_call_expr .or. &
						    node%args(i)%kind == method_call_expr .or. &
						    node%args(i)%kind == fn_call_intr_expr) then
							! Subscripted, dot-expr, or fn-return receiver: evaluate to get value.
							call compile_node(prog, cs, node%args(i))
						else if (node%args(i)%is_loc) then
							call emit(prog, OP_LOAD_REF_LOCAL,  a = node%args(i)%id_index)
						else
							call emit(prog, OP_LOAD_REF_GLOBAL, a = node%args(i)%id_index)
						end if
					end if
				end do

				! Pass 3: for by-ref args with a subscripted or dot-expr
				! receiver (call_recv_eligible, bytecode.f90), compile the
				! receiver chain's OWN subscript bound sub-expressions -- a
				! second, separate compilation from Pass 2's value-push
				! above -- so OP_RET's writeback pops pre-evaluated values
				! (frame_t%recv_slots) instead of re-walking the receiver's
				! subscripts via set_val at return time.  Preserves today's
				! double-evaluation of a receiver subscript expression
				! (once here for the value, once for the index) rather than
				! fixing it; both now happen at call time instead of split
				! across call/return.  Order must match call_recv_slot_start.
				do i = 1, size(node%is_ref)
					if (call_recv_eligible(node, i)) &
						call compile_member_chain_slots(prog, cs, node%args(i))
				end do
			end if

			call emit(prog, OP_CALL, a = node%id_index, b = idx)
			if (allocated(node%lsubscripts)) then
				! Subscript bound sub-expressions pushed on top of the
				! already-on-stack return value; the VM reads them as a slot
				! window above it instead of AST-walking node%lsubscripts(:).
				call compile_subscript_slots(prog, cs, node)
				call emit(prog, OP_SUBSCRIPT_TOS, a = idx)
			end if
		end if

	! ---- indirect call through a fn-pointer value ------------------------------
	! Unlike fn_call_expr, the target fn is not known until compile time either:
	! it is resolved at runtime from the callee's fn_type value.  All args are
	! by-value in v1 (node%is_ref is allocated all-.false.), so this is just a
	! flat arg push followed by loading the callee value on top
	case (fn_call_ptr_expr)
		idx = add_node(prog, node)

		if (allocated(node%args)) then
			do i = 1, size(node%args)
				call compile_node(prog, cs, node%args(i))
			end do
		end if

		! Load the callee fn-pointer value (node%id_index/is_loc identify the
		! *variable* holding it, not a fn id)
		if (node%is_loc) then
			call emit(prog, OP_LOAD_LOCAL, a = node%id_index)
		else
			call emit(prog, OP_LOAD_GLOBAL, a = node%id_index)
		end if

		call emit(prog, OP_CALL_PTR, b = idx)
		if (allocated(node%lsubscripts)) then
			call compile_subscript_slots(prog, cs, node)
			call emit(prog, OP_SUBSCRIPT_TOS, a = idx)
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
				if (intr_id_ == INTR_READLN .and. &
					(.not. allocated(node%args) .or. size(node%args) == 0)) then
					! No-arg readln(): reads stdin, no file slot to write back to.
					! Use the native dispatch path instead
					call emit(prog, OP_CALL_INTR, a = intr_id_, b = 0)
				else
					! Push the file argument, encode its slot for writeback in c.
					! Guard: the file arg must be a plain variable (no subscripts, no
					! member access).  If the grammar ever permits readln(arr[i]) or
					! a struct-member file, the slot encoding would be wrong; fail
					! loudly here rather than silently writing to the wrong slot.
					if (node%args(1)%kind /= name_expr) then
						write(*,*) 'compile: readln/close: file argument must be a ' // &
							'plain variable (subscripted/member file not supported)'
						call internal_error()
					end if
					if (allocated(node%args(1)%lsubscripts)) then
						write(*,*) 'compile: readln/close: subscripted file argument not supported'
						call internal_error()
					end if
					if (allocated(node%args(1)%member)) then
						write(*,*) 'compile: readln/close: member file argument not supported'
						call internal_error()
					end if
					call compile_node(prog, cs, node%args(1))
					slot_c = int(node%args(1)%id_index, 8) * 2 + &
					         merge(1_8, 0_8, node%args(1)%is_loc)
					call emit(prog, OP_CALL_INTR, a = intr_id_, b = 1, c = slot_c)
				end if

			case (INTR_SIZE)
				! OP_SIZE_NAT: read array size directly from slot without deep-copying.
				! Only fires when args(1) is a plain variable and (if 2-arg) dim is a
				! literal i32 constant.  Anything else falls back to generic OP_CALL_INTR.
				nargs_ = 0
				if (allocated(node%args)) nargs_ = size(node%args)
				if (nargs_ >= 1 .and. &
				    node%args(1)%kind == name_expr .and. &
				    .not. allocated(node%args(1)%lsubscripts) .and. &
				    .not. allocated(node%args(1)%member)) then
					if (nargs_ == 1) then
						call emit(prog, OP_SIZE_NAT, &
							a = node%args(1)%id_index, b = -1, &
							c = merge(1_8, 0_8, node%args(1)%is_loc))
					else if (node%args(2)%kind == literal_expr .and. &
					         node%args(2)%val%type == i32_type) then
						call emit(prog, OP_SIZE_NAT, &
							a = node%args(1)%id_index, &
							b = node%args(2)%val%sca%i32, &
							c = merge(1_8, 0_8, node%args(1)%is_loc))
					else
						! Non-constant dim: fall back to generic path.
						do i = 1, nargs_
							call compile_node(prog, cs, node%args(i))
						end do
						call emit(prog, OP_CALL_INTR, a = INTR_SIZE, b = nargs_)
					end if
				else
					! Complex array arg: fall back to generic path.
					do i = 1, nargs_
						call compile_node(prog, cs, node%args(i))
					end do
					call emit(prog, OP_CALL_INTR, a = INTR_SIZE, b = nargs_)
				end if

			case default
				! Native dispatch: push all args, then emit OP_CALL_INTR.
				nargs_ = 0
				if (allocated(node%args)) then
					do i = 1, size(node%args)
						call compile_node(prog, cs, node%args(i))
					end do
					nargs_ = size(node%args)
				end if
				call emit(prog, OP_CALL_INTR, a = intr_id_, b = nargs_)
			end select
		end block
		if (allocated(node%lsubscripts)) then
			idx = add_node(prog, node)
			call compile_subscript_slots(prog, cs, node)
			call emit(prog, OP_SUBSCRIPT_TOS, a = idx)
		end if

	! ---- return statement ------------------------------------------------------
	! Inside a compiled function body: push return value (or unknown sentinel for
	! void) then emit OP_RET.  At top level: push return value then OP_HALT.
	case (return_statement)
		if (node%right%val%type == void_type) then
			! Void return: push unknown_type as a dummy return slot.
			const_idx = add_const(prog, unknown_val())
			call emit(prog, OP_LOAD_CONST, a = const_idx)
		else
			call compile_node(prog, cs, node%right)
		end if
		if (cs%in_fn_body) then
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

module subroutine compile_tree(tree, prog, fns)

	type(syntax_node_t), intent(in) :: tree
	type(program_t), intent(out) :: prog
	type(fns_t), intent(in), target, optional :: fns

	!*******

	type(compiler_state_t) :: cs

	!print *, 'starting compile_tree()'

	cs = new_compiler_state()
	if (present(fns)) cs%fns => fns
	prog = new_program()
	call compile_node(prog, cs, tree)

end subroutine compile_tree

!===============================================================================

end submodule syntran__compile_ctrl

!===============================================================================
