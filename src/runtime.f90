
!===============================================================================

module syntran__runtime_m

	! Evaluation-time runtime state and helper procedures for the bytecode
	! VM (vm_exec.f90, vm_intr.f90).  Formerly split across eval.f90 and its
	! submodules (eval_array.f90/eval_control.f90/eval_expr.f90/eval_fn.f90),
	! which implemented an AST-walking evaluator (SYNTRAN_BACKEND=ast) as an
	! alternative to the VM.  That walker is gone; what remains here is the
	! subset the VM itself still calls for constructs its native opcodes
	! delegate to a fallback path for (struct/dot-chain member access,
	! general array-slice subscripting, array-literal construction, and
	! for-loop lazy iteration) -- see runtime_array.f90/runtime_expr.f90/
	! runtime_control.f90's module docstrings for the split
	!
	! Even the fallback paths no longer AST-walk: compile_ctrl.f90 compiles
	! every sub-expression these procedures would otherwise have evaluated
	! to bytecode ahead of time (compile_subscript_slots/
	! compile_member_chain_slots/compile_array_expr_slots), and the VM pops
	! the results into an operand-stack slot window passed in via each
	! procedure's optional `slots` (and, for chains, `pos` cursor) argument.
	! syntax_node_t is still consulted here, but only for its static shape
	! (subscript kind/omit flags, member-chain structure, param lists,
	! array-literal element/dimension counts) -- never evaluated

	use syntran__bool_m
	use syntran__compiler_m, only: bounds_check
	use syntran__math_m

	! consider grouping/encapsulating in a math bitwise module?
	use syntran__math_left_shift_m
	use syntran__math_right_shift_m
	use syntran__math_bit_xor_m
	use syntran__math_bit_or_m
	use syntran__math_bit_and_m
	use syntran__math_bit_not_m

	use syntran__types_m

	! subscript_dim_nslots/subscript_slot_start/subscript_total_nslots/
	! chain_total_nslots/array_expr_nslots: the slot-layout helpers used
	! throughout this module (and by compile_ctrl.f90's compile_*_slots
	! emitters) to agree on where each pre-evaluated bound/element value
	! lands in a `slots` window
	use syntran__bytecode_m

	implicit none

	!********

	type state_t
		! Run time (eval time) state

		logical :: quiet

		type(fns_t) :: fns

		! Parser state that must survive across REPL lines.  Structs and
		! enum declarations are a no-op at eval time -- compile_ctrl.f90
		! `cycle`s past struct_declaration and enum_declaration nodes, and
		! neither structs_t nor enums_t is referenced anywhere outside the
		! parser.  They live here only because state_t is the REPL's one
		! long-lived object, c.f. syntax_parse() (core.f90), which
		! round-trips them through a per-line parser_t
		type(structs_t) :: structs
		type(enums_t) :: enums

		type(vars_t) :: vars, locs

		! Script arguments passed after `--` on the command line
		type(string_vector_t) :: script_args

		! Source directory for resolving relative file paths in open()
		! This is the directory containing the main script being evaluated
		character(len = :), allocatable :: src_dir

		! Runtime-error halt flag and accumulated runtime diagnostics (rt_*
		! codes).  Set by rt_throw() at a runtime-error call site.  Unlike
		! parser diagnostics (node%diagnostics), which can accumulate many
		! errors, evaluation halts after the first runtime error, so rt_diags
		! will have at most one entry in practice
		logical :: rt_halt = .false.
		type(string_vector_t) :: rt_diags

		! Set once a no-arg readln() (stdin) reads past the end of input.
		! Mirrors file_t%eof, but stdin has no file handle to store it on
		logical :: stdin_eof = .false.

	end type state_t

	!********

	! For-loop iterator frame: one entry per active native for loop (vm_exec.f90's
	! for_iters(:) stack).  for_kind uses array_t's kind constants: bound_array,
	! step_array, len_array, expl_array, size_array, unif_array, array_expr, or
	! str_type for string iteration.
	!
	! Declared here (rather than in vm_exec.f90) so array_at() below can take a
	! single for_iter_t instead of the 8 separate fields it used to unpack --
	! those 8 fields are exactly this type's contents plus the loop counter,
	! which the caller sets before calling.
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
		! Enum/struct elements of `array` (array_t has no value_t component of
		! its own), set only when array%type is enum_type/struct_type.
		! c.f. array_at()'s allocated(iter%struct) check below
		type(value_t), allocatable :: struct(:)
		! expl_array/size_array elements, pre-evaluated at OP_FOR_SETUP time
		! (compile_array_expr_slots) instead of AST-walked per-iteration by
		! array_at.  1-based, same indexing as prog%nodes(node_idx)%array%elems.
		! Unallocated for every other for_kind
		type(value_t), allocatable :: elem_vals(:)
	end type for_iter_t

	!********

	interface
		! Implemented in runtime_expr.f90

		recursive module subroutine eval_name_expr(node, state, res, slots)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
			! node%lsubscripts(:)'s bound sub-expressions were already
			! compiled to bytecode and popped by the VM's OP_SLICE handler --
			! see subscript_dim_nslots (bytecode.f90)
			type(value_t), intent(in) :: slots(:)
		end subroutine

		module subroutine promote_i32_i64(val)
			type(value_t), intent(inout) :: val
		end subroutine

		module function str_char_slice(s, node, state, isub, slots) result(out)
			character(len = *), intent(in) :: s
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			integer, intent(in) :: isub
			type(value_t), intent(in) :: slots(:)
			character(len = :), allocatable :: out
		end function

		module subroutine str_char_assign(s, node, state, isub, rhs, slots)
			character(len = *), intent(inout) :: s
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			integer, intent(in) :: isub
			character(len = *), intent(in) :: rhs
			type(value_t), intent(in) :: slots(:)
		end subroutine

	end interface

	interface
		! Implemented in runtime_array.f90

		recursive module subroutine set_val(node, var, state, val, index_, slots, pos)
			type(syntax_node_t), intent(in) :: node
			type(value_t), intent(inout) :: var
			type(state_t), intent(inout) :: state
			type(value_t), intent(in) :: val
			integer(kind = 8), optional, intent(in) :: index_
			! `slots`/`pos`: pre-evaluated subscript bound values for the
			! whole member chain (compile_member_chain_slots,
			! compile_ctrl.f90) plus a running consumption cursor, used
			! instead of AST-walking via sub_eval/field_slice_bounds when
			! present -- see chain_total_nslots' docstring (bytecode.f90)
			type(value_t), intent(in), optional :: slots(:)
			integer, intent(inout), optional :: pos
		end subroutine

		recursive module subroutine get_val(node, var, state, res, index_, slots, pos)
			type(syntax_node_t), intent(in) :: node
			type(value_t), intent(in) :: var
			type(state_t), intent(inout) :: state
			integer(kind = 8), optional, intent(in) :: index_
			type(value_t), intent(out) :: res
			type(value_t), intent(in), optional :: slots(:)
			integer, intent(inout), optional :: pos
		end subroutine

		module subroutine allocate_array(val, cap)
			type(value_t), intent(inout) :: val
			integer(kind = 8), intent(in) :: cap
		end subroutine

		module function new_array(type, cap) result(vector)
			integer, intent(in) :: type
			integer, intent(in), optional :: cap
			type(array_t) :: vector
		end function

		module subroutine apply_assign_op(lhs, rhs, op)
			type(value_t), intent(inout) :: lhs
			type(value_t), intent(in) :: rhs
			type(syntax_token_t), intent(in) :: op
		end subroutine

		module subroutine eval_subscript_1d(node, state, i, lsub, ssub, usub, asub, contributes_rank, slots)
			type(syntax_node_t), intent(in)    :: node
			type(state_t),       intent(inout) :: state
			integer,             intent(in)    :: i
			integer(kind = 8),   intent(out)   :: lsub, ssub, usub
			type(i64_vector_t),  intent(inout) :: asub
			logical,             intent(out)   :: contributes_rank
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module subroutine eval_slice_rank1(node, state, res, slots)
			type(syntax_node_t), intent(in)    :: node
			type(state_t),       intent(inout) :: state
			type(value_t),       intent(out)   :: res
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module subroutine eval_assign_slice_rank1(node, state, id, res, slots)
			type(syntax_node_t), intent(in)    :: node
			type(state_t),       intent(inout) :: state
			integer,             intent(in)    :: id
			type(value_t),       intent(inout) :: res
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module subroutine get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_slice, slots)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(i64_vector_t), allocatable, intent(out) :: asubs(:)
			integer(kind = 8), allocatable, intent(out) :: lsubs(:), ssubs(:), usubs(:)
			integer, intent(out) :: rank_slice
			type(value_t), intent(in) :: slots(:)
		end subroutine

		module subroutine get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
			type(i64_vector_t), intent(in), allocatable :: asubs(:)
			integer(kind = 8), intent(in) :: lsubs(:), ssubs(:)
			integer(kind = 8), intent(inout) :: usubs(:), subs(:)
		end subroutine

		module subroutine field_slice_bounds(member_node, field_val, state, rank_slice, lsubs, ssubs, usubs, asubs, slots)
			type(syntax_node_t),             intent(in)    :: member_node
			type(value_t),                   intent(in)    :: field_val
			type(state_t),                   intent(inout) :: state
			integer,                         intent(out)   :: rank_slice
			integer(kind = 8), allocatable,  intent(out)   :: lsubs(:), ssubs(:), usubs(:)
			type(i64_vector_t),  allocatable, intent(out)   :: asubs(:)
			type(value_t),                   intent(in)    :: slots(:)
		end subroutine

		module subroutine str_slice_bounds(node, isub, sz, state, il, iu, step, slots)
			type(syntax_node_t), intent(in)    :: node
			integer,             intent(in)    :: isub
			integer(kind = 8),   intent(in)    :: sz
			type(state_t),       intent(inout) :: state
			integer(kind = 8),   intent(out)   :: il, iu, step
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module subroutine get_field_slice_val(member_node, field_val, state, res, slots)
			type(syntax_node_t), intent(in)    :: member_node
			type(value_t),       intent(in)    :: field_val
			type(state_t),       intent(inout) :: state
			type(value_t),       intent(out)   :: res
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module subroutine set_field_slice_val(member_node, field_val, state, val, slots)
			type(syntax_node_t), intent(in)    :: member_node
			type(value_t),       intent(inout) :: field_val
			type(state_t),       intent(inout) :: state
			type(value_t),       intent(in)    :: val
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

		module function subscript_i32_eval(subs, array) result(index_)
			integer(kind = 8), intent(in) :: subs(:)
			type(array_t) :: array
			integer(kind = 8) :: index_
		end function

		module function sub_eval(node, var, state, slots) result(index_)
			type(syntax_node_t) :: node
			type(value_t) :: var
			type(state_t), intent(inout) :: state
			! Every node%lsubscripts(i) is scalar_sub here (callers guarantee
			! this), so like subscript_eval, slots(i) is dimension i's value
			! directly
			type(value_t), intent(in) :: slots(:)
			integer(kind = 8) :: index_
		end function

		recursive module function subscript_eval(node, state, slots) result(index_)
			type(syntax_node_t) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(in) :: slots(:)
			integer(kind = 8) :: index_
		end function

		module subroutine array_at(val, iter)
			type(value_t), intent(inout) :: val
			type(for_iter_t), intent(in) :: iter
		end subroutine

		module subroutine get_array_val(array, i, val)
			type(array_t), intent(in) :: array
			integer(kind = 8), intent(in) :: i
			type(value_t), intent(out) :: val
		end subroutine

		module subroutine set_array_val(array, i, val)
			type(array_t), intent(inout) :: array
			integer(kind = 8), intent(in) :: i
			type(value_t), intent(in) :: val
		end subroutine

		module subroutine apply_subscripts_to_val(node, val, state, res, slots)
			type(syntax_node_t), intent(in)    :: node
			type(value_t),       intent(in)    :: val
			type(state_t),       intent(inout) :: state
			type(value_t),       intent(out)   :: res
			type(value_t),       intent(in)    :: slots(:)
		end subroutine

	end interface

	interface
		! Implemented in runtime_control.f90

		recursive module subroutine eval_assignment_expr(node, state, res, rhs_in, slots)
			! Only called by the VM's OP_STORE_SLICE handler (vm_exec.f90),
			! which always compiles+pushes node%right and, when
			! node%lsubscripts is allocated, node%lsubscripts(:)'s bound
			! sub-expressions -- so both args are always supplied (slots may
			! be a zero-length window)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
			type(value_t), intent(in) :: rhs_in
			type(value_t), intent(in) :: slots(:)
		end subroutine

		recursive module subroutine eval_array_expr(node, state, res, slots)
			! Only called by the VM's OP_NEW_ARRAY handler (vm_exec.f90),
			! which always compiles+pushes node's sub-expressions first
			! (compile_array_expr_slots) -- so slots is always supplied
			! (array_expr_nslots(node)-sized); consumed via a monotonic
			! cursor in the same order -- see array_expr_nslots' docstring
			! (bytecode.f90) for the per-kind consumption order
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
			type(value_t), intent(in) :: slots(:)
		end subroutine

	end interface

contains

!===============================================================================

subroutine state_destroy(state)

	! Explicitly tear down state_t's nested-allocatable-value_t containers
	! (%vars, %locs, %structs, %enums, %fns) before state goes out of scope,
	! instead of trusting the compiler's implicit deep deallocation of them
	! -- see value_array_destroy() (value.f90) and the *_destroy family in
	! types_copy.f90.  Call this at every exit of syntran_interpret()/
	! syntran_eval() (syntran.f90), which own state_t's only instance per
	! interpret/eval call

	type(state_t), intent(inout) :: state

	call vars_destroy(state%vars)
	call vars_destroy(state%locs)
	call structs_destroy(state%structs)
	call enums_destroy(state%enums)
	call fns_destroy(state%fns)

end subroutine state_destroy

!===============================================================================

subroutine rt_throw(state, msg)

	! Record a runtime error (R*) on state and set the halt flag.  Call sites
	! that used to do `write(*,*) err_rt(...); call internal_error()` should
	! instead do `call rt_throw(state, err_rt(...)); return` (or `exit` from a
	! dispatch loop).  Unwinding is then handled by rt_halt checks up the call
	! stack; eval_dispatch() is responsible for printing and exiting non-quiet
	! runs, and syntran_eval() is responsible for surfacing rt_diags through
	! the `diags` out-arg for quiet/test runs

	type(state_t), intent(inout) :: state
	character(len = *), intent(in) :: msg

	call state%rt_diags%push(msg)
	state%rt_halt = .true.

end subroutine rt_throw

!===============================================================================

subroutine open_file_impl(state, filename, mode, must_open, file_)

	! Shared implementation behind open() and std::try_open(), called from
	! the bytecode VM (vm_intr.f90).
	!
	! Mode-string errors (R6/R7) always throw -- a malformed mode literal is a
	! program bug, not an I/O condition.  A failure of the underlying Fortran
	! open() throws R8 only when must_open is true; otherwise it returns a
	! closed handle for the caller to inspect via f.is_open

	type(state_t), intent(inout) :: state
	character(len = *), intent(in) :: filename, mode
	logical, intent(in) :: must_open
	type(file_t), intent(out) :: file_

	!********

	character :: char_
	character(len = :), allocatable :: status_, resolved_path
	integer :: i, io

	file_%name_ = filename   ! Keep original name for error messages
	file_%unit_ = -1         ! newunit= is undefined if open() fails

	do i = 1, len(mode)
		char_ = mode(i: i)
		select case (char_)
		case ("r")
			file_%mode_read = .true.

		case ("w")
			file_%mode_write = .true.

		case default
			call rt_throw(state, err_rt(RC_BAD_FILE_MODE, "bad file mode character """// &
				char_//""""))
			return

		end select
	end do

	if (file_%mode_read .and. file_%mode_write) then
		! Maybe "rw" mode could be allowed in the future, but i'm not sure
		! what a useful application would be.  Perhaps if I exposed a
		! rewind() or seek() fn
		call rt_throw(state, err_rt(RC_FILE_RW_MODE, "cannot open file """//filename &
			//""" in combined read/write mode """//mode//""""))
		return
	end if

	if (file_%mode_read) then
		status_ = "old"
	else
		status_ = "unknown"
	end if

	! Resolve relative paths using src_dir from state
	! This is the key change for thread-safety
	resolved_path = resolve_path(state%src_dir, filename)

	open(newunit = file_%unit_, file = resolved_path, &
		status = status_, iostat = io)

	if (io /= 0) then
		! Decode fortran iostat codes in message?  I just looked up the docs
		! and there's not much about open iostat other than 0 is success.
		! Read iostats are more descriptive
		file_%unit_ = -1
		if (must_open) then
			call rt_throw(state, err_rt(RC_OPEN_FILE, "cannot open file """//resolved_path// &
				""" (iostat = "//str(io)//")"))
		end if
		return
	end if

	file_%eof = .false.
	file_%is_open = .true.

end subroutine open_file_impl

!===============================================================================

!function divceil(num, den) result(res)
elemental function divceil(num, den) result(res)

	! Integer division ceiling
	!
	! I initially made this elemental so I could call product() on a vector
	! result, but I need to loop and select case for index arr_sub anyway so
	! just a scalar fn would've sufficed

	integer(kind = 8), intent(in) :: num, den
	integer(kind = 8) :: res

	! I basically have to divide integers and take the ceiling (not floor) here.
	! There are methods that work for positive ints but fail for negatives.  In
	! C you can do it by casting bools to ints (ew)

	res = num / den
	if (mod(num, den) /= 0) res = res + 1  ! TODO: sign? -1 if negative? tests seem ok

	!!!if (num < 0 .and. den

	!print *, "num, den, ceil(num/den) = ", num, den, res

end function divceil

!===============================================================================

end module syntran__runtime_m

!===============================================================================
