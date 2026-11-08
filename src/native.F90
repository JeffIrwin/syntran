
!===============================================================================

module syntran__native_m

	! Native backend: compiles an AST (syntax_node_t) to machine code through
	! LLVM, as an alternative to compiling it to bytecode for the VM (c.f.
	! compile.f90) or to Fortran source (c.f. transpile.f90).  User docs are in
	! doc/native.md.
	!
	! Like the transpiler, this lowers the typed AST rather than the bytecode,
	! because many opcodes call back into the evaluator by node index.  The
	! parser's slot ids (node%id_index) say which variable a name is, so
	! syntran's scopes and shadowing never need to be mirrored here.  A global
	! variable is an LLVM global, and the top-level statements are `main()`.
	!
	! The runtime that a compiled program links with is src/rt/syntran_crt.c.
	! It is embedded in syntran by src/gen_native_rt.sh, and written next to the
	! object file when linking.  Strings are heap objects that the generated code
	! passes by pointer (see `sy_str` there).  The code that makes a string owns
	! it, until it is moved into a variable or consumed by an operation (see
	! nval_t and dispose()).
	!
	! The LLVM-C API is declared in llvm_c.f90, which is only interfaces.  Every
	! procedure that calls it is inside `#ifdef SYNTRAN_LLVM`, so that syntran
	! still builds where LLVM is not installed.  See `native_available`.

	use, intrinsic :: iso_c_binding
	use syntran__bytecode_m, only: struct_move_chain, struct_move_member, take_member_assign
	use syntran__compiler_m, only: bounds_check
	use syntran__consts_m
	use syntran__errors_m
	use syntran__linalg_m, only: blas_enabled
	use syntran__llvm_c_m
	use syntran__native_rt_m
	use syntran__runtime_m
	use syntran_rt, only: rt_str_t, rt_fmt_split, rt_fmt_parse, rt_fmt_spec_t
	use syntran__transpile_m, only: elem_type, intr_display_name, is_arr, &
		enums_slot_of, is_name_char, is_numeric_type, structs_member, structs_slot_of, wider_type
	use syntran__types_m
	use syntran__utils_m
	use syntran__value_m

	implicit none

	! Nothing that this module uses is passed on to the modules that use it
	private
	public :: native_t, native_tree, native_available

	! Was syntran built with LLVM?  If not, native_tree() can only say so
#ifdef SYNTRAN_LLVM
	logical, parameter :: native_available = .true.
#else
	logical, parameter :: native_available = .false.
#endif

	!********

	type native_t

		! Options and result of one native compilation.  Pass to syntran_eval(),
		! syntran_interpret_file(), etc. to compile instead of interpret

		! Print the value of the last statement of the program when it finishes,
		! like the CLI does unless it is given `--quiet`
		logical :: print_result = .true.

		! The CLI trims and left-adjusts the result of a *file*.  It does not for
		! a `-c` command string
		logical :: trim_result = .false.

		! The file to write.  What it is depends on its extension: `.ll` is the
		! textual LLVM IR, `.o` is an object file, and anything else is an
		! executable
		character(len = :), allocatable :: out

		! Instead of writing a file, compile the program in memory and run it, in
		! this process, like `--jit`.  Then `out` isn't used.  The program prints
		! and exits as an executable would, so a runtime error or std::exit()
		! ends syntran.  `script_args` of the state are its arguments, and
		! `exit_code` is what it returns if it runs to the end
		logical :: jit = .false.

		! Like `jit`, but for testing, so that a unit test can evaluate thousands
		! of small programs without linking each.  The program doesn't print or exit:
		! the outcome of the run is left in the fields below
		logical :: capture = .false.

		! The outcome of a run with `capture`.  `status` is 0 if the program ran to
		! the end, 1 if a runtime error stopped it, and 2 if it called std::exit()
		integer :: status = 0, exit_code = 0

		! The type of the result, so that a caller can read it from its text, and
		! its value if it is a float, since the text has too few digits for an f32
		integer :: result_type = unknown_type
		real :: result_f32 = 0.0
		double precision :: result_f64 = 0.d0

		! The result of the program, which is the text that `print_result` makes it
		! write, and the report of its runtime error
		character(len = :), allocatable :: result, message

	end type native_t

	!********

#ifdef SYNTRAN_LLVM

	type nval_t

		! A value that has been built: the LLVM value, its syntran type, and
		! whether the code that made it still owns it.  Only a str or an array can
		! be owned.  Whoever consumes an owned value has to free it, with
		! dispose(), or move it into a variable.  A variable's value or a literal
		! is not owned.  An array also has the type and rank of its elements

		type(c_ptr) :: v = c_null_ptr
		integer :: ty = unknown_type
		logical :: owned = .false.
		integer :: elem = unknown_type, rank = 0

		! The position in the enum table of an enum, or of the elements of an
		! array of them
		integer :: sk = 0

		! The type name of a fn pointer, which is what `str()` of one makes
		character(len = :), allocatable :: tname

	end type nval_t

	!********

	type loop_t

		! A counted loop that is being built: the counter's stack slot, and the
		! blocks of its condition, body, and end

		type(c_ptr) :: it = c_null_ptr, cond_bb = c_null_ptr, body_bb = c_null_ptr
		type(c_ptr) :: end_bb = c_null_ptr

	end type loop_t

	!********

	! How a node of a fused elementwise expression is made, c.f. fuse_kind(): an
	! operator on each element, a scalar that is evaluated once, a slice that is
	! read in place, an array variable, or any other array, which is made first
	integer, parameter :: fk_bin = 1, fk_neg = 2, fk_scalar = 3, fk_view = 4, fk_arr = 5, &
		fk_mat = 6

	type fleaf_t

		! An operand of a fused expression, evaluated once before its loop: a
		! scalar value, or an array whose elements are read in the loop.  `data` is
		! where the elements are, `off` the position of the first one, which is the
		! start of a slice, and `cnt` how many there are.  A slice is not an array
		! of its own, so it can only be compared with another by its length

		type(nval_t) :: v
		logical :: is_arr = .false., view = .false.
		type(c_ptr) :: data = c_null_ptr, off = c_null_ptr, cnt = c_null_ptr
		integer :: ety = unknown_type

	end type fleaf_t

	!********

	type fnret_t
		! What a fn pointer variable returns, for the subscripts of a call of it
		type(value_t), allocatable :: v
	end type fnret_t

	!********

	! The TBAA types that tbaa() tags loads and stores with: the location of the
	! running statement, the header of an array, and each type of its elements
	integer, parameter :: tb_loc = 1, tb_hdr = 2, tb_i32 = 3, tb_i64 = 4, tb_f32 = 5, tb_f64 = 6

	!********

	type slots_t

		! The variables of one scope, by slot id (node%id_index): the globals, or
		! the locals of the fn being built.  `p` is where each one is stored, `ty`
		! its syntran type, and `own` whether the slot owns its string, which has
		! to be freed when the fn returns.  A by-reference parameter's slot is
		! the caller's variable, so it owns nothing

		type(c_ptr), allocatable :: p(:)
		integer, allocatable :: ty(:)
		logical, allocatable :: own(:)

		! The type and rank of the elements, for a slot that is an array
		integer, allocatable :: elem(:), rank(:)

		! The position in the struct table of the struct that a slot holds, or
		! that an array in it has for elements
		integer, allocatable :: sk(:)

		! What each slot that holds a fn pointer returns
		type(fnret_t), allocatable :: fnret(:)

	end type slots_t

	!********

	type place_t

		! Where a variable, an element of an array, or a member of a struct is
		! stored: the address of the storage, its type, and what it holds if
		! it's an array or a struct

		type(c_ptr) :: p = c_null_ptr
		integer :: ty = unknown_type, elem = unknown_type, rank = 0, sk = 0

		! A last subscript of the chain that is not an element: a slice of the
		! array, or a substring of the string, that `p` is the address of.  `subs`
		! is the stack array of `sy_sub` records that build_subs made, `nsub` of
		! them, and `pend` is what is disposed of by place_done().  The rank of
		! a slice is `out_rank`
		logical :: has_subs = .false.
		type(c_ptr) :: subs = c_null_ptr
		integer :: nsub = 0, out_rank = 0
		type(nval_t), allocatable :: pend(:)

	end type place_t

	!********

	type nemitter_t

		! Internal state of the emitter

		type(c_ptr) :: ctx = c_null_ptr, llmod = c_null_ptr, tm = c_null_ptr

		! The builder that the code is built with, and another for the allocas,
		! which all go in a block of their own at the top of the fn.  That keeps
		! them out of loops, and out of the way of the code
		type(c_ptr) :: bld = c_null_ptr, abld = c_null_ptr
		type(c_ptr) :: ablk = c_null_ptr, body = c_null_ptr

		type(c_ptr) :: t_void, t_i1, t_i8, t_i32, t_i64, t_f32, t_f64, t_ptr

		! The TBAA access tags, made at first use, and the id of the metadata kind.
		! See tbaa().  They let LLVM tell that a store to an element can't change
		! the header of an array or the location of the statement
		type(c_ptr) :: tbaa_root = c_null_ptr, tbaa_tag(6) = c_null_ptr
		integer :: tbaa_kind = -1

		logical :: print_result = .true., trim_result = .false.

		! The type of the value that the program printed as its result, and
		! whether it is being run in memory and captured, which wants a float exactly
		integer :: res_ty = unknown_type
		logical :: capture = .false.

		! The arguments of the program, if it is run in memory
		type(string_vector_t) :: script_args

		! The directory that its relative file paths are resolved against, if it is
		! run in memory.  Empty is the working directory
		character(len = :), allocatable :: src_dir

		! Counter for the names of string literals
		integer :: nstr = 0

		! Does the program call BLAS or LAPACK, which it has to be linked with?
		logical :: uses_blas = .false.

		type(slots_t) :: g, l

		! The user fns, for their signatures, and the global variables, for their
		! types
		type(fns_t), pointer :: fns => null()
		type(vars_t), pointer :: vars => null()

		! The structs, for their members.  The descriptor that the runtime
		! describes each one with is made at its first use, by position
		type(structs_t), pointer :: structs => null()
		type(c_ptr), allocatable :: sdesc(:)

		! The enums, and the tables of the values and the names of the variants of
		! each, which are made at their first use, by position
		type(enums_t), pointer :: enums => null()
		type(c_ptr), allocatable :: evals(:), enames(:)

		! The LLVM fn and type of each user fn, by id.  They are declared at their
		! first reference
		type(c_ptr), allocatable :: fnv(:), fnt(:)

		! The fn being built: its LLVM value, the block that every `return` goes
		! to, the stack slot of its return value, and its syntran return type
		type(c_ptr) :: fn = c_null_ptr, exit_bb = c_null_ptr, retslot = c_null_ptr
		integer :: ret_type = void_type
		logical :: in_main = .true.

		! Stack slots of the fn being built which hold arrays that it has to free
		! when it returns, e.g. what a loop iterates over
		type(c_ptr), allocatable :: temps(:)
		logical, allocatable :: temp_is_str(:)
		integer :: ntemps = 0

		! Where `break` and `continue` go, for each loop that is being built
		type(c_ptr), allocatable :: brk(:), cnt(:)
		integer :: depth = 0

		! Where a `break` goes when it's not in a loop: the end of the outermost
		! block, which is only set while that block is being built
		type(c_ptr) :: block_exit = c_null_ptr

		! Source location of the statement being emitted, for diagnostics
		integer :: cur_src_id = 0, cur_src_pos = 0

		! The statement that the last diagnostic was reported for.  There is only
		! one diagnostic per statement, because the rest would be consequences
		! of the first
		integer :: diag_src_id = -1, diag_src_pos = -1

		! The locations of the statements, for the report of a runtime error.  A
		! location is numbered by its first use, from 1, and the table is made of
		! the source id and position of each.  `cur_loc` is the one of the
		! statement being emitted, and `last_loc` is what was last stored in the
		! runtime, in the block `last_bb`, so that it isn't stored again
		integer, allocatable :: loc_src(:), loc_pos(:)
		integer :: nloc = 0, cur_loc = 0, last_loc = -1
		type(c_ptr) :: last_bb = c_null_ptr

		! The name of each user fn, by id, for the trace of the calls
		type(string_t), allocatable :: fnames(:)

		! Is the target x86?  A float that is out of range converts to the smallest
		! integer there, rather than saturating
		logical :: x86 = .false.

		! Does the fn being built push a frame of the call stack?
		logical :: pushes = .false.

		! Is the range being emitted the header of a `for` loop, so that a step of
		! zero is an error of the loop, and not of the array?  Is the place being
		! emitted the target of an assignment?  They are for the text of an error
		logical :: for_hdr = .false., assigning = .false.

		! The values of the subscripts of the target of an assignment to a member,
		! which must be made before the value that is assigned.  Mode 1 keeps each
		! one in a stack slot as it makes it, in `idx_slots`, and mode 2 reads them
		! back in the same order, instead of making them again
		integer :: idx_mode = 0, idx_n = 0
		type(c_ptr), allocatable :: idx_slots(:)

		type(string_vector_t) :: diags

	end type nemitter_t

#endif

	!********

contains

!===============================================================================

#ifndef SYNTRAN_LLVM

subroutine native_tree(tree, state, n, diags)

	! Without LLVM there is nothing to compile with

	type(syntax_node_t), intent(in) :: tree
	type(state_t), intent(in), target :: state
	type(native_t), intent(inout) :: n
	type(string_vector_t), intent(out) :: diags

	diags = new_string_vector()
	call diags%push(err_prefix//'this build of syntran has no LLVM, so it cannot '// &
		'compile natively.  Build it with SYNTRAN_LLVM, e.g. `fpm build '// &
		'--features debug,llvm-blas`'//color_reset)

end subroutine native_tree

#else

!===============================================================================

function cs(s) result(r)
	! A Fortran string as a C string
	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r
	r = s//c_null_char
end function cs

!===============================================================================

subroutine native_tree(tree, state, n, diags)

	! Compile a whole program, and write the file that `n%out` names.  There are
	! diagnostics, and no file, for constructs which are not supported yet

	type(syntax_node_t), intent(in) :: tree
	type(state_t), intent(in), target :: state
	type(native_t), intent(inout) :: n
	type(string_vector_t), intent(out) :: diags

	!********

	integer :: i, last

	logical :: ok

	type(c_ptr) :: p

	type(integer_vector_t) :: done

	type(nemitter_t) :: em

	diags = new_string_vector()

	em%print_result = n%print_result
	em%capture = n%capture
	em%trim_result  = n%trim_result
	em%script_args = state%script_args
	em%src_dir = ''
	if (allocated(state%src_dir)) em%src_dir = state%src_dir
	em%diags = new_string_vector()
	em%fns => state%fns
	em%vars => state%vars
	em%structs => state%structs
	em%enums => state%enums

	call em_init(em, ok)
	if (.not. ok) then
		diags = em%diags
		call em_free(em)
		return
	end if

	if (tree%kind /= translation_unit .or. .not. allocated(tree%members)) then
		call unsupported(em, 'this kind of input')
	end if

	! Declarations aren't statements that run, so they have no value
	last = 0
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			select case (tree%members(i)%kind)
			case (fn_declaration, struct_declaration, enum_declaration)
			case default
				last = i
			end select
		end do
	end if

	! The globals of the program are made first, because a fn uses them by
	! subscripting, which does not say what their type is.  A fn can only use
	! those that are declared at the top level
	call predeclare_globals(em, tree)

	! The fns next.  Each is built on its own.  Those of the modules that the
	! program uses are among them
	done = new_integer_vector()
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == fn_declaration) then
				call done%push(tree%members(i)%id_index)
				call emit_fn(em, tree%members(i))
			else if (tree%members(i)%kind == struct_declaration) then
				call emit_struct_methods(em, tree%members(i), done)
			end if
		end do
		do i = 1, size(tree%members)
			if (tree%members(i)%kind /= use_statement) cycle
			if (.not. allocated(tree%members(i)%member)) cycle
			call emit_module_fns(em, tree%members(i)%member, done)
		end do
	end if

	call begin_fn(em, 'main', em%t_i32, void_type, .true.)
	call rt_void(em, 'sy_set_args', [LLVMGetParam(em%fn, 0_c_int), LLVMGetParam(em%fn, 1_c_int)])
	if (bounds_check) call rt_void(em, 'sy_set_bounds', [ci32(em, 1)])

	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == fn_declaration) cycle
			if (i == last) then
				call emit_result_stmt(em, tree%members(i))
			else
				call emit_stmt(em, tree%members(i))
			end if
		end do
	end if
	if (last == 0) call emit_result_text(em, 'Error: <invalid_value>')

	call trace_tables(em)
	call end_fn(em)

	diags = em%diags
	if (diags%len_ == 0) call finish(em, n, diags)

	call em_free(em)

end subroutine native_tree

!===============================================================================

recursive subroutine predeclare_globals(em, unit)

	! Make the global variables of a translation unit and of the modules that it
	! uses, which are numbered together with the program's own

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: unit

	integer :: i

	type(c_ptr) :: p

	if (.not. allocated(unit%members)) return

	do i = 1, size(unit%members)
		associate (m => unit%members(i))
			if (m%kind == use_statement) then
				if (allocated(m%member)) call predeclare_globals(em, m%member)
			else if (m%kind == let_expr .and. .not. m%is_empty .and. .not. m%is_loc) then
				if (supported_val(m%val)) p = var_ptr(em, m, m%val%type)
			end if
		end associate
	end do

end subroutine predeclare_globals

!===============================================================================

recursive subroutine emit_module_fns(em, unit, done)

	! Build the fns of an imported module, and of the modules that it imports.
	! `done` has the ids of the fns that are already built, which a module that is
	! imported from two places isn't to do again

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: unit
	type(integer_vector_t), intent(inout) :: done

	integer :: i, j

	logical :: found

	if (.not. allocated(unit%members)) return

	do i = 1, size(unit%members)
		if (unit%members(i)%kind /= use_statement) cycle
		if (.not. allocated(unit%members(i)%member)) cycle
		call emit_module_fns(em, unit%members(i)%member, done)
	end do

	do i = 1, size(unit%members)
		if (unit%members(i)%kind == struct_declaration) then
			call emit_struct_methods(em, unit%members(i), done)
			cycle
		end if
		if (unit%members(i)%kind /= fn_declaration) cycle

		found = .false.
		do j = 1, done%len_
			if (done%v(j) == unit%members(i)%id_index) found = .true.
		end do
		if (found) cycle

		call done%push(unit%members(i)%id_index)
		call emit_fn(em, unit%members(i))
	end do

end subroutine emit_module_fns

!===============================================================================

recursive subroutine emit_use(em, node)

	! A module's own top-level statements, like `let count = 0;`, run where it is
	! imported.  Its fns are built with the rest of them.  The parser numbered
	! the module's variables and fns together with the importer's, so they need
	! no mapping

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	integer :: i

	if (.not. allocated(node%member)) return
	if (.not. allocated(node%member%members)) return

	do i = 1, size(node%member%members)
		select case (node%member%members(i)%kind)
		case (fn_declaration, struct_declaration, enum_declaration)
		case default
			call emit_stmt(em, node%member%members(i))
		end select
	end do

end subroutine emit_use

!===============================================================================

subroutine em_init(em, ok)

	! Make the LLVM context, module, builders, and the target machine for the
	! computer that syntran is running on

	type(nemitter_t), intent(inout) :: em
	logical, intent(out) :: ok

	!********

	character(len = :), allocatable :: triple, cpu, feats

	type(c_ptr) :: tgt, msg, p_triple, p_cpu, p_feats, dl

	ok = .false.

	em%ctx   = LLVMContextCreate()
	em%llmod = LLVMModuleCreateWithNameInContext(cs('syntran'), em%ctx)
	em%bld   = LLVMCreateBuilderInContext(em%ctx)
	em%abld  = LLVMCreateBuilderInContext(em%ctx)

	em%t_void = LLVMVoidTypeInContext(em%ctx)
	em%t_i1   = LLVMInt1TypeInContext(em%ctx)
	em%t_i8   = LLVMInt8TypeInContext(em%ctx)
	em%t_i32  = LLVMInt32TypeInContext(em%ctx)
	em%t_i64  = LLVMInt64TypeInContext(em%ctx)
	em%t_f32  = LLVMFloatTypeInContext(em%ctx)
	em%t_f64  = LLVMDoubleTypeInContext(em%ctx)
	em%t_ptr  = LLVMPointerTypeInContext(em%ctx, 0)

	call sy_llvm_init_native()

	! The program is built for this machine, with all of its instruction set
	p_triple = LLVMGetDefaultTargetTriple()
	p_cpu    = LLVMGetHostCPUName()
	p_feats  = LLVMGetHostCPUFeatures()
	triple = c_to_f_string(p_triple)
	cpu    = c_to_f_string(p_cpu)
	feats  = c_to_f_string(p_feats)
	call LLVMDisposeMessage(p_triple)
	call LLVMDisposeMessage(p_cpu)
	call LLVMDisposeMessage(p_feats)

	if (LLVMGetTargetFromTriple(cs(triple), tgt, msg) /= 0) then
		call em%diags%push(err_prefix//'LLVM has no target for `'//triple//'`: '// &
			c_to_f_string(msg)//color_reset)
		call LLVMDisposeMessage(msg)
		return
	end if

	em%tm = LLVMCreateTargetMachine(tgt, cs(triple), cs(cpu), cs(feats), &
		LLVM_CODEGEN_DEFAULT, LLVM_RELOC_PIC, LLVM_CODE_MODEL_DEFAULT)

	em%x86 = index(triple, 'x86_64') == 1 .or. index(triple, 'i386') == 1 .or. &
		index(triple, 'i686') == 1
	call LLVMSetTarget(em%llmod, cs(triple))
	dl = LLVMCreateTargetDataLayout(em%tm)
	call LLVMSetModuleDataLayout(em%llmod, dl)
	call LLVMDisposeTargetData(dl)

	ok = .true.

end subroutine em_init

!===============================================================================

subroutine em_free(em)

	type(nemitter_t), intent(inout) :: em

	if (c_associated(em%tm)) call LLVMDisposeTargetMachine(em%tm)
	if (c_associated(em%abld)) call LLVMDisposeBuilder(em%abld)
	if (c_associated(em%bld)) call LLVMDisposeBuilder(em%bld)
	if (c_associated(em%llmod)) call LLVMDisposeModule(em%llmod)
	if (c_associated(em%ctx)) call LLVMContextDispose(em%ctx)

end subroutine em_free

!===============================================================================

subroutine unsupported(em, what, pos)

	! Report that a construct can't be compiled yet.  The caret goes at the
	! optional source offset `pos` if given, otherwise at the start of the
	! statement being emitted

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: what
	integer, intent(in), optional :: pos

	!********

	integer :: start, length

	type(text_span_t) :: span

	if (em%cur_src_id == em%diag_src_id .and. em%cur_src_pos == em%diag_src_pos &
			.and. em%cur_src_id > 0) return
	em%diag_src_id  = em%cur_src_id
	em%diag_src_pos = em%cur_src_pos

	start = em%cur_src_pos
	if (present(pos)) then
		if (pos > 0) start = pos
	end if

	if (em%cur_src_id < 1 .or. em%cur_src_id > src_registry%len_ .or. start < 1) then
		! No source location, which shouldn't happen for a statement from a parsed
		! program
		call em%diags%push(err_pre(EC_NATIVE_UNSUPPORTED)//what// &
			' is not supported by the native backend yet'//color_reset)
		return
	end if

	associate (context => src_registry%v(em%cur_src_id))

		! Underline the word at the location, e.g. a keyword or a fn name
		length = 1
		do while (start + length <= len(context%text))
			if (.not. is_name_char(context%text(start + length: start + length))) exit
			length = length + 1
		end do

		span = text_span_t(start, length)
		call em%diags%push(err_native_unsupported(context, span, what))

	end associate

end subroutine unsupported

!===============================================================================
! Locations, for the report of a runtime error

function loc_id(em) result(k)

	! The number of the location of the statement that is being emitted, which is
	! made at its first use.  It's 0 if the statement has no location

	type(nemitter_t), intent(inout) :: em
	integer :: k

	integer :: j
	integer, allocatable :: tmp(:)

	k = 0
	if (em%cur_src_id < 1 .or. em%cur_src_pos < 1) return

	! The latest are the most likely to be wanted again
	do j = em%nloc, 1, -1
		if (em%loc_src(j) == em%cur_src_id .and. em%loc_pos(j) == em%cur_src_pos) then
			k = j
			return
		end if
	end do

	if (.not. allocated(em%loc_src)) allocate(em%loc_src(64), em%loc_pos(64))
	if (em%nloc == size(em%loc_src)) then
		allocate(tmp(2 * em%nloc))
		tmp(1: em%nloc) = em%loc_src
		call move_alloc(tmp, em%loc_src)
		allocate(tmp(2 * em%nloc))
		tmp(1: em%nloc) = em%loc_pos
		call move_alloc(tmp, em%loc_pos)
	end if

	em%nloc = em%nloc + 1
	em%loc_src(em%nloc) = em%cur_src_id
	em%loc_pos(em%nloc) = em%cur_src_pos
	k = em%nloc

end function loc_id

!===============================================================================

function rt_global(em, name, ty) result(g)

	! A global variable of the runtime, which is declared at its first use

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr), intent(in) :: ty
	type(c_ptr) :: g

	g = LLVMGetNamedGlobal(em%llmod, cs(name))
	if (.not. c_associated(g)) g = LLVMAddGlobal(em%llmod, ty, cs(name))

end function rt_global

!===============================================================================

subroutine set_loc(em, force)

	! Tell the runtime that the statement being emitted is the one that runs, in
	! case it fails.  This is skipped if the runtime was already told in this
	! block, unless `force` says that a call has changed it since

	type(nemitter_t), intent(inout) :: em
	logical, intent(in), optional :: force

	type(c_ptr) :: bb, ignored

	if (em%cur_loc < 1) return

	bb = LLVMGetInsertBlock(em%bld)
	if (.not. present(force)) then
		if (em%last_loc == em%cur_loc .and. c_associated(em%last_bb, bb)) return
	end if

	ignored = LLVMBuildStore(em%bld, ci32(em, em%cur_loc), rt_global(em, 'sy_loc', em%t_i32))
	call tbaa(em, ignored, tb_loc)
	em%last_loc = em%cur_loc
	em%last_bb = bb

end subroutine set_loc

!===============================================================================

subroutine tbaa(em, inst, k)

	! Tag a load or store with the TBAA type `k`, one of the `tb_*`.  Two accesses
	! of different types never alias, e.g. a store of an `f64` element and a load of
	! the data pointer of an array.  That is what lets LLVM hoist the loads of an
	! array's header out of a loop that writes its elements, and so vectorize it.
	! Only tag memory that is always accessed with the tagged type: an access
	! without a tag may alias anything, so it is always safe to leave one out

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: inst
	integer, intent(in) :: k

	!********

	character(len = 4), parameter :: names(6) = ['loc ', 'hdr ', 'i32 ', 'i64 ', 'f32 ', 'f64 ']

	type(c_ptr) :: zero, root_name, ty, tag

	if (k < 1 .or. k > size(names)) return

	if (em%tbaa_kind < 0) em%tbaa_kind = LLVMGetMDKindIDInContext(em%ctx, cs('tbaa'), 4)

	if (.not. c_associated(em%tbaa_tag(k))) then
		if (.not. c_associated(em%tbaa_root)) then
			root_name = LLVMMDStringInContext2(em%ctx, cs('syntran'), 7_c_size_t)
			em%tbaa_root = LLVMMDNodeInContext2(em%ctx, [root_name], 1_c_size_t)
		end if
		zero = LLVMValueAsMetadata(ci64(em, 0_8))
		ty = LLVMMDNodeInContext2(em%ctx, [ &
			LLVMMDStringInContext2(em%ctx, cs(trim(names(k))), int(len_trim(names(k)), c_size_t)), &
			em%tbaa_root, zero], 3_c_size_t)
		tag = LLVMMDNodeInContext2(em%ctx, [ty, ty, zero], 3_c_size_t)
		em%tbaa_tag(k) = LLVMMetadataAsValue(em%ctx, tag)
	end if

	call LLVMSetMetadata(inst, em%tbaa_kind, em%tbaa_tag(k))

end subroutine tbaa

!===============================================================================

function tbaa_elem(ety) result(k)

	! The TBAA type of the elements of type `ety`, or 0 for the ones that aren't tagged

	integer, intent(in) :: ety
	integer :: k

	select case (ety)
	case (i32_type);  k = tb_i32
	case (i64_type);  k = tb_i64
	case (f32_type);  k = tb_f32
	case (f64_type);  k = tb_f64
	case default;     k = 0
	end select

end function tbaa_elem

!===============================================================================

subroutine push_frame(em, id)

	! Push a frame of the call stack for the fn that is being built, with the
	! location of the call, which the caller stored.  This is inline, because
	! calls are common.  Only a full stack calls the runtime, to grow it

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: id

	type(c_ptr) :: gdepth, d, full, grow_bb, push_bb, base, slot, ignored
	type(c_ptr) :: no_args(0)

	gdepth = rt_global(em, 'sy_depth', em%t_i32)
	d = LLVMBuildLoad2(em%bld, em%t_i32, gdepth, cs(''))
	full = LLVMBuildICmp(em%bld, LLVM_INT_SGE, d, LLVMBuildLoad2(em%bld, em%t_i32, &
		rt_global(em, 'sy_fcap', em%t_i32), cs('')), cs(''))

	grow_bb = new_block(em, 'grow')
	push_bb = new_block(em, 'push')
	ignored = LLVMBuildCondBr(em%bld, full, grow_bb, push_bb)

	call LLVMPositionBuilderAtEnd(em%bld, grow_bb)
	call rt_void(em, 'sy_frames_grow', no_args)
	ignored = LLVMBuildBr(em%bld, push_bb)

	! A frame is two i32s: the fn and the location of the call
	call LLVMPositionBuilderAtEnd(em%bld, push_bb)
	base = LLVMBuildLoad2(em%bld, em%t_ptr, rt_global(em, 'sy_frames', em%t_ptr), cs(''))
	slot = LLVMBuildGEP2(em%bld, em%t_i32, base, &
		[LLVMBuildMul(em%bld, d, ci32(em, 2), cs(''))], 1, cs(''))
	ignored = LLVMBuildStore(em%bld, ci32(em, id), slot)
	slot = LLVMBuildGEP2(em%bld, em%t_i32, slot, [ci32(em, 1)], 1, cs(''))
	ignored = LLVMBuildStore(em%bld, LLVMBuildLoad2(em%bld, em%t_i32, &
		rt_global(em, 'sy_loc', em%t_i32), cs('')), slot)
	ignored = LLVMBuildStore(em%bld, LLVMBuildAdd(em%bld, d, ci32(em, 1), cs('')), gdepth)

	em%last_loc = -1

end subroutine push_frame

!===============================================================================

subroutine pop_frame(em)

	type(nemitter_t), intent(inout) :: em

	type(c_ptr) :: gdepth, ignored

	gdepth = rt_global(em, 'sy_depth', em%t_i32)
	ignored = LLVMBuildStore(em%bld, LLVMBuildSub(em%bld, &
		LLVMBuildLoad2(em%bld, em%t_i32, gdepth, cs('')), ci32(em, 1), cs('')), gdepth)

end subroutine pop_frame

!===============================================================================

subroutine err_if(em, bad, code, msg)

	! If the i1 `bad` is true, report a runtime error whose text is known and end
	! the program.  This is a branch to a block of its own, which isn't taken

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: bad
	character(len = *), intent(in) :: code, msg

	type(c_ptr) :: bad_bb, good_bb, ignored

	bad_bb  = new_block(em, 'fail')
	good_bb = new_block(em, 'ok')
	ignored = LLVMBuildCondBr(em%bld, bad, bad_bb, good_bb)

	call LLVMPositionBuilderAtEnd(em%bld, bad_bb)
	call rt_void(em, 'sy_error', [c_string(em, code), c_string(em, msg)])
	ignored = LLVMBuildBr(em%bld, good_bb)

	call LLVMPositionBuilderAtEnd(em%bld, good_bb)

end subroutine err_if

!===============================================================================

subroutine step_check(em, st, what, code_i, code_f)

	! A step of a range can't be zero.  `what` is "for loop" or "array".  A float
	! step has a code of its own, and its text says 0.0

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: st
	character(len = *), intent(in) :: what, code_i, code_f

	if (st%ty == f32_type .or. st%ty == f64_type) then
		call err_if(em, LLVMBuildFCmp(em%bld, LLVM_REAL_OEQ, st%v, &
			LLVMConstNull(LLVMTypeOf(st%v)), cs('')), code_f, what//' step is 0.0')
	else
		call err_if(em, LLVMBuildICmp(em%bld, LLVM_INT_EQ, st%v, &
			LLVMConstNull(LLVMTypeOf(st%v)), cs('')), code_i, what//' step is 0')
	end if

end subroutine step_check

!===============================================================================

subroutine trace_tables(em)

	! Hand the runtime the table of the locations of the statements, and the one
	! of the names of the fns, for the report of a runtime error.  This is called
	! at the start of `main()`, which is in its block of the allocas, since the
	! tables are only complete once the whole program is built

	type(nemitter_t), intent(inout) :: em

	character(len = :), allocatable :: text

	integer :: i, k, last, nfn
	integer :: i1(1)

	type(c_ptr) :: tys(4), vals(4), lty, gl, gn, saved
	type(c_ptr), allocatable :: ents(:), names(:), files(:)

	if (em%nloc < 1) return

	tys = [em%t_ptr, em%t_i32, em%t_i32, em%t_ptr]
	lty = LLVMStructTypeInContext(em%ctx, tys, 4, 0)

	allocate(ents(em%nloc), files(max(src_registry%len_, 1)))
	files = c_null_ptr

	do k = 1, em%nloc
		associate (src => em%loc_src(k), pos => em%loc_pos(k))

			if (src < 1 .or. src > src_registry%len_) then
				vals = [LLVMConstNull(em%t_ptr), ci32(em, 0), ci32(em, 0), LLVMConstNull(em%t_ptr)]
				ents(k) = LLVMConstStructInContext(em%ctx, vals, 4, 0)
				cycle
			end if

			associate (context => src_registry%v(src))

				! The same line lookup as rt_trace_snippet() makes
				i1 = maxloc(context%lines, context%lines <= pos)
				i = min(size(context%lines) - 1, max(1, i1(1)))

				last = context%lines(i+1) - 1
				do while (last > context%lines(i) .and. (context%text(last:last) == line_feed &
						.or. context%text(last:last) == carriage_return))
					last = last - 1
				end do
				text = tabs2spaces(context%text(context%lines(i): last))

				if (.not. c_associated(files(src))) files(src) = c_string(em, context%src_file)

				vals = [files(src), ci32(em, i), ci32(em, pos - context%lines(i) + 1), &
					c_string(em, text)]
				ents(k) = LLVMConstStructInContext(em%ctx, vals, 4, 0)

			end associate
		end associate
	end do

	gl = LLVMAddGlobal(em%llmod, LLVMArrayType2(lty, int(em%nloc, c_int64_t)), cs('.locs'))
	call LLVMSetInitializer(gl, LLVMConstArray(lty, ents, em%nloc))
	call LLVMSetGlobalConstant(gl, 1)
	call LLVMSetLinkage(gl, LLVM_PRIVATE_LINKAGE)

	nfn = 0
	if (allocated(em%fnames)) nfn = size(em%fnames)
	! A fn id is one-based, so the table has an empty entry first
	allocate(names(nfn + 1))
	names = LLVMConstNull(em%t_ptr)
	do i = 1, nfn
		if (allocated(em%fnames(i)%s)) names(i + 1) = c_string(em, em%fnames(i)%s)
	end do
	gn = LLVMAddGlobal(em%llmod, LLVMArrayType2(em%t_ptr, int(nfn + 1, c_int64_t)), &
		cs('.fnames'))
	call LLVMSetInitializer(gn, LLVMConstArray(em%t_ptr, names, nfn + 1))
	call LLVMSetGlobalConstant(gn, 1)
	call LLVMSetLinkage(gn, LLVM_PRIVATE_LINKAGE)

	! The call goes in the block of the allocas, which runs first
	saved = LLVMGetInsertBlock(em%bld)
	call LLVMPositionBuilderAtEnd(em%bld, em%ablk)
	call rt_void(em, 'sy_set_trace', [gl, ci32(em, em%nloc), gn, ci32(em, nfn + 1)])
	call LLVMPositionBuilderAtEnd(em%bld, saved)

end subroutine trace_tables

!===============================================================================
! Building blocks

function ci32(em, x) result(v)
	type(nemitter_t), intent(in) :: em
	integer, intent(in) :: x
	type(c_ptr) :: v
	v = LLVMConstInt(em%t_i32, int(x, c_int64_t), 1)
end function ci32

function ci64(em, x) result(v)
	type(nemitter_t), intent(in) :: em
	integer(kind = 8), intent(in) :: x
	type(c_ptr) :: v
	v = LLVMConstInt(em%t_i64, int(x, c_int64_t), 1)
end function ci64

function cbool(em, x) result(v)
	type(nemitter_t), intent(in) :: em
	logical, intent(in) :: x
	type(c_ptr) :: v
	v = LLVMConstInt(em%t_i1, merge(1_c_int64_t, 0_c_int64_t, x), 0)
end function cbool

!===============================================================================

function ll_type(em, ty) result(t)

	! The LLVM type that a variable of a syntran type is stored as

	type(nemitter_t), intent(in) :: em
	integer, intent(in) :: ty
	type(c_ptr) :: t

	select case (ty)
	case (i64_type)
		t = em%t_i64
	case (f32_type)
		t = em%t_f32
	case (f64_type)
		t = em%t_f64
	case (bool_type)
		t = em%t_i1
	case (str_type, array_type, struct_type, fn_type, file_type)
		t = em%t_ptr
	case default
		t = em%t_i32
	end select

end function ll_type

!===============================================================================

function ptr_type(ty) result(p)
	! Is a value of this type a heap object that its holder owns?
	integer, intent(in) :: ty
	logical :: p
	p = ty == str_type .or. ty == array_type .or. ty == struct_type .or. ty == file_type
end function ptr_type

!===============================================================================

function free_fn(ty) result(name)
	! The runtime fn that frees a value of a heap type
	integer, intent(in) :: ty
	character(len = :), allocatable :: name
	select case (ty)
	case (str_type)
		name = 'sy_str_free'
	case (struct_type)
		name = 'sy_struct_free'
	case (file_type)
		name = 'sy_file_free'
	case default
		name = 'sy_arr_free'
	end select
end function free_fn

!===============================================================================

function dup_fn(ty) result(name)
	! The runtime fn that copies a value of a heap type so that it can be owned
	integer, intent(in) :: ty
	character(len = :), allocatable :: name
	select case (ty)
	case (str_type)
		name = 'sy_str_dup'
	case (struct_type)
		name = 'sy_struct_dup'
	case (file_type)
		name = 'sy_file_dup'
	case default
		name = 'sy_arr_dup'
	end select
end function dup_fn

!===============================================================================

recursive function may_clobber(node) result(c)

	! Whether evaluating an expression can write to a variable: it calls a user fn,
	! a method, or a fn pointer, or it assigns something.  A str or array that is
	! only borrowed from a variable (load_var()) is not safe to hold across that,
	! because the variable may be reassigned, which frees what it held.  The
	! interpreter copies an operand as it evaluates it, so it never sees this

	type(syntax_node_t), intent(in) :: node
	logical :: c

	integer :: i

	c = .true.
	select case (node%kind)
	case (fn_call_expr, method_call_expr, fn_call_ptr_expr, assignment_expr)
		return
	case (fn_call_intr_expr)
		! std::move() leaves its variable empty, which frees what it held
		if (node%identifier%text == 'move') return
	end select

	c = .false.
	if (allocated(node%left))   c = may_clobber(node%left)
	if (c) return
	if (allocated(node%right))  c = may_clobber(node%right)
	if (c) return
	if (allocated(node%member)) c = may_clobber(node%member)
	if (c) return
	if (allocated(node%lbound_)) c = may_clobber(node%lbound_)
	if (c) return
	if (allocated(node%step))    c = may_clobber(node%step)
	if (c) return
	if (allocated(node%ubound_)) c = may_clobber(node%ubound_)
	if (c) return
	if (allocated(node%len_))    c = may_clobber(node%len_)
	if (c) return

	if (allocated(node%args)) then
		do i = 1, size(node%args)
			if (may_clobber(node%args(i))) c = .true.
		end do
	end if
	if (c) return
	if (allocated(node%elems)) then
		do i = 1, size(node%elems)
			if (may_clobber(node%elems(i))) c = .true.
		end do
	end if
	if (c) return
	if (allocated(node%members)) then
		do i = 1, size(node%members)
			if (may_clobber(node%members(i))) c = .true.
		end do
	end if
	if (c) return
	if (allocated(node%lsubscripts)) then
		do i = 1, size(node%lsubscripts)
			if (may_clobber(node%lsubscripts(i))) c = .true.
		end do
	end if

end function may_clobber

!===============================================================================

function lent_unsafe(node, i, any_ref) result(u)

	! Whether the by-value argument `i` of a call, which a variable only lends to
	! the callee, can change before the callee is done with it.  That's so if the
	! callee can assign the variable (a global), if a by-reference argument can
	! be the same variable, or if a later argument is evaluated by something that
	! can write to it

	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	logical, intent(in) :: any_ref
	logical :: u

	u = any_ref .or. .not. node%args(i)%is_loc .or. later_clobbers(node, i)

end function lent_unsafe

!===============================================================================

function later_clobbers(node, i) result(c)

	! Whether any argument of a call after the `i`th can write to a variable

	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	logical :: c

	integer :: j

	c = .false.
	do j = i + 1, size(node%args)
		if (may_clobber(node%args(j))) c = .true.
	end do

end function later_clobbers

!===============================================================================

subroutine own_if_borrowed(em, v)

	! Copy a str or array that is only lent by a variable, so that it stays as it
	! is while later operands or arguments are evaluated.  Then `v` is owned, and
	! whatever consumes it frees the copy

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(inout) :: v

	if (.not. ptr_type(v%ty) .or. v%owned) return
	v%v = rt_call(em, dup_fn(v%ty), em%t_ptr, [v%v])
	v%owned = .true.

end subroutine own_if_borrowed

!===============================================================================

recursive function emit_arg(em, node, i) result(r)

	! The `i`th argument of a call, as emit_expr() makes it, but owned if a later
	! argument can write to the variable that it's lent by, c.f. may_clobber()

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: i
	type(nval_t) :: r

	r = emit_expr(em, node%args(i))
	if (later_clobbers(node, i)) call own_if_borrowed(em, r)

end function emit_arg

!===============================================================================

function elem_code(ty) result(c)

	! The runtime's code for the type of an array's elements, c.f. the enum in
	! syntran_crt.c

	integer, intent(in) :: ty
	integer :: c

	select case (ty)
	case (i64_type)
		c = 1
	case (f32_type)
		c = 2
	case (f64_type)
		c = 3
	case (bool_type)
		c = 4
	case (str_type)
		c = 5
	case (array_type)
		c = 6
	case (struct_type)
		c = 7
	case default
		c = 0
	end select

end function elem_code

!===============================================================================

function elem_ll(em, ty) result(t)
	! How an element of an array is stored.  A bool is a byte, not an i1
	type(nemitter_t), intent(in) :: em
	integer, intent(in) :: ty
	type(c_ptr) :: t
	if (ty == bool_type) then
		t = em%t_i8
	else
		t = ll_type(em, ty)
	end if
end function elem_ll

!===============================================================================

function arr_val(v, owned, node_val) result(r)

	! An array value from the LLVM pointer, with its element type and rank from
	! the syntran value that it's of

	type(c_ptr), intent(in) :: v
	logical, intent(in) :: owned
	type(value_t), intent(in) :: node_val
	type(nval_t) :: r

	r%v = v
	r%ty = array_type
	r%owned = owned
	if (allocated(node_val%array)) then
		r%elem = node_val%array%type
		r%rank = node_val%array%rank
	end if

end function arr_val

!===============================================================================

function dummy_val(em, ty) result(r)

	! A value to carry on with after something unsupported was reported, which
	! the types of the rest of the statement still agree with.  Nothing is
	! compiled once there is a diagnostic

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: ty
	type(nval_t) :: r

	r%ty = ty
	r%owned = .false.

	select case (ty)
	case (i64_type)
		r%v = ci64(em, 0_8)
	case (f32_type)
		r%v = LLVMConstReal(em%t_f32, 0.0_c_double)
	case (f64_type)
		r%v = LLVMConstReal(em%t_f64, 0.0_c_double)
	case (bool_type)
		r%v = cbool(em, .false.)
	case (str_type)
		r%v = str_literal(em, '')
	case (array_type, struct_type, null_type, fn_type, file_type)
		r%v = LLVMConstNull(em%t_ptr)
	case default
		r%ty = i32_type
		r%v = ci32(em, 0)
	end select

end function dummy_val

!===============================================================================

function str_literal(em, s) result(p)

	! A string literal is a constant of the program, which is immortal so that
	! freeing it does nothing.  It has the layout of `sy_str` in the runtime:
	! length, immortal flag, and the characters with a NUL after them

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: s
	type(c_ptr) :: p

	!********

	type(c_ptr) :: elems(3), vals(3), sty, g

	elems(1) = em%t_i64
	elems(2) = em%t_i64
	elems(3) = LLVMArrayType2(em%t_i8, int(len(s) + 1, c_int64_t))
	sty = LLVMStructTypeInContext(em%ctx, elems, 3, 0)

	vals(1) = ci64(em, int(len(s), 8))
	vals(2) = ci64(em, 1_8)
	vals(3) = LLVMConstStringInContext(em%ctx, s, int(len(s), c_int), 0)

	g = LLVMAddGlobal(em%llmod, sty, cs('.str.'//str(em%nstr)))
	em%nstr = em%nstr + 1
	call LLVMSetInitializer(g, LLVMConstStructInContext(em%ctx, vals, 3, 0))
	call LLVMSetGlobalConstant(g, 1)
	call LLVMSetLinkage(g, LLVM_PRIVATE_LINKAGE)

	p = g

end function str_literal

!===============================================================================

function rt_call(em, name, ret, args) result(v)

	! Call a function of the runtime, or of libm.  It's declared at its first
	! call, with the types of the arguments that it's called with

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr), intent(in) :: ret
	type(c_ptr), intent(in) :: args(:)
	type(c_ptr) :: v

	!********

	integer :: i, n

	type(c_ptr) :: fn, fty
	type(c_ptr), allocatable :: ptys(:)

	n = size(args)
	allocate(ptys(max(n, 1)))
	do i = 1, n
		ptys(i) = LLVMTypeOf(args(i))
	end do
	fty = LLVMFunctionType(ret, ptys, n, 0)

	fn = LLVMGetNamedFunction(em%llmod, cs(name))
	if (.not. c_associated(fn)) fn = LLVMAddFunction(em%llmod, cs(name), fty)

	v = LLVMBuildCall2(em%bld, fty, fn, args, n, cs(''))

end function rt_call

!===============================================================================

subroutine rt_void(em, name, args)
	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr), intent(in) :: args(:)
	type(c_ptr) :: ignored
	ignored = rt_call(em, name, em%t_void, args)
end subroutine rt_void

!===============================================================================

function entry_alloca(em, ty, name) result(p)

	! A stack slot, which goes in the alloca block of the fn

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: ty
	character(len = *), intent(in) :: name
	type(c_ptr) :: p

	p = LLVMBuildAlloca(em%abld, ty, cs(name))

end function entry_alloca

!===============================================================================

subroutine dispose(em, v)

	! Free a value that was made, if it's still owned

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v

	if (.not. v%owned) return
	if (ptr_type(v%ty)) call rt_void(em, free_fn(v%ty), [v%v])

end subroutine dispose

!===============================================================================

function fp_to_int(em, v, to) result(r)

	! A float to an integer, truncated.  What it does when the float is out of
	! range or NaN is undefined in LLVM, so it is whatever the machine does, like
	! the interpreter: x86 gives the smallest integer, and others saturate and
	! give 0 for NaN

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: v
	integer, intent(in) :: to
	type(c_ptr) :: r

	!********

	logical :: from32

	real(c_double) :: lo, hi

	type(c_ptr) :: ity, fty, ok, small

	from32 = c_associated(LLVMTypeOf(v), em%t_f32)
	ity = merge(em%t_i32, em%t_i64, to == i32_type)
	fty = merge(em%t_f32, em%t_f64, from32)

	if (to == i32_type) then
		r = rt_call(em, merge('llvm.fptosi.sat.i32.f32', 'llvm.fptosi.sat.i32.f64', from32), ity, [v])
	else
		r = rt_call(em, merge('llvm.fptosi.sat.i64.f32', 'llvm.fptosi.sat.i64.f64', from32), ity, [v])
	end if
	if (.not. em%x86) return

	if (to == i32_type) then
		hi = 2147483648.0_c_double
		lo = merge(-2147483648.0_c_double, -2147483649.0_c_double, from32)
	else
		hi = 9223372036854775808.0_c_double
		lo = -9223372036854775808.0_c_double
	end if

	! Everything that is not in range, including NaN, since the compares are ordered
	ok = LLVMBuildAnd(em%bld, &
		LLVMBuildFCmp(em%bld, merge(LLVM_REAL_OGE, LLVM_REAL_OGT, to == i64_type .or. from32), &
			v, LLVMConstReal(fty, lo), cs('')), &
		LLVMBuildFCmp(em%bld, LLVM_REAL_OLT, v, LLVMConstReal(fty, hi), cs('')), cs(''))
	small = LLVMConstInt(ity, ishft(1_c_int64_t, merge(31, 63, to == i32_type)), 0)
	r = LLVMBuildSelect(em%bld, ok, r, small, cs(''))

end function fp_to_int

!===============================================================================

function convert(em, v, to) result(r)

	! Convert a number to another type of number

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v
	integer, intent(in) :: to
	type(nval_t) :: r

	r%ty = to
	r%owned = .false.
	r%v = v%v

	if (v%ty == to) return

	select case (to)
	case (i32_type)
		select case (v%ty)
		case (i64_type)
			r%v = LLVMBuildTrunc(em%bld, v%v, em%t_i32, cs(''))
		case (f32_type, f64_type)
			r%v = fp_to_int(em, v%v, i32_type)
		end select

	case (i64_type)
		select case (v%ty)
		case (i32_type)
			r%v = LLVMBuildSExt(em%bld, v%v, em%t_i64, cs(''))
		case (f32_type, f64_type)
			r%v = fp_to_int(em, v%v, i64_type)
		end select

	case (f32_type)
		select case (v%ty)
		case (i32_type, i64_type)
			r%v = LLVMBuildSIToFP(em%bld, v%v, em%t_f32, cs(''))
		case (f64_type)
			r%v = LLVMBuildFPTrunc(em%bld, v%v, em%t_f32, cs(''))
		end select

	case (f64_type)
		select case (v%ty)
		case (i32_type, i64_type)
			r%v = LLVMBuildSIToFP(em%bld, v%v, em%t_f64, cs(''))
		case (f32_type)
			r%v = LLVMBuildFPExt(em%bld, v%v, em%t_f64, cs(''))
		end select
	end select

end function convert

!===============================================================================

function str_of(em, v) result(s)

	! The string of a value, like str() makes.  This consumes `v`.  The result
	! is owned, unless it's the same string that was given, which still belongs
	! to whoever has it

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v
	type(nval_t) :: s

	s%ty = str_type
	s%owned = .true.

	select case (v%ty)
	case (str_type)
		s = v
	case (i32_type)
		s%v = rt_call(em, 'sy_str_i32', em%t_ptr, [v%v])
	case (i64_type)
		s%v = rt_call(em, 'sy_str_i64', em%t_ptr, [v%v])
	case (f32_type)
		s%v = rt_call(em, 'sy_str_f32', em%t_ptr, [v%v])
	case (f64_type)
		s%v = rt_call(em, 'sy_str_f64', em%t_ptr, [v%v])
	case (bool_type)
		s%v = rt_call(em, 'sy_str_bool', em%t_ptr, &
			[LLVMBuildZExt(em%bld, v%v, em%t_i8, cs(''))])
	case (array_type)
		if (v%elem == enum_type) then
			s%v = rt_call(em, 'sy_arr_str_names', em%t_ptr, [v%v, enum_names(em, v%sk)])
		else
			s%v = rt_call(em, 'sy_arr_str', em%t_ptr, [v%v])
		end if
		call dispose(em, v)
	case (struct_type)
		s%v = rt_call(em, 'sy_struct_str', em%t_ptr, [v%v])
		call dispose(em, v)
	case (file_type)
		s%v = rt_call(em, 'sy_file_str', em%t_ptr, [v%v])
		call dispose(em, v)
	case (null_type)
		s%v = str_literal(em, 'null')
	case (fn_type)
		! A fn pointer is its type, not where it points to
		s%v = str_literal(em, v%tname)
	case (enum_type)
		s%v = LLVMBuildLoad2(em%bld, em%t_ptr, LLVMBuildGEP2(em%bld, em%t_ptr, &
			enum_names(em, v%sk), [v%v], 1, cs('')), cs(''))
	case default
		call unsupported(em, 'a value of type `'//kind_name(v%ty)//'`')
		s = dummy_val(em, str_type)
	end select

end function str_of

!===============================================================================
! Variables

function supported_type(ty) result(ok)

	! Can a variable or parameter have this type?

	integer, intent(in) :: ty
	logical :: ok

	ok = is_numeric_type(ty) .or. ty == bool_type .or. ty == str_type

end function supported_type

!===============================================================================

function holdable(ty) result(ok)

	! Can a variable, parameter, or element of an array have this type?  A struct
	! can, but nothing is computed with one, unlike with the types above

	integer, intent(in) :: ty
	logical :: ok

	ok = supported_type(ty) .or. ty == struct_type .or. ty == enum_type .or. ty == fn_type .or. ty == file_type

end function holdable

!===============================================================================

recursive function supported_val(val) result(ok)

	! Can a variable or parameter be of the type of this value?  An array of
	! numbers, bools, or strs can, and so can a fn pointer if every type in its
	! signature can

	type(value_t), intent(in) :: val
	logical :: ok

	integer :: i

	if (val%type == fn_type) then
		ok = .true.
		if (allocated(val%fn_params)) then
			do i = 1, size(val%fn_params)
				if (.not. supported_val(val%fn_params(i))) ok = .false.
			end do
		end if
		if (allocated(val%fn_ret)) then
			if (val%fn_ret%type /= void_type) then
				if (.not. supported_val(val%fn_ret)) ok = .false.
			end if
		end if
	else if (val%type /= array_type) then
		ok = holdable(val%type)
	else if (.not. allocated(val%array)) then
		ok = .false.
	else
		! A file is not copied by the runtime when it's in an array
		ok = holdable(val%array%type) .and. val%array%type /= file_type
	end if

end function supported_val

!===============================================================================

subroutine slots_ensure(s, id)

	! Make room for slot `id`

	type(slots_t), intent(inout) :: s
	integer, intent(in) :: id

	integer :: n, i
	integer, allocatable :: itmp(:)
	logical, allocatable :: ltmp(:)
	type(c_ptr), allocatable :: ptmp(:)
	type(fnret_t), allocatable :: ftmp(:)

	if (.not. allocated(s%p)) then
		n = max(id, 16)
		allocate(s%p(n), s%ty(n), s%own(n), s%elem(n), s%rank(n), s%sk(n), s%fnret(n))
		s%p    = c_null_ptr
		s%ty   = unknown_type
		s%own  = .false.
		s%elem = unknown_type
		s%rank = 0
		s%sk   = 0
		return
	end if

	if (id <= size(s%p)) return

	n = max(id, 2 * size(s%p))
	allocate(ptmp(n), itmp(n), ltmp(n))
	ptmp = c_null_ptr
	itmp = unknown_type
	ltmp = .false.
	ptmp(1: size(s%p)) = s%p
	itmp(1: size(s%ty)) = s%ty
	ltmp(1: size(s%own)) = s%own
	call move_alloc(ptmp, s%p)
	call move_alloc(itmp, s%ty)
	call move_alloc(ltmp, s%own)

	allocate(itmp(n))
	itmp = unknown_type
	itmp(1: size(s%elem)) = s%elem
	call move_alloc(itmp, s%elem)
	allocate(itmp(n))
	itmp = 0
	itmp(1: size(s%rank)) = s%rank
	call move_alloc(itmp, s%rank)
	allocate(itmp(n))
	itmp = 0
	itmp(1: size(s%sk)) = s%sk
	call move_alloc(itmp, s%sk)

	allocate(ftmp(n))
	do i = 1, size(s%fnret)
		if (allocated(s%fnret(i)%v)) call move_alloc(s%fnret(i)%v, ftmp(i)%v)
	end do
	call move_alloc(ftmp, s%fnret)

end subroutine slots_ensure

!===============================================================================

function var_ptr(em, node, ty) result(p)

	! Where a variable is stored.  It's made at its first reference, with the
	! type `ty`: a global is an LLVM global, and a local is a stack slot in the
	! alloca block of the fn, with a null string in it so that it can always be
	! freed

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: ty
	type(c_ptr) :: p

	!********

	integer :: id

	type(c_ptr) :: g, ignored

	id = node%id_index
	p = c_null_ptr

	if (id < 1) return

	if (.not. holdable(ty) .and. .not. (ty == array_type .and. supported_val(node%val))) then
		call unsupported(em, 'a variable of type `'//kind_name(ty)//'`')
		return
	end if

	if (node%is_loc) then
		call slots_ensure(em%l, id)
		if (.not. c_associated(em%l%p(id))) then
			em%l%p(id) = entry_alloca(em, ll_type(em, ty), 'l'//str(id))
			if (ptr_type(ty)) then
				ignored = LLVMBuildStore(em%abld, LLVMConstNull(em%t_ptr), em%l%p(id))
			end if
			em%l%ty(id)  = ty
			em%l%own(id) = ptr_type(ty)
			call slot_array_info(em, em%l, id, node%val)
		end if
		p = em%l%p(id)

	else
		call slots_ensure(em%g, id)
		if (.not. c_associated(em%g%p(id))) call make_global(em, id, ty, node%val)
		p = em%g%p(id)
	end if

end function var_ptr

!===============================================================================

subroutine slot_array_info(em, s, id, val)

	! Remember the type and rank of the elements of the array that a slot holds,
	! and the struct that it or its elements are

	type(nemitter_t), intent(in) :: em
	type(slots_t), intent(inout) :: s
	integer, intent(in) :: id
	type(value_t), intent(in) :: val

	if (associated(em%structs)) then
		if (val%type == struct_type) then
			s%sk(id) = structs_slot_of(em%structs, val)
		else if (val%type == array_type .and. allocated(val%array)) then
			if (val%array%type == struct_type) s%sk(id) = structs_slot_of(em%structs, val)
		else if (val%type == fn_type .and. allocated(val%fn_ret)) then
			! For a fn pointer, the struct that it returns
			if (val%fn_ret%type == struct_type) s%sk(id) = structs_slot_of(em%structs, val%fn_ret)
		end if
	end if

	if (val%type == fn_type .and. allocated(val%fn_ret)) then
		! The copy of a value is a defined assignment, which needs the target to exist
		if (.not. allocated(s%fnret(id)%v)) allocate(s%fnret(id)%v)
		s%fnret(id)%v = val%fn_ret
	end if

	if (val%type /= array_type) return
	if (.not. allocated(val%array)) return
	s%elem(id) = val%array%type
	s%rank(id) = val%array%rank

end subroutine slot_array_info

!===============================================================================

subroutine make_global(em, id, ty, val)

	! A global variable of the type `ty`, which is zero until it's assigned.
	! Its value is only used to describe an array

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: id, ty
	type(value_t), intent(in) :: val

	type(c_ptr) :: g

	g = LLVMAddGlobal(em%llmod, ll_type(em, ty), cs('g'//str(id)))
	call LLVMSetInitializer(g, LLVMConstNull(ll_type(em, ty)))
	call LLVMSetLinkage(g, LLVM_INTERNAL_LINKAGE)
	em%g%p(id)  = g
	em%g%ty(id) = ty
	call slot_array_info(em, em%g, id, val)

end subroutine make_global

!===============================================================================

function ref_slot(em, node) result(p)

	! Where the variable of a subscripted reference is stored.  Its `let` has
	! almost always been emitted, but a fn which uses a global is built before the
	! top-level statements, so it might not have been.  Then the variable's type is
	! in the parser's table

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(c_ptr) :: p

	integer :: id

	id = node%id_index
	p = c_null_ptr
	if (id < 1) return

	if (node%is_loc) then
		call slots_ensure(em%l, id)
		p = em%l%p(id)
		return
	end if

	call slots_ensure(em%g, id)
	if (.not. c_associated(em%g%p(id)) .and. associated(em%vars)) then
		if (allocated(em%vars%vals)) then
			if (id <= size(em%vars%vals)) then
				associate (val => em%vars%vals(id))
					if (supported_val(val)) call make_global(em, id, val%type, val)
				end associate
			end if
		end if
	end if
	p = em%g%p(id)

end function ref_slot

!===============================================================================

function slot_type(em, node) result(ty)
	! The type that a variable was made with
	type(nemitter_t), intent(in) :: em
	type(syntax_node_t), intent(in) :: node
	integer :: ty
	if (node%is_loc) then
		ty = em%l%ty(node%id_index)
	else
		ty = em%g%ty(node%id_index)
	end if
end function slot_type

!===============================================================================

function load_var(em, node) result(r)

	! Read a variable.  A string is only borrowed from the variable

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	type(c_ptr) :: p

	! The first slot is `std::PI`, which is a constant
	if (.not. node%is_loc .and. node%id_index == 1 .and. node%val%type == f64_type) then
		r%ty = f64_type
		r%owned = .false.
		r%v = LLVMConstReal(em%t_f64, 3.14159265358979323846_c_double)
		return
	end if

	! And the next three are `std::IN`, `std::OUT`, and `std::ERR`, which are
	! new handles of the standard files
	if (.not. node%is_loc .and. node%id_index >= 2 .and. node%id_index <= 4 &
			.and. node%val%type == file_type) then
		r%ty = file_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_file_std', em%t_ptr, [ci32(em, node%id_index - 2)])
		return
	end if

	p = var_ptr(em, node, node%val%type)
	if (.not. c_associated(p)) then
		r = dummy_val(em, node%val%type)
		return
	end if

	r%ty = slot_type(em, node)
	r%owned = .false.
	r%v = LLVMBuildLoad2(em%bld, ll_type(em, r%ty), p, cs(''))
	if (r%ty == array_type) r = arr_val(r%v, .false., node%val)

end function load_var

!===============================================================================

subroutine store_var(em, p, vty, v)

	! Store a value into a variable of the type `vty`.  A string variable owns
	! its string, so it takes the one that it's given if that is owned, or a
	! copy of it, and then frees the string that it had

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: p
	integer, intent(in) :: vty
	type(nval_t), intent(in) :: v

	!********

	type(c_ptr) :: old, new_
	type(c_ptr) :: ignored

	type(nval_t) :: cv

	if (ptr_type(vty)) then
		if (v%ty /= vty .and. .not. (v%ty == null_type .and. vty == struct_type)) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `'// &
				kind_name(vty)//'`')
			return
		end if
		if (v%owned) then
			new_ = v%v
		else
			new_ = rt_call(em, dup_fn(vty), em%t_ptr, [v%v])
		end if
		old = LLVMBuildLoad2(em%bld, em%t_ptr, p, cs(''))
		ignored = LLVMBuildStore(em%bld, new_, p)
		call rt_void(em, free_fn(vty), [old])

	else if (is_numeric_type(vty) .or. vty == bool_type .or. vty == enum_type) then
		if (v%ty /= vty .and. .not. (is_numeric_type(v%ty) .and. is_numeric_type(vty))) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `'// &
				kind_name(vty)//'`')
			return
		end if
		cv = convert(em, v, vty)
		ignored = LLVMBuildStore(em%bld, cv%v, p)

	else if (vty == fn_type) then
		! A fn pointer is only an address, which is not owned
		if (v%ty /= fn_type) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `'// &
				kind_name(vty)//'`')
			return
		end if
		ignored = LLVMBuildStore(em%bld, v%v, p)

	else
		call unsupported(em, 'a variable of type `'//kind_name(vty)//'`')
	end if

end subroutine store_var

!===============================================================================
! Expressions

recursive function emit_expr(em, node) result(r)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	select case (node%kind)

	case (literal_expr)
		r = emit_literal(em, node)

	case (name_expr, dot_expr)
		if (allocated(node%member)) then
			r = emit_member_read(em, node)
		else if (allocated(node%lsubscripts)) then
			r = emit_subscript(em, node)
		else
			r = load_var(em, node)
		end if

	case (array_expr)
		r = emit_array_expr(em, node)

	case (assignment_expr)
		r = emit_assign_val(em, node)

	case (let_expr)
		! A `let` used as a value, like `(let a = 1) + (let b = a)`.  Its value is
		! the variable it made
		call emit_let(em, node)
		r = load_var(em, node)

	case (binary_expr)
		r = emit_binary(em, node)

	case (unary_expr)
		r = emit_unary(em, node)

	case (fn_call_intr_expr)
		if (allocated(node%lsubscripts)) then
			r = emit_subscripted_call(em, node)
		else
			r = emit_intr_call(em, node)
		end if

	case (fn_call_expr, method_call_expr)
		if (allocated(node%lsubscripts)) then
			r = emit_subscripted_call(em, node)
		else
			r = emit_user_call(em, node)
		end if

	case (fn_ref_expr)
		! The address of a user fn, which is declared here if this is its first
		! reference
		r%ty = fn_type
		r%owned = .false.
		r%v = fn_llvm(em, node%id_index)
		if (.not. c_associated(r%v)) r = dummy_val(em, fn_type)

	case (fn_call_ptr_expr)
		if (allocated(node%lsubscripts)) then
			r = emit_subscripted_call(em, node)
		else
			r = emit_ptr_call(em, node)
		end if

	case (struct_instance_expr)
		r = emit_struct_instance(em, node)

	case (enum_access_expr)
		r = emit_enum_access(em, node)

	case (enum_cast_expr)
		r = emit_enum_cast(em, node)

	case default
		call unsupported(em, 'a `'//kind_name(node%kind)//'`')
		r = dummy_val(em, node%val%type)

	end select

	! An enum is only an index, so which enum it is has to go with it
	if (r%ty == enum_type .or. (r%ty == array_type .and. r%elem == enum_type)) then
		if (associated(em%enums)) r%sk = enums_slot_of(em%enums, node%val)
	end if

	! And a fn pointer's type is all that `str()` of it makes
	if (r%ty == fn_type) r%tname = value_type_name(node%val)

end function emit_expr

!===============================================================================

function emit_literal(em, node) result(r)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	r%ty = node%val%type
	r%owned = .false.

	select case (node%val%type)
	case (i32_type)
		r%v = ci32(em, node%val%sca%i32)
	case (i64_type)
		r%v = ci64(em, node%val%sca%i64)
	case (f32_type)
		r%v = LLVMConstReal(em%t_f32, real(node%val%sca%f32, c_double))
	case (f64_type)
		r%v = LLVMConstReal(em%t_f64, real(node%val%sca%f64, c_double))
	case (bool_type)
		r%v = cbool(em, node%val%sca%bool)
	case (str_type)
		r%v = str_literal(em, node%val%str%s)
	case (null_type)
		r%v = LLVMConstNull(em%t_ptr)
	case default
		call unsupported(em, 'a literal of type `'//kind_name(node%val%type)//'`')
		r = dummy_val(em, node%val%type)
	end select

end function emit_literal

!===============================================================================
! Arrays

function arr_data(em, a) result(d)

	! The pointer to the elements of an array.  It's the field at offset 16, c.f.
	! sy_arr in the runtime

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	type(c_ptr) :: d

	type(c_ptr) :: g

	g = LLVMBuildGEP2(em%bld, em%t_i8, a, [ci64(em, 16_8)], 1, cs(''))
	d = LLVMBuildLoad2(em%bld, em%t_ptr, g, cs(''))
	call tbaa(em, d, tb_hdr)

end function arr_data

!===============================================================================

function arr_len(em, a) result(n)
	! The total number of elements of an array, an i64, which is its first field
	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	type(c_ptr) :: n
	n = LLVMBuildLoad2(em%bld, em%t_i64, a, cs(''))
	call tbaa(em, n, tb_hdr)
end function arr_len

!===============================================================================

function arr_dim(em, a, k) result(n)

	! The extent of an array along the dimension `k`, which is an i64 value and
	! is zero-indexed.  The extents start at offset 24

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a, k
	type(c_ptr) :: n

	type(c_ptr) :: g, off

	off = LLVMBuildAdd(em%bld, LLVMBuildMul(em%bld, k, ci64(em, 8_8), cs('')), &
		ci64(em, 24_8), cs(''))
	g = LLVMBuildGEP2(em%bld, em%t_i8, a, [off], 1, cs(''))
	n = LLVMBuildLoad2(em%bld, em%t_i64, g, cs(''))
	call tbaa(em, n, tb_hdr)

end function arr_dim

!===============================================================================

function store_sizes(em, vals) result(p)

	! A stack array of i64s, to pass the shape of an array to the runtime

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: vals(:)
	type(c_ptr) :: p

	integer :: i

	type(c_ptr) :: slot, ignored

	p = entry_alloca(em, LLVMArrayType2(em%t_i64, int(max(size(vals), 1), c_int64_t)), 'dims')
	do i = 1, size(vals)
		slot = LLVMBuildGEP2(em%bld, em%t_i64, p, [ci32(em, i - 1)], 1, cs(''))
		ignored = LLVMBuildStore(em%bld, vals(i), slot)
	end do

end function store_sizes

!===============================================================================

function lin_index(em, a, idx) result(lin)

	! The position in storage of the element of an array at the zero-based
	! subscripts `idx`, which are i64s.  Storage is column-major

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	type(c_ptr), intent(in) :: idx(:)
	type(c_ptr) :: lin

	integer :: k

	type(c_ptr) :: bad_bb, good_bb, ignored, sz

	! Like the interpreter's, a subscript is only checked in a build that asks for
	! it.  The unsigned comparison is also true for a negative subscript
	if (bounds_check) then
		do k = 1, size(idx)
			sz = arr_dim(em, a, ci64(em, int(k - 1, 8)))
			bad_bb  = new_block(em, 'oob')
			good_bb = new_block(em, 'inbounds')
			ignored = LLVMBuildCondBr(em%bld, LLVMBuildICmp(em%bld, LLVM_INT_UGE, idx(k), sz, &
				cs('')), bad_bb, good_bb)
			call LLVMPositionBuilderAtEnd(em%bld, bad_bb)
			call rt_void(em, 'sy_oob', [idx(k), ci32(em, k), ci32(em, size(idx)), sz])
			ignored = LLVMBuildBr(em%bld, good_bb)
			call LLVMPositionBuilderAtEnd(em%bld, good_bb)
		end do
	end if

	lin = idx(size(idx))
	do k = size(idx) - 1, 1, -1
		lin = LLVMBuildAdd(em%bld, LLVMBuildMul(em%bld, lin, &
			arr_dim(em, a, ci64(em, int(k - 1, 8))), cs('')), idx(k), cs(''))
	end do

end function lin_index

!===============================================================================

function elem_ptr(em, a, ety, lin) result(p)
	! The address of the element at position `lin` of an array
	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a, lin
	integer, intent(in) :: ety
	type(c_ptr) :: p
	p = LLVMBuildGEP2(em%bld, elem_ll(em, ety), arr_data(em, a), [lin], 1, cs(''))
end function elem_ptr

!===============================================================================

function elem_at(em, d, ety, lin) result(p)
	! The address of the element at position `lin` of an array whose data pointer
	! `d` was loaded already.  Loops load it once before they start, because the
	! stores in the loop might alias the array header, which would otherwise make
	! LLVM reload `d` in every iteration and stop it from vectorizing.  An array
	! never gets new data in its lifetime
	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: d, lin
	integer, intent(in) :: ety
	type(c_ptr) :: p
	p = LLVMBuildGEP2(em%bld, elem_ll(em, ety), d, [lin], 1, cs(''))
end function elem_at

!===============================================================================

function elem_load(em, ety, p) result(r)

	! Read an element, which a string only lends

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: ety
	type(c_ptr), intent(in) :: p
	type(nval_t) :: r

	r%ty = ety
	r%owned = .false.
	if (ety == bool_type) then
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, LLVMBuildLoad2(em%bld, em%t_i8, p, cs('')), &
			LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
	else
		r%v = LLVMBuildLoad2(em%bld, ll_type(em, ety), p, cs(''))
		call tbaa(em, r%v, tbaa_elem(ety))
	end if

end function elem_load

!===============================================================================

subroutine elem_store(em, p, ety, v)

	! Write an element.  This consumes `v`

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: p
	integer, intent(in) :: ety
	type(nval_t), intent(in) :: v

	type(c_ptr) :: ignored

	type(nval_t) :: cv

	if (ptr_type(ety)) then
		call store_var(em, p, ety, v)

	else if (ety == bool_type) then
		if (v%ty /= bool_type) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `bool`')
			return
		end if
		ignored = LLVMBuildStore(em%bld, LLVMBuildZExt(em%bld, v%v, em%t_i8, cs('')), p)

	else if (is_numeric_type(ety) .and. is_numeric_type(v%ty)) then
		cv = convert(em, v, ety)
		ignored = LLVMBuildStore(em%bld, cv%v, p)
		call tbaa(em, ignored, tbaa_elem(ety))

	else if (ety == enum_type .and. v%ty == enum_type) then
		ignored = LLVMBuildStore(em%bld, v%v, p)

	else if (ety == fn_type .and. v%ty == fn_type) then
		ignored = LLVMBuildStore(em%bld, v%v, p)

	else
		call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `'// &
			kind_name(ety)//'`')
	end if

end subroutine elem_store

!===============================================================================

recursive function scalar_index(em, sub) result(i)

	! The i64 value of a subscript which is a single integer

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: sub
	type(c_ptr) :: i

	type(nval_t) :: v

	integer :: n_saved

	type(c_ptr) :: ignored
	type(c_ptr), allocatable :: tmp(:), saved(:)

	if (em%idx_mode == 2) then
		! Made already, before the value of the assignment
		em%idx_n = em%idx_n + 1
		i = LLVMBuildLoad2(em%bld, em%t_i64, em%idx_slots(em%idx_n), cs(''))
		return
	end if

	if (em%idx_mode == 1) then
		! The subscript expression is made outside of the recording, since the
		! subscripts inside of it (`a.b[k[0]]`) are not the ones to be replayed.
		! A copied-back method call in it also uses the slots
		n_saved = em%idx_n
		if (allocated(em%idx_slots)) then
			saved = em%idx_slots
		else
			allocate(saved(0))
		end if
		em%idx_mode = 0
		v = emit_expr(em, sub)
		em%idx_mode = 1
		em%idx_n = n_saved
		call move_alloc(saved, em%idx_slots)
	else
		v = emit_expr(em, sub)
	end if
	if (v%ty /= i32_type .and. v%ty /= i64_type) then
		call unsupported(em, 'a subscript of type `'//kind_name(v%ty)//'`')
		v = dummy_val(em, i64_type)
	end if
	v = convert(em, v, i64_type)
	i = v%v

	if (em%idx_mode == 1) then
		if (.not. allocated(em%idx_slots)) allocate(em%idx_slots(0))
		em%idx_n = em%idx_n + 1
		if (em%idx_n > size(em%idx_slots)) then
			allocate(tmp(max(2 * size(em%idx_slots), 4)))
			tmp(1: size(em%idx_slots)) = em%idx_slots
			call move_alloc(tmp, em%idx_slots)
		end if
		em%idx_slots(em%idx_n) = entry_alloca(em, em%t_i64, 'idx')
		ignored = LLVMBuildStore(em%bld, i, em%idx_slots(em%idx_n))
	end if

end function scalar_index

!===============================================================================

function all_scalar_lead(node, n) result(yes)
	! Are the first n subscripts of a reference single elements?
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: n
	logical :: yes
	yes = all(node%lsubscripts(1: n)%sub_kind == scalar_sub)
end function all_scalar_lead

!===============================================================================

recursive function all_scalar(node) result(yes)
	! Are all the subscripts of a reference single elements?
	type(syntax_node_t), intent(in) :: node
	logical :: yes
	integer :: i
	yes = .true.
	do i = 1, size(node%lsubscripts)
		if (node%lsubscripts(i)%sub_kind /= scalar_sub) yes = .false.
	end do
end function all_scalar

!===============================================================================

function ref_info(em, node, ps, vty, ety, rank_) result(ok)

	! Where the variable of a subscripted reference is, and its type, and the
	! type and rank of its elements if it's an array

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(c_ptr), intent(out) :: ps
	integer, intent(out) :: vty, ety, rank_
	logical :: ok

	integer :: id

	ok = .false.
	vty = unknown_type
	ety = unknown_type
	rank_ = 0

	ps = ref_slot(em, node)
	if (.not. c_associated(ps)) then
		call unsupported(em, 'a subscript of a variable whose type is unknown')
		return
	end if

	id = node%id_index
	if (node%is_loc) then
		vty   = em%l%ty(id)
		ety   = em%l%elem(id)
		rank_ = em%l%rank(id)
	else
		vty   = em%g%ty(id)
		ety   = em%g%elem(id)
		rank_ = em%g%rank(id)
	end if

	if (vty /= array_type .and. vty /= str_type) then
		call unsupported(em, 'a subscript of a variable that is not an array')
		return
	end if
	ok = .true.

end function ref_info

!===============================================================================

recursive function build_subs(em, node, pending) result(p)

	! The subscripts of a reference, as a stack array of the runtime's `sy_sub`
	! records, six 64-bit words each.  A subscript which is a vector of indices
	! is an array that may be owned, and is in `pending` for the caller to
	! dispose of after the call

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t), allocatable, intent(out) :: pending(:)
	type(c_ptr) :: p

	!********

	integer :: i, n, omit, kind_

	type(c_ptr) :: w(6)

	n = size(node%lsubscripts)
	allocate(pending(n))
	p = entry_alloca(em, LLVMArrayType2(em%t_i64, int(6 * n, c_int64_t)), 'subs')

	do i = 1, n
		associate (sub => node%lsubscripts(i))

			w = ci64(em, 0_8)
			omit = 0
			if (sub%lsub_omit) omit = omit + 1
			if (sub%usub_omit) omit = omit + 2

			select case (sub%sub_kind)

			case (scalar_sub)
				kind_ = 0
				w(2) = scalar_index(em, sub)

			case (range_sub, step_sub)
				kind_ = merge(1, 3, sub%sub_kind == range_sub)
				if (.not. sub%lsub_omit) w(2) = scalar_index(em, sub)
				if (.not. sub%usub_omit) w(3) = scalar_index(em, node%usubscripts(i))
				if (sub%sub_kind == step_sub) w(4) = scalar_index(em, node%ssubscripts(i))
				w(5) = ci64(em, int(omit, 8))

			case (all_sub)
				kind_ = 2

			case default
				! A vector of indices
				kind_ = 4
				pending(i) = emit_expr(em, sub)
				if (pending(i)%ty /= array_type) then
					call unsupported(em, 'a subscript of type `'//kind_name(pending(i)%ty)//'`')
					pending(i) = dummy_val(em, array_type)
				end if
				w(6) = pending(i)%v

			end select

			w(1) = ci64(em, int(kind_, 8))
			call put_words(em, p, 6 * (i - 1), w)

		end associate
	end do

end function build_subs

!===============================================================================

subroutine put_words(em, p, first, w)

	! Store words into a stack array of 64-bit words, from the zero-based
	! position `first`.  A pointer is stored in a word just as well

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: p, w(:)
	integer, intent(in) :: first

	integer :: i

	type(c_ptr) :: slot, ignored

	do i = 1, size(w)
		slot = LLVMBuildGEP2(em%bld, em%t_i64, p, [ci32(em, first + i - 1)], 1, cs(''))
		ignored = LLVMBuildStore(em%bld, w(i), slot)
	end do

end subroutine put_words

!===============================================================================

subroutine dispose_all(em, vals)
	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: vals(:)
	integer :: i
	do i = 1, size(vals)
		call dispose(em, vals(i))
	end do
end subroutine dispose_all

!===============================================================================

recursive function elem_target(em, node, ety) result(p)

	! The address of the element that a subscripted reference to an array names,
	! if it has one scalar subscript per dimension.  Otherwise it's null, and
	! this has said that it isn't supported

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(out) :: ety
	type(c_ptr) :: p

	!********

	integer :: i, vty, rank_

	type(c_ptr) :: ps, a
	type(c_ptr), allocatable :: idx(:)

	p = c_null_ptr

	if (.not. ref_info(em, node, ps, vty, ety, rank_)) return
	if (vty /= array_type) then
		call unsupported(em, 'a substring')
		return
	end if

	if (size(node%lsubscripts) /= rank_ .or. .not. all_scalar(node)) then
		call unsupported(em, 'a slice of an array')
		return
	end if

	a = LLVMBuildLoad2(em%bld, em%t_ptr, ps, cs(''))
	allocate(idx(rank_))
	do i = 1, rank_
		idx(i) = scalar_index(em, node%lsubscripts(i))
	end do
	p = elem_ptr(em, a, ety, lin_index(em, a, idx))

end function elem_target

!===============================================================================

recursive function emit_subscript(em, node) result(r)

	! A variable with subscripts: an element or slice of an array, or a character
	! or substring of a string

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: vty, ety, rank_

	type(c_ptr) :: ps, a

	r = dummy_val(em, node%val%type)
	if (.not. ref_info(em, node, ps, vty, ety, rank_)) return

	a = LLVMBuildLoad2(em%bld, em%t_ptr, ps, cs(''))
	r = subscript_val(em, a, vty, ety, rank_, node)

end function emit_subscript

!===============================================================================

function fnret_of(s, id, val) result(ok)

	! What the fn pointer in a slot returns, if it's known

	type(slots_t), intent(in) :: s
	integer, intent(in) :: id
	type(value_t), intent(out) :: val
	logical :: ok

	ok = .false.
	if (.not. allocated(s%fnret)) return
	if (id < 1 .or. id > size(s%fnret)) return
	if (.not. allocated(s%fnret(id)%v)) return
	val = s%fnret(id)%v
	ok = .true.

end function fnret_of

!===============================================================================

recursive function emit_subscripted_call(em, node) result(r)

	! `f(x)[i]` or `sum(a, 0)[i]`: the subscripts of a call.  The call is made
	! without them, and then they select from its result, which is freed after
	! what they select is copied out of it

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	logical :: ok

	type(nval_t) :: whole

	type(syntax_node_t) :: call_node

	call_node = node
	deallocate(call_node%lsubscripts)
	if (allocated(call_node%usubscripts)) deallocate(call_node%usubscripts)
	if (allocated(call_node%ssubscripts)) deallocate(call_node%ssubscripts)

	r = dummy_val(em, node%val%type)

	if (node%kind == fn_call_ptr_expr) then
		! The type of the call is part of the type of the pointer, and the type of
		! the node is that of what the subscripts select from it
		if (allocated(node%left)) then
			if (.not. find_fn_ret(node%left, call_node%val)) then
				call unsupported(em, 'a subscript of the result of a call of unknown type')
				return
			end if
		else
			! A variable, which remembers what its fn returns
			if (node%is_loc) then
				ok = fnret_of(em%l, node%id_index, call_node%val)
			else
				ok = fnret_of(em%g, node%id_index, call_node%val)
			end if
			if (.not. ok) then
				call unsupported(em, 'a subscript of the result of a call of unknown type')
				return
			end if
		end if
	end if

	whole = emit_expr(em, call_node)
	if (whole%ty /= array_type .and. whole%ty /= str_type) then
		call unsupported(em, 'a subscript of a fn result of type `'//kind_name(whole%ty)//'`')
		call dispose(em, whole)
		return
	end if

	if (whole%ty == array_type .and. whole%elem == unknown_type .and. all_scalar(node)) then
		! An intrinsic that makes an array: the parser typed the call as the
		! element that the subscripts select, which has one subscript per dimension
		whole%elem = node%val%type
		whole%rank = size(node%lsubscripts)
	end if

	r = subscript_val(em, whole%v, whole%ty, whole%elem, whole%rank, node)

	! What is selected may be a string or a struct that the array owns, or a
	! slice that is made of them, so it has to be copied before the array is freed
	if (whole%owned .and. .not. r%owned .and. ptr_type(r%ty)) then
		r%v = rt_call(em, dup_fn(r%ty), em%t_ptr, [r%v])
		r%owned = .true.
	end if
	call dispose(em, whole)

end function emit_subscripted_call

!===============================================================================

recursive function subscript_val(em, a, vty, ety, rank_, node) result(r)

	! The subscripts of `node` applied to the array or string `a`, which is
	! lent.  `vty`, `ety`, and `rank_` are the type of `a`, and the type and rank
	! of its elements.  An element that is a str or struct is lent as well

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	integer, intent(in) :: vty, ety, rank_
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: i, nsub

	type(c_ptr) :: p, subs, last
	type(c_ptr), allocatable :: idx(:)

	type(nval_t) :: el
	type(nval_t), allocatable :: pending(:)

	r = dummy_val(em, node%val%type)

	nsub = size(node%lsubscripts)

	if (vty == str_type) then
		if (nsub /= 1) then
			call unsupported(em, 'this subscript of a string')
			return
		end if
		subs = build_subs(em, node, pending)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_sub', em%t_ptr, [a, subs])
		call dispose_all(em, pending)

	else if (nsub == rank_ .and. all_scalar(node)) then
		allocate(idx(rank_))
		do i = 1, rank_
			idx(i) = scalar_index(em, node%lsubscripts(i))
		end do
		p = elem_ptr(em, a, ety, lin_index(em, a, idx))
		r = elem_load(em, ety, p)

	else if (nsub == rank_) then
		subs = build_subs(em, node, pending)
		r = arr_val(rt_call(em, 'sy_arr_slice', em%t_ptr, [a, subs]), .true., node%val)
		call dispose_all(em, pending)

	else if (nsub == rank_ + 1 .and. ety == str_type .and. .not. all_scalar_lead(node, rank_)) then
		! A substring of each of the elements of a slice of an array of strings
		subs = build_subs(em, node, pending)
		r = arr_val(rt_call(em, 'sy_arr_str_sub', em%t_ptr, [a, subs]), .true., node%val)
		r%elem = str_type
		r%rank = count(node%lsubscripts(1: rank_)%sub_kind /= scalar_sub)
		call dispose_all(em, pending)

	else if (nsub == rank_ + 1 .and. ety == str_type) then
		! A character, or substring, of an element of an array of strings
		allocate(idx(rank_))
		do i = 1, rank_
			idx(i) = scalar_index(em, node%lsubscripts(i))
		end do
		p = elem_ptr(em, a, ety, lin_index(em, a, idx))
		el = elem_load(em, ety, p)
		subs = build_subs(em, node, pending)
		last = LLVMBuildGEP2(em%bld, em%t_i64, subs, [ci32(em, 6 * rank_)], 1, cs(''))
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_sub', em%t_ptr, [el%v, last])
		call dispose_all(em, pending)

	else
		call unsupported(em, 'a subscript with the wrong number of dimensions')
	end if

end function subscript_val

!===============================================================================

recursive function emit_array_expr(em, node) result(r)

	! An array literal, one of the kinds that an array_expr has

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	character(len = :), allocatable :: sfx

	integer :: t, kind_, i, n

	logical :: in_for

	type(c_ptr) :: sizes, valp, ignored, total, off, a, d, p
	type(c_ptr), allocatable :: dims(:)

	type(nval_t) :: lb, ub, st, v
	type(nval_t), allocatable :: vals(:)

	r = arr_val(LLVMConstNull(em%t_ptr), .false., node%val)

	in_for = em%for_hdr
	em%for_hdr = .false.

	if (.not. allocated(node%val%array)) then
		call unsupported(em, 'an array of unknown type')
		return
	end if
	t = node%val%array%type
	kind_ = node%val%array%kind

	if (.not. holdable(t)) then
		call unsupported(em, 'an array of type `'//kind_name(t)//'`')
		return
	end if

	r%owned = .true.

	select case (kind_)

	case (bound_array)
		if (t /= i32_type .and. t /= i64_type) then
			call unsupported(em, 'a range of type `'//kind_name(t)//'`')
			r%owned = .false.
			return
		end if
		sfx = merge('i32', 'i64', t == i32_type)
		lb = convert(em, emit_expr(em, node%lbound_), t)
		ub = convert(em, emit_expr(em, node%ubound_), t)
		r%v = rt_call(em, 'sy_arr_range_'//sfx, em%t_ptr, [lb%v, ub%v])

	case (step_array)
		select case (t)
		case (i32_type)
			sfx = 'i32'
		case (i64_type)
			sfx = 'i64'
		case (f32_type)
			sfx = 'f32'
		case (f64_type)
			sfx = 'f64'
		case default
			call unsupported(em, 'a range of type `'//kind_name(t)//'`')
			r%owned = .false.
			return
		end select
		lb = convert(em, emit_expr(em, node%lbound_), t)
		st = convert(em, emit_expr(em, node%step), t)
		ub = convert(em, emit_expr(em, node%ubound_), t)
		if (in_for) then
			call step_check(em, st, 'for loop', 'R23', 'R24')
		else
			call step_check(em, st, 'array', 'R25', 'R26')
		end if
		r%v = rt_call(em, 'sy_arr_step_'//sfx, em%t_ptr, [lb%v, st%v, ub%v])

	case (len_array)
		if (t /= f32_type .and. t /= f64_type) then
			call unsupported(em, 'a range of type `'//kind_name(t)//'`')
			r%owned = .false.
			return
		end if
		sfx = merge('f32', 'f64', t == f32_type)
		lb = convert(em, emit_expr(em, node%lbound_), t)
		ub = convert(em, emit_expr(em, node%ubound_), t)
		st = convert(em, emit_expr(em, node%len_), i64_type)
		r%v = rt_call(em, 'sy_arr_linspace_'//sfx, em%t_ptr, [lb%v, ub%v, st%v])

	case (unif_array)
		! [v; n, m] is v repeated n * m times, in the shape [n, m]
		if (node%lbound_%kind == literal_expr .and. (node%lbound_%val%type == struct_type &
				.or. node%lbound_%val%type == enum_type)) then
			! `[Node; 0]`: the parser only makes it for sizes that are all zero, so
			! the fill is never copied and is just a placeholder of the right type
			v = dummy_val(em, t)
		else
			v = emit_expr(em, node%lbound_)
		end if
		if (is_numeric_type(t)) v = convert(em, v, t)
		valp = entry_alloca(em, elem_ll(em, t), 'fill')
		if (t == bool_type) then
			ignored = LLVMBuildStore(em%bld, LLVMBuildZExt(em%bld, v%v, em%t_i8, cs('')), valp)
		else
			ignored = LLVMBuildStore(em%bld, v%v, valp)
		end if

		n = size(node%size_)
		allocate(dims(n))
		do i = 1, n
			dims(i) = scalar_index(em, node%size_(i))
		end do
		sizes = store_sizes(em, dims)
		r%v = rt_call(em, 'sy_arr_fill', em%t_ptr, [ci32(em, elem_code(t)), ci32(em, n), &
			sizes, valp])
		call dispose(em, v)

	case (expl_array, size_array)
		! The elements are evaluated first, because one that is itself an array is
		! spliced in, and the length of the whole has to be known to make it
		n = size(node%elems)
		allocate(vals(max(n, 1)))
		total = ci64(em, 0_8)
		do i = 1, n
			vals(i) = emit_expr(em, node%elems(i))
			if (vals(i)%ty == array_type) then
				if (vals(i)%elem /= t) then
					call unsupported(em, 'an array of arrays of a different type')
					call dispose(em, vals(i))
					vals(i) = dummy_val(em, t)
					total = LLVMBuildAdd(em%bld, total, ci64(em, 1_8), cs(''))
				else
					total = LLVMBuildAdd(em%bld, total, arr_len(em, vals(i)%v), cs(''))
				end if
			else
				total = LLVMBuildAdd(em%bld, total, ci64(em, 1_8), cs(''))
			end if
		end do

		sizes = store_sizes(em, [total])
		a = rt_call(em, 'sy_arr_new', em%t_ptr, [ci32(em, elem_code(t)), ci32(em, 1), sizes])
		d = arr_data(em, a)

		off = ci64(em, 0_8)
		do i = 1, n
			if (vals(i)%ty == array_type) then
				call rt_void(em, 'sy_arr_copy_into', [a, off, vals(i)%v])
				off = LLVMBuildAdd(em%bld, off, arr_len(em, vals(i)%v), cs(''))
				call dispose(em, vals(i))
			else
				p = LLVMBuildGEP2(em%bld, elem_ll(em, t), d, [off], 1, cs(''))
				call elem_store(em, p, t, vals(i))
				off = LLVMBuildAdd(em%bld, off, ci64(em, 1_8), cs(''))
			end if
		end do

		r%v = a
		if (kind_ == size_array) then
			n = size(node%size_)
			if (allocated(dims)) deallocate(dims)
			allocate(dims(n))
			do i = 1, n
				dims(i) = scalar_index(em, node%size_(i))
			end do
			sizes = store_sizes(em, dims)
			call rt_void(em, 'sy_expl_check', [total, ci32(em, n), sizes])
			r%v = rt_call(em, 'sy_arr_reshape', em%t_ptr, [a, ci32(em, n), sizes])
		end if

	case default
		call unsupported(em, 'this kind of array literal')
		r%owned = .false.

	end select

end function emit_array_expr

!===============================================================================

!===============================================================================
! Structs

function c_string(em, s) result(g)

	! A constant string of the program with a NUL after it, for the descriptors
	! that the runtime describes a struct with, and for its messages

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: s
	type(c_ptr) :: g

	g = LLVMAddGlobal(em%llmod, LLVMArrayType2(em%t_i8, int(len(s) + 1, c_int64_t)), &
		cs('.cs.'//str(em%nstr)))
	em%nstr = em%nstr + 1
	call LLVMSetInitializer(g, LLVMConstStringInContext(em%ctx, s, int(len(s), c_int), 0))
	call LLVMSetGlobalConstant(g, 1)
	call LLVMSetLinkage(g, LLVM_PRIVATE_LINKAGE)

end function c_string

!===============================================================================

function struct_desc(em, k) result(d)

	! The runtime's description of the struct at position `k` of the table: its
	! name, and the name and kind of each member, c.f. `sy_sdesc` in the runtime.
	! It's a constant of the program that's made at the first use

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: k
	type(c_ptr) :: d

	!********

	character(len = :), allocatable :: mname

	integer :: i, n

	logical :: ok

	type(c_ptr) :: tys(4), vals(4), dty(3), dvals(3), mty, gm, aux
	type(c_ptr), allocatable :: ms(:)

	integer :: code

	type(value_t) :: mval

	d = c_null_ptr
	if (.not. associated(em%structs)) return
	if (k < 1) return

	if (.not. allocated(em%sdesc)) then
		allocate(em%sdesc(size(em%structs%table)))
		em%sdesc = c_null_ptr
	end if
	if (c_associated(em%sdesc(k))) then
		d = em%sdesc(k)
		return
	end if

	n = 0
	if (allocated(em%structs%table(k)%val%member_names%v)) &
		n = size(em%structs%table(k)%val%member_names%v)
	allocate(ms(max(n, 1)))
	ms = c_null_ptr

	tys = [em%t_ptr, em%t_i32, em%t_i32, em%t_ptr]
	mty = LLVMStructTypeInContext(em%ctx, tys, 4, 0)

	do i = 1, n
		call structs_member(em%structs, k, i, mval, mname, ok)
		if (.not. ok) then
			call unsupported(em, 'a struct of unknown type')
			return
		end if
		if (.not. supported_val(mval) .or. mval%type == file_type) then
			call unsupported(em, 'a struct member of type `'//kind_name(mval%type)//'`')
			return
		end if

		! An enum, or an array of them, has the names of its variants too
		code = elem_code(mval%type)
		aux = LLVMConstNull(em%t_ptr)
		if (mval%type == fn_type) then
			! A fn pointer prints as its type
			code = 9
			aux = c_string(em, value_type_name(mval))
		else if (mval%type == enum_type) then
			code = 8
			aux = enum_names(em, enums_slot_of(em%enums, mval))
		else if (mval%type == array_type .and. allocated(mval%array)) then
			if (mval%array%type == enum_type) &
				aux = enum_names(em, enums_slot_of(em%enums, mval))
		end if

		vals(1) = c_string(em, mname)
		vals(2) = ci32(em, code)
		vals(3) = ci32(em, 0)
		vals(4) = aux
		ms(i) = LLVMConstStructInContext(em%ctx, vals, 4, 0)
	end do

	gm = LLVMAddGlobal(em%llmod, LLVMArrayType2(mty, int(n, c_int64_t)), cs('.sm.'//str(k)))
	call LLVMSetInitializer(gm, LLVMConstArray(mty, ms, n))
	call LLVMSetGlobalConstant(gm, 1)
	call LLVMSetLinkage(gm, LLVM_PRIVATE_LINKAGE)

	dty = [em%t_ptr, em%t_i64, em%t_ptr]
	dvals = [c_string(em, em%structs%table(k)%key), ci64(em, int(n, 8)), gm]
	d = LLVMAddGlobal(em%llmod, LLVMStructTypeInContext(em%ctx, dty, 3, 0), &
		cs('.sd.'//str(k)))
	call LLVMSetInitializer(d, LLVMConstStructInContext(em%ctx, dvals, 3, 0))
	call LLVMSetGlobalConstant(d, 1)
	call LLVMSetLinkage(d, LLVM_PRIVATE_LINKAGE)

	em%sdesc(k) = d

end function struct_desc

!===============================================================================

recursive function emit_struct_instance(em, node, imove) result(r)

	! `Point{x = 1, y = 2}` is a new struct that owns its members.  They are in
	! the order of the members' indices, which is the order of their slots.  The
	! member `imove`, if there is one, is not copied but taken out of the variable
	! or member that it names, like the interpreter does for `x = S{m = x}`

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in), optional :: imove
	type(nval_t) :: r

	!********

	character(len = :), allocatable :: mname

	integer :: i, k

	logical :: ok

	type(c_ptr) :: d, s

	type(nval_t) :: v

	type(value_t) :: mval

	r = dummy_val(em, struct_type)

	k = 0
	if (associated(em%structs)) k = structs_slot_of(em%structs, node%val)
	if (k == 0) then
		call unsupported(em, 'a struct of unknown type')
		return
	end if

	d = struct_desc(em, k)
	if (.not. c_associated(d)) return

	s = rt_call(em, 'sy_struct_new', em%t_ptr, [d])

	if (allocated(node%members)) then
		do i = 1, size(node%members)
			call structs_member(em%structs, k, i, mval, mname, ok)
			if (.not. ok) then
				call unsupported(em, 'a struct of unknown type')
				return
			end if
			if (present(imove)) then
				if (i == imove) then
					v = take_place(em, node%members(i))
				else
					v = emit_expr(em, node%members(i))
				end if
			else
				v = emit_expr(em, node%members(i))
			end if
			call elem_store(em, LLVMBuildGEP2(em%bld, em%t_i64, s, [ci64(em, int(i, 8))], 1, &
				cs('')), mval%type, v)
		end do
	end if

	r%v = s
	r%owned = .true.

end function emit_struct_instance

!===============================================================================

recursive function take_place(em, node) result(v)

	! Move a struct out of a variable or a member, leaving `null` behind.  The
	! value is owned by the caller, and the place no longer owns it.  A struct
	! that is moved is never read again before it is written, because the callers
	! check that with the interpreter's own rules (cant_fail() etc.)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: v

	!********

	logical :: ok

	type(c_ptr) :: ignored

	type(place_t) :: pl

	ok = place_of(em, node, pl)
	if (.not. ok) then
		v = dummy_val(em, struct_type)
		return
	end if

	if (pl%ty /= struct_type .or. pl%has_subs) then
		! Not something that can be moved, so copy it
		call place_done(em, pl)
		v = emit_expr(em, node)
		return
	end if

	v = elem_load(em, pl%ty, pl%p)
	ignored = LLVMBuildStore(em%bld, LLVMConstNull(em%t_ptr), pl%p)
	call place_done(em, pl)
	v%owned = .true.

end function take_place

!===============================================================================

recursive subroutine emit_struct_methods(em, decl, done)

	! Build the methods of a struct, which are fns.  `done` has the ids of the
	! fns that are already built, as for emit_module_fns()

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl
	type(integer_vector_t), intent(inout) :: done

	integer :: j, k, m

	logical :: found

	if (.not. allocated(decl%members)) return
	if (.not. associated(em%structs)) return
	if (.not. allocated(em%structs%table)) return

	k = 0
	do j = 1, size(em%structs%table)
		if (.not. allocated(em%structs%table(j)%key)) cycle
		if (em%structs%table(j)%id_index == decl%id_index) k = j
	end do
	if (k == 0) return

	do j = 1, size(decl%members)
		if (decl%members(j)%kind /= fn_declaration) cycle

		found = .false.
		do m = 1, done%len_
			if (done%v(m) == decl%members(j)%id_index) found = .true.
		end do
		if (found) cycle

		call done%push(decl%members(j)%id_index)
		call emit_fn(em, decl%members(j), k)
	end do

end subroutine emit_struct_methods

!===============================================================================
! Enums

subroutine enum_tables(em, k)

	! The constant tables of the enum at position `k` of the table, which hold
	! the backing value of each of its variants and the name of each as a str.
	! The value of an enum is the index of its variant

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: k

	!********

	integer :: i, n

	type(c_ptr) :: g
	type(c_ptr), allocatable :: vs(:), ns(:)

	if (.not. allocated(em%evals)) then
		allocate(em%evals(size(em%enums%table)), em%enames(size(em%enums%table)))
		em%evals = c_null_ptr
		em%enames = c_null_ptr
	end if
	if (c_associated(em%evals(k))) return

	associate (e => em%enums%table(k)%val)

		n = e%num_vars
		allocate(vs(max(n, 1)), ns(max(n, 1)))
		vs = c_null_ptr
		ns = c_null_ptr
		do i = 1, n
			vs(i) = ci32(em, e%variant_values(i))
			ns(i) = str_literal(em, em%enums%table(k)%key//'.'//e%variant_names%v(i)%s)
		end do

		g = LLVMAddGlobal(em%llmod, LLVMArrayType2(em%t_i32, int(n, c_int64_t)), &
			cs('.ev.'//str(k)))
		call LLVMSetInitializer(g, LLVMConstArray(em%t_i32, vs, n))
		call LLVMSetGlobalConstant(g, 1)
		call LLVMSetLinkage(g, LLVM_PRIVATE_LINKAGE)
		em%evals(k) = g

		g = LLVMAddGlobal(em%llmod, LLVMArrayType2(em%t_ptr, int(n, c_int64_t)), &
			cs('.en.'//str(k)))
		call LLVMSetInitializer(g, LLVMConstArray(em%t_ptr, ns, n))
		call LLVMSetGlobalConstant(g, 1)
		call LLVMSetLinkage(g, LLVM_PRIVATE_LINKAGE)
		em%enames(k) = g

	end associate

end subroutine enum_tables

!===============================================================================

function enum_names(em, k) result(g)

	! The table of the names of the variants of an enum, as strs

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: k
	type(c_ptr) :: g

	g = LLVMConstNull(em%t_ptr)
	if (k < 1 .or. .not. associated(em%enums)) then
		call unsupported(em, 'an enum of unknown type')
		return
	end if

	call enum_tables(em, k)
	g = em%enames(k)

end function enum_names

!===============================================================================

function enum_backing(em, v) result(b)

	! The backing value of an enum variant, which is looked up by its index

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v
	type(c_ptr) :: b

	b = ci32(em, 0)
	if (v%sk < 1 .or. .not. associated(em%enums)) then
		call unsupported(em, 'an enum of unknown type')
		return
	end if

	call enum_tables(em, v%sk)
	b = LLVMBuildLoad2(em%bld, em%t_i32, LLVMBuildGEP2(em%bld, em%t_i32, em%evals(v%sk), &
		[v%v], 1, cs('')), cs(''))

end function enum_backing

!===============================================================================

function enum_compare(em, op, l, r, op_text) result(res)

	! Compare two enum values.  Equality is that of their indices, unless two
	! variants of the enum have the same backing value, and an ordering is always
	! that of the backing values

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: op
	type(nval_t), intent(in) :: l, r
	character(len = *), intent(in) :: op_text
	type(nval_t) :: res

	!********

	integer :: i, j, k

	logical :: by_value

	type(c_ptr) :: a, b

	type(nval_t) :: lk, rk

	res = dummy_val(em, bool_type)

	k = max(l%sk, r%sk)
	if (k < 1 .or. .not. associated(em%enums)) then
		call unsupported(em, 'an enum of unknown type')
		return
	end if

	by_value = .not. (op == double_equals_token .or. op == bang_equals_token)
	associate (e => em%enums%table(k)%val)
		do i = 1, e%num_vars
			do j = i + 1, e%num_vars
				if (e%variant_values(i) == e%variant_values(j)) by_value = .true.
			end do
		end do
	end associate

	a = l%v
	b = r%v
	if (by_value) then
		lk = l
		rk = r
		lk%sk = k
		rk%sk = k
		a = enum_backing(em, lk)
		b = enum_backing(em, rk)
	end if

	select case (op)
	case (double_equals_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_EQ, a, b, cs(''))
	case (bang_equals_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, a, b, cs(''))
	case (less_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_SLT, a, b, cs(''))
	case (less_equals_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_SLE, a, b, cs(''))
	case (greater_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_SGT, a, b, cs(''))
	case (greater_equals_token)
		res%v = LLVMBuildICmp(em%bld, LLVM_INT_SGE, a, b, cs(''))
	case default
		call unsupported(em, 'the `'//op_text//'` operator on enums')
	end select

end function enum_compare

!===============================================================================

function emit_enum_access(em, node) result(r)

	! `Suit.Clubs` is the index of the variant, which is not necessarily its
	! backing value

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	integer :: i, k

	r = dummy_val(em, enum_type)

	k = 0
	if (associated(em%enums)) k = enums_slot_of(em%enums, node%val)
	if (k == 0 .or. .not. allocated(node%val%enum_variant)) then
		call unsupported(em, 'an enum of unknown type')
		return
	end if

	associate (e => em%enums%table(k)%val)
		do i = 1, e%num_vars
			if (e%variant_names%v(i)%s == node%val%enum_variant) then
				r%ty = enum_type
				r%v = ci32(em, i - 1)
				r%sk = k
				return
			end if
		end do
	end associate

	call unsupported(em, 'an enum variant that is not found')

end function emit_enum_access

!===============================================================================

recursive function emit_enum_cast(em, node) result(r)

	! `Suit(2)` is the first variant with the backing value 2

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	integer :: k

	type(nval_t) :: v

	r = dummy_val(em, enum_type)

	k = 0
	if (associated(em%enums)) k = enums_slot_of(em%enums, node%val)
	if (k == 0) then
		call unsupported(em, 'an enum of unknown type')
		return
	end if

	v = emit_expr(em, node%right)
	if (.not. is_numeric_type(v%ty)) then
		call unsupported(em, 'an enum cast of a value of type `'//kind_name(v%ty)//'`')
		call dispose(em, v)
		return
	end if
	v = convert(em, v, i32_type)

	call enum_tables(em, k)
	r%v = rt_call(em, 'sy_enum_of', em%t_i32, [em%evals(k), &
		ci32(em, em%enums%table(k)%val%num_vars), v%v, c_string(em, em%enums%table(k)%key)])
	r%ty = enum_type
	r%sk = k

end function emit_enum_cast

!===============================================================================
! Places

subroutine check_not_null(em, sp)

	! A member of a struct that is `null` is a runtime error

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: sp

	type(c_ptr) :: bad, good, ignored

	bad  = new_block(em, 'null')
	good = new_block(em, 'nonnull')
	ignored = LLVMBuildCondBr(em%bld, LLVMBuildICmp(em%bld, LLVM_INT_EQ, sp, &
		LLVMConstNull(em%t_ptr), cs('')), bad, good)

	call LLVMPositionBuilderAtEnd(em%bld, bad)
	call rt_void(em, 'sy_error', [c_string(em, 'R47'), c_string(em, 'cannot '// &
		merge('assign', 'access', em%assigning)//' a member of null')])
	ignored = LLVMBuildBr(em%bld, good)

	call LLVMPositionBuilderAtEnd(em%bld, good)

end subroutine check_not_null

!===============================================================================

recursive function place_subscript(em, pl, owner) result(ok)

	! The element of the array that a place holds, if there is one scalar subscript
	! per dimension.  Otherwise the subscripts are left in the place, if they are a
	! slice of the array or a substring, since nothing can follow them.  Anything
	! else this has said that it isn't supported

	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(inout) :: pl
	type(syntax_node_t), intent(in) :: owner
	logical :: ok

	integer :: i, nsub

	type(c_ptr) :: a
	type(c_ptr), allocatable :: idx(:)

	ok = .false.
	nsub = size(owner%lsubscripts)

	if (pl%ty == str_type) then
		if (nsub /= 1) then
			call unsupported(em, 'this subscript of a string')
			return
		end if
		pl%subs = build_subs(em, owner, pl%pend)
		pl%has_subs = .true.
		pl%nsub = nsub
		ok = .true.
		return
	else if (pl%ty /= array_type) then
		call unsupported(em, 'a subscript of a value that is not an array')
		return
	end if

	if (nsub == pl%rank .and. all_scalar(owner)) then
		a = LLVMBuildLoad2(em%bld, em%t_ptr, pl%p, cs(''))
		allocate(idx(pl%rank))
		do i = 1, pl%rank
			idx(i) = scalar_index(em, owner%lsubscripts(i))
		end do

		pl%p = elem_ptr(em, a, pl%elem, lin_index(em, a, idx))
		pl%ty = pl%elem
		pl%elem = unknown_type
		pl%rank = 0
		ok = .true.

	else if (nsub == pl%rank .or. (nsub == pl%rank + 1 .and. pl%elem == str_type)) then
		pl%subs = build_subs(em, owner, pl%pend)
		pl%has_subs = .true.
		pl%nsub = nsub
		pl%out_rank = count(owner%lsubscripts(1: pl%rank)%sub_kind /= scalar_sub)
		ok = .true.

	else
		call unsupported(em, 'a subscript with the wrong number of dimensions')
	end if

end function place_subscript

!===============================================================================

recursive function place_member(em, m, pl) result(ok)

	! Move a place that holds a struct to its member `m`, and to what is after it
	! in the chain

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: m
	type(place_t), intent(inout) :: pl
	logical :: ok

	!********

	character(len = :), allocatable :: mname

	logical :: mok

	type(c_ptr) :: sp

	type(value_t) :: mval

	ok = .false.

	if (pl%has_subs) then
		call unsupported(em, 'a member of a slice')
		return
	end if

	if (pl%ty /= struct_type .or. pl%sk < 1 .or. .not. associated(em%structs)) then
		call unsupported(em, 'a member of a value whose struct is unknown')
		return
	end if

	sp = LLVMBuildLoad2(em%bld, em%t_ptr, pl%p, cs(''))
	call check_not_null(em, sp)

	call structs_member(em%structs, pl%sk, m%id_index, mval, mname, mok)
	if (.not. mok) then
		call unsupported(em, 'a member of a struct whose type is unknown')
		return
	end if

	! Slot 0 of the struct is its descriptor
	pl%p = LLVMBuildGEP2(em%bld, em%t_i64, sp, [ci64(em, int(m%id_index, 8))], 1, cs(''))
	pl%ty = mval%type
	pl%elem = unknown_type
	pl%rank = 0
	pl%sk = 0
	if (mval%type == array_type .and. allocated(mval%array)) then
		pl%elem = mval%array%type
		pl%rank = mval%array%rank
	end if
	if (mval%type == struct_type .or. pl%elem == struct_type) then
		pl%sk = structs_slot_of(em%structs, mval)
	end if

	if (allocated(m%lsubscripts)) then
		if (.not. place_subscript(em, pl, m)) return
	end if

	if (allocated(m%member)) then
		ok = place_member(em, m%member, pl)
		return
	end if

	ok = .true.

end function place_member

!===============================================================================

recursive function place_of(em, node, pl) result(ok)

	! The place of a reference to a member of a struct, or to an element of an
	! array of structs: its variable, then the subscripts and members that follow
	! it.  If it can't be done, this has said that it isn't supported

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(place_t), intent(out) :: pl
	logical :: ok

	integer :: id

	ok = .false.

	if (node%root_kind /= 0) then
		call unsupported(em, 'a member of the result of a fn here')
		return
	end if

	pl%p = ref_slot(em, node)
	if (.not. c_associated(pl%p)) then
		call unsupported(em, 'a member of a variable whose type is unknown')
		return
	end if

	id = node%id_index
	if (node%is_loc) then
		pl%ty   = em%l%ty(id)
		pl%elem = em%l%elem(id)
		pl%rank = em%l%rank(id)
		pl%sk   = em%l%sk(id)
	else
		pl%ty   = em%g%ty(id)
		pl%elem = em%g%elem(id)
		pl%rank = em%g%rank(id)
		pl%sk   = em%g%sk(id)
	end if

	if (allocated(node%lsubscripts)) then
		if (.not. place_subscript(em, pl, node)) return
	end if

	if (allocated(node%member)) then
		ok = place_member(em, node%member, pl)
		return
	end if

	ok = .true.

end function place_of

!===============================================================================

function place_load(em, pl) result(r)

	! Read what a place holds.  A str, array, or struct is only lent, unless the
	! place is a slice or substring, which is made new

	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(in) :: pl
	type(nval_t) :: r

	!********

	type(c_ptr) :: a, whole

	if (.not. pl%has_subs) then
		r = elem_load(em, pl%ty, pl%p)
		if (pl%ty == array_type) then
			r%elem = pl%elem
			r%rank = pl%rank
		end if
		return
	end if

	a = LLVMBuildLoad2(em%bld, em%t_ptr, pl%p, cs(''))
	r%owned = .true.

	if (pl%ty == str_type) then
		r%ty = str_type
		r%v = rt_call(em, 'sy_str_sub', em%t_ptr, [a, pl%subs])

	else if (pl%nsub == pl%rank) then
		r%ty = array_type
		r%v = rt_call(em, 'sy_arr_slice', em%t_ptr, [a, pl%subs])
		r%elem = pl%elem
		r%rank = pl%out_rank

	else if (pl%out_rank == 0) then
		! A substring of one element, which is the only one of an array of one
		whole = rt_call(em, 'sy_arr_str_sub', em%t_ptr, [a, pl%subs])
		r%ty = str_type
		r%v = rt_call(em, 'sy_str_dup', em%t_ptr, [LLVMBuildLoad2(em%bld, em%t_ptr, &
			arr_data(em, whole), cs(''))])
		call rt_void(em, 'sy_arr_free', [whole])

	else
		r%ty = array_type
		r%v = rt_call(em, 'sy_arr_str_sub', em%t_ptr, [a, pl%subs])
		r%elem = str_type
		r%rank = pl%out_rank
	end if

end function place_load

!===============================================================================

subroutine arr_set_all(em, a, ety, v)

	! a = v, for a single element v.  This consumes `v`

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	integer, intent(in) :: ety
	type(nval_t), intent(inout) :: v

	type(c_ptr) :: valp, ignored

	valp = entry_alloca(em, elem_ll(em, ety), 'fill')
	if (ptr_type(ety)) ignored = LLVMBuildStore(em%bld, LLVMConstNull(em%t_ptr), valp)
	call elem_store(em, valp, ety, v)
	call rt_void(em, 'sy_arr_set_all', [a, valp])
	if (ptr_type(ety)) call rt_void(em, free_fn(ety), &
		[LLVMBuildLoad2(em%bld, em%t_ptr, valp, cs(''))])
	v%owned = .false.
	call dispose(em, v)
	v%owned = .false.

end subroutine arr_set_all

!===============================================================================

subroutine arr_slice_store(em, a, subs, ety, v)

	! a[subs] = v, where v is an array of the same size or a single element.  This
	! consumes `v`

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a, subs
	integer, intent(in) :: ety
	type(nval_t), intent(inout) :: v

	type(c_ptr) :: valp, ignored

	if (v%ty == array_type) then
		! The runtime copies raw elements, so the rhs needs the type of the elements
		! of the target, e.g. `a[:, i] = [x, y]` of an i32 array with i64 x and y
		v = arr_cast(em, v, ety)
		call rt_void(em, 'sy_arr_slice_set', [a, subs, v%v, LLVMConstNull(em%t_ptr)])
	else
		valp = entry_alloca(em, elem_ll(em, ety), 'fill')
		if (ptr_type(ety)) ignored = LLVMBuildStore(em%bld, LLVMConstNull(em%t_ptr), valp)
		call elem_store(em, valp, ety, v)
		call rt_void(em, 'sy_arr_slice_set', [a, subs, LLVMConstNull(em%t_ptr), valp])
		if (ptr_type(ety)) call rt_void(em, free_fn(ety), &
			[LLVMBuildLoad2(em%bld, em%t_ptr, valp, cs(''))])
		v%owned = .false.
	end if
	call dispose(em, v)
	v%owned = .false.

end subroutine arr_slice_store

!===============================================================================

subroutine place_store(em, pl, v)

	! Write to a place, which is an element or a member, or a slice or substring.
	! This consumes `v`

	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(in) :: pl
	type(nval_t), intent(inout) :: v

	type(c_ptr) :: a

	if (.not. pl%has_subs) then
		! An array member keeps the type of its elements, e.g. an i32 array that is
		! assigned the i64 array `[x, y]`
		if (pl%ty == array_type .and. v%ty == array_type .and. pl%elem /= unknown_type) &
			v = arr_cast(em, v, pl%elem)
		call elem_store(em, pl%p, pl%ty, v)
		return
	end if

	a = LLVMBuildLoad2(em%bld, em%t_ptr, pl%p, cs(''))

	if (pl%ty == array_type .and. pl%nsub == pl%rank) then
		call arr_slice_store(em, a, pl%subs, pl%elem, v)
		return
	end if

	if (v%ty == array_type .and. v%elem == str_type .and. pl%ty == array_type) then
		call rt_void(em, 'sy_arr_str_set_arr', [a, pl%subs, v%v])
	else if (v%ty /= str_type) then
		call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a substring')
	else if (pl%ty == str_type) then
		call rt_void(em, 'sy_str_set', [pl%p, pl%subs, v%v])
	else
		call rt_void(em, 'sy_arr_str_set', [a, pl%subs, v%v])
	end if
	call dispose(em, v)
	v%owned = .false.

end subroutine place_store

!===============================================================================

subroutine place_done(em, pl)
	! Free what the subscripts of a place made, after it was read or written
	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(in) :: pl
	if (allocated(pl%pend)) call dispose_all(em, pl%pend)
end subroutine place_done

!===============================================================================

function is_file_var(em, node) result(yes)

	! Is the variable at the root of this reference a file handle?  The standard
	! ones are the slots of `std::IN`, `std::OUT`, and `std::ERR`

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical :: yes

	type(c_ptr) :: slot

	yes = .false.
	if (.not. node%is_loc .and. node%id_index >= 2 .and. node%id_index <= 4) then
		yes = node%member%id_index >= 1 .and. node%member%id_index <= 3 .and. &
			node%member%val%type /= unknown_type
		return
	end if

	slot = ref_slot(em, node)
	if (.not. c_associated(slot)) return
	if (node%is_loc) then
		yes = em%l%ty(node%id_index) == file_type
	else
		yes = em%g%ty(node%id_index) == file_type
	end if

end function is_file_var

!===============================================================================

function emit_file_member(em, node) result(r)

	! `f.is_open`, `f.eof`, or `f.name`

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	type(nval_t) :: f

	if (.not. node%is_loc .and. node%id_index >= 2 .and. node%id_index <= 4) then
		f%ty = file_type
		f%owned = .true.
		f%v = rt_call(em, 'sy_file_std', em%t_ptr, [ci32(em, node%id_index - 2)])
	else
		f%ty = file_type
		f%owned = .false.
		f%v = LLVMBuildLoad2(em%bld, em%t_ptr, ref_slot(em, node), cs(''))
	end if

	r%owned = .false.
	select case (node%member%id_index)
	case (FILE_MEM_IS_OPEN)
		r%ty = bool_type
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_file_is_open', em%t_i8, &
			[f%v]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
	case (FILE_MEM_EOF)
		r%ty = bool_type
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_file_eof_member', em%t_i8, &
			[f%v]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
	case default
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_file_name', em%t_ptr, [f%v])
	end select

	call dispose(em, f)

end function emit_file_member

!===============================================================================

recursive function find_fn_ret(node, ret) result(found)

	! What the fn that a reference points to returns.  If the reference is a chain of
	! members, like `a.b.f`, then it's the type of its last member that says, and the
	! type of the first, `a`, doesn't

	type(syntax_node_t), intent(in) :: node
	type(value_t), intent(inout) :: ret
	logical :: found

	found = .false.
	if (allocated(node%member)) then
		found = find_fn_ret(node%member, ret)
		if (found) return
	end if
	if (allocated(node%val%fn_ret)) then
		ret = node%val%fn_ret
		found = .true.
	end if

end function find_fn_ret

!===============================================================================

recursive function chain_scalar_subs(node) result(n)

	! How many subscripts a chain of members has, or -1 if one of them isn't a
	! single element

	type(syntax_node_t), intent(in) :: node
	integer :: n

	integer :: m

	n = 0
	if (allocated(node%lsubscripts)) then
		if (any(node%lsubscripts%sub_kind /= scalar_sub)) then
			n = -1
			return
		end if
		n = size(node%lsubscripts)
	end if
	if (allocated(node%member)) then
		m = chain_scalar_subs(node%member)
		if (m < 0) then
			n = -1
		else
			n = n + m
		end if
	end if

end function chain_scalar_subs

!===============================================================================

recursive function emit_member_read(em, node) result(r)

	! `p.x`, `p.a[0].b`, `ps[1].x`, or `f().x`, the member of a temporary struct

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	type(c_ptr) :: slot, ignored

	type(nval_t) :: root_v

	type(place_t) :: pl

	type(syntax_node_t) :: root

	r = dummy_val(em, node%val%type)

	! A member of a file handle is not a member of a struct
	if (node%root_kind == 0 .and. node%id_index >= 1 .and. allocated(node%member)) then
		if (.not. allocated(node%lsubscripts) .and. .not. allocated(node%member%member)) then
			if (is_file_var(em, node)) then
				r = emit_file_member(em, node)
				if (allocated(node%member%lsubscripts)) then
					! A character or substring of `f.name`
					root_v = r
					slot = entry_alloca(em, em%t_ptr, 'tmp')
					ignored = LLVMBuildStore(em%bld, root_v%v, slot)
					pl%p = slot
					pl%ty = str_type
					if (place_subscript(em, pl, node%member)) then
						r = place_load(em, pl)
						call place_done(em, pl)
					else
						r = dummy_val(em, node%val%type)
					end if
					call dispose(em, root_v)
				end if
				return
			end if
		end if
	end if

	if (node%root_kind == 0) then
		if (.not. place_of(em, node, pl)) return
		r = place_load(em, pl)
		call place_done(em, pl)
		return
	end if

	if (node%root_kind /= fn_call_expr .and. node%root_kind /= method_call_expr .and. &
			node%root_kind /= fn_call_ptr_expr) then
		call unsupported(em, 'a member of the result of a `'//kind_name(node%root_kind)//'`')
		return
	end if

	! The call makes a struct that is only the member's until it is copied out.
	! If it has subscripts, like `f()[i].x`, then they select the struct from the
	! array that the call makes, and they stay with the call
	root = node
	root%kind = node%root_kind
	root%root_kind = 0
	deallocate(root%member)

	if (node%root_kind /= fn_call_ptr_expr) then
		! The type of the call is that of the struct, and not of the member
		root%val%type = struct_type
		if (allocated(em%fns%fns(node%id_index)%type%struct_cookie)) &
			root%val%struct_cookie = em%fns%fns(node%id_index)%type%struct_cookie
		if (allocated(em%fns%fns(node%id_index)%type%struct_name)) &
			root%val%struct_name = em%fns%fns(node%id_index)%type%struct_name
	end if

	if (node%root_kind == fn_call_ptr_expr) then
		! What the fn returns is part of the type of the pointer.  The struct is
		! the type of the call, and not that of the member that is read from it
		if (allocated(node%left)) then
			if (.not. find_fn_ret(node%left, root%val)) then
				call unsupported(em, 'a member of the result of a call of unknown type')
				return
			end if
		else
			pl%sk = merge(em%l%sk(node%id_index), em%g%sk(node%id_index), node%is_loc)
			if (pl%sk < 1 .or. .not. associated(em%structs)) then
				call unsupported(em, 'a member of the result of a call of unknown type')
				return
			end if
			root%val%type = struct_type
			root%val%struct_cookie = em%structs%table(pl%sk)%val%cookie
			root%val%struct_name = em%structs%table(pl%sk)%key
		end if
	end if

	root_v = emit_expr(em, root)
	if (root_v%ty /= struct_type .or. .not. associated(em%structs)) then
		call unsupported(em, 'a member of a fn result of type `'//kind_name(root_v%ty)//'`')
		call dispose(em, root_v)
		return
	end if

	if (node%root_kind == fn_call_ptr_expr) then
		pl%sk = structs_slot_of(em%structs, root%val)
	else
		pl%sk = structs_slot_of(em%structs, em%fns%fns(node%id_index)%type)
	end if
	pl%ty = struct_type
	slot = entry_alloca(em, em%t_ptr, 'tmp')
	ignored = LLVMBuildStore(em%bld, root_v%v, slot)
	pl%p = slot

	if (.not. place_member(em, node%member, pl)) then
		call dispose(em, root_v)
		return
	end if

	r = place_load(em, pl)
	if (ptr_type(r%ty) .and. .not. r%owned) then
		r%v = rt_call(em, dup_fn(r%ty), em%t_ptr, [r%v])
		r%owned = .true.
	end if
	call place_done(em, pl)
	call dispose(em, root_v)

end function emit_member_read

!===============================================================================

recursive function emit_binary(em, node) result(r)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	type(nval_t) :: l, rr

	if (is_arr(node%left%val) .or. is_arr(node%right%val)) then
		r = emit_array_binary(em, node)
		return
	end if

	l  = emit_expr(em, node%left)
	if (may_clobber(node%right)) call own_if_borrowed(em, l)
	rr = emit_expr(em, node%right)
	r  = binop_vals(em, node%op%kind, l, rr, node%op%text)

end function emit_binary

!===============================================================================

function binop_vals(em, op, l, r, op_text) result(res)

	! Apply the binary operator `op` to two values.  This consumes both of them

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: op
	type(nval_t), intent(in) :: l, r
	character(len = *), intent(in) :: op_text
	type(nval_t) :: res

	!********

	integer :: ct, bits

	type(c_ptr) :: masked

	type(nval_t) :: a, b

	res%owned = .false.

	! Strings are joined.  Comparing them is not supported yet
	if (l%ty == str_type .and. r%ty == str_type) then
		select case (op)
		case (plus_token)
			res%ty = str_type
			res%owned = .true.
			res%v = rt_call(em, 'sy_str_cat', em%t_ptr, [l%v, r%v])
		case (double_equals_token, bang_equals_token)
			res%ty = bool_type
			res%v = str_test(em, 'sy_str_eq', l%v, r%v)
			if (op == bang_equals_token) res%v = LLVMBuildNot(em%bld, res%v, cs(''))
		case (less_token)
			res%ty = bool_type
			res%v = str_test(em, 'sy_str_lt', l%v, r%v)
		case (greater_token)
			res%ty = bool_type
			res%v = str_test(em, 'sy_str_lt', r%v, l%v)
		case (less_equals_token)
			res%ty = bool_type
			res%v = LLVMBuildNot(em%bld, str_test(em, 'sy_str_lt', r%v, l%v), cs(''))
		case (greater_equals_token)
			res%ty = bool_type
			res%v = LLVMBuildNot(em%bld, str_test(em, 'sy_str_lt', l%v, r%v), cs(''))
		case default
			call unsupported(em, 'the `'//op_text//'` operator on strings')
			res = dummy_val(em, bool_type)
			return
		end select
		call dispose(em, l)
		call dispose(em, r)
		return
	end if

	! Structs can only be compared to `null`, for whether they are
	if ((l%ty == struct_type .or. l%ty == null_type) .and. &
			(r%ty == struct_type .or. r%ty == null_type)) then
		res%ty = bool_type
		select case (op)
		case (double_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_EQ, l%v, r%v, cs(''))
		case (bang_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, l%v, r%v, cs(''))
		case default
			call unsupported(em, 'the `'//op_text//'` operator on structs')
			res = dummy_val(em, bool_type)
			return
		end select
		call dispose(em, l)
		call dispose(em, r)
		return
	end if

	! Enums are compared by their backing values, which are the variants' indices
	! unless two variants have the same one
	if (l%ty == enum_type .and. r%ty == enum_type) then
		res = enum_compare(em, op, l, r, op_text)
		return
	end if

	! Fn pointers are the same if they point to the same fn
	if (l%ty == fn_type .and. r%ty == fn_type) then
		res%ty = bool_type
		select case (op)
		case (double_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_EQ, l%v, r%v, cs(''))
		case (bang_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, l%v, r%v, cs(''))
		case default
			call unsupported(em, 'the `'//op_text//'` operator on fn pointers')
			res = dummy_val(em, bool_type)
		end select
		return
	end if

	! Booleans
	if (l%ty == bool_type .and. r%ty == bool_type) then
		res%ty = bool_type
		select case (op)
		case (and_keyword)
			res%v = LLVMBuildAnd(em%bld, l%v, r%v, cs(''))
		case (or_keyword)
			res%v = LLVMBuildOr(em%bld, l%v, r%v, cs(''))
		case (double_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_EQ, l%v, r%v, cs(''))
		case (bang_equals_token)
			res%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, l%v, r%v, cs(''))
		case default
			call unsupported(em, 'the `'//op_text//'` operator on booleans')
			res = dummy_val(em, bool_type)
		end select
		return
	end if

	if (.not. (is_numeric_type(l%ty) .and. is_numeric_type(r%ty))) then
		call unsupported(em, 'the `'//op_text//'` operator on `'//kind_name(l%ty)// &
			'` and `'//kind_name(r%ty)//'`')
		res = dummy_val(em, l%ty)
		return
	end if

	! Shifts are in the width of the left operand, and are logical.  A shift by
	! as many bits as it has or more is 0, which LLVM leaves undefined.  A negative
	! count is masked like Fortran's shifts do
	if (op == double_less_token .or. op == double_greater_token) then
		if (l%ty /= i32_type .and. l%ty /= i64_type) then
			call unsupported(em, 'a shift of type `'//kind_name(l%ty)//'`')
			res = dummy_val(em, l%ty)
			return
		end if
		bits = merge(32, 64, l%ty == i32_type)
		a = l
		b = convert(em, r, l%ty)
		res%ty = l%ty
		! The count is masked to the width, so a negative one wraps like in Fortran
		masked = LLVMBuildAnd(em%bld, b%v, &
			LLVMConstInt(ll_type(em, l%ty), int(bits - 1, c_int64_t), 0), cs(''))
		if (op == double_less_token) then
			res%v = LLVMBuildShl(em%bld, a%v, masked, cs(''))
		else
			res%v = LLVMBuildLShr(em%bld, a%v, masked, cs(''))
		end if
		res%v = LLVMBuildSelect(em%bld, &
			LLVMBuildICmp(em%bld, LLVM_INT_SLT, b%v, &
				LLVMConstInt(ll_type(em, l%ty), int(bits, c_int64_t), 0), cs('')), &
			res%v, LLVMConstNull(ll_type(em, l%ty)), cs(''))
		return
	end if

	! A power of a float and an i32 is done by repeated multiplication, like
	! Fortran does.  Everything else converts to the wider type first
	if (op == double_star_token .and. (l%ty == f32_type .or. l%ty == f64_type) &
			.and. r%ty == i32_type) then
		res%ty = l%ty
		if (l%ty == f64_type) then
			res%v = rt_call(em, 'llvm.powi.f64.i32', em%t_f64, [l%v, r%v])
		else
			res%v = rt_call(em, 'llvm.powi.f32.i32', em%t_f32, [l%v, r%v])
		end if
		return
	end if

	ct = wider_type(l%ty, r%ty)
	a = convert(em, l, ct)
	b = convert(em, r, ct)
	res = num_binop(em, op, ct, a%v, b%v, op_text)

end function binop_vals

!===============================================================================

function str_test(em, name, a, b) result(t)

	! A comparison of two strings by the runtime, which is an i8, as an i1

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr), intent(in) :: a, b
	type(c_ptr) :: t

	t = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, name, em%t_i8, [a, b]), &
		LLVMConstNull(em%t_i8), cs(''))

end function str_test

!===============================================================================

function num_binop(em, op, ct, a, b, op_text) result(res)

	! A binary operator on two numbers which are both of the type `ct`

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: op, ct
	type(c_ptr), intent(in) :: a, b
	character(len = *), intent(in) :: op_text
	type(nval_t) :: res

	!********

	logical :: is_int

	type(c_ptr) :: zero, one, minus_one, is_zero, is_neg1, safe, q, bad

	is_int = ct == i32_type .or. ct == i64_type

	res%ty = ct
	res%owned = .false.

	select case (op)

	case (plus_token)
		if (is_int) then
			res%v = LLVMBuildAdd(em%bld, a, b, cs(''))
		else
			res%v = LLVMBuildFAdd(em%bld, a, b, cs(''))
		end if

	case (minus_token)
		if (is_int) then
			res%v = LLVMBuildSub(em%bld, a, b, cs(''))
		else
			res%v = LLVMBuildFSub(em%bld, a, b, cs(''))
		end if

	case (star_token)
		if (is_int) then
			res%v = LLVMBuildMul(em%bld, a, b, cs(''))
		else
			res%v = LLVMBuildFMul(em%bld, a, b, cs(''))
		end if

	case (slash_token)
		if (.not. is_int) then
			res%v = LLVMBuildFDiv(em%bld, a, b, cs(''))
		else
			! x / 0 is 0, as the interpreter has it, and LLVM has no result for
			! it.  Nor for the smallest integer divided by -1, which wraps
			zero = LLVMConstNull(ll_type(em, ct))
			one = LLVMConstInt(ll_type(em, ct), 1_c_int64_t, 1)
			minus_one = LLVMConstInt(ll_type(em, ct), -1_c_int64_t, 1)
			is_zero = LLVMBuildICmp(em%bld, LLVM_INT_EQ, b, zero, cs(''))
			is_neg1 = LLVMBuildICmp(em%bld, LLVM_INT_EQ, b, minus_one, cs(''))
			bad = LLVMBuildOr(em%bld, is_zero, is_neg1, cs(''))
			safe = LLVMBuildSelect(em%bld, bad, one, b, cs(''))
			q = LLVMBuildSDiv(em%bld, a, safe, cs(''))
			q = LLVMBuildSelect(em%bld, is_neg1, LLVMBuildSub(em%bld, zero, a, cs('')), &
				q, cs(''))
			res%v = LLVMBuildSelect(em%bld, is_zero, zero, q, cs(''))
		end if

	case (percent_token)
		if (.not. is_int) then
			res%v = LLVMBuildFRem(em%bld, a, b, cs(''))
		else
			! x % 0 is x, and x % -1 is 0
			zero = LLVMConstNull(ll_type(em, ct))
			one = LLVMConstInt(ll_type(em, ct), 1_c_int64_t, 1)
			minus_one = LLVMConstInt(ll_type(em, ct), -1_c_int64_t, 1)
			is_zero = LLVMBuildICmp(em%bld, LLVM_INT_EQ, b, zero, cs(''))
			is_neg1 = LLVMBuildICmp(em%bld, LLVM_INT_EQ, b, minus_one, cs(''))
			bad = LLVMBuildOr(em%bld, is_zero, is_neg1, cs(''))
			safe = LLVMBuildSelect(em%bld, bad, one, b, cs(''))
			q = LLVMBuildSRem(em%bld, a, safe, cs(''))
			res%v = LLVMBuildSelect(em%bld, is_zero, a, q, cs(''))
		end if

	case (double_star_token)
		select case (ct)
		case (i32_type)
			res%v = rt_call(em, 'sy_ipow_i32', em%t_i32, [a, b])
		case (i64_type)
			res%v = rt_call(em, 'sy_ipow_i64', em%t_i64, [a, b])
		case (f32_type)
			res%v = rt_call(em, 'powf', em%t_f32, [a, b])
		case (f64_type)
			res%v = rt_call(em, 'pow', em%t_f64, [a, b])
		end select

	case (amp_token, pipe_token, caret_token)
		if (.not. is_int) then
			call unsupported(em, 'the `'//op_text//'` operator on floats')
			res = dummy_val(em, ct)
		else if (op == amp_token) then
			res%v = LLVMBuildAnd(em%bld, a, b, cs(''))
		else if (op == pipe_token) then
			res%v = LLVMBuildOr(em%bld, a, b, cs(''))
		else
			res%v = LLVMBuildXor(em%bld, a, b, cs(''))
		end if

	case (double_equals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)
		res%ty = bool_type
		if (is_int) then
			select case (op)
			case (double_equals_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_EQ, a, b, cs(''))
			case (bang_equals_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, a, b, cs(''))
			case (less_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_SLT, a, b, cs(''))
			case (less_equals_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_SLE, a, b, cs(''))
			case (greater_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_SGT, a, b, cs(''))
			case (greater_equals_token)
				res%v = LLVMBuildICmp(em%bld, LLVM_INT_SGE, a, b, cs(''))
			end select
		else
			! Like Fortran, everything is false for a NaN, except /=
			select case (op)
			case (double_equals_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_OEQ, a, b, cs(''))
			case (bang_equals_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_UNE, a, b, cs(''))
			case (less_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_OLT, a, b, cs(''))
			case (less_equals_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_OLE, a, b, cs(''))
			case (greater_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_OGT, a, b, cs(''))
			case (greater_equals_token)
				res%v = LLVMBuildFCmp(em%bld, LLVM_REAL_OGE, a, b, cs(''))
			end select
		end if

	case default
		call unsupported(em, 'the `'//op_text//'` operator')
		res = dummy_val(em, ct)

	end select

end function num_binop

!===============================================================================

recursive function emit_unary(em, node) result(r)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: out, p, q, xd, od

	type(loop_t) :: lp

	type(nval_t) :: x, el, y

	if (is_arr(node%right%val)) then
		if (fuse_wanted(node)) then
			r = fuse_array(em, node)
			return
		end if

		! Each element
		x = emit_expr(em, node%right)
		r = dummy_val(em, array_type)
		if (x%ty /= array_type .or. .not. allocated(node%val%array)) then
			call unsupported(em, 'an operation on an array')
			return
		end if
		ety = node%val%array%type
		if (.not. supported_type(ety)) then
			call unsupported(em, 'an operation on an array of type `'//kind_name(ety)//'`')
			return
		end if
		out = rt_call(em, 'sy_arr_like', em%t_ptr, [x%v, ci32(em, elem_code(ety))])
		xd = arr_data(em, x%v)
		od = arr_data(em, out)
		call loop_begin(em, arr_len(em, x%v), lp, q)
		el = elem_load(em, x%elem, elem_at(em, xd, x%elem, q))
		y = unary_val(em, node%op%kind, node%op%text, el)
		call elem_store(em, elem_at(em, od, ety, q), ety, y)
		call loop_end(em, lp)
		call dispose(em, x)
		r = arr_val(out, .true., node%val)
		return
	end if

	x = emit_expr(em, node%right)
	r = unary_val(em, node%op%kind, node%op%text, x)

end function emit_unary

!===============================================================================

function unary_val(em, op, op_text, x) result(r)

	! Apply a unary operator to a value of a number or a bool

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: op
	character(len = *), intent(in) :: op_text
	type(nval_t), intent(in) :: x
	type(nval_t) :: r

	r%ty = x%ty
	r%owned = .false.

	select case (op)

	case (minus_token)
		if (x%ty == i32_type .or. x%ty == i64_type) then
			r%v = LLVMBuildSub(em%bld, LLVMConstNull(ll_type(em, x%ty)), x%v, cs(''))
		else if (x%ty == f32_type .or. x%ty == f64_type) then
			r%v = LLVMBuildFNeg(em%bld, x%v, cs(''))
		else
			call unsupported(em, 'a negative of type `'//kind_name(x%ty)//'`')
			r = dummy_val(em, x%ty)
		end if

	case (plus_token)
		r = x

	case (not_keyword)
		if (x%ty == bool_type) then
			r%v = LLVMBuildNot(em%bld, x%v, cs(''))
		else
			call unsupported(em, 'a `not` of type `'//kind_name(x%ty)//'`')
			r = dummy_val(em, x%ty)
		end if

	case (bang_token)
		if (x%ty == i32_type .or. x%ty == i64_type) then
			r%v = LLVMBuildNot(em%bld, x%v, cs(''))
		else
			call unsupported(em, 'a bitwise not of type `'//kind_name(x%ty)//'`')
			r = dummy_val(em, x%ty)
		end if

	case default
		call unsupported(em, 'the `'//op_text//'` operator')
		r = dummy_val(em, x%ty)

	end select

end function unary_val

!===============================================================================

subroutine loop_begin(em, n, lp, cur)

	! Start a loop that counts from 0 up to the i64 `n`.  `cur` is the counter

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: n
	type(loop_t), intent(out) :: lp
	type(c_ptr), intent(out) :: cur

	type(c_ptr) :: ignored

	lp%it = entry_alloca(em, em%t_i64, 'it')
	ignored = LLVMBuildStore(em%bld, ci64(em, 0_8), lp%it)

	lp%cond_bb = new_block(em, 'loop')
	lp%body_bb = new_block(em, 'lbody')
	lp%end_bb  = new_block(em, 'lend')

	ignored = LLVMBuildBr(em%bld, lp%cond_bb)
	call LLVMPositionBuilderAtEnd(em%bld, lp%cond_bb)
	cur = LLVMBuildLoad2(em%bld, em%t_i64, lp%it, cs(''))
	ignored = LLVMBuildCondBr(em%bld, LLVMBuildICmp(em%bld, LLVM_INT_SLT, cur, n, cs('')), &
		lp%body_bb, lp%end_bb)
	call LLVMPositionBuilderAtEnd(em%bld, lp%body_bb)

end subroutine loop_begin

!===============================================================================

subroutine loop_end(em, lp)

	! Finish a loop, and carry on after it

	type(nemitter_t), intent(inout) :: em
	type(loop_t), intent(in) :: lp

	type(c_ptr) :: cur, ignored

	cur = LLVMBuildLoad2(em%bld, em%t_i64, lp%it, cs(''))
	ignored = LLVMBuildStore(em%bld, LLVMBuildAdd(em%bld, cur, ci64(em, 1_8), cs('')), lp%it)
	ignored = LLVMBuildBr(em%bld, lp%cond_bb)
	call LLVMPositionBuilderAtEnd(em%bld, lp%end_bb)

end subroutine loop_end

!===============================================================================

recursive function emit_array_binary(em, node) result(r)

	! A binary operator with an array on at least one side

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	type(nval_t) :: l, rr

	r = dummy_val(em, array_type)

	if (node%op%kind == matmul_token) then
		l  = emit_expr(em, node%left)
		if (may_clobber(node%right)) call own_if_borrowed(em, l)
		rr = emit_expr(em, node%right)
		r  = matmul_vals(em, l, rr, node%val)
		return
	end if
	if (.not. allocated(node%val%array)) then
		call unsupported(em, 'an operation on an array')
		return
	end if

	if (fuse_wanted(node)) then
		r = fuse_array(em, node)
		return
	end if

	l  = emit_expr(em, node%left)
	if (may_clobber(node%right)) call own_if_borrowed(em, l)
	rr = emit_expr(em, node%right)
	r  = arr_binop_vals(em, node%op%kind, node%op%text, l, rr, node%val)

end function emit_array_binary

!===============================================================================

function fuse_kind(node) result(k)

	! How a node of a fused expression is made, as an `fk_*`.  An operator that
	! is made for each element has to be one that the elements of the result can
	! be of a number type, and its operands arrays or numbers.  A slice is only
	! read in place if it's of an array of rank 1 and its range is consecutive.
	! Anything else of an array is made first, as `fk_mat`

	type(syntax_node_t), intent(in) :: node
	integer :: k

	k = fk_scalar
	if (.not. is_arr(node%val)) return

	k = fk_mat
	if (.not. allocated(node%val%array)) return

	select case (node%kind)

	case (binary_expr)
		if (.not. is_numeric_type(node%val%array%type)) return
		select case (node%op%kind)
		case (plus_token, minus_token, star_token, slash_token)
			if (fuse_operand_ok(node%left%val) .and. fuse_operand_ok(node%right%val)) k = fk_bin
		end select

	case (unary_expr)
		if (.not. is_numeric_type(node%val%array%type)) return
		if (node%op%kind == minus_token .and. is_arr(node%right%val)) k = fk_neg

	case (name_expr)
		if (allocated(node%member)) return
		if (.not. allocated(node%lsubscripts)) then
			k = fk_arr
		else if (size(node%lsubscripts) == 1 .and. node%val%array%rank == 1 .and. &
				is_numeric_type(node%val%array%type)) then
			if (node%lsubscripts(1)%sub_kind == range_sub .or. &
					node%lsubscripts(1)%sub_kind == all_sub) k = fk_view
		end if

	end select

end function fuse_kind

!===============================================================================

function fuse_operand_ok(val) result(ok)

	! Whether a value can be the operand of a fused operator: an array, or a number

	type(value_t), intent(in) :: val
	logical :: ok

	ok = is_arr(val) .or. is_numeric_type(val%type)

end function fuse_operand_ok

!===============================================================================

recursive subroutine fuse_scan(node, nops, nleaf, nview, maxrank)

	! Count the operators, operands, and slices of an expression, and find the
	! greatest rank of its arrays, which is 99 if it's not known

	type(syntax_node_t), intent(in) :: node
	integer, intent(inout) :: nops, nleaf, nview, maxrank

	select case (fuse_kind(node))

	case (fk_bin)
		nops = nops + 1
		call fuse_scan(node%left, nops, nleaf, nview, maxrank)
		call fuse_scan(node%right, nops, nleaf, nview, maxrank)

	case (fk_neg)
		nops = nops + 1
		call fuse_scan(node%right, nops, nleaf, nview, maxrank)

	case (fk_view)
		nleaf = nleaf + 1
		nview = nview + 1
		maxrank = max(maxrank, 1)

	case (fk_arr, fk_mat)
		nleaf = nleaf + 1
		if (allocated(node%val%array)) then
			maxrank = max(maxrank, node%val%array%rank)
		else
			maxrank = 99
		end if

	case default
		nleaf = nleaf + 1

	end select

end subroutine fuse_scan

!===============================================================================

function fuse_wanted(node) result(w)

	! Whether an operator on arrays is made in one loop, with the operators in
	! its operands and without copying the slices of them.  One operator on
	! whole arrays is the same as arr_binop_vals(), so that is left to it.  A
	! slice has no shape of its own, so the arrays have to be of rank 1 if there
	! is one

	type(syntax_node_t), intent(in) :: node
	logical :: w

	integer :: nops, nleaf, nview, maxrank

	w = .false.
	if (fuse_kind(node) /= fk_bin .and. fuse_kind(node) /= fk_neg) return

	! The operands are read in place, so a call among them could change them
	if (may_clobber(node)) return

	nops = 0
	nleaf = 0
	nview = 0
	maxrank = 0
	call fuse_scan(node, nops, nleaf, nview, maxrank)

	w = (nops >= 2 .or. nview >= 1) .and. (nview == 0 .or. maxrank <= 1)

end function fuse_wanted

!===============================================================================

recursive function fuse_collect(em, node, leaves, nl) result(rep)

	! Evaluate the operands of a fused expression, in the order that they are
	! written, and check that their shapes agree.  `rep` is the first array of
	! the node, which the others have the shape of, or 0 if there is none

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(fleaf_t), intent(inout) :: leaves(:)
	integer, intent(inout) :: nl
	integer :: rep

	!********

	integer :: lrep, rrep, vty, ety, rank_

	type(c_ptr) :: ps, a, subs, offp, cntp

	type(nval_t) :: v
	type(nval_t), allocatable :: pending(:)

	rep = 0

	select case (fuse_kind(node))

	case (fk_bin)
		lrep = fuse_collect(em, node%left, leaves, nl)
		rrep = fuse_collect(em, node%right, leaves, nl)
		if (lrep > 0 .and. rrep > 0) &
			call fuse_shape_check(em, leaves(lrep), leaves(rrep), node%op%text)
		rep = merge(lrep, rrep, lrep > 0)

	case (fk_neg)
		rep = fuse_collect(em, node%right, leaves, nl)

	case (fk_view)
		nl = nl + 1
		leaves(nl)%ety = node%val%array%type
		if (.not. ref_info(em, node, ps, vty, ety, rank_)) then
			leaves(nl)%v = dummy_val(em, leaves(nl)%ety)
			return
		end if

		a = LLVMBuildLoad2(em%bld, em%t_ptr, ps, cs(''))
		subs = build_subs(em, node, pending)
		offp = entry_alloca(em, em%t_i64, 'voff')
		cntp = entry_alloca(em, em%t_i64, 'vcnt')
		call rt_void(em, 'sy_slice_view', [a, subs, offp, cntp])
		call dispose_all(em, pending)

		leaves(nl)%is_arr = .true.
		leaves(nl)%view = .true.
		leaves(nl)%data = arr_data(em, a)
		leaves(nl)%off = LLVMBuildLoad2(em%bld, em%t_i64, offp, cs(''))
		leaves(nl)%cnt = LLVMBuildLoad2(em%bld, em%t_i64, cntp, cs(''))
		rep = nl

	case (fk_arr, fk_mat)
		nl = nl + 1
		v = emit_expr(em, node)
		leaves(nl)%v = v
		leaves(nl)%ety = v%elem
		if (v%ty /= array_type .or. .not. is_numeric_type(v%elem)) then
			call unsupported(em, 'an operation on an array of type `'//kind_name(v%elem)//'`')
			leaves(nl)%v = dummy_val(em, f64_type)
			leaves(nl)%ety = f64_type
			return
		end if
		leaves(nl)%is_arr = .true.
		leaves(nl)%data = arr_data(em, v%v)
		leaves(nl)%off = ci64(em, 0_8)
		leaves(nl)%cnt = arr_len(em, v%v)
		rep = nl

	case default
		nl = nl + 1
		leaves(nl)%v = emit_expr(em, node)
		leaves(nl)%v%owned = .false.

	end select

end function fuse_collect

!===============================================================================

subroutine fuse_shape_check(em, a, b, op_text)

	! The operands of an operator have to be the same shape, which is only their
	! length if one is a slice, since they are all of rank 1 then

	type(nemitter_t), intent(inout) :: em
	type(fleaf_t), intent(in) :: a, b
	character(len = *), intent(in) :: op_text

	if (a%view .or. b%view) then
		call rt_void(em, 'sy_binop_len', [a%cnt, b%cnt, c_string(em, op_text)])
	else
		call rt_void(em, 'sy_binop_shape', [a%v%v, b%v%v, c_string(em, op_text)])
	end if

end subroutine fuse_shape_check

!===============================================================================

recursive function fuse_elem(em, node, leaves, k, q) result(r)

	! The element `q` of a fused expression, made from the elements of its
	! operands with the scalar operators, which `fuse_collect` made, in the same
	! order that `k` counts.  An operator gives the type of the elements of its
	! array, as it would if it were stored in one

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(fleaf_t), intent(in) :: leaves(:)
	integer, intent(inout) :: k
	type(c_ptr), intent(in) :: q
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: idx

	type(nval_t) :: x, y

	select case (fuse_kind(node))

	case (fk_bin)
		x = fuse_elem(em, node%left, leaves, k, q)
		y = fuse_elem(em, node%right, leaves, k, q)
		r = binop_vals(em, node%op%kind, x, y, node%op%text)
		ety = node%val%array%type
		if (r%ty /= ety .and. is_numeric_type(r%ty)) r = convert(em, r, ety)

	case (fk_neg)
		x = fuse_elem(em, node%right, leaves, k, q)
		r = unary_val(em, node%op%kind, node%op%text, x)
		ety = node%val%array%type
		if (r%ty /= ety .and. is_numeric_type(r%ty)) r = convert(em, r, ety)

	case default
		k = k + 1
		if (leaves(k)%is_arr) then
			idx = q
			if (leaves(k)%view) idx = LLVMBuildAdd(em%bld, q, leaves(k)%off, cs(''))
			r = elem_load(em, leaves(k)%ety, elem_at(em, leaves(k)%data, leaves(k)%ety, idx))
		else
			r = leaves(k)%v
			r%owned = .false.
		end if

	end select

end function fuse_elem

!===============================================================================

function fuse_array(em, root) result(r)

	! An elementwise expression of arrays, which is the operators of the node and
	! of its operands, in one loop that reads the elements of the operands.  A
	! slice is read in place, and nothing is made for the operators in between

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: root
	type(nval_t) :: r

	!********

	integer :: i, nops, nleaf, nview, maxrank, nl, rep, ety, k

	type(c_ptr) :: out, q, od, n

	type(loop_t) :: lp

	type(fleaf_t), allocatable :: leaves(:)

	type(nval_t) :: z

	nops = 0
	nleaf = 0
	nview = 0
	maxrank = 0
	call fuse_scan(root, nops, nleaf, nview, maxrank)

	allocate(leaves(nleaf))
	nl = 0
	rep = fuse_collect(em, root, leaves, nl)

	ety = root%val%array%type
	r = dummy_val(em, array_type)
	if (rep < 1) then
		call unsupported(em, 'an operation on an array')
		return
	end if

	n = leaves(rep)%cnt
	if (leaves(rep)%view) then
		out = rt_call(em, 'sy_arr_vec', em%t_ptr, [ci32(em, elem_code(ety)), n])
	else
		out = rt_call(em, 'sy_arr_like', em%t_ptr, [leaves(rep)%v%v, ci32(em, elem_code(ety))])
	end if
	od = arr_data(em, out)

	call loop_begin(em, n, lp, q)
	k = 0
	z = fuse_elem(em, root, leaves, k, q)
	call elem_store(em, elem_at(em, od, ety, q), ety, z)
	call loop_end(em, lp)

	do i = 1, nleaf
		call dispose(em, leaves(i)%v)
	end do

	r = arr_val(out, .true., root%val)

end function fuse_array

!===============================================================================

function matmul_vals(em, l, rr, res_val) result(r)

	! The matrix product `l @ rr`, where the type of the result is that of
	! `res_val`.  Both operands are converted to it first, as the interpreter does.
	! Two vectors have a scalar product.  This consumes both operands

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: l, rr
	type(value_t), intent(in) :: res_val
	type(nval_t) :: r

	!********

	integer :: ety

	type(nval_t) :: x, y

	ety = res_val%type
	if (ety == array_type .and. allocated(res_val%array)) ety = res_val%array%type

	if (l%ty /= array_type .or. rr%ty /= array_type .or. &
			.not. (ety == i32_type .or. ety == i64_type .or. ety == f32_type .or. &
			ety == f64_type)) then
		call unsupported(em, 'a matrix product of these types')
		r = dummy_val(em, array_type)
		return
	end if

	x = arr_cast(em, l, ety)
	y = arr_cast(em, rr, ety)

	if (res_val%type /= array_type) then
		r%ty = ety
		r%owned = .false.
		r%v = rt_call(em, 'sy_dot_'//type_sfx(ety), ll_type(em, ety), [x%v, y%v, ci32(em, 1)])
	else
		if (ety == f32_type .or. ety == f64_type) em%uses_blas = .true.
		r = arr_val(rt_call(em, 'sy_matmul', em%t_ptr, [x%v, y%v]), .true., res_val)
	end if

	call dispose(em, x)
	call dispose(em, y)

end function matmul_vals

!===============================================================================

function type_sfx(ty) result(s)

	! The suffix of the runtime fns of a numeric type, like `f64` for sy_dot_f64

	integer, intent(in) :: ty
	character(len = :), allocatable :: s

	select case (ty)
	case (i32_type);  s = 'i32'
	case (i64_type);  s = 'i64'
	case (f32_type);  s = 'f32'
	case default;     s = 'f64'
	end select

end function type_sfx

!===============================================================================

function arr_binop_vals(em, op, op_text, l, rr, res_val) result(r)

	! Apply a binary operator to each element, where an operand is a scalar or an
	! array of the same shape as the other, which is one at least.  The
	! elements are made with the scalar operator, so they have the same semantics.
	! `res_val` is the syntran value of the result.  This consumes both operands

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: op
	character(len = *), intent(in) :: op_text
	type(nval_t), intent(in) :: l, rr
	type(value_t), intent(in) :: res_val
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: out, q, shape_src, ld, rd, od

	type(loop_t) :: lp

	type(nval_t) :: x, y, z

	r = dummy_val(em, array_type)

	ety = res_val%array%type
	if (.not. supported_type(ety)) then
		call unsupported(em, 'an operation on an array of type `'//kind_name(ety)//'`')
		return
	end if

	if (l%ty == array_type) then
		shape_src = l%v
	else
		shape_src = rr%v
	end if

	if (l%ty == array_type .and. rr%ty == array_type) &
		call rt_void(em, 'sy_binop_shape', [l%v, rr%v, c_string(em, op_text)])

	out = rt_call(em, 'sy_arr_like', em%t_ptr, [shape_src, ci32(em, elem_code(ety))])

	od = arr_data(em, out)
	if (l%ty == array_type) ld = arr_data(em, l%v)
	if (rr%ty == array_type) rd = arr_data(em, rr%v)

	call loop_begin(em, arr_len(em, shape_src), lp, q)
	if (l%ty == array_type) then
		x = elem_load(em, l%elem, elem_at(em, ld, l%elem, q))
		x%sk = l%sk
	else
		x = l
		x%owned = .false.
	end if
	if (rr%ty == array_type) then
		y = elem_load(em, rr%elem, elem_at(em, rd, rr%elem, q))
		y%sk = rr%sk
	else
		y = rr
		y%owned = .false.
	end if
	z = binop_vals(em, op, x, y, op_text)
	call elem_store(em, elem_at(em, od, ety, q), ety, z)
	call loop_end(em, lp)

	call dispose(em, l)
	call dispose(em, rr)
	r = arr_val(out, .true., res_val)

end function arr_binop_vals

!===============================================================================

recursive function emit_reduce(em, base, a, rtype) result(r)

	! sum(), product(), minval(), maxval(), all(), any(), or count() of a whole
	! array, in storage order.  This consumes `a`

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	type(nval_t), intent(in) :: a
	integer, intent(in) :: rtype
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: acc, q, ignored, init, ad

	type(loop_t) :: lp

	type(nval_t) :: x, cur, nxt

	ety = a%elem
	r = dummy_val(em, rtype)

	select case (base)
	case ('all', 'any', 'count')
		if (ety /= bool_type) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
				kind_name(ety)//'`')
			return
		end if
	case default
		if (.not. is_numeric_type(ety)) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
				kind_name(ety)//'`')
			return
		end if
	end select

	select case (base)
	case ('sum', 'count')
		init = LLVMConstNull(ll_type(em, merge(rtype, ety, base == 'count')))
	case ('product')
		select case (ety)
		case (i32_type)
			init = ci32(em, 1)
		case (i64_type)
			init = ci64(em, 1_8)
		case default
			init = LLVMConstReal(ll_type(em, ety), 1.0_c_double)
		end select
	case ('all')
		init = cbool(em, .true.)
	case ('any')
		init = cbool(em, .false.)
	case ('minval', 'maxval')
		! An empty array has no extreme, which is an error.  If it did, it would be
		! the extreme of the type, like Fortran gives
		call err_if(em, LLVMBuildICmp(em%bld, LLVM_INT_EQ, arr_len(em, a%v), &
			LLVMConstNull(em%t_i64), cs('')), 'R36', 'minval/maxval of an empty array')
		select case (ety)
		case (i32_type)
			init = LLVMConstInt(em%t_i32, merge(2147483647_c_int64_t, -2147483647_c_int64_t - 1, &
				base == 'minval'), 1)
		case (i64_type)
			init = LLVMConstInt(em%t_i64, merge(huge(0_c_int64_t), -huge(0_c_int64_t) - 1, &
				base == 'minval'), 1)
		case (f32_type)
			init = LLVMConstReal(em%t_f32, real(merge(huge(0.0), -huge(0.0), base == 'minval'), c_double))
		case default
			init = LLVMConstReal(em%t_f64, merge(huge(0.0d0), -huge(0.0d0), base == 'minval'))
		end select
	end select

	if (base == 'count') then
		acc = entry_alloca(em, ll_type(em, rtype), 'acc')
	else if (base == 'all' .or. base == 'any') then
		acc = entry_alloca(em, em%t_i1, 'acc')
	else
		acc = entry_alloca(em, ll_type(em, ety), 'acc')
	end if
	ignored = LLVMBuildStore(em%bld, init, acc)

	ad = arr_data(em, a%v)
	call loop_begin(em, arr_len(em, a%v), lp, q)
	x = elem_load(em, ety, elem_at(em, ad, ety, q))

	select case (base)
	case ('sum', 'product', 'minval', 'maxval')
		cur%ty = ety
		cur%owned = .false.
		cur%v = LLVMBuildLoad2(em%bld, ll_type(em, ety), acc, cs(''))
		select case (base)
		case ('sum')
			nxt = binop_vals(em, plus_token, cur, x, '+')
		case ('product')
			nxt = binop_vals(em, star_token, cur, x, '*')
		case default
			nxt = min_max(em, base == 'minval', cur, x)
		end select
		ignored = LLVMBuildStore(em%bld, nxt%v, acc)

	case ('all', 'any')
		cur%ty = bool_type
		cur%owned = .false.
		cur%v = LLVMBuildLoad2(em%bld, em%t_i1, acc, cs(''))
		nxt = binop_vals(em, merge(and_keyword, or_keyword, base == 'all'), cur, x, base)
		ignored = LLVMBuildStore(em%bld, nxt%v, acc)

	case ('count')
		cur%v = LLVMBuildLoad2(em%bld, ll_type(em, rtype), acc, cs(''))
		nxt%v = LLVMBuildAdd(em%bld, cur%v, LLVMBuildZExt(em%bld, x%v, ll_type(em, rtype), cs('')), &
			cs(''))
		ignored = LLVMBuildStore(em%bld, nxt%v, acc)
	end select

	call loop_end(em, lp)
	call dispose(em, a)

	r%ty = merge(rtype, ety, base == 'count')
	if (base == 'all' .or. base == 'any') r%ty = bool_type
	r%owned = .false.
	r%v = LLVMBuildLoad2(em%bld, ll_type(em, r%ty), acc, cs(''))
	r = convert(em, r, rtype)

end function emit_reduce

!===============================================================================

recursive function emit_reduce_ext(em, base, node) result(r)

	! sum(), product(), minval(), maxval(), count(), all(), or any() with a
	! dimension, a mask, or both.  The runtime does all of it, and makes an array
	! with the other dimensions, whose one element is unwrapped if there are none

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: n, op, rety

	logical :: has_dim, has_mask

	type(c_ptr) :: res, dimv, maskv

	type(nval_t) :: a, d, m

	n = size(node%args)
	r = dummy_val(em, node%val%type)

	! `f(a, mask)` is the only one that has a mask in the second place
	has_dim  = node%args(2)%val%type /= array_type
	has_mask = n == 3 .or. .not. has_dim

	a = emit_arg(em, node, 1)
	if (a%ty /= array_type) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
			kind_name(a%ty)//'`', node%identifier%pos)
		call dispose(em, a)
		return
	end if

	if (a%rank == 0 .and. allocated(node%args(1)%val%array)) a%rank = node%args(1)%val%array%rank
	if (a%rank == 0) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of an array of unknown rank', &
			node%identifier%pos)
		call dispose(em, a)
		return
	end if

	select case (base)
	case ('all', 'any', 'count')
		if (a%elem /= bool_type) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
				kind_name(a%elem)//'`', node%identifier%pos)
			call dispose(em, a)
			return
		end if
	case default
		if (.not. is_numeric_type(a%elem)) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
				kind_name(a%elem)//'`', node%identifier%pos)
			call dispose(em, a)
			return
		end if
	end select

	select case (base)
	case ('sum');     op = 0
	case ('product'); op = 1
	case ('minval');  op = 2
	case ('maxval');  op = 3
	case ('count');   op = 4
	case ('all');     op = 5
	case default;     op = 6
	end select

	! No dimension is the smallest i64, which is all of them.  A dimension that the
	! program gives is checked by the runtime
	dimv = ci64(em, -huge(0_8) - 1_8)
	if (has_dim) then
		d = convert(em, emit_arg(em, node, 2), i64_type)
		dimv = d%v
	end if
	maskv = LLVMConstNull(em%t_ptr)
	if (has_mask) then
		m = emit_arg(em, node, n)
		maskv = m%v
	end if

	rety = merge(i64_type, a%elem, base == 'count')
	if (base == 'all' .or. base == 'any') rety = bool_type

	res = rt_call(em, 'sy_arr_reduce', em%t_ptr, [a%v, dimv, ci32(em, op), maskv])
	call dispose(em, a)
	if (has_mask) call dispose(em, m)

	if (has_dim .and. a%rank > 1) then
		r%ty = array_type
		r%owned = .true.
		r%v = res
		r%elem = rety
		r%rank = a%rank - 1
	else
		! The array has one element, which is the result
		r = elem_load(em, rety, elem_ptr(em, res, rety, ci64(em, 0_8)))
		call rt_void(em, 'sy_arr_free', [res])
	end if

end function emit_reduce_ext

!===============================================================================

recursive function emit_intr_call(em, node) result(r)

	! Call of an intrinsic fn which returns a value

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	character(len = :), allocatable :: name, base

	integer :: i, n, iu

	type(c_ptr) :: zero, valp, sizes
	type(c_ptr) :: none(0)
	type(c_ptr), allocatable :: dims(:)

	type(nval_t) :: piece, acc, a, b

	type(place_t) :: pl

	type(c_ptr) :: ignored

	name = node%identifier%text
	base = name
	if (len(name) > 1) then
		if (name(1:1) == '0') then
			! An overload, which the parser named like `0abs_i32`
			base = intr_display_name(name)
		end if
	end if

	n = 0
	if (allocated(node%args)) n = size(node%args)

	select case (base)

	case ('str')
		! The string of each argument, joined
		acc%ty = str_type
		acc%owned = .false.
		acc%v = str_literal(em, '')
		do i = 1, size(node%args)
			piece = str_of(em, emit_arg(em, node, i))
			if (i == 1) then
				acc = piece
			else
				acc = binop_vals(em, plus_token, acc, piece, '+')
			end if
		end do
		if (.not. acc%owned) then
			! `str("a")` is a copy, which can be changed without changing the literal
			acc%v = rt_call(em, 'sy_str_dup', em%t_ptr, [acc%v])
			acc%owned = .true.
		end if
		r = acc

	case ('format')
		r = emit_format(em, node, 1)

	case ('dot')
		a = emit_arg(em, node, 1)
		b = emit_arg(em, node, 2)
		r%ty = node%val%type
		r%owned = .false.
		r%v = rt_call(em, 'sy_dot_'//type_sfx(r%ty), ll_type(em, r%ty), &
			[a%v, b%v, ci32(em, 0)])
		call dispose(em, a)
		call dispose(em, b)

	case ('norm2')
		a = emit_arg(em, node, 1)
		r%ty = node%val%type
		r%owned = .false.
		r%v = rt_call(em, 'sy_norm2_'//type_sfx(r%ty), ll_type(em, r%ty), &
			[arr_data(em, a%v), arr_len(em, a%v)])
		call dispose(em, a)

	case ('shape')
		a = emit_arg(em, node, 1)
		r = arr_val(rt_call(em, 'sy_shape', em%t_ptr, [a%v]), .true., node%val)
		call dispose(em, a)

	case ('transpose')
		a = emit_arg(em, node, 1)
		r = arr_val(rt_call(em, 'sy_transpose', em%t_ptr, [a%v]), .true., node%val)
		r%sk = a%sk
		call dispose(em, a)

	case ('solve')
		em%uses_blas = .true.
		a = emit_arg(em, node, 1)
		b = emit_arg(em, node, 2)
		r = arr_val(rt_call(em, 'sy_solve', em%t_ptr, [a%v, b%v]), .true., node%val)
		call dispose(em, a)
		call dispose(em, b)

	case ('move')
		! Take the array or str out of its variable or member, and leave an empty one
		! there.  The value is then owned, like a fn result, so assigning it moves it
		! with no copy.  The parser checked that the arg is a plain variable or member
		if (place_of(em, node%args(1), pl)) then
			r = elem_load(em, pl%ty, pl%p)
			if (pl%ty == array_type) then
				r%elem = pl%elem
				r%rank = pl%rank
				sizes = store_sizes(em, [(ci64(em, 0_8), i = 1, max(pl%rank, 1))])
				zero = rt_call(em, 'sy_arr_new', em%t_ptr, &
					[ci32(em, elem_code(pl%elem)), ci32(em, max(pl%rank, 1)), sizes])
			else
				zero = str_literal(em, '')
			end if
			ignored = LLVMBuildStore(em%bld, zero, pl%p)
			r%owned = .true.
			call place_done(em, pl)
		else
			r = dummy_val(em, node%val%type)
		end if

	case ('lu_factor')
		! The matrix is changed in place, through the array that the variable holds,
		! and so is `info`, which is a variable
		em%uses_blas = .true.
		a = emit_arg(em, node, 1)
		if (n >= 2) then
			valp = var_ptr(em, node%args(2), i32_type)
		else
			valp = LLVMConstNull(em%t_ptr)
		end if
		r = arr_val(rt_call(em, 'sy_lu_factor', em%t_ptr, [a%v, valp]), .true., node%val)
		call dispose(em, a)

	case ('lu_solve')
		em%uses_blas = .true.
		a = emit_arg(em, node, 1)
		b = emit_arg(em, node, 2)
		piece = emit_arg(em, node, 3)
		call rt_void(em, 'sy_lu_solve', [a%v, b%v, piece%v])
		call dispose(em, a)
		call dispose(em, b)
		call dispose(em, piece)
		r = dummy_val(em, i32_type)

	case ('len')
		! The length is the first field of a string
		a = emit_arg(em, node, 1)
		r%ty = i64_type
		r%owned = .false.
		r%v = LLVMBuildLoad2(em%bld, em%t_i64, a%v, cs(''))
		call dispose(em, a)
		r = convert(em, r, node%val%type)

	case ('size')
		! The number of elements, or the extent along a dimension
		a = emit_arg(em, node, 1)
		r%ty = i64_type
		r%owned = .false.
		if (a%ty /= array_type) then
			call unsupported(em, 'the intrinsic fn `size` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, i64_type)
		else if (n == 1) then
			r%v = arr_len(em, a%v)
		else
			b = convert(em, emit_arg(em, node, 2), i64_type)
			r%v = rt_call(em, 'sy_size_dim', em%t_i64, [a%v, b%v])
		end if
		call dispose(em, a)
		r = convert(em, r, node%val%type)

	case ('sum', 'product', 'minval', 'maxval', 'all', 'any', 'count')
		! Of a whole array, or along a dimension, or of the elements where a mask is true
		if (n /= 1) then
			r = emit_reduce_ext(em, base, node)
		else
			a = emit_arg(em, node, 1)
			if (a%ty /= array_type) then
				call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
					kind_name(a%ty)//'`', node%identifier%pos)
				r = dummy_val(em, node%val%type)
			else
				r = emit_reduce(em, base, a, node%val%type)
			end if
		end if

	case ('parse_i32', 'parse_i64', 'parse_f32', 'parse_f64', 'parse_bool')
		! The `ok` is a variable that the runtime sets, like the `info` of
		! lu_factor(), or null to make bad text a runtime error
		a = emit_arg(em, node, 1)
		if (n >= 2) then
			valp = var_ptr(em, node%args(2), bool_type)
		else
			valp = LLVMConstNull(em%t_ptr)
		end if
		r%owned = .false.
		select case (base)
		case ('parse_i32')
			r%ty = i32_type
			r%v = rt_call(em, 'sy_parse_i32', em%t_i32, [a%v, valp])
		case ('parse_i64')
			r%ty = i64_type
			r%v = rt_call(em, 'sy_parse_i64', em%t_i64, [a%v, valp])
		case ('parse_f32')
			r%ty = f32_type
			r%v = rt_call(em, 'sy_parse_f32', em%t_f32, [a%v, valp])
		case ('parse_f64')
			r%ty = f64_type
			r%v = rt_call(em, 'sy_parse_f64', em%t_f64, [a%v, valp])
		case default
			r%ty = bool_type
			r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_parse_bool', em%t_i8, &
				[a%v, valp]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
		end select
		call dispose(em, a)

	case ('reshape')
		! The same elements in the shape that an array of extents says.  The rank
		! of the result is known to the parser
		a = emit_arg(em, node, 1)
		b = emit_arg(em, node, 2)
		r = dummy_val(em, array_type)
		if (a%ty /= array_type .or. b%ty /= array_type .or. .not. allocated(node%val%array)) then
			call unsupported(em, 'the intrinsic fn `reshape` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
		else
			if (.not. a%owned) then
				a%v = rt_call(em, 'sy_arr_dup', em%t_ptr, [a%v])
				a%owned = .true.
			end if
			iu = node%val%array%rank
			allocate(dims(max(iu, 1)))
			do i = 1, iu
				piece = convert(em, elem_load(em, b%elem, elem_ptr(em, b%v, b%elem, &
					ci64(em, int(i - 1, 8)))), i64_type)
				dims(i) = piece%v
			end do
			sizes = store_sizes(em, dims(1: iu))
			call rt_void(em, 'sy_reshape_check', [a%v, ci32(em, iu), sizes])
			r = arr_val(rt_call(em, 'sy_arr_reshape', em%t_ptr, [a%v, ci32(em, iu), &
				sizes]), .true., node%val)
		end if
		call dispose(em, b)

	case ('open', 'try_open')
		! A new handle, which is owned like a string is
		a = emit_arg(em, node, 1)
		b = emit_arg(em, node, 2)
		r%ty = file_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_file_open', em%t_ptr, &
			[a%v, b%v, ci32(em, merge(1, 0, base == 'open'))])
		call dispose(em, a)
		call dispose(em, b)

	case ('readln')
		! A line of a file, or of stdin without one
		r%ty = str_type
		r%owned = .true.
		if (n == 0) then
			r%v = rt_call(em, 'sy_stdin_readln', em%t_ptr, [c_ptr ::])
		else
			a = emit_arg(em, node, 1)
			r%v = rt_call(em, 'sy_file_readln', em%t_ptr, [a%v])
			call dispose(em, a)
		end if

	case ('eof')
		r%ty = bool_type
		r%owned = .false.
		if (n == 0) then
			r%v = rt_call(em, 'sy_stdin_at_eof', em%t_i8, [c_ptr ::])
		else
			a = emit_arg(em, node, 1)
			r%v = rt_call(em, 'sy_file_eof', em%t_i8, [a%v])
			call dispose(em, a)
		end if
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, r%v, LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))

	case ('args')
		r = arr_val(rt_call(em, 'sy_args', em%t_ptr, none), .true., node%val)

	case ('getenv')
		a = emit_arg(em, node, 1)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_getenv', em%t_ptr, [a%v])
		call dispose(em, a)

	case ('hasenv')
		a = emit_arg(em, node, 1)
		r%ty = bool_type
		r%owned = .false.
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_hasenv', em%t_i8, [a%v]), &
			LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
		call dispose(em, a)

	case ('read_file')
		! The text of a file, which is owned like a string is
		a = emit_arg(em, node, 1)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_read_file', em%t_ptr, [a%v])
		call dispose(em, a)

	case ('system')
		a = emit_arg(em, node, 1)
		r%ty = i32_type
		r%owned = .false.
		r%v = rt_call(em, 'sy_system', em%t_i32, [a%v])
		call dispose(em, a)

	case ('exists')
		a = emit_arg(em, node, 1)
		r%ty = bool_type
		r%owned = .false.
		r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_exists', em%t_i8, [a%v]), &
			LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
		call dispose(em, a)

	case ('read', 'read_i32', 'read_i64', 'read_f32', 'read_f64')
		! std::read_i32() etc. of a value, or of `n` of them as an array
		a = emit_arg(em, node, 1)
		if (node%val%type == array_type .and. allocated(node%val%array)) then
			iu = node%val%array%type
			b = convert(em, emit_arg(em, node, 2), i64_type)
			r = arr_val(rt_call(em, 'sy_file_read_arr', em%t_ptr, [a%v, ci32(em, elem_code(iu)), &
				b%v]), .true., node%val)
		else
			r%ty = node%val%type
			r%owned = .false.
			select case (r%ty)
			case (i64_type)
				r%v = rt_call(em, 'sy_file_read_i64', em%t_i64, [a%v])
			case (f32_type)
				r%v = rt_call(em, 'sy_file_read_f32', em%t_f32, [a%v])
			case (f64_type)
				r%v = rt_call(em, 'sy_file_read_f64', em%t_f64, [a%v])
			case default
				r%v = rt_call(em, 'sy_file_read_i32', em%t_i32, [a%v])
			end select
		end if
		call dispose(em, a)

	case ('repeat')
		a = emit_arg(em, node, 1)
		b = convert(em, emit_arg(em, node, 2), i64_type)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_repeat', em%t_ptr, [a%v, b%v])
		call dispose(em, a)

	case ('char')
		a = convert(em, emit_arg(em, node, 1), i32_type)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_char', em%t_ptr, [a%v])

	case ('i32', 'i64', 'f32', 'f64', 'exp', 'log', 'log10', 'sqrt', 'cos', 'sin', 'tan', &
			'acos', 'asin', 'atan', 'log2', 'abs', 'sind', 'cosd', 'tand', 'asind', 'acosd', &
			'atand')
		! Of a number, or of each element of an array
		a = emit_arg(em, node, 1)
		if (a%ty == array_type) then
			r = emit_intr_map(em, base, a, node)
		else
			r = intr_scalar(em, base, a, node%val%type, node%identifier%pos)
		end if

	case ('stack_trace', 'caller')
		! The calls that led here, as a text.  The argument is the number of frames to
		! leave out, or the depth: 1 by default for std::caller(), the fn that called
		! this one
		if (n >= 1) then
			a = convert(em, emit_arg(em, node, 1), i32_type)
			valp = a%v
		else
			valp = ci32(em, merge(1, 0, base == 'caller'))
		end if
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_'//base, em%t_ptr, [valp])

	case ('min', 'max')
		! Of any number of arguments, as a chain.  A NaN loses to a number, like
		! it does in Fortran
		acc = emit_arg(em, node, 1)
		do i = 2, n
			b = emit_arg(em, node, i)
			acc = min_max(em, base == 'min', acc, b)
		end do
		r = acc

	case default
		call unsupported(em, 'the intrinsic fn `'// &
			intr_display_name(node%identifier%text)//'`', node%identifier%pos)
		r = dummy_val(em, node%val%type)

	end select

end function emit_intr_call

!===============================================================================

function intr_scalar(em, base, a, rtype, pos) result(r)

	! A conversion or a math fn of one number, where `rtype` is the type of the
	! result.  This consumes `a`

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	type(nval_t), intent(in) :: a
	integer, intent(in) :: rtype, pos
	type(nval_t) :: r

	type(c_ptr) :: zero

	select case (base)

	case ('i32', 'i64')
		if (a%ty == str_type) then
			! The code of a single character
			r%ty = i32_type
			r%owned = .false.
			r%v = rt_call(em, 'sy_str_code', em%t_i32, [a%v, ci32(em, merge(1, 0, base == 'i64'))])
			call dispose(em, a)
			r = convert(em, r, merge(i32_type, i64_type, base == 'i32'))
		else if (a%ty == enum_type) then
			! The backing value of the variant
			r%ty = i32_type
			r%owned = .false.
			r%v = enum_backing(em, a)
			r = convert(em, r, merge(i32_type, i64_type, base == 'i32'))
		else if (is_numeric_type(a%ty)) then
			r = convert(em, a, merge(i32_type, i64_type, base == 'i32'))
		else
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', pos)
			r = dummy_val(em, rtype)
		end if

	case ('f32', 'f64')
		if (is_numeric_type(a%ty)) then
			r = convert(em, a, merge(f32_type, f64_type, base == 'f32'))
		else
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', pos)
			r = dummy_val(em, rtype)
		end if

	case ('exp', 'log', 'log10', 'sqrt', 'cos', 'sin', 'tan', 'acos', 'asin', 'atan', &
			'log2')
		if (a%ty /= f32_type .and. a%ty /= f64_type) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', pos)
			r = dummy_val(em, rtype)
			return
		end if
		r%ty = a%ty
		r%owned = .false.
		if (base == 'log2') then
			! The interpreter's log2 is log(x) / log(2), in the argument's own
			! precision
			zero = LLVMConstReal(ll_type(em, a%ty), 2.0_c_double)
			r%v = LLVMBuildFDiv(em%bld, libm(em, 'log', a%ty, a%v), &
				libm(em, 'log', a%ty, zero), cs(''))
		else
			r%v = libm(em, base, a%ty, a%v)
		end if

	case ('sind', 'cosd', 'tand', 'asind', 'acosd', 'atand')
		if (a%ty /= f32_type .and. a%ty /= f64_type) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', pos)
			r = dummy_val(em, rtype)
			return
		end if
		r%ty = a%ty
		r%owned = .false.
		if (base(len(base):) == 'd' .and. base(1:1) /= 'a') then
			! Of degrees, in the runtime
			r%v = rt_call(em, 'sy_'//base//'_'//type_sfx(a%ty), ll_type(em, a%ty), [a%v])
		else
			! In degrees, which the interpreter makes by multiplying radians by 180 / pi
			r%v = LLVMBuildFMul(em%bld, libm(em, base(1: len(base) - 1), a%ty, a%v), &
				LLVMConstReal(ll_type(em, a%ty), 57.295779513082322865_c_double), cs(''))
		end if

	case ('abs')
		r%ty = a%ty
		r%owned = .false.
		select case (a%ty)
		case (i32_type)
			r%v = rt_call(em, 'llvm.abs.i32', em%t_i32, [a%v, cbool(em, .false.)])
		case (i64_type)
			r%v = rt_call(em, 'llvm.abs.i64', em%t_i64, [a%v, cbool(em, .false.)])
		case (f32_type)
			r%v = rt_call(em, 'llvm.fabs.f32', em%t_f32, [a%v])
		case (f64_type)
			r%v = rt_call(em, 'llvm.fabs.f64', em%t_f64, [a%v])
		case default
			call unsupported(em, 'the intrinsic fn `abs` of a value of type `'// &
				kind_name(a%ty)//'`', pos)
			r = dummy_val(em, rtype)
		end select

	end select

end function intr_scalar

!===============================================================================

function arr_cast(em, a, ety) result(r)

	! The elements of a numeric array converted to the type `ety`.  This consumes
	! `a`, and any other array is left as it is

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: a
	integer, intent(in) :: ety
	type(nval_t) :: r

	!********

	type(c_ptr) :: out, q, ad, od

	type(loop_t) :: lp

	type(nval_t) :: x, y

	if (a%elem == ety .or. .not. (is_numeric_type(a%elem) .and. is_numeric_type(ety))) then
		r = a
		return
	end if

	out = rt_call(em, 'sy_arr_like', em%t_ptr, [a%v, ci32(em, elem_code(ety))])
	ad = arr_data(em, a%v)
	od = arr_data(em, out)
	call loop_begin(em, arr_len(em, a%v), lp, q)
	x = elem_load(em, a%elem, elem_at(em, ad, a%elem, q))
	y = convert(em, x, ety)
	call elem_store(em, elem_at(em, od, ety, q), ety, y)
	call loop_end(em, lp)

	call dispose(em, a)
	r = a
	r%v = out
	r%owned = .true.
	r%elem = ety

end function arr_cast

!===============================================================================

recursive function emit_intr_map(em, base, a, node) result(r)

	! A scalar fn of each element of an array.  This consumes `a`

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	type(nval_t), intent(in) :: a
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: out, q, ad, od

	type(loop_t) :: lp

	type(nval_t) :: x, y

	r = dummy_val(em, array_type)
	if (.not. allocated(node%val%array)) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of an array', node%identifier%pos)
		return
	end if

	ety = node%val%array%type
	if (.not. supported_type(ety) .or. (a%elem == str_type .and. base /= 'i32' .and. &
			base /= 'i64')) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
			kind_name(a%elem)//'`', node%identifier%pos)
		return
	end if

	out = rt_call(em, 'sy_arr_like', em%t_ptr, [a%v, ci32(em, elem_code(ety))])
	ad = arr_data(em, a%v)
	od = arr_data(em, out)
	call loop_begin(em, arr_len(em, a%v), lp, q)
	x = elem_load(em, a%elem, elem_at(em, ad, a%elem, q))
	x%sk = a%sk
	y = intr_scalar(em, base, x, ety, node%identifier%pos)
	call elem_store(em, elem_at(em, od, ety, q), ety, y)
	call loop_end(em, lp)

	call dispose(em, a)
	r = arr_val(out, .true., node%val)

end function emit_intr_map

!===============================================================================

function libm(em, base, ty, x) result(v)

	! A math fn of the C library, `sin` or `sinf`, which LLVM knows how to fold

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	integer, intent(in) :: ty
	type(c_ptr), intent(in) :: x
	type(c_ptr) :: v

	if (ty == f32_type) then
		v = rt_call(em, base//'f', em%t_f32, [x])
	else
		v = rt_call(em, base, em%t_f64, [x])
	end if

end function libm

!===============================================================================

function min_max(em, is_min, a, b) result(r)

	! min() or max() of two numbers, which are converted to the wider type

	type(nemitter_t), intent(inout) :: em
	logical, intent(in) :: is_min
	type(nval_t), intent(in) :: a, b
	type(nval_t) :: r

	integer :: ct

	type(nval_t) :: x, y

	if (.not. (is_numeric_type(a%ty) .and. is_numeric_type(b%ty))) then
		call unsupported(em, 'min() or max() of a value of type `'//kind_name(a%ty)//'`')
		r = dummy_val(em, a%ty)
		return
	end if

	ct = wider_type(a%ty, b%ty)
	x = convert(em, a, ct)
	y = convert(em, b, ct)
	r%ty = ct
	r%owned = .false.

	select case (ct)
	case (i32_type)
		r%v = rt_call(em, merge('llvm.smin.i32', 'llvm.smax.i32', is_min), em%t_i32, [x%v, y%v])
	case (i64_type)
		r%v = rt_call(em, merge('llvm.smin.i64', 'llvm.smax.i64', is_min), em%t_i64, [x%v, y%v])
	case (f32_type)
		r%v = rt_call(em, merge('llvm.minnum.f32', 'llvm.maxnum.f32', is_min), em%t_f32, [x%v, y%v])
	case (f64_type)
		r%v = rt_call(em, merge('llvm.minnum.f64', 'llvm.maxnum.f64', is_min), em%t_f64, [x%v, y%v])
	end select

end function min_max

!===============================================================================
! Statements

recursive subroutine emit_stmt(em, node)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: i

	integer :: prev_loc

	type(nval_t) :: v

	! A statement that fails reports where it is.  Like the interpreter, the
	! statement that contains this one is the location again after it
	prev_loc = em%cur_loc
	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
		em%cur_loc = loc_id(em)
	end if

	if (node%is_empty) then
		em%cur_loc = prev_loc
		return
	end if

	if (node%src_id > 0) call set_loc(em)

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .false.)

	case (let_expr)
		call emit_let(em, node)

	case (assignment_expr)
		call emit_assign(em, node)

	case (fn_call_intr_expr)
		call emit_call_stmt(em, node)

	case (if_statement)
		call emit_if(em, node, .false.)

	case (while_statement)
		call emit_while(em, node)

	case (for_statement)
		call emit_for(em, node)

	case (switch_statement)
		call emit_switch(em, node, .false.)

	case (use_statement)
		call emit_use(em, node)

	case (break_statement)
		if (em%depth > 0) then
			call branch_away(em, em%brk(em%depth))
		else if (c_associated(em%block_exit)) then
			call branch_away(em, em%block_exit)
		end if

	case (continue_statement)
		if (em%depth > 0) call branch_away(em, em%cnt(em%depth))

	case (return_statement)
		call emit_return(em, node)

	case (fn_declaration, struct_declaration, enum_declaration)
		! Built separately, since a fn is its own LLVM function.  A struct is
		! only a type, and its methods are fns

	case default
		! A bare expression statement, which at most has side effects from calls
		v = emit_expr(em, node)
		call dispose(em, v)

	end select

	em%cur_loc = prev_loc

end subroutine emit_stmt

!===============================================================================

recursive subroutine emit_let(em, node)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	type(c_ptr) :: p
	type(nval_t) :: v

	v = emit_expr(em, node%right)
	p = var_ptr(em, node, node%val%type)
	if (.not. c_associated(p)) then
		call dispose(em, v)
		return
	end if
	call store_var(em, p, node%val%type, v)

end subroutine emit_let

!===============================================================================

recursive subroutine emit_assign(em, node)

	! An assignment statement.  Its value is not used

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	type(nval_t) :: v

	v = emit_assign_val(em, node, .true.)
	call dispose(em, v)

end subroutine emit_assign

!===============================================================================

recursive function emit_assign_val(em, node, as_stmt) result(res)

	! Plain and compound assignment to a variable or an element of an array.  The
	! value is that of the target afterward, which is lent, since `a = b = 1` is
	! an assignment of an assignment

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in), optional :: as_stmt
	type(nval_t) :: res

	!********

	integer :: op, ety, vty, rank_, imove

	logical :: as_stmt_, by_idx, ok

	type(c_ptr) :: p, ps, a, subs, valp, ignored
	type(c_ptr), allocatable :: saved(:)

	type(nval_t) :: v, cur
	type(nval_t), allocatable :: pending(:)

	type(place_t) :: pl

	res = dummy_val(em, node%val%type)

	as_stmt_ = .false.
	if (present(as_stmt)) as_stmt_ = as_stmt

	if (node%op%kind /= equals_token) then
		! `x += y` is `x = x + y`
		select case (node%op%kind)
		case (plus_equals_token);    op = plus_token
		case (minus_equals_token);   op = minus_token
		case (star_equals_token);    op = star_token
		case (slash_equals_token);   op = slash_token
		case (percent_equals_token); op = percent_token
		case (double_star_equals_token);   op = double_star_token
		case (double_less_equals_token);   op = double_less_token
		case (double_greater_equals_token);op = double_greater_token
		case (amp_equals_token);     op = amp_token
		case (pipe_equals_token);    op = pipe_token
		case (caret_equals_token);   op = caret_token
		case default
			call unsupported(em, 'the `'//node%op%text//'` operator')
			return
		end select
	end if

	if (allocated(node%member)) then
		! A member of a struct, or an element of one.  The value is made after the
		! subscripts of the target, like the interpreter does.  The place is made
		! after the value, because the value might change what the struct is.  So the
		! subscripts are made first to find their values, and again, to find the
		! place, with the values that they had
		by_idx = node%val%type /= str_type .and. chain_scalar_subs(node) > 0
		if (by_idx) then
			em%idx_mode = 1
			em%idx_n = 0
			em%assigning = .true.
			ok = place_of(em, node, pl)
			em%assigning = .false.
			em%idx_mode = 0
			call place_done(em, pl)
			if (.not. ok) return
			saved = em%idx_slots(1: em%idx_n)
		end if
		imove = 0
		if (node%op%kind == equals_token) imove = struct_move_chain(node%right, node)
		if (imove > 0) then
			v = emit_struct_instance(em, node%right, imove)
		else
			v = emit_expr(em, node%right)
		end if
		if (by_idx) then
			em%idx_slots = saved
			em%idx_n = 0
			em%idx_mode = 2
		end if
		em%assigning = .true.
		ok = place_of(em, node, pl)
		em%assigning = .false.
		em%idx_mode = 0
		if (.not. ok) then
			call dispose(em, v)
			return
		end if
		if (node%op%kind /= equals_token) then
			cur = place_load(em, pl)
			if (cur%ty == array_type) then
				if (node%op%kind == matmul_token) then
					call unsupported(em, 'a matrix product')
					return
				end if
				if (.not. allocated(node%val%array)) then
					call unsupported(em, 'an operation on an array')
					return
				end if
				v = to_elem_type(em, v, node%val%array%type)
				v = arr_binop_vals(em, op, node%op%text, cur, v, node%val)
			else if (pl%has_subs) then
				call unsupported(em, 'a compound assignment to a substring')
				return
			else
				v = binop_vals(em, op, cur, v, node%op%text)
			end if
		end if
		call place_store(em, pl, v)
		if (pl%has_subs .and. present(as_stmt)) then
			if (as_stmt) then
				call place_done(em, pl)
				return
			end if
		end if
		res = place_load(em, pl)
		call place_done(em, pl)
		return
	end if

	if (allocated(node%lsubscripts)) then
		if (.not. ref_info(em, node, ps, vty, ety, rank_)) return

		if (vty == str_type .or. size(node%lsubscripts) /= rank_ .or. .not. all_scalar(node)) then
			! A character or substring, or a slice
			if (node%op%kind /= equals_token) then
				! Only a slice of an array, which is made, operated on, and put back
				if (vty /= array_type .or. size(node%lsubscripts) /= rank_ .or. &
						node%op%kind == matmul_token .or. .not. allocated(node%val%array)) then
					call unsupported(em, 'a compound assignment to a slice or substring')
					return
				end if
			end if
			if (vty == str_type .and. size(node%lsubscripts) /= 1) then
				call unsupported(em, 'this subscript of a string')
				return
			end if
			if (vty == array_type .and. size(node%lsubscripts) /= rank_ .and. .not. &
					(size(node%lsubscripts) == rank_ + 1 .and. ety == str_type)) then
				call unsupported(em, 'a subscript with the wrong number of dimensions')
				return
			end if

			subs = build_subs(em, node, pending)
			v = emit_expr(em, node%right)
			if (vty == str_type) then
				if (v%ty /= str_type) then
					call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a substring')
				else
					call rt_void(em, 'sy_str_set', [ps, subs, v%v])
				end if
			else
				a = LLVMBuildLoad2(em%bld, em%t_ptr, ps, cs(''))
				if (size(node%lsubscripts) == rank_ + 1) then
					! A substring of each of the elements of an array of strings
					if (v%ty == array_type .and. v%elem == str_type) then
						call rt_void(em, 'sy_arr_str_set_arr', [a, subs, v%v])
					else if (v%ty /= str_type) then
						call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a substring')
					else
						call rt_void(em, 'sy_arr_str_set', [a, subs, v%v])
					end if
				else
					if (node%op%kind /= equals_token) then
						cur = arr_val(rt_call(em, 'sy_arr_slice', em%t_ptr, [a, subs]), .true., node%val)
						v = arr_binop_vals(em, op, node%op%text, cur, v, node%val)
					end if
					call arr_slice_store(em, a, subs, ety, v)
					if (.not. as_stmt_) res = arr_val(rt_call(em, 'sy_arr_slice', em%t_ptr, [a, subs]), &
						.true., node%val)
				end if
			end if
			call dispose(em, v)
			call dispose_all(em, pending)
			return
		end if

		! An element
		p = elem_target(em, node, ety)
		if (.not. c_associated(p)) return

		v = emit_expr(em, node%right)
		if (node%op%kind /= equals_token) then
			cur = elem_load(em, ety, p)
			v = binop_vals(em, op, cur, v, node%op%text)
		end if
		call elem_store(em, p, ety, v)
		res = elem_load(em, ety, p)
		return
	end if

	p = var_ptr(em, node, node%val%type)
	if (.not. c_associated(p)) return

	imove = 0
	if (node%op%kind == equals_token .and. node%val%type == struct_type) &
		imove = struct_move_member(node%right, node%id_index, node%is_loc)
	if (node%op%kind == equals_token .and. node%val%type == struct_type .and. &
			take_member_assign(node)) then
		! `x = x.a.b` takes the member out of x, instead of copying it and then
		! freeing all of x
		v = take_place(em, node%right)
	else if (imove > 0) then
		v = emit_struct_instance(em, node%right, imove)
	else
		v = emit_expr(em, node%right)
	end if

	if (node%op%kind /= equals_token) then
		cur = load_var(em, node)
		if (is_arr(node%val)) then
			if (node%op%kind == matmul_token) then
				call unsupported(em, 'a matrix product')
				return
			end if
			if (allocated(node%val%array)) v = to_elem_type(em, v, node%val%array%type)
			v = arr_binop_vals(em, op, node%op%text, cur, v, node%val)
		else
			v = binop_vals(em, op, cur, v, node%op%text)
		end if
	end if

	if (slot_type(em, node) == array_type .and. v%ty == array_type .and. &
			allocated(node%val%array)) then
		! The array keeps the type of its elements, e.g. an i32 array that is
		! assigned the i64 array `[x, y]`
		if (v%elem /= node%val%array%type) v = arr_cast(em, v, node%val%array%type)
	end if

	if (slot_type(em, node) == array_type .and. v%ty /= array_type .and. &
			node%op%kind == equals_token) then
		! `a = x` sets every element of the array and keeps its shape
		ety = node%val%array%type
		if (is_numeric_type(ety)) v = convert(em, v, ety)
		call arr_set_all(em, LLVMBuildLoad2(em%bld, em%t_ptr, p, cs('')), ety, v)
	else
		call store_var(em, p, slot_type(em, node), v)
	end if
	res = load_var(em, node)

end function emit_assign_val

!===============================================================================

function to_elem_type(em, v, ety) result(r)

	! The right hand side of a compound assignment to a whole array, in the type of
	! the elements of the array, before the operation.  So `a *= 2.5` of an array of
	! ints multiplies by 2, as the interpreter does, but `a[i] *= 2.5` doesn't.
	! Others convert the result

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v
	integer, intent(in) :: ety
	type(nval_t) :: r

	r = v
	if (.not. is_numeric_type(ety)) return
	if (v%ty == array_type) then
		if (v%elem /= ety .and. is_numeric_type(v%elem)) r = arr_cast(em, v, ety)
	else if (is_numeric_type(v%ty) .and. v%ty /= ety) then
		r = convert(em, v, ety)
	end if

end function to_elem_type

!===============================================================================

recursive subroutine emit_call_stmt(em, node)

	! A call whose value, if any, is discarded

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: i, n

	type(c_ptr) :: parts, slot, ignored
	type(c_ptr), allocatable :: args(:)

	type(nval_t) :: v, fs
	type(nval_t), allocatable :: pieces(:)

	select case (node%identifier%text)

	case ('println', 'print')
		! The whole line is written at once, after evaluating every argument,
		! like the interpreter does
		n = 0
		if (allocated(node%args)) n = size(node%args)

		allocate(pieces(max(n, 1)))
		do i = 1, n
			pieces(i) = str_of(em, emit_expr(em, node%args(i)))
			if (later_clobbers(node, i)) call own_if_borrowed(em, pieces(i))
		end do

		parts = entry_alloca(em, LLVMArrayType2(em%t_ptr, int(max(n, 1), c_int64_t)), &
			'parts')
		do i = 1, n
			slot = LLVMBuildGEP2(em%bld, em%t_ptr, parts, [ci32(em, i - 1)], 1, cs(''))
			ignored = LLVMBuildStore(em%bld, pieces(i)%v, slot)
		end do

		if (node%identifier%text == 'println') then
			call rt_void(em, 'sy_println', [parts, ci32(em, n)])
		else
			call rt_void(em, 'sy_print', [parts, ci32(em, n)])
		end if

		do i = 1, n
			call dispose(em, pieces(i))
		end do

	case ('printf')
		! print(format(...))
		fs = emit_format(em, node, 1)
		parts = entry_alloca(em, LLVMArrayType2(em%t_ptr, 1_c_int64_t), 'parts')
		ignored = LLVMBuildStore(em%bld, fs%v, parts)
		call rt_void(em, 'sy_print', [parts, ci32(em, 1)])
		call dispose(em, fs)

	case ('writef')
		! write(f, format(...))
		v = emit_expr(em, node%args(1))
		fs = emit_format(em, node, 2)
		call rt_void(em, 'sy_file_wcheck', [v%v, ci32(em, 0)])
		call rt_void(em, 'sy_file_wstr', [v%v, fs%v])
		call dispose(em, fs)
		call dispose(em, v)

	case ('exit')
		v = convert(em, emit_expr(em, node%args(1)), i32_type)
		call rt_void(em, 'sy_exit', [v%v])

	case ('print_trace')
		! The label is optional
		if (allocated(node%args)) then
			if (size(node%args) >= 1) then
				v = emit_expr(em, node%args(1))
				call rt_void(em, 'sy_print_trace', [v%v])
				call dispose(em, v)
				return
			end if
		end if
		call rt_void(em, 'sy_print_trace', [LLVMConstNull(em%t_ptr)])

	case ('writeln')
		call emit_file_write(em, node, .true.)

	case ('write')
		call emit_file_write(em, node, .false.)

	case ('close')
		v = emit_expr(em, node%args(1))
		call rt_void(em, 'sy_file_close', [v%v])
		call dispose(em, v)

	case default
		v = emit_intr_call(em, node)
		call dispose(em, v)

	end select

end subroutine emit_call_stmt

!===============================================================================

function fspec_global(em, spec) result(g)

	! The parsed spec of a placeholder of a format string, as a constant of the
	! program, which has the layout of `sy_fspec` in the runtime

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: spec
	type(c_ptr) :: g

	!********

	integer :: ierr

	type(c_ptr) :: vals(8)

	type(rt_fmt_spec_t) :: sp

	call rt_fmt_parse(spec, sp, ierr)

	vals(1) = ci32(em, iachar(sp%fill))
	vals(2) = ci32(em, iachar(sp%align))
	vals(3) = ci32(em, iachar(sp%ty))
	vals(4) = ci32(em, merge(1, 0, sp%plus))
	vals(5) = ci32(em, merge(1, 0, sp%alt))
	vals(6) = ci32(em, merge(1, 0, sp%zero))
	vals(7) = ci32(em, sp%width)
	vals(8) = ci32(em, sp%prec)

	g = LLVMAddGlobal(em%llmod, LLVMArrayType2(em%t_i32, 8_c_int64_t), cs('.fs.'//str(em%nstr)))
	em%nstr = em%nstr + 1
	call LLVMSetInitializer(g, LLVMConstArray(em%t_i32, vals, 8))
	call LLVMSetGlobalConstant(g, 1)
	call LLVMSetLinkage(g, LLVM_PRIVATE_LINKAGE)

end function fspec_global

!===============================================================================

recursive function emit_format(em, node, k) result(r)

	! format() of the call `node`, where the argument `k` is the literal format
	! string and the values are the arguments after it.  The text between the
	! placeholders is split off here, and each value is formatted by the runtime
	! with the parsed spec of its placeholder.  This is a new string

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	integer, intent(in) :: k
	type(nval_t) :: r

	!********

	integer :: i, n, ierr

	type(c_ptr) :: pieces, vals, slot, spec, ignored

	type(nval_t) :: v, f, g

	type(rt_str_t), allocatable :: fpieces(:), fspecs(:)

	r = dummy_val(em, str_type)

	call rt_fmt_split(node%args(k)%val%str%s, fpieces, fspecs, ierr)
	n = size(fspecs)

	pieces = entry_alloca(em, LLVMArrayType2(em%t_ptr, int(n + 1, c_int64_t)), 'pieces')
	vals   = entry_alloca(em, LLVMArrayType2(em%t_ptr, int(max(n, 1), c_int64_t)), 'fvals')

	do i = 1, n
		slot = LLVMBuildGEP2(em%bld, em%t_ptr, pieces, [ci32(em, i - 1)], 1, cs(''))
		ignored = LLVMBuildStore(em%bld, str_literal(em, fpieces(i)%s), slot)
	end do
	slot = LLVMBuildGEP2(em%bld, em%t_ptr, pieces, [ci32(em, n)], 1, cs(''))
	ignored = LLVMBuildStore(em%bld, str_literal(em, fpieces(n + 1)%s), slot)

	do i = 1, n
		v = emit_expr(em, node%args(k + i))
		spec = fspec_global(em, fspecs(i)%s)

		select case (v%ty)
		case (i32_type)
			f%v = rt_call(em, 'sy_format_i32', em%t_ptr, [v%v, spec])
		case (i64_type)
			f%v = rt_call(em, 'sy_format_i64', em%t_ptr, [v%v, spec])
		case (f32_type)
			f%v = rt_call(em, 'sy_format_f32', em%t_ptr, [v%v, spec])
		case (f64_type)
			f%v = rt_call(em, 'sy_format_f64', em%t_ptr, [v%v, spec])
		case (bool_type)
			f%v = rt_call(em, 'sy_format_bool', em%t_ptr, &
				[LLVMBuildZExt(em%bld, v%v, em%t_i8, cs('')), spec])
		case (str_type)
			f%v = rt_call(em, 'sy_format_str', em%t_ptr, [v%v, spec])
		case (array_type)
			if (v%elem == i32_type .or. v%elem == i64_type .or. v%elem == f32_type .or. &
					v%elem == f64_type .or. v%elem == bool_type .or. v%elem == str_type) then
				f%v = rt_call(em, 'sy_format_arr', em%t_ptr, [v%v, spec])
			else
				call unsupported(em, 'formatting an array of type `'//kind_name(v%elem)//'`', &
					node%identifier%pos)
			end if
		case (struct_type, enum_type)
			! The string that str() makes of it
			g = str_of(em, v)
			v%owned = .false.
			f%v = rt_call(em, 'sy_format_str', em%t_ptr, [g%v, spec])
			call dispose(em, g)
		case default
			call unsupported(em, 'formatting a value of type `'//kind_name(v%ty)//'`', &
				node%identifier%pos)
		end select

		slot = LLVMBuildGEP2(em%bld, em%t_ptr, vals, [ci32(em, i - 1)], 1, cs(''))
		ignored = LLVMBuildStore(em%bld, f%v, slot)
		call dispose(em, v)
	end do

	r%ty = str_type
	r%owned = .true.
	r%v = rt_call(em, 'sy_format_join', em%t_ptr, [ci32(em, n), pieces, vals])

end function emit_format

!===============================================================================

recursive subroutine emit_file_write(em, node, is_writeln)

	! writeln() and write() of a file and any number of values.  Every value is
	! built first, then the file is checked, and then each value is written on
	! its own, as its string in a text file or as its bytes in a binary one,
	! which the runtime tells apart

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_writeln

	!********

	integer :: i, n

	type(c_ptr) :: names, tname

	type(nval_t) :: f
	type(nval_t), allocatable :: vals(:)

	n = size(node%args) - 1
	f = emit_expr(em, node%args(1))
	allocate(vals(max(n, 1)))
	do i = 1, n
		vals(i) = emit_expr(em, node%args(i + 1))
		if (later_clobbers(node, i + 1)) call own_if_borrowed(em, vals(i))
	end do

	call rt_void(em, 'sy_file_wcheck', [f%v, ci32(em, merge(1, 0, is_writeln))])

	do i = 1, n
		associate (v => vals(i))

			tname = c_string(em, value_type_name(node%args(i + 1)%val))

			select case (v%ty)
			case (i32_type)
				call rt_void(em, 'sy_file_wi32', [f%v, v%v])
			case (i64_type)
				call rt_void(em, 'sy_file_wi64', [f%v, v%v])
			case (f32_type)
				call rt_void(em, 'sy_file_wf32', [f%v, v%v])
			case (f64_type)
				call rt_void(em, 'sy_file_wf64', [f%v, v%v])
			case (bool_type)
				call rt_void(em, 'sy_file_wbool', [f%v, LLVMBuildZExt(em%bld, v%v, em%t_i8, cs(''))])
			case (str_type)
				call rt_void(em, 'sy_file_wstr', [f%v, v%v])
			case (array_type)
				names = LLVMConstNull(em%t_ptr)
				if (v%elem == enum_type) names = enum_names(em, v%sk)
				call rt_void(em, 'sy_file_warr', [f%v, v%v, names, tname])
			case (struct_type)
				call rt_void(em, 'sy_file_wstruct', [f%v, v%v, tname])
			case (enum_type)
				call rt_void(em, 'sy_file_wenum', [f%v, v%v, enum_names(em, v%sk), tname])
			case default
				call unsupported(em, 'writing a value of type `'//kind_name(v%ty)//'` to a file')
			end select

		end associate
	end do

	if (is_writeln) call rt_void(em, 'sy_file_wnl', [f%v])

	do i = 1, n
		call dispose(em, vals(i))
	end do
	call dispose(em, f)

end subroutine emit_file_write

!===============================================================================
! The result of the program

recursive subroutine emit_result_stmt(em, node)

	! Emit the last statement of the program such that its value is the program's
	! result.  This is what the interpreter does.  A statement that has no
	! value, like a loop, has an invalid one

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: i

	type(nval_t) :: v

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
		em%cur_loc = loc_id(em)
		call set_loc(em)
	end if

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .true.)

	case (if_statement)
		call emit_if(em, node, .true.)

	case (switch_statement)
		call emit_switch(em, node, .true.)

	case (use_statement)
		! Its value is nothing
		call emit_stmt(em, node)
		call emit_result_text(em, 'Error: <invalid_value>')

	case (while_statement, for_statement, break_statement, continue_statement)
		call emit_stmt(em, node)
		call emit_result_text(em, 'Error: <invalid_value>')

	case (return_statement)
		call emit_stmt(em, node)

	case (fn_declaration, struct_declaration, enum_declaration)

	case (fn_call_expr, method_call_expr)
		if (node%val%type == void_type) then
			v = emit_expr(em, node)
			call emit_result_text(em, 'Error: <invalid_value>')
		else
			v = emit_expr(em, node)
			call emit_result(em, v)
		end if

	case (assignment_expr)
		if (allocated(node%lsubscripts) .or. allocated(node%member)) then
			! Its value is the target afterward, an element, a slice, or a member
			v = emit_assign_val(em, node)
			call emit_result(em, v)
		else
			call emit_stmt(em, node)
			v = load_var(em, node)
			if (v%ty == enum_type .or. (v%ty == array_type .and. v%elem == enum_type)) then
				if (associated(em%enums)) v%sk = enums_slot_of(em%enums, node%val)
			end if
			call emit_result(em, v)
		end if

	case (let_expr)
		call emit_stmt(em, node)
		if (.not. node%is_empty) then
			if (.not. (allocated(node%lsubscripts) .or. allocated(node%member))) then
				v = load_var(em, node)
				if (v%ty == enum_type .or. (v%ty == array_type .and. v%elem == enum_type)) then
					if (associated(em%enums)) v%sk = enums_slot_of(em%enums, node%val)
				end if
				call emit_result(em, v)
			end if
		end if

	case (fn_call_intr_expr)
		if (node%val%type == void_type) then
			call emit_call_stmt(em, node)
			! println() has an empty result.  exit() never gets to have one
			if (node%identifier%text /= 'exit') call emit_result_text(em, '')
		else
			v = emit_expr(em, node)
			call emit_result(em, v)
		end if

	case default
		if (node%val%type == unknown_type .or. node%val%type == void_type) then
			call emit_stmt(em, node)
			call emit_result_text(em, 'Error: <invalid_value>')
		else
			v = emit_expr(em, node)
			call emit_result(em, v)
		end if

	end select

end subroutine emit_result_stmt

!===============================================================================

subroutine emit_result(em, v)

	! Print the program's result, given its value.  This consumes it

	type(nemitter_t), intent(inout) :: em
	type(nval_t), intent(in) :: v

	type(nval_t) :: s

	if (.not. em%print_result) then
		call dispose(em, v)
		return
	end if

	em%res_ty = v%ty
	if (em%capture) then
		if (v%ty == f32_type) call rt_void(em, 'sy_result_f32', [v%v])
		if (v%ty == f64_type) call rt_void(em, 'sy_result_f64', [v%v])
	end if
	s = str_of(em, v)
	call rt_void(em, 'sy_result', [s%v, ci32(em, merge(1, 0, em%trim_result))])
	call dispose(em, s)

end subroutine emit_result

!===============================================================================

subroutine emit_result_text(em, text)

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: text

	if (.not. em%print_result) return
	call rt_void(em, 'sy_result', [str_literal(em, text), &
		ci32(em, merge(1, 0, em%trim_result))])

end subroutine emit_result_text

!===============================================================================
! Functions

subroutine begin_fn(em, name, ret_ll, ret_ty, is_main, fn, fn_id)

	! Start building a fn.  There's a block for the allocas, which is first, one
	! for the code, and one that every `return` goes to.  `fn` is an LLVM fn that
	! was already declared, or this is `main()`, which is the top-level
	! statements of the program

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr), intent(in) :: ret_ll
	integer, intent(in) :: ret_ty
	logical, intent(in) :: is_main
	type(c_ptr), intent(in), optional :: fn
	integer, intent(in), optional :: fn_id

	!********

	type(c_ptr) :: fty, ignored
	type(c_ptr) :: nothing(1)

	if (present(fn)) then
		em%fn = fn
	else if (is_main) then
		! main(argc, argv)
		fty = LLVMFunctionType(ret_ll, [em%t_i32, em%t_ptr], 2, 0)
		em%fn = LLVMAddFunction(em%llmod, cs(name), fty)
	else
		nothing = c_null_ptr
		fty = LLVMFunctionType(ret_ll, nothing, 0, 0)
		em%fn = LLVMAddFunction(em%llmod, cs(name), fty)
	end if

	em%in_main = is_main
	em%ret_type = ret_ty
	em%depth = 0
	em%ntemps = 0
	em%block_exit = c_null_ptr

	if (allocated(em%l%p)) deallocate(em%l%p, em%l%ty, em%l%own, em%l%elem, em%l%rank, em%l%sk, em%l%fnret)

	em%ablk = LLVMAppendBasicBlockInContext(em%ctx, em%fn, cs('alloca'))
	em%body = LLVMAppendBasicBlockInContext(em%ctx, em%fn, cs('body'))
	em%exit_bb = LLVMAppendBasicBlockInContext(em%ctx, em%fn, cs('exit'))
	call LLVMPositionBuilderAtEnd(em%abld, em%ablk)
	call LLVMPositionBuilderAtEnd(em%bld, em%body)

	! A fn that doesn't return on some path returns a zero or an empty string
	em%retslot = c_null_ptr
	if (.not. is_main .and. ret_ty /= void_type) then
		em%retslot = entry_alloca(em, ll_type(em, ret_ty), 'ret')
		ignored = LLVMBuildStore(em%abld, LLVMConstNull(ll_type(em, ret_ty)), em%retslot)
	end if

	! The call stack, for the trace of a runtime error
	em%last_loc = -1
	em%pushes = present(fn_id)
	if (present(fn_id)) call push_frame(em, fn_id)

end subroutine begin_fn

!===============================================================================

subroutine end_fn(em)

	! Finish the fn.  Whatever is left falls through to the exit block, which
	! frees the strings that the fn's variables own, and returns

	type(nemitter_t), intent(inout) :: em

	integer :: i

	type(c_ptr) :: ignored, old

	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, em%exit_bb)

	call LLVMPositionBuilderAtEnd(em%bld, em%exit_bb)

	if (allocated(em%l%p)) then
		do i = 1, size(em%l%p)
			if (.not. em%l%own(i)) cycle
			old = LLVMBuildLoad2(em%bld, em%t_ptr, em%l%p(i), cs(''))
			call rt_void(em, free_fn(em%l%ty(i)), [old])
		end do
	end if

	do i = 1, em%ntemps
		old = LLVMBuildLoad2(em%bld, em%t_ptr, em%temps(i), cs(''))
		call rt_void(em, merge('sy_str_free', 'sy_arr_free', em%temp_is_str(i)), [old])
	end do

	if (em%pushes) call pop_frame(em)

	if (em%in_main) then
		ignored = LLVMBuildRet(em%bld, ci32(em, 0))
	else if (em%ret_type == void_type) then
		ignored = LLVMBuildRetVoid(em%bld)
	else
		ignored = LLVMBuildRet(em%bld, LLVMBuildLoad2(em%bld, &
			ll_type(em, em%ret_type), em%retslot, cs('')))
	end if

	! The allocas go before the code
	ignored = LLVMBuildBr(em%abld, em%body)

end subroutine end_fn

!===============================================================================

function is_terminated(em) result(t)
	! Does the block that is being built already end in a branch or a return?
	type(nemitter_t), intent(in) :: em
	logical :: t
	t = c_associated(LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(em%bld)))
end function is_terminated

!===============================================================================

subroutine branch_away(em, dest)

	! Jump to a block, for good: `break`, `continue`, and `return`.  What follows
	! in the same block of source can never run, but still needs to go
	! somewhere, so it's built in a block of its own that nothing branches to

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: dest

	type(c_ptr) :: ignored

	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, dest)
	call LLVMPositionBuilderAtEnd(em%bld, new_block(em, 'dead'))

end subroutine branch_away

!===============================================================================

function new_block(em, name) result(bb)
	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: name
	type(c_ptr) :: bb
	bb = LLVMAppendBasicBlockInContext(em%ctx, em%fn, cs(name))
end function new_block

!===============================================================================

subroutine push_loop(em, brk, cnt)

	! Where `break` and `continue` go inside the loop that's about to be built

	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: brk, cnt

	type(c_ptr), allocatable :: tmp(:)

	if (.not. allocated(em%brk)) then
		allocate(em%brk(8), em%cnt(8))
	else if (em%depth >= size(em%brk)) then
		allocate(tmp(2 * size(em%brk)))
		tmp(1: em%depth) = em%brk(1: em%depth)
		call move_alloc(tmp, em%brk)
		allocate(tmp(size(em%brk)))
		tmp(1: em%depth) = em%cnt(1: em%depth)
		call move_alloc(tmp, em%cnt)
	end if

	em%depth = em%depth + 1
	em%brk(em%depth) = brk
	em%cnt(em%depth) = cnt

end subroutine push_loop

!===============================================================================


!===============================================================================

recursive subroutine emit_block(em, node, is_result)

	! The statements of a block.  If is_result, the value of the last one is the
	! program's result.
	!
	! A `break` that is not in a loop jumps to the end of the outermost block, so
	! that one gets a block to jump to

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	integer :: i, n

	logical :: wrap

	type(c_ptr) :: end_bb, ignored

	wrap = em%depth == 0 .and. .not. c_associated(em%block_exit)
	if (wrap) then
		end_bb = new_block(em, 'endblock')
		em%block_exit = end_bb
	end if

	n = 0
	if (allocated(node%members)) n = size(node%members)

	if (is_result .and. n == 0) then
		call emit_result_text(em, 'Error: <invalid_value>')
	else
		do i = 1, n
			if (is_result .and. i == n) then
				call emit_result_stmt(em, node%members(i))
			else
				call emit_stmt(em, node%members(i))
			end if
		end do
	end if

	if (wrap) then
		if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, end_bb)
		call LLVMPositionBuilderAtEnd(em%bld, end_bb)
		em%block_exit = c_null_ptr
	end if

end subroutine emit_block

!===============================================================================
! User fns

function fn_llvm(em, id) result(fn)

	! The LLVM fn of a user fn, which is declared at its first reference.  Only
	! numbers, bools, and strings can be passed or returned, so far.  A
	! by-reference parameter is a pointer to the caller's variable

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: id
	type(c_ptr) :: fn

	!********

	character(len = :), allocatable :: name

	integer :: i, n, n_old, off

	logical :: ok

	type(c_ptr) :: ret
	type(c_ptr), allocatable :: ptys(:), tmp(:)

	fn = c_null_ptr

	if (.not. associated(em%fns)) return
	if (id < 1 .or. id > size(em%fns%fns)) return

	if (.not. allocated(em%fnv)) then
		allocate(em%fnv(max(id, 16)), em%fnt(max(id, 16)))
		em%fnv = c_null_ptr
		em%fnt = c_null_ptr
	else if (id > size(em%fnv)) then
		n_old = size(em%fnv)
		allocate(tmp(max(id, 2 * n_old)))
		tmp = c_null_ptr
		tmp(1: n_old) = em%fnv
		call move_alloc(tmp, em%fnv)
		allocate(tmp(size(em%fnv)))
		tmp = c_null_ptr
		tmp(1: n_old) = em%fnt(1: n_old)
		call move_alloc(tmp, em%fnt)
	end if

	if (c_associated(em%fnv(id))) then
		fn = em%fnv(id)
		return
	end if

	associate (f => em%fns%fns(id))

		! A method has its receiver first, by reference, which is not among the
		! parameters that the fn declares
		off = merge(1, 0, f%is_method)
		n = off
		if (allocated(f%params)) n = n + size(f%params)
		allocate(ptys(max(n, 1)))

		ok = .true.
		do i = 1, n
			if (param_is_ref(em, id, i)) then
				ptys(i) = em%t_ptr
			else
				ptys(i) = ll_type(em, f%params(i - off)%type)
			end if
			if (i > off) then
				if (.not. supported_val(f%params(i - off))) ok = .false.
			end if
		end do

		if (f%type%type == void_type) then
			ret = em%t_void
		else
			ret = ll_type(em, f%type%type)
			if (.not. supported_val(f%type)) ok = .false.
		end if

		if (.not. ok) then
			call unsupported(em, 'a fn with a parameter or return value that is not '// &
				'a number, bool, string, struct, or array of those')
			return
		end if

		name = 'f'//str(id)
		if (allocated(f%node)) name = name//'_'//f%node%identifier%text

		em%fnt(id) = LLVMFunctionType(ret, ptys, n, 0)
		em%fnv(id) = LLVMAddFunction(em%llmod, cs(name), em%fnt(id))
		call LLVMSetLinkage(em%fnv(id), LLVM_INTERNAL_LINKAGE)

	end associate

	fn = em%fnv(id)

end function fn_llvm

!===============================================================================

function param_is_ref(em, id, i) result(r)

	! Is parameter i of a user fn passed by reference, `&` or `&const`?

	type(nemitter_t), intent(in) :: em
	integer, intent(in) :: id, i
	logical :: r

	r = .false.
	if (.not. allocated(em%fns%fns(id)%node)) return
	if (.not. allocated(em%fns%fns(id)%node%is_ref)) return
	if (i > size(em%fns%fns(id)%node%is_ref)) return
	r = em%fns%fns(id)%node%is_ref(i)

end function param_is_ref

!===============================================================================

function param_is_const(em, id, i) result(r)

	! Is parameter i of a user fn declared `&const`?

	type(nemitter_t), intent(in) :: em
	integer, intent(in) :: id, i
	logical :: r

	r = .false.
	if (.not. allocated(em%fns%fns(id)%node)) return
	if (.not. allocated(em%fns%fns(id)%node%is_const_ref)) return
	if (i > size(em%fns%fns(id)%node%is_const_ref)) return
	r = em%fns%fns(id)%node%is_const_ref(i)

end function param_is_const

!===============================================================================

recursive subroutine emit_fn(em, decl, self_sk)

	! Build a user fn.  A parameter passed by value is a stack slot of its own, so
	! the body may assign to it, and a string is copied in so that the fn owns it.
	! A method has `self_sk`, the position of its struct in the table, and its
	! receiver is the first of the parameters of the node, but not of the fn

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl
	integer, intent(in), optional :: self_sk

	!********

	integer :: i, slot, ptype, off

	type(c_ptr) :: fn, pv, ignored, p
	type(nval_t) :: copy

	if (decl%src_id > 0) then
		em%cur_src_id  = decl%src_id
		em%cur_src_pos = decl%src_pos
		em%cur_loc = loc_id(em)
	end if

	fn = fn_llvm(em, decl%id_index)
	if (.not. c_associated(fn)) return

	call fn_name_set(em, decl%id_index, decl%identifier%text)

	associate (f => em%fns%fns(decl%id_index))

		call begin_fn(em, '', em%t_void, f%type%type, .false., fn, decl%id_index)

		off = merge(1, 0, present(self_sk))

		if (allocated(decl%params)) then
			do i = 1, size(decl%params)
				slot  = decl%params(i)
				pv = LLVMGetParam(fn, i - 1)

				call slots_ensure(em%l, slot)
				if (i <= off) then
					ptype = struct_type
					em%l%ty(slot) = ptype
					em%l%sk(slot) = self_sk
				else
					ptype = f%params(i - off)%type
					em%l%ty(slot) = ptype
					call slot_array_info(em, em%l, slot, f%params(i - off))
				end if

				if (param_is_ref(em, decl%id_index, i)) then
					! The caller's variable itself
					em%l%p(slot) = pv
					em%l%own(slot) = .false.
				else
					p = entry_alloca(em, ll_type(em, ptype), 'p'//str(slot))
					em%l%p(slot) = p
					if (ptr_type(ptype)) then
						ignored = LLVMBuildStore(em%abld, LLVMConstNull(em%t_ptr), p)
						em%l%own(slot) = .true.
						copy%ty = ptype
						copy%owned = .false.
						copy%v = pv
						call store_var(em, p, ptype, copy)
					else
						ignored = LLVMBuildStore(em%bld, pv, p)
					end if
				end if
			end do
		end if

		call emit_stmt(em, decl%body)
		call end_fn(em)

	end associate

end subroutine emit_fn

!===============================================================================

subroutine fn_name_set(em, id, name)

	! Remember the name of a fn, for the trace of the calls

	type(nemitter_t), intent(inout) :: em
	integer, intent(in) :: id
	character(len = *), intent(in) :: name

	type(string_t), allocatable :: tmp(:)

	if (.not. allocated(em%fnames)) allocate(em%fnames(max(id, 16)))
	if (id > size(em%fnames)) then
		allocate(tmp(max(id, 2 * size(em%fnames))))
		tmp(1: size(em%fnames)) = em%fnames
		call move_alloc(tmp, em%fnames)
	end if
	em%fnames(id)%s = name

end subroutine fn_name_set

!===============================================================================

recursive function emit_user_call(em, node) result(r)

	! Call a user fn.  A by-value argument is converted to the type of the
	! parameter.  A string is lent to the fn, which copies what it keeps.  A
	! by-reference argument is the address of the variable, and so is an
	! `&const` one, unless it's an expression, which gets a stack slot

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: i, n, id, off, saved_n

	logical :: any_ref, copy_back, ok

	type(c_ptr) :: fn, ignored, recv_slot
	type(c_ptr), allocatable :: args(:), saved(:)

	type(nval_t) :: v, cur
	type(nval_t), allocatable :: pending(:)

	type(place_t) :: pl

	id = node%id_index
	r = dummy_val(em, node%val%type)
	copy_back = .false.
	saved_n = 0

	! The receiver of a method is its first argument
	off = merge(1, 0, node%kind == method_call_expr)

	fn = fn_llvm(em, id)
	if (.not. c_associated(fn)) return

	n = 0
	if (allocated(node%args)) n = size(node%args)
	allocate(args(max(n, 1)), pending(max(n, 1)))
	args = em%t_ptr

	! Whether any argument but the receiver is by reference, which could be the same
	! variable as a by-value argument
	any_ref = .false.
	do i = 1 + off, n
		if (param_is_ref(em, id, i)) any_ref = .true.
	end do

	do i = 1, n
		associate (arg => node%args(i))

			if (param_is_ref(em, id, i)) then
				if (off == 1 .and. i == 1 .and. (arg%root_kind /= 0 .or. &
						arg%kind == fn_call_expr .or. arg%kind == method_call_expr .or. &
						arg%kind == fn_call_ptr_expr)) then
					! The receiver of a method is made by a call, e.g. `f().g()` or
					! `f()[i].g()`.  It is a temporary that the method gets the address of
					v = emit_expr(em, arg)
					args(i) = entry_alloca(em, ll_type(em, v%ty), 'tmp')
					ignored = LLVMBuildStore(em%bld, v%v, args(i))
					pending(i) = v
				else if (arg%kind == name_expr .and. .not. allocated(arg%lsubscripts) &
						.and. .not. allocated(arg%member)) then
					args(i) = var_ptr(em, arg, arg%val%type)
					if (.not. c_associated(args(i))) return
				else if (arg%kind == dot_expr .or. allocated(arg%member) .or. &
						(off == 1 .and. i == 1 .and. allocated(arg%lsubscripts))) then
					! A member of a struct, or an element of an array of them
					copy_back = off == 1 .and. i == 1 .and. .not. em%fns%fns(id)%is_const_method
					if (copy_back) copy_back = chain_scalar_subs(arg) > 0
					if (copy_back) em%idx_mode = 1
					em%idx_n = 0
					ok = place_of(em, arg, pl)
					em%idx_mode = 0
					if (.not. ok) return
					if (pl%has_subs) then
						call unsupported(em, 'a slice of a member as a by-reference argument')
						return
					end if
					if (copy_back) then
						! Like the interpreter, the method gets a copy of a receiver that is
						! an element of an array, which is put back when it returns.  Whatever
						! else the method does to the array, then, is not seen by the method,
						! and the receiver is made again after it, with the subscripts that it
						! had.  A method that makes the receiver go away fails that, instead
						! of writing to memory that was freed.  A receiver that is only
						! members of a variable isn't copied, since the interpreter moves it,
						! and a deep recursion on a list would be cubic if it was copied
						if (em%idx_n > 0) saved = em%idx_slots(1: em%idx_n)
						saved_n = em%idx_n
						cur = place_load(em, pl)
						recv_slot = entry_alloca(em, em%t_ptr, 'recv')
						ignored = LLVMBuildStore(em%bld, rt_call(em, dup_fn(struct_type), &
							em%t_ptr, [cur%v]), recv_slot)
						call place_done(em, pl)
						args(i) = recv_slot
					else
						args(i) = pl%p
					end if
				else if (param_is_const(em, id, i)) then
					v = emit_expr(em, arg)
					args(i) = entry_alloca(em, ll_type(em, v%ty), 'tmp')
					ignored = LLVMBuildStore(em%bld, v%v, args(i))
					pending(i) = v
				else
					call unsupported(em, 'a by-reference argument that is not a plain variable')
					return
				end if

			else
				v = emit_expr(em, arg)
				if (lent_unsafe(node, i, any_ref)) call own_if_borrowed(em, v)
				if (is_numeric_type(v%ty) .and. is_numeric_type( &
						em%fns%fns(id)%params(i - off)%type)) then
					pending(i) = v
					pending(i)%owned = v%owned
					v = convert(em, v, em%fns%fns(id)%params(i - off)%type)
				else
					pending(i) = v
				end if
				args(i) = v%v
			end if

		end associate
	end do

	r%ty = em%fns%fns(id)%type%type
	r%owned = ptr_type(r%ty)
	if (r%ty == array_type) r = arr_val(c_null_ptr, .true., em%fns%fns(id)%type)
	if (r%ty == void_type) then
		ignored = LLVMBuildCall2(em%bld, em%fnt(id), fn, args, n, cs(''))
		r%v = ignored
	else
		r%v = LLVMBuildCall2(em%bld, em%fnt(id), fn, args, n, cs(''))
	end if

	! The fn changed the location that the runtime has
	call set_loc(em, force = .true.)

	if (copy_back) call put_receiver_back()

	do i = 1, n
		call dispose(em, pending(i))
	end do

contains

	subroutine put_receiver_back()

		! The receiver is put back by the return of the method, which is in its
		! frame, at its declaration, if that can fail

		integer :: save_src, save_pos, save_loc

		type(nval_t) :: back

		save_src = em%cur_src_id
		save_pos = em%cur_src_pos
		save_loc = em%cur_loc

		if (bounds_check) then
			call push_frame(em, id)
			if (allocated(em%fns%fns(id)%node)) then
				if (em%fns%fns(id)%node%src_id > 0) then
					em%cur_src_id  = em%fns%fns(id)%node%src_id
					em%cur_src_pos = em%fns%fns(id)%node%src_pos
					em%cur_loc = loc_id(em)
					call set_loc(em, force = .true.)
				end if
			end if
		end if

		if (saved_n > 0) em%idx_slots(1: saved_n) = saved
		em%idx_n = 0
		em%idx_mode = 2
		em%assigning = .true.
		ok = place_of(em, node%args(1), pl)
		em%assigning = .false.
		em%idx_mode = 0

		if (ok) then
			back%ty = struct_type
			back%owned = .true.
			back%v = LLVMBuildLoad2(em%bld, em%t_ptr, recv_slot, cs(''))
			call place_store(em, pl, back)
			call place_done(em, pl)
		end if

		if (bounds_check) call pop_frame(em)
		em%cur_src_id  = save_src
		em%cur_src_pos = save_pos
		em%cur_loc = save_loc
		call set_loc(em, force = .true.)

	end subroutine put_receiver_back

end function emit_user_call

!===============================================================================

recursive function emit_ptr_call(em, node) result(r)

	! Call through a fn pointer, which is a variable or a member of a struct.
	! Every argument is by value, and the parser only accepts one of the type of
	! its parameter, so the signature of the call is made from the arguments and
	! the type of the result.  That is the signature of any fn it can point to.
	! As in emit_user_call(), a heap argument is lent to the callee, which copies
	! what it keeps

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: i, n

	type(c_ptr) :: fty, ret, p
	type(c_ptr), allocatable :: args(:), ptys(:)

	type(nval_t) :: callee, v
	type(nval_t), allocatable :: pending(:)

	r = dummy_val(em, node%val%type)

	if (allocated(node%left)) then
		callee = emit_expr(em, node%left)
	else
		p = var_ptr(em, node, fn_type)
		if (.not. c_associated(p)) return
		callee%ty = fn_type
		callee%v = LLVMBuildLoad2(em%bld, em%t_ptr, p, cs(''))
	end if
	if (callee%ty /= fn_type) then
		call unsupported(em, 'a call of a `'//kind_name(callee%ty)//'`')
		return
	end if

	n = 0
	if (allocated(node%args)) n = size(node%args)
	allocate(args(max(n, 1)), ptys(max(n, 1)), pending(max(n, 1)))
	args = em%t_ptr
	ptys = em%t_ptr

	do i = 1, n
		v = emit_expr(em, node%args(i))
		if (lent_unsafe(node, i, .false.)) call own_if_borrowed(em, v)
		pending(i) = v
		args(i) = v%v
		if (v%ty /= null_type) ptys(i) = ll_type(em, v%ty)
	end do

	if (node%val%type == void_type) then
		ret = em%t_void
	else
		ret = ll_type(em, node%val%type)
	end if
	fty = LLVMFunctionType(ret, ptys, n, 0)

	r%ty = node%val%type
	r%owned = ptr_type(r%ty)
	if (r%ty == array_type) r = arr_val(c_null_ptr, .true., node%val)
	r%v = LLVMBuildCall2(em%bld, fty, callee%v, args, n, cs(''))

	! The fn changed the location that the runtime has
	call set_loc(em, force = .true.)

	do i = 1, n
		call dispose(em, pending(i))
	end do

end function emit_ptr_call

!===============================================================================

recursive subroutine emit_return(em, node)

	! At the top level, returning ends the program and prints the value, like the
	! interpreter does.  In a fn it stores the value, which is a string that the
	! caller will own, and goes to the exit block

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	logical :: is_void

	type(c_ptr) :: ignored

	type(nval_t) :: v

	is_void = node%right%val%type == void_type

	if (em%in_main) then
		if (is_void) then
			call emit_result_text(em, 'Error: <invalid_value>')
		else
			v = emit_expr(em, node%right)
			call emit_result(em, v)
		end if

	else if (.not. is_void) then
		v = emit_expr(em, node%right)

		if (ptr_type(em%ret_type)) then
			if (v%ty /= em%ret_type .and. .not. (v%ty == null_type .and. &
					em%ret_type == struct_type)) then
				call unsupported(em, 'returning a `'//kind_name(v%ty)//'` from a fn of `'// &
					kind_name(em%ret_type)//'`')
			else
				if (.not. v%owned) then
					v%v = rt_call(em, dup_fn(v%ty), em%t_ptr, [v%v])
					v%owned = .true.
				end if
				ignored = LLVMBuildStore(em%bld, v%v, em%retslot)
			end if
		else
			if (is_numeric_type(v%ty) .and. is_numeric_type(em%ret_type)) &
				v = convert(em, v, em%ret_type)
			ignored = LLVMBuildStore(em%bld, v%v, em%retslot)
		end if
	end if

	call branch_away(em, em%exit_bb)

end subroutine emit_return

!===============================================================================
! Control flow

recursive subroutine emit_switch(em, node, is_result)

	! A switch statement is a chain of arms.  An arm whose value (or one of its
	! values) matches the subject and whose guard holds runs its body and then
	! leaves the switch.  If the guard is false, it falls through to the next arm,
	! like the interpreter's.  The `default` arm is last, wherever it is in the
	! source.  The values of an arm are tested in order, and only up to the first
	! that matches, because a value can be a call with side effects.  A range of
	! values is half-open, `lo <= subject < hi`
	!
	! If is_result, the value of the body that ran is the program's result, and a
	! switch that matched nothing has none

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	integer :: i, j, nelems, narms

	type(c_ptr) :: end_bb, arm_bb, match_bb, val_bb, fail_bb, run_bb, chk_bb, ignored

	type(nval_t) :: subj, held, v, t

	subj = emit_expr(em, node%condition)
	if (.not. supported_type(subj%ty) .and. subj%ty /= enum_type .and. &
			.not. (subj%ty == array_type .and. (supported_type(subj%elem) .or. &
			subj%elem == enum_type))) then
		call unsupported(em, 'a `switch` on a value of type `'//kind_name(subj%ty)//'`')
		call dispose(em, subj)
		return
	end if

	! Compared as a loan, and freed at the end
	held = subj
	held%owned = .false.

	end_bb = new_block(em, 'endswitch')

	narms = 0
	if (allocated(node%members)) narms = size(node%members)

	do i = 1, narms

		arm_bb = new_block(em, 'arm')

		nelems = 0
		if (allocated(node%members(i)%elems)) nelems = size(node%members(i)%elems)

		if (nelems > 0) then
			match_bb = new_block(em, 'match')

			do j = 1, nelems
				if (j < nelems) then
					val_bb = new_block(em, 'val')
					fail_bb = val_bb
				else
					val_bb = c_null_ptr
					fail_bb = arm_bb
				end if

				associate (cv => node%members(i)%elems(j))
					if (cv%kind == case_range) then
						! Not below the lower bound, and below the upper.  The upper
						! isn't evaluated for a subject below the lower
						v = emit_expr(em, cv%lbound_)
						t = binop_vals(em, less_token, held, v, '<')
						chk_bb = new_block(em, 'chk')
						ignored = LLVMBuildCondBr(em%bld, t%v, fail_bb, chk_bb)
						call LLVMPositionBuilderAtEnd(em%bld, chk_bb)
						v = emit_expr(em, cv%ubound_)
						t = binop_vals(em, less_token, held, v, '<')
					else
						v = emit_expr(em, cv)
						if (held%ty == array_type .and. v%ty == array_type .and. &
								(held%elem == v%elem .or. (is_int_type(held%elem) .and. &
								is_int_type(v%elem)))) then
							! The same shape and elements
							t%ty = bool_type
							t%owned = .false.
							t%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_arr_eq', &
								em%t_i8, [held%v, v%v]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
							call dispose(em, v)
						else
							t = binop_vals(em, double_equals_token, held, v, '==')
						end if
					end if
				end associate

				if (t%ty /= bool_type) then
					call unsupported(em, 'a `switch` on a value of type `'//kind_name(held%ty)//'`')
					t = dummy_val(em, bool_type)
				end if
				ignored = LLVMBuildCondBr(em%bld, t%v, match_bb, fail_bb)
				if (j < nelems) call LLVMPositionBuilderAtEnd(em%bld, val_bb)
			end do

			call LLVMPositionBuilderAtEnd(em%bld, match_bb)
		end if

		! The guard is only evaluated once a value has matched, or at once for an
		! arm that is nothing but a guard
		if (allocated(node%members(i)%condition)) then
			run_bb = new_block(em, 'run')
			ignored = LLVMBuildCondBr(em%bld, emit_cond(em, node%members(i)%condition), &
				run_bb, arm_bb)
			call LLVMPositionBuilderAtEnd(em%bld, run_bb)
		end if

		call emit_arm_body(node%members(i)%body)
		if (.not. is_terminated(em)) then
			call dispose(em, subj)
			ignored = LLVMBuildBr(em%bld, end_bb)
		end if

		call LLVMPositionBuilderAtEnd(em%bld, arm_bb)

	end do

	if (allocated(node%else_clause)) then
		call emit_arm_body(node%else_clause)
	else if (is_result) then
		call emit_result_text(em, 'Error: <invalid_value>')
	end if
	if (.not. is_terminated(em)) then
		call dispose(em, subj)
		ignored = LLVMBuildBr(em%bld, end_bb)
	end if

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)

contains

	recursive subroutine emit_arm_body(body)
		type(syntax_node_t), intent(in) :: body
		if (is_result) then
			call emit_result_stmt(em, body)
		else
			call emit_stmt(em, body)
		end if
	end subroutine emit_arm_body

end subroutine emit_switch

!===============================================================================

recursive function emit_cond(em, node) result(c)

	! A condition, which is an i1

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(c_ptr) :: c

	type(nval_t) :: v

	v = emit_expr(em, node)
	if (v%ty /= bool_type) then
		call unsupported(em, 'a condition of type `'//kind_name(v%ty)//'`')
		v = dummy_val(em, bool_type)
	end if
	c = v%v

end function emit_cond

!===============================================================================

recursive subroutine emit_if(em, node, is_result)

	! `if`, `else if`, `else`.  If is_result, the value of the last statement of
	! the branch that's taken is the program's result, and there's none if no
	! branch is

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	type(c_ptr) :: then_bb, else_bb, end_bb, ignored

	then_bb = new_block(em, 'then')
	end_bb  = new_block(em, 'endif')
	else_bb = end_bb
	if (allocated(node%else_clause) .or. is_result) else_bb = new_block(em, 'else')

	ignored = LLVMBuildCondBr(em%bld, emit_cond(em, node%condition), then_bb, else_bb)

	call LLVMPositionBuilderAtEnd(em%bld, then_bb)
	call emit_branch(node%if_clause)
	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, end_bb)

	if (allocated(node%else_clause)) then
		call LLVMPositionBuilderAtEnd(em%bld, else_bb)
		call emit_branch(node%else_clause)
		if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, end_bb)
	else if (is_result) then
		call LLVMPositionBuilderAtEnd(em%bld, else_bb)
		call emit_result_text(em, 'Error: <invalid_value>')
		if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, end_bb)
	end if

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)

contains

	recursive subroutine emit_branch(clause)
		type(syntax_node_t), intent(in) :: clause
		if (is_result) then
			call emit_result_stmt(em, clause)
		else
			call emit_stmt(em, clause)
		end if
	end subroutine emit_branch

end subroutine emit_if

!===============================================================================

recursive subroutine emit_while(em, node)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	type(c_ptr) :: cond_bb, body_bb, end_bb, ignored

	cond_bb = new_block(em, 'while')
	body_bb = new_block(em, 'body')
	end_bb  = new_block(em, 'endwhile')

	ignored = LLVMBuildBr(em%bld, cond_bb)
	call LLVMPositionBuilderAtEnd(em%bld, cond_bb)
	call set_loc(em)
	ignored = LLVMBuildCondBr(em%bld, emit_cond(em, node%condition), body_bb, end_bb)

	call LLVMPositionBuilderAtEnd(em%bld, body_bb)
	call push_loop(em, end_bb, cond_bb)
	call emit_stmt(em, node%body)
	em%depth = em%depth - 1
	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, cond_bb)

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)

end subroutine emit_while

!===============================================================================

function is_int_range(node) result(yes)

	! Is an iterable a range of integers, `[a: b]` or `[a: step: b]`?

	type(syntax_node_t), intent(in) :: node
	logical :: yes

	yes = .false.
	if (node%kind /= array_expr) return
	if (.not. allocated(node%val%array)) return
	if (node%val%array%kind /= bound_array .and. node%val%array%kind /= step_array) return
	yes = node%val%array%type == i32_type .or. node%val%array%type == i64_type

end function is_int_range

!===============================================================================

recursive subroutine emit_for_array(em, node)

	! A loop over the elements of an array, in storage order.  The array is
	! evaluated once, into a copy that is only the loop's, so that the body can't
	! change what it visits.  It's freed at the end of the loop, or by the fn when
	! it returns from inside of the loop

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: elem

	logical :: is_str

	type(c_ptr) :: it, p, slot, n, cur, ep, cond_bb, body_bb, latch_bb, end_bb, ignored

	type(nval_t) :: a, v

	is_str = node%array%val%type == str_type
	if (node%array%val%type /= array_type .and. .not. is_str) then
		call unsupported(em, 'a `for` loop over a value of type `'// &
			kind_name(node%array%val%type)//'`')
		return
	end if
	if (is_str) then
		elem = str_type
	else
		if (.not. allocated(node%array%val%array)) return
		elem = node%array%val%array%type
		if (.not. holdable(elem)) then
			call unsupported(em, 'a `for` loop over an array of type `'//kind_name(elem)//'`')
			return
		end if
	end if

	em%for_hdr = node%array%kind == array_expr
	a = emit_expr(em, node%array)
	em%for_hdr = .false.
	if (.not. a%owned) then
		a%v = rt_call(em, dup_fn(a%ty), em%t_ptr, [a%v])
		a%owned = .true.
	end if
	slot = new_temp(em, is_str)
	ignored = LLVMBuildStore(em%bld, a%v, slot)

	n = arr_len(em, a%v)
	it = entry_alloca(em, em%t_i64, 'it')
	ignored = LLVMBuildStore(em%bld, ci64(em, 0_8), it)

	p = var_ptr(em, node, elem)
	if (.not. c_associated(p)) return

	! A struct loop variable is the same struct as the elements are
	if (elem == struct_type .and. associated(em%structs)) then
		if (node%is_loc) then
			em%l%sk(node%id_index) = structs_slot_of(em%structs, node%array%val)
		else
			em%g%sk(node%id_index) = structs_slot_of(em%structs, node%array%val)
		end if
	end if

	cond_bb  = new_block(em, 'for')
	body_bb  = new_block(em, 'body')
	latch_bb = new_block(em, 'latch')
	end_bb   = new_block(em, 'endfor')

	ignored = LLVMBuildBr(em%bld, cond_bb)

	call LLVMPositionBuilderAtEnd(em%bld, cond_bb)
	cur = LLVMBuildLoad2(em%bld, em%t_i64, it, cs(''))
	ignored = LLVMBuildCondBr(em%bld, LLVMBuildICmp(em%bld, LLVM_INT_SLT, cur, n, cs('')), &
		body_bb, end_bb)

	call LLVMPositionBuilderAtEnd(em%bld, body_bb)
	if (is_str) then
		v%ty = str_type
		v%owned = .true.
		v%v = rt_call(em, 'sy_str_at', em%t_ptr, [a%v, cur])
	else
		ep = elem_ptr(em, a%v, elem, cur)
		v = elem_load(em, elem, ep)
	end if
	call store_var(em, p, elem, v)
	call push_loop(em, end_bb, latch_bb)
	call emit_stmt(em, node%body)
	em%depth = em%depth - 1
	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, latch_bb)

	call LLVMPositionBuilderAtEnd(em%bld, latch_bb)
	cur = LLVMBuildLoad2(em%bld, em%t_i64, it, cs(''))
	ignored = LLVMBuildStore(em%bld, LLVMBuildAdd(em%bld, cur, ci64(em, 1_8), cs('')), it)
	ignored = LLVMBuildBr(em%bld, cond_bb)

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)
	call rt_void(em, merge('sy_str_free', 'sy_arr_free', is_str), &
		[LLVMBuildLoad2(em%bld, em%t_ptr, slot, cs(''))])
	ignored = LLVMBuildStore(em%bld, LLVMConstNull(em%t_ptr), slot)

end subroutine emit_for_array

!===============================================================================

function new_temp(em, is_str) result(slot)

	! A stack slot for an array that the fn must free when it returns, if it
	! hasn't been before.  It's null until it's set

	type(nemitter_t), intent(inout) :: em
	logical, intent(in) :: is_str
	type(c_ptr) :: slot

	type(c_ptr) :: ignored
	type(c_ptr), allocatable :: tmp(:)
	logical, allocatable :: ltmp(:)

	slot = entry_alloca(em, em%t_ptr, 'tmp')
	ignored = LLVMBuildStore(em%abld, LLVMConstNull(em%t_ptr), slot)

	if (.not. allocated(em%temps)) then
		allocate(em%temps(8), em%temp_is_str(8))
	end if
	if (em%ntemps >= size(em%temps)) then
		allocate(tmp(2 * size(em%temps)), ltmp(2 * size(em%temps)))
		tmp(1: em%ntemps) = em%temps(1: em%ntemps)
		ltmp(1: em%ntemps) = em%temp_is_str(1: em%ntemps)
		call move_alloc(tmp, em%temps)
		call move_alloc(ltmp, em%temp_is_str)
	end if
	em%ntemps = em%ntemps + 1
	em%temps(em%ntemps) = slot
	em%temp_is_str(em%ntemps) = is_str

end function new_temp

!===============================================================================

recursive subroutine emit_for(em, node)

	! A loop over an integer range, `[a: b]` or `[a: step: b]`, which doesn't
	! include `b`, in either direction.  syntran evaluates the range once, and the
	! body may assign to the loop variable without affecting the iteration, so a
	! hidden counter drives the loop.  It's an i64, so that stepping past the
	! end of an i32 range can't wrap

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: elem

	logical :: stepped

	type(c_ptr) :: it, p, cond_bb, body_bb, latch_bb, end_bb, ignored
	type(c_ptr) :: cur, ahead, behind, zero, nxt, wrapped, step_bb
	type(nval_t) :: lb, ub, st, iv

	! Any other iterable is an array that is made first
	if (.not. is_int_range(node%array)) then
		call emit_for_array(em, node)
		return
	end if
	elem = node%array%val%array%type

	stepped = node%array%val%array%kind == step_array

	lb = convert(em, emit_expr(em, node%array%lbound_), i64_type)
	ub = convert(em, emit_expr(em, node%array%ubound_), i64_type)
	if (stepped) then
		st = convert(em, emit_expr(em, node%array%step), i64_type)
		call step_check(em, st, 'for loop', 'R23', 'R24')
	else
		st%v = ci64(em, 1_8)
	end if

	it = entry_alloca(em, em%t_i64, 'it')
	ignored = LLVMBuildStore(em%bld, lb%v, it)

	p = var_ptr(em, node, elem)
	if (.not. c_associated(p)) return

	cond_bb  = new_block(em, 'for')
	body_bb  = new_block(em, 'body')
	latch_bb = new_block(em, 'latch')
	end_bb   = new_block(em, 'endfor')

	ignored = LLVMBuildBr(em%bld, cond_bb)

	! The step's sign is the direction of the loop
	call LLVMPositionBuilderAtEnd(em%bld, cond_bb)
	cur = LLVMBuildLoad2(em%bld, em%t_i64, it, cs(''))
	behind = LLVMBuildICmp(em%bld, LLVM_INT_SLT, cur, ub%v, cs(''))
	if (stepped) then
		zero = ci64(em, 0_8)
		ahead = LLVMBuildICmp(em%bld, LLVM_INT_SGT, cur, ub%v, cs(''))
		behind = LLVMBuildSelect(em%bld, &
			LLVMBuildICmp(em%bld, LLVM_INT_SGT, st%v, zero, cs('')), behind, ahead, cs(''))
	end if
	ignored = LLVMBuildCondBr(em%bld, behind, body_bb, end_bb)

	call LLVMPositionBuilderAtEnd(em%bld, body_bb)
	iv%ty = i64_type
	iv%owned = .false.
	iv%v = cur
	call store_var(em, p, elem, iv)
	call push_loop(em, end_bb, latch_bb)
	call emit_stmt(em, node%body)
	em%depth = em%depth - 1
	if (.not. is_terminated(em)) ignored = LLVMBuildBr(em%bld, latch_bb)

	call LLVMPositionBuilderAtEnd(em%bld, latch_bb)
	cur = LLVMBuildLoad2(em%bld, em%t_i64, it, cs(''))
	nxt = LLVMBuildAdd(em%bld, cur, st%v, cs(''))
	if (stepped) then
		! If cur + step wraps around the end of an i64, the next value is past
		! the bound, so the loop is done.  It would otherwise start over at the
		! other end of the i64s, like `[0: 0x4000000000000000: 0x7fffffffffffffff]`.
		! The unstepped loop can't wrap, since it only runs while cur < ub
		step_bb = new_block(em, 'step')
		wrapped = LLVMBuildSelect(em%bld, &
			LLVMBuildICmp(em%bld, LLVM_INT_SGT, st%v, zero, cs('')), &
			LLVMBuildICmp(em%bld, LLVM_INT_SLT, nxt, cur, cs('')), &
			LLVMBuildICmp(em%bld, LLVM_INT_SGT, nxt, cur, cs('')), cs(''))
		ignored = LLVMBuildCondBr(em%bld, wrapped, end_bb, step_bb)
		call LLVMPositionBuilderAtEnd(em%bld, step_bb)
	end if
	ignored = LLVMBuildStore(em%bld, nxt, it)
	ignored = LLVMBuildBr(em%bld, cond_bb)

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)

end subroutine emit_for


!===============================================================================
! Output

subroutine finish(em, n, diags)

	! Check the module, optimize it, and write what `n%out` asks for, or run it in
	! memory if `n%jit` or `n%capture`

	type(nemitter_t), intent(inout) :: em
	type(native_t), intent(inout) :: n
	type(string_vector_t), intent(inout) :: diags

	!********

	character(len = :), allocatable :: out, obj, crt, cc, cmd, flags, libs

	integer :: i, ou, ios, es, cmd_stat, ln

	type(c_ptr) :: msg, err, opts

	logical :: is_ll, is_obj

	if (.not. (n%jit .or. n%capture)) then
		if (.not. allocated(n%out)) then
			call diags%push(err_prefix//'no output file for the native backend'//color_reset)
			return
		end if
		out = n%out
	end if

	if (LLVMVerifyModule(em%llmod, LLVM_RETURN_STATUS_ACTION, msg) /= 0) then
		call diags%push(err_prefix//'internal error, LLVM rejected the module: '// &
			c_to_f_string(msg)//color_reset)
		call LLVMDisposeMessage(msg)
		return
	end if

	opts = LLVMCreatePassBuilderOptions()
	err = LLVMRunPasses(em%llmod, cs('default<O2>'), em%tm, opts)
	call LLVMDisposePassBuilderOptions(opts)
	if (c_associated(err)) then
		call diags%push(err_prefix//'internal error, LLVM failed to optimize: '// &
			c_to_f_string(LLVMGetErrorMessage(err))//color_reset)
		return
	end if

	if (n%jit .or. n%capture) then
		call jit_run(em, n, diags)
		return
	end if

	! cmd.exe has no way to quote a double quote, c.f. sh_quote()
	if (is_windows() .and. index(out, '"') > 0) then
		call diags%push(err_prefix//'the output file name `'//out// &
			'` cannot have a double quote'//color_reset)
		return
	end if

	ln = len(out)
	is_ll  = ln > 3 .and. out(max(ln-2,1):) == '.ll'
	is_obj = ln > 2 .and. out(max(ln-1,1):) == '.o'

	if (is_ll) then
		if (LLVMPrintModuleToFile(em%llmod, cs(out), msg) /= 0) then
			call diags%push(err_prefix//'cannot write `'//out//'`: '// &
				c_to_f_string(msg)//color_reset)
			call LLVMDisposeMessage(msg)
		end if
		return
	end if

	obj = out
	if (.not. is_obj) obj = out//'.tmp.o'

	if (LLVMTargetMachineEmitToFile(em%tm, em%llmod, cs(obj), LLVM_OBJECT_FILE, msg) /= 0) then
		call diags%push(err_prefix//'cannot write `'//obj//'`: '// &
			c_to_f_string(msg)//color_reset)
		call LLVMDisposeMessage(msg)
		return
	end if
	if (is_obj) return

	! Link the object with the runtime, which is written next to it as C source
	crt = out//'.tmp.c'
	open(newunit = ou, file = crt, action = 'write', status = 'replace', iostat = ios)
	if (ios /= 0) then
		call diags%push(err_prefix//'cannot write `'//crt//'`'//color_reset)
		call delete_file(obj)
		return
	end if
	block
		type(string_vector_t) :: rt
		rt = native_rt_src()
		do i = 1, rt%len_
			write(ou, '(a)') rt%v(i)%s
		end do
	end block
	close(ou)

	! mingw has `gcc`, but no `cc`
	cc = 'cc'
	if (is_windows()) cc = 'gcc'
	call get_environment_variable('CC', length = ln)
	if (ln > 0) then
		deallocate(cc)
		allocate(character(len = ln) :: cc)
		call get_environment_variable('CC', cc)
	end if

	! The same floating point as the interpreter, so no fused multiply-adds
	flags = '-O2 -ffp-contract=off'

	! The linear algebra fns use BLAS and LAPACK if syntran does, so that the
	! results are the same as the interpreter's.  Nothing else links them
	libs = '-lm'
	if (em%uses_blas .and. blas_enabled) then
		flags = flags//' -DSY_BLAS'
		libs = '-llapack -lblas -lm'
	end if
	cmd = cc//' '//flags//' '//sh_quote(obj)//' '//sh_quote(crt)//' '//libs//' -o '// &
		sh_quote(out)

	call execute_command_line(cmd, wait = .true., exitstat = es, cmdstat = cmd_stat)
	if (cmd_stat /= 0 .or. es /= 0) then
		call diags%push(err_prefix//'linking failed: `'//cmd//'`'//color_reset)
		if (em%uses_blas .and. blas_enabled) then
			! Not a fallback to the runtime's own code, which would be slow without a word
			call diags%push(fg_bright_green//'help'//color_reset//': this program uses linear '// &
				'algebra (`@`, `std::solve()`, etc.), which links BLAS and LAPACK '// &
				'(`-llapack -lblas`), so they must be installed where it is compiled, e.g. '// &
				'`sudo apt-get install libopenblas-dev`')
		end if
	end if

	call delete_file(obj)
	call delete_file(crt)

end subroutine finish

!===============================================================================

subroutine jit_run(em, n, diags)

	! Run the module that is built, here, and keep what it did in `n`.  The JIT
	! takes the module and its context, so the emitter must not free them

	use, intrinsic :: iso_fortran_env, only: output_unit

	type(nemitter_t), intent(inout) :: em
	type(native_t), intent(inout) :: n
	type(string_vector_t), intent(inout) :: diags

	!********

	character(len = :), allocatable :: argbuf

	integer :: i, status, rc

	type(c_ptr) :: m, c

	! Nothing is built after this, and a builder has to go before its context
	call LLVMDisposeBuilder(em%abld)
	call LLVMDisposeBuilder(em%bld)
	em%abld = c_null_ptr
	em%bld = c_null_ptr

	m = em%llmod
	c = em%ctx
	em%llmod = c_null_ptr
	em%ctx = c_null_ptr

	n%status = 0
	n%exit_code = 0
	n%result = ''
	n%message = ''

	! Every run sets it, so that one never sees the directory of an earlier one
	call sy_set_src_dir(cs(em%src_dir))

	if (.not. n%capture) then
		! Run it like an executable: the runtime prints and exits for itself, and
		! main() only returns if the program ran to the end.  Anything of ours that
		! is buffered goes out first, so that it comes before the program's output
		flush(output_unit)

		argbuf = ''
		do i = 1, em%script_args%len_
			argbuf = argbuf//em%script_args%v(i)%s//c_null_char
		end do
		argbuf = argbuf//c_null_char

		status = sy_llvm_jit_exec(m, c, int(em%script_args%len_, c_int), cs(argbuf), rc)
		if (status < 0) then
			call diags%push(err_prefix//'internal error, LLVM failed to run the program: '// &
				c_to_f_string(sy_llvm_jit_error())//color_reset)
			return
		end if
		n%exit_code = rc
		return
	end if

	status = sy_llvm_jit_run(m, c)
	if (status < 0) then
		call diags%push(err_prefix//'internal error, LLVM failed to run the program: '// &
			c_to_f_string(sy_llvm_jit_error())//color_reset)
		return
	end if

	n%status = status
	n%exit_code = sy_jit_exit_code()
	n%result_type = em%res_ty
	n%result_f32 = sy_jit_value_f32()
	n%result_f64 = sy_jit_value_f64()
	n%result = c_to_f_string(sy_jit_result())
	n%message = c_to_f_string(sy_jit_message())

end subroutine jit_run

!===============================================================================

function is_windows() result(r)

	! gfortran doesn't define _WIN32 for the preprocessor, so ask the environment
	! instead.  Windows sets OS for every process, including under Git Bash

	logical :: r

	character(len = 16) :: os

	call get_environment_variable('OS', os)
	r = trim(os) == 'Windows_NT'

end function is_windows

!===============================================================================

function sh_quote(s) result(r)

	! `execute_command_line()` runs the command in cmd.exe on Windows, which
	! doesn't treat single quotes as quotes.  A path can't have a double quote
	! there, and finish() rejects one.  In a POSIX shell a single quote ends the
	! quoted string, so it's closed, escaped, and opened again: 'it'\''s'

	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r

	if (is_windows()) then
		r = '"'//s//'"'
	else
		r = "'"//replace_all(s, "'", "'\''")//"'"
	end if

end function sh_quote

!===============================================================================

subroutine delete_file(filename)

	character(len = *), intent(in) :: filename

	integer :: u, ios

	open(newunit = u, file = filename, status = 'old', iostat = ios)
	if (ios == 0) close(u, status = 'delete')

end subroutine delete_file

#endif

!===============================================================================

end module syntran__native_m

!===============================================================================
