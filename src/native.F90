
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
	use syntran__consts_m
	use syntran__errors_m
	use syntran__llvm_c_m
	use syntran__native_rt_m
	use syntran__runtime_m
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

	end type slots_t

	!********

	type place_t

		! Where a variable, an element of an array, or a member of a struct is
		! stored: the address of the storage, its type, and what it holds if
		! it's an array or a struct

		type(c_ptr) :: p = c_null_ptr
		integer :: ty = unknown_type, elem = unknown_type, rank = 0, sk = 0

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

		logical :: print_result = .true., trim_result = .false.

		! Counter for the names of string literals
		integer :: nstr = 0

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
	em%trim_result  = n%trim_result
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
	case (str_type, array_type, struct_type, fn_type)
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
	p = ty == str_type .or. ty == array_type .or. ty == struct_type
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
	case default
		name = 'sy_arr_dup'
	end select
end function dup_fn

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
	case (array_type, struct_type, null_type, fn_type)
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
			r%v = LLVMBuildFPToSI(em%bld, v%v, em%t_i32, cs(''))
		end select

	case (i64_type)
		select case (v%ty)
		case (i32_type)
			r%v = LLVMBuildSExt(em%bld, v%v, em%t_i64, cs(''))
		case (f32_type, f64_type)
			r%v = LLVMBuildFPToSI(em%bld, v%v, em%t_i64, cs(''))
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

	ok = supported_type(ty) .or. ty == struct_type .or. ty == enum_type .or. ty == fn_type

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
		ok = holdable(val%array%type)
	end if

end function supported_val

!===============================================================================

subroutine slots_ensure(s, id)

	! Make room for slot `id`

	type(slots_t), intent(inout) :: s
	integer, intent(in) :: id

	integer :: n
	integer, allocatable :: itmp(:)
	logical, allocatable :: ltmp(:)
	type(c_ptr), allocatable :: ptmp(:)

	if (.not. allocated(s%p)) then
		n = max(id, 16)
		allocate(s%p(n), s%ty(n), s%own(n), s%elem(n), s%rank(n), s%sk(n))
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
		end if
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

	case (binary_expr)
		r = emit_binary(em, node)

	case (unary_expr)
		r = emit_unary(em, node)

	case (fn_call_intr_expr)
		r = emit_intr_call(em, node)

	case (fn_call_expr, method_call_expr)
		r = emit_user_call(em, node)

	case (fn_ref_expr)
		! The address of a user fn, which is declared here if this is its first
		! reference
		r%ty = fn_type
		r%owned = .false.
		r%v = fn_llvm(em, node%id_index)
		if (.not. c_associated(r%v)) r = dummy_val(em, fn_type)

	case (fn_call_ptr_expr)
		r = emit_ptr_call(em, node)

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

end function arr_data

!===============================================================================

function arr_len(em, a) result(n)
	! The total number of elements of an array, an i64, which is its first field
	type(nemitter_t), intent(inout) :: em
	type(c_ptr), intent(in) :: a
	type(c_ptr) :: n
	n = LLVMBuildLoad2(em%bld, em%t_i64, a, cs(''))
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

	v = emit_expr(em, sub)
	if (v%ty /= i32_type .and. v%ty /= i64_type) then
		call unsupported(em, 'a subscript of type `'//kind_name(v%ty)//'`')
		v = dummy_val(em, i64_type)
	end if
	v = convert(em, v, i64_type)
	i = v%v

end function scalar_index

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

	integer :: i, vty, ety, rank_, nsub

	type(c_ptr) :: ps, a, p, subs, last
	type(c_ptr), allocatable :: idx(:)

	type(nval_t) :: el
	type(nval_t), allocatable :: pending(:)

	r = dummy_val(em, node%val%type)
	if (.not. ref_info(em, node, ps, vty, ety, rank_)) return

	nsub = size(node%lsubscripts)
	a = LLVMBuildLoad2(em%bld, em%t_ptr, ps, cs(''))

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
		p = elem_target(em, node, ety)
		if (c_associated(p)) r = elem_load(em, ety, p)

	else if (nsub == rank_) then
		subs = build_subs(em, node, pending)
		r = arr_val(rt_call(em, 'sy_arr_slice', em%t_ptr, [a, subs]), .true., node%val)
		call dispose_all(em, pending)

	else if (nsub == rank_ + 1 .and. ety == str_type) then
		! A character, or substring, of an element of an array of strings
		do i = 1, rank_
			if (node%lsubscripts(i)%sub_kind /= scalar_sub) then
				call unsupported(em, 'a substring of a slice of strings')
				return
			end if
		end do
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

end function emit_subscript

!===============================================================================

recursive function emit_array_expr(em, node) result(r)

	! An array literal, one of the kinds that an array_expr has

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	character(len = :), allocatable :: sfx

	integer :: t, kind_, i, n

	type(c_ptr) :: sizes, valp, ignored, total, off, a, d, p
	type(c_ptr), allocatable :: dims(:)

	type(nval_t) :: lb, ub, st, v
	type(nval_t), allocatable :: vals(:)

	r = arr_val(LLVMConstNull(em%t_ptr), .false., node%val)

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
		v = emit_expr(em, node%lbound_)
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
		if (.not. supported_val(mval)) then
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

recursive function emit_struct_instance(em, node) result(r)

	! `Point{x = 1, y = 2}` is a new struct that owns its members.  They are in
	! the order of the members' indices, which is the order of their slots

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
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
			v = emit_expr(em, node%members(i))
			call elem_store(em, LLVMBuildGEP2(em%bld, em%t_i64, s, [ci64(em, int(i, 8))], 1, &
				cs('')), mval%type, v)
		end do
	end if

	r%v = s
	r%owned = .true.

end function emit_struct_instance

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

	by_value = .not. (op == eequals_token .or. op == bang_equals_token)
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
	case (eequals_token)
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
		ci32(em, em%enums%table(k)%val%num_vars), v%v])
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
	call rt_void(em, 'sy_fatal', [c_string(em, 'member of a null struct')])
	ignored = LLVMBuildBr(em%bld, good)

	call LLVMPositionBuilderAtEnd(em%bld, good)

end subroutine check_not_null

!===============================================================================

recursive function place_subscript(em, pl, subs) result(ok)

	! The element of the array that a place holds, if there is one scalar subscript
	! per dimension.  Otherwise this has said that it isn't supported

	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(inout) :: pl
	type(syntax_node_t), intent(in) :: subs(:)
	logical :: ok

	integer :: i

	type(c_ptr) :: a
	type(c_ptr), allocatable :: idx(:)

	ok = .false.

	if (pl%ty == str_type) then
		call unsupported(em, 'a substring of a member')
		return
	else if (pl%ty /= array_type) then
		call unsupported(em, 'a subscript of a value that is not an array')
		return
	end if

	if (size(subs) /= pl%rank) then
		call unsupported(em, 'a subscript with the wrong number of dimensions')
		return
	end if
	do i = 1, size(subs)
		if (subs(i)%sub_kind /= scalar_sub) then
			call unsupported(em, 'a slice of a member')
			return
		end if
	end do

	a = LLVMBuildLoad2(em%bld, em%t_ptr, pl%p, cs(''))
	allocate(idx(pl%rank))
	do i = 1, pl%rank
		idx(i) = scalar_index(em, subs(i))
	end do

	pl%p = elem_ptr(em, a, pl%elem, lin_index(em, a, idx))
	pl%ty = pl%elem
	pl%elem = unknown_type
	pl%rank = 0
	ok = .true.

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
		if (.not. place_subscript(em, pl, m%lsubscripts)) return
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
		if (.not. place_subscript(em, pl, node%lsubscripts)) return
	end if

	if (allocated(node%member)) then
		ok = place_member(em, node%member, pl)
		return
	end if

	ok = .true.

end function place_of

!===============================================================================

function place_load(em, pl) result(r)

	! Read what a place holds.  A str, array, or struct is only lent

	type(nemitter_t), intent(inout) :: em
	type(place_t), intent(in) :: pl
	type(nval_t) :: r

	r = elem_load(em, pl%ty, pl%p)
	if (pl%ty == array_type) then
		r%elem = pl%elem
		r%rank = pl%rank
	end if

end function place_load

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

	if (node%root_kind == 0) then
		if (.not. place_of(em, node, pl)) return
		r = place_load(em, pl)
		return
	end if

	if (node%root_kind /= fn_call_expr .and. node%root_kind /= method_call_expr) then
		call unsupported(em, 'a member of the result of a `'//kind_name(node%root_kind)//'`')
		return
	end if
	if (allocated(node%lsubscripts)) then
		call unsupported(em, 'a subscripted fn call')
		return
	end if

	! The call makes a struct that is only the member's until it is copied out
	root = node
	root%kind = node%root_kind
	root%root_kind = 0
	deallocate(root%member)

	root_v = emit_expr(em, root)
	if (root_v%ty /= struct_type .or. .not. associated(em%structs)) then
		call unsupported(em, 'a member of a fn result of type `'//kind_name(root_v%ty)//'`')
		call dispose(em, root_v)
		return
	end if

	pl%sk = structs_slot_of(em%structs, em%fns%fns(node%id_index)%type)
	pl%ty = struct_type
	slot = entry_alloca(em, em%t_ptr, 'tmp')
	ignored = LLVMBuildStore(em%bld, root_v%v, slot)
	pl%p = slot

	if (.not. place_member(em, node%member, pl)) then
		call dispose(em, root_v)
		return
	end if

	r = place_load(em, pl)
	if (ptr_type(r%ty)) then
		r%v = rt_call(em, dup_fn(r%ty), em%t_ptr, [r%v])
		r%owned = .true.
	end if
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

	type(nval_t) :: a, b

	res%owned = .false.

	! Strings are joined.  Comparing them is not supported yet
	if (l%ty == str_type .and. r%ty == str_type) then
		select case (op)
		case (plus_token)
			res%ty = str_type
			res%owned = .true.
			res%v = rt_call(em, 'sy_str_cat', em%t_ptr, [l%v, r%v])
		case (eequals_token, bang_equals_token)
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
		case (eequals_token)
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
		case (eequals_token)
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
		case (eequals_token)
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
	! as many bits as it has or more is 0, which LLVM leaves undefined
	if (op == lless_token .or. op == ggreater_token) then
		if (l%ty /= i32_type .and. l%ty /= i64_type) then
			call unsupported(em, 'a shift of type `'//kind_name(l%ty)//'`')
			res = dummy_val(em, l%ty)
			return
		end if
		bits = merge(32, 64, l%ty == i32_type)
		a = l
		b = convert(em, r, l%ty)
		res%ty = l%ty
		if (op == lless_token) then
			res%v = LLVMBuildShl(em%bld, a%v, b%v, cs(''))
		else
			res%v = LLVMBuildLShr(em%bld, a%v, b%v, cs(''))
		end if
		res%v = LLVMBuildSelect(em%bld, &
			LLVMBuildICmp(em%bld, LLVM_INT_ULT, b%v, &
				LLVMConstInt(ll_type(em, l%ty), int(bits, c_int64_t), 0), cs('')), &
			res%v, LLVMConstNull(ll_type(em, l%ty)), cs(''))
		return
	end if

	! A power of a float and an i32 is done by repeated multiplication, like
	! Fortran does.  Everything else converts to the wider type first
	if (op == sstar_token .and. (l%ty == f32_type .or. l%ty == f64_type) &
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

	case (sstar_token)
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

	case (eequals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)
		res%ty = bool_type
		if (is_int) then
			select case (op)
			case (eequals_token)
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
			case (eequals_token)
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

	type(c_ptr) :: out, p, q

	type(loop_t) :: lp

	type(nval_t) :: x, el, y

	if (is_arr(node%right%val)) then
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
		call loop_begin(em, arr_len(em, x%v), lp, q)
		el = elem_load(em, x%elem, elem_ptr(em, x%v, x%elem, q))
		y = unary_val(em, node%op%kind, node%op%text, el)
		call elem_store(em, elem_ptr(em, out, ety, q), ety, y)
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
		call unsupported(em, 'a matrix product')
		return
	end if
	if (.not. allocated(node%val%array)) then
		call unsupported(em, 'an operation on an array')
		return
	end if

	l  = emit_expr(em, node%left)
	rr = emit_expr(em, node%right)
	r  = arr_binop_vals(em, node%op%kind, node%op%text, l, rr, node%val)

end function emit_array_binary

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

	type(c_ptr) :: out, q, shape_src

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

	out = rt_call(em, 'sy_arr_like', em%t_ptr, [shape_src, ci32(em, elem_code(ety))])

	call loop_begin(em, arr_len(em, shape_src), lp, q)
	if (l%ty == array_type) then
		x = elem_load(em, l%elem, elem_ptr(em, l%v, l%elem, q))
	else
		x = l
		x%owned = .false.
	end if
	if (rr%ty == array_type) then
		y = elem_load(em, rr%elem, elem_ptr(em, rr%v, rr%elem, q))
	else
		y = rr
		y%owned = .false.
	end if
	z = binop_vals(em, op, x, y, op_text)
	call elem_store(em, elem_ptr(em, out, ety, q), ety, z)
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

	type(c_ptr) :: acc, q, ignored, init

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
		! What Fortran gives for an empty array is the extreme of the type
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

	call loop_begin(em, arr_len(em, a%v), lp, q)
	x = elem_load(em, ety, elem_ptr(em, a%v, ety, q))

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

recursive function emit_intr_call(em, node) result(r)

	! Call of an intrinsic fn which returns a value

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	character(len = :), allocatable :: name, base

	integer :: i, n, iu

	type(c_ptr) :: zero
	type(c_ptr), allocatable :: dims(:)

	type(nval_t) :: piece, acc, a, b

	name = node%identifier%text
	base = name
	if (len(name) > 1) then
		if (name(1:1) == '0') then
			! An overload, which the parser named like `0abs_i32`
			base = name(2:)
			iu = index(base, '_')
			if (iu > 0) base = base(1: iu-1)
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
			piece = str_of(em, emit_expr(em, node%args(i)))
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

	case ('len')
		! The length is the first field of a string
		a = emit_expr(em, node%args(1))
		r%ty = i64_type
		r%owned = .false.
		r%v = LLVMBuildLoad2(em%bld, em%t_i64, a%v, cs(''))
		call dispose(em, a)
		r = convert(em, r, node%val%type)

	case ('size')
		! The number of elements, or the extent along a dimension
		a = emit_expr(em, node%args(1))
		r%ty = i64_type
		r%owned = .false.
		if (a%ty /= array_type) then
			call unsupported(em, 'the intrinsic fn `size` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, i64_type)
		else if (n == 1) then
			r%v = arr_len(em, a%v)
		else
			b = convert(em, emit_expr(em, node%args(2)), i64_type)
			r%v = arr_dim(em, a%v, b%v)
		end if
		call dispose(em, a)
		r = convert(em, r, node%val%type)

	case ('sum', 'product', 'minval', 'maxval', 'all', 'any', 'count')
		! Of a whole array.  A dimension or a mask is not supported yet
		if (n /= 1) then
			call unsupported(em, 'the intrinsic fn `'//base//'` with a dimension or mask', &
				node%identifier%pos)
			r = dummy_val(em, node%val%type)
		else
			a = emit_expr(em, node%args(1))
			if (a%ty /= array_type) then
				call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
					kind_name(a%ty)//'`', node%identifier%pos)
				r = dummy_val(em, node%val%type)
			else
				r = emit_reduce(em, base, a, node%val%type)
			end if
		end if

	case ('parse_i32', 'parse_i64', 'parse_f32', 'parse_f64', 'parse_bool')
		a = emit_expr(em, node%args(1))
		r%owned = .false.
		select case (base)
		case ('parse_i32')
			r%ty = i32_type
			r%v = rt_call(em, 'sy_parse_i32', em%t_i32, [a%v])
		case ('parse_i64')
			r%ty = i64_type
			r%v = rt_call(em, 'sy_parse_i64', em%t_i64, [a%v])
		case ('parse_f32')
			r%ty = f32_type
			r%v = rt_call(em, 'sy_parse_f32', em%t_f32, [a%v])
		case ('parse_f64')
			r%ty = f64_type
			r%v = rt_call(em, 'sy_parse_f64', em%t_f64, [a%v])
		case default
			r%ty = bool_type
			r%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_parse_bool', em%t_i8, &
				[a%v]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
		end select
		call dispose(em, a)

	case ('reshape')
		! The same elements in the shape that an array of extents says.  The rank
		! of the result is known to the parser
		a = emit_expr(em, node%args(1))
		b = emit_expr(em, node%args(2))
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
			r = arr_val(rt_call(em, 'sy_arr_reshape', em%t_ptr, [a%v, ci32(em, iu), &
				store_sizes(em, dims(1: iu))]), .true., node%val)
		end if
		call dispose(em, b)

	case ('repeat')
		a = emit_expr(em, node%args(1))
		b = convert(em, emit_expr(em, node%args(2)), i64_type)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_repeat', em%t_ptr, [a%v, b%v])
		call dispose(em, a)

	case ('char')
		a = convert(em, emit_expr(em, node%args(1)), i32_type)
		r%ty = str_type
		r%owned = .true.
		r%v = rt_call(em, 'sy_str_char', em%t_ptr, [a%v])

	case ('i32', 'i64', 'f32', 'f64', 'exp', 'log', 'log10', 'sqrt', 'cos', 'sin', 'tan', &
			'acos', 'asin', 'atan', 'log2', 'abs')
		! Of a number, or of each element of an array
		a = emit_expr(em, node%args(1))
		if (a%ty == array_type) then
			r = emit_intr_map(em, base, a, node)
		else
			r = intr_scalar(em, base, a, node%val%type, node%identifier%pos)
		end if

	case ('min', 'max')
		! Of any number of arguments, as a chain.  A NaN loses to a number, like
		! it does in Fortran
		acc = emit_expr(em, node%args(1))
		do i = 2, n
			b = emit_expr(em, node%args(i))
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
			r%v = rt_call(em, 'sy_str_code', em%t_i32, [a%v])
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

recursive function emit_intr_map(em, base, a, node) result(r)

	! A scalar fn of each element of an array.  This consumes `a`

	type(nemitter_t), intent(inout) :: em
	character(len = *), intent(in) :: base
	type(nval_t), intent(in) :: a
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: ety

	type(c_ptr) :: out, q

	type(loop_t) :: lp

	type(nval_t) :: x, y

	r = dummy_val(em, array_type)
	if (.not. allocated(node%val%array)) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of an array', node%identifier%pos)
		return
	end if

	ety = node%val%array%type
	if (.not. supported_type(ety) .or. a%elem == str_type) then
		call unsupported(em, 'the intrinsic fn `'//base//'` of an array of type `'// &
			kind_name(a%elem)//'`', node%identifier%pos)
		return
	end if

	out = rt_call(em, 'sy_arr_like', em%t_ptr, [a%v, ci32(em, elem_code(ety))])
	call loop_begin(em, arr_len(em, a%v), lp, q)
	x = elem_load(em, a%elem, elem_ptr(em, a%v, a%elem, q))
	y = intr_scalar(em, base, x, ety, node%identifier%pos)
	call elem_store(em, elem_ptr(em, out, ety, q), ety, y)
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

	type(nval_t) :: v

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
	end if

	if (node%is_empty) return

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

	v = emit_assign_val(em, node)

end subroutine emit_assign

!===============================================================================

recursive function emit_assign_val(em, node) result(res)

	! Plain and compound assignment to a variable or an element of an array.  The
	! value is that of the target afterward, which is lent, since `a = b = 1` is
	! an assignment of an assignment

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: res

	!********

	integer :: op, ety, vty, rank_

	type(c_ptr) :: p, ps, a, subs, valp, ignored

	type(nval_t) :: v, cur
	type(nval_t), allocatable :: pending(:)

	type(place_t) :: pl

	res = dummy_val(em, node%val%type)

	if (node%op%kind /= equals_token) then
		! `x += y` is `x = x + y`
		select case (node%op%kind)
		case (plus_equals_token);    op = plus_token
		case (minus_equals_token);   op = minus_token
		case (star_equals_token);    op = star_token
		case (slash_equals_token);   op = slash_token
		case (percent_equals_token); op = percent_token
		case (sstar_equals_token);   op = sstar_token
		case (lless_equals_token);   op = lless_token
		case (ggreater_equals_token);op = ggreater_token
		case (amp_equals_token);     op = amp_token
		case (pipe_equals_token);    op = pipe_token
		case (caret_equals_token);   op = caret_token
		case default
			call unsupported(em, 'the `'//node%op%text//'` operator')
			return
		end select
	end if

	if (allocated(node%member)) then
		! A member of a struct, or an element of one.  The value is made first,
		! because it might change what the struct is
		v = emit_expr(em, node%right)
		if (.not. place_of(em, node, pl)) then
			call dispose(em, v)
			return
		end if
		if (node%op%kind /= equals_token) then
			cur = place_load(em, pl)
			if (pl%ty == array_type) then
				if (node%op%kind == matmul_token) then
					call unsupported(em, 'a matrix product')
					return
				end if
				v = arr_binop_vals(em, op, node%op%text, cur, v, node%val)
			else
				v = binop_vals(em, op, cur, v, node%op%text)
			end if
		end if
		call elem_store(em, pl%p, pl%ty, v)
		res = place_load(em, pl)
		return
	end if

	if (allocated(node%lsubscripts)) then
		if (.not. ref_info(em, node, ps, vty, ety, rank_)) return

		if (vty == str_type .or. size(node%lsubscripts) /= rank_ .or. .not. all_scalar(node)) then
			! A character or substring, or a slice
			if (node%op%kind /= equals_token) then
				call unsupported(em, 'a compound assignment to a slice or substring')
				return
			end if
			if (vty == str_type .and. size(node%lsubscripts) /= 1) then
				call unsupported(em, 'this subscript of a string')
				return
			end if
			if (vty == array_type .and. size(node%lsubscripts) /= rank_) then
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
				if (v%ty == array_type) then
					call rt_void(em, 'sy_arr_slice_set', [a, subs, v%v, LLVMConstNull(em%t_ptr)])
				else
					valp = entry_alloca(em, elem_ll(em, ety), 'fill')
					if (ety == str_type) ignored = LLVMBuildStore(em%bld, LLVMConstNull(em%t_ptr), valp)
					call elem_store(em, valp, ety, v)
					call rt_void(em, 'sy_arr_slice_set', [a, subs, LLVMConstNull(em%t_ptr), valp])
					if (ety == str_type) call rt_void(em, 'sy_str_free', &
						[LLVMBuildLoad2(em%bld, em%t_ptr, valp, cs(''))])
					v%owned = .false.
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

	v = emit_expr(em, node%right)

	if (node%op%kind /= equals_token) then
		cur = load_var(em, node)
		if (is_arr(node%val)) then
			if (node%op%kind == matmul_token) then
				call unsupported(em, 'a matrix product')
				return
			end if
			v = arr_binop_vals(em, op, node%op%text, cur, v, node%val)
		else
			v = binop_vals(em, op, cur, v, node%op%text)
		end if
	end if

	call store_var(em, p, slot_type(em, node), v)
	res = load_var(em, node)

end function emit_assign_val

!===============================================================================

recursive subroutine emit_call_stmt(em, node)

	! A call whose value, if any, is discarded

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: i, n

	type(c_ptr) :: parts, slot, ignored
	type(c_ptr), allocatable :: args(:)

	type(nval_t) :: v
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

	case ('exit')
		v = convert(em, emit_expr(em, node%args(1)), i32_type)
		call rt_void(em, 'sy_exit', [v%v])

	case default
		v = emit_intr_call(em, node)
		call dispose(em, v)

	end select

end subroutine emit_call_stmt

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

	case (let_expr, assignment_expr)
		call emit_stmt(em, node)
		if (.not. node%is_empty) then
			if (.not. (allocated(node%lsubscripts) .or. allocated(node%member))) then
				v = load_var(em, node)
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

subroutine begin_fn(em, name, ret_ll, ret_ty, is_main, fn)

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

	!********

	type(c_ptr) :: fty, ignored
	type(c_ptr) :: nothing(1)

	if (present(fn)) then
		em%fn = fn
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

	if (allocated(em%l%p)) deallocate(em%l%p, em%l%ty, em%l%own, em%l%elem, em%l%rank, em%l%sk)

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
	end if

	fn = fn_llvm(em, decl%id_index)
	if (.not. c_associated(fn)) return

	associate (f => em%fns%fns(decl%id_index))

		call begin_fn(em, '', em%t_void, f%type%type, .false., fn)

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

recursive function emit_user_call(em, node) result(r)

	! Call a user fn.  A by-value argument is converted to the type of the
	! parameter.  A string is lent to the fn, which copies what it keeps.  A
	! by-reference argument is the address of the variable, and so is an
	! `&const` one, unless it's an expression, which gets a stack slot

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	!********

	integer :: i, n, id, off

	type(c_ptr) :: fn, ignored
	type(c_ptr), allocatable :: args(:)

	type(nval_t) :: v
	type(nval_t), allocatable :: pending(:)

	type(place_t) :: pl

	id = node%id_index
	r = dummy_val(em, node%val%type)

	! The receiver of a method is its first argument
	off = merge(1, 0, node%kind == method_call_expr)

	fn = fn_llvm(em, id)
	if (.not. c_associated(fn)) return

	n = 0
	if (allocated(node%args)) n = size(node%args)
	allocate(args(max(n, 1)), pending(max(n, 1)))
	args = em%t_ptr

	do i = 1, n
		associate (arg => node%args(i))

			if (param_is_ref(em, id, i)) then
				if (arg%kind == name_expr .and. .not. allocated(arg%lsubscripts) &
						.and. .not. allocated(arg%member)) then
					args(i) = var_ptr(em, arg, arg%val%type)
					if (.not. c_associated(args(i))) return
				else if (arg%kind == dot_expr .or. allocated(arg%member) .or. &
						(off == 1 .and. i == 1 .and. allocated(arg%lsubscripts))) then
					! A member of a struct, or an element of an array of them
					if (.not. place_of(em, arg, pl)) return
					args(i) = pl%p
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

	do i = 1, n
		call dispose(em, pending(i))
	end do

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
			.not. (subj%ty == array_type .and. supported_type(subj%elem))) then
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
								held%elem == v%elem) then
							! The same shape and elements
							t%ty = bool_type
							t%owned = .false.
							t%v = LLVMBuildICmp(em%bld, LLVM_INT_NE, rt_call(em, 'sy_arr_eq', &
								em%t_i8, [held%v, v%v]), LLVMConstInt(em%t_i8, 0_c_int64_t, 0), cs(''))
							call dispose(em, v)
						else
							t = binop_vals(em, eequals_token, held, v, '==')
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

	a = emit_expr(em, node%array)
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
	type(c_ptr) :: cur, ahead, behind, zero
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
	ignored = LLVMBuildStore(em%bld, LLVMBuildAdd(em%bld, cur, st%v, cs('')), it)
	ignored = LLVMBuildBr(em%bld, cond_bb)

	call LLVMPositionBuilderAtEnd(em%bld, end_bb)

end subroutine emit_for


!===============================================================================
! Output

subroutine finish(em, n, diags)

	! Check the module, optimize it, and write what `n%out` asks for

	type(nemitter_t), intent(inout) :: em
	type(native_t), intent(in) :: n
	type(string_vector_t), intent(inout) :: diags

	!********

	character(len = :), allocatable :: out, obj, crt, cc, cmd, flags

	integer :: i, ou, ios, es, cmd_stat, ln

	type(c_ptr) :: msg, err, opts

	logical :: is_ll, is_obj

	if (.not. allocated(n%out)) then
		call diags%push(err_prefix//'no output file for the native backend'//color_reset)
		return
	end if
	out = n%out

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

	cc = 'cc'
	call get_environment_variable('CC', length = ln)
	if (ln > 0) then
		deallocate(cc)
		allocate(character(len = ln) :: cc)
		call get_environment_variable('CC', cc)
	end if

	! The same floating point as the interpreter, so no fused multiply-adds
	flags = '-O2 -ffp-contract=off'
	cmd = cc//' '//flags//' '//sh_quote(obj)//' '//sh_quote(crt)//' -lm -o '//sh_quote(out)

	call execute_command_line(cmd, wait = .true., exitstat = es, cmdstat = cmd_stat)
	if (cmd_stat /= 0 .or. es /= 0) then
		call diags%push(err_prefix//'linking failed: `'//cmd//'`'//color_reset)
	end if

	call delete_file(obj)
	call delete_file(crt)

end subroutine finish

!===============================================================================

function sh_quote(s) result(r)
	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r
	r = "'"//s//"'"
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
