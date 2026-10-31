
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
		is_name_char, is_numeric_type, wider_type
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
		! whether the code that made it still owns it.  Only a str can be owned.
		! Whoever consumes an owned value has to free it, with dispose(), or
		! move it into a variable.  A variable's value or a literal is not owned

		type(c_ptr) :: v = c_null_ptr
		integer :: ty = unknown_type
		logical :: owned = .false.

	end type nval_t

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

	end type slots_t

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

		! The user fns, for their signatures
		type(fns_t), pointer :: fns => null()

		! The LLVM fn and type of each user fn, by id.  They are declared at their
		! first reference
		type(c_ptr), allocatable :: fnv(:), fnt(:)

		! The fn being built: its LLVM value, the block that every `return` goes
		! to, the stack slot of its return value, and its syntran return type
		type(c_ptr) :: fn = c_null_ptr, exit_bb = c_null_ptr, retslot = c_null_ptr
		integer :: ret_type = void_type
		logical :: in_main = .true.

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

	type(nemitter_t) :: em

	diags = new_string_vector()

	em%print_result = n%print_result
	em%trim_result  = n%trim_result
	em%diags = new_string_vector()
	em%fns => state%fns

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

	! The fns first.  Each is built on its own, and a global variable that one
	! refers to is made at its first reference
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == fn_declaration) call emit_fn(em, tree%members(i))
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
	case (str_type)
		t = em%t_ptr
	case default
		t = em%t_i32
	end select

end function ll_type

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
	if (v%ty == str_type) call rt_void(em, 'sy_str_free', [v%v])

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
		allocate(s%p(n), s%ty(n), s%own(n))
		s%p   = c_null_ptr
		s%ty  = unknown_type
		s%own = .false.
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

	if (.not. supported_type(ty)) then
		call unsupported(em, 'a variable of type `'//kind_name(ty)//'`')
		return
	end if

	if (node%is_loc) then
		call slots_ensure(em%l, id)
		if (.not. c_associated(em%l%p(id))) then
			em%l%p(id) = entry_alloca(em, ll_type(em, ty), 'l'//str(id))
			if (ty == str_type) then
				ignored = LLVMBuildStore(em%abld, LLVMConstNull(em%t_ptr), em%l%p(id))
			end if
			em%l%ty(id)  = ty
			em%l%own(id) = ty == str_type
		end if
		p = em%l%p(id)

	else
		call slots_ensure(em%g, id)
		if (.not. c_associated(em%g%p(id))) then
			g = LLVMAddGlobal(em%llmod, ll_type(em, ty), cs('g'//str(id)))
			call LLVMSetInitializer(g, LLVMConstNull(ll_type(em, ty)))
			call LLVMSetLinkage(g, LLVM_INTERNAL_LINKAGE)
			em%g%p(id)  = g
			em%g%ty(id) = ty
		end if
		p = em%g%p(id)
	end if

end function var_ptr

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

	p = var_ptr(em, node, node%val%type)
	if (.not. c_associated(p)) then
		r = dummy_val(em, node%val%type)
		return
	end if

	r%ty = slot_type(em, node)
	r%owned = .false.
	r%v = LLVMBuildLoad2(em%bld, ll_type(em, r%ty), p, cs(''))

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

	if (vty == str_type) then
		if (v%ty /= str_type) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a string')
			return
		end if
		if (v%owned) then
			new_ = v%v
		else
			new_ = rt_call(em, 'sy_str_dup', em%t_ptr, [v%v])
		end if
		old = LLVMBuildLoad2(em%bld, em%t_ptr, p, cs(''))
		ignored = LLVMBuildStore(em%bld, new_, p)
		call rt_void(em, 'sy_str_free', [old])

	else if (is_numeric_type(vty) .or. vty == bool_type) then
		if (v%ty /= vty .and. .not. (is_numeric_type(v%ty) .and. is_numeric_type(vty))) then
			call unsupported(em, 'assigning a `'//kind_name(v%ty)//'` to a `'// &
				kind_name(vty)//'`')
			return
		end if
		cv = convert(em, v, vty)
		ignored = LLVMBuildStore(em%bld, cv%v, p)

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

	case (name_expr)
		if (allocated(node%lsubscripts) .or. allocated(node%member)) then
			call unsupported(em, 'a subscript or member of a variable')
			r = dummy_val(em, node%val%type)
		else
			r = load_var(em, node)
		end if

	case (binary_expr)
		r = emit_binary(em, node)

	case (unary_expr)
		r = emit_unary(em, node)

	case (fn_call_intr_expr)
		r = emit_intr_call(em, node)

	case (fn_call_expr)
		r = emit_user_call(em, node)

	case default
		call unsupported(em, 'a `'//kind_name(node%kind)//'`')
		r = dummy_val(em, node%val%type)

	end select

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
	case default
		call unsupported(em, 'a literal of type `'//kind_name(node%val%type)//'`')
		r = dummy_val(em, node%val%type)
	end select

end function emit_literal

!===============================================================================

recursive function emit_binary(em, node) result(r)

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	type(nval_t) :: r

	type(nval_t) :: l, rr

	if (is_arr(node%left%val) .or. is_arr(node%right%val)) then
		call unsupported(em, 'an operation on an array')
		r = dummy_val(em, node%val%type)
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

	type(nval_t) :: x

	if (is_arr(node%right%val)) then
		call unsupported(em, 'an operation on an array')
		r = dummy_val(em, node%val%type)
		return
	end if

	x = emit_expr(em, node%right)
	r%ty = x%ty
	r%owned = .false.

	select case (node%op%kind)

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
		call unsupported(em, 'the `'//node%op%text//'` operator')
		r = dummy_val(em, x%ty)

	end select

end function emit_unary

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

	case ('i32', 'i64')
		a = emit_expr(em, node%args(1))
		if (a%ty == str_type) then
			! The code of a single character
			r%ty = i32_type
			r%owned = .false.
			r%v = rt_call(em, 'sy_str_code', em%t_i32, [a%v])
			call dispose(em, a)
			r = convert(em, r, merge(i32_type, i64_type, base == 'i32'))
		else if (is_numeric_type(a%ty)) then
			r = convert(em, a, merge(i32_type, i64_type, base == 'i32'))
		else
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, node%val%type)
		end if

	case ('f32', 'f64')
		a = emit_expr(em, node%args(1))
		if (is_numeric_type(a%ty)) then
			r = convert(em, a, merge(f32_type, f64_type, base == 'f32'))
		else
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, node%val%type)
		end if

	case ('exp', 'log', 'log10', 'sqrt', 'cos', 'sin', 'tan', 'acos', 'asin', 'atan', &
			'log2')
		a = emit_expr(em, node%args(1))
		if (a%ty /= f32_type .and. a%ty /= f64_type) then
			call unsupported(em, 'the intrinsic fn `'//base//'` of a value of type `'// &
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, node%val%type)
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
		a = emit_expr(em, node%args(1))
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
				kind_name(a%ty)//'`', node%identifier%pos)
			r = dummy_val(em, node%val%type)
		end select

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

	case (fn_declaration)
		! Built separately, since a fn is its own LLVM function

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

	if (is_arr(node%val)) then
		call unsupported(em, 'an array variable')
		return
	end if

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

	! Plain and compound assignment to a variable

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	integer :: op

	type(c_ptr) :: p
	type(nval_t) :: v, cur

	if (allocated(node%lsubscripts) .or. allocated(node%member) .or. &
			is_arr(node%val) .or. is_arr(node%right%val)) then
		call unsupported(em, 'an assignment to an element, member, or array')
		return
	end if

	p = var_ptr(em, node, node%val%type)
	if (.not. c_associated(p)) return

	v = emit_expr(em, node%right)

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
			call dispose(em, v)
			return
		end select
		cur = load_var(em, node)
		v = binop_vals(em, op, cur, v, node%op%text)
	end if

	call store_var(em, p, slot_type(em, node), v)

end subroutine emit_assign

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

	case (while_statement, for_statement, break_statement, continue_statement)
		call emit_stmt(em, node)
		call emit_result_text(em, 'Error: <invalid_value>')

	case (return_statement)
		call emit_stmt(em, node)

	case (fn_declaration)

	case (fn_call_expr)
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
	em%block_exit = c_null_ptr

	if (allocated(em%l%p)) deallocate(em%l%p, em%l%ty, em%l%own)

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
			call rt_void(em, 'sy_str_free', [old])
		end do
	end if

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

	integer :: i, n, n_old

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

		n = 0
		if (allocated(f%params)) n = size(f%params)
		allocate(ptys(max(n, 1)))

		ok = .true.
		do i = 1, n
			if (param_is_ref(em, id, i)) then
				ptys(i) = em%t_ptr
				if (.not. supported_type(f%params(i)%type)) ok = .false.
			else
				ptys(i) = ll_type(em, f%params(i)%type)
				if (.not. supported_type(f%params(i)%type)) ok = .false.
			end if
		end do

		if (f%type%type == void_type) then
			ret = em%t_void
		else
			ret = ll_type(em, f%type%type)
			if (.not. supported_type(f%type%type)) ok = .false.
		end if

		if (.not. ok) then
			call unsupported(em, 'a fn with a parameter or return value that is not '// &
				'a number, bool, or string')
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

recursive subroutine emit_fn(em, decl)

	! Build a user fn.  A parameter passed by value is a stack slot of its own, so
	! the body may assign to it, and a string is copied in so that the fn owns it

	type(nemitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl

	!********

	integer :: i, slot, ptype

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

		if (allocated(decl%params)) then
			do i = 1, size(decl%params)
				slot  = decl%params(i)
				ptype = f%params(i)%type
				pv = LLVMGetParam(fn, i - 1)

				call slots_ensure(em%l, slot)
				em%l%ty(slot) = ptype

				if (param_is_ref(em, decl%id_index, i)) then
					! The caller's variable itself
					em%l%p(slot) = pv
					em%l%own(slot) = .false.
				else
					p = entry_alloca(em, ll_type(em, ptype), 'p'//str(slot))
					em%l%p(slot) = p
					if (ptype == str_type) then
						ignored = LLVMBuildStore(em%abld, LLVMConstNull(em%t_ptr), p)
						em%l%own(slot) = .true.
						copy%ty = str_type
						copy%owned = .false.
						copy%v = pv
						call store_var(em, p, str_type, copy)
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

	integer :: i, n, id

	type(c_ptr) :: fn, ignored
	type(c_ptr), allocatable :: args(:)

	type(nval_t) :: v
	type(nval_t), allocatable :: pending(:)

	id = node%id_index
	r = dummy_val(em, node%val%type)

	if (node%kind /= fn_call_expr) then
		call unsupported(em, 'a method call')
		return
	end if

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
						em%fns%fns(id)%params(i)%type)) then
					pending(i) = v
					pending(i)%owned = v%owned
					v = convert(em, v, em%fns%fns(id)%params(i)%type)
				else
					pending(i) = v
				end if
				args(i) = v%v
			end if

		end associate
	end do

	r%ty = em%fns%fns(id)%type%type
	r%owned = r%ty == str_type
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

		if (em%ret_type == str_type) then
			if (v%ty /= str_type) then
				call unsupported(em, 'returning a `'//kind_name(v%ty)//'` from a fn of strings')
			else
				if (.not. v%owned) then
					v%v = rt_call(em, 'sy_str_dup', em%t_ptr, [v%v])
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

	if (node%array%kind /= array_expr) then
		call unsupported(em, 'a `for` loop over an array or a string')
		return
	end if
	if (node%array%val%array%kind /= bound_array .and. &
			node%array%val%array%kind /= step_array) then
		call unsupported(em, 'a `for` loop over an array or a string')
		return
	end if
	elem = node%array%val%array%type
	if (elem /= i32_type .and. elem /= i64_type) then
		call unsupported(em, 'a `for` loop over an array or a string')
		return
	end if

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
