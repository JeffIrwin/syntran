
!===============================================================================

module syntran__transpile_m

	! Fortran backend: lowers an AST (syntax_node_t) into the source text of a
	! standalone modern Fortran program, as an alternative to compiling it to
	! bytecode for the VM (c.f. compile.f90).
	!
	! Module + submodule layout mirrors compile.f90 + compile_ctrl.f90:
	!
	!   transpile_expr.f90 - expressions, literals, names, intrinsic fn calls
	!   transpile_stmt.f90 - statements, fns, and assembling the whole program
	!
	! The generated program embeds the runtime module from rt/syntran_rt.f90
	! (via the generated transpile_rt.f90), so it can be compiled by itself with
	! any Fortran compiler.
	!
	! Name resolution is already done by the parser.  Every binding (let, for
	! loop variable, fn parameter) gets a slot (node%is_loc, node%id_index) that
	! is unique within its fn, or within the top level for globals, even when
	! syntran scopes shadow a name.  The emitter exploits that: a variable is
	! always emitted as <name>_g<id> (global) or <name>_l<id> (local), so syntran
	! scopes and Fortran's lack of block-local declarations never interact.

	use syntran__consts_m
	use syntran__errors_m
	use syntran__runtime_m
	use syntran__transpile_rt_m
	use syntran__types_m
	use syntran__utils_m
	use syntran__value_m

	implicit none

	!********

	type transpile_t

		! Options and result of one transpilation.  Pass to syntran_eval(),
		! syntran_interpret_file(), etc. to transpile instead of interpret

		! Print the value of the program's last statement when it finishes, like
		! the CLI does unless it is given `--quiet`
		logical :: print_result = .true.

		! The CLI trims and left-adjusts the result of a *file*.  It does not for
		! a `-c` command string
		logical :: trim_result = .false.

		! The generated Fortran source, one element per line
		type(string_vector_t) :: src

	end type transpile_t

	!********

	type slot_info_t

		! What the emitter remembers about a declared variable.  A reference to
		! a variable (a name_expr) only carries the type of the whole reference,
		! which for a subscripted one is the type of the element or slice.  We
		! need to know what is being subscripted to tell, say, the character of a
		! string `s[i]` from an element of an array of strings `names[i]`

		logical :: known = .false.

		! The variable's own type, and for an array its element type and rank
		integer :: type = unknown_type, elem = unknown_type, rank = 0

	end type slot_info_t

	!********

	type emitter_t

		! Internal state of the emitter.  There is one body and one set of
		! declarations per Fortran procedure being generated, plus one set of
		! module-level declarations for syntran's global variables

		! Current indentation level of em%body, in units of 4 spaces
		integer :: indent = 0

		! Counter for hidden compiler-generated variables
		integer :: tmp_count = 0

		! Source location of the statement being emitted, for diagnostics
		integer :: cur_src_id = 0, cur_src_pos = 0

		! The statement that the last diagnostic was reported for.  There is only
		! one diagnostic per statement, because the rest would be consequences
		! of the first
		integer :: diag_src_id = -1, diag_src_pos = -1

		! Fn table, for the signatures of user fns
		type(fns_t), pointer :: fns => null()

		! Fortran source of the current procedure's body and of its local
		! declarations.  Locals get declared lazily as they are encountered, so
		! the declarations can be written ahead of the body once it is done
		type(string_vector_t) :: body, decls

		! Module-level declarations.  syntran's global variables are module
		! variables so that fns can see them
		type(string_vector_t) :: gdecls

		! Every procedure emitted so far (a fn's whole source, or the top-level
		! program's), in order
		type(string_vector_t) :: procs

		! Slots which have already been declared.  The key is
		! 2 * id_index + (1 if local else 0)
		type(integer_vector_t) :: seen_local, seen_global

		! The type of each declared variable, indexed by slot id
		type(slot_info_t), allocatable :: local_slots(:), global_slots(:)

		! Error messages
		type(string_vector_t) :: diags

		! Are we emitting a condition that is evaluated more than once per time that
		! its statement is reached, i.e. of a loop or of an `else if`?  Statements
		! can't be hoisted out of an expression there
		logical :: in_cond = .false.

		! Number of syntran loops enclosing the statement being emitted.  A
		! `break` or `continue` is Fortran's `exit` or `cycle` in a loop
		integer :: loop_depth = 0

		! Outside of a loop, `break` leaves the outermost enclosing block, which
		! is emitted as a named Fortran `block` construct.  This is that
		! construct's name while inside one, or empty
		character(len = :), allocatable :: break_label

		! Are we emitting the program's top-level statements, as opposed to the
		! body of a fn?
		logical :: top_level = .true.

		! See transpile_t
		logical :: print_result = .true., trim_result = .false.

	end type emitter_t

!===============================================================================

	interface

		! Implemented in transpile_stmt.f90
		!
		! `state` supplies the fn table, which has the fns' signatures that the
		! tree itself does not.  Diagnostics for constructs that the transpiler
		! does not support yet are copied to `diags`
		module subroutine transpile_tree(tree, state, t, diags)
			type(syntax_node_t), intent(in) :: tree
			type(state_t), intent(in), target :: state
			type(transpile_t), intent(inout) :: t
			type(string_vector_t), intent(out) :: diags
		end subroutine transpile_tree

		recursive module subroutine emit_stmt(em, node)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
		end subroutine emit_stmt

		! Implemented in transpile_expr.f90

		! Fortran expression for `node`.  May add statements to em%body first, to
		! hold a hidden temporary that the expression refers to
		recursive module function emit_expr(em, node) result(s)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
			character(len = :), allocatable :: s
		end function emit_expr

		! Fortran name of the variable in slot `id` of a fn (or of the global
		! scope), whose name in the syntran source is `name`
		module function slot_name(name, is_loc, id) result(s)
			character(len = *), intent(in) :: name
			logical, intent(in) :: is_loc
			integer, intent(in) :: id
			character(len = :), allocatable :: s
		end function slot_name

		! Fortran name of the variable that a name_expr, let_expr, for_statement,
		! etc. refers to
		module function var_name(node) result(s)
			type(syntax_node_t), intent(in) :: node
			character(len = :), allocatable :: s
		end function var_name

		! Fortran name of a user fn, from its declaration or a call to it
		module function fn_name(name, id) result(s)
			character(len = *), intent(in) :: name
			integer, intent(in) :: id
			character(len = :), allocatable :: s
		end function fn_name

		! A whole Fortran declaration of a variable called `name` with the type of
		! `val`, e.g. `integer(int32) :: x` or `real(real64), allocatable :: x(:,:)`.
		! Sets `ok` to false (and returns garbage) if the type can't be
		! transpiled yet
		module function decl_line(val, name, ok) result(s)
			type(value_t), intent(in) :: val
			character(len = *), intent(in) :: name
			logical, intent(out) :: ok
			character(len = :), allocatable :: s
		end function decl_line

		! Declare the variable of a binding node (once) in the module scope if it
		! is global, or in the current procedure if it is local
		module subroutine declare_var(em, node, val)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
			type(value_t), intent(in) :: val
		end subroutine declare_var

		! Declare a hidden temporary variable in the current procedure and return
		! its name.  `type_str` is a Fortran type spec like `integer(int32)`.
		! Pass `dims` like `(:,:)` for an allocatable array
		module function new_tmp(em, type_str, prefix, dims) result(name)
			type(emitter_t), intent(inout) :: em
			character(len = *), intent(in) :: type_str, prefix
			character(len = *), intent(in), optional :: dims
			character(len = :), allocatable :: name
		end function new_tmp

		! An assignment, as a statement.  Used by expressions that assign, which
		! hoist the assignment ahead of their own statement
		recursive module subroutine emit_assign(em, node)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
		end subroutine emit_assign

		! Fortran designator of a variable reference, which may be subscripted:
		! the name_expr of an expression, or the target of an assignment_expr.
		! With `hoist`, subscripts that aren't trivial are evaluated into
		! temporaries first, for use of the designator more than once.  `target`
		! is for the target of an assignment, which can't be a fn like a character
		! of a string at an index that's an expression can be
		recursive module function emit_name_ref(em, node, hoist, target) result(s)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
			logical, intent(in), optional :: hoist, target
			character(len = :), allocatable :: s
		end function emit_name_ref

		! Convert Fortran expression `s` of syntran type `from` to type `to`
		module function convert(s, from, to) result(r)
			character(len = *), intent(in) :: s
			integer, intent(in) :: from, to
			character(len = :), allocatable :: r
		end function convert

	end interface

!===============================================================================

contains

!===============================================================================

function elem_type(val) result(t)

	! The type of a value, or of its elements if it's an array

	type(value_t), intent(in) :: val
	integer :: t

	t = val%type
	if (val%type == array_type) then
		if (allocated(val%array)) t = val%array%type
	end if

end function elem_type

!===============================================================================

function is_arr(val) result(a)
	type(value_t), intent(in) :: val
	logical :: a
	a = val%type == array_type
end function is_arr

!===============================================================================

function is_numeric_type(type_)
	integer, intent(in) :: type_
	logical :: is_numeric_type
	is_numeric_type = any(type_ == [i32_type, i64_type, f32_type, f64_type])
end function is_numeric_type

!===============================================================================

function wider_type(a, b) result(c)

	! The type that syntran's arithmetic promotes a pair of numeric types to:
	! f64 > f32 > i64 > i32

	integer, intent(in) :: a, b
	integer :: c

	if (a == f64_type .or. b == f64_type) then
		c = f64_type
	else if (a == f32_type .or. b == f32_type) then
		c = f32_type
	else if (a == i64_type .or. b == i64_type) then
		c = i64_type
	else
		c = a
	end if

end function wider_type

!===============================================================================

function type_kind_suffix(t) result(s)

	! `int32`, `real64`, etc., the Fortran kind of a numeric type

	integer, intent(in) :: t
	character(len = :), allocatable :: s

	select case (t)
	case (i32_type)
		s = 'int32'
	case (i64_type)
		s = 'int64'
	case (f32_type)
		s = 'real32'
	case (f64_type)
		s = 'real64'
	case default
		s = 'int32'
	end select

end function type_kind_suffix

!===============================================================================

function type_spec(type_) result(s)

	! Fortran type spec for a scalar syntran type, e.g. for a temporary

	integer, intent(in) :: type_
	character(len = :), allocatable :: s

	select case (type_)
	case (i32_type)
		s = 'integer(int32)'
	case (i64_type)
		s = 'integer(int64)'
	case (f32_type)
		s = 'real(real32)'
	case (f64_type)
		s = 'real(real64)'
	case (bool_type)
		s = 'logical'
	case (str_type)
		s = 'character(len = :), allocatable'
	case default
		s = 'integer(int32)'
	end select

end function type_spec

!===============================================================================

function unparen(s) result(r)

	! Remove a pair of parentheses which wrap the whole of a Fortran expression.
	! Every operation is emitted fully parenthesized, which is more than a
	! condition or the right-hand side of an assignment needs

	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r

	integer :: i, depth
	logical :: in_quote

	r = s
	if (len(s) < 2) return
	if (s(1:1) /= '(' .or. s(len(s):len(s)) /= ')') return

	! Does the paren that is opened first close at the very end?
	depth = 0
	in_quote = .false.
	do i = 1, len(s)
		if (s(i:i) == "'") in_quote = .not. in_quote
		if (in_quote) cycle
		if (s(i:i) == '(') then
			depth = depth + 1
		else if (s(i:i) == ')') then
			depth = depth - 1
			if (depth == 0 .and. i < len(s)) return
		end if
	end do

	if (depth == 0) r = s(2: len(s) - 1)

end function unparen

!===============================================================================

function is_simple(node) result(simple)

	! Can this expression be evaluated more than once, without a temporary,
	! with no side effects or significant cost?

	type(syntax_node_t), intent(in) :: node
	logical :: simple

	simple = node%kind == literal_expr
	if (node%kind == name_expr) simple = .not. allocated(node%lsubscripts)

end function is_simple

!===============================================================================

subroutine em_line(em, s)

	! Append a line to the current procedure body at the current indentation

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: s

	call em%body%push(repeat('    ', em%indent)//s)

end subroutine em_line

!===============================================================================

subroutine record_slot(em, is_loc, id, val)

	! Remember the type of a variable that was just declared

	type(emitter_t), intent(inout) :: em
	logical, intent(in) :: is_loc
	integer, intent(in) :: id
	type(value_t), intent(in) :: val

	type(slot_info_t) :: info
	type(slot_info_t), allocatable :: tmp(:)

	info%known = .true.
	info%type = val%type
	if (val%type == array_type .and. allocated(val%array)) then
		info%elem = val%array%type
		info%rank = val%array%rank
	end if

	if (is_loc) then
		if (.not. allocated(em%local_slots)) allocate(em%local_slots(max(16, 2 * id)))
		if (id > size(em%local_slots)) then
			allocate(tmp(2 * id))
			tmp(1: size(em%local_slots)) = em%local_slots
			call move_alloc(tmp, em%local_slots)
		end if
		em%local_slots(id) = info
	else
		if (.not. allocated(em%global_slots)) allocate(em%global_slots(max(16, 2 * id)))
		if (id > size(em%global_slots)) then
			allocate(tmp(2 * id))
			tmp(1: size(em%global_slots)) = em%global_slots
			call move_alloc(tmp, em%global_slots)
		end if
		em%global_slots(id) = info
	end if

end subroutine record_slot

!===============================================================================

function lookup_slot(em, is_loc, id) result(info)

	! The type of a declared variable.  info%known is false if it hasn't been

	type(emitter_t), intent(in) :: em
	logical, intent(in) :: is_loc
	integer, intent(in) :: id
	type(slot_info_t) :: info

	if (is_loc) then
		if (allocated(em%local_slots)) then
			if (id >= 1 .and. id <= size(em%local_slots)) info = em%local_slots(id)
		end if
	else
		if (allocated(em%global_slots)) then
			if (id >= 1 .and. id <= size(em%global_slots)) info = em%global_slots(id)
		end if
	end if

end function lookup_slot

!===============================================================================

subroutine em_unsupported(em, what, pos)

	! Report that a construct can't be transpiled yet.  The caret goes at the
	! optional source offset `pos` if given, otherwise at the start of the
	! statement being emitted

	type(emitter_t), intent(inout) :: em
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
		call em%diags%push(err_pre(EC_TRANSPILE_UNSUPPORTED)//what// &
			' is not supported by the Fortran transpiler yet'//color_reset)
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
		call em%diags%push(err_transpile_unsupported(context, span, what))

	end associate

end subroutine em_unsupported

!===============================================================================

logical function is_name_char(c)
	character, intent(in) :: c
	is_name_char = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') &
		.or. (c >= '0' .and. c <= '9') .or. c == '_'
end function is_name_char

!===============================================================================

end module syntran__transpile_m

!===============================================================================

