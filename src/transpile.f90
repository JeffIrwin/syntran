
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
	! syntran scopes shadow a name.  The emitter exploits that to name variables
	! <name>_g<id> (global) or <name>_l<id> (local), so syntran scopes and
	! Fortran's lack of block-local declarations never interact.
	!
	! That suffix is only added where it's needed.  Before emitting anything, a
	! pre-pass (collect_names()) gives the first variable or fn of each name
	! just its own name, unless that could clash with a Fortran intrinsic or
	! with a name that the emitter makes up (see bare_ok()).  Shadowed names and
	! the rest keep the suffix.

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

		! Position in the struct table of the struct that the variable, or the
		! elements of an array, is.  0 for anything else
		integer :: sk = 0

		! The variable's Fortran name, if it isn't the one that its references
		! give.  That's `self` of a method, which the parser makes the root of the
		! reference `field` for the implicit `self.field`
		character(len = :), allocatable :: fname

		! For a fn pointer, the type of what the fn returns.  These aren't a
		! value_t, whose nested components (a fn returning a fn) the compiler
		! can't be trusted to free, as value_destroy() says
		integer :: ret_type = unknown_type, ret_elem = unknown_type, ret_rank = 0

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

		! The `, step, lb = lb, ub = ub` arguments of rt_str_step_set(), left by
		! the designator of an assignment target which is a stepped slice of a
		! string, like `s[:-1:]`.  Fortran can't assign to a strided substring, so
		! the designator is the whole string and the assignment is a call.  Not
		! allocated otherwise
		character(len = :), allocatable :: str_step

		! Source location of the statement being emitted, for diagnostics
		integer :: cur_src_id = 0, cur_src_pos = 0

		! The statement that the last diagnostic was reported for.  There is only
		! one diagnostic per statement, because the rest would be consequences
		! of the first
		integer :: diag_src_id = -1, diag_src_pos = -1

		! Fn table, for the signatures of user fns
		type(fns_t), pointer :: fns => null()

		! Enum table, for the variants of the enums.  An enum value is emitted as
		! the zero-based index of its variant, and the enum's helper fns (see
		! emit_enum_procs()) map that to the variant's name and backing value
		type(enums_t), pointer :: enums => null()

		! Struct table.  A struct is a Fortran derived type, whose members are
		! components named by the index that the parser gave each of them
		type(structs_t), pointer :: structs => null()

		! Definitions of the derived types, which come before the module's
		! variables
		type(string_vector_t) :: tdecls

		! The signatures of the fn pointers that are used, each a type in the
		! generated program: an abstract interface of the signature and a derived
		! type with a procedure pointer component of it, which is the value.  The
		! key of a signature is its type name, like `fn(i32): i32`
		type(value_vector_t) :: sigs
		type(string_vector_t) :: sig_keys

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

		! Which globals (by slot id), locals of the current fn (by slot id), and
		! fns (by id) are named as they are in the syntran source, without a
		! suffix.  Anything out of range of these is not
		logical, allocatable :: bare_global(:), bare_local(:), bare_fn(:)

		! The lowercase names of the globals and fns that are, which a local of
		! the same name would hide inside its fn
		type(string_vector_t) :: bare_names

		! The type of each declared variable, indexed by slot id
		type(slot_info_t), allocatable :: local_slots(:), global_slots(:)

		! Error messages
		type(string_vector_t) :: diags

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
		module function slot_name(em, name, is_loc, id) result(s)
			type(emitter_t), intent(in) :: em
			character(len = *), intent(in) :: name
			logical, intent(in) :: is_loc
			integer, intent(in) :: id
			character(len = :), allocatable :: s
		end function slot_name

		! Fortran name of the variable that a name_expr, let_expr, for_statement,
		! etc. refers to
		module function var_name(em, node) result(s)
			type(emitter_t), intent(in) :: em
			type(syntax_node_t), intent(in) :: node
			character(len = :), allocatable :: s
		end function var_name

		! Fortran name of a user fn, from its declaration or a call to it
		module function fn_name(em, name, id) result(s)
			type(emitter_t), intent(in) :: em
			character(len = *), intent(in) :: name
			integer, intent(in) :: id
			character(len = :), allocatable :: s
		end function fn_name

		! The name of a fn without its suffix, as a Fortran name.  It's taken from
		! the fn's declaration, not from `name` of a call, so that every use of a
		! fn agrees
		module function fn_base(em, name, id) result(s)
			type(emitter_t), intent(in) :: em
			character(len = *), intent(in) :: name
			integer, intent(in) :: id
			character(len = :), allocatable :: s
		end function fn_base

		! A syntran name made into a Fortran one: only name characters, starting
		! with a letter, and not too long.  There's no suffix yet
		module function name_base(name, is_fn) result(s)
			character(len = *), intent(in) :: name
			logical, intent(in) :: is_fn
			character(len = :), allocatable :: s
		end function name_base

		! Can a name made by name_base() be used as is, with no suffix?  It can't
		! if it could be taken for a name that the emitter makes up, or if it
		! hides a Fortran intrinsic or the runtime's names.  `module_scope` is
		! for a global or a fn, which are also seen by the interfaces of fn pointers
		module function bare_ok(base, module_scope) result(ok)
			character(len = *), intent(in) :: base
			logical, intent(in) :: module_scope
			logical :: ok
		end function bare_ok

		! Fortran name of the component for member `id` of the struct at position
		! `k` of the table
		module function member_fname(em, k, id) result(s)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k, id
			character(len = :), allocatable :: s
		end function member_fname

		! A whole Fortran declaration of a variable called `name` with the type of
		! `val`, e.g. `integer(int32) :: x` or `real(real64), allocatable :: x(:,:)`.
		! Sets `ok` to false (and returns garbage) if the type can't be
		! transpiled yet
		module function decl_line(em, val, name, ok) result(s)
			type(emitter_t), intent(inout) :: em
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

		! A Fortran expression for the string that syntran makes from a value,
		! given the Fortran expression `s` for the value itself
		module function str_of(em, val, s) result(r)
			type(emitter_t), intent(inout) :: em
			type(value_t), intent(in) :: val
			character(len = *), intent(in) :: s
			character(len = :), allocatable :: r
		end function str_of

		! Position in the enum table of the enum that `val` is a value or an array
		! of, or 0 if it is unknown
		module function enum_slot_of(em, val) result(k)
			type(emitter_t), intent(in) :: em
			type(value_t), intent(in) :: val
			integer :: k
		end function enum_slot_of

		! Name of a helper fn of the enum at position `k` of the table.  The
		! `suffix` is `str` for the name of a variant as a string, `strs` for it
		! as an rt_str_t which is elemental, `val` for its backing value, or `of`
		! for the variant that has a backing value
		module function enum_fn(em, k, suffix) result(s)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k
			character(len = *), intent(in) :: suffix
			character(len = :), allocatable :: s
		end function enum_fn

		! Comparison of two enum expressions, `op` being a comparison token.  It
		! is the comparison of backing values, which only differs from comparing
		! the variants' indices if the enum has aliases
		module function enum_cmp(em, k, op, l, r) result(s)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k, op
			character(len = *), intent(in) :: l, r
			character(len = :), allocatable :: s
		end function enum_cmp

		! Position in the struct table of the struct that `val` is a value or an
		! array of, or 0 if it is unknown
		module function struct_slot_of(em, val) result(k)
			type(emitter_t), intent(in) :: em
			type(value_t), intent(in) :: val
			integer :: k
		end function struct_slot_of

		! Position in the signatures of the fn pointer type of `val`, which is added
		! if it's new
		module function fptr_slot(em, val) result(k)
			type(emitter_t), intent(inout) :: em
			type(value_t), intent(in) :: val
			integer :: k
		end function fptr_slot

		! `a[i] = rhs` for an array of strings with a subscript of a character of
		! each element, where `i` is a range.  A loop over the elements
		module subroutine emit_str_slice_assign(em, node, done)
			type(emitter_t), intent(inout) :: em
			type(syntax_node_t), intent(in) :: node
			logical, intent(out) :: done
		end subroutine emit_str_slice_assign

		! Name of the derived type of the enum at position `k` of the table, which
		! its helper fns are named after
		module function enum_tname(em, k) result(s)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k
			character(len = :), allocatable :: s
		end function enum_tname

		! Name of the derived type of the struct at position `k` of the table
		module function struct_tname(em, k) result(s)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k
			character(len = :), allocatable :: s
		end function struct_tname

		! The type and the name of the member of struct `k` which the parser gave
		! the index `id`.  `ok` is false if there is no such member
		module subroutine struct_member(em, k, id, val, name, ok)
			type(emitter_t), intent(in) :: em
			integer, intent(in) :: k, id
			type(value_t), intent(out) :: val
			character(len = :), allocatable, intent(out) :: name
			logical, intent(out) :: ok
		end subroutine struct_member

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
	case (enum_type)
		! The index of the variant
		s = 'integer(int32)'
	case (file_type)
		s = 'type(rt_file_t)'
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

function literal_value(s, v) result(is_lit)

	! Is the Fortran expression `s` a single string literal, like `'it''s'`
	! (and not a concatenation of them)?  If so, `v` is the string that it is
	! the literal of

	character(len = *), intent(in) :: s
	character(len = :), allocatable, intent(out) :: v
	logical :: is_lit

	integer :: i, n

	is_lit = .false.
	v = ''

	n = len(s)
	if (n < 2) return
	if (s(1:1) /= "'" .or. s(n:n) /= "'") return

	i = 2
	do while (i <= n)
		if (s(i:i) == "'") then
			if (i < n) then
				if (s(i+1:i+1) == "'") then
					! A doubled quote
					v = v//"'"
					i = i + 2
					cycle
				end if
			end if
			! The closing quote, which has to be the last character
			if (i /= n) return
			is_lit = .true.
			return
		end if
		v = v//s(i:i)
		i = i + 1
	end do

end function literal_value

!===============================================================================

function quote_literal(v) result(s)

	! The Fortran literal of the printable string `v`, the inverse of
	! literal_value()

	character(len = *), intent(in) :: v
	character(len = :), allocatable :: s

	integer :: i

	s = "'"
	do i = 1, len(v)
		if (v(i:i) == "'") then
			s = s//"''"
		else
			s = s//v(i:i)
		end if
	end do
	s = s//"'"

end function quote_literal

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

recursive function int_literal(node, val) result(is_lit)

	! Is this an integer literal, possibly with a sign?  The parser keeps the
	! sign of `-1` as a unary operator.  If so, its value is `val`

	type(syntax_node_t), intent(in) :: node
	integer(kind = 8), intent(out) :: val
	logical :: is_lit

	integer(kind = 8) :: inner

	is_lit = .false.
	val = 0

	if (node%kind == literal_expr) then
		if (node%val%type == i32_type) then
			val = node%val%sca%i32
			is_lit = .true.
		else if (node%val%type == i64_type) then
			val = node%val%sca%i64
			is_lit = .true.
		end if

	else if (node%kind == unary_expr) then
		if (node%op%kind == minus_token .or. node%op%kind == plus_token) then
			if (int_literal(node%right, inner)) then
				is_lit = .true.
				val = inner
				if (node%op%kind == minus_token) val = -inner
			end if
		end if
	end if

end function int_literal

!===============================================================================

function is_file_var(node) result(is_var)

	! Is this a plain variable that a file handle is updated through, as an
	! argument that the callee may define?  The constants std::IN, std::OUT, and
	! std::ERR are slots 1 to 4 of the global scope (see emit_name_ref()), but they
	! are emitted as fn results.  So are subscripted names and other expressions

	type(syntax_node_t), intent(in) :: node
	logical :: is_var

	is_var = node%kind == name_expr
	if (.not. is_var) return

	if (allocated(node%lsubscripts)) is_var = .false.
	if (.not. node%is_loc .and. node%id_index >= 1 .and. node%id_index <= 4) &
		is_var = .false.

end function is_file_var

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
	if (val%type == struct_type .or. info%elem == struct_type) then
		info%sk = struct_slot_of(em, val)
	else if (val%type == fn_type .and. allocated(val%fn_ret)) then
		! For a fn pointer, the struct that the fn returns
		info%sk = struct_slot_of(em, val%fn_ret)
		info%ret_type = val%fn_ret%type
		if (val%fn_ret%type == array_type .and. allocated(val%fn_ret%array)) then
			info%ret_elem = val%fn_ret%array%type
			info%ret_rank = val%fn_ret%array%rank
		end if
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

