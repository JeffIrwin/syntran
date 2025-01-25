
!===============================================================================

module syntran__types_m

	!use syntran__consts_m
	!use syntran__errors_m
	!use syntran__utils_m
	use syntran__value_m

	implicit none

	integer, parameter :: &
		TYPE_ARRAY_STRUCT_MISMATCH = 5, &
		TYPE_RANK_MISMATCH = 4, &
		TYPE_ARRAY_MISMATCH = 3, &
		TYPE_STRUCT_MISMATCH = 2, &
		TYPE_MISMATCH = 1, &
		TYPE_MATCH = 0

	!********

	type fn_t
		! Function signature: input and output types

		! Return type.  "A type is a value!"
		type(value_t) :: type

		! TODO: add a way to represent polymorphic intrinsic fn params, e.g.
		! i32 min(1, 2) vs f32 min(1.0, 2.0), but not bool min(true, false).
		! Maybe add an matrix of types(:,:) for each allowable type of a param?

		! Arguments/parameters.  Technically, "arguments" in most languages are
		! what Fortran calls "actual arguments" and "parameters" are Fortran
		! "dummy arguments"
		type(value_t), allocatable :: params(:)
		type(string_vector_t) :: param_names

		! Min number of variadic params.  Default < 0 means fn is not variadic
		!
		! This works for fns like max() or print() which have a min limit but an
		! unlimited upper bound of parameters.  For functions with a fixed
		! number of optional parameters, there should be a different mechanism

		integer :: variadic_min = -1, variadic_type

		! Reference to the function definition, i.e. the syntax node containing
		! the function parameters and body
		type(syntax_node_t), allocatable :: node

		logical :: is_intr = .true.

		contains
			procedure, pass(dst) :: copy => fn_copy
			generic, public :: assignment(=) => copy

	end type fn_t

	!********

	type fn_ternary_tree_node_t

		character :: split_char = ''
		type(fn_ternary_tree_node_t), allocatable :: left, mid, right

		type(fn_t), allocatable :: val
		integer :: id_index

		contains
			procedure, pass(dst) :: copy => fn_ternary_tree_copy
			generic, public :: assignment(=) => copy

	end type fn_ternary_tree_node_t

	!********

	type fn_dict_t
		! This is the fn dictionary of a single scope
		type(fn_ternary_tree_node_t), allocatable :: root
	end type fn_dict_t

	!********

	type fns_t

		! A list of function dictionaries for each scope level used during
		! parsing
		type(fn_dict_t) :: dict

		! Flat array of fns from all scopes, used for efficient interpreted
		! evaluation
		type(fn_t), allocatable :: fns(:)
		integer :: num_intr_fns

		! This is the scope level.  Each nested block statement that is entered
		! pushes 1 to scope.  Popping out of a block decrements the scope.
		! Each scope level has its own fn dict in dicts(:)
		integer :: scope = 1

		! TODO: scoping for nested fns?
		contains
			procedure :: &
				insert => fn_insert, &
				search => fn_search
		!		push_scope, pop_scope

	end type fns_t

	!********

	type syntax_token_t

		integer :: kind
		type(value_t) :: val
		integer :: pos
		!integer :: unit_ = 0  ! translation unit (src file) index for error diagnostic context
		integer :: unit_   ! translation unit (src file) index for error diagnostic context
		character(len = :), allocatable :: text

	end type syntax_token_t

	!********

	type syntax_node_t

		! FIXME: when adding new members here, make sure to explicitly copy them
		! in syntax_node_copy, or else assignment will yield bugs

		integer :: kind = 0

		! This structure could be more efficient.  For example, unary
		! expressions don't use left, name expressions don't use left or right,
		! binary expressions don't use an identifier, etc.  On the other hand,
		! they're all allocatable, so they shouldn't take up much space if not
		! allocated

		type(syntax_node_t), allocatable :: left, right, members(:), &
			condition, if_clause, else_clause, body, array, member

		! Array expression syntax nodes.  TODO: rename lbound, ubound to avoid
		! conflicts w/ Fortran keywords
		type(syntax_node_t), allocatable :: lbound, step, ubound, len_, &
			elems(:), rank

		! TODO: rename `size`
		type(syntax_node_t), allocatable :: lsubscripts(:), size(:), args(:), &
			usubscripts(:), ssubscripts(:)

		! Either scalar_sub, range_sub (unit step [0:2]), all_sub ([:]), or
		! step_sub ([0: 2: 8])
		integer :: sub_kind

		type(syntax_token_t) :: op, identifier

		integer :: id_index = 0, num_locs
		logical :: is_loc = .false.

		integer, allocatable :: params(:)
		logical, allocatable :: is_ref(:)  ! is param passed by reference?

		type(value_t) :: val

		!type(struct_t), allocatable :: struct
		character(len = :), allocatable :: struct_name

		type(string_vector_t) :: diagnostics

		! Only used to handle comment/whitespace lines for now
		logical :: is_empty = .false.

		! Is the parser expecting more input, e.g. from a continued line in the
		! interactive interpreter to match a semicolon, brace, etc.?
		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		contains

			! TODO: rename to to_str() for consistency with value_t
			procedure :: str => syntax_node_str, log_diagnostics

			! For gfortran, use a hand-written copy constructor.  For ifx, use
			! the intrinsic copy constructor.  If you try anything else, both
			! compilers will crash :(
			!
			! TODO: try gfortran 13.  Maybe it also needs the intrinsic like ifx
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy
#endif

	end type syntax_node_t

	!********

	! Dependencies between types could make this module difficult to split into
	! separate files.  I think I like the monolithic design anyway

	type ternary_tree_node_t

		character :: split_char = ''
		type(ternary_tree_node_t), allocatable :: left, mid, right

		type(value_t), allocatable :: val
		integer :: id_index

		contains
			!procedure :: print => ternary_node_print
			procedure, pass(dst) :: copy => ternary_tree_copy
			generic, public :: assignment(=) => copy

	end type ternary_tree_node_t

	!********

	type var_dict_t
		! This is the variable dictionary of a single scope
		type(ternary_tree_node_t), allocatable :: root
	end type var_dict_t

	!********

	type vars_t

		! A list of variable dictionaries for each scope level used during
		! parsing
		type(var_dict_t), allocatable :: dicts(:)
		integer :: scope_cap

		! Flat array of variables from all scopes, used for efficient
		! interpreted evaluation
		type(value_t), allocatable :: vals(:)

		! This is the scope level.  Each nested block statement that is entered
		! pushes 1 to scope.  Popping out of a block decrements the scope.
		! Each scope level has its own variable dict in dicts(:)
		integer :: scope = 1

		contains
			procedure :: &
				insert => var_insert, &
				search => var_search, &
				push_scope, pop_scope

			! This is required unfortunately
			procedure, pass(dst) :: copy => vars_copy
			generic, public :: assignment(=) => copy

	end type vars_t

	!********

	type struct_t
		! User-defined structure, aka derived type

		type(string_vector_t) :: member_names

		type(vars_t) :: vars  ! can't compile w/o allocatable if vars_t is defined below
		!type(vars_t), allocatable :: vars
		integer :: num_vars = 0

		contains
			! This is also required unfortunately
			procedure, pass(dst) :: copy => struct_copy
			generic, public :: assignment(=) => copy

	end type struct_t

	!********

	type struct_ternary_tree_node_t

		character :: split_char = ''
		type(struct_ternary_tree_node_t), allocatable :: left, mid, right

		type(struct_t), allocatable :: val
		integer :: id_index

		contains
			! This is also required unfortunately too
			procedure, pass(dst) :: copy => struct_ternary_tree_copy
			generic, public :: assignment(=) => copy
			!final :: struct_ternary_tree_final

	end type struct_ternary_tree_node_t

	!********

	type struct_dict_t
		! This is the struct dictionary of a single scope
		type(struct_ternary_tree_node_t), allocatable :: root
	end type struct_dict_t

	!********

	type structs_t

		! A list of struct dictionaries used during parsing
		!type(struct_dict_t) :: dicts(scope_max)
		type(struct_dict_t) :: dict

		!! Flat array of structs, used for efficient interpreted evaluation
		!type(struct_t), allocatable :: structs(:)

		contains
			procedure :: &
				insert => struct_insert, &
				exists => struct_exists, &
				search => struct_search

	end type structs_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

	!********

	type syntax_node_vector_t
		type(syntax_node_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_node
			procedure, pass(dst) :: copy => syntax_node_vector_copy
			generic, public :: assignment(=) => copy
	end type syntax_node_vector_t

!===============================================================================

contains

!===============================================================================

recursive subroutine vars_copy(dst, src)

	! Deep copy.  This overwrites dst with src

	class(vars_t), intent(inout) :: dst
	class(vars_t), intent(in)    :: src

	!********

	integer :: i

	!print *, 'starting vars_copy()'

	dst%scope = src%scope
	dst%scope_cap = src%scope_cap

	if (allocated(src%dicts)) then

		if (allocated(dst%dicts)) deallocate(dst%dicts)
		allocate(dst%dicts( size(src%dicts) ))

		do i = 1, size(src%dicts)
			if (allocated(src%dicts(i)%root)) then
				if (.not. allocated(dst%dicts(i)%root)) allocate(dst%dicts(i)%root)
				dst%dicts(i)%root = src%dicts(i)%root
			else if (allocated(dst%dicts(i)%root)) then
				deallocate(dst%dicts(i)%root)
			end if
		end do

	else if (allocated(dst%dicts)) then
		deallocate(dst%dicts)
	end if

	if (allocated(src%vals)) then
		if (allocated(dst%vals)) deallocate(dst%vals)
		allocate(dst%vals( size(src%vals) ))
		dst%vals = src%vals
	else if (allocated(dst%vals)) then
		deallocate(dst%vals)
	end if

	!print *, 'done vars_copy()'

end subroutine vars_copy

!===============================================================================

recursive subroutine struct_copy(dst, src)

	! Deep copy.  This overwrites dst with src

	class(struct_t), intent(inout) :: dst
	class(struct_t), intent(in)    :: src

	!********

	!print *, 'starting struct_copy()'

	dst%member_names = src%member_names
	dst%num_vars = src%num_vars
	dst%vars = src%vars

	!print *, 'done struct_copy()'

end subroutine struct_copy

!===============================================================================

recursive subroutine fn_copy(dst, src)

	! Deep copy.  This overwrites dst with src

	class(fn_t), intent(inout) :: dst
	class(fn_t), intent(in)    :: src

	!********

	!print *, 'starting fn_copy()'

	dst%type          = src%type
	dst%variadic_min  = src%variadic_min
	dst%variadic_type = src%variadic_type
	dst%param_names   = src%param_names
	dst%is_intr       = src%is_intr

	if (allocated(src%params)) then
		if (allocated(dst%params)) deallocate(dst%params)
		allocate(dst%params( size(src%params) ))
		dst%params = src%params
	else if (allocated(dst%params)) then
		deallocate(dst%params)
	end if

	if (allocated(src%node)) then
		if (.not. allocated(dst%node)) allocate(dst%node)
		dst%node = src%node
	else if (allocated(dst%node)) then
		deallocate(dst%node)
	end if

	!print *, 'done fn_copy()'

end subroutine fn_copy

!===============================================================================

recursive subroutine fn_ternary_tree_copy(dst, src)

	! Deep copy.  This overwrites dst with src.  If dst had keys that weren't in
	! source, they will be gone!
	!
	! This should be avoided for efficient compilation, but the interactive
	! interpreter uses it to backup and restore the variable dict for
	! partially-evaluated continuation lines

	class(fn_ternary_tree_node_t), intent(inout) :: dst
	class(fn_ternary_tree_node_t), intent(in)    :: src

	!********

	!print *, 'starting fn_ternary_tree_node_t()'

	dst%split_char = src%split_char

	dst%id_index = src%id_index

	if (allocated(src%val)) then
		if (.not. allocated(dst%val)) allocate(dst%val)
		dst%val = src%val
	! TODO: else deallocate?  Other tree copiers too
	end if

	if (allocated(src%left)) then
		if (.not. allocated(dst%left)) allocate(dst%left)
		dst%left = src%left
	end if

	if (allocated(src%mid)) then
		if (.not. allocated(dst%mid)) allocate(dst%mid)
		dst%mid = src%mid
	end if

	if (allocated(src%right)) then
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

	!print *, 'done fn_ternary_tree_node_t()'

end subroutine fn_ternary_tree_copy

!===============================================================================

function fn_search(dict, key, id_index, iostat) result(val)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(fns_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(fn_t) :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: i, io

	i = dict%scope

	val = fn_ternary_search(dict%dict%root, key, id_index, io)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		val = fn_ternary_search(dict%dict%root, key, id_index, io)
	end do

	if (present(iostat)) iostat = io

end function fn_search

!===============================================================================

! TODO: ternary tree insert/search fns for vars and fns could potentially be a
! separate translation unit

subroutine fn_insert(dict, key, val, id_index, iostat, overwrite)

	class(fns_t) :: dict
	character(len = *), intent(in) :: key
	type(fn_t), intent(in) :: val
	integer, intent(inout) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer :: i, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)

	!! num_fns is already incremented by caller
	!id_index = id_index + 1

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call fn_ternary_insert(dict%dict%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine fn_insert

!===============================================================================

recursive function syntax_node_str(node, indent) result(str_)

	! Convert tree to string in JSON-ish format.  Incomplete since I've added so
	! many new members

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str_

	!********

	character(len = :), allocatable :: indentl, kind, left, op, right, val, &
		type, identifier, block

	integer :: i

	indentl = ''
	if (present(indent)) indentl = indent

	kind = indentl//'    kind  = '//kind_name(node%kind)//line_feed

	left  = ''
	op    = ''
	right = ''
	val   = ''
	block = ''

	identifier = ''

	type  = indentl//'    type  = '//kind_name(node%val%type)//line_feed

	! FIXME: add str conversions for more recent kinds: condition, if_clause,
	! etc.

	if      (node%kind == binary_expr) then

		left  = indentl//'    left  = '//node%left %str(indentl//'    ') &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == fn_declaration) then
		val = indentl//'    body = '//node%body%str(indentl//'    ')//line_feed

	else if (node%kind == fn_call_expr) then
		val = indentl//'    id_index = '//str(node%id_index)//line_feed

	else if (node%kind == return_statement) then
		val = indentl//'    expr = '//node%right%str(indentl//'    ')//line_feed

	else if (node%kind == block_statement) then

		do i = 1, size(node%members)
			block = block // node%members(i)%str(indentl//'    ')
		end do
		block = block // line_feed

	else if (node%kind == translation_unit) then

		type = ''
		do i = 1, size(node%members)
			block = block // node%members(i)%str(indentl//'    ')
		end do
		block = block // line_feed

	else if (node%kind == assignment_expr) then

		identifier  = indentl//'    identifier = '//node%identifier%text &
				//line_feed

		op    = indentl//'    op    = '//node%op%text//line_feed

		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == unary_expr) then

		op    = indentl//'    op    = '//node%op%text//line_feed
		right = indentl//'    right = '//node%right%str(indentl//'    ') &
				//line_feed

	else if (node%kind == literal_expr) then
		val   = indentl//'    val   = '//node%val%to_str()//line_feed
	end if

	str_ = line_feed// &
		indentl//'{'//line_feed// &
			kind       // &
			type       // &
			identifier // &
			left       // &
			op         // &
			right      // &
			val        // &
			block      // &
		indentl//'}'

end function syntax_node_str

!===============================================================================

subroutine log_diagnostics(node, ou)

	class(syntax_node_t), intent(in) :: node
	integer, optional   , intent(in) :: ou

	!********

	character(len = :), allocatable :: s
	integer :: i, oul, nlog, nomit

	oul = output_unit
	if (present(ou)) oul = ou

	! This is nice for unclosed parens instead of having a huge number of
	! cascading errors.  4 seems like a good default bc 4 errors fit on my
	! laptop screen

	if (maxerr > 0) then
		nlog = min(node%diagnostics%len_, maxerr)
	else
		nlog = node%diagnostics%len_
	end if

	do i = 1, nlog
		write(oul, '(a)') node%diagnostics%v(i)%s
		write(oul,*)
	end do

	nomit = node%diagnostics%len_ - nlog
	if (nomit > 0) then
		if (nomit > 1) then
			s = "s"
		else
			s = ""
		end if
		write(oul, *) fg_bold//'[[ '//str(nomit) &
			//' more error'//s//' omitted ]]'//color_reset
	end if

end subroutine log_diagnostics

!===============================================================================

recursive subroutine syntax_node_vector_copy(dst, src)

	class(syntax_node_vector_t), intent(inout) :: dst
	class(syntax_node_vector_t), intent(in)    :: src

	!********

	integer :: i

	dst%len_ = src%len_
	dst%cap = src%cap

	if (allocated(dst%v)) deallocate(dst%v)
	allocate(dst%v( size(src%v) ))
	do i = 1 , size(src%v)
		dst%v(i) = src%v(i)
	end do

end subroutine syntax_node_vector_copy

!===============================================================================

recursive subroutine syntax_node_copy(dst, src)

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my node type, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading

	class(syntax_node_t), intent(inout) :: dst
	class(syntax_node_t), intent(in)    :: src

	!********

	if (debug > 3) print *, 'starting syntax_node_copy()'

	!print *, 'src%kind = ', src%kind
	!print *, 'dst%kind = ', dst%kind

	dst%kind = src%kind
	dst%op   = src%op

	dst%val  = src%val
	!dst%val%sca%file_     = src%val%sca%file_
	!dst%val%sca%file_%eof = src%val%sca%file_%eof

	dst%identifier = src%identifier
	dst%id_index   = src%id_index
	dst%num_locs   = src%num_locs
	dst%is_loc     = src%is_loc

	if (allocated(src%struct_name)) then
		dst%struct_name = src%struct_name
	else if (allocated(dst%struct_name)) then
		deallocate(dst%struct_name)
	end if

	dst%expecting       = src%expecting
	dst%first_expecting = src%first_expecting

	!print *, 'allocated(src%first_expected) = ', allocated(src%first_expected)
	if (allocated(src%first_expected)) then
		dst%first_expected = src%first_expected
	else if (allocated(dst%first_expected)) then
		deallocate(dst%first_expected)
	end if

	dst%diagnostics    = src%diagnostics

	dst%is_empty    = src%is_empty

	dst%sub_kind = src%sub_kind

	!if (allocated(src%val)) then
	!	if (.not. allocated(dst%val)) allocate(dst%val)
	!	dst%val = src%val
	!end if

	if (allocated(src%left)) then
		!if (debug > 1) print *, 'copy() left'
		if (.not. allocated(dst%left)) allocate(dst%left)
		!print *, 'allocated(dst%left) = ', allocated(dst%left)
		dst%left = src%left
		!call syntax_node_copy(dst%left, src%left)
	else if (allocated(dst%left)) then
		deallocate(dst%left)
	end if

	if (allocated(src%right)) then
		!if (debug > 1) print *, 'copy() right'
		if (.not. allocated(dst%right)) allocate(dst%right)
		!print *, 'allocated(dst%right) = ', allocated(dst%right)
		dst%right = src%right
		!call syntax_node_copy(dst%right, src%right)
	else if (allocated(dst%right)) then
		deallocate(dst%right)
	end if

	if (allocated(src%params)) then
		! Primitive int array.  No need to explicitly allocate
		dst%params = src%params
	else if (allocated(dst%params)) then
		deallocate(dst%params)
	end if

	if (allocated(src%is_ref)) then
		dst%is_ref = src%is_ref
	else if (allocated(dst%is_ref)) then
		deallocate(dst%is_ref)
	end if

	if (allocated(src%condition)) then
		if (.not. allocated(dst%condition)) allocate(dst%condition)
		dst%condition = src%condition
	else if (allocated(dst%condition)) then
		deallocate(dst%condition)
	end if

	if (allocated(src%body)) then
		if (.not. allocated(dst%body)) allocate(dst%body)
		dst%body = src%body
	else if (allocated(dst%body)) then
		deallocate(dst%body)
	end if

	if (allocated(src%array)) then
		if (.not. allocated(dst%array)) allocate(dst%array)
		dst%array = src%array
	else if (allocated(dst%array)) then
		deallocate(dst%array)
	end if

	if (allocated(src%lbound)) then
		if (.not. allocated(dst%lbound)) allocate(dst%lbound)
		dst%lbound = src%lbound
	else if (allocated(dst%lbound)) then
		deallocate(dst%lbound)
	end if

	if (allocated(src%ubound)) then
		if (.not. allocated(dst%ubound)) allocate(dst%ubound)
		dst%ubound = src%ubound
	else if (allocated(dst%ubound)) then
		deallocate(dst%ubound)
	end if

	if (allocated(src%step)) then
		if (.not. allocated(dst%step)) allocate(dst%step)
		dst%step = src%step
	else if (allocated(dst%step)) then
		deallocate(dst%step)
	end if

	if (allocated(src%len_)) then
		if (.not. allocated(dst%len_)) allocate(dst%len_)
		dst%len_ = src%len_
	else if (allocated(dst%len_)) then
		deallocate(dst%len_)
	end if

	if (allocated(src%rank)) then
		if (.not. allocated(dst%rank)) allocate(dst%rank)
		dst%rank = src%rank
	else if (allocated(dst%rank)) then
		deallocate(dst%rank)
	end if

	if (allocated(src%elems)) then
		call syntax_nodes_copy(dst%elems, src%elems)
	else if (allocated(dst%elems)) then
		deallocate(dst%elems)
	end if

	if (allocated(src%lsubscripts)) then
		call syntax_nodes_copy(dst%lsubscripts, src%lsubscripts)
	else if (allocated(dst%lsubscripts)) then
		deallocate(dst%lsubscripts)
	end if

	if (allocated(src%usubscripts)) then
		call syntax_nodes_copy(dst%usubscripts, src%usubscripts)
	else if (allocated(dst%usubscripts)) then
		deallocate(dst%usubscripts)
	end if

	if (allocated(src%ssubscripts)) then
		call syntax_nodes_copy(dst%ssubscripts, src%ssubscripts)
	else if (allocated(dst%ssubscripts)) then
		deallocate(dst%ssubscripts)
	end if

	if (allocated(src%args)) then
		call syntax_nodes_copy(dst%args, src%args)
	else if (allocated(dst%args)) then
		deallocate(dst%args)
	end if

	if (allocated(src%size)) then
		call syntax_nodes_copy(dst%size, src%size)
	else if (allocated(dst%size)) then
		deallocate(dst%size)
	end if

	if (allocated(src%if_clause)) then
		if (.not. allocated(dst%if_clause)) allocate(dst%if_clause)
		dst%if_clause = src%if_clause
	else if (allocated(dst%if_clause)) then
		deallocate(dst%if_clause)
	end if

	if (allocated(src%else_clause)) then
		if (.not. allocated(dst%else_clause)) allocate(dst%else_clause)
		dst%else_clause = src%else_clause
	else if (allocated(dst%else_clause)) then
		deallocate(dst%else_clause)
	end if

	if (allocated(src%members)) then
		!print *, 'copying members'
		call syntax_nodes_copy(dst%members, src%members)
	else if (allocated(dst%members)) then
		deallocate(dst%members)
	end if

	if (allocated(src%member)) then
		if (.not. allocated(dst%member)) allocate(dst%member)
		dst%member = src%member
	else if (allocated(dst%member)) then
		deallocate(dst%member)
	end if

	if (debug > 3) print *, 'done syntax_node_copy()'

end subroutine syntax_node_copy

!===============================================================================

recursive subroutine ternary_tree_copy(dst, src)

	! Deep copy.  This overwrites dst with src.  If dst had keys that weren't in
	! source, they will be gone!
	!
	! This should be avoided for efficient compilation, but the interactive
	! interpreter uses it to backup and restore the variable dict for
	! partially-evaluated continuation lines

	class(ternary_tree_node_t), intent(inout) :: dst
	class(ternary_tree_node_t), intent(in)    :: src

	!********

	!if (.not. allocated(dst)) allocate(dst)

	dst%split_char = src%split_char

	dst%id_index = src%id_index

	if (allocated(src%val)) then
		if (.not. allocated(dst%val)) allocate(dst%val)
		dst%val = src%val
	end if

	if (allocated(src%left)) then
		if (.not. allocated(dst%left)) allocate(dst%left)
		dst%left = src%left
	end if

	if (allocated(src%mid)) then
		if (.not. allocated(dst%mid)) allocate(dst%mid)
		dst%mid = src%mid
	end if

	if (allocated(src%right)) then
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

end subroutine ternary_tree_copy

!===============================================================================

subroutine var_insert(dict, key, val, id_index, iostat, overwrite)

	! There are a couple reasons for having this wrapper:
	!
	!   - dict is not allocatable, while dict%root is.  type-bound
	!     methods are not allowed for allocatable types
	!   - it's an abstraction away from the dict implementation.
	!     currently I'm using a ternary tree, but the dict could
	!     alternatively be implemented using another data structure like a trie
	!     or a radix tree
	!   - having an allocatable root is helpful both for the ternary
	!     insert/delete implementation and for moving the dict without
	!     copying in syntax_parse()

	class(vars_t) :: dict
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer :: i, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)
	!print *, 'val = ', val%to_str()

	! Note that this is different than the fn insert default
	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine var_insert

!===============================================================================

subroutine var_search(dict, key, id_index, iostat, val)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(value_t) :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: i, io

	i = dict%scope

	call ternary_search(dict%dicts(i)%root, key, id_index, io, val)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		call ternary_search(dict%dicts(i)%root, key, id_index, io, val)
	end do

	if (present(iostat)) iostat = io

end subroutine var_search

!===============================================================================

subroutine push_scope(dict)

	class(vars_t) :: dict

	!********

	integer :: i

	type(var_dict_t), allocatable :: tmp(:)

	!print *, "dict%scope = ", dict%scope

	if (dict%scope >= dict%scope_cap) then
		!! Grow dicts pointer array
		!print *, "Growing dict array"

		dict%scope_cap = dict%scope_cap * 2
		call move_alloc(dict%dicts, tmp)
		allocate(dict%dicts( dict%scope_cap ))

		do i = 1, dict%scope
			! The `root` member is also allocatable.  Moving its pointer is
			! inexpensive
			call move_alloc(tmp(i)%root, dict%dicts(i)%root)
		end do

		deallocate(tmp)
	end if

	dict%scope = dict%scope + 1

end subroutine push_scope

!===============================================================================

subroutine pop_scope(dict)

	class(vars_t) :: dict

	integer :: i

	!print *, 'starting pop_scope()'

	i = dict%scope

	! It's possible that a scope may not have any local vars, so its dict
	! is not allocated
	if (allocated(dict%dicts(i)%root)) then

		! Does this automatically deallocate children? (mid, left, right)  May
		! need recursive deep destructor
		deallocate(dict%dicts(i)%root)

	end if

	dict%scope = dict%scope - 1

	! The parser should catch an unexpected `}`
	if (dict%scope < 1) then
		write(*,*) err_int_prefix//'scope stack is empty'
		call internal_error()
	end if

end subroutine pop_scope

!===============================================================================

subroutine push_token(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(syntax_token_vector_t) :: vector
	type(syntax_token_t) :: val

	!********

	type(syntax_token_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_token

!===============================================================================

subroutine push_node(vector, val)

	class(syntax_node_vector_t) :: vector
	type(syntax_node_t) :: val

	!********

	type(syntax_node_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector ====================================='

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))

		!print *, 'copy 1'
		!!tmp(1: vector%cap) = vector%v
		do i = 1, vector%cap
			tmp(i) = vector%v(i)
		end do

		!print *, 'move'
		!!call move_alloc(tmp, vector%v)

		deallocate(vector%v)
		allocate(vector%v( tmp_cap ))

		! Unfortunately we have to copy TO tmp AND back FROM tmp.  I guess the
		! fact that each node itself has allocatable members creates invalid
		! references otherwise.

		!print *, 'copy 2'
		!!vector%v(1: vector%cap) = tmp(1: vector%cap)
		do i = 1, vector%cap
			vector%v(i) = tmp(i)
		end do

		vector%cap = tmp_cap

	end if

	!print *, 'set val'
	vector%v( vector%len_ ) = val
	!print *, 'done push_node'

end subroutine push_node

!===============================================================================

function new_token(kind, pos, text, val) result(token)

	integer :: kind, pos

	character(len = *) :: text

	type(value_t), optional :: val

	type(syntax_token_t) :: token

	token%kind = kind
	token%pos  = pos
	token%text = text

	if (present(val)) token%val  = val

end function new_token

!===============================================================================

integer function lookup_type(name, structs, struct) result(type)

	character(len = *), intent(in) :: name

	type(structs_t), intent(in) :: structs

	type(struct_t), intent(out) :: struct

	!********

	integer :: io, struct_id

	! Immo also has an "any" type.  Should I allow that?

	select case (name)
		case ("bool")
			type = bool_type

		case ("f32")
			type = f32_type
		case ("f64")
			type = f64_type

		case ("i32")
			type = i32_type
		case ("i64")
			type = i64_type

		case ("str")
			type = str_type

		case default

			! TODO: this should be able to use %exists instead of %search,
			! possible minor perf boost
			call structs%search(name, struct_id, io, struct)
			!print *, "struct search io = ", io

			if (io == 0) then
				type = struct_type
				!print *, "struct num vars = ", struct%num_vars
			else
				type = unknown_type
			end if

	end select
	!print *, 'lookup_type = ', type

end function lookup_type

!===============================================================================

function new_syntax_node_vector() result(vector)

	type(syntax_node_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_node_vector

!===============================================================================

recursive subroutine syntax_nodes_copy(dst, src)

	! Array copy

	type(syntax_node_t), allocatable :: dst(:)
	type(syntax_node_t), intent(in)  :: src(:)

	!********

	integer :: i

	if (allocated(dst)) deallocate(dst)
	allocate(dst( size(src) ))
	do i = 1, size(src)
		dst(i) = src(i)
	end do

end subroutine syntax_nodes_copy

!===============================================================================

function new_syntax_token_vector() result(vector)

	type(syntax_token_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_token_vector

!===============================================================================

function new_literal_value(type, bool, i32, i64, f32, f64, str) result(val)

	integer, intent(in) :: type

	integer(kind = 4), intent(in), optional :: i32
	integer(kind = 8), intent(in), optional :: i64
	real   (kind = 4), intent(in), optional :: f32
	real   (kind = 8), intent(in), optional :: f64
	logical          , intent(in), optional :: bool
	character(len=*) , intent(in), optional :: str

	type(value_t) :: val

	val%type = type
	if (present(bool)) val%sca%bool  = bool
	if (present(f32 )) val%sca%f32   = f32
	if (present(f64 )) val%sca%f64   = f64
	if (present(i32 )) val%sca%i32   = i32
	if (present(i64 )) val%sca%i64   = i64
	if (present(str )) val%sca%str%s = str

end function new_literal_value

!===============================================================================

integer function get_keyword_kind(text) result(kind)

	character(len = *), intent(in) :: text

	! Here we start to depart from Fortran syntax (true, not .true.)
	select case (text)

		case ("true")
			kind = true_keyword

		case ("false")
			kind = false_keyword

		case ("not")
			kind = not_keyword

		case ("and")
			kind = and_keyword

		case ("or")
			kind = or_keyword

		case ("let")
			kind = let_keyword

		case ("if")
			kind = if_keyword

		case ("else")
			kind = else_keyword

		case ("for")
			kind = for_keyword

		case ("in")
			kind = in_keyword

		case ("while")
			kind = while_keyword

		case ("fn")
			kind = fn_keyword

		case ("struct")
			kind = struct_keyword

		case ("include")
			kind = include_keyword

		case ("return")
			kind = return_keyword

		case ("break")
			kind = break_keyword

		case ("continue")
			kind = continue_keyword

		case default
			kind = identifier_token

	end select

	!print *, 'get_keyword_kind = ', kind

end function get_keyword_kind

!===============================================================================

function new_declaration_expr(identifier, op, right) result(expr)

	! TODO: IMO this fn is overly abstracted.  It's only used once, so
	! just paste it their and delete the fn.  That will make it easier to
	! refactor and consolidate declaration_expr and assignment_expr parsing

	type(syntax_token_t), intent(in) :: identifier, op
	type(syntax_node_t) , intent(in) :: right

	type(syntax_node_t) :: expr

	!********

	expr%kind = let_expr

	allocate(expr%right)

	expr%identifier = identifier

	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent
	expr%val = right%val

	!expr%val%type = right%val%type
	!if (expr%val%type == array_type) then
	!	!print *, 'new_declaration_expr array_type'
	!	allocate(expr%val%array)
	!	expr%val%array = right%val%array
	!end if

end function new_declaration_expr

!===============================================================================

logical function is_assignment_op(op)

	! Is the operator some type of assignment operator, either regular or
	! compound (augmented)?

	integer, intent(in) :: op

	is_assignment_op = any(op == [equals_token, plus_equals_token, &
		minus_equals_token, star_equals_token, slash_equals_token, &
		sstar_equals_token, percent_equals_token, &
		amp_equals_token, pipe_equals_token, caret_equals_token, &
		lless_equals_token, ggreater_equals_token])

end function is_assignment_op

!===============================================================================

recursive subroutine ternary_search(node, key, id_index, iostat, val)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(value_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	iostat = exit_success

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		call ternary_search(node%left , key, id_index, iostat, val)
		return
	else if (k > node%split_char) then
		call ternary_search(node%right, key, id_index, iostat, val)
		return
	else if (len(ey) > 0) then
		call ternary_search(node%mid  , ey, id_index, iostat, val)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		return
	end if

	val      = node%val
	id_index = node%id_index

	!print *, 'done ternary_search'
	!print *, ''

end subroutine ternary_search

!===============================================================================

logical function is_binary_op_allowed(left, op, right, left_arr, right_arr) &
		result(allowed)

	! Is an operation allowed with the types of operator op and left/right
	! operands?

	integer, intent(in) :: left, op, right
	integer, intent(in), optional :: left_arr, right_arr

	!print *, 'left, right = ', left, right

	!! This dynamic variable typing can be useful for testing
	!allowed = .true.
	!return

	allowed = .false.

	if (left == unknown_type .or. right == unknown_type) then
		! Stop cascading errors
		allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, plus_equals_token)
			! the + operator works on numbers and strings

			if (left == array_type .and. right == array_type) then

				! Would recursion help for arrays here?  It seems like it
				! wouldn't reduce very many LOC

				! TODO: should vec str + scalar str be allowed?

				allowed = &
					(is_num_type(left_arr) .and. is_num_type(right_arr))

			else if (left == array_type) then
				allowed = &
					(is_num_type(left_arr) .and. is_num_type(right))

			else if (right == array_type) then
				allowed = &
					(is_num_type(left) .and. is_num_type(right_arr))

			else
				allowed = &
					(is_num_type(left) .and. is_num_type(right)) .or. &
					(left == str_type  .and. right == str_type)

			end if

		case (minus_token, star_token, sstar_token, slash_token, &
			minus_equals_token, star_equals_token, slash_equals_token, &
			sstar_equals_token, percent_token, percent_equals_token, &
			greater_token, less_token, greater_equals_token, &
			less_equals_token)
			! these operators work on numbers but not strings

			if (left == array_type .and. right == array_type) then
				allowed = is_num_type(left_arr) .and. is_num_type(right_arr)
			else if (left == array_type) then
				allowed = is_num_type(left_arr) .and. is_num_type(right)
			else if (right == array_type) then
				allowed = is_num_type(left) .and. is_num_type(right_arr)
			else
				allowed = is_num_type(left) .and. is_num_type(right)
			end if

		case ( &
				lless_token, ggreater_token, &
				lless_equals_token, ggreater_equals_token)

			! Bitwise shift operators work on any combination of ints

			if (left == array_type .and. right == array_type) then
				allowed = is_int_type(left_arr) .and. is_int_type(right_arr)
			else if (left == array_type) then
				allowed = is_int_type(left_arr) .and. is_int_type(right)
			else if (right == array_type) then
				allowed = is_int_type(left) .and. is_int_type(right_arr)
			else
				allowed = is_int_type(left) .and. is_int_type(right)
			end if

		case ( &
				caret_token, pipe_token, amp_token, &
				caret_equals_token, pipe_equals_token, amp_equals_token)

			! Other bitwise binary operators (besides shift) only work on ints
			! of matching sizes (both 32 or 64 bit)

			if (left == array_type .and. right == array_type) then
				allowed = is_int_type(left_arr) .and. left_arr == right_arr
			else if (left == array_type) then
				allowed = is_int_type(left_arr) .and. left_arr == right
			else if (right == array_type) then
				allowed = is_int_type(left) .and. left == right_arr
			else
				allowed = is_int_type(left) .and. left == right
			end if

		case (and_keyword, or_keyword)

			if (left == array_type .and. right == array_type) then
				allowed = left_arr == bool_type .and. right_arr == bool_type

			else if (left  == array_type) then
				allowed = left_arr == bool_type .and. right == bool_type

			else if (right == array_type) then
				allowed = left == bool_type .and. right_arr == bool_type

			else
				allowed = left == bool_type .and. right == bool_type

			end if

		case (equals_token)

			! `array = scalar` is allowed but `scalar = array` is not

			if (left == array_type) then
				allowed = &
					(is_float_type(left_arr) .and. is_float_type(right)) .or. &
					(is_int_type  (left_arr) .and. is_int_type  (right)) .or. &
					(left_arr == right) .or. (left == right)

			else
				allowed = &
					(is_float_type(left) .and. is_float_type(right)) .or. &
					(is_int_type  (left) .and. is_int_type  (right)) .or. &
					(left == right)

			end if

		case (eequals_token, bang_equals_token)

			if (left == file_type .or. right == file_type) then
				allowed = .false.
				return
			end if

			! TODO: allow and then implement comparisons on mixed float types
			if (left == array_type .and. right == array_type) then
				allowed = &
					(is_int_type(left_arr) .and. is_int_type(right_arr)) .or. &
					(left_arr == right_arr)

			else if (left  == array_type) then
				allowed = &
					(is_int_type(left_arr) .and. is_int_type(right)) .or. &
					(left_arr == right)

			else if (right == array_type) then
				allowed = &
					(is_int_type(left) .and. is_int_type(right_arr)) .or. &
					(left == right_arr)

			else

				! Fortran allows comparing ints and floats for strict equality, e.g.
				! 1 == 1.0 is indeed true.  I'm not sure if I like that
				allowed = &
					(is_int_type(left) .and. is_int_type(right)) .or. &
					(left == right)

			end if

	end select

end function is_binary_op_allowed

!===============================================================================

logical function is_unary_op_allowed(op, right, right_arr)

	! Is a unary operation allowed with kinds operator op and right operand?

	integer, intent(in) :: op, right, right_arr

	is_unary_op_allowed = .false.

	if (right == unknown_type) then
		! Stop cascading errors
		is_unary_op_allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, minus_token)
			if (right == array_type) then
				is_unary_op_allowed = is_num_type(right_arr)
			else
				is_unary_op_allowed = is_num_type(right)
			end if

		case (bang_token)
			if (right == array_type) then
				is_unary_op_allowed = is_int_type(right_arr)
			else
				is_unary_op_allowed = is_int_type(right)
			end if

		case (not_keyword)
			if (right == array_type) then
				is_unary_op_allowed = right_arr == bool_type
			else
				is_unary_op_allowed = right == bool_type
			end if

	end select

end function is_unary_op_allowed

!===============================================================================

integer function get_unary_op_prec(kind) result(prec)

	! Get unary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (plus_token, minus_token, not_keyword, bang_token)
			! arithmetic +, arithmetic -, logical not, bitwise not
			prec = 12

		case default
			prec = 0

	end select

end function get_unary_op_prec

!===============================================================================

integer function get_binary_op_prec(kind) result(prec)

	! Get binary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		! Syntran operator precedence is closest to rust:
		!
		!     https://doc.rust-lang.org/reference/expressions.html
		!
		! The exception is that ordering comparisons <, >, <=, and >= have
		! higher precedence than (in)equality comparisons == and !=.  In rust,
		! all comparisons have the same precedence.
		!
		! This is somewhat similar to C, except for bitwise and `&`, bitwise or
		! `|`, and bitwise xor `^`, which have higher precedence here (and in
		! rust) than comparisons.  The fact that C works this way could be
		! considered a poor design, but it is due to historical reasons
		! predating even C, according to Dennis Ritchie:
		!
		!    http://cm.bell-labs.co/who/dmr/chist.html
		!
		! C (and C++) precedence:
		!
		!    https://en.cppreference.com/w/c/language/operator_precedence
		!
		! Note that here, a higher `prec` int return value means higher
		! precedence, while the C++ ref is the opposite numerically (but the
		! same top to bottom)

		!********

		! FIXME: increment the unary operator precedence in the fn above after
		! increasing the max binary precedence
		case (sstar_token)
			prec = 11

		case (star_token, slash_token, percent_token)
			prec = 10

		case (plus_token, minus_token)
			prec = 9

		case (lless_token, ggreater_token) ! `<<`, `>>`
			prec = 8

		case (amp_token) ! `&`
			prec = 7

		case (caret_token) ! `^`, aka circumflex, hat
			prec = 6

		case (pipe_token) ! `|`
			prec = 5

		case (less_token, less_equals_token, &
				greater_token, greater_equals_token)
			prec = 4

		case (eequals_token, bang_equals_token)
			prec = 3

		case (and_keyword)  ! `and` (logical, not bitwise)
			prec = 2

		case (or_keyword)  ! `or` (logical)
			prec = 1

		case default
			prec = 0

	end select
	!print *, "prec = ", prec

end function get_binary_op_prec

!===============================================================================

logical function is_num_type(type)

	integer, intent(in) :: type

	is_num_type = any(type == [i32_type, i64_type, f32_type, f64_type])

end function is_num_type

!===============================================================================

logical function is_int_type(type)

	integer, intent(in) :: type

	is_int_type = any(type == [i32_type, i64_type])

end function is_int_type

!===============================================================================

logical function is_float_type(type)

	integer, intent(in) :: type

	is_float_type = any(type == [f32_type, f64_type])

end function is_float_type

!===============================================================================

function new_name_expr(identifier, val) result(expr)

	type(syntax_token_t), intent(in) :: identifier
	type(syntax_node_t) :: expr
	type(value_t) :: val

	expr%kind = name_expr
	expr%identifier = identifier
	expr%val = val

	!if (expr%val%type == array_type) then
	!	!if (.not. allocated(expr%val%array)) allocate(expr%val%array)
	!	allocate(expr%val%array)
	!	!expr%val%array = right%val%array
	!	expr%val%array = val%array
	!end if

end function new_name_expr

!===============================================================================

function new_binary_expr(left, op, right) result(expr)

	type(syntax_node_t) , intent(in) :: left, right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	integer :: larrtype, rarrtype, type_, ltype, rtype

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left  = ', left %str()
	if (debug > 1) print *, 'op    = ', op%text
	if (debug > 1) print *, 'right = ', right%str()

	expr%kind = binary_expr

	allocate(expr%left)
	allocate(expr%right)

	expr%left  = left
	expr%op    = op
	expr%right = right

	!print *, 'left type  = ', kind_name(left%val%type)

	larrtype = unknown_type
	rarrtype = unknown_type
	if (left %val%type == array_type) larrtype = left %val%array%type
	if (right%val%type == array_type) rarrtype = right%val%array%type

	!print *, 'larrtype = ', kind_name(larrtype)

	ltype = left%val%type
	rtype = right%val%type

	! Pass the result value type up the tree for type checking in parent
	type_ = get_binary_op_kind(ltype, op%kind, rtype, &
		larrtype, rarrtype)
	!print *, 'type_ = ', kind_name(type_)

	if (any(type_ == [bool_array_type, f32_array_type, f64_array_type, &
		i32_array_type, i64_array_type, str_array_type])) then

		allocate(expr%val%array)

		expr%val%array%type = array_to_scalar_type(type_)
		if (left%val%type == array_type) then
			expr%val%array%rank = left %val%array%rank
		else
			expr%val%array%rank = right%val%array%rank
		end if

		expr%val%type = array_type

	! TODO: other array sub types.  Maybe make a mold_val() helper fn similar to
	! mold() (for arrays)

	else
		expr%val%type = type_

	end if

	! TODO: array subtype if subscripted?  I think parse_primary_expr should
	! already set the subtype when subscripts are present

	if (debug > 1) print *, 'new_binary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_binary_expr'

end function new_binary_expr

!===============================================================================

function new_unary_expr(op, right) result(expr)

	type(syntax_node_t) , intent(in) :: right
	type(syntax_token_t), intent(in) :: op

	type(syntax_node_t) :: expr

	!********

	if (debug > 1) print *, 'new_unary_expr'

	expr%kind = unary_expr

	allocate(expr%right)

	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent.  IIRC
	! all unary operators result in the same type as their operand, hence there
	! is a get_binary_op_kind() fn but no get_unary_op_kind() fn

	expr%val = right%val
	!expr%val%type = right%val%type

	if (debug > 1) print *, 'new_unary_expr = ', expr%str()
	if (debug > 1) print *, 'done new_unary_expr'

end function new_unary_expr

!===============================================================================

recursive integer function get_binary_op_kind( &
		left, op, right, &
		left_arr, right_arr &
		) &
		result(kind_)

	! Return the resulting type yielded by operator op on operands left and
	! right

	integer, intent(in) :: left, op, right
	integer, intent(in) :: left_arr, right_arr

	select case (op)
	case ( &
			eequals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)
		!print *, 'bool_type'

		! Comparison operations can take 2 numbers, but always return a bool of
		! some rank

		if (left == array_type .or. right == array_type) then
			kind_ = bool_array_type
		else
			kind_ = bool_type
		end if

	case (lless_token, ggreater_token)
		! Bitwise shifts return the left operand's type for scalars
		if (left == array_type .or. right == array_type) then

			! This logic could be refactored, could probably lose an indentation
			! level
			if (left == array_type) then
				! Left array, right scalar or array
				if (left_arr == i32_type) then
					kind_ = i32_array_type
				else
					kind_ = i64_array_type
				end if
			else
				! Left scalar, right array
				if (left == i32_type) then
					kind_ = i32_array_type
				else
					kind_ = i64_array_type
				end if
			end if

		else
			! All scalars
			kind_ = left
		end if

	case default
		!print *, 'default'

		! Other operations return the same type as their operands if they match,
		! or cast "up" to the type of the operand with the greatest range or
		! precision
		!
		! FIXME: i64, f64, etc.

		kind_ = unknown_type

		if (left == array_type .and. right == array_type) then
			kind_ = get_binary_op_kind(left_arr, op, right_arr, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else if (left == array_type) then
			kind_ = get_binary_op_kind(left_arr, op, right, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else if (right == array_type) then
			kind_ = get_binary_op_kind(left, op, right_arr, unknown_type, unknown_type)
			kind_ = scalar_to_array_type(kind_)

		else
			! Default scalar case (no arrays)

			if (left == right) then
				kind_ = left

			else if (left == f64_type .or. right == f64_type) then
				! int + float casts to float, f32 + f64 casts to f64
				!
				! Order matters compared to next condition branch!
				kind_ = f64_type

			else if (left == f32_type .or. right == f32_type) then
				! int + float casts to float
				kind_ = f32_type

			else if ( &
				(left  == i64_type .and. is_int_type(right)) .or. &
				(right == i64_type .and. is_int_type(left ))) then

				! i32+i64 and i64+i32 cast to i64
				kind_ = i64_type

			end if
		end if
	end select

end function get_binary_op_kind

!===============================================================================

function scalar_to_array_type(scalar_type_) result(array_type_)

	! Convert a scalar type to its corresponding array type

	integer, intent(in) :: scalar_type_
	integer :: array_type_

	select case (scalar_type_)
	case (bool_type)
		array_type_ = bool_array_type

	case (f32_type)
		array_type_ = f32_array_type

	case (f64_type)
		array_type_ = f64_array_type

	case (i32_type)
		array_type_ = i32_array_type

	case (i64_type)
		array_type_ = i64_array_type

	case (str_type)
		array_type_ = str_array_type

	! TODO: file_type?

	case default
		array_type_ = unknown_type

	end select

end function scalar_to_array_type

!===============================================================================

function array_to_scalar_type(array_type_) result(scalar_type_)

	! Convert an array type to its corresponding scalar type

	integer, intent(in) :: array_type_
	integer :: scalar_type_

	select case (array_type_)
	case (bool_array_type)
		scalar_type_ = bool_type

	case (f32_array_type)
		scalar_type_ = f32_type

	case (f64_array_type)
		scalar_type_ = f64_type

	case (i32_array_type)
		scalar_type_ = i32_type

	case (i64_array_type)
		scalar_type_ = i64_type

	case (str_array_type)
		scalar_type_ = str_type

	! TODO: file_type?

	case default
		scalar_type_ = unknown_type

	end select

end function array_to_scalar_type

!===============================================================================

function new_bool(bool) result(expr)

	logical, intent(in) :: bool
	type(syntax_node_t) :: expr

	! The expression node is a generic literal expression, while its child val
	! member indicates the specific type (e.g. bool_type or i32_type)
	expr%kind = literal_expr
	expr%val = new_literal_value(bool_type, bool = bool)

end function new_bool

!********

function new_f32(f32) result(expr)

	real(kind = 4), intent(in) :: f32
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(f32_type, f32 = f32)

end function new_f32

!********

function new_f64(f64) result(expr)

	real(kind = 8), intent(in) :: f64
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(f64_type, f64 = f64)

end function new_f64

!********

function new_i32(i32) result(expr)

	integer(kind = 4), intent(in) :: i32
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(i32_type, i32 = i32)

end function new_i32

!********

function new_i64(i64) result(expr)

	integer(kind = 8), intent(in) :: i64
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(i64_type, i64 = i64)

end function new_i64

!********

function new_str(str) result(expr)

	character(len = *), intent(in) :: str
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(str_type, str = str)

end function new_str

!===============================================================================

recursive function fn_ternary_search(node, key, id_index, iostat) result(val)

	type(fn_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(fn_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	iostat = exit_success

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		val = fn_ternary_search(node%left , key, id_index, iostat)
		return
	else if (k > node%split_char) then
		val = fn_ternary_search(node%right, key, id_index, iostat)
		return
	else if (len(ey) > 0) then
		val = fn_ternary_search(node%mid  , ey, id_index, iostat)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		return
	end if

	!allocate(val)
	val      = node%val
	id_index = node%id_index

	!print *, 'done fn_ternary_search'
	!print *, ''

end function fn_ternary_search

!===============================================================================

recursive subroutine fn_ternary_insert(node, key, val, id_index, iostat, overwrite)

	type(fn_ternary_tree_node_t), intent(inout), allocatable :: node
	character(len = *), intent(in) :: key
	type(fn_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out) :: iostat
	logical, intent(in) :: overwrite

	!********

	character :: k
	character(len = :), allocatable :: ey

	iostat = exit_success

	!print *, 'inserting key ', quote(key)

	! key == k//ey.  Get it? :)
	k   = key(1:1)
	 ey = key(2:)

	if (.not. allocated(node)) then
		!print *, 'allocate'
		allocate(node)
		node%split_char = k
	else if (k < node%split_char) then
		!print *, 'left'
		call fn_ternary_insert(node%left , key, val, id_index, iostat, overwrite)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call fn_ternary_insert(node%right, key, val, id_index, iostat, overwrite)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call fn_ternary_insert(node%mid  , ey, val, id_index, iostat, overwrite)
		return
	end if

	! node%val doesn't really need to be declared as allocatable (it's
	! a scalar anyway), but it's just a convenient way to check if
	! a duplicate key has already been inserted or not.  We could add
	! a separate logical member to node for this instead if needed

	! This is not necessarily a failure unless we don't want to overwrite.  In
	! the evaluator, we will insert values for vars which have already been
	! declared
	if (allocated(node%val) .and. .not. overwrite) then
		!print *, 'key already inserted'
		iostat = exit_failure
		return
	end if

	!if (.not. allocated(node%val)) allocate(node%val)
	if (allocated(node%val)) deallocate(node%val)
	allocate(node%val)

	node%val      = val
	node%id_index = id_index

	!print *, "inserted index ", id_index
	!print *, 'done inserting'
	!print *, ''

end subroutine fn_ternary_insert

!===============================================================================

recursive subroutine ternary_insert(node, key, val, id_index, iostat, overwrite)

	type(ternary_tree_node_t), intent(inout), allocatable :: node
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out) :: iostat
	logical, intent(in) :: overwrite

	!********

	character :: k
	character(len = :), allocatable :: ey

	iostat = exit_success

	!print *, 'inserting key ', quote(key)
	!print *, 'len(key) = ', len(key)
	!print *, 'iachar = ', iachar(key(1:1))

	! This should be unreachable
	if (len(key) <= 0) then
		!print *, 'len <= 0, return early'
		return
	end if

	! key == k//ey.  Get it? :)
	k   = key(1:1)
	 ey = key(2:)

	if (.not. allocated(node)) then
		!print *, 'allocate'
		allocate(node)
		node%split_char = k
	else if (k < node%split_char) then
		!print *, 'left'
		call ternary_insert(node%left , key, val, id_index, iostat, overwrite)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call ternary_insert(node%right, key, val, id_index, iostat, overwrite)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call ternary_insert(node%mid  , ey, val, id_index, iostat, overwrite)
		return
	end if

	! node%val doesn't really need to be declared as allocatable (it's
	! a scalar anyway), but it's just a convenient way to check if
	! a duplicate key has already been inserted or not.  We could add
	! a separate logical member to node for this instead if needed

	! This is not necessarily a failure unless we don't want to overwrite.  In
	! the evaluator, we will insert values for vars which have already been
	! declared
	if (allocated(node%val) .and. .not. overwrite) then
		!print *, 'key already inserted'
		iostat = exit_failure
		return
	end if

	if (.not. allocated(node%val)) allocate(node%val)
	node%val      = val
	node%id_index = id_index

	!print *, 'done inserting'
	!print *, ''

end subroutine ternary_insert

!===============================================================================

recursive function struct_ternary_exists(node, key) result(exists)

	type(struct_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	logical :: exists

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	exists = .false.

	! Search key not found
	if (.not. allocated(node)) return

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		exists = struct_ternary_exists(node%left , key)
		return
	else if (k > node%split_char) then
		exists = struct_ternary_exists(node%right, key)
		return
	else if (len(ey) > 0) then
		exists = struct_ternary_exists(node%mid  ,  ey)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		exists = .false.
		return
	end if

	exists = .true.

	!print *, 'done struct_ternary_exists'
	!print *, ''

end function struct_ternary_exists

!===============================================================================

recursive subroutine struct_ternary_search(node, key, id_index, iostat, val)

	type(struct_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(struct_t) :: val  ! intent inout?

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	iostat = exit_success

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		call struct_ternary_search(node%left , key, id_index, iostat, val)
		!print *, "return left"
		return
	else if (k > node%split_char) then
		call struct_ternary_search(node%right, key, id_index, iostat, val)
		!print *, "return right"
		return
	else if (len(ey) > 0) then
		call struct_ternary_search(node%mid  , ey, id_index, iostat, val)
		!print *, "return mid"
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		return
	end if

	!allocate(val)
	val      = node%val
	!val%vars = node%val%vars
	id_index = node%id_index
	!val%members = node%val%members

	!print *, 'done struct_ternary_search'
	!print *, ''

end subroutine struct_ternary_search

!===============================================================================

!recursive subroutine ternary_tree_final(node)
!	type(ternary_tree_node_t), intent(inout), allocatable :: node
!		!type(ternary_tree_node_t), allocatable :: left, mid, right
!		!type(value_t), allocatable :: val
!
!	if (.not. allocated(node)) return
!
!	!if (allocated(node%val)) deallocate(node%val)
!
!	if (allocated(node%left )) then
!		call ternary_tree_final(node%left )
!		deallocate(node%left)
!	end if
!	if (allocated(node%mid  )) then
!		call ternary_tree_final(node%mid  )
!		deallocate(node%mid)
!	end if
!	if (allocated(node%right)) then
!		call ternary_tree_final(node%right)
!		deallocate(node%right)
!	end if
!
!end subroutine ternary_tree_final

!===============================================================================

recursive subroutine struct_ternary_insert(node, key, val, id_index, iostat, overwrite)

	type(struct_ternary_tree_node_t), intent(inout), allocatable :: node
	character(len = *), intent(in) :: key
	type(struct_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out) :: iostat
	logical, intent(in) :: overwrite

	!********

	character :: k
	character(len = :), allocatable :: ey

	iostat = exit_success

	!print *, 'inserting key ', quote(key)

	! key == k//ey.  Get it? :)
	k   = key(1:1)
	 ey = key(2:)

	if (.not. allocated(node)) then
		!print *, 'allocate'
		allocate(node)
		node%split_char = k
	else if (k < node%split_char) then
		!print *, 'left'
		call struct_ternary_insert(node%left , key, val, id_index, iostat, overwrite)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call struct_ternary_insert(node%right, key, val, id_index, iostat, overwrite)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call struct_ternary_insert(node%mid  , ey, val, id_index, iostat, overwrite)
		return
	end if

	! node%val doesn't really need to be declared as allocatable (it's
	! a scalar anyway), but it's just a convenient way to check if
	! a duplicate key has already been inserted or not.  We could add
	! a separate logical member to node for this instead if needed

	! This is not necessarily a failure unless we don't want to overwrite.  In
	! the evaluator, we will insert values for vars which have already been
	! declared
	if (allocated(node%val) .and. .not. overwrite) then
		!print *, 'key already inserted'
		iostat = exit_failure
		return
	end if

	if (.not. allocated(node%val)) allocate(node%val)
	node%val      = val
	!node%val%vars = val%vars
	node%id_index = id_index
	!node%val%members = val%members

	!print *, 'done inserting'
	!print *, ''

end subroutine struct_ternary_insert

!===============================================================================

recursive subroutine struct_ternary_tree_copy(dst, src)

	! Deep copy.  This overwrites dst with src.  If dst had keys that weren't in
	! source, they will be gone!
	!
	! This should be avoided for efficient compilation, but the interactive
	! interpreter uses it to backup and restore the variable dict for
	! partially-evaluated continuation lines

	class(struct_ternary_tree_node_t), intent(inout) :: dst
	class(struct_ternary_tree_node_t), intent(in)    :: src

	!********

	!print *, 'starting struct_ternary_tree_node_t()'

	dst%split_char = src%split_char

	dst%id_index = src%id_index

	if (allocated(src%val)) then
		if (.not. allocated(dst%val)) allocate(dst%val)
		dst%val = src%val
	end if

	if (allocated(src%left)) then
		if (.not. allocated(dst%left)) allocate(dst%left)
		dst%left = src%left
	end if

	if (allocated(src%mid)) then
		if (.not. allocated(dst%mid)) allocate(dst%mid)
		dst%mid = src%mid
	end if

	if (allocated(src%right)) then
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

	!print *, 'done struct_ternary_tree_node_t()'

end subroutine struct_ternary_tree_copy

!===============================================================================

!recursive subroutine struct_ternary_tree_final(src)
!!subroutine struct_ternary_tree_final(src)
!
!	!class(struct_ternary_tree_node_t) :: src
!	type(struct_ternary_tree_node_t) :: src
!
!	if (allocated(src%val)) then
!		deallocate(src%val)
!	end if
!
!	if (allocated(src%left)) then
!		call struct_ternary_tree_final(src%left)
!	end if
!
!	if (allocated(src%mid)) then
!		call struct_ternary_tree_final(src%mid)
!	end if
!
!	if (allocated(src%right)) then
!		call struct_ternary_tree_final(src%right)
!	end if
!
!end subroutine struct_ternary_tree_final

!===============================================================================

subroutine struct_insert(dict, key, val, id_index, iostat, overwrite)

	class(structs_t) :: dict
	character(len = *), intent(in) :: key
	type(struct_t), intent(in) :: val
	integer, intent(inout) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer :: io
	logical :: overwritel

	!print *, 'inserting ', quote(key)
	id_index = id_index + 1

	! Note that this is different than the fn insert default.  Re-declared
	! structs are caught in the caller (in parse_struct_declaration())
	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	call struct_ternary_insert(dict%dict%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine struct_insert

!===============================================================================

function struct_exists(dict, key) result(exists)

	! Check if a key exists, without copying an output val unlike
	! struct_search()

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	logical :: exists

	exists = struct_ternary_exists(dict%dict%root, key)

end function struct_exists

!===============================================================================

subroutine struct_search(dict, key, id_index, iostat, val)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(struct_t) :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: io

	!print *, "starting struct search"

	call struct_ternary_search(dict%dict%root, key, id_index, io, val)
	!print *, "io = ", io

	if (present(iostat)) iostat = io

end subroutine struct_search

!===============================================================================

function type_name(a) result(str_)
	! c.f. lookup_type() which is mostly the inverse of this
	type(value_t), intent(in) :: a
	character(len = :), allocatable :: str_, array_name

	if (a%type == struct_type) then
		str_ = a%struct_name
	else if (a%type == array_type) then

		if (a%array%type == struct_type) then
			array_name = a%struct_name
		else
			array_name = type_name_primitive(a%array%type)
		end if

		str_ = "["//array_name//"; "

		! Repeat ":, " appropriately
		str_ = str_//repeat(":, ", max(a%array%rank - 1, 0))
		str_ = str_//":]"

	else
		str_ = type_name_primitive(a%type)
	end if

end function type_name

!===============================================================================

function type_name_primitive(itype) result(str_)
	! c.f. lookup_type() which is mostly the inverse of this
	integer, intent(in) :: itype
	character(len = :), allocatable :: str_

	select case (itype)
	case (i32_type)
		str_ = "i32"
	case (i64_type)
		str_ = "i64"
	case (f32_type)
		str_ = "f32"
	case (f64_type)
		str_ = "f64"
	case (str_type)
		str_ = "str"
	case (bool_type)
		str_ = "bool"
	case (any_type)
		str_ = "any"
	case default
		str_ = "unknown"
	end select

end function type_name_primitive

!===============================================================================

integer function types_match(a, b) result(io)

	! Check if the type of value `a` matches value `b`. Arguments are not
	! transitive!  If `a` is of value any_type, enforcement is less strict.
	!
	! Numeric casting, e.g. i32 to f32, is not allowed.  Maybe we could add a
	! flag if some callers need to allow casting

	type(value_t), intent(in) :: a, b

	!****************

	io = TYPE_MATCH

	if (.not. (a%type == any_type .or. a%type == b%type)) then
		! Top-level type mismatch (e.g. f32 vs str)
		io = TYPE_MISMATCH
		return
	end if

	if (a%type == struct_type) then
		if (a%struct_name /= b%struct_name) then
			! Both are structs but different kinds of structs
			io = TYPE_STRUCT_MISMATCH
			return
		end if
	end if

	if (a%type == array_type) then

		if (.not. (a%array%type == any_type .or. a%array%type == b%array%type)) then
			! Both arrays but with different types of elements
			io = TYPE_ARRAY_MISMATCH
			return
		end if

		if (.not. (a%array%rank < 0 .or. a%array%rank == b%array%rank)) then
			! Both arrays but with different ranks (e.g. vector vs matrix)
			io = TYPE_RANK_MISMATCH
			return
		end if

		if (a%array%type == struct_type) then
			if (a%struct_name /= b%struct_name) then
				! Both are arrays of structs but different kinds of structs
				io = TYPE_ARRAY_STRUCT_MISMATCH
				return
			end if
		end if

	end if

end function types_match

!===============================================================================

end module syntran__types_m

!===============================================================================

