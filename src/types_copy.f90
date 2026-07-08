
!===============================================================================

submodule (syntran__types_m) syntran__types_copy

	implicit none

!===============================================================================

contains

!===============================================================================

recursive module subroutine syntax_token_copy(dst, src)

	! Deep copy.  syntax_token_t has no assignment(=) of its own, but its val
	! component (value_t) does.  Invoking that via `dst%val = src%val` here
	! would hit a gfortran defined-assignment code-gen bug that leaks val's
	! nested allocatable components, so value_copy() is called directly
	! instead of `=`

	class(syntax_token_t), intent(inout) :: dst
	class(syntax_token_t), intent(in)    :: src

	dst%kind  = src%kind
	call value_copy(dst%val, src%val)
	dst%pos   = src%pos
	dst%unit_ = src%unit_

	if (allocated(src%text)) then
		dst%text = src%text
	else if (allocated(dst%text)) then
		deallocate(dst%text)
	end if

end subroutine syntax_token_copy

!===============================================================================

recursive module subroutine vars_copy(dst, src)

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

recursive module subroutine struct_copy(dst, src)

	! Deep copy.  This overwrites dst with src

	class(struct_t), intent(inout) :: dst
	class(struct_t), intent(in)    :: src

	!********

	!print *, 'starting struct_copy()'

	dst%member_names = src%member_names
	dst%num_vars = src%num_vars
	dst%vars = src%vars

	if (allocated(src%cookie)) then
		dst%cookie = src%cookie
	else if (allocated(dst%cookie)) then
		deallocate(dst%cookie)
	end if

	!print *, 'done struct_copy()'

end subroutine struct_copy

!===============================================================================

recursive module subroutine fn_copy(dst, src)

	! Deep copy.  This overwrites dst with src

	class(fn_t), intent(inout) :: dst
	class(fn_t), intent(in)    :: src

	!********

	!print *, 'starting fn_copy()'

	dst%type            = src%type
	dst%variadic_min    = src%variadic_min
	dst%variadic_max    = src%variadic_max
	dst%variadic_type   = src%variadic_type
	dst%param_names     = src%param_names
	dst%is_intr         = src%is_intr
	dst%is_method       = src%is_method
	dst%is_const_method = src%is_const_method

	if (allocated(src%variadic_name)) then
		if (allocated(dst%variadic_name)) deallocate(dst%variadic_name)
		dst%variadic_name = src%variadic_name
	else if (allocated(dst%variadic_name)) then
		deallocate(dst%variadic_name)
	end if

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

recursive module subroutine fn_ternary_tree_copy(dst, src)

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
		call fn_copy(dst%val, src%val)
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

recursive module subroutine syntax_node_vector_copy(dst, src)

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

recursive module subroutine syntax_node_copy(dst, src)

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

	call value_copy(dst%val, src%val)
	!dst%val%sca%file_     = src%val%sca%file_
	!dst%val%sca%file_%eof = src%val%sca%file_%eof

	dst%identifier = src%identifier
	dst%id_index   = src%id_index
	dst%num_locs   = src%num_locs
	dst%is_loc     = src%is_loc
	dst%root_kind  = src%root_kind

	if (allocated(src%struct_name)) then
		dst%struct_name = src%struct_name
	else if (allocated(dst%struct_name)) then
		deallocate(dst%struct_name)
	end if

	if (allocated(src%module_prefix)) then
		dst%module_prefix = src%module_prefix
	else if (allocated(dst%module_prefix)) then
		deallocate(dst%module_prefix)
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
	dst%lsub_omit = src%lsub_omit
	dst%usub_omit = src%usub_omit

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

	if (allocated(src%is_const_ref)) then
		dst%is_const_ref = src%is_const_ref
	else if (allocated(dst%is_const_ref)) then
		deallocate(dst%is_const_ref)
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

recursive module subroutine syntax_node_move(src, dst)

	! Move src into dst (freshly allocated).  Transfers all allocatable
	! components from src to dst via move_alloc (O(1) for node arrays/strings)
	! and value_move for the val member.  After return, src's allocatables are
	! unallocated; scalars are undefined.  Mirrors value_move() in value.f90.
	!
	! dst is allocatable so callers can pass unallocated allocatable components
	! (e.g. expr%right) without pre-allocating.

	type(syntax_node_t), intent(inout)            :: src
	type(syntax_node_t), allocatable, intent(out) :: dst

	!********

	allocate(dst)

	! POD scalars
	dst%kind            = src%kind
	dst%op              = src%op
	dst%identifier      = src%identifier
	dst%id_index        = src%id_index
	dst%num_locs        = src%num_locs
	dst%is_loc          = src%is_loc
	dst%root_kind       = src%root_kind
	dst%sub_kind        = src%sub_kind
	dst%lsub_omit       = src%lsub_omit
	dst%usub_omit       = src%usub_omit
	dst%is_empty        = src%is_empty
	dst%expecting       = src%expecting
	dst%first_expecting = src%first_expecting

	! diagnostics: move the internal array, copy scalars
	call move_alloc(src%diagnostics%v, dst%diagnostics%v)
	dst%diagnostics%len_ = src%diagnostics%len_
	dst%diagnostics%cap  = src%diagnostics%cap

	! value_t member
	call value_move(src%val, dst%val)

	! Allocatable strings
	call move_alloc(src%struct_name,    dst%struct_name)
	call move_alloc(src%module_prefix,  dst%module_prefix)
	call move_alloc(src%first_expected, dst%first_expected)

	! Allocatable primitive arrays
	call move_alloc(src%params,       dst%params)
	call move_alloc(src%is_ref,       dst%is_ref)
	call move_alloc(src%is_const_ref, dst%is_const_ref)

	! Scalar allocatable node children
	call move_alloc(src%left,        dst%left)
	call move_alloc(src%right,       dst%right)
	call move_alloc(src%condition,   dst%condition)
	call move_alloc(src%if_clause,   dst%if_clause)
	call move_alloc(src%else_clause, dst%else_clause)
	call move_alloc(src%body,        dst%body)
	call move_alloc(src%array,       dst%array)
	call move_alloc(src%member,      dst%member)
	call move_alloc(src%lbound,      dst%lbound)
	call move_alloc(src%step,        dst%step)
	call move_alloc(src%ubound,      dst%ubound)
	call move_alloc(src%len_,        dst%len_)
	call move_alloc(src%rank,        dst%rank)

	! Array allocatable node children
	call move_alloc(src%members,     dst%members)
	call move_alloc(src%elems,       dst%elems)
	call move_alloc(src%lsubscripts, dst%lsubscripts)
	call move_alloc(src%usubscripts, dst%usubscripts)
	call move_alloc(src%ssubscripts, dst%ssubscripts)
	call move_alloc(src%args,        dst%args)
	call move_alloc(src%size,        dst%size)

end subroutine syntax_node_move

!===============================================================================

recursive module subroutine syntax_node_move_into(src, dst)

	! Like syntax_node_move but dst is non-allocatable (already associated with
	! storage).  Overwrites dst's scalar fields and move_allocs all allocatable
	! components (move_alloc handles deallocation of dst's existing allocatables
	! before the transfer).  Used where dst is a non-allocatable variable that
	! needs to receive a new value without a deep copy, e.g. the accumulator in
	! the binary operator loop in parse_expr.

	type(syntax_node_t), intent(inout) :: src, dst

	!********

	! POD scalars
	dst%kind            = src%kind
	dst%op              = src%op
	dst%identifier      = src%identifier
	dst%id_index        = src%id_index
	dst%num_locs        = src%num_locs
	dst%is_loc          = src%is_loc
	dst%root_kind       = src%root_kind
	dst%sub_kind        = src%sub_kind
	dst%lsub_omit       = src%lsub_omit
	dst%usub_omit       = src%usub_omit
	dst%is_empty        = src%is_empty
	dst%expecting       = src%expecting
	dst%first_expecting = src%first_expecting

	! diagnostics: move the internal array, copy scalars
	call move_alloc(src%diagnostics%v, dst%diagnostics%v)
	dst%diagnostics%len_ = src%diagnostics%len_
	dst%diagnostics%cap  = src%diagnostics%cap

	! value_t member
	call value_move(src%val, dst%val)

	! Allocatable strings
	call move_alloc(src%struct_name,    dst%struct_name)
	call move_alloc(src%module_prefix,  dst%module_prefix)
	call move_alloc(src%first_expected, dst%first_expected)

	! Allocatable primitive arrays
	call move_alloc(src%params,       dst%params)
	call move_alloc(src%is_ref,       dst%is_ref)
	call move_alloc(src%is_const_ref, dst%is_const_ref)

	! Scalar allocatable node children
	call move_alloc(src%left,        dst%left)
	call move_alloc(src%right,       dst%right)
	call move_alloc(src%condition,   dst%condition)
	call move_alloc(src%if_clause,   dst%if_clause)
	call move_alloc(src%else_clause, dst%else_clause)
	call move_alloc(src%body,        dst%body)
	call move_alloc(src%array,       dst%array)
	call move_alloc(src%member,      dst%member)
	call move_alloc(src%lbound,      dst%lbound)
	call move_alloc(src%step,        dst%step)
	call move_alloc(src%ubound,      dst%ubound)
	call move_alloc(src%len_,        dst%len_)
	call move_alloc(src%rank,        dst%rank)

	! Array allocatable node children
	call move_alloc(src%members,     dst%members)
	call move_alloc(src%elems,       dst%elems)
	call move_alloc(src%lsubscripts, dst%lsubscripts)
	call move_alloc(src%usubscripts, dst%usubscripts)
	call move_alloc(src%ssubscripts, dst%ssubscripts)
	call move_alloc(src%args,        dst%args)
	call move_alloc(src%size,        dst%size)

end subroutine syntax_node_move_into

!===============================================================================

recursive module subroutine ternary_tree_copy(dst, src)

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
	dst%is_const = src%is_const

	if (allocated(src%val)) then
		if (.not. allocated(dst%val)) allocate(dst%val)
		call value_copy(dst%val, src%val)
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

! struct_ternary_tree_copy() was here.  It's no longer needed: structs_t is
! now a flat hash table (struct_entry_t table(:) in types.f90) instead of a
! ternary tree, so default (intrinsic) assignment suffices -- it recurses
! elementwise over table(:), and each struct_entry_t's allocatable val
! component already uses struct_copy() via struct_t's own defined
! assignment(=)

!===============================================================================

recursive module subroutine syntax_nodes_copy(dst, src)

	! Array copy

	type(syntax_node_t), allocatable, intent(inout) :: dst(:)
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

end submodule syntran__types_copy

!===============================================================================

