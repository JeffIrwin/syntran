
!===============================================================================

submodule (syntran__types_m) syntran__types_dict

	implicit none

!===============================================================================

contains

!===============================================================================

module function fn_search(dict, key, id_index, iostat) result(val)

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

module subroutine fn_insert(dict, key, val, id_index, iostat, overwrite)

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

	!! num_fns is already incremented by caller *except* with intrinsic fns,
	!! where their index doesn't really matter
	!id_index = id_index + 1

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call fn_ternary_insert(dict%dict%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine fn_insert

!===============================================================================

module subroutine var_insert(dict, key, val, id_index, iostat, overwrite)

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

module subroutine var_search(dict, key, id_index, iostat, val)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(value_t), intent(out) :: val

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

module subroutine push_scope(dict)

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

module subroutine pop_scope(dict)

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

module subroutine push_token(vector, val)

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
		if (vector%cap > 0) tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_token

!===============================================================================

module subroutine push_node(vector, val)

	class(syntax_node_vector_t) :: vector
	type(syntax_node_t) :: val

	!********

	type(syntax_node_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		tmp_cap = 2 * vector%len_
		allocate(tmp(tmp_cap))
		do i = 1, vector%cap
			tmp(i) = vector%v(i)
		end do
		deallocate(vector%v)
		allocate(vector%v(tmp_cap))
		do i = 1, vector%cap
			vector%v(i) = tmp(i)
		end do
		vector%cap = tmp_cap
	end if

	vector%v(vector%len_) = val

end subroutine push_node

!===============================================================================

module subroutine push_node_move(vector, val)

	class(syntax_node_vector_t) :: vector
	type(syntax_node_t), intent(inout) :: val

	!********

	type(syntax_node_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		tmp_cap = 2 * vector%len_
		allocate(tmp(tmp_cap))
		do i = 1, vector%cap
			call syntax_node_move_into(vector%v(i), tmp(i))
		end do
		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap
	end if

	call syntax_node_move_into(val, vector%v(vector%len_))

end subroutine push_node_move

!===============================================================================

recursive module subroutine ternary_search(node, key, id_index, iostat, val)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(value_t), intent(out) :: val

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

recursive module subroutine ternary_insert(node, key, val, id_index, iostat, overwrite)

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

recursive module function fn_ternary_search(node, key, id_index, iostat) result(val)

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

recursive module subroutine fn_ternary_insert(node, key, val, id_index, iostat, overwrite)

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

recursive module function struct_ternary_exists(node, key) result(exists)

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

recursive module subroutine struct_ternary_search(node, key, id_index, iostat, val)

	type(struct_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(struct_t), intent(out) :: val

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

recursive module subroutine struct_ternary_insert(node, key, val, id_index, iostat, overwrite)

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

module subroutine struct_insert(dict, key, val, id_index, iostat, overwrite)

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

module function struct_exists(dict, key) result(exists)

	! Check if a key exists, without copying an output val unlike
	! struct_search()

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	logical :: exists

	exists = struct_ternary_exists(dict%dict%root, key)

end function struct_exists

!===============================================================================

module subroutine struct_search(dict, key, id_index, iostat, val)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(struct_t), intent(out) :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: io

	!print *, "starting struct search"

	call struct_ternary_search(dict%dict%root, key, id_index, io, val)
	!print *, "io = ", io

	if (present(iostat)) iostat = io

end subroutine struct_search

!===============================================================================

end submodule syntran__types_dict

!===============================================================================

