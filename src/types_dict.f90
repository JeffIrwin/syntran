
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

	class(fns_t), intent(in), target :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(fn_t), pointer :: val

	integer, intent(out), optional :: iostat

	!********

	integer :: i, io

	i = dict%scope

	val => fn_ternary_search(dict%dict%root, key, id_index, io)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		val => fn_ternary_search(dict%dict%root, key, id_index, io)
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

module subroutine var_insert(dict, key, val, id_index, iostat, overwrite, is_const)

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
	logical, intent(in), optional :: is_const

	!********

	integer :: i, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)
	!print *, 'val = ', val%to_str()

	! Note that this is different than the fn insert default
	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel, is_const)

	if (present(iostat)) iostat = io

end subroutine var_insert

!===============================================================================

module subroutine var_search(dict, key, id_index, iostat, val, is_const)

	! An id_index is not normally part of dictionary searching, but we use it
	! here for converting the dictionary into an array after parsing and before
	! evaluation for better performance

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	integer, intent(out) :: id_index
	type(value_t), intent(out) :: val

	integer, intent(out), optional :: iostat
	logical, intent(out), optional :: is_const

	!********

	integer :: i, io

	i = dict%scope

	call ternary_search(dict%dicts(i)%root, key, id_index, io, val, is_const)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		call ternary_search(dict%dicts(i)%root, key, id_index, io, val, is_const)
	end do

	if (present(iostat)) iostat = io

end subroutine var_search

!===============================================================================

module function var_is_const(dict, key) result(is_const)

	! Cheap check for whether a declared variable is const, without copying
	! its value_t like search() does.  Defaults to .false. if key is not
	! found in any scope

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key

	logical :: is_const

	!********

	integer :: i
	logical :: found

	i = dict%scope

	call ternary_is_const(dict%dicts(i)%root, key, is_const, found)

	! If not found in current scope, search parent scopes too
	do while (.not. found .and. i > 1)
		i = i - 1
		call ternary_is_const(dict%dicts(i)%root, key, is_const, found)
	end do

end function var_is_const

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
		write(*,*) err_int(IC_SCOPE_STACK_EMPTY, 'scope stack is empty')
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

recursive module subroutine ternary_search(node, key, id_index, iostat, val, is_const)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(value_t), intent(out) :: val
	logical, intent(out), optional :: is_const

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	iostat = exit_success
	if (present(is_const)) is_const = .false.

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		call ternary_search(node%left , key, id_index, iostat, val, is_const)
		return
	else if (k > node%split_char) then
		call ternary_search(node%right, key, id_index, iostat, val, is_const)
		return
	else if (len(ey) > 0) then
		call ternary_search(node%mid  , ey, id_index, iostat, val, is_const)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		return
	end if

	val      = node%val
	id_index = node%id_index
	if (present(is_const)) is_const = node%is_const

	!print *, 'done ternary_search'
	!print *, ''

end subroutine ternary_search

!===============================================================================

recursive module subroutine ternary_is_const(node, key, is_const, found)

	! Cheap existence + const-flag check, without copying node%val out like
	! ternary_search() does.  Mirrors struct_exists()

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	logical, intent(out) :: is_const, found

	!********

	character :: k
	character(len = :), allocatable :: ey

	is_const = .false.
	found    = .false.

	if (.not. allocated(node)) then
		! Search key not found
		return
	end if

	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		call ternary_is_const(node%left , key, is_const, found)
		return
	else if (k > node%split_char) then
		call ternary_is_const(node%right, key, is_const, found)
		return
	else if (len(ey) > 0) then
		call ternary_is_const(node%mid  , ey, is_const, found)
		return
	end if

	if (.not. allocated(node%val)) return

	found    = .true.
	is_const = node%is_const

end subroutine ternary_is_const

!===============================================================================

recursive module subroutine ternary_insert(node, key, val, id_index, iostat, overwrite, is_const)

	type(ternary_tree_node_t), intent(inout), allocatable :: node
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out) :: iostat
	logical, intent(in) :: overwrite
	logical, intent(in), optional :: is_const

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
		call ternary_insert(node%left , key, val, id_index, iostat, overwrite, is_const)
		return
	else if (k > node%split_char) then
		!print *, 'right'
		call ternary_insert(node%right, key, val, id_index, iostat, overwrite, is_const)
		return
	end if

	!print *, 'mid'

	if (len(ey) /= 0) then
		call ternary_insert(node%mid  , ey, val, id_index, iostat, overwrite, is_const)
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
	if (present(is_const)) node%is_const = is_const

	!print *, 'done inserting'
	!print *, ''

end subroutine ternary_insert

!===============================================================================

recursive module function fn_ternary_search(node, key, id_index, iostat) result(val)

	type(fn_ternary_tree_node_t), intent(in), allocatable, target :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(fn_t), pointer :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key ', quote(key)

	iostat = exit_success

	if (.not. allocated(node)) then
		! Search key not found
		iostat = exit_failure
		val => null()
		return
	end if

	! :)
	k   = key(1:1)
	 ey = key(2:)

	if (k < node%split_char) then
		val => fn_ternary_search(node%left , key, id_index, iostat)
		return
	else if (k > node%split_char) then
		val => fn_ternary_search(node%right, key, id_index, iostat)
		return
	else if (len(ey) > 0) then
		val => fn_ternary_search(node%mid  , ey, id_index, iostat)
		return
	end if

	!print *, 'setting val'

	if (.not. allocated(node%val)) then
		iostat = exit_failure
		val => null()
		return
	end if

	val      => node%val
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

subroutine struct_grow(dict)

	! Double dict%table's capacity (or allocate an initial table) and rehash
	! all existing entries into it.  Mirrors map_i32_resize() in utils.f90.
	!
	! This is a submodule-local helper (no "module" prefix, no interface
	! declaration in types.f90), not part of the structs_t public API
	!
	! This does its own probing instead of calling struct_insert(), because
	! struct_insert() increments id_index on every call -- appropriate for a
	! genuinely new declaration, but not for relocating an existing entry
	! during a rehash

	class(structs_t) :: dict

	!********

	type(struct_entry_t), allocatable :: old_table(:)
	integer :: old_capacity, i, new_capacity
	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx

	old_capacity = dict%capacity
	new_capacity = max(2 * old_capacity, 8)

	if (old_capacity > 0) call move_alloc(dict%table, old_table)

	dict%capacity = new_capacity
	allocate(dict%table(new_capacity))
	! dict%count is unchanged -- rehashing doesn't add or remove entries

	do i = 1, old_capacity
		if (.not. allocated(old_table(i)%key)) cycle

		hash_val = fnv_1a(old_table(i)%key)
		hash_idx = int(modulo(hash_val, int(dict%capacity, int64)) + 1)

		do probe = 0, dict%capacity - 1
			idx = modulo(hash_idx + probe - 1, dict%capacity) + 1
			if (.not. allocated(dict%table(idx)%key)) then
				call move_alloc(old_table(i)%key, dict%table(idx)%key)
				call move_alloc(old_table(i)%val, dict%table(idx)%val)
				dict%table(idx)%id_index = old_table(i)%id_index
				exit
			end if
		end do
	end do

end subroutine struct_grow

!===============================================================================

module function struct_find(dict, key) result(slot)

	! Returns the table slot for `key`, or 0 if not present.  The slot is
	! only valid until the next insert() call: a resize() (triggered by
	! insert() growing the table) rehashes everything, invalidating
	! previously-returned slots and pointers from get().  Every current
	! caller reads get()/id_at() immediately after find(), with no
	! intervening insert(), so this is safe

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key

	integer :: slot

	!********

	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx

	slot = 0

	if (dict%capacity <= 0) return

	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(dict%capacity, int64)) + 1)

	do probe = 0, dict%capacity - 1
		idx = modulo(hash_idx + probe - 1, dict%capacity) + 1

		if (.not. allocated(dict%table(idx)%key)) then
			! Empty slot => key not present
			return
		else if (is_str_eq(dict%table(idx)%key, key)) then
			slot = idx
			return
		end if
	end do

end function struct_find

!===============================================================================

module function struct_get(dict, slot) result(val)

	class(structs_t), intent(in), target :: dict
	integer, intent(in) :: slot

	type(struct_t), pointer :: val

	val => dict%table(slot)%val

end function struct_get

!===============================================================================

module function struct_id_at(dict, slot) result(id_index)

	class(structs_t), intent(in) :: dict
	integer, intent(in) :: slot

	integer :: id_index

	id_index = dict%table(slot)%id_index

end function struct_id_at

!===============================================================================

module function struct_exists(dict, key) result(exists)

	! Check if a key exists, without going through get() like other callers do

	class(structs_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	logical :: exists

	exists = struct_find(dict, key) > 0

end function struct_exists

!===============================================================================

module subroutine struct_insert(dict, key, val, id_index, iostat, overwrite)

	class(structs_t) :: dict
	character(len = *), intent(in) :: key
	type(struct_t), intent(in) :: val
	integer, intent(inout) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)
	id_index = id_index + 1

	! Note that this is different than the fn insert default.  Re-declared
	! structs are caught in the caller (in parse_struct_declaration())
	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	io = exit_success

	if (dict%capacity <= 0 .or. &
			real(dict%count) / real(dict%capacity) >= dict%load_factor_threshold) then
		call struct_grow(dict)
	end if

	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(dict%capacity, int64)) + 1)

	do probe = 0, dict%capacity - 1
		idx = modulo(hash_idx + probe - 1, dict%capacity) + 1

		if (.not. allocated(dict%table(idx)%key)) then
			! Empty slot - insert new entry.  struct_t has a defined
			! assignment(=) (struct_copy), which -- unlike intrinsic
			! assignment -- does not auto-allocate an unallocated allocatable
			! target, so val must be explicitly allocated first
			dict%table(idx)%key = key
			if (.not. allocated(dict%table(idx)%val)) allocate(dict%table(idx)%val)
			dict%table(idx)%val = val
			dict%table(idx)%id_index = id_index
			dict%count = dict%count + 1
			exit
		else if (is_str_eq(dict%table(idx)%key, key)) then
			! Key already inserted
			if (.not. overwritel) then
				io = exit_failure
				exit
			end if
			if (.not. allocated(dict%table(idx)%val)) allocate(dict%table(idx)%val)
			dict%table(idx)%val = val
			dict%table(idx)%id_index = id_index
			exit
		end if
	end do

	if (present(iostat)) iostat = io

end subroutine struct_insert

!===============================================================================

recursive module subroutine ternary_closest(node, prefix, target_low, &
		target_unqual_low, min_dist, min_qdist, closest)

	! Walk a ternary tree accumulating the key character-by-character and
	! track the terminal key whose *unqualified* (post "::") form has the
	! lowest Levenshtein distance from `target_unqual_low`, breaking ties by
	! the distance of the full qualified key from `target_low` (both already
	! lower-cased by the caller).

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: prefix, target_low, target_unqual_low
	integer, intent(inout) :: min_dist, min_qdist
	character(len = :), allocatable, intent(inout) :: closest

	!********

	character(len = :), allocatable :: key, key_low, key_unqual
	integer :: dist, qdist

	if (.not. allocated(node)) return

	key = prefix//node%split_char

	! Terminal node: a value is stored here
	if (allocated(node%val)) then
		key_low = to_lower(key)
		if (key_low /= target_low) then
			key_unqual = unqualified_name(key)
			dist  = levenshtein(target_unqual_low, to_lower(key_unqual))
			qdist = levenshtein(target_low, key_low)
			if (dist < min_dist .or. (dist == min_dist .and. qdist < min_qdist)) then
				min_dist  = dist
				min_qdist = qdist
				closest   = key
			end if
		end if
	end if

	! Explore all three branches
	call ternary_closest(node%left , prefix, target_low, target_unqual_low, &
		min_dist, min_qdist, closest)
	call ternary_closest(node%right, prefix, target_low, target_unqual_low, &
		min_dist, min_qdist, closest)
	call ternary_closest(node%mid  , key   , target_low, target_unqual_low, &
		min_dist, min_qdist, closest)

end subroutine ternary_closest

!===============================================================================

recursive module subroutine fn_ternary_closest(node, prefix, target_low, &
		target_unqual_low, min_dist, min_qdist, closest)

	! Like ternary_closest but for fn_ternary_tree_node_t.

	type(fn_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: prefix, target_low, target_unqual_low
	integer, intent(inout) :: min_dist, min_qdist
	character(len = :), allocatable, intent(inout) :: closest

	!********

	character(len = :), allocatable :: key, key_low, key_unqual, display
	integer :: dist, qdist

	if (.not. allocated(node)) return

	key = prefix//node%split_char

	if (allocated(node%val)) then
		key_low = to_lower(key)
		if (key_low /= target_low) then
			! De-mangle internal overload keys (e.g. "0tan_f32" -> "tan") so we
			! never surface a "0"-prefixed name in a suggestion.
			key_unqual = unqualified_name(key)
			display    = overload_display_name(key_unqual)
			dist = levenshtein(target_unqual_low, to_lower(key_unqual))
			if (display /= key_unqual) &
				dist = min(dist, levenshtein(target_unqual_low, to_lower(display)))
			qdist = levenshtein(target_low, key_low)
			if (dist < min_dist .or. (dist == min_dist .and. qdist < min_qdist)) then
				min_dist  = dist
				min_qdist = qdist
				closest   = overload_display_name(key)
			end if
		end if
	end if

	call fn_ternary_closest(node%left , prefix, target_low, target_unqual_low, &
		min_dist, min_qdist, closest)
	call fn_ternary_closest(node%right, prefix, target_low, target_unqual_low, &
		min_dist, min_qdist, closest)
	call fn_ternary_closest(node%mid  , key   , target_low, target_unqual_low, &
		min_dist, min_qdist, closest)

end subroutine fn_ternary_closest

!===============================================================================

module function var_closest(dict, key) result(closest)

	! Return the closest declared variable name to `key` across all current
	! scopes, or "" when no name is close enough (threshold: edit distance <=
	! max(2, len(unqualified key)/3)).  Candidates are ranked by the
	! Levenshtein distance of their unqualified (post "::") name, with the
	! full module-qualified distance as a tie-breaker.

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	character(len = :), allocatable :: closest

	!********

	integer :: i, min_dist, min_qdist, threshold
	character(len = :), allocatable :: target_low, target_unqual_low

	closest           = ""
	min_dist          = huge(min_dist)
	min_qdist         = huge(min_qdist)
	target_low        = to_lower(key)
	target_unqual_low = to_lower(unqualified_name(key))

	do i = 1, dict%scope
		if (.not. allocated(dict%dicts(i)%root)) cycle
		call ternary_closest(dict%dicts(i)%root, "", target_low, &
			target_unqual_low, min_dist, min_qdist, closest)
	end do

	! Only keep the suggestion when it is close enough
	threshold = max(2, len(target_unqual_low) / 3)
	if (min_dist > threshold) closest = ""

end function var_closest

!===============================================================================

module function fn_closest(dict, key) result(closest)

	! Return the closest declared function name to `key`, or "" when none is
	! close enough.  See var_closest() for the unqualified-name ranking with
	! qualified-name tie-breaker.

	class(fns_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	character(len = :), allocatable :: closest

	!********

	integer :: min_dist, min_qdist, threshold
	character(len = :), allocatable :: target_low, target_unqual_low

	closest           = ""
	min_dist          = huge(min_dist)
	min_qdist         = huge(min_qdist)
	target_low        = to_lower(key)
	target_unqual_low = to_lower(unqualified_name(key))

	call fn_ternary_closest(dict%dict%root, "", target_low, &
		target_unqual_low, min_dist, min_qdist, closest)

	threshold = max(2, len(target_unqual_low) / 3)
	if (min_dist > threshold) closest = ""

end function fn_closest

!===============================================================================

end submodule syntran__types_dict

!===============================================================================

