
!===============================================================================

submodule (syntran__types_m) syntran__types_dict

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine fn_grow(dict)

	! Double dict%table's capacity (or allocate an initial table) and rehash
	! all existing entries into it.  Mirrors struct_grow() in this same file.
	!
	! This is a submodule-local helper (no "module" prefix, no interface
	! declaration in types.f90), not part of the fns_t public API
	!
	! This does its own probing instead of calling fn_insert(), because
	! fn_insert() takes id_index as given by the caller -- appropriate for a
	! genuinely new declaration, but not for relocating an existing entry
	! during a rehash

	class(fns_t) :: dict

	!********

	type(fn_entry_t), allocatable :: old_table(:)
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

end subroutine fn_grow

!===============================================================================

module subroutine var_dict_copy(dst, src)

	! Deep copy.  See the interface comment in types.f90 for why this can't
	! be `dst%table = src%table`

	type(var_dict_t), intent(inout) :: dst
	type(var_dict_t), intent(in)    :: src

	!********

	integer :: i

	dst%capacity = src%capacity
	dst%count = src%count
	dst%load_factor_threshold = src%load_factor_threshold

	if (allocated(dst%table)) deallocate(dst%table)
	if (.not. allocated(src%table)) return

	allocate(dst%table( size(src%table) ))
	do i = 1, size(src%table)

		if (allocated(src%table(i)%key)) dst%table(i)%key = src%table(i)%key

		if (allocated(src%table(i)%val)) then
			allocate(dst%table(i)%val)
			call value_copy(dst%table(i)%val, src%table(i)%val)
		end if

		dst%table(i)%id_index = src%table(i)%id_index
		dst%table(i)%is_const = src%table(i)%is_const

	end do

end subroutine var_dict_copy

!===============================================================================

module subroutine fn_entry_table_copy(dst, src)

	! Deep copy.  See var_dict_copy() and the interface comment in types.f90

	type(fn_entry_t), allocatable, intent(inout) :: dst(:)
	type(fn_entry_t), intent(in) :: src(:)

	!********

	integer :: i

	if (allocated(dst)) deallocate(dst)
	allocate(dst( size(src) ))

	do i = 1, size(src)

		if (allocated(src(i)%key)) dst(i)%key = src(i)%key

		if (allocated(src(i)%val)) then
			allocate(dst(i)%val)
			call fn_copy(dst(i)%val, src(i)%val)
		end if

		dst(i)%id_index = src(i)%id_index

	end do

end subroutine fn_entry_table_copy

!===============================================================================

module function fn_find(dict, key) result(slot)

	! Returns the table slot for `key`, or 0 if not present.  The slot is
	! only valid until the next insert() call: a resize() (triggered by
	! insert() growing the table) rehashes everything, invalidating
	! previously-returned slots and pointers from get().  Every current
	! caller reads get()/id_at() immediately after find(), with no
	! intervening insert(), so this is safe

	class(fns_t), intent(in) :: dict
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

end function fn_find

!===============================================================================

module function fn_get(dict, slot) result(val)

	class(fns_t), intent(in), target :: dict
	integer, intent(in) :: slot

	type(fn_t), pointer :: val

	val => dict%table(slot)%val

end function fn_get

!===============================================================================

module function fn_id_at(dict, slot) result(id_index)

	class(fns_t), intent(in) :: dict
	integer, intent(in) :: slot

	integer :: id_index

	id_index = dict%table(slot)%id_index

end function fn_id_at

!===============================================================================

module subroutine fn_insert(dict, key, val, id_index, iostat, overwrite)

	class(fns_t) :: dict
	character(len = *), intent(in) :: key
	type(fn_t), intent(in) :: val
	integer, intent(inout) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)

	!! num_fns is already incremented by caller *except* with intrinsic fns,
	!! where their index doesn't really matter
	!id_index = id_index + 1

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	io = exit_success

	if (dict%capacity <= 0 .or. &
			real(dict%count) / real(dict%capacity) >= dict%load_factor_threshold) then
		call fn_grow(dict)
	end if

	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(dict%capacity, int64)) + 1)

	do probe = 0, dict%capacity - 1
		idx = modulo(hash_idx + probe - 1, dict%capacity) + 1

		if (.not. allocated(dict%table(idx)%key)) then
			! Empty slot - insert new entry.  fn_t has a defined
			! assignment(=) (fn_copy), which -- unlike intrinsic
			! assignment -- does not auto-allocate an unallocated allocatable
			! target, so val must be explicitly allocated first.  Not
			! `dict%table(idx)%val = val` though -- `=` on an fn_t hits the
			! same gfortran/mingw defined-assignment code-gen bug documented
			! throughout types_copy.f90/value.f90; call fn_copy() directly
			dict%table(idx)%key = key
			if (.not. allocated(dict%table(idx)%val)) allocate(dict%table(idx)%val)
			call fn_copy(dict%table(idx)%val, val)
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
			call fn_copy(dict%table(idx)%val, val)
			dict%table(idx)%id_index = id_index
			exit
		end if
	end do

	if (present(iostat)) iostat = io

end subroutine fn_insert

!===============================================================================

subroutine var_grow(dict)

	! Double dict%table's capacity (or allocate an initial table) and rehash
	! all existing entries into it.  Mirrors struct_grow()/fn_grow() below in
	! this same file, but operates on a single scope's var_dict_t directly:
	! vars_t keeps one hash table per scope, in dicts(:), rather than a
	! single table
	!
	! This is a submodule-local helper (no "module" prefix, no interface
	! declaration in types.f90), not part of the vars_t public API

	type(var_dict_t), intent(inout) :: dict

	!********

	type(var_entry_t), allocatable :: old_table(:)
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
				dict%table(idx)%is_const = old_table(i)%is_const
				exit
			end if
		end do
	end do

end subroutine var_grow

!===============================================================================

function var_dict_find(dict, key) result(slot)

	! Returns dict%table's slot for `key`, or 0 if not present.  Mirrors
	! struct_find()/fn_find(), but for a single scope's var_dict_t.
	! var_search()/var_is_const() call this once per scope, walking from the
	! innermost scope outward until a slot is found or scope 1 is exhausted

	type(var_dict_t), intent(in) :: dict
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

end function var_dict_find

!===============================================================================

module subroutine var_insert(dict, key, val, id_index, iostat, overwrite, is_const)

	! There are a couple reasons for having this wrapper:
	!
	!   - dict is not allocatable, while dict%dicts(i)%table is.  type-bound
	!     methods are not allowed for allocatable types
	!   - it's an abstraction away from the dict implementation: an
	!     open-addressing hash table (FNV-1a + linear probing), like
	!     structs_t/fns_t

	class(vars_t) :: dict
	character(len = *), intent(in) :: key
	type(value_t), intent(in) :: val
	integer, intent(in) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite
	logical, intent(in), optional :: is_const

	!********

	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx, i, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)
	!print *, 'val = ', val%to_str()

	! Note that this is different than the fn insert default
	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i  = dict%scope
	io = exit_success

	if (dict%dicts(i)%capacity <= 0 .or. &
			real(dict%dicts(i)%count) / real(dict%dicts(i)%capacity) >= &
			dict%dicts(i)%load_factor_threshold) then
		call var_grow(dict%dicts(i))
	end if

	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(dict%dicts(i)%capacity, int64)) + 1)

	do probe = 0, dict%dicts(i)%capacity - 1
		idx = modulo(hash_idx + probe - 1, dict%dicts(i)%capacity) + 1

		if (.not. allocated(dict%dicts(i)%table(idx)%key)) then
			! Empty slot - insert new entry.  value_t has a defined
			! assignment(=), which -- unlike intrinsic assignment -- does not
			! auto-allocate an unallocated allocatable target, so val must be
			! explicitly allocated first
			dict%dicts(i)%table(idx)%key = key
			if (.not. allocated(dict%dicts(i)%table(idx)%val)) &
				allocate(dict%dicts(i)%table(idx)%val)
			call value_copy(dict%dicts(i)%table(idx)%val, val)
			dict%dicts(i)%table(idx)%id_index = id_index
			if (present(is_const)) dict%dicts(i)%table(idx)%is_const = is_const
			dict%dicts(i)%count = dict%dicts(i)%count + 1
			exit
		else if (is_str_eq(dict%dicts(i)%table(idx)%key, key)) then
			! Key already inserted
			if (.not. overwritel) then
				io = exit_failure
				exit
			end if
			if (.not. allocated(dict%dicts(i)%table(idx)%val)) &
				allocate(dict%dicts(i)%table(idx)%val)
			call value_copy(dict%dicts(i)%table(idx)%val, val)
			dict%dicts(i)%table(idx)%id_index = id_index
			if (present(is_const)) dict%dicts(i)%table(idx)%is_const = is_const
			exit
		end if
	end do

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

	integer :: i, io, slot

	if (present(is_const)) is_const = .false.

	i = dict%scope
	slot = var_dict_find(dict%dicts(i), key)

	! If not found in current scope, search parent scopes too
	do while (slot == 0 .and. i > 1)
		i = i - 1
		slot = var_dict_find(dict%dicts(i), key)
	end do

	if (slot > 0) then
		io       = exit_success
		! Not `val = dict%dicts(i)%table(slot)%val` -- same class of
		! gfortran/mingw defined-assignment bug as push_value() in
		! value.f90; call value_copy() directly
		call value_copy(val, dict%dicts(i)%table(slot)%val)
		id_index = dict%dicts(i)%table(slot)%id_index
		if (present(is_const)) is_const = dict%dicts(i)%table(slot)%is_const
	else
		io = exit_failure
	end if

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

	integer :: i, slot

	is_const = .false.

	i = dict%scope
	slot = var_dict_find(dict%dicts(i), key)

	! If not found in current scope, search parent scopes too
	do while (slot == 0 .and. i > 1)
		i = i - 1
		slot = var_dict_find(dict%dicts(i), key)
	end do

	if (slot > 0) is_const = dict%dicts(i)%table(slot)%is_const

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
			! The `table` member is also allocatable.  Moving its pointer is
			! inexpensive
			call move_alloc(tmp(i)%table, dict%dicts(i)%table)
			dict%dicts(i)%capacity = tmp(i)%capacity
			dict%dicts(i)%count    = tmp(i)%count
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

	! It's possible that a scope may not have any local vars, so its table
	! is not allocated
	if (allocated(dict%dicts(i)%table)) then
		deallocate(dict%dicts(i)%table)
	end if
	dict%dicts(i)%capacity = 0
	dict%dicts(i)%count    = 0

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

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))

		! Not `tmp(1:vector%cap) = vector%v` -- syntax_token_t wraps a
		! value_t (nested allocatables), so whole-array assignment hits the
		! same gfortran/mingw defined-assignment code-gen bug documented
		! throughout types_copy.f90/value.f90.  Copy element-wise instead
		do i = 1, vector%cap
			! Not `tmp(i) = vector%v(i)` -- `=` (even defined assignment)
			! into a syntax_token_t array element with a nested-allocatable
			! value_t hits the gfortran/mingw code-gen bug; call the copy
			! subroutine directly.  This is the token-lexing hot path.
			call syntax_token_copy(tmp(i), vector%v(i))
		end do

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	! Not `vector%v( vector%len_ ) = val` -- see above
	call syntax_token_copy(vector%v( vector%len_ ), val)

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
			! Not `tmp(i) = vector%v(i)` -- `=` (defined assignment) into a
			! syntax_node_t array element with nested allocatables hits the
			! gfortran/mingw code-gen bug; call the copy subroutine directly
			call syntax_node_copy(tmp(i), vector%v(i))
		end do
		deallocate(vector%v)
		allocate(vector%v(tmp_cap))
		do i = 1, vector%cap
			call syntax_node_copy(vector%v(i), tmp(i))
		end do
		vector%cap = tmp_cap
	end if

	! Not `vector%v(vector%len_) = val` -- see above
	call syntax_node_copy(vector%v(vector%len_), val)

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

! ternary_search(), ternary_is_const(), and ternary_insert() were here.
! var_dict_t is now a flat hash table (var_entry_t table(:) in types.f90)
! instead of a recursive ternary tree, so they were replaced by
! var_dict_find() and var_grow() above, with var_search()/var_is_const()/
! var_insert() reading/writing dict%dicts(i)%table(:) directly

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
			! target, so val must be explicitly allocated first.  Not
			! `dict%table(idx)%val = val` though -- `=` on a struct_t hits
			! the same gfortran/mingw defined-assignment code-gen bug
			! documented throughout types_copy.f90/value.f90; call
			! struct_copy() directly
			dict%table(idx)%key = key
			if (.not. allocated(dict%table(idx)%val)) allocate(dict%table(idx)%val)
			call struct_copy(dict%table(idx)%val, val)
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
			call struct_copy(dict%table(idx)%val, val)
			dict%table(idx)%id_index = id_index
			exit
		end if
	end do

	if (present(iostat)) iostat = io

end subroutine struct_insert

!===============================================================================

! ternary_closest() was here.  var_dict_t is now a flat hash table, so
! var_closest() below scans dict%dicts(i)%table(:) directly, mirroring
! fn_closest()'s scan of fns_t's single table

!===============================================================================

module function var_closest(dict, key) result(closest)

	! Return the closest declared variable name to `key` across all current
	! scopes, or "" when no name is close enough (threshold: edit distance <=
	! max(2, len(unqualified key)/3)).  Candidates are ranked by the
	! Levenshtein distance of their unqualified (post "::") name, with the
	! full module-qualified distance as a tie-breaker.  See fn_closest() for
	! the same ranking over a single flat table.

	class(vars_t), intent(in) :: dict
	character(len = *), intent(in) :: key
	character(len = :), allocatable :: closest

	!********

	integer :: i, j, min_dist, min_qdist, threshold, dist, qdist
	character(len = :), allocatable :: target_low, target_unqual_low, &
		key_, key_low, key_unqual

	closest           = ""
	min_dist          = huge(min_dist)
	min_qdist         = huge(min_qdist)
	target_low        = to_lower(key)
	target_unqual_low = to_lower(unqualified_name(key))

	do i = 1, dict%scope
		do j = 1, dict%dicts(i)%capacity
			if (.not. allocated(dict%dicts(i)%table(j)%key)) cycle

			key_ = dict%dicts(i)%table(j)%key
			key_low = to_lower(key_)
			if (key_low == target_low) cycle

			key_unqual = unqualified_name(key_)
			dist  = levenshtein(target_unqual_low, to_lower(key_unqual))
			qdist = levenshtein(target_low, key_low)
			if (dist < min_dist .or. (dist == min_dist .and. qdist < min_qdist)) then
				min_dist  = dist
				min_qdist = qdist
				closest   = key_
			end if
		end do
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

	integer :: i, min_dist, min_qdist, threshold, dist, qdist
	character(len = :), allocatable :: target_low, target_unqual_low, &
		key_, key_low, key_unqual, display

	closest           = ""
	min_dist          = huge(min_dist)
	min_qdist         = huge(min_qdist)
	target_low        = to_lower(key)
	target_unqual_low = to_lower(unqualified_name(key))

	do i = 1, dict%capacity
		if (.not. allocated(dict%table(i)%key)) cycle

		key_ = dict%table(i)%key
		key_low = to_lower(key_)
		if (key_low == target_low) cycle

		! De-mangle internal overload keys (e.g. "0tan_f32" -> "tan") so we
		! never surface a "0"-prefixed name in a suggestion.
		key_unqual = unqualified_name(key_)
		display    = overload_display_name(key_unqual)
		dist = levenshtein(target_unqual_low, to_lower(key_unqual))
		if (display /= key_unqual) &
			dist = min(dist, levenshtein(target_unqual_low, to_lower(display)))
		qdist = levenshtein(target_low, key_low)
		if (dist < min_dist .or. (dist == min_dist .and. qdist < min_qdist)) then
			min_dist  = dist
			min_qdist = qdist
			closest   = overload_display_name(key_)
		end if
	end do

	threshold = max(2, len(target_unqual_low) / 3)
	if (min_dist > threshold) closest = ""

end function fn_closest

!===============================================================================

end submodule syntran__types_dict

!===============================================================================

