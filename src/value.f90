
!===============================================================================

module syntran__value_m

	use syntran__consts_m
	use syntran__errors_m

	implicit none

	!********

	type file_t

		character(len = :), allocatable :: name_

		integer :: unit_  ! fortran file unit

		! TODO: extend with more modes, e.g. binary, text, append (?)
		!
		! c.f. python open modes:  https://docs.python.org/3/library/functions.html#open
		logical :: &
			mode_read  = .false., &
			mode_write = .false.

		logical :: is_open = .false.
		logical :: eof = .false.
		logical :: is_std = .false.   ! true for std::IN/OUT/ERR — close() is forbidden
		! Do we need a separate iostat beyond eof?

	end type file_t

	!********

	type scalar_t

		! Scalar value type for numeric/bool values.  Cannot be an array!
		! String and file values are stored in value_t%str / value_t%file_
		! so that this type is a plain POD — copies are cheap (no allocatable
		! components).

		logical           :: bool
		integer(kind = 4) :: i32
		integer(kind = 8) :: i64
		real   (kind = 4) :: f32
		real   (kind = 8) :: f64

		contains
			procedure :: to_str => scalar_to_str

	end type scalar_t

	!********

	type array_t

		! The array type is i32_type, f32_type, etc. while the kind is
		! unif_array, bound_array, len_array, step_array, or expl_array
		integer :: type, kind
		type(scalar_t), allocatable :: lbound, step, ubound

		! Note that these are arrays of primitive Fortran types, instead of
		! arrays of generic value_t.  This performs better since we can put
		! a type select/case outside of loops for processing arrays, as opposed
		! to inside of a loop for type selection of every element
		logical(kind = 1), allocatable :: bool(:)

		integer(kind = 4), allocatable ::  i32(:)
		integer(kind = 8), allocatable ::  i64(:)

		real   (kind = 4), allocatable ::  f32(:)
		real   (kind = 8), allocatable ::  f64(:)

		type(string_t   ), allocatable ::  str(:)

		! TODO: file arrays

		integer :: rank
		integer(kind = 8) :: len_, cap
		integer(kind = 8), allocatable :: size(:)

		contains
			procedure :: push => push_array
			procedure :: trim => trim_array

	end type array_t

	!********

	type value_t
		integer :: type = unknown_type

		! Numeric/bool scalars.  scalar_t is a plain POD (no allocatable
		! components) so copying it is cheap.  str_ and file types are
		! handled by the allocatable components below.
		type(scalar_t) :: sca

		! String and file values — moved out of scalar_t so that scalar_t
		! (and therefore sca copies) are POD-cheap.  Only allocated when
		! value%type == str_type or file_type respectively.
		type(string_t), allocatable :: str
		type(file_t  ), allocatable :: file_

		! Back when array_t could contain value_t's, gfortran would use up infinite
		! RAM trying to parse the circular type dependencies unless this was a
		! pointer.  But pointers lead to nasty memory leaks (e.g. aoc 2023 day
		! 07)
		!
		! Now arrays can contain a scalar_t instead of a value_t, so there
		! are no longer any circular type dependencies
		!
		! Note that a type containing itself is fine (e.g. ternary_tree_node_t),
		! but two types containing each other is bad
		type(array_t), allocatable :: array

		! i played with having a separate `struct_val_t` type and having an
		! array of those, but it works better just having a direct array of
		! `value_t`'s here instead
		type(value_t), allocatable :: struct(:)
		character(len = :), allocatable :: struct_name

		! Canonical, alias-independent struct identity: "<defining src file>::<local
		! struct name>".  Used for type matching so the same struct reached via
		! different module aliases/import paths is recognized as the same type.
		! struct_name above remains the display name and may be re-qualified per
		! import path
		character(len = :), allocatable :: struct_cookie

		contains
			procedure :: to_str => value_to_str
			procedure :: to_f32 => value_to_f32
			procedure :: to_f64 => value_to_f64
			procedure :: to_i32 => value_to_i32
			procedure :: to_i64 => value_to_i64
			procedure :: to_i32_array => value_to_i32_array  ! for user-facing casting fn
			procedure :: to_i64_array => value_to_i64_array
			procedure :: to_f32_array => value_to_f32_array
			procedure :: to_f64_array => value_to_f64_array
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => value_copy
			generic, public :: assignment(=) => copy
#endif

	end type value_t

	type value_vector_t
		type(value_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push      => push_value
			procedure :: push_move => push_value_move
	end type value_vector_t

!===============================================================================

contains

!===============================================================================

function new_value_vector() result(vector)

	type(value_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 64  ! Large enough to avoid growth churn in hot loops

	allocate(vector%v( vector%cap ))

end function new_value_vector

!===============================================================================

subroutine push_value(vector, val)

	! Push a deep copy of val onto the stack.

	class(value_vector_t) :: vector
	type(value_t) :: val

	!********

	type(value_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		! Move existing elements into tmp (cheap: move_alloc for arrays/structs).
		! After each value_move, vector%v(i) has its allocatable components cleared,
		! making the subsequent move_alloc safe.
		do i = 1, vector%cap
			call value_move(vector%v(i), tmp(i))
		end do
		! Replace vector%v with the new larger allocation in one descriptor swap.
		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap
	end if

	! Not `vector%v(vector%len_) = val` -- this is the hottest value_t
	! assignment in the whole interpreter (every LOAD_CONST/LOAD_GLOBAL/
	! LOAD_LOCAL goes through here), and `=` on a value_t hits the same
	! gfortran/mingw defined-assignment code-gen bug documented on
	! syntax_token_copy() and vars_copy() in types_copy.f90.  Call
	! value_copy() directly instead; it's also the one that already knows
	! how to overwrite a reused (possibly stale-allocated) stack slot -- see
	! its "reused stack slot" comment below
	call value_copy(vector%v( vector%len_ ), val)   ! deep copy: source (val) must stay live

end subroutine push_value

!===============================================================================

subroutine push_value_move(vector, val)

	! Push val onto the stack by moving it (consuming val).
	! Used for freshly computed temporaries that have no other live references
	! (e.g. binop results, intrinsic return values).  Avoids the deep copy in
	! push_value; for array/struct values this is O(1) instead of O(n).

	class(value_vector_t) :: vector
	type(value_t), intent(inout) :: val   ! consumed; undefined after return

	!********

	type(value_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		do i = 1, vector%cap
			call value_move(vector%v(i), tmp(i))
		end do
		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap
	end if

	call value_move(val, vector%v( vector%len_ ))

end subroutine push_value_move

!===============================================================================

!recursive subroutine free_value(src)
!
!   ! This is not necessary, although I experimented with it while working on
!   ! memory corruption bugs.  Sometimes fortran crashes with a stack trace that
!   ! points to deallocation, often with other non-sensical lines in the stack
!   ! trace on "end subroutine" or "end function" or even "end module"
!   !
!   ! In every case that I can remember, these crashes are due to incorrect
!   ! *initialization* of a recursive struct, and there is nothing wrong with
!   ! its deallocator under normal circumstances when it is initialized
!   ! correctly.  Specifically, I have seen issues when assigning whole built-in
!   ! arrays of recursive structs (e.g. syntax_node_t(:)).  The array copier
!   ! does not invoke my overloaded custom copy assignment operator, resulting
!   ! in incorrect initialization
!
!	type(value_t) :: src
!
!	!********
!
!	integer :: i
!
!	print *, "starting free_value()"
!
!	if (allocated(src%struct_name)) then
!		deallocate(src%struct_name)
!	end if
!
!	if (allocated(src%array)) then
!		deallocate(src%array)
!	end if
!
!	if (allocated(src%struct)) then
!		do i = 1, size(src%struct)
!			call free_value(src%struct(i))
!		end do
!		deallocate(src%struct)
!	end if
!
!	print *, "ending free_value()"
!
!end subroutine free_value

!===============================================================================

subroutine value_reset(val)

	! Reset val to unknown_type, freeing any allocatable components.
	! Used by the VM call-frame locals pool to clean up a reused slot.
	! Fast path for primitive scalars (no allocatables to free).

	type(value_t), intent(inout) :: val

	select case (val%type)
	case (bool_type, i32_type, i64_type, f32_type, f64_type)
		! Scalar: nothing allocated, just clear the type tag.
		val%type = unknown_type
	case default
		! Don't rely on a bare `deallocate(val%array)` / `deallocate(val%struct)`
		! here -- both can be arbitrarily deeply nested (array_t containing
		! str(:) of string_t with its own allocatable %s; struct(:) is a
		! recursive value_t array), and gfortran's implicit deep
		! deallocation of that doesn't reliably free every level (same bug
		! already fixed piecewise in value_copy()/array_copy() and
		! eval_fn_call()'s locs teardown). value_destroy() clears every
		! level explicitly, including str/file_/struct_name/struct_cookie
		call value_destroy(val)
		val%type = unknown_type
	end select

end subroutine value_reset

!===============================================================================

recursive subroutine value_move(src, dst)
	! Note the args are reversed wrt value_copy.  It is however consistent with
	! build-in move_alloc() (and `mv file1 file2`)
	!
	! This is kind of a fake move.  Arrays and structs are moved, but primitive
	! scalars are just copied.
	!
	! Note: dst%type = src%type is set unconditionally first.  Callers (e.g.
	! parse_expr.f90) may read src's type tag AFTER a move to this subroutine
	! returns; that is safe because this routine does not clear src%type.
	! src's allocatables are, however, cleared (moved to dst) for array/struct/
	! str/file types.

	type(value_t), intent(inout) :: src
	type(value_t), intent(out)   :: dst

	!********

	if (debug > 3) print *, 'starting value_move()'

	dst%type = src%type

	select case (src%type)
	case (array_type)
		call move_alloc(src%array, dst%array)
		! Struct arrays also use struct(:) for elements and struct_name for type tag.
		if (allocated(src%struct)) call move_alloc(src%struct, dst%struct)
		if (allocated(src%struct_name)) call move_alloc(src%struct_name, dst%struct_name)
		if (allocated(src%struct_cookie)) call move_alloc(src%struct_cookie, dst%struct_cookie)

	case (struct_type)
		call move_alloc(src%struct_name, dst%struct_name)
		if (allocated(src%struct_cookie)) call move_alloc(src%struct_cookie, dst%struct_cookie)
		call move_alloc(src%struct, dst%struct)

	case (str_type)
		call move_alloc(src%str, dst%str)

	case (file_type)
		call move_alloc(src%file_, dst%file_)

	case default
		! POD copy: scalar_t now contains only bool/i32/i64/f32/f64 — cheap.
		dst%sca = src%sca

	end select

end subroutine value_move

!===============================================================================

subroutine array_move(src, dst)

	! Like value_move(), but for a plain (non-allocatable) array_t variable.
	! dst cannot be move_alloc'd wholesale since it isn't itself allocatable
	! (unlike value_t%array), so each of array_t's allocatable components is
	! moved individually instead

	type(array_t), intent(inout) :: src
	type(array_t), intent(out)   :: dst

	dst%type = src%type
	dst%kind = src%kind
	dst%rank = src%rank
	dst%len_ = src%len_
	dst%cap  = src%cap

	call move_alloc(src%lbound, dst%lbound)
	call move_alloc(src%step,   dst%step)
	call move_alloc(src%ubound, dst%ubound)

	call move_alloc(src%bool, dst%bool)
	call move_alloc(src%i32 , dst%i32 )
	call move_alloc(src%i64 , dst%i64 )
	call move_alloc(src%f32 , dst%f32 )
	call move_alloc(src%f64 , dst%f64 )
	call move_alloc(src%str , dst%str )

	call move_alloc(src%size, dst%size)

end subroutine array_move

!===============================================================================

subroutine array_copy(dst, src)

	! Deep copy of a plain (non-allocatable) array_t variable.  Like
	! value_copy(), but for array_t.
	!
	! The str(:) component needs special handling: string_t has its own
	! allocatable %s, so a whole-array `dst%str = src%str` is a
	! double-nested reallocation-on-assignment in a single implicit
	! statement, which gfortran doesn't handle correctly (see the identical
	! str_type fix in value_copy() below).  Every other component here is
	! single-level (a plain primitive array, or scalar_t which has no
	! nested allocatables of its own), so plain `=` is safe for those

	type(array_t), intent(inout) :: dst
	type(array_t), intent(in)    :: src

	!********

	integer(kind = 8) :: i

	dst%type = src%type
	dst%kind = src%kind
	dst%rank = src%rank
	dst%len_ = src%len_
	dst%cap  = src%cap

	if (allocated(src%lbound)) then
		if (.not. allocated(dst%lbound)) allocate(dst%lbound)
		dst%lbound = src%lbound
	else if (allocated(dst%lbound)) then
		deallocate(dst%lbound)
	end if

	if (allocated(src%step)) then
		if (.not. allocated(dst%step)) allocate(dst%step)
		dst%step = src%step
	else if (allocated(dst%step)) then
		deallocate(dst%step)
	end if

	if (allocated(src%ubound)) then
		if (.not. allocated(dst%ubound)) allocate(dst%ubound)
		dst%ubound = src%ubound
	else if (allocated(dst%ubound)) then
		deallocate(dst%ubound)
	end if

	if (allocated(src%bool)) then
		dst%bool = src%bool
	else if (allocated(dst%bool)) then
		deallocate(dst%bool)
	end if

	if (allocated(src%i32)) then
		dst%i32 = src%i32
	else if (allocated(dst%i32)) then
		deallocate(dst%i32)
	end if

	if (allocated(src%i64)) then
		dst%i64 = src%i64
	else if (allocated(dst%i64)) then
		deallocate(dst%i64)
	end if

	if (allocated(src%f32)) then
		dst%f32 = src%f32
	else if (allocated(dst%f32)) then
		deallocate(dst%f32)
	end if

	if (allocated(src%f64)) then
		dst%f64 = src%f64
	else if (allocated(dst%f64)) then
		deallocate(dst%f64)
	end if

	if (allocated(src%str)) then
		! Double-nested (outer str(:) allocatable + each element's own
		! allocatable %s) — don't rely on a whole-array `dst%str = src%str`
		! to reallocate both levels correctly in one shot.  Reallocate the
		! outer array explicitly, then copy each element's %s individually
		if (allocated(dst%str)) deallocate(dst%str)
		allocate(dst%str( size(src%str) ))
		do i = 1, size(src%str, kind = 8)
			dst%str(i)%s = src%str(i)%s
		end do
	else if (allocated(dst%str)) then
		deallocate(dst%str)
	end if

	if (allocated(src%size)) then
		dst%size = src%size
	else if (allocated(dst%size)) then
		deallocate(dst%size)
	end if

end subroutine array_copy

!===============================================================================

recursive subroutine value_copy(dst, src)

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my types, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading

	class(value_t), intent(inout) :: dst
	type(value_t),  intent(in)    :: src

	!********

	integer :: i

	if (debug > 3) print *, 'starting value_copy()'

	dst%type = src%type
	dst%sca  = src%sca   ! POD copy: bool/i32/i64/f32/f64 only — cheap

	! Guard str/file_ copies on type, not just on allocated().  A reused stack
	! slot may carry a stale allocatable from a prior value (different type); we
	! must not propagate it to dst when the current type no longer uses it.
	if (src%type == str_type .and. allocated(src%str)) then
		! Don't rely on `dst%str = src%str` doing an implicit reallocation of
		! the outer allocatable AND the nested allocatable %s component in one
		! shot — gfortran leaks the old %s block when dst%str was already
		! allocated to a different length.  Deallocate/reallocate explicitly
		! and copy the (now simple, single-level) character component instead
		if (allocated(dst%str)) deallocate(dst%str)
		allocate(dst%str)
		dst%str%s = src%str%s
	else if (allocated(dst%str)) then
		deallocate(dst%str)
	end if

	if (src%type == file_type .and. allocated(src%file_)) then
		if (.not. allocated(dst%file_)) allocate(dst%file_)
		dst%file_ = src%file_   ! copies all fields including name_ (alloc char)
	else if (allocated(dst%file_)) then
		deallocate(dst%file_)
	end if

	if (allocated(src%struct_name)) then
		dst%struct_name = src%struct_name
	else if (allocated(dst%struct_name)) then
		deallocate(dst%struct_name)
	end if

	if (allocated(src%struct_cookie)) then
		dst%struct_cookie = src%struct_cookie
	else if (allocated(dst%struct_cookie)) then
		deallocate(dst%struct_cookie)
	end if

	if (allocated(src%array)) then
		! Don't rely on a whole-object `dst%array = src%array` here either —
		! array_t's str(:) component has the same double-nested
		! reallocation-on-assignment problem as value_t%str above.  See
		! array_copy()
		if (.not. allocated(dst%array)) allocate(dst%array)
		call array_copy(dst%array, src%array)
	else if (allocated(dst%array)) then
		deallocate(dst%array)
	end if

	if (allocated(src%struct)) then
		if (allocated(dst%struct)) deallocate(dst%struct)
		allocate(dst%struct( size(src%struct) ))
		do i = 1, size(src%struct)
			call value_copy(dst%struct(i), src%struct(i))
		end do
	else if (allocated(dst%struct)) then
		deallocate(dst%struct)
	end if

end subroutine value_copy

!===============================================================================

subroutine array_destroy(arr)

	! Explicitly deallocate arr's allocatable components one level at a
	! time, mirroring array_copy()'s per-component handling.  Used ahead of
	! a whole-array deallocation of value_t(:) (e.g. before move_alloc()
	! implicitly deallocates its "to" argument) so that implicit,
	! compiler-generated deep deallocation of a value_t array never has to
	! walk a live, deeply-nested allocatable tree itself -- by the time it
	! runs, every component here is already empty

	type(array_t), intent(inout) :: arr

	!********

	integer(kind = 8) :: i

	if (allocated(arr%lbound)) deallocate(arr%lbound)
	if (allocated(arr%step))   deallocate(arr%step)
	if (allocated(arr%ubound)) deallocate(arr%ubound)

	if (allocated(arr%bool)) deallocate(arr%bool)
	if (allocated(arr%i32))  deallocate(arr%i32)
	if (allocated(arr%i64))  deallocate(arr%i64)
	if (allocated(arr%f32))  deallocate(arr%f32)
	if (allocated(arr%f64))  deallocate(arr%f64)

	if (allocated(arr%str)) then
		do i = 1, size(arr%str, kind = 8)
			if (allocated(arr%str(i)%s)) deallocate(arr%str(i)%s)
		end do
		deallocate(arr%str)
	end if

	if (allocated(arr%size)) deallocate(arr%size)

end subroutine array_destroy

!===============================================================================

recursive subroutine value_destroy(val)

	! Explicitly deallocate val's allocatable components one level at a
	! time, instead of relying on implicit/compiler-generated deep
	! deallocation of a value_t (or an array of value_t) to correctly walk
	! a deeply-nested allocatable tree (struct(:) of struct(:) of array_t
	! containing str(:) of string_t, etc.) in one shot.  See array_destroy()

	type(value_t), intent(inout) :: val

	!********

	integer :: i

	if (allocated(val%str)) deallocate(val%str)
	if (allocated(val%file_)) deallocate(val%file_)
	if (allocated(val%struct_name)) deallocate(val%struct_name)
	if (allocated(val%struct_cookie)) deallocate(val%struct_cookie)

	if (allocated(val%array)) then
		call array_destroy(val%array)
		deallocate(val%array)
	end if

	if (allocated(val%struct)) then
		do i = 1, size(val%struct)
			call value_destroy(val%struct(i))
		end do
		deallocate(val%struct)
	end if

end subroutine value_destroy

!===============================================================================

subroutine value_array_destroy(vals)

	! Safely deallocate an array of value_t: explicitly destroy each
	! element first (see value_destroy()), then deallocate the array
	! itself.  Use this instead of a bare `deallocate(vals)` wherever a
	! growable pool/buffer of value_t needs to be freed or resized --
	! gfortran's implicit deep deallocation of an array of a type this
	! deeply nested (str(:) of string_t with its own allocatable %s;
	! struct(:) is itself a recursive value_t array) is not reliable

	type(value_t), allocatable, intent(inout) :: vals(:)

	!********

	integer :: i

	if (.not. allocated(vals)) return

	do i = 1, size(vals)
		call value_destroy(vals(i))
	end do
	deallocate(vals)

end subroutine value_array_destroy

!===============================================================================

function mold(mold_, type_) result(array)

	! Construct array meta-data, such as type, rank, and size, based on a given
	! mold
	!
	! The actual allocation of array%i32 or array%bool (appropriately depending
	! on the type) and setting of its values is done outside of here in the
	! calling fn

	type(array_t), intent(in) :: mold_

	integer, intent(in) :: type_

	type(array_t), allocatable :: array

	allocate(array)

	array%type = type_

	!array%kind = expl_array
	array%rank = mold_%rank

	array%len_ = mold_%len_
	array%cap  = mold_%cap
	array%size = mold_%size

end function mold

!===============================================================================

subroutine push_array(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(array_t) :: vector
	type(value_t)  :: val

	!********

	integer(kind = 4), allocatable :: tmp_i32 (:)
	integer(kind = 8), allocatable :: tmp_i64 (:)

	real   (kind = 4), allocatable :: tmp_f32 (:)
	real   (kind = 8), allocatable :: tmp_f64 (:)

	logical(kind = 1), allocatable :: tmp_bool(:)

	type(string_t   ), allocatable :: tmp_str (:)

	integer(kind = 8) :: tmp_cap, i

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_

		if (vector%type == i32_type) then

			allocate(tmp_i32 ( tmp_cap ))
			tmp_i32(1: vector%cap) = vector%i32
			call move_alloc(tmp_i32, vector%i32)

		else if (vector%type == i64_type) then

			allocate(tmp_i64 ( tmp_cap ))
			tmp_i64(1: vector%cap) = vector%i64
			call move_alloc(tmp_i64, vector%i64)

		else if (vector%type == f32_type) then

			allocate(tmp_f32 ( tmp_cap ))
			tmp_f32(1: vector%cap) = vector%f32
			call move_alloc(tmp_f32, vector%f32)

		else if (vector%type == f64_type) then

			allocate(tmp_f64 ( tmp_cap ))
			tmp_f64(1: vector%cap) = vector%f64
			call move_alloc(tmp_f64, vector%f64)

		else if (vector%type == bool_type) then

			allocate(tmp_bool( tmp_cap ))
			tmp_bool(1: vector%cap) = vector%bool
			call move_alloc(tmp_bool, vector%bool)

		else if (vector%type == str_type) then

			! Not `tmp_str(1:vector%cap) = vector%str` -- string_t has its
			! own allocatable %s component, so whole-array assignment hits
			! the same gfortran/mingw bug documented on set_array_val() in
			! eval_array.f90.  Copy element-wise instead
			allocate(tmp_str ( tmp_cap ))
			do i = 1, vector%cap
				tmp_str(i)%s = vector%str(i)%s
			end do
			call move_alloc(tmp_str, vector%str)

		else
			! FIXME: when adding new types, implement it below too to set the
			! last val
			write(*,*) err_int(IC_PUSH_ARRAY_TYPE, 'push_array type not implemented')
			call internal_error()
		end if

		vector%cap = tmp_cap

	end if

	select case (vector%type)
	case (i32_type)
		vector%i32 ( vector%len_ ) = val%sca%i32
	case (i64_type)
		vector%i64 ( vector%len_ ) = val%sca%i64
	case (f32_type)
		vector%f32 ( vector%len_ ) = val%sca%f32
	case (f64_type)
		vector%f64 ( vector%len_ ) = val%sca%f64
	case (bool_type)
		vector%bool( vector%len_ ) = val%sca%bool
	case (str_type)
		! Not `vector%str(vector%len_) = val%str` -- see the note above
		if (allocated(vector%str(vector%len_)%s)) deallocate(vector%str(vector%len_)%s)
		vector%str(vector%len_)%s = val%str%s
	case default
		write(*,*) err_int(IC_PUSH_ARRAY_TYPE, 'push_array type not implemented')
		call internal_error()
	end select

end subroutine push_array

!===============================================================================

subroutine trim_array(vector)

	class(array_t) :: vector

	!********

	type(string_t), allocatable :: tmp_str(:)
	integer(kind = 8) :: i

	select case (vector%type)
	case (i32_type)
		vector%i32 = vector%i32(1: vector%len_)

	case (i64_type)
		vector%i64 = vector%i64(1: vector%len_)

	case (f32_type)
		vector%f32 = vector%f32(1: vector%len_)

	case (f64_type)
		vector%f64 = vector%f64(1: vector%len_)

	case (bool_type)
		vector%bool = vector%bool(1: vector%len_)

	case (str_type)
		! Not `vector%str = vector%str(1:vector%len_)` -- same class of
		! gfortran/mingw bug as push_array() above; copy element-wise
		! through a temp array instead
		allocate(tmp_str(vector%len_))
		do i = 1, vector%len_
			tmp_str(i)%s = vector%str(i)%s
		end do
		call move_alloc(tmp_str, vector%str)

	! TODO: str case, bool case.  File?  Struct?  Other types?
	case default
		write(*,*) err_int(IC_TRIM_ARRAY_TYPE, 'trim_array() implemented for this type')
		call internal_error()
	end select

end subroutine trim_array

!===============================================================================

function value_to_f32(val) result(ans)

	class(value_t) :: val

	real(kind = 4) :: ans

	select case (val%type)

		case (f32_type)
			ans = val%sca%f32

		case (f64_type)
			ans = real(val%sca%f64)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = real(val%sca%i64)

		case (str_type)

			! There is no user-facing `f32()` fn (or `f64()`) yet anyway, unlike `i32()`
			write(*,*) err_int(IC_CONVERT_F32, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f32.  Use `parse_f32()`')
			call internal_error()

		case default
			write(*,*) err_int(IC_CONVERT_F32, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f32 ')
			call internal_error()

	end select

end function value_to_f32

!===============================================================================

function value_to_f64(val) result(ans)

	class(value_t) :: val

	real(kind = 8) :: ans

	select case (val%type)

		case (f32_type)
			ans = val%sca%f32

		case (f64_type)
			ans = val%sca%f64

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = real(val%sca%i64)

		case (str_type)

			write(*,*) err_int(IC_CONVERT_F64, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f64.  Use `parse_f64()`')
			call internal_error()

		case default
			write(*,*) err_int(IC_CONVERT_F64, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f64 ')
			call internal_error()

	end select

end function value_to_f64

!===============================================================================

function value_to_i32(val) result(ans)

	class(value_t) :: val

	integer(kind = 4) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 4)

		case (f64_type)
			ans = int(val%sca%f64, 4)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = int(val%sca%i64, 4)

		case (str_type)

			if (allocated(val%str) .and. len(val%str%s) == 1) then
				ans = iachar(val%str%s)
			else
				write(*,*) err_int(IC_CONVERT_I32, 'cannot convert from type `' &
					//kind_name(val%type)//'` to i32.  Use `parse_i32()`')
				call internal_error()
			end if

		case default
			write(*,*) err_int(IC_CONVERT_I32, 'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 ')
			call internal_error()

	end select

end function value_to_i32

!===============================================================================

function value_to_i32_array(val) result(ans)

	class(value_t) :: val

	type(array_t) :: ans

	!print *, "starting value_to_i32_array()"
	!print *, "val%type = ", kind_name(val%type)
	!print *, "val%array%type = ", kind_name(val%array%type)

	ans = mold(val%array, i32_type)

	select case (val%array%type)

		case (f32_type)
			ans%i32 = int(val%array%f32, 4)

		case (f64_type)
			ans%i32 = int(val%array%f64, 4)

		case (i32_type)
			ans%i32 = val%array%i32

		case (i64_type)
			ans%i32 = int(val%array%i64, 4)

		!case (str_type)

		!	! TODO: loops are needed for str array conversion via iachar
		!	if (all(len(val%sca%str%s) == 1)) then
		!		!ans = iachar(val%sca%str%s)
		!		ans%i32 = iachar(val%array%str%s)

		!	else
		!		write(*,*) err_int_prefix//'cannot convert from type `' &
		!			//kind_name(val%type)//'` to i32.  Use `parse_i32()`'//color_reset
		!		call internal_error()
		!	end if

		case default
			write(*,*) err_int(IC_CONVERT_I32_ARR, 'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 ')
			call internal_error()

	end select

end function value_to_i32_array

!===============================================================================

function value_to_i64(val) result(ans)

	class(value_t) :: val

	integer(kind = 8) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 8)

		case (f64_type)
			ans = int(val%sca%f64, 8)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = val%sca%i64

		case default
			write(*,*) err_int(IC_CONVERT_I64, 'cannot convert from type `' &
				//kind_name(val%type)//'` to i64.  Use `parse_i64()`')
			call internal_error()

	end select

end function value_to_i64

!===============================================================================

function value_to_i64_array(val) result(ans)

	class(value_t) :: val

	type(array_t) :: ans

	!print *, "starting value_to_i64_array()"
	!print *, "val%type = ", kind_name(val%type)
	!print *, "val%array%type = ", kind_name(val%array%type)

	ans = mold(val%array, i64_type)

	select case (val%array%type)

		case (f32_type)
			ans%i64 = int(val%array%f32, 8)

		case (f64_type)
			ans%i64 = int(val%array%f64, 8)

		case (i32_type)
			ans%i64 = val%array%i32

		case (i64_type)
			!ans%i64 = int(val%array%i64, 4)
			ans%i64 = val%array%i64

		case default
			write(*,*) err_int(IC_CONVERT_I64_ARR, 'cannot convert from type `' &
				//kind_name(val%type)//'` to i64 ')
			call internal_error()

	end select

end function value_to_i64_array

!===============================================================================

function value_to_f32_array(val) result(ans)

	class(value_t) :: val

	type(array_t) :: ans

	ans = mold(val%array, f32_type)

	select case (val%array%type)

		case (f32_type)
			ans%f32 = val%array%f32

		case (f64_type)
			ans%f32 = real(val%array%f64, 4)

		case (i32_type)
			ans%f32 = real(val%array%i32)

		case (i64_type)
			ans%f32 = real(val%array%i64)

		case default
			write(*,*) err_int(IC_CONVERT_F32_ARR, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f32 ')
			call internal_error()

	end select

end function value_to_f32_array

!===============================================================================

function value_to_f64_array(val) result(ans)

	class(value_t) :: val

	type(array_t) :: ans

	ans = mold(val%array, f64_type)

	select case (val%array%type)

		case (f32_type)
			ans%f64 = real(val%array%f32, 8)

		case (f64_type)
			ans%f64 = val%array%f64

		case (i32_type)
			ans%f64 = real(val%array%i32, 8)

		case (i64_type)
			ans%f64 = real(val%array%i64, 8)

		case default
			write(*,*) err_int(IC_CONVERT_F64_ARR, 'cannot convert from type `' &
				//kind_name(val%type)//'` to f64 ')
			call internal_error()

	end select

end function value_to_f64_array

!===============================================================================

recursive function value_to_str(val) result(ans)

	class(value_t) :: val

	character(len = :), allocatable :: ans

	!********

	integer :: j
	integer(kind = 8) :: i8, prod, n

	type(char_vector_t) :: str_vec

	!print *, "val type = ", kind_name(val%type)

	select case (val%type)

		case (struct_type)

			str_vec = new_char_vector()
			call str_vec%push(val%struct_name//"{")

			n = size(val%struct)
			do i8 = 1, n

				! It would be nice to label each member with its name

				!call str_vec%push( val%struct(i8)%struct_name//" = " )

				call str_vec%push( trimw(val%struct(i8)%to_str()) )

				if (i8 < n) call str_vec%push(", ")

			end do
			call str_vec%push("}")
			ans = str_vec%trim()

		case (array_type)

			! This whole case could be an array_to_str() fn

			!if (val%array%kind == bound_array) then
			!	! This is unreachable in short tests.  Not sure why I wrote it
			!	ans = '['//val%array%lbound%to_str(val%array%type)//': ' &
			!	         //val%array%ubound%to_str(val%array%type)//']'
			!	return
			!end if

			!print *, 'array type = ', val%array%type

			!! You would think that this would help
			!if (val%array%type == i32_type) then
			!	str_vec = new_char_vector( 12 * val%array%len_ )
			!else if (val%array%type == f32_type) then
			!	str_vec = new_char_vector( 16 * val%array%len_ )
			!end if

			! This naming is terrible.  It's a string builder, not a vector of
			! strings
			str_vec = new_char_vector()

			call str_vec%push('[')
			if (val%array%rank > 1) call str_vec%push(line_feed)

			!! Debug w/o recursive io
			!call str_vec%push( kind_name(val%array%type) )
			!call str_vec%push(str(int(val%array%len_)))

			if (val%array%type == i32_type) then

				!! Recursive IO stalls execution
				!print *, 'size = ', val%array%size

				do i8 = 1, int(val%array%len_)

					call str_vec%push(str(val%array%i32(i8)))
					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == i64_type) then

				!! Recursive IO stalls execution
				!print *, 'size = ', val%array%size

				do i8 = 1, val%array%len_

					call str_vec%push(str(val%array%i64(i8)))
					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == f32_type) then

				do i8 = 1, val%array%len_

					!! Nice alignment, but breaks tests
					!write(buf16, '(es16.6)') val%array%f32(i8)
					!call str_vec%push(buf16)

					! Trimmed string (not aligned)
					call str_vec%push(str(val%array%f32(i8)))

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == f64_type) then

				do i8 = 1, val%array%len_

					!! Nice alignment, but breaks tests
					!write(buf16, '(es16.6)') val%array%f64(i8)
					!call str_vec%push(buf16)

					! Trimmed string (not aligned)
					call str_vec%push(str(val%array%f64(i8)))

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == bool_type) then

				do i8 = 1, val%array%len_

					call str_vec%push(str(val%array%bool(i8)))

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == str_type) then

				do i8 = 1, val%array%len_

					call str_vec%push(val%array%str(i8)%s)

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == struct_type) then
	
				n = size(val%struct)
				do i8 = 1, n
					! Just recurse instead of nesting a loop
					call str_vec%push( val%struct(i8)%to_str() )
					if (i8 < n) call str_vec%push(", ")
				end do

			else

				! Do *not* print anything in this function, as recursive IO will
				! cause a hang
				call str_vec%push(err_prefix//"<invalid_array_value>"//color_reset)

			end if

			if (val%array%rank > 1) call str_vec%push(line_feed)
			call str_vec%push(']')

			ans = str_vec%v( 1: str_vec%len_ )

		case (str_type)
			! TODO: wrap in quotes for clarity?  Would be a breaking change.
			if (allocated(val%str)) then
				ans = val%str%s
			else
				ans = ''
			end if

		case (file_type)
			if (allocated(val%file_)) then
				ans = "{file_unit: "//str(val%file_%unit_)//", filename: """// &
					val%file_%name_//"""}"
			else
				ans = "{file_unit: <unset>}"
			end if

		case default
			ans = val%sca%to_str(val%type)

	end select

end function value_to_str

!===============================================================================

recursive function scalar_to_str(val, type) result(ans)

	class(scalar_t) :: val

	integer, intent(in) :: type

	character(len = :), allocatable :: ans

	!********

	character(len = 16) :: buf16
	character(len = 28) :: buf28

	select case (type)

		case (void_type)
			ans = ''

		case (bool_type)
			! TODO: use bool1_str() and other primitive converters
			if (val%bool) then
				ans = "true"
			else
				ans = "false"
			end if

		case (f32_type)
			write(buf16, '(es16.6)') val%f32
			!ans = trim(buf16)
			ans = buf16  ! no trim for alignment

		case (f64_type)
			write(buf28, '(es25.15)') val%f64
			!ans = trim(buf28)
			ans = buf28  ! no trim for alignment

		case (i32_type)
			ans = i32_str(val%i32)

		case (i64_type)
			ans = i64_str(val%i64)

		case default
			ans = err_prefix//"<invalid_value>"//color_reset

	end select

end function scalar_to_str

!===============================================================================

end module syntran__value_m

!===============================================================================

