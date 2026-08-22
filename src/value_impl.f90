
!===============================================================================

submodule (syntran__value_m) syntran__value_impl_m

	implicit none

contains


!===============================================================================

module procedure new_value_vector


	vector%len_ = 0
	vector%cap = 64  ! Large enough to avoid growth churn in hot loops

	allocate(vector%v( vector%cap ))

end procedure new_value_vector


!===============================================================================

module procedure push_value

	! Push a deep copy of val onto the stack.


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

	vector%v( vector%len_ ) = val   ! deep copy: source (val) must stay live

end procedure push_value


!===============================================================================

module procedure push_value_move

	! Push val onto the stack by moving it (consuming val).
	! Used for freshly computed temporaries that have no other live references
	! (e.g. binop results, intrinsic return values).  Avoids the deep copy in
	! push_value; for array/struct values this is O(1) instead of O(n).


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

end procedure push_value_move


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

module procedure value_reset

	! Reset val to unknown_type, freeing any allocatable components.
	! Used by the VM call-frame locals pool to clean up a reused slot.
	! Fast path for primitive scalars (no allocatables to free).


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
		! already fixed piecewise in value_copy()/array_copy()).
		! value_destroy() clears every level explicitly, including
		! str/file_/struct_name/struct_cookie/enum_name/enum_variant/enum_cookie
		call value_destroy(val)
		val%type = unknown_type
	end select

end procedure value_reset


!===============================================================================

module procedure value_move
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


	!********

	if (debug > 3) print *, 'starting value_move()'

	dst%type = src%type

	! Plain integer, not allocatable -- always cheap to copy regardless of
	! type, same as %sca in value_copy()
	dst%struct_reg_idx = src%struct_reg_idx

	select case (src%type)
	case (array_type)
		call move_alloc(src%array, dst%array)
		! Struct arrays also use struct(:) for elements and struct_name for type tag.
		if (allocated(src%struct)) call move_alloc(src%struct, dst%struct)
		if (allocated(src%struct_name)) call move_alloc(src%struct_name, dst%struct_name)
		if (allocated(src%struct_cookie)) call move_alloc(src%struct_cookie, dst%struct_cookie)
		! Enum arrays likewise use struct(:) for elements and enum_name for type tag.
		if (allocated(src%enum_name)) call move_alloc(src%enum_name, dst%enum_name)
		if (allocated(src%enum_cookie)) call move_alloc(src%enum_cookie, dst%enum_cookie)

	case (struct_type)
		call move_alloc(src%struct_name, dst%struct_name)
		if (allocated(src%struct_cookie)) call move_alloc(src%struct_cookie, dst%struct_cookie)
		call move_alloc(src%struct, dst%struct)

	case (enum_type)
		dst%sca = src%sca   ! the backing i32 ordinal rides along here
		call move_alloc(src%enum_name, dst%enum_name)
		if (allocated(src%enum_variant)) call move_alloc(src%enum_variant, dst%enum_variant)
		if (allocated(src%enum_cookie)) call move_alloc(src%enum_cookie, dst%enum_cookie)
		! An enum_cast_expr node's %val is enum_type but also carries a baked
		! struct(:) of candidate variants (c.f. parse_enum_cast); move it too
		if (allocated(src%struct)) call move_alloc(src%struct, dst%struct)

	case (str_type)
		call move_alloc(src%str, dst%str)

	case (file_type)
		call move_alloc(src%file_, dst%file_)

	case (fn_type)
		dst%sca = src%sca   ! fn_index rides along here
		if (allocated(src%fn_params)) call move_alloc(src%fn_params, dst%fn_params)
		if (allocated(src%fn_ret))    call move_alloc(src%fn_ret,    dst%fn_ret)

	case default
		! POD copy: scalar_t now contains only bool/i32/i64/f32/f64 — cheap.
		dst%sca = src%sca

	end select

end procedure value_move


!===============================================================================

module procedure array_move

	! Like value_move(), but for a plain (non-allocatable) array_t variable.
	! dst cannot be move_alloc'd wholesale since it isn't itself allocatable
	! (unlike value_t%array), so each of array_t's allocatable components is
	! moved individually instead


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

end procedure array_move


!===============================================================================

module procedure array_copy

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

end procedure array_copy


!===============================================================================

module procedure value_copy

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my types, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading


	!********

	integer :: i

	if (debug > 3) print *, 'starting value_copy()'

	dst%type = src%type
	dst%sca  = src%sca   ! POD copy: bool/i32/i64/f32/f64 only — cheap

	! Plain integer, not allocatable -- always cheap to copy regardless of
	! type, same as %sca above
	dst%struct_reg_idx = src%struct_reg_idx

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

	if (allocated(src%enum_name)) then
		dst%enum_name = src%enum_name
	else if (allocated(dst%enum_name)) then
		deallocate(dst%enum_name)
	end if

	if (allocated(src%enum_variant)) then
		dst%enum_variant = src%enum_variant
	else if (allocated(dst%enum_variant)) then
		deallocate(dst%enum_variant)
	end if

	if (allocated(src%enum_cookie)) then
		dst%enum_cookie = src%enum_cookie
	else if (allocated(dst%enum_cookie)) then
		deallocate(dst%enum_cookie)
	end if

	if (allocated(src%array)) then
		! Don't rely on a whole-object `dst%array = src%array` here either —
		! array_t's str(:) component has the same double-nested
		! reallocation-on-assignment problem as value_t%str above.  See
		! array_copy()
		if (.not. allocated(dst%array)) allocate(dst%array)
		call array_copy(dst%array, src%array)
	else if (allocated(dst%array)) then
		! Explicitly tear down array_t's own nested allocatables (str(:) of
		! string_t, each with its own allocatable %s) before freeing the
		! outer allocatable -- same distrust of bare deallocate() as
		! value_array_destroy() below, applied one level in
		call array_destroy(dst%array)
		deallocate(dst%array)
	end if

	if (allocated(src%struct)) then
		if (allocated(dst%struct)) call value_array_destroy(dst%struct)
		allocate(dst%struct( size(src%struct) ))
		do i = 1, size(src%struct)
			call value_copy(dst%struct(i), src%struct(i))
		end do
	else if (allocated(dst%struct)) then
		call value_array_destroy(dst%struct)
	end if

	if (allocated(src%fn_params)) then
		if (allocated(dst%fn_params)) call value_array_destroy(dst%fn_params)
		allocate(dst%fn_params( size(src%fn_params) ))
		do i = 1, size(src%fn_params)
			call value_copy(dst%fn_params(i), src%fn_params(i))
		end do
	else if (allocated(dst%fn_params)) then
		call value_array_destroy(dst%fn_params)
	end if

	if (allocated(src%fn_ret)) then
		if (.not. allocated(dst%fn_ret)) allocate(dst%fn_ret)
		call value_copy(dst%fn_ret, src%fn_ret)
	else if (allocated(dst%fn_ret)) then
		call value_destroy(dst%fn_ret)
		deallocate(dst%fn_ret)
	end if

end procedure value_copy


!===============================================================================

module procedure array_destroy

	! Explicitly deallocate arr's allocatable components one level at a
	! time, mirroring array_copy()'s per-component handling.  Used ahead of
	! a whole-array deallocation of value_t(:) (e.g. before move_alloc()
	! implicitly deallocates its "to" argument) so that implicit,
	! compiler-generated deep deallocation of a value_t array never has to
	! walk a live, deeply-nested allocatable tree itself -- by the time it
	! runs, every component here is already empty


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

end procedure array_destroy


!===============================================================================

module procedure value_destroy

	! Explicitly deallocate val's allocatable components one level at a
	! time, instead of relying on implicit/compiler-generated deep
	! deallocation of a value_t (or an array of value_t) to correctly walk
	! a deeply-nested allocatable tree (struct(:) of struct(:) of array_t
	! containing str(:) of string_t, etc.) in one shot.  See array_destroy()


	!********

	integer :: i

	if (allocated(val%str)) deallocate(val%str)
	if (allocated(val%file_)) deallocate(val%file_)
	if (allocated(val%struct_name)) deallocate(val%struct_name)
	if (allocated(val%struct_cookie)) deallocate(val%struct_cookie)
	if (allocated(val%enum_name)) deallocate(val%enum_name)
	if (allocated(val%enum_variant)) deallocate(val%enum_variant)
	if (allocated(val%enum_cookie)) deallocate(val%enum_cookie)

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

	if (allocated(val%fn_params)) then
		do i = 1, size(val%fn_params)
			call value_destroy(val%fn_params(i))
		end do
		deallocate(val%fn_params)
	end if

	if (allocated(val%fn_ret)) then
		call value_destroy(val%fn_ret)
		deallocate(val%fn_ret)
	end if

end procedure value_destroy


!===============================================================================

module procedure value_array_destroy

	! Safely deallocate an array of value_t: explicitly destroy each
	! element first (see value_destroy()), then deallocate the array
	! itself.  Use this instead of a bare `deallocate(vals)` wherever a
	! growable pool/buffer of value_t needs to be freed or resized --
	! gfortran's implicit deep deallocation of an array of a type this
	! deeply nested (str(:) of string_t with its own allocatable %s;
	! struct(:) is itself a recursive value_t array) is not reliable


	!********

	integer :: i

	if (.not. allocated(vals)) return

	do i = 1, size(vals)
		call value_destroy(vals(i))
	end do
	deallocate(vals)

end procedure value_array_destroy


!===============================================================================

module procedure value_array_copy

	! Deep copy an array of value_t.  Use this instead of a whole-array
	! `dst = src` assignment wherever dst starts out unallocated (or a
	! different size than src): older gfortran mis-generates the
	! allocate-on-assignment for an allocatable array of a type with
	! recursive allocatable components (value_t%struct(:)) and a defined
	! assignment(=) -- it allocates dst but shallow-copies the nested
	! struct(:) block instead of invoking value_copy elementwise, so src and
	! dst end up sharing (and later double-freeing) the same block.  c.f.
	! value_array_destroy() above for the same distrust of compiler-
	! generated deep (de)allocation of this type


	!********

	integer :: i

	call value_array_destroy(dst)
	allocate(dst( size(src) ))

	do i = 1, size(src)
		dst(i) = src(i)  ! scalar assignment -> value_copy() defined assignment
	end do

end procedure value_array_copy


!===============================================================================

module procedure mold

	! Construct array meta-data, such as type, rank, and size, based on a given
	! mold
	!
	! The actual allocation of array%i32 or array%bool (appropriately depending
	! on the type) and setting of its values is done outside of here in the
	! calling fn




	allocate(array)

	array%type = type_

	!array%kind = expl_array
	array%rank = mold_%rank

	array%len_ = mold_%len_
	array%cap  = mold_%cap
	array%size = mold_%size

end procedure mold


!===============================================================================

module procedure copy_composite_id

	! Copy the components that describe a struct or enum type beyond
	! %array%type: struct_name/struct_cookie and enum_name/enum_cookie.
	! mold() doesn't carry these, so anything that builds a struct- or
	! enum-typed array result from a mold has to copy them explicitly, or
	! else member access and type-equality checks on the result break (the
	! former needs struct_name to look up the struct, the latter needs
	! struct_cookie/enum_cookie).
	!
	! %enum_variant is deliberately not copied: it names one particular
	! variant, so it belongs to a scalar enum value, not to an array of
	! them.


	if (allocated(src%struct_name)) dst%struct_name = src%struct_name
	if (allocated(src%struct_cookie)) dst%struct_cookie = src%struct_cookie
	if (allocated(src%enum_name)) dst%enum_name = src%enum_name
	if (allocated(src%enum_cookie)) dst%enum_cookie = src%enum_cookie

end procedure copy_composite_id


!===============================================================================

module procedure push_array

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out


	!********

	integer(kind = 4), allocatable :: tmp_i32 (:)
	integer(kind = 8), allocatable :: tmp_i64 (:)

	real   (kind = 4), allocatable :: tmp_f32 (:)
	real   (kind = 8), allocatable :: tmp_f64 (:)

	logical(kind = 1), allocatable :: tmp_bool(:)

	type(string_t   ), allocatable :: tmp_str (:)

	integer(kind = 8) :: tmp_cap

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

			allocate(tmp_str ( tmp_cap ))
			tmp_str (1: vector%cap) = vector%str
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
		vector%str ( vector%len_ ) = val%str
	case default
		write(*,*) err_int(IC_PUSH_ARRAY_TYPE, 'push_array type not implemented')
		call internal_error()
	end select

end procedure push_array


!===============================================================================

module procedure trim_array


	!********

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
		vector%str = vector%str(1: vector%len_)

	! TODO: str case, bool case.  File?  Struct?  Other types?
	case default
		write(*,*) err_int(IC_TRIM_ARRAY_TYPE, 'trim_array() implemented for this type')
		call internal_error()
	end select

end procedure trim_array


!===============================================================================

module procedure value_to_f32



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

end procedure value_to_f32


!===============================================================================

module procedure value_to_f64



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

end procedure value_to_f64


!===============================================================================

module procedure value_to_i32



	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 4)

		case (f64_type)
			ans = int(val%sca%f64, 4)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = int(val%sca%i64, 4)

		case (enum_type)
			! The backing ordinal, e.g. i32(Card.Queen) -> 11
			ans = val%sca%i32

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

end procedure value_to_i32


!===============================================================================

module procedure value_to_i32_array



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

end procedure value_to_i32_array


!===============================================================================

module procedure value_to_i64



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

end procedure value_to_i64


!===============================================================================

module procedure value_to_i64_array



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

end procedure value_to_i64_array


!===============================================================================

module procedure value_to_f32_array



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

end procedure value_to_f32_array


!===============================================================================

module procedure value_to_f64_array



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

end procedure value_to_f64_array


!===============================================================================

module procedure struct_reg_set

	! Register (or overwrite, e.g. on REPL struct redeclaration) a struct's
	! member names under its canonical cookie, and return the slot index.
	! Called once per struct declaration by parse_struct_declaration()
	! (parse_fn.f90), which stashes the returned idx on every value_t built
	! from that struct (c.f. value_t%struct_reg_idx) so that
	! value_to_str()'s round-trip struct branch below can look member names
	! up with a plain array index -- this hash-by-cookie lookup only runs
	! at parse time (once per declaration), never per print call


	!********

	integer :: i, new_cap
	logical :: found

	type(string_vector_t), allocatable :: tmp(:)

	if (struct_reg_map%capacity <= 0) call struct_reg_map%init(64)

	found = struct_reg_map%get(cookie, idx)
	if (.not. found) then

		struct_reg_len = struct_reg_len + 1
		idx = struct_reg_len
		call struct_reg_map%set(cookie, idx)

		if (.not. allocated(struct_reg_names)) then
			allocate(struct_reg_names(64))

		else if (idx > size(struct_reg_names)) then

			! Grow by moving ownership of each entry's name vector into the
			! bigger array, instead of a whole-array assignment of a type
			! with a nested allocatable
			new_cap = 2 * size(struct_reg_names)
			allocate(tmp(new_cap))
			do i = 1, struct_reg_len - 1
				call move_alloc(struct_reg_names(i)%v, tmp(i)%v)
			end do
			call move_alloc(tmp, struct_reg_names)

		end if
	end if

	if (allocated(struct_reg_names(idx)%v)) deallocate(struct_reg_names(idx)%v)
	allocate(struct_reg_names(idx)%v( size(names) ))
	do i = 1, size(names)
		struct_reg_names(idx)%v(i)%s = names(i)%s
	end do

end procedure struct_reg_set


!===============================================================================

module procedure value_to_str


	! When .true., render str members quoted/escaped and give f32/i64
	! scalars an explicit `'f32`/`'i64` type suffix, so the result lexes back
	! to an equivalent syntran literal.  Only meant to be set by the
	! struct_type case below (labeling members also implies round-trip mode
	! for their values) -- top-level println()/str() calls never pass this,
	! so plain `println("hi")` and `println(1.5f)` are unaffected


	!********

	integer :: j
	integer(kind = 8) :: i8, prod, n

	logical :: rt, has_names

	type(char_vector_t) :: str_vec

	!print *, "val type = ", kind_name(val%type)

	rt = .false.
	if (present(roundtrip)) rt = roundtrip

	select case (val%type)

		case (struct_type)

			str_vec = new_char_vector()
			call str_vec%push(val%struct_name//"{")

			n = size(val%struct)

			! Member names are looked up by a plain array index into the
			! module-level struct_reg_names registry, resolved once at parse
			! time (c.f. struct_reg_set()) and carried on every struct value
			! as %struct_reg_idx -- no hashing/lookup-by-cookie here, since
			! this runs on every print call.  Fall back to unlabeled
			! values-only output if the index is unset/out of range or stale
			! (member count mismatch) rather than mislabeling
			has_names = .false.
			if (val%struct_reg_idx >= 1 .and. val%struct_reg_idx <= struct_reg_len) then
				if (allocated(struct_reg_names(val%struct_reg_idx)%v)) then
					has_names = size(struct_reg_names(val%struct_reg_idx)%v) == n
				end if
			end if

			do i8 = 1, n

				if (has_names) then
					call str_vec%push( struct_reg_names(val%struct_reg_idx)%v(i8)%s//" = " )
				end if

				! Once inside a struct, member values must round-trip too
				call str_vec%push( trimw(val%struct(i8)%to_str(roundtrip = .true.)) )

				if (i8 < n) call str_vec%push(", ")

			end do
			call str_vec%push("}")
			ans = str_vec%trim()

		case (enum_type)
			ans = val%enum_name//"."//val%enum_variant

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
					if (rt) call str_vec%push("'i64")
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
					if (rt) call str_vec%push("'f32")

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

					if (rt) then
						call str_vec%push(quote_escape(val%array%str(i8)%s))
					else
						call str_vec%push(val%array%str(i8)%s)
					end if

					if (i8 >= val%array%len_) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i8, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (any(val%array%type == [struct_type, enum_type])) then

				n = size(val%struct)
				do i8 = 1, n
					! Just recurse instead of nesting a loop
					call str_vec%push( val%struct(i8)%to_str(roundtrip = rt) )
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
			! Bare (non-member) strings print raw, unquoted -- that's the
			! ordinary, expected behavior for most languages.  Quoting only
			! kicks in when this is a struct member (rt = .true.), so that
			! struct print output can be pasted back in as valid syntran
			if (allocated(val%str)) then
				if (rt) then
					ans = quote_escape(val%str%s)
				else
					ans = val%str%s
				end if
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

		case (fn_type)
			ans = value_type_name(val)

		case default
			ans = val%sca%to_str(val%type)

			! f32/i64 scalars need an explicit type suffix to round-trip:
			! without it, a bare literal like `1.500000E+00` or `7` re-lexes
			! as f64/i32 respectively, and pasting it back into a struct
			! member of type f32/i64 is an E65 type mismatch
			if (rt .and. val%type == f32_type) then
				ans = trimw(ans)//"'f32"
			else if (rt .and. val%type == i64_type) then
				ans = trimw(ans)//"'i64"
			end if

	end select

end procedure value_to_str


!===============================================================================

module procedure value_type_name

	! Single source of truth for rendering a value_t's type as user-facing
	! text, e.g. "i32", "[f64; :]", "MyStruct", "fn(i32): i32".  Used both for
	! printing a fn-pointer value (value_to_str's fn_type case, below) and --
	! via types_ops.f90's type_name(), which just delegates here -- for
	! diagnostic messages (bad-arg-type, etc.).  c.f. lookup_type() which is
	! mostly the inverse of this
	!
	! This lives in value.f90, not types_ops.f90, so that fn_type's signature
	! rendering (which needs this same logic for its param/return types) can
	! call it directly: syntran__types_m depends on syntran__value_m, not the
	! other way around, so only this direction avoids a circular dependency



	!********

	character(len = :), allocatable :: array_name

	integer :: i

	if (a%type == struct_type) then
		str_ = a%struct_name
	else if (a%type == enum_type) then
		str_ = a%enum_name
	else if (a%type == array_type) then

		if (a%array%type == struct_type) then
			array_name = a%struct_name
		else if (a%array%type == enum_type) then
			array_name = a%enum_name
		else
			array_name = value_type_name_primitive(a%array%type)
		end if

		str_ = "["//array_name//"; "

		! Repeat ":, " appropriately
		str_ = str_//repeat(":, ", max(a%array%rank - 1, 0))
		str_ = str_//":]"

	else if (a%type == fn_type) then

		str_ = "fn("
		if (allocated(a%fn_params)) then
			do i = 1, size(a%fn_params)
				str_ = str_//value_type_name(a%fn_params(i))
				if (i < size(a%fn_params)) str_ = str_//", "
			end do
		end if
		str_ = str_//")"

		if (allocated(a%fn_ret)) then
			if (a%fn_ret%type /= void_type) str_ = str_//": "//value_type_name(a%fn_ret)
		end if

	else
		str_ = value_type_name_primitive(a%type)
	end if

end procedure value_type_name


!===============================================================================

module procedure value_type_name_primitive
	! Primitive (non-struct/array/fn) type-name mapping.  c.f. lookup_type()
	! which is mostly the inverse of this



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
	case (void_type)
		str_ = "void"
	case (fn_type)
		str_ = "fn"
	case default
		str_ = "unknown"
	end select

end procedure value_type_name_primitive


!===============================================================================

module procedure scalar_to_str




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

end procedure scalar_to_str


!===============================================================================

end submodule syntran__value_impl_m

!===============================================================================
