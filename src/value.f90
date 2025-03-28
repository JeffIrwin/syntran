
!===============================================================================

module syntran__value_m

	use syntran__consts_m
	use syntran__errors_m
	!use syntran__types_m
	!use syntran__utils_m

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
		! Do we need a separate iostat beyond eof?

	end type file_t

	!********

	type scalar_t

		! Scalar value type.  Cannot be an array!

		type(file_t)      :: file_
		type(string_t)    :: str

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

		type(scalar_t) :: sca

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

		contains
			procedure :: to_str => value_to_str
			procedure :: to_f32 => value_to_f32
			procedure :: to_f64 => value_to_f64
			procedure :: to_i32 => value_to_i32
			procedure :: to_i64 => value_to_i64
			procedure :: to_i32_array => value_to_i32_array  ! for user-facing casting fn
			procedure :: to_i64_array => value_to_i64_array
			procedure, pass(dst) :: copy => value_copy
			generic, public :: assignment(=) => copy

	end type value_t

	type value_vector_t
		type(value_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_value
	end type value_vector_t

!===============================================================================

contains

!===============================================================================

function new_value_vector() result(vector)

	type(value_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_value_vector

!===============================================================================

subroutine push_value(vector, val)

	class(value_vector_t) :: vector
	type(value_t) :: val

	!********

	type(value_t), allocatable :: tmp(:)

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
	!print *, 'done push_value'

end subroutine push_value

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

recursive subroutine value_move(src, dst)
	! Note the args are reversed wrt value_copy.  It is however consistent with
	! build-in move_alloc() (and `mv file1 file2`)
	!
	! This is kind of a fake move.  Arrays and structs are moved, but primitive
	! scalars are just copied

	type(value_t), intent(inout) :: src
	type(value_t), intent(out)   :: dst

	!********

	integer :: i

	if (debug > 3) print *, 'starting value_move()'

	dst%type = src%type

	select case (src%type)
	case (array_type)
		call move_alloc(src%array, dst%array)

	case (struct_type)
		call move_alloc(src%struct_name, dst%struct_name)
		call move_alloc(src%struct, dst%struct)

	case default
		! This copy (not move) could be inefficient for large string scalars.
		! Might be worth making value%sca allocatable if it doesn't add too much
		! complexity.  Otherwise, consider further selecting the case by each
		! scalar type
		dst%sca  = src%sca

	end select

end subroutine value_move

!===============================================================================

recursive subroutine value_copy(dst, src)

	! Deep copy.  Default Fortran assignment operator doesn't handle recursion
	! correctly for my types, leaving dangling refs to src when it is
	! deallocated.
	!
	! Args have to be in the confusing dst, src order for overloading

	class(value_t), intent(inout) :: dst
	class(value_t), intent(in)    :: src

	!********

	integer :: i

	if (debug > 3) print *, 'starting value_copy()'

	dst%type = src%type
	dst%sca  = src%sca

	if (allocated(src%struct_name)) then
		dst%struct_name = src%struct_name
	end if

	if (allocated(src%array)) then
		if (.not. allocated(dst%array)) allocate(dst%array)
		dst%array = src%array
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
			write(*,*) err_int_prefix//'push_array type not implemented'
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
		vector%str ( vector%len_ ) = val%sca%str
	case default
		write(*,*) err_int_prefix//'push_array type not implemented'
		call internal_error()
	end select

end subroutine push_array

!===============================================================================

subroutine trim_array(vector)

	class(array_t) :: vector

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
		write(*,*) err_int_prefix//'trim_array() implemented for this type'//color_reset
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
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to f32.  Use `parse_f32()`'//color_reset
			call internal_error()

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to f32 '//color_reset
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

			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to f64.  Use `parse_f64()`'//color_reset
			call internal_error()

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to f64 '//color_reset
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

			if (len(val%sca%str%s) == 1) then
				ans = iachar(val%sca%str%s)
			else
				write(*,*) err_int_prefix//'cannot convert from type `' &
					//kind_name(val%type)//'` to i32.  Use `parse_i32()`'//color_reset
				call internal_error()
			end if

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 '//color_reset
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
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 '//color_reset
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
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i64.  Use `parse_i64()`'//color_reset
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
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i64 '//color_reset
			call internal_error()

	end select

end function value_to_i64_array

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

		case (str_type)
			! TODO: wrap str in quotes for clarity, both scalars and str array
			! elements?  This would be a breaking change.  Update tests.
			ans = val%str%s

		case (file_type)
			ans = "{file_unit: "//str(val%file_%unit_)//", filename: """// &
				val%file_%name_//"""}"

		case default
			ans = err_prefix//"<invalid_value>"//color_reset

	end select

end function scalar_to_str

!===============================================================================

end module syntran__value_m

!===============================================================================

