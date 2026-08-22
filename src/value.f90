
!===============================================================================

module syntran__value_m

	use syntran__consts_m
	use syntran__errors_m
	use syntran__utils_m

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

		! Fn pointer runtime dispatch key: id_index into fns%fns()/prog%fn_entry()
		! for the target user-defined fn.  Only meaningful when value_t%type ==
		! fn_type.  Lives in this POD scalar_t so value_t%sca copies (both
		! value_copy's `dst%sca = src%sca` and value_move's default-case scalar
		! copy) carry it for free
		integer           :: fn_index = 0

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

		! Index into value_to_str()'s struct_reg_names registry (0 = unset),
		! resolved once at parse time (c.f. struct_reg_set()) and copied
		! alongside struct_cookie ever since.  Plain non-allocatable integer,
		! so it's a cheap array index at print time instead of a per-print
		! hash lookup by struct_cookie
		integer :: struct_reg_idx = 0

		! Enum type name, e.g. "Dir", used when value%type == enum_type.
		! Mirrors struct_name's role for type-descriptor rendering (fn param/
		! return types, etc.)
		character(len = :), allocatable :: enum_name

		! Enum variant name, e.g. "North", for an actual enum value (as
		! opposed to a bare enum type descriptor, which leaves this
		! unallocated).  Printed as "<enum_name>.<enum_variant>".  The
		! backing i32 ordinal is stored in sca%i32
		character(len = :), allocatable :: enum_variant

		! Canonical, alias-independent enum identity: "<defining src
		! file>::<local enum name>".  Used for type matching, mirroring
		! struct_cookie above
		character(len = :), allocatable :: enum_cookie

		! Fn pointer signature, used when value%type == fn_type.  fn_params(i)
		! and fn_ret carry only the *type* of each param/return (like fn_t%params
		! /fn_t%type in types.f90), not runtime values -- used for type-checking
		! calls through a fn pointer and for rendering the signature in
		! type_name().  Self-referential value_t is fine (c.f. struct(:) above);
		! only two types containing *each other* breaks gfortran
		type(value_t), allocatable :: fn_params(:)
		type(value_t), allocatable :: fn_ret

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

	!********

	! Struct member-name registry, keyed by struct_cookie ("<src file>::
	! <StructName>"), used so value_to_str() can label a struct's members by
	! name.  This lives here rather than being passed in as an argument
	! because struct_t/structs_t (types.f90) are declared in syntran__types_m,
	! which itself `use`s syntran__value_m -- passing them down would be a
	! circular module dependency (c.f. value_type_name()'s note above on the
	! same constraint).  Populated once per struct declaration by
	! parse_struct_declaration() (parse_fn.f90), read at print time
	type(map_i32_t), save :: struct_reg_map
	type(string_vector_t), allocatable, save :: struct_reg_names(:)
	integer, save :: struct_reg_len = 0

!===============================================================================


	! Bodies live in value_impl.f90 so that editing them does not
	! invalidate this module's .mod and rebuild the tree -- only the
	! .smod changes, which fpm's dependents don't recompile on.
	interface

		module function new_value_vector() result(vector)
			type(value_vector_t) :: vector
		end function new_value_vector

		module subroutine push_value(vector, val)
			class(value_vector_t) :: vector
			type(value_t) :: val
		end subroutine push_value

		module subroutine push_value_move(vector, val)
			class(value_vector_t) :: vector
			type(value_t), intent(inout) :: val   ! consumed; undefined after return
		end subroutine push_value_move

		module subroutine value_reset(val)
			type(value_t), intent(inout) :: val
		end subroutine value_reset

		module recursive subroutine value_move(src, dst)
			type(value_t), intent(inout) :: src
			type(value_t), intent(out)   :: dst
		end subroutine value_move

		module subroutine array_move(src, dst)
			type(array_t), intent(inout) :: src
			type(array_t), intent(out)   :: dst
		end subroutine array_move

		module subroutine array_copy(dst, src)
			type(array_t), intent(inout) :: dst
			type(array_t), intent(in)    :: src
		end subroutine array_copy

		module recursive subroutine value_copy(dst, src)
			class(value_t), intent(inout) :: dst
			type(value_t),  intent(in)    :: src
		end subroutine value_copy

		module subroutine array_destroy(arr)
			type(array_t), intent(inout) :: arr
		end subroutine array_destroy

		module recursive subroutine value_destroy(val)
			type(value_t), intent(inout) :: val
		end subroutine value_destroy

		module subroutine value_array_destroy(vals)
			type(value_t), allocatable, intent(inout) :: vals(:)
		end subroutine value_array_destroy

		module subroutine value_array_copy(dst, src)
			type(value_t), allocatable, intent(inout) :: dst(:)
			type(value_t), intent(in) :: src(:)
		end subroutine value_array_copy

		module function mold(mold_, type_) result(array)
			type(array_t), intent(in) :: mold_
			integer, intent(in) :: type_
			type(array_t), allocatable :: array
		end function mold

		module subroutine copy_composite_id(dst, src)
			type(value_t), intent(inout) :: dst
			type(value_t), intent(in)    :: src
		end subroutine copy_composite_id

		module subroutine push_array(vector, val)
			class(array_t) :: vector
			type(value_t)  :: val
		end subroutine push_array

		module subroutine trim_array(vector)
			class(array_t) :: vector
		end subroutine trim_array

		module function value_to_f32(val) result(ans)
			class(value_t) :: val
			real(kind = 4) :: ans
		end function value_to_f32

		module function value_to_f64(val) result(ans)
			class(value_t) :: val
			real(kind = 8) :: ans
		end function value_to_f64

		module function value_to_i32(val) result(ans)
			class(value_t) :: val
			integer(kind = 4) :: ans
		end function value_to_i32

		module function value_to_i32_array(val) result(ans)
			class(value_t) :: val
			type(array_t) :: ans
		end function value_to_i32_array

		module function value_to_i64(val) result(ans)
			class(value_t) :: val
			integer(kind = 8) :: ans
		end function value_to_i64

		module function value_to_i64_array(val) result(ans)
			class(value_t) :: val
			type(array_t) :: ans
		end function value_to_i64_array

		module function value_to_f32_array(val) result(ans)
			class(value_t) :: val
			type(array_t) :: ans
		end function value_to_f32_array

		module function value_to_f64_array(val) result(ans)
			class(value_t) :: val
			type(array_t) :: ans
		end function value_to_f64_array

		module function struct_reg_set(cookie, names) result(idx)
			character(len = *), intent(in) :: cookie
			type(string_t), intent(in) :: names(:)
			integer :: idx
		end function struct_reg_set

		module recursive function value_to_str(val, roundtrip) result(ans)
			class(value_t) :: val
			logical, intent(in), optional :: roundtrip
			character(len = :), allocatable :: ans
		end function value_to_str

		module recursive function value_type_name(a) result(str_)
			type(value_t), intent(in) :: a
			character(len = :), allocatable :: str_
		end function value_type_name

		module function value_type_name_primitive(itype) result(str_)
			integer, intent(in) :: itype
			character(len = :), allocatable :: str_
		end function value_type_name_primitive

		module recursive function scalar_to_str(val, type) result(ans)
			class(scalar_t) :: val
			integer, intent(in) :: type
			character(len = :), allocatable :: ans
		end function scalar_to_str

	end interface

end module syntran__value_m

!===============================================================================

