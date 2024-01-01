
!===============================================================================

module syntran__types_m

	use syntran__errors_m
	use syntran__utils_m

	implicit none

	! Split parameters to constants module?  But it's nice to have kind_name()
	! et al. in the same file as the related constants

	! Debug logging verbosity (0 == silent)
	integer, parameter :: debug = 0

	integer :: maxerr  ! TODO: move this (not default) into a settings struct that gets passed around
	integer, parameter :: maxerr_def = 4

	! Must be larger than largest token enum below
	integer, parameter :: magic = 128

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
			all_sub              = 81, &
			include_keyword      = 80, &
			hash_token           = 79, &
			percent_equals_token = 78, &
			sstar_equals_token   = 77, &
			i64_token            = 76, &
			i64_type             = 75, &
			file_type            = 74, &
			slash_equals_token   = 73, &
			step_sub             = 72, &
			range_sub            = 71, &
			scalar_sub           = 70, &
			star_equals_token    = 69, &
			minus_equals_token   = 68, &
			plus_equals_token    = 67, &
			percent_token        = 66, &
			str_token            = 65, &
			str_type             = 64, &
			any_type             = 63, &
			void_type            = 62, &
			fn_keyword           = 61, &
			fn_declaration       = 60, &
			translation_unit     = 59, &
			fn_call_expr         = 58, &
			unknown_type         = 57, &
			comma_token          = 56, &
			array_type           = 55, &
			array_expr           = 54, &
			expl_array           = 53, &
			impl_array           = 52, &
			f32_type             = 51, &
			f32_token            = 50, &
			greater_equals_token = 49, &
			greater_token        = 48, &
			less_equals_token    = 47, &
			less_token           = 46, &
			let_expr             = 45, &
			while_statement      = 44, &
			colon_token          = 43, &
			for_statement        = 42, &
			lbracket_token       = 41, &
			rbracket_token       = 40, &
			if_statement         = 39, &
			while_keyword        = 38, &
			in_keyword           = 37, &
			for_keyword          = 36, &
			else_keyword         = 35, &
			if_keyword           = 34, &
			semicolon_token      = 33, &
			block_statement      = 32, &
			expr_statement       = 31, &
			lbrace_token         = 30, &
			rbrace_token         = 29, &
			sstar_token          = 28, &
			let_keyword          = 27, &
			name_expr            = 26, &
			equals_token         = 25, & ! '='
			assignment_expr      = 24, &
			bang_equals_token    = 23, &
			eequals_token        = 22, & ! '=='
			and_keyword          = 21, &
			or_keyword           = 20, &
			not_keyword          = 19, &
			bool_type            = 18, &
			literal_expr         = 17, &
			true_keyword         = 16, &
			false_keyword        = 15, &
			identifier_token     = 14, &
			unary_expr           = 13, &
			lparen_token         = 12, &
			rparen_token         = 11, &
			i32_type             = 10, &
			binary_expr          =  9, &
			star_token           =  8, &
			slash_token          =  7, &
			bad_token            =  6, &
			plus_token           =  5, &
			minus_token          =  4, &
			whitespace_token     =  3, &
			i32_token            =  2, &
			eof_token            =  1

	!********

	type file_t
		character(len = :), allocatable :: name_
		integer :: unit_  ! fortran file unit
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

		contains
			procedure :: to_str => scalar_to_str

	end type scalar_t

	!********

	type array_t

		! The array type is i32_type, f32_type, etc. while the kind is
		! impl_array (bound-based) or expl_array (CSV list)
		integer :: type, kind
		!type(value_t), allocatable :: lbound, step, ubound
		type(scalar_t), allocatable :: lbound, step, ubound

		! Note that these are arrays of primitive Fortran types, instead of
		! arrays of generic value_t.  This performs better since we can put
		! a type select/case outside of loops for processing arrays, as opposed
		! to inside of a loop for type selection of every element
		logical(kind = 1), allocatable :: bool(:)

		integer(kind = 4), allocatable ::  i32(:)
		integer(kind = 8), allocatable ::  i64(:)

		real   (kind = 4), allocatable ::  f32(:)

		type(string_t   ), allocatable ::  str(:)

		! TODO: file arrays

		integer :: rank
		integer(kind = 8) :: len_, cap
		integer(kind = 8), allocatable :: size(:)

		contains
			procedure :: push => push_array

	end type array_t

	!********

	type value_t
		integer :: type

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

		contains
			procedure :: to_str => value_to_str
			procedure :: to_i32 => value_to_i32
			procedure :: to_i64 => value_to_i64

	end type value_t

	!********

	interface add
		module procedure add_value_t
	end interface add

	interface subtract
		module procedure subtract_value_t
	end interface subtract

	interface mul
		module procedure mul_value_t
	end interface mul

	interface div
		module procedure div_value_t
	end interface div

	interface pow
		module procedure pow_value_t
	end interface pow

	interface mod_
		module procedure modulo_value_t
	end interface mod_

	!********

	type param_t
		! Function parameter (argument)

		integer :: type
		character(len = :), allocatable :: name

		integer :: array_type, rank

		! TODO: add a way to represent polymorphic intrinsic fn params, e.g.
		! i32 min(1, 2) vs f32 min(1.0, 2.0), but not bool min(true, false).
		! Maybe add an array of types(:) for each allowable type of a param?

	end type param_t

	!********

	type fn_t
		! Function signature: input and output types

		! Return type
		integer :: type, array_type, rank

		! Arguments/parameters.  Technically, "arguments" in most languages are
		! what Fortran calls "actual arguments" and "parameters" are Fortran
		! "dummy arguments"
		type(param_t), allocatable :: params(:)

		! Min number of variadic params.  Default < 0 means fn is not variadic
		!
		! This works for fns like max() or print() which have a min limit but an
		! unlimited upper bound of parameters.  For functions with a fixed
		! number of optional parameters, there should be a different mechanism

		integer :: variadic_min = -1, variadic_type

		! Reference to the function definition, i.e. the syntax node containing
		! the function parameters and body
		!
		! TODO: my experience has taught me that Fortran pointers are extremely
		! dangerous (see memory leaks due to now removed value_t -> array_t
		! pointer).  Can we avoid having a pointer here and make it allocatable
		! instead?
		type(syntax_node_t), pointer :: node => null()

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

	! Fixed-size limit to the scope level for now
	integer, parameter :: scope_max = 64

	type fns_t

		! A list of function dictionaries for each scope level used during
		! parsing
		type(fn_dict_t) :: dicts(scope_max)

		! Flat array of fns from all scopes, used for efficient interpreted
		! evaluation
		type(fn_t), allocatable :: fns(:)

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

		integer :: kind

		! This structure could be more efficient.  For example, unary
		! expressions don't use left, name expressions don't use left or right,
		! binary expressions don't use an identifier, etc.  On the other hand,
		! they're all allocatable, so they shouldn't take up much space if not
		! allocated

		type(syntax_node_t), allocatable :: left, right, members(:), &
			condition, if_clause, else_clause, body, array

		! Array expression syntax nodes.  TODO: rename lbound, ubound to avoid
		! conflicts w/ Fortran keywords
		type(syntax_node_t), allocatable :: lbound, step, ubound, len_, &
			elems(:), rank

		! TODO: rename `size`
		type(syntax_node_t), allocatable :: lsubscripts(:), size(:), args(:), &
			usubscripts(:)!, ssubscripts(:) !TODO: subscript step

		! Either scalar_sub, range_sub (unit step [0:2]), all_sub ([:]), or
		! step_sub ([0: 2: 8])
		integer :: sub_kind

		type(syntax_token_t) :: op, identifier
		integer :: id_index

		integer, allocatable :: params(:)

		type(value_t) :: val

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

			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy

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
		type(var_dict_t) :: dicts(scope_max)

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

	end type vars_t

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
	end type syntax_node_vector_t

!===============================================================================

contains

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
			write(*,*) 'Error: push_array type not implemented'
			call internal_error()
		end if

		vector%cap = tmp_cap

	end if

	if      (vector%type == i32_type) then
		vector%i32 ( vector%len_ ) = val%sca%i32
	else if (vector%type == i64_type) then
		vector%i64 ( vector%len_ ) = val%sca%i64
	else if (vector%type == f32_type) then
		vector%f32 ( vector%len_ ) = val%sca%f32
	else if (vector%type == bool_type) then
		vector%bool( vector%len_ ) = val%sca%bool
	else if (vector%type == str_type) then
		vector%str ( vector%len_ ) = val%sca%str
	else
		write(*,*) 'Error: push_array type not implemented'
		call internal_error()
	end if

end subroutine push_array

!===============================================================================

recursive function scalar_to_str(val, type) result(ans)

	class(scalar_t) :: val

	integer, intent(in) :: type

	character(len = :), allocatable :: ans

	!********

	character(len = 16) :: buf16
	character(len = 32) :: buffer

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

		case (i32_type)
			write(buffer, '(i0)') val%i32
			ans = trim(buffer)

		case (i64_type)
			write(buffer, '(i0)') val%i64
			ans = trim(buffer)

		case (str_type)
			! TODO: wrap str in quotes for clarity, both scalars and str array
			! elements.  Update tests.
			ans = val%str%s

		case (file_type)
			ans = "{file_unit: "//str(val%file_%unit_)//", filename: """// &
				val%file_%name_//"""}"

		case default
			ans = err_prefix//"<invalid_value>"//color_reset

	end select

end function scalar_to_str

!===============================================================================

function value_to_i32(val) result(ans)

	class(value_t) :: val

	integer(kind = 4) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 4)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			ans = int(val%sca%i64, 4)

		case (str_type)

			if (len(val%sca%str%s) == 1) then
				ans = iachar(val%sca%str%s)
			else
				write(*,*) err_int_prefix//'cannot convert from type `' &
					//kind_name(val%type)//'` to i32 '//color_reset
				call internal_error()
			end if

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i32 '//color_reset
			call internal_error()

	end select

end function value_to_i32

!===============================================================================

function value_to_i64(val) result(ans)

	class(value_t) :: val

	integer(kind = 8) :: ans

	select case (val%type)

		case (f32_type)
			ans = int(val%sca%f32, 8)

		case (i32_type)
			ans = val%sca%i32

		case (i64_type)
			!write(buffer, '(i0)') val%sca%i64
			!ans = trim(buffer)
			ans = val%sca%i64

		case default
			write(*,*) err_int_prefix//'cannot convert from type `' &
				//kind_name(val%type)//'` to i64 '//color_reset
			call internal_error()

	end select

end function value_to_i64

!===============================================================================

recursive function value_to_str(val) result(ans)

	class(value_t) :: val

	character(len = :), allocatable :: ans

	!********

	!character(len = 16) :: buf16

	integer :: j
	integer(kind = 8) :: i8, prod

	!type(string_vector_t) :: str_vec
	type(char_vector_t) :: str_vec

	select case (val%type)

		case (array_type)

			! This whole case could be an array_to_str() fn

			if (val%array%kind == impl_array) then
				ans = '['//val%array%lbound%to_str(val%array%type)//': ' &
				         //val%array%ubound%to_str(val%array%type)//']'
				return
			end if

			!print *, 'array type = ', val%array%type

			!! You would think that this would help
			!if (val%array%type == i32_type) then
			!	str_vec = new_char_vector( 12 * val%array%len_ )
			!else if (val%array%type == f32_type) then
			!	str_vec = new_char_vector( 16 * val%array%len_ )
			!end if
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

			else
				write(*,*) 'Error: array ans conversion not implemented' &
					//' for this type'
				call internal_error()
			end if

			if (val%array%rank > 1) call str_vec%push(line_feed)
			call str_vec%push(']')

			ans = str_vec%v( 1: str_vec%len_ )

		case default
			ans = val%sca%to_str(val%type)

	end select

end function value_to_str

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

	val = fn_ternary_search(dict%dicts(i)%root, key, id_index, io)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		val = fn_ternary_search(dict%dicts(i)%root, key, id_index, io)
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
	integer, intent(in) :: id_index

	integer, intent(out), optional :: iostat
	logical, intent(in), optional :: overwrite

	!********

	integer :: i, io
	logical :: overwritel

	!print *, 'inserting ', quote(key)

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call fn_ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine fn_insert

!===============================================================================

recursive function syntax_node_str(node, indent) result(str)

	! Convert tree to string in JSON-ish format.  Incomplete since I've added so
	! many new members

	class(syntax_node_t) :: node

	character(len = *), optional :: indent

	character(len = :), allocatable :: str

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

	str = line_feed// &
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

	dst%kind = src%kind
	dst%op   = src%op

	dst%val  = src%val
	!dst%val%sca%file_     = src%val%sca%file_
	!dst%val%sca%file_%eof = src%val%sca%file_%eof

	dst%identifier  = src%identifier
	dst%id_index    = src%id_index

	dst%expecting       = src%expecting
	dst%first_expecting = src%first_expecting

	dst%first_expected = src%first_expected
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
		dst%left = src%left
	else if (allocated(dst%left)) then
		deallocate(dst%left)
	end if

	if (allocated(src%right)) then
		!if (debug > 1) print *, 'copy() right'
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	else if (allocated(dst%right)) then
		deallocate(dst%right)
	end if

	if (allocated(src%params)) then
		! Primitive int array.  No need to explicitly allocate
		dst%params = src%params
	else if (allocated(dst%params)) then
		deallocate(dst%params)
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

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine var_insert

!===============================================================================

function var_search(dict, key, id_index, iostat) result(val)

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

	val = ternary_search(dict%dicts(i)%root, key, id_index, io)

	! If not found in current scope, search parent scopes too
	do while (io /= exit_success .and. i > 1)
		i = i - 1
		val = ternary_search(dict%dicts(i)%root, key, id_index, io)
	end do

	if (present(iostat)) iostat = io

end function var_search

!===============================================================================

subroutine push_scope(dict)

	class(vars_t) :: dict

	dict%scope = dict%scope + 1

	! TODO: make a growable array of dicts for unlimited scope levels
	if (dict%scope > scope_max) then
		write(*,*) 'Error: too many nested blocks > '//str(scope_max)
		call internal_error()
	end if

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
		write(*,*) 'Error: scope stack is empty'
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

subroutine add_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!********

	!print *, 'starting add_value_t()'
	!print *, 'res%type = ', kind_name(res%type)

	! Case selector must be a scalar expression, so use this nasty hack.
	! This will break if magic is smaller than the largest type enum
	! parameter
	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 + right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 + right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 + right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 + right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	! Usually, adding f32 to i32 casts to an i32 result.  But for compound
	! assignment we may want to make it an i32, e.g. i += 3.1;
	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 + right%sca%i32)
	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 + right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 + right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 + right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 + right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i64_type)
		res%sca%f32 = left%sca%f32 + real(right%sca%i64)

	case        (magic**2 * f32_type + magic * i64_type + f32_type)
		res%sca%f32 = real(left%sca%i64) + right%sca%f32

	case        (magic**2 * str_type + magic * str_type + str_type)
		res%sca%str%s = left%sca%str%s // right%sca%str%s

	case default
		! FIXME: other numeric types (f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine add_value_t

!===============================================================================

subroutine mul_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 * right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 * right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 * right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 * right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 * right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 * right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 * right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 * right%sca%i32

	case        (magic**2 * f32_type + magic * f32_type + i64_type)
		res%sca%f32 = left%sca%f32 * real(right%sca%i64)

	case        (magic**2 * f32_type + magic * i64_type + f32_type)
		res%sca%f32 = real(left%sca%i64) * right%sca%f32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 * right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine mul_value_t

!===============================================================================

subroutine div_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 / right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 / right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 / right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 / right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 / right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 / right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 / right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 / right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 / right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine div_value_t

!===============================================================================

subroutine subtract_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 - right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 - right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 - right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 - right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 - right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 - right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 - right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 - right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 - right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine subtract_value_t

!===============================================================================

subroutine modulo_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	! The Fortran mod() fn is consistent with the C operator `%`, while modulo()
	! works differently for negative args (it's actually a remainder function,
	! not a true modulo)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = mod(left%sca%i32, right%sca%i32)

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = mod(left%sca%i64, right%sca%i64)

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = mod(left%sca%i64, int(right%sca%i32, 8))

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = mod(int(left%sca%i32, 8), right%sca%i64)

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = mod(int(left%sca%f32), right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = mod(left%sca%i32, int(right%sca%f32))

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = mod(left%sca%f32, right%sca%f32)

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = mod(left%sca%f32, real(right%sca%i32))

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = mod(real(left%sca%i32), right%sca%f32)

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine modulo_value_t

!===============================================================================

subroutine pow_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%sca%i32 = left%sca%i32 ** right%sca%i32

	case        (magic**2 * i64_type + magic * i64_type + i64_type)
		res%sca%i64 = left%sca%i64 ** right%sca%i64

	case        (magic**2 * i64_type + magic * i64_type + i32_type)
		res%sca%i64 = left%sca%i64 ** right%sca%i32

	case        (magic**2 * i64_type + magic * i32_type + i64_type)
		res%sca%i64 = left%sca%i32 ** right%sca%i64

	! TODO: i64/f32 casting, i32 LHS w/ i64 RHS

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%sca%i32 = int(left%sca%f32 ** right%sca%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%sca%i32 = int(left%sca%i32 ** right%sca%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%sca%f32 = left%sca%f32 ** right%sca%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%sca%f32 = left%sca%f32 ** right%sca%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%sca%f32 = left%sca%i32 ** right%sca%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine pow_value_t

!===============================================================================

subroutine internal_error()

	write(*,*) fg_bold_bright_red//'Fatal error'//color_reset
	call exit(exit_failure)

end subroutine internal_error

!===============================================================================

function kind_token(kind)

	integer, intent(in) :: kind

	character(len = :), allocatable :: kind_token

	character(len = *), parameter :: tokens(*) = [ &
			"End of file          ", & !  1
			"[0-9]                ", & !  2
			"[\s]                 ", & !  3
			"-                    ", & !  4
			"+                    ", & !  5
			"Bad token            ", & !  6
			"/                    ", & !  7
			"*                    ", & !  8
			"Binary expression    ", & !  9
			"i32 expression       ", & ! 10
			")                    ", & ! 11
			"(                    ", & ! 12
			"Unary expression     ", & ! 13
			"Identifier           ", & ! 14
			"false                ", & ! 15
			"true                 ", & ! 16
			"Literal expression   ", & ! 17
			"bool expression      ", & ! 18
			"not                  ", & ! 19
			"or                   ", & ! 20
			"and                  ", & ! 21
			"==                   ", & ! 22
			"!=                   ", & ! 23
			"Assignment expression", & ! 24
			"=                    ", & ! 25
			"Name expression      ", & ! 26
			"let                  ", & ! 27
			"**                   ", & ! 28
			"}                    ", & ! 29
			"{                    ", & ! 30
			"Expression statement ", & ! 31
			"Block statement      ", & ! 32
			";                    ", & ! 33
			"if                   ", & ! 34
			"else                 ", & ! 35
			"for                  ", & ! 36
			"in                   ", & ! 37
			"while                ", & ! 38
			"if statement         ", & ! 39
			"]                    ", & ! 40
			"[                    ", & ! 41
			"for                  ", & ! 42
			":                    ", & ! 43
			"while                ", & ! 44
			"let expression       ", & ! 45
			"<                    ", & ! 46
			"<=                   ", & ! 47
			">                    ", & ! 48
			">=                   ", & ! 49
			"[0-9.+-e]            ", & ! 50
			"f32 expression       ", & ! 51
			"Implicit array       ", & ! 52
			"Explicit array       ", & ! 53
			"Array expression     ", & ! 54
			"Array type           ", & ! 55
			",                    ", & ! 56
			"Unknown type         ", & ! 57
			"fn call expression   ", & ! 58
			"Translation unit     ", & ! 59
			"fn declaration       ", & ! 60
			"fn keyword           ", & ! 61
			"void type            ", & ! 62
			"any type             ", & ! 63
			"str type             ", & ! 64
			"str token            ", & ! 65
			"%                    ", & ! 66
			"+=                   ", & ! 67
			"-=                   ", & ! 68
			"*=                   ", & ! 69
			"scalar subript       ", & ! 70
			"range subript        ", & ! 71
			"step subcript        ", & ! 72
			"/=                   ", & ! 73
			"file type            ", & ! 74
			"i64 type             ", & ! 75
			"i64 token            ", & ! 76
			"**= token            ", & ! 77
			"%=                   ", & ! 78
			"#                    ", & ! 79
			"include              ", & ! 80
			"all_sub              ", & ! 81
			"unknown              "  & ! inf
		]

	if (.not. (1 <= kind .and. kind <= size(tokens))) then
		kind_token = "unknown"
		return
	end if

	kind_token = trim(tokens(kind))

end function kind_token

!===============================================================================

function kind_name(kind)

	integer, intent(in) :: kind

	character(len = :), allocatable :: kind_name

	character(len = *), parameter :: names(*) = [ &
			"eof_token           ", & !  1
			"i32_token           ", & !  2
			"whitespace_token    ", & !  3
			"minus_token         ", & !  4
			"plus_token          ", & !  5
			"bad_token           ", & !  6
			"slash_token         ", & !  7
			"star_token          ", & !  8
			"binary_expr         ", & !  9
			"i32_type            ", & ! 10
			"rparen_token        ", & ! 11
			"lparen_token        ", & ! 12
			"unary_expr          ", & ! 13
			"identifier_token    ", & ! 14
			"false_keyword       ", & ! 15
			"true_keyword        ", & ! 16
			"literal_expr        ", & ! 17
			"bool_type           ", & ! 18
			"not_keyword         ", & ! 19
			"or_keyword          ", & ! 20
			"and_keyword         ", & ! 21
			"eequals_token       ", & ! 22
			"bang_equals_token   ", & ! 23
			"assignment_expr     ", & ! 24
			"equals_token        ", & ! 25
			"name_expr           ", & ! 26
			"let_keyword         ", & ! 27
			"sstar_token         ", & ! 28
			"rbrace_token        ", & ! 29
			"lbrace_token        ", & ! 30
			"expr_statement      ", & ! 31
			"block_statement     ", & ! 32
			"semicolon_token     ", & ! 33
			"if_keyword          ", & ! 34
			"else_keyword        ", & ! 35
			"for_keyword         ", & ! 36
			"in_keyword          ", & ! 37
			"while_keyword       ", & ! 38
			"if_statement        ", & ! 39
			"rbracket_token      ", & ! 40
			"lbracket_token      ", & ! 41
			"for_statement       ", & ! 42
			"colon_token         ", & ! 43
			"while_statement     ", & ! 44
			"let_expr            ", & ! 45
			"less_token          ", & ! 46
			"less_equals_token   ", & ! 47
			"greater_token       ", & ! 48
			"greater_equals_token", & ! 49
			"f32_token           ", & ! 50
			"f32_type            ", & ! 51
			"impl_array          ", & ! 52
			"expl_array          ", & ! 53
			"array_expr          ", & ! 54
			"array_type          ", & ! 55
			"comma_token         ", & ! 56
			"unknown_type        ", & ! 57
			"fn_call_expr        ", & ! 58
			"translation_unit    ", & ! 59
			"fn_declaration      ", & ! 60
			"fn_keyword          ", & ! 61
			"void_type           ", & ! 62
			"any_type            ", & ! 63
			"str_type            ", & ! 64
			"str_token           ", & ! 65
			"percent_token       ", & ! 66
			"plus_equals_token   ", & ! 67
			"minus_equals_token  ", & ! 68
			"star_equals_token   ", & ! 69
			"scalar_sub          ", & ! 70
			"range_sub           ", & ! 71
			"step_sub            ", & ! 72
			"slash_equals_token  ", & ! 73
			"file_type           ", & ! 74
			"i64_type            ", & ! 75
			"i64_token           ", & ! 76
			"sstar_equals_token  ", & ! 77
			"percent_equals_token", & ! 78
			"hash_token          ", & ! 79
			"include_keyword     ", & ! 80
			"all_sub             ", & ! 81
			"unknown             "  & ! inf (trailing comma hack)
		]
			! FIXME: update kind_tokens array too

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end function kind_name

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

integer function lookup_type(name) result(type)

	character(len = *), intent(in) :: name

	! Immo also has an "any" type.  Should I allow that?

	select case (name)
		case ("bool")
			type = bool_type

		case ("f32")
			type = f32_type

		case ("i32")
			type = i32_type
		case ("i64")
			type = i64_type

		case ("str")
			type = str_type

		case default
			type = unknown_type
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

subroutine syntax_nodes_copy(dst, src)

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

function new_literal_value(type, bool, i32, i64, f32, str) result(val)

	integer, intent(in) :: type

	integer(kind = 4), intent(in), optional :: i32
	integer(kind = 8), intent(in), optional :: i64
	real   (kind = 4), intent(in), optional :: f32
	logical          , intent(in), optional :: bool
	character(len=*) , intent(in), optional :: str

	type(value_t) :: val

	val%type = type
	if (present(bool)) val%sca%bool  = bool
	if (present(f32 )) val%sca%f32   = f32
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

		case ("include")
			kind = include_keyword

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
		sstar_equals_token, percent_equals_token])

end function is_assignment_op

!===============================================================================

recursive function ternary_search(node, key, id_index, iostat) result(val)

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
		val = ternary_search(node%left , key, id_index, iostat)
		return
	else if (k > node%split_char) then
		val = ternary_search(node%right, key, id_index, iostat)
		return
	else if (len(ey) > 0) then
		val = ternary_search(node%mid  , ey, id_index, iostat)
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

end function ternary_search

!===============================================================================

logical function is_binary_op_allowed(left, op, right)

	! Is an operation allowed with the types of operator op and left/right
	! operands?

	integer, intent(in) :: left, op, right

	!print *, 'left, right = ', left, right

	!! This dynamic variable typing can be useful for testing
	!is_binary_op_allowed = .true.
	!return

	is_binary_op_allowed = .false.

	if (left == unknown_type .or. right == unknown_type) then
		! Stop cascading errors
		is_binary_op_allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, plus_equals_token)

			is_binary_op_allowed = &
				(is_num_type(left) .and. is_num_type(right)) .or. &
				(left == str_type  .and. right == str_type)

		case (minus_token, sstar_token, star_token, slash_token, &
				less_token   , less_equals_token, &
				greater_token, greater_equals_token, &
				percent_token, minus_equals_token, star_equals_token, &
				slash_equals_token, sstar_equals_token, percent_equals_token)

			is_binary_op_allowed = is_num_type(left) .and. is_num_type(right)

		case (and_keyword, or_keyword)
			is_binary_op_allowed = left == bool_type .and. right == bool_type

		case (equals_token, eequals_token, bang_equals_token)

			! Fortran allows comparing ints and floats for strict equality, e.g.
			! 1 == 1.0 is indeed true.  I'm not sure if I like that
			is_binary_op_allowed = &
				(is_int_type(left) .and. is_int_type(right)) .or. &
				(left == right)

	end select

end function is_binary_op_allowed

!===============================================================================

logical function is_unary_op_allowed(op, operand)

	! Is a unary operation allowed with kinds operator op and operand?

	integer, intent(in) :: op, operand

	is_unary_op_allowed = .false.

	if (operand == unknown_type) then
		! Stop cascading errors
		is_unary_op_allowed = .true.
		return
	end if

	select case (op)

		case (plus_token, minus_token)
			is_unary_op_allowed = is_num_type(operand)

		case (not_keyword)
			is_unary_op_allowed = operand == bool_type

	end select

end function is_unary_op_allowed

!===============================================================================

integer function get_unary_op_prec(kind) result(prec)

	! Get unary operator precedence

	integer, intent(in) :: kind

	!********

	select case (kind)

		case (plus_token, minus_token, not_keyword)
			prec = 8

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

		! FIXME: increment the unary operator precedence above after increasing
		! the max binary precedence

		! Follow C operator precedence here, except possible for bitwise and/or

		case (sstar_token)
			prec = 7

		case (star_token, slash_token, percent_token)
			prec = 6

		case (plus_token, minus_token)
			prec = 5

		case (less_token, less_equals_token, &
				greater_token, greater_equals_token)
			prec = 4

		case (eequals_token, bang_equals_token)
			prec = 3

		case (and_keyword)
			prec = 2

		case (or_keyword)
			prec = 1

		case default
			prec = 0

	end select

end function get_binary_op_prec

!===============================================================================

logical function is_num_type(type)

	integer, intent(in) :: type

	! FIXME: other numeric types (f64, etc.)
	is_num_type = any(type == [i32_type, i64_type, f32_type])

end function is_num_type

!===============================================================================

logical function is_int_type(type)

	integer, intent(in) :: type

	! FIXME: other numeric types (i64, f64, etc.)
	is_int_type = any(type == [i32_type, i64_type])

end function is_int_type

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

	if (debug > 1) print *, 'new_binary_expr'
	if (debug > 1) print *, 'left  = ', left %str()
	if (debug > 1) print *, 'right = ', right%str()

	expr%kind = binary_expr

	allocate(expr%left)
	allocate(expr%right)

	expr%left  = left
	expr%op    = op
	expr%right = right

	! Pass the result value type up the tree for type checking in parent
	expr%val%type = get_binary_op_kind(left%val%type, op%kind, right%val%type)

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

integer function get_binary_op_kind(left, op, right)

	! Return the resulting type yielded by operator op on operands left and
	! right

	integer, intent(in) :: left, op, right

	select case (op)
	case ( &
			eequals_token, bang_equals_token, less_token, less_equals_token, &
			greater_token, greater_equals_token)
		!print *, 'bool_type'

		! Comparison operations can take 2 numbers, but always return a bool
		get_binary_op_kind = bool_type

	case default
		!print *, 'default'

		! Other operations return the same type as their operands if they match
		! or cast up
		!
		! FIXME: i64, f64, etc.

		if (left == right) then
			get_binary_op_kind = left
			return
		end if

		if (left == f32_type .or. right == f32_type) then
			! int + float casts to float
			get_binary_op_kind = f32_type
			return
		end if

		if ( &
			(left  == i64_type .and. is_int_type(right)) .or. &
			(right == i64_type .and. is_int_type(left ))) then

			! i32+i64 and i64+i32 cast to i64
			get_binary_op_kind = i64_type
			return

		end if

		get_binary_op_kind = unknown_type

	end select

end function get_binary_op_kind

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

	node%val      = val
	node%id_index = id_index

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

	node%val      = val
	node%id_index = id_index

	!print *, 'done inserting'
	!print *, ''

end subroutine ternary_insert

!===============================================================================

end module syntran__types_m

!===============================================================================

