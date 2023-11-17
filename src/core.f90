
!===============================================================================

module core_m

	! This module contains private members.  For the public API, see syntran.f90

	use iso_fortran_env

	use errors_m
	use utils

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	! Debug logging verbosity (0 == silent)
	integer, parameter :: debug = 0

	integer, parameter ::   &
		syntran_major =  0, &
		syntran_minor =  0, &
		syntran_patch =  22

	! TODO:
	!  - str comparison operations:
	!    * !=
	!    * >, <, etc. via lexicographical ordering? careful w/ strs that have
	!      matching leading chars but diff lens
	!    * ==:  done
	!  - fuzz testing
	!  - substring indexing and slicing:
	!    * str len intrinsic?  name it len() or size()?
	!    * first, single-character indexing
	!      > done
	!    * then, range-based slicing
	!      > done
	!    * string arrays get an optional extra rank.  omitting the extra rank
	!      refers to the whole string at that position in the array:
	!      > str_vec[0] == str_vec[:,0]
	!      > str_mat[0,0] == str_mat[:,0,0]
	!      > etc.
	!  - fix memory leaks.  noticeable in paraview wave
	!  - split doc into multiple README's, add TOC, cross-linking, etc.  Only
	!    include quick-start and links in top-level README
	!  - file reading
	!    * readln() to read 1 line?
	!    * how to handle eof? 404? open with r/w modes?
	!      > add an eof() intrinsic fn which takes a file type.  eof state needs
	!        to be stored inside file type.  initialize to eof false when
	!        opening, update after every readln
	!      > also add a file_stat() fn which checks IO of previous file
	!        operation. this way I don't need to add structs, multiple return
	!        vals, or out args yet
	!  - arrays
	!    * add slice subscripts:
	!      > a[:]     -> a[0], a[1], a[2], ...
	!      > a[1:4]   -> a[1], a[2], a[3]
	!      > a[1:2:6] -> a[1], a[3], a[5]
	!      > higher rank:  a[:,1], a[2,:], a[2:4, 1], ...
	!    * refactor the way implicit arrays are handled as for loop iterators
	!    * operations: vector addition, dot product, scalar-vector mult, ...
	!  - compound assignment: %=, logical &=, |=, etc.
	!    * +=, -=, *=, /= done
	!    * Does any language have "**="? This will
	!  - ++, --
	!  - tetration operator ***? ints only? just for fun
	!  - functions
	!    * check return value is correct type
	!    * intrinsic
	!      > read
	!      > len (of str)
	!      > abs, norm, dot
	!      > exp, log
	!      > trig: sin, cos, tan, asin, ...
	!      > norm, sum, product
	!      > reshape
	!      > system
	!    * recursive user-defined fns
	!    * done:
	!      > exp  (non-variadic, non-polymorphic)
	!      > min, max (variadic but non-polymorphic)
	!      > size (non-variadic but polymorphic)
	!      > writeln, println, open, close, str casting
	!      > non-recursive user-defined fns
	!  - structs
	!  - make syntax highlighting plugins for vim and TextMate (VSCode et al.)
	!  - enums
	!  - xor, xnor
	!  - bitwise operators
	!
	!****************************************

	! Must be larger than largest token enum below
	integer, parameter :: magic = 128

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
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

	! A note on naming: Immo calls '==' equals_equals_token, but I think that
	! invites tab-completion mistakes so I went with eequals_token.  Same for
	! sstar_token (and upcoming pplus_token, mminus_token, etc.)

	!********

	type file_t
		character(len = :), allocatable :: name_
		integer :: unit_
		logical :: eof = .false.
		! Do we need a separate iostat beyond eof?
	end type file_t

	type value_t
		integer :: type

		type(file_t)      :: file_
		type(string_t)    :: str

		logical           :: bool
		integer(kind = 4) :: i32
		real   (kind = 4) :: f32
		type(array_t), pointer :: array => null()

		contains
			procedure :: to_str => value_to_str

	end type value_t

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

	!********

	type array_t

		! The array type is i32_type, f32_type, etc. while the kind is
		! impl_array (bound-based) or expl_array (CSV list)
		integer :: type, kind
		type(value_t), allocatable :: lbound, step, ubound

		! Note that these are arrays of primitive Fortran types, instead of
		! arrays of generic value_t.  This performs better since we can put
		! a type select/case outside of loops for processing arrays, as opposed
		! to inside of a loop for type selection of every element
		logical(kind = 1), allocatable :: bool(:)
		integer(kind = 4), allocatable ::  i32(:)
		real   (kind = 4), allocatable ::  f32(:)
		type(string_t   ), allocatable ::  str(:)

		! TODO: file arrays

		integer :: len, cap, rank
		integer, allocatable :: size(:)

		contains
			procedure :: push => push_array

	end type array_t

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

		! Array expression syntax nodes
		type(syntax_node_t), allocatable :: lbound, step, ubound, len, &
			elems(:), rank

		type(syntax_node_t), allocatable :: subscripts(:), size(:), args(:), &
			usubscripts(:)!, ssubscripts(:) !TODO: subscript step

		! Either scalar_sub, range_sub (unit step), or step_sub
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

	type lexer_t

		! The lexer takes a string of characters and divides it into into tokens
		! or words

		integer :: pos

		type(string_vector_t) :: diagnostics

		type(text_context_t) :: context

		character(len = :), allocatable :: text

		! Both the lexer and the parser have current() and lex()/next() member
		! fns.  current_char() returns a char, while the others return syntax
		! tokens
		contains
			procedure :: lex, peek => peek_char, current => current_char, &
				lookahead => lookahead_char, read_single_line_comment

	end type lexer_t

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

	type parser_t

		! The parser takes a string of tokens (technically an array) and
		! constructs higher-level structures such as terms and expressions, like
		! constructing a phrase or sentence from words

		type(syntax_token_t), allocatable :: tokens(:)
		integer :: pos

		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		type(string_vector_t) :: diagnostics

		type(text_context_t) :: context

		type(vars_t) :: vars
		integer :: num_vars = 0

		type(fns_t) :: fns
		integer :: num_fns = 0

		contains
			procedure :: match, tokens_str, current_kind, &
				current => current_token, next => next_parser_token, &
				peek => parser_peek_token, peek_kind, &
				parse_expr, parse_primary_expr, parse_expr_statement, &
				parse_statement, parse_block_statement, parse_if_statement, &
				current_pos, peek_pos, parse_for_statement, &
				parse_while_statement, parse_array_expr, parse_unit, &
				parse_fn_declaration, parse_subscripts, parse_type, &
				parse_size

	end type parser_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

	!********

	type syntax_node_vector_t
		type(syntax_node_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push => push_node
	end type syntax_node_vector_t

!===============================================================================

contains

!===============================================================================

function declare_intrinsic_fns() result(fns)

	type(fns_t) :: fns

	!********

	integer :: id_index, num_fns

	type(fn_t) :: exp_fn, min_fn, max_fn, println_fn, size_fn, open_fn, &
		close_fn, readln_fn, writeln_fn, str_fn

	! Increment index for each fn and then set num_fns
	id_index = 0

	!********

	! TODO: polymorphic in f32, f64, etc.
	exp_fn%type = f32_type
	allocate(exp_fn%params(1))
	exp_fn%params(1)%type = f32_type
	exp_fn%params(1)%name = "x"

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

	! TODO: push_fn() fn, or just increment id_index inside insert()?
	id_index = id_index + 1
	call fns%insert("exp", exp_fn, id_index)

	!********

	! TODO: polymorphic in any numeric type i32, f32, i64, f64, etc.  Also an
	! array version min(array) as opposed to Fortran's minval()
	!
	! In Fortran, min() is polymorphic and variadic, but all args must be the
	! same type.  For example, min(1, 2) and min(1.1, 2.1) are allowed, but
	! min(1, 2.1) does not compile.  I think that's a reasonable restriction

	min_fn%type = i32_type
	allocate(min_fn%params(2))

	min_fn%params(1)%type = i32_type
	min_fn%params(1)%name = "a0"

	min_fn%params(2)%type = i32_type
	min_fn%params(2)%name = "a1"

	min_fn%variadic_min  = 0
	min_fn%variadic_type = i32_type

	id_index = id_index + 1
	call fns%insert("min", min_fn, id_index)

	!********

	max_fn%type = i32_type
	allocate(max_fn%params(2))

	max_fn%params(1)%type = i32_type
	max_fn%params(1)%name = "a0"

	! We could make max() and min() work with just 1 argument too.  I'm not sure
	! why you would want to be able to take the max of 1 number, but it seems
	! like an arbitrary limitation.  Anyway we follow the Fortran convention
	! here

	max_fn%params(2)%type = i32_type
	max_fn%params(2)%name = "a1"

	max_fn%variadic_min = 0
	max_fn%variadic_type = i32_type

	id_index = id_index + 1
	call fns%insert("max", max_fn, id_index)

	!********

	println_fn%type = void_type ! TODO?

	allocate(println_fn%params(0))

	println_fn%variadic_min  = 0
	println_fn%variadic_type = any_type

	id_index = id_index + 1
	call fns%insert("println", println_fn, id_index)

	!********

	str_fn%type = str_type

	allocate(str_fn%params(0))

	str_fn%variadic_min  = 0
	str_fn%variadic_type = any_type

	id_index = id_index + 1
	call fns%insert("str", str_fn, id_index)

	!********

	open_fn%type = file_type
	allocate(open_fn%params(1))
	open_fn%params(1)%type = str_type
	open_fn%params(1)%name = "filename"

	id_index = id_index + 1
	call fns%insert("open", open_fn, id_index)

	!********

	! TODO: document readln()

	readln_fn%type = str_type
	allocate(readln_fn%params(1))
	readln_fn%params(1)%type = file_type
	readln_fn%params(1)%name = "filename"

	id_index = id_index + 1
	call fns%insert("readln", readln_fn, id_index)

	!********

	writeln_fn%type = void_type
	allocate(writeln_fn%params(1))
	writeln_fn%params(1)%type = file_type
	writeln_fn%params(1)%name = "file_handle"

	writeln_fn%variadic_min  = 0
	!writeln_fn%variadic_min = 1
	writeln_fn%variadic_type = any_type

	id_index = id_index + 1
	call fns%insert("writeln", writeln_fn, id_index)

	!********

	close_fn%type = void_type
	allocate(close_fn%params(1))
	close_fn%params(1)%type = file_type
	close_fn%params(1)%name = "file_handle"

	id_index = id_index + 1
	call fns%insert("close", close_fn, id_index)

	!********

	size_fn%type = i32_type
	allocate(size_fn%params(2))

	size_fn%params(1)%type = array_type

	!size_fn%params(1)%array_type = i32_type
	size_fn%params(1)%array_type = any_type
	size_fn%params(1)%rank = -1  ! negative means any rank

	size_fn%params(1)%name = "array"

	size_fn%params(2)%type = i32_type
	size_fn%params(2)%name = "dim"

	id_index = id_index + 1
	call fns%insert("size", size_fn, id_index)

	! It might also be useful to make size() variadic and have size(array)
	! return the product of each dimension's size.  It should just have a single
	! optional param though, not unlimited arity like min/max.

	!********

	! FIXME: when adding new functions, remember to copy them into the
	! fns%fns(:) array below

	num_fns = id_index
	allocate(fns%fns(num_fns))

	fns%fns = &
		[ &
			min_fn    , &
			max_fn    , &
			println_fn, &
			str_fn    , &
			open_fn   , &
			readln_fn , &
			writeln_fn, &
			close_fn  , &
			size_fn     &
		]

end function declare_intrinsic_fns

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

recursive function ternary_search(node, key, id_index, iostat) result(val)

	type(ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(value_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key "', key, '"'

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

recursive function fn_ternary_search(node, key, id_index, iostat) result(val)

	type(fn_ternary_tree_node_t), intent(in), allocatable :: node
	character(len = *), intent(in) :: key

	integer, intent(out) :: id_index
	integer, intent(out) :: iostat
	type(fn_t) :: val

	!********

	character :: k
	character(len = :), allocatable :: ey

	!print *, 'searching key "', key, '"'

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

	!print *, 'inserting "', key, '"'

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call fn_ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine fn_insert

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

	!print *, 'inserting "', key, '"'

	overwritel = .true.
	if (present(overwrite)) overwritel = overwrite

	i = dict%scope
	call ternary_insert(dict%dicts(i)%root, key, val, id_index, io, overwritel)

	if (present(iostat)) iostat = io

end subroutine var_insert

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

	!print *, 'inserting key "', key, '"'

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

	!print *, 'inserting key "', key, '"'

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

function new_syntax_token_vector() result(vector)

	type(syntax_token_vector_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_token_vector

!===============================================================================

subroutine push_token(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(syntax_token_vector_t) :: vector
	type(syntax_token_t) :: val

	!********

	type(syntax_token_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len ) = val

end subroutine push_token

!===============================================================================

function new_array(type, cap) result(vector)

	integer, intent(in) :: type
	integer, intent(in), optional :: cap
	type(array_t) :: vector

	vector%len = 0

	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2  ! I think a small default makes sense here
	end if

	if      (type == i32_type) then
		allocate(vector%i32 ( vector%cap ))
	else if (type == f32_type) then
		allocate(vector%f32 ( vector%cap ))
	else if (type == bool_type) then
		allocate(vector%bool( vector%cap ))
	else if (type == str_type) then
		allocate(vector%str ( vector%cap ))
	else
		write(*,*) 'Error: array type not implemented'
		call internal_error()
	end if

	vector%type = type

end function new_array

!===============================================================================

subroutine push_array(vector, val)

	! Is there a way to have a generic unlimited polymorphic vector?  I couldn't
	! figure it out

	class(array_t) :: vector
	type(value_t)  :: val

	!********

	integer(kind = 4), allocatable :: tmp_i32 (:)
	real   (kind = 4), allocatable :: tmp_f32 (:)
	logical(kind = 1), allocatable :: tmp_bool(:)
	type(string_t   ), allocatable :: tmp_str (:)

	integer :: tmp_cap

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len

		if (vector%type == i32_type) then

			allocate(tmp_i32 ( tmp_cap ))
			tmp_i32(1: vector%cap) = vector%i32
			call move_alloc(tmp_i32, vector%i32)

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
		vector%i32 ( vector%len ) = val%i32
	else if (vector%type == f32_type) then
		vector%f32 ( vector%len ) = val%f32
	else if (vector%type == bool_type) then
		vector%bool( vector%len ) = val%bool
	else if (vector%type == str_type) then
		vector%str ( vector%len ) = val%str
	else
		write(*,*) 'Error: push_array type not implemented'
		call internal_error()
	end if

end subroutine push_array

!===============================================================================

function new_syntax_node_vector() result(vector)

	type(syntax_node_vector_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_syntax_node_vector

!===============================================================================

subroutine push_node(vector, val)

	class(syntax_node_vector_t) :: vector
	type(syntax_node_t) :: val

	!********

	type(syntax_node_t), allocatable :: tmp(:)

	integer :: tmp_cap, i

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector ====================================='

		tmp_cap = 2 * vector%len
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
	vector%v( vector%len ) = val
	!print *, 'done push_node'

end subroutine push_node

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
			"file_type           "  & ! 74
		]
			! FIXME: update kind_tokens array too

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end function kind_name

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
			"file type            "  & ! 74
		]

	if (.not. (1 <= kind .and. kind <= size(tokens))) then
		kind_token = "unknown"
		return
	end if

	kind_token = trim(tokens(kind))

end function kind_token

!===============================================================================

recursive function syntax_node_str(node, indent) result(str)

	! Convert tree to string in JSON-ish format

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
	end if

	if (allocated(src%right)) then
		!if (debug > 1) print *, 'copy() right'
		if (.not. allocated(dst%right)) allocate(dst%right)
		dst%right = src%right
	end if

	if (allocated(src%params)) then
		! Primitive int array.  No need to explicitly allocate
		dst%params = src%params
	end if

	if (allocated(src%condition)) then
		if (.not. allocated(dst%condition)) allocate(dst%condition)
		dst%condition = src%condition
	end if

	if (allocated(src%body)) then
		if (.not. allocated(dst%body)) allocate(dst%body)
		dst%body = src%body
	end if

	if (allocated(src%array)) then
		if (.not. allocated(dst%array)) allocate(dst%array)
		dst%array = src%array
	end if

	if (allocated(src%lbound)) then
		if (.not. allocated(dst%lbound)) allocate(dst%lbound)
		dst%lbound = src%lbound
	end if

	if (allocated(src%ubound)) then
		if (.not. allocated(dst%ubound)) allocate(dst%ubound)
		dst%ubound = src%ubound
	end if

	if (allocated(src%step)) then
		if (.not. allocated(dst%step)) allocate(dst%step)
		dst%step = src%step
	end if

	if (allocated(src%len)) then
		if (.not. allocated(dst%len)) allocate(dst%len)
		dst%len = src%len
	end if

	if (allocated(src%rank)) then
		if (.not. allocated(dst%rank)) allocate(dst%rank)
		dst%rank = src%rank
	end if

	if (allocated(src%elems)) then
		call syntax_nodes_copy(dst%elems, src%elems)
	end if

	if (allocated(src%subscripts)) then
		call syntax_nodes_copy(dst%subscripts, src%subscripts)
	end if

	if (allocated(src%usubscripts)) then
		call syntax_nodes_copy(dst%usubscripts, src%usubscripts)
	end if

	if (allocated(src%args)) then
		call syntax_nodes_copy(dst%args, src%args)
	end if

	if (allocated(src%size)) then
		call syntax_nodes_copy(dst%size, src%size)
	end if

	if (allocated(src%if_clause)) then
		if (.not. allocated(dst%if_clause)) allocate(dst%if_clause)
		dst%if_clause = src%if_clause
	end if

	if (allocated(src%else_clause)) then
		if (.not. allocated(dst%else_clause)) allocate(dst%else_clause)
		dst%else_clause = src%else_clause
	end if

	if (allocated(src%members)) then
		!print *, 'copying members'
		call syntax_nodes_copy(dst%members, src%members)
	end if

	if (debug > 3) print *, 'done syntax_node_copy()'

end subroutine syntax_node_copy

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

character function peek_char(lexer, offset)

	class(lexer_t) :: lexer

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = lexer%pos + offset

	if (pos < 1 .or. pos > len(lexer%text)) then
		peek_char = null_char
		return
	end if

	peek_char = lexer%text(pos: pos)

end function peek_char

!===============================================================================

character function current_char(lexer)
	class(lexer_t) :: lexer
	current_char = lexer%peek(0)
end function current_char

character function lookahead_char(lexer)
	class(lexer_t) :: lexer
	lookahead_char = lexer%peek(1)
end function lookahead_char

!===============================================================================

function lex(lexer) result(token)

	class(lexer_t) :: lexer

	type(syntax_token_t) :: token

	!********

	character(len = :), allocatable :: text

	integer :: kind
	integer :: start, io, i32

	logical :: float

	real(kind = 4) :: f32

	type(char_vector_t) :: char_vec
	type(text_span_t) :: span
	type(value_t) :: val

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char)
		return
	end if

	start = lexer%pos

	if (is_digit(lexer%current())) then

		float = .false.

		do while (is_float(lexer%current()))

			if (is_sign(lexer%current()) .and. .not. &
				is_expo(lexer%peek(-1))) exit

			float = float .or. .not. is_digit(lexer%current())

			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		if (float) then

			! TODO: after f64 is supported, consider parsing that as the default
			! float type and requiring something like 1.0f for f32

			! This io check can catch problems like `1.234e+1e+2` which look
			! like a float but aren't correctly formatted
			read(text, *, iostat = io) f32
			if (io /= exit_success) then
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_float( &
					lexer%context, span, text))
			end if

			val   = new_literal_value(f32_type, f32 = f32)
			token = new_token(f32_token, start, text, val)

		else

			read(text, *, iostat = io) i32
			if (io /= exit_success) then
				span = new_span(start, len(text))
				call lexer%diagnostics%push(err_bad_int( &
					lexer%context, span, text))
			end if

			val   = new_literal_value(i32_type, i32 = i32)
			token = new_token(i32_token, start, text, val)

		end if

		return

	end if

	if (lexer%current() == '"') then

		! Skip the current quote
		lexer%pos = lexer%pos + 1

		char_vec = new_char_vector()
		do

			! Make a quote literal by doubling it
			if (lexer%current() == '"') then
				lexer%pos = lexer%pos + 1
				if (lexer%current() /= '"') then
					exit
				end if
			end if

			call char_vec%push(lexer%current())
			lexer%pos = lexer%pos + 1

			if (lexer%pos > len(lexer%text)) exit

		end do

		text  = lexer%text(start: lexer%pos-1)

		if (lexer%pos > len(lexer%text)) then
			token = new_token(bad_token, lexer%pos, text)
			span = new_span(start, len(text))
			call lexer%diagnostics%push( &
				err_unterminated_str(lexer%context, &
				span, text))
			return
		end if

		val   = new_literal_value(str_type, str = char_vec%v( 1: char_vec%len ))
		token = new_token(str_token, start, text, val)

		return

	end if

	if (is_whitespace(lexer%current())) then

		do while (is_whitespace(lexer%current()))
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		token = new_token(whitespace_token, start, text)
		return

	end if

	if (is_letter(lexer%current()) .or. lexer%current() == '_') then

		do while (is_alphanum(lexer%current()) .or. lexer%current() == '_')
			lexer%pos = lexer%pos + 1
		end do
		text = lexer%text(start: lexer%pos-1)

		! This block handles booleans as well as identifiers, but note that it
		! does not set the value here like the is_digit() case for numbers
		! above.  The boolean value is not set until parse_primary_expr().

		kind = get_keyword_kind(text)
		token = new_token(kind, start, text)
		return

	end if

	select case (lexer%current())

		case ("+")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(plus_equals_token, lexer%pos, "+=")
			else
				token = new_token(plus_token, lexer%pos, lexer%current())
			end if

			! FIXME: prefix/postfix inc/dec operators (++, --)

		case ("-")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(minus_equals_token, lexer%pos, "-=");
			else
				token = new_token(minus_token, lexer%pos, lexer%current())
			end if

		case ("*")
			if (lexer%lookahead() == "*") then
				lexer%pos = lexer%pos + 1
				token = new_token(sstar_token, lexer%pos, "**")
			else if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(star_equals_token, lexer%pos, "*=")
			else
				token = new_token(star_token, lexer%pos, lexer%current())
			end if

		case ("/")
			if (lexer%lookahead() == "/") then

				call lexer%read_single_line_comment()

				! FIXME: make "trivia" token types instead of overloading
				! whitespace_token for comments.  This is what Immo did
				text = lexer%text(start: lexer%pos-1)
				token = new_token(whitespace_token, start, text)

			else if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(slash_equals_token, lexer%pos, "/=")

			else
				token = new_token(slash_token, lexer%pos, lexer%current())
			end if

		case ("%")
			token = new_token(percent_token, lexer%pos, lexer%current())

		case ("(")
			token = new_token(lparen_token, lexer%pos, lexer%current())

		case (")")
			token = new_token(rparen_token, lexer%pos, lexer%current())

		case ("{")
			token = new_token(lbrace_token, lexer%pos, lexer%current())

		case ("}")
			token = new_token(rbrace_token, lexer%pos, lexer%current())

		case ("[")
			token = new_token(lbracket_token, lexer%pos, lexer%current())

		case ("]")
			token = new_token(rbracket_token, lexer%pos, lexer%current())

		case (":")
			token = new_token(colon_token, lexer%pos, lexer%current())

		case (";")
			token = new_token(semicolon_token, lexer%pos, lexer%current())

		case (",")
			token = new_token(comma_token, lexer%pos, lexer%current())

		case ("=")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(eequals_token, lexer%pos, "==")
			else
				token = new_token(equals_token, lexer%pos, lexer%current())
			end if

		case ("!")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(bang_equals_token, lexer%pos, "!=")
			else

				! FIXME: refactor w/ default case below since Fortran is weird
				! about breaking in select case
				token = new_token(bad_token, lexer%pos, lexer%current())
				span = new_span(lexer%pos, len(lexer%current()))
				call lexer%diagnostics%push( &
					err_unexpected_char(lexer%context, &
					span, lexer%current()))

			end if

		case ("<")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(less_equals_token, lexer%pos, "<=")
			else
				token = new_token(less_token, lexer%pos, lexer%current())
			end if

		case (">")
			if (lexer%lookahead() == "=") then
				lexer%pos = lexer%pos + 1
				token = new_token(greater_equals_token, lexer%pos, ">=")
			else
				token = new_token(greater_token, lexer%pos, lexer%current())
			end if

		case default

			token = new_token(bad_token, lexer%pos, lexer%current())
			span = new_span(lexer%pos, len(lexer%current()))
			call lexer%diagnostics%push( &
				err_unexpected_char(lexer%context, &
				span, lexer%current()))

	end select
	lexer%pos = lexer%pos + 1

	! FIXME: arrow keys create bad tokens in bash on Windows.  Fix that (better
	! yet, override up arrow to do what it does in bash.  c.f. rubik-js)

end function lex

!===============================================================================

! I am NOT planning on implementing multi-line comments.  Use block-insertion in
! your editor to comment-out multiple lines with "//"

subroutine read_single_line_comment(lexer)

	class(lexer_t) :: lexer

	lexer%pos = lexer%pos + 2

	loop: do

		!print *, 'char = ', lexer%current()
		select case (lexer%current())
			case (null_char, carriage_return, line_feed)
				exit loop
		end select

		lexer%pos = lexer%pos + 1
	end do loop
	!print *, 'done'

end subroutine read_single_line_comment

!===============================================================================

function new_literal_value(type, bool, i32, f32, str) result(val)

	integer, intent(in) :: type

	integer(kind = 4), intent(in), optional :: i32
	real   (kind = 4), intent(in), optional :: f32
	logical          , intent(in), optional :: bool
	character(len=*) , intent(in), optional :: str

	type(value_t) :: val

	val%type = type
	if (present(bool)) val%bool  = bool
	if (present(f32 )) val%f32   = f32
	if (present(i32 )) val%i32   = i32
	if (present(str )) val%str%s = str

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

		case default
			kind = identifier_token

	end select

	!print *, 'get_keyword_kind = ', kind

end function get_keyword_kind

!===============================================================================

function new_lexer(text, src_file) result(lexer)

	character(len = *) :: text, src_file

	type(lexer_t) :: lexer

	!********

	integer :: i, i0, nlines

	integer, allocatable :: lines(:)

	lexer%text     = text
	lexer%pos      = 1

	lexer%diagnostics = new_string_vector()

	! Count lines
	nlines = 0
	i = 0
	!outer: do
	do
		i = i + 1
		if (i > len(text)) exit !outer

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

			nlines = nlines + 1

			!do
			!	i = i + 1
			!	if (i > len(text)) exit outer
			!	if (text(i:i) /= line_feed .and. &
			!	    text(i:i) /= carriage_return) exit
			!end do

		end if

	end do !outer

	!print *, 'nlines = ', nlines

	allocate(lines(nlines + 1))

	! Get character indices for the start of each line and save them in lines(:)
	nlines = 0
	i = 0
	i0 = 0
	do
		i = i + 1
		if (i > len(text)) exit

		if (i == len(text) .or. &
			text(i:i) == line_feed .or. &
			text(i:i) == carriage_return) then

			nlines = nlines + 1

			lines(nlines) = i0 + 1
			i0 = i

		end if

	end do
	lines(nlines + 1) = len(text) + 1

	!print *, 'lines = ', lines

	if (debug > 1) then
		write(*,*) 'lines = '
		do i = 1, nlines
			write(*, '(i5,a)') i, ' | '//text(lines(i): lines(i+1) - 2)
		end do
	end if

	! TODO: delete lexer%text in favor of lexer%context%text.  It appears in
	! a lot of places
	lexer%context = new_context(text, src_file, lines)

end function new_lexer

!===============================================================================

function new_parser(str, src_file) result(parser)

	character(len = *) :: str, src_file

	type(parser_t) :: parser

	!********

	type(syntax_token_t)        :: token
	type(syntax_token_vector_t) :: tokens

	type(lexer_t) :: lexer

	! Get an array of tokens
	tokens = new_syntax_token_vector()
	lexer = new_lexer(str, src_file)
	do
		token = lexer%lex()

		if (token%kind /= whitespace_token .and. &
		    token%kind /= bad_token) then
			call tokens%push(token)
		end if

		if (token%kind == eof_token) exit
	end do

	! Convert to standard array (and class member)
	parser%tokens = tokens%v( 1: tokens%len )

	parser%pos = 1

	parser%diagnostics = lexer%diagnostics

	parser%context = lexer%context

	!print *, 'tokens%len = ', tokens%len
	if (debug > 1) print *, parser%tokens_str()

end function new_parser

!===============================================================================

function tokens_str(parser) result(str)

	class(parser_t) :: parser

	character(len = :), allocatable :: str

	!********

	integer :: i

	! This will crash for very long token lists, but it should suffice for basic
	! debugging

	str = 'tokens = '//line_feed//'['//line_feed
	do i = 1, size(parser%tokens)
		str = str//tab &
				//'<'//           parser%tokens(i)%text  //'> ' &
				//'<'//kind_name( parser%tokens(i)%kind )//'>'  &
				//line_feed
	end do
	str = str//']'//line_feed

end function tokens_str

!===============================================================================

function syntax_parse(str, vars, fns, src_file, allow_continue) result(tree)

	character(len = *) :: str

	type(vars_t), intent(inout) :: vars

	type(fns_t), intent(inout) :: fns

	type(syntax_node_t) :: tree

	character(len = *), optional, intent(in)  :: src_file

	logical, intent(in), optional :: allow_continue

	!********

	character(len = :), allocatable :: src_filel

	logical :: allow_continuel

	type(fns_t) :: fns0

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	type(vars_t) :: vars0

	if (debug > 0) print *, 'syntax_parse'
	if (debug > 1) print *, 'str = ', str

	!! "exp"
	!print *, 'key = ', &
	!	fns%dicts(1)%root%split_char, &
	!	fns%dicts(1)%root%mid%split_char, &
	!	fns%dicts(1)%root%mid%mid%split_char

	src_filel = '<stdin>'
	if (present(src_file)) src_filel = src_file

	allow_continuel = .false.
	if (present(allow_continue)) allow_continuel = allow_continue

	parser = new_parser(str, src_filel)

	! Do nothing for blank lines (or comments)
	if (parser%current_kind() == eof_token) then
		tree%is_empty = .true.
		tree%diagnostics = parser%diagnostics
		return
	end if

	! Point parser member to vars dict.  This could be done in the
	! constructor new_parser(), but it seems reasonable to do it here since it
	! has to be moved back later.  The dict vars0 comes from the
	! interactive interpreter's history, it has nothing to do with scoping
	!print *, 'moving vars'
	if (allocated(vars%dicts(1)%root)) then

		if (allow_continuel) then
			! Backup existing vars.  Only copy for interactive interpreter.
			! This logic is slightly redundant as allow_continuel should _only_
			! be set true for the interactive interpreter with stdin, which is
			! also the only case where vars%root will be allocated.
			! Calling syntran_interpret() on a multi-line string is deprecated,
			! since syntran_eval() can parse it all in one syntax_parse() call.

			! The root type has an overloaded assignment op, but the vars
			! type itself does not (and I don't want to expose or encourage
			! copying)
			allocate(vars0%dicts(1)%root)
			vars0%dicts(1)%root = vars%dicts(1)%root

			!print *, 'vars%vals = '
			!do i = 1, size(vars%vals)
			!	print *, vars%vals(i)%to_str()
			!end do

			! Backup vals array and set num_vars in parser object
			vars0%vals = vars%vals
			parser%num_vars = size(vars%vals)

		end if

		! Only the 1st scope level matters from interpreter.  It doesn't
		! evaluate until the block is finished
		call move_alloc(vars%dicts(1)%root, parser%vars%dicts(1)%root)
		call move_alloc(vars%vals         , parser%vars%vals)

	end if

	!print *, 'moving fns'
	if (allocated(fns%dicts(1)%root)) then

		allocate(fns0%dicts(1)%root)
		fns0%dicts(1)%root = fns%dicts(1)%root

		!print *, 'fns%fns = '
		!do i = 1, size(fns%fns)
		!	print *, fns%fns(i)%to_str()
		!end do

		!! With intrinsic fns, this is always allocated
		!if (allocated(fns%fns)) then
			!print *, 'copy fns'
			fns0%fns = fns%fns
			parser%num_fns = size(fns%fns)
		!else
		!	parser%num_fns = 0
		!end if

		!print *, 'parser%num_fns = ', parser%num_fns

		! Only the 1st scope level matters from interpreter.  It doesn't
		! evaluate until the block is finished
		call move_alloc(fns%dicts(1)%root, parser%fns%dicts(1)%root)
		if (allocated(fns%fns)) call move_alloc(fns%fns          , parser%fns%fns)

	end if

	!*******************************
	! Parse the tokens
	tree = parser%parse_unit()
	!*******************************

	tree%expecting       = parser%expecting
	tree%first_expecting = parser%first_expecting

	tree%first_expected = parser%first_expected

	if (debug > 1) print *, 'tree = ', tree%str()

	if (tree%expecting .and. allow_continuel) then

		! If expecting more input, don't push diagnostics yet.  Also undo any
		! variable declarations, since they will be re-declared when we continue
		! parsing the current stdin line from its start again.

		if (allocated(vars0%dicts(1)%root)) then
			call move_alloc(vars0%dicts(1)%root, vars%dicts(1)%root)
			call move_alloc(vars0%vals         , vars%vals)
		end if

		if (allocated(fns0%dicts(1)%root)) then
			call move_alloc(fns0%dicts(1)%root, fns%dicts(1)%root)
			call move_alloc(fns0%fns          , fns%fns)
		end if

		return

	end if

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

	tree%diagnostics = parser%diagnostics

	! Move back.  It's possible that vars were empty before this call but not
	! anymore
	if (allocated(parser%vars%dicts(1)%root)) then
		call move_alloc(parser%vars%dicts(1)%root, vars%dicts(1)%root)
	end if

	if (allocated(parser%fns%dicts(1)%root)) then
		call move_alloc(parser%fns%dicts(1)%root, fns%dicts(1)%root)
	end if

	! When parsing is finished, we are done with the variable dictionary
	! parser%vars%dicts.  Allocate an array for efficient evaluation without
	! dictionary lookups.  Indices in the array are already saved in each node's
	! id_index member

	!print *, 'parser%num_vars = ', parser%num_vars
	if (allocated(vars%vals)) deallocate(vars%vals)
	allocate(vars%vals( parser%num_vars ))

	if (allocated(vars0%vals)) then
		vars%vals( 1: size(vars0%vals) ) = vars0%vals
	end if

	!print *, 'parser%num_fns = ', parser%num_fns
	if (allocated(fns%fns)) deallocate(fns%fns)
	allocate(fns%fns( parser%num_fns ))

	if (allocated(fns0%fns)) then
		fns%fns( 1: size(fns0%fns) ) = fns0%fns
	end if

	if (debug > 0) print *, 'done syntax_parse'

end function syntax_parse

!===============================================================================

function parse_unit(parser) result(unit)

	class(parser_t) :: parser

	type(syntax_node_t) :: unit

	!********

	type(syntax_node_vector_t) :: members
	type(syntax_token_t) :: dummy

	integer :: i, pos0

	!print *, 'starting parse_unit()'

	members = new_syntax_node_vector()
	i = 0

	!left  = parser%match(lbrace_token)

	!! Pushing scope breaks interactive interpretation, but we may want it later
	!! for interpetting multiple files.  Another alternative would be chaining
	!! interpreted statements like Immo does

	!call parser%vars%push_scope()

	do while (parser%current_kind() /= eof_token)

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		if (parser%current_kind() == fn_keyword) then
			call members%push(parser%parse_fn_declaration())
		else
			call members%push(parser%parse_statement())
		end if

		! Break infinite loops
		if (parser%pos == pos0) dummy = parser%next()

	end do

	!call parser%vars%pop_scope()

	!right = parser%match(rbrace_token)

	unit%kind = translation_unit

	! Convert to standard array
	call syntax_nodes_copy(unit%members, members%v( 1: members%len ))

	! Eof is matched in the caller syntax_parse() to deal with broken stdin
	! lines with interactive interpretation

end function parse_unit

!===============================================================================

subroutine parse_type(parser, type_text, rank)

	! TODO: encapsulate out-args in struct if adding any more

	class(parser_t) :: parser

	character(len = :), intent(out), allocatable :: type_text

	integer, intent(out) :: rank

	!********

	type(syntax_token_t) :: colon, type, comma, lbracket, rbracket, semi

	if (parser%current_kind() == lbracket_token) then

		! Array param
		lbracket = parser%match(lbracket_token)
		type     = parser%match(identifier_token)
		semi     = parser%match(semicolon_token)

		rank  = 0
		do while ( &
			parser%current_kind() /= rbracket_token .and. &
			parser%current_kind() /= eof_token)

			rank = rank + 1
			colon = parser%match(colon_token)
			if (parser%current_kind() /= rbracket_token) then
				comma = parser%match(comma_token)
			end if

		end do
		!print *, 'rank = ', rank

		rbracket = parser%match(rbracket_token)

	else
		! Scalar param
		type = parser%match(identifier_token)
		rank = -1
	end if

	type_text = type%text

end subroutine parse_type

!===============================================================================

function parse_fn_declaration(parser) result(decl)

	class(parser_t) :: parser

	type(syntax_node_t) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: i, pos0, pos1, pos2, rank, itype

	type(fn_t) :: fn

	type( string_vector_t) :: names, types
	type(logical_vector_t) :: is_array
	type(integer_vector_t) :: ranks

	type(syntax_node_t) :: body
	type(syntax_token_t) :: fn_kw, identifier, lparen, rparen, colon, &
		name, comma, dummy

	type(text_span_t) :: span

	type(value_t) :: val

	! Like a for statement, a fn declaration has its own scope (for its
	! parameters).  Its block body will have yet another scope
	call parser%vars%push_scope()

	fn_kw = parser%match(fn_keyword)

	identifier = parser%match(identifier_token)

	!print *, 'parsing fn ', identifier%text

	pos1 = parser%pos

	lparen = parser%match(lparen_token)

	! Parse parameter names and types.  Save in temp string vectors initially
	names    = new_string_vector()
	types    = new_string_vector()
	is_array = new_logical_vector()
	ranks    = new_integer_vector()

	! Array params use this syntax:
	!
	!     fn sum_fn(v: [i32; :]): i32
	!     {
	!         let s = 0;
	!         for i in [0: size(v, 0)]
	!             s = s + v[i];
	!         s;
	!     }
	!
	!     fn mat_fn(a: [i32; :,:]): i32
	!     {
	!         // do something with a[i,j]
	!     }

	do while ( &
		parser%current_kind() /= rparen_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		name  = parser%match(identifier_token)
		colon = parser%match(colon_token)

		call parser%parse_type(type_text, rank)

		call names%push( name%text )
		call types%push( type_text )
		call ranks%push( rank      )

		! This array is technically redundant but helps readability?
		call is_array%push( rank >= 0 )

		if (parser%current_kind() /= rparen_token) then
			comma = parser%match(comma_token)
		end if

		! Break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	rparen = parser%match(rparen_token)
	pos2 = parser%pos

	! Now that we have the number of params, save them

	allocate(fn  %params( names%len ))
	allocate(decl%params( names%len ))

	do i = 1, names%len
		!print *, 'name, type = ', names%v(i)%s, ', ', types%v(i)%s

		fn%params(i)%name = names%v(i)%s

		itype = lookup_type( types%v(i)%s )
		if (itype == unknown_type) then

			! TODO: make an array of pos's for each param to underline
			! individual param, not whole param list

			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context, span, types%v(i)%s))

		end if

		if (is_array%v(i)) then
			fn%params(i)%type = array_type
			fn%params(i)%array_type = itype
			fn%params(i)%rank = ranks%v(i)
		else
			fn%params(i)%type = itype
		end if

		! Declare the parameter variable
		parser%num_vars = parser%num_vars + 1

		! Save parameters by id_index.  TODO: stack frames
		decl%params(i) = parser%num_vars

		! Create a value_t object to store the type
		val%type = fn%params(i)%type
		if (is_array%v(i)) then
			allocate(val%array)
			val%array%type = fn%params(i)%array_type
			val%array%rank = fn%params(i)%rank
		end if

		call parser%vars%insert(fn%params(i)%name, val, parser%num_vars)

	end do

	! Rust uses "->" as a delimiter between the fn and its return type.  Here
	! I choose ":" instead as it seems more consistent, at least for normal
	! non-assignable fns.  There is some discussion on the Rust reasoning here:
	!
	!     https://stackoverflow.com/questions/35018919/whats-the-origin-of-in-rust-function-definition-return-types
	!

	fn%type = void_type
	if (parser%current_kind() == colon_token) then

		colon = parser%match(colon_token)

		pos1 = parser%pos
		call parser%parse_type(type_text, rank)
		pos2 = parser%pos

		itype = lookup_type(type_text)

		if (itype == unknown_type) then
			span = new_span(pos1, pos2 - pos1 + 1)
			call parser%diagnostics%push(err_bad_type( &
				parser%context, span, type_text))
		end if

		if (rank >= 0) then
			fn%type = array_type
			fn%rank = rank
			fn%array_type = itype
		else
			fn%type = itype
		end if

	end if
	!print *, 'fn%type = ', fn%type

	body = parser%parse_statement()

	! Insert fn into parser%fns

	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	allocate(decl%body)

	decl%kind = fn_declaration

	decl%identifier = identifier
	decl%body       = body

	call parser%vars%pop_scope()

	allocate(fn%node)
	fn%node = decl

	call parser%fns%insert(identifier%text, fn, decl%id_index)

	!print *, 'size(decl%params) = ', size(decl%params)
	!print *, 'decl%params = ', decl%params

end function parse_fn_declaration

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
		case ("str")
			type = str_type
		case default
			type = unknown_type
	end select
	!print *, 'lookup_type = ', type

end function lookup_type

!===============================================================================

function parse_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	type(syntax_token_t) :: semi

	select case (parser%current_kind())

		case (lbrace_token)
			statement = parser%parse_block_statement()

		case (if_keyword)
			statement = parser%parse_if_statement()

		case (for_keyword)
			statement = parser%parse_for_statement()

		case (while_keyword)
			statement = parser%parse_while_statement()

		case default
			statement = parser%parse_expr_statement()
			semi      = parser%match(semicolon_token)

	end select

end function parse_statement

!===============================================================================

function parse_for_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	!integer :: bound_beg, bound_end

	type(syntax_node_t)  :: array, body
	type(syntax_token_t) :: for_token, in_token, identifier

	!  For loop syntax:
	!
	!    for i in [1: 5]
	!       { i; }
	!    // 1, 2, 3, 4 // ubound not inclusive
	!
	!  steps:
	!
	!    for i in [1: 2: 7]
	!    // 1,    3,    5
	!
	!   // And finally, after doing some array handling work, something like:
	!    for i in [1, 2, 4, 5]
	!    // 1, 2,   4, 5  // last elem *is* included
	!
	!  * For steps, rust has `for x in (1..10).step_by(2) {}`, which I hate

	for_token  = parser%match(for_keyword)

	call parser%vars%push_scope()

	identifier = parser%match(identifier_token)

	in_token   = parser%match(in_keyword)

	array      = parser%parse_array_expr()

	parser%num_vars = parser%num_vars + 1
	statement%id_index = parser%num_vars

	! Auto declare loop iterator in for statement (HolyC doesn't let you do
	! that!).  The 'let' keyword is not used:
	!
	!     for i in [lower, upper]
	!        {}

	! Insert the identifier's type into the dict. This is a local scope, so
	! there's no need to check io

	!print *, 'identifier%text = ', identifier%text
	call parser%vars%insert(identifier%text, array%lbound%val, &
		statement%id_index)

	body = parser%parse_statement()

	allocate(statement%array)
	allocate(statement%body)

	statement%kind = for_statement

	statement%identifier = identifier
	statement%array      = array
	statement%body       = body

	call parser%vars%pop_scope()

end function parse_for_statement

!===============================================================================

function parse_while_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end

	type(syntax_node_t)  :: body, condition
	type(syntax_token_t) :: while_token
	type(text_span_t) :: span

	while_token  = parser%match(while_keyword)

	cond_beg  = parser%peek_pos(0)
	condition = parser%parse_expr()
	cond_end  = parser%peek_pos(0) - 1

	! Check that condition type is bool
	if (condition%val%type /= bool_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context, span, parser%context%text(cond_beg: cond_end), &
			"while-loop"))
	end if

	body = parser%parse_statement()

	allocate(statement%condition, statement%body)

	statement%kind = while_statement

	statement%condition = condition
	statement%body      = body

end function parse_while_statement

!===============================================================================

function parse_if_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end

	type(syntax_node_t)  :: condition, if_clause, else_clause
	type(syntax_token_t) :: if_token, else_token
	type(text_span_t) :: span

	!print *, 'parse_if_statement'

	if_token  = parser%match(if_keyword)

	cond_beg  = parser%peek_pos(0)
	condition = parser%parse_expr()

	!cond_end  = parser%peek_pos(-1)
	cond_end  = parser%peek_pos(0) - 1

	!print *, 'cond_beg, cond_end = ', cond_beg, cond_end

	! Check that condition type is bool
	if (condition%val%type /= bool_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context, span, parser%context%text(cond_beg: cond_end), &
			"if-statement"))
	end if

	if_clause = parser%parse_statement()  ! Immo calls this "then statement"

	allocate(statement%condition, statement%if_clause)

	statement%kind = if_statement
	statement%condition = condition
	statement%if_clause = if_clause

	if (parser%current_kind() == else_keyword) then
		!print *, 'parsing else clause'

		else_token = parser%match(else_keyword)
		else_clause = parser%parse_statement()

		allocate(statement%else_clause)
		statement%else_clause = else_clause

	!else
	!	print *, 'no else clause'
	end if

	! No additional parsing work is required to handle "else if".  That's just
	! an else clause which contains another if statement

	!print *, 'done parse_if_statement'

end function parse_if_statement

!===============================================================================

function parse_block_statement(parser) result(block)

	class(parser_t) :: parser

	type(syntax_node_t) :: block

	!********

	type(syntax_node_vector_t) :: members
	type(syntax_token_t) :: left, right, dummy

	integer :: i, pos0

	members = new_syntax_node_vector()
	i = 0

	left  = parser%match(lbrace_token)

	call parser%vars%push_scope()

	do while ( &
		parser%current_kind() /= eof_token .and. &
		parser%current_kind() /= rbrace_token)

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		call members%push(parser%parse_statement())

		! Avoid infinite loops on malformed blocks like this:
		!   {
		!     4) + 5;
		!   }
		if (parser%pos == pos0) dummy = parser%next()

	end do

	call parser%vars%pop_scope()

	right = parser%match(rbrace_token)

	block%kind = block_statement

	! Convert to standard array
	call syntax_nodes_copy(block%members, members%v( 1: members%len ))

end function parse_block_statement

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

recursive function parse_expr_statement(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, ltype, rtype, pos0, span0, span1

	type(syntax_node_t) :: right
	type(syntax_token_t) :: let, identifier, op

	type(text_span_t) :: span

	!print *, 'starting parse_expr_statement()'

	! TODO: provide a way to declare variable types without initializing them?
	! Rust discourages mutability, instead preferring patterns like this:
	!
	!      let x = if condition
	!      {
	!          y
	!      }
	!      else
	!      {
	!          z
	!      };
	!
	! The above might be hard to do, as it would require checking that the types
	! of both condition branches match the LHS type

	if (parser%peek_kind(0) == let_keyword      .and. &
	    parser%peek_kind(1) == identifier_token .and. &
	    parser%peek_kind(2) == equals_token) then

		!print *, 'let expr'

		! The if-statement above already verifies tokens, so we can use next()
		! instead of match() here

		let        = parser%next()
		identifier = parser%next()
		op         = parser%next()

		right      = parser%parse_expr_statement()
		!right      = parser%parse_expr()

		!! I think the way to get conditional initialization like rust is
		!! something like this.  May need to peek current and check if it's
		!! if_keyword or not
		!right      = parser%parse_statement()
		!!semi       = parser%match(semicolon_token)

		expr = new_declaration_expr(identifier, op, right)

		!print *, 'expr ident text = ', expr%identifier%text

		! Increment the variable array index and save it in the expr node.
		! TODO: make this a push_var fn?  parse_for_statement uses it too
		parser%num_vars = parser%num_vars + 1
		expr%id_index   = parser%num_vars

		! Insert the identifier's type into the dict and check that it
		! hasn't already been declared
		call parser%vars%insert(identifier%text, expr%val, &
			expr%id_index, io, overwrite = .false.)

		!print *, 'io = ', io
		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_redeclare_var(parser%context, &
				span, identifier%text))
		end if

		return

	end if

	if (parser%peek_kind(0) == identifier_token) then

		! There may or may not be a subscript expression after an identifier, so
		! we can't know how many spaces ahead an equals_token might be without
		! looking ahead

		! %pos is the lexer token index, %current_pos() is the character index!
		pos0 = parser%pos

		!print *, 'assign expr'

		identifier = parser%match(identifier_token)

		! Parse array subscript indices if present

		! Subscript can appear in assignment expr but not let expr, because let
		! must initialize the whole array
		span0 = parser%current_pos()
		call parser%parse_subscripts(expr%subscripts, expr%usubscripts)

		if (size(expr%subscripts) <= 0) deallocate(expr%subscripts)
		span1 = parser%current_pos() - 1

		if (.not. is_assignment_op(parser%current_kind())) then
			! Rewind and do the default case (same as outside the assignment if
			! block).  Could use goto or probably refactor somehow
			parser%pos = pos0
			!print *, 'pos0 = ', pos0
			expr = parser%parse_expr()
			return
		end if
		!print *, 'parsing assignment'

		op    = parser%next()
		right = parser%parse_expr_statement()

		! regular vs compound assignment exprs are denoted by the op.  all of
		! them are the same kind
		expr%kind = assignment_expr

		allocate(expr%right)

		expr%identifier = identifier

		expr%op    = op
		expr%right = right

		!print *, 'expr ident text = ', expr%identifier%text

		! Get the identifier's type and index from the dict and check that it
		! has been declared
		expr%val = parser%vars%search(identifier%text, expr%id_index, io)

		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context, &
				span, identifier%text))
		end if

		!print *, 'type = ', kind_name(expr%val%type)

		!print *, 'associated(expr%val%array) = ', associated(expr%val%array)

		if (size(expr%subscripts) > 0) then

			if (expr%val%type == str_type) then
				!print *, 'str type'
				! TODO: check rank == 1
			else if (expr%val%type /= array_type) then
				span = new_span(span0, span1 - span0 + 1)
				call parser%diagnostics%push( &
					err_scalar_subscript(parser%context, &
					span, identifier%text))
				return
			end if

			!print *, 'type = ', expr%val%type

			if (expr%val%type /= str_type) then
				expr%val%type = expr%val%array%type

				!print *, 'rank = ', expr%val%array%rank
				!print *, 'subs = ', size(expr%subscripts)

				if (expr%val%array%rank /= size(expr%subscripts)) then
					span = new_span(span0, span1 - span0 + 1)
					call parser%diagnostics%push( &
						err_bad_sub_count(parser%context, span, identifier%text, &
						expr%val%array%rank, size(expr%subscripts)))
				end if
			end if

		end if

		ltype = expr%val%type
		rtype = expr%right%val%type

		!if (rtype == array_type) rtype = expr%right%val%array%type
		!if (rtype == array_type) rtype = right%val%array%type

		! This check could be moved inside of is_binary_op_allowed, but we would
		! need to pass parser to it to push diagnostics
		if (.not. is_binary_op_allowed(ltype, op%kind, rtype)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context, &
				span, op%text, &
				kind_name(ltype), &
				kind_name(rtype)))

		end if

		return

	end if

	expr = parser%parse_expr()
	!semi       = parser%match(semicolon_token)

end function parse_expr_statement

!===============================================================================

logical function is_assignment_op(op)

	! Is the operator some type of assignment operator, either regular or
	! compound (augmented)?

	integer, intent(in) :: op

	is_assignment_op = any(op == [equals_token, plus_equals_token, &
		minus_equals_token, star_equals_token, slash_equals_token])

end function is_assignment_op

!===============================================================================

subroutine parse_subscripts(parser, subscripts, usubscripts)
	! TODO: just take whole expr as arg, instead of subscripts AND usubscripts

	! Parse array subscript if present

	class(parser_t) :: parser
	type(syntax_node_t), intent(out), allocatable :: subscripts(:), &
		usubscripts(:)

	!********

	integer :: pos0, span0

	type(syntax_node_t) :: subscript, usubscript
	type(syntax_node_vector_t) :: subscripts_vec, usubscripts_vec
	type(syntax_token_t) :: lbracket, rbracket, comma, &
		dummy, colon

	type(text_span_t) :: span

	if (parser%current_kind() /= lbracket_token) then

		!! The function has to return something.  Caller deallocates
		!subscripts = []
		allocate( subscripts(0))
		!allocate(usubscripts(0))
		return

	end if

	!print *, 'parsing subscripts'

	 subscripts_vec = new_syntax_node_vector()
	usubscripts_vec = new_syntax_node_vector()

	lbracket  = parser%match(lbracket_token)

	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		pos0  = parser%pos
		span0 = parser%current_pos()
		subscript = parser%parse_expr()

		!print *, 'subscript = ', subscript%str()
		!print *, 'subscript = ', parser%context%text(span0: parser%current_pos()-1)

		if (subscript%val%type /= i32_type) then
			span = new_span(span0, parser%current_pos() - span0)
			call parser%diagnostics%push( &
				err_non_int_subscript(parser%context, span, &
				parser%context%text(span0: parser%current_pos()-1)))
		end if

		! TODO: set step_sub case for non-unit range step
		if (parser%current_kind() == colon_token) then
			colon = parser%match(colon_token)
			subscript%sub_kind = range_sub

			usubscript = parser%parse_expr()
			! TODO: type check i32

		else
			subscript%sub_kind = scalar_sub

			! TODO: initialize empty usubscript struct

		end if

		! Parallel arrays subscripts and usubscripts should be same size? Not
		! sure how much will need to change for multi-rank ranges
		call  subscripts_vec%push( subscript)
		call usubscripts_vec%push(usubscript)

		! Break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

	!print *, 'parsing rbracket'
	rbracket  = parser%match(rbracket_token)
	!print *, 'done'

	! TODO: check that # of subscripts matches array rank, both LHS and
	! RHS parsing.  May need to pass identifier to this function.  LHS and RHS
	! cases are different in tricky ways.  RHS has already lookup up identifier
	! in vars dictionary when it calls parse_subscripts(), but LHS has not.
	! When LHS calls this, it does not yet know whether the identifier is an
	! array or a scalar or a function call in an expression statement.
	!
	! Check that the expr is actually an array (not a scalar), or do that next
	! to err_bad_sub_count() elsewhere
	!
	! So, only check rank match here if subscripts%len > 0

	call syntax_nodes_copy(subscripts, &
		subscripts_vec%v( 1: subscripts_vec%len ))

	call syntax_nodes_copy(usubscripts, &
		usubscripts_vec%v( 1: usubscripts_vec%len ))

end subroutine parse_subscripts

!===============================================================================

recursive function parse_expr(parser, parent_prec) result(expr)

	! In episode 3, Immo renamed this fn to "ParseBinaryExpression()", but
	! I consider that confusing because the result could be either unary or
	! binary

	class(parser_t) :: parser

	integer, optional, intent(in) :: parent_prec

	type(syntax_node_t) :: expr

	!********

	integer :: parent_precl, prec, ltype, rtype

	type(syntax_node_t) :: right
	type(syntax_token_t) :: op
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_expr'
	if (debug > 1) print *, 'pos = ', parser%pos

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

		rtype = right%val%type

		!! segfault
		!if (rtype == array_type) rtype = right%val%array%type

		if (.not. is_unary_op_allowed(op%kind, rtype)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_unary_types(parser%context, span, op%text, &
				kind_name(expr%right%val%type)))

		end if

	else
		expr = parser%parse_primary_expr()
	end if

	do
		prec = get_binary_op_prec(parser%current_kind())
		if (prec == 0 .or. prec <= parent_precl) exit

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_binary_expr(expr, op, right)

		ltype = expr%left %val%type
		rtype = expr%right%val%type

		!if (ltype == array_type) ltype = expr%left %val%array%type
		!if (rtype == array_type) rtype = expr%right%val%array%type

		if (.not. is_binary_op_allowed(ltype, op%kind, rtype)) then

			!print *, 'bin not allowed in parse_expr'

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context, &
				span, op%text, &
				kind_name(ltype), &
				kind_name(rtype)))

		end if

	end do

end function parse_expr

!===============================================================================

logical function is_num_type(type)

	integer, intent(in) :: type

	! FIXME: other numeric types (i64, f64, etc.)
	is_num_type = type == i32_type .or. type == f32_type

end function is_num_type

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
				slash_equals_token)

			is_binary_op_allowed = is_num_type(left) .and. is_num_type(right)

		case (and_keyword, or_keyword)
			is_binary_op_allowed = left == bool_type .and. right == bool_type

		case (equals_token, eequals_token, bang_equals_token)

			! Fortran allows comparing ints and floats for strict equality, e.g.
			! 1 == 1.0 is indeed true.  I'm not sure if I like that
			is_binary_op_allowed = left == right

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

! You can't chain together member fn calls and their children like
! parser%current()%kind in Fortran, so use this helper fn instead

integer function current_kind(parser)
	class(parser_t) :: parser
	current_kind = parser%peek_kind(0)
end function current_kind

integer function peek_kind(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_kind = peek%kind
end function peek_kind

!********

integer function current_pos(parser)

	! Get the current character index.  If you want the token index, use
	! parser%pos instead

	class(parser_t) :: parser

	current_pos = parser%peek_pos(0)

end function current_pos

integer function peek_pos(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_pos = peek%pos
end function peek_pos

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

function parse_size(parser) result(size)

	class(parser_t) :: parser

	type(syntax_node_vector_t) :: size

	!********

	integer :: span_beg, span_end

	type(syntax_node_t)  :: len
	type(syntax_token_t) :: comma
	type(text_span_t) :: span

	size = new_syntax_node_vector()
	do while ( &
		parser%current_kind() /= rbracket_token .and. &
		parser%current_kind() /= eof_token)

		span_beg = parser%peek_pos(0)
		len      = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'len = ', parser%context%text(span_beg: span_end)

		if (len%val%type /= i32_type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			! TODO: different diag for each (or at least some) case
			call parser%diagnostics%push(err_non_int_range( &
				parser%context, span, &
				parser%context%text(span_beg: span_end)))
		end if

		call size%push(len)

		! TODO: break infinite loop?

		if (parser%current_kind() /= rbracket_token) then
			comma = parser%match(comma_token)
		end if

	end do

end function parse_size

!===============================================================================

function parse_array_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: span_beg, span_end, pos0

	type(syntax_node_t)  :: lbound, step, ubound, len, elem
	type(syntax_node_vector_t) :: elems, size
	type(syntax_token_t) :: lbracket, rbracket, colon, semicolon, comma, dummy
	type(text_span_t) :: span

	!print *, 'starting parse_array_expr()'

	! This function parses arrays of the following forms:
	!
	!     // i32
	!     let a = [imin:        imax];      // current loop syntax
	!     let a = [imin: istep: imax];
	!     let a = [iconst           ; len]; // this one is like Rust
	!
	!     // f32
	!     let a = [fmin: fstep: fmax];      // consistent with i32
	!     let a = [fmin:        fmax; len]; // no default unit step like i32
	!     let a = [fconst           ; len];
	!
	!     // Rank-2, rank-3, etc.  No range variations, only all elements
	!     // the same value
	!     let a = [fconst           ; rows, cols];  // row-major like Fortran
	!     let a = [fconst           ; rows, cols, sheets];
	!
	!     // Explicit list for any rank-1 type
	!     [elem_0, elem_1, elem_2, ... ]
	!
	! A note on the term "rank-1":  I think there's a good argument to be made
	! that for a language with 0-based arrays, we should call vectors "rank-0"
	! and matrices "rank-1".  However, I'm calling them "rank-1" and "rank-2"
	! respectively, as that's what Fortran calls them and I hadn't thought that
	! far ahead :)
	!
	! NumPy uses the same convention for "rank-1" as us.  In fact, NumPy has
	! something below a vector called a "rank-0" array

	lbracket = parser%match(lbracket_token)

	span_beg = parser%peek_pos(0)
	lbound   = parser%parse_expr()
	span_end = parser%peek_pos(0) - 1

	!print *, 'lbound = ', parser%context%text(span_beg: span_end)

	! TODO: should type checking be done by caller, or should we pass an
	! expected type arg for the RHS of this check?

	! TODO: check if lbound%val is allocated, e.g. for assigning one array to
	! a cat of another?  How would this work for rank-2+?
	!
	!     let a = [0: 3];
	!     let b = [a, 5, 6];
	!              ^ segfault

	!! TODO: there should still be *some* type checking.  At least, implicit
	!! ranges cannot use bool
	!if (lbound%val%type /= i32_type) then
	!	span = new_span(span_beg, span_end - span_beg + 1)
	!	call parser%diagnostics%push(err_non_int_range( &
	!		parser%context, span, parser%context%text(span_beg: span_end)))
	!end if

	if (parser%current_kind() == semicolon_token) then

		! Implicit constant-value array form [lbound; len]

		semicolon    = parser%match(semicolon_token)

		! rank-2+ arrays:
		!
		! [lbound; rows, cols]
		! [lbound; rows, cols, sheets, ...]

		size = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)
		allocate(expr%lbound)
		allocate(expr%len)

		call syntax_nodes_copy(expr%size, size%v( 1: size%len ))

		expr%kind           = array_expr

		expr%val%type       = array_type

		expr%val%array%type = lbound%val%type
		expr%val%array%kind = impl_array
		expr%val%array%rank = size%len

		!print *, 'expr%val%type       = ', expr%val%type
		!print *, 'expr%val%array%type = ', expr%val%array%type

		! Does this syntax node need to own these members, or can we just save
		! them in the array_t?  I think they do need to be duplicated, as they
		! may be an expression and not just a literal.  So, sizes have to be
		! allocated dynamically during evaluation, not during parsing

		expr%lbound = lbound
		expr%len    = len

		return

	end if

	if (parser%current_kind() == colon_token) then

		! Implicit array form unit step [lbound: ubound] or [lbound: step: ubound]
		colon    = parser%match(colon_token)

		span_beg = parser%peek_pos(0)
		ubound   = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		if (ubound%val%type /= lbound%val%type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_non_int_range( &
				parser%context, span, parser%context%text(span_beg: span_end)))
		end if

		if (parser%current_kind() == colon_token) then

			! Implicit form [lbound: step: ubound]

			! Step has just been parsed as ubound above
			step = ubound

			colon    = parser%match(colon_token)

			span_beg = parser%peek_pos(0)
			ubound   = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			if (ubound%val%type /= lbound%val%type) then
				span = new_span(span_beg, span_end - span_beg + 1)
				call parser%diagnostics%push(err_non_int_range( &
					parser%context, span, &
					parser%context%text(span_beg: span_end)))
			end if

			! If [lbound: step: ubound] are all specified, then specifying the
			! len would be overconstrained!  Next token must be rbracket

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%step)
			allocate(expr%ubound)

			expr%kind           = array_expr

			!expr%val%type       = lbound%val%type
			expr%val%type       = array_type

			expr%val%array%type = lbound%val%type
			expr%val%array%kind = impl_array
			expr%val%array%rank = 1

			expr%lbound = lbound
			expr%step   = step
			expr%ubound = ubound

			return

		end if

		if (parser%current_kind() == semicolon_token) then

			! Implicit form [lbound: ubound; len]

			semicolon    = parser%match(semicolon_token)

			span_beg = parser%peek_pos(0)
			len      = parser%parse_expr()
			span_end = parser%peek_pos(0) - 1

			!print *, 'len = ', parser%context%text(span_beg: span_end)

			if (len%val%type /= i32_type) then
				span = new_span(span_beg, span_end - span_beg + 1)
				! TODO: different diag for each (or at least some) case
				call parser%diagnostics%push(err_non_int_range( &
					parser%context, span, &
					parser%context%text(span_beg: span_end)))
			end if

			rbracket = parser%match(rbracket_token)

			allocate(expr%val%array)
			allocate(expr%lbound)
			allocate(expr%ubound)
			allocate(expr%len)

			expr%kind           = array_expr

			!expr%val%type       = lbound%val%type
			expr%val%type       = array_type

			expr%val%array%type = lbound%val%type
			expr%val%array%kind = impl_array
			expr%val%array%rank = 1

			expr%lbound = lbound
			expr%ubound = ubound
			expr%len    = len

			return

		end if

		! Implicit array form unit step [lbound: ubound]

		rbracket = parser%match(rbracket_token)

		!print *, 'lbound = ', lbound%str()
		!print *, 'ubound = ', ubound%str()

		allocate(expr%val%array)
		allocate(expr%lbound)
		allocate(expr%ubound)

		expr%kind                 = array_expr

		!expr%val%type             = lbound%val%type
		expr%val%type       = array_type

		expr%val%array%type       = lbound%val%type
		expr%val%array%kind       = impl_array
		expr%val%array%rank = 1

		expr%lbound = lbound
		expr%ubound = ubound

		return

	end if

	! Explicit array form [elem_0, elem_1, elem_2, ... ].  elem_0 has already been
	! parsed as lbound above

	!print *, 'elem ', lbound%val%str()

	elems = new_syntax_node_vector()
	call elems%push(lbound)
	do while (&
		parser%current_kind() /= rbracket_token  .and. &
		parser%current_kind() /= semicolon_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos
		comma    = parser%match(comma_token)

		span_beg = parser%peek_pos(0)
		elem     = parser%parse_expr()
		span_end = parser%peek_pos(0) - 1

		!print *, 'elem ', elem%val%str()

		if (elem%val%type /= lbound%val%type) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_het_array( &
				parser%context, span, parser%context%text(span_beg: span_end)))
		end if

		call elems%push(elem)

		! break infinite loop
		if (parser%pos == pos0) dummy = parser%next()

	end do

	if (parser%current_kind() == semicolon_token) then

		! Rank-2+ array: [elem_0, elem_1, elem_2, ... ; size_0, size_1, ... ];
		semicolon = parser%match(semicolon_token)

		size = parser%parse_size()

		rbracket = parser%match(rbracket_token)

		allocate(expr%val%array)

		call syntax_nodes_copy(expr%size, size%v( 1: size%len ))

		expr%kind           = array_expr

		expr%val%type       = array_type

		expr%val%array%type = lbound%val%type
		expr%val%array%kind = expl_array
		expr%val%array%rank = size%len

		call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len ))

		return

	end if

	! Rank-1 array (size is implicitly defined by number of elements)

	rbracket = parser%match(rbracket_token)

	allocate(expr%val%array)
	expr%kind           = array_expr

	!expr%val%type       = lbound%val%type
	expr%val%type       = array_type

	expr%val%array%type = lbound%val%type
	expr%val%array%kind = expl_array
	expr%val%array%rank = 1

	call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len ))

	! TODO: allow arbitrarily combinations catting expl and impl arrays, e.g.
	!
	!        [0: 3   ,   5, 6,   10: 13    ]
	!     // [0, 1, 2,   5, 6,   10, 11, 12]
	!
	! But how useful would that really be?

end function parse_array_expr

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	character(len = :), allocatable :: param_type, arg_type
	integer :: i, io, id_index, param_rank, arg_rank, span0, span1, &
		ptype, atype, exp_rank
	logical :: bool, types_match

	type(fn_t) :: fn
	type(syntax_node_t) :: arg
	type(syntax_node_vector_t) :: args
	type(syntax_token_t) :: left, right, keyword, identifier, &
		comma, lparen, rparen, token
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_primary_expr'

	select case (parser%current_kind())

		case (lparen_token)

			! Left and right parens are not explicitly included as nodes in the
			! parse tree, they just change the connectivity of the tree

			left  = parser%next()

			! These two lines are the difference between allowing statement
			! "a = (b = 1)" or not.  Note that "a = b = 1" is allowed either way

			!expr  = parser%parse_expr()
			expr  = parser%parse_expr_statement()

			right = parser%match(rparen_token)

		case (lbracket_token)

			! Brackets are matched within parse_array_expr
			expr = parser%parse_array_expr()

			!print *, '2 expr%val%type = ', expr%val%type
			!print *, '2 expr%val%array%type = ', expr%val%array%type

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			expr = new_bool(bool)

			!print *, 'expr%val%bool = ', expr%val%bool

		case (identifier_token)

			! TODO: make fns for these long, deeply-nested cases

			if (parser%peek_kind(1) /= lparen_token) then

				! Variable name expression

				identifier = parser%match(identifier_token)

				!print *, 'RHS identifier = ', identifier%text
				!print *, '%current_kind() = ', kind_name(parser%current_kind())

				!print *, 'searching'
				expr = new_name_expr(identifier, &
					parser%vars%search(identifier%text, id_index, io))
				expr%id_index = id_index

				if (io /= exit_success) then
					span = new_span(identifier%pos, len(identifier%text))
					call parser%diagnostics%push( &
						err_undeclare_var(parser%context, &
						span, identifier%text))
				end if

				!print *, 'type = ', kind_name(expr%val%type)
				!print *, 'associated(expr%val%array) = ', &
				!	associated(expr%val%array)

				!print *, '%current_kind() = ', kind_name(parser%current_kind())
				span0 = parser%current_pos()
				call parser%parse_subscripts(expr%subscripts, expr%usubscripts)

				span1 = parser%current_pos() - 1
				if (size(expr%subscripts) <= 0) then
					deallocate(expr%subscripts)
				else if (expr%val%type == array_type) then

					! this is not necessarily true for strings
					expr%val%type = expr%val%array%type

					! TODO: allow rank+1 for str arrays
					if (expr%val%array%rank /= size(expr%subscripts)) then
						span = new_span(span0, span1 - span0 + 1)
						call parser%diagnostics%push( &
							err_bad_sub_count(parser%context, span, &
							identifier%text, &
							expr%val%array%rank, size(expr%subscripts)))
					end if

				else if (expr%val%type == str_type) then
					!print *, 'string type'

					exp_rank = 1
					if (size(expr%subscripts) /= exp_rank) then
						span = new_span(span0, span1 - span0 + 1)
						call parser%diagnostics%push( &
							err_bad_sub_count(parser%context, span, &
							identifier%text, &
							exp_rank, size(expr%subscripts)))
					end if
				else
					span = new_span(span0, span1 - span0 + 1)
					call parser%diagnostics%push( &
						err_scalar_subscript(parser%context, &
						span, identifier%text))
				end if

			else

				! Function call expression
				identifier = parser%match(identifier_token)

				!print *, 'parsing fn_call_expr'
				!print *, 'identifier = ', identifier%text

				args = new_syntax_node_vector()
				lparen  = parser%match(lparen_token)

				do while ( &
					parser%current_kind() /= rparen_token .and. &
					parser%current_kind() /= eof_token)

					arg = parser%parse_expr()
					call args%push(arg)

					if (parser%current_kind() /= rparen_token) then
						comma = parser%match(comma_token)
					end if
				end do

				rparen  = parser%match(rparen_token)

				fn = parser%fns%search(identifier%text, id_index, io)
				if (io /= exit_success) then

					span = new_span(identifier%pos, len(identifier%text))
					call parser%diagnostics%push( &
						err_undeclare_fn(parser%context, &
						span, identifier%text))

					! No more tokens are consumed below, so we can just return
					! to skip cascading fn arg count/type errors
					return

				end if

				expr%kind = fn_call_expr

				expr%identifier = identifier

				expr%val%type = fn%type
				if (fn%type == array_type) then
					allocate(expr%val%array)
					expr%val%array%type = fn%array_type
					expr%val%array%rank = fn%rank
				end if

				! Intrinsic fns don't have a syntax node: they are implemented
				! in Fortran, not syntran
				if (associated(fn%node)) then
					!print *, 'assigning fn node'

					! If I understand my own code, this is inlining:  every fn
					! call gets its own copy of the fn body.  This expansion
					! happens at parse time, not eval time, so fn calls in
					! a loop will all share one body

					! TODO: could we do this with a pointer instead? I think
					! copying is a waste of memory.  Also try to encapsulate
					! both body and params into a wrapped type (fn_t?)

					allocate(expr%body)
					expr%body = fn%node%body
					expr%params = fn%node%params

				end if

				! TODO: does fn need to be a syntax node member?  I think we can
				! just look it up later by identifier/id_index like we do for
				! variable value
				!expr%fn = fn

				!print *, 'fn params size = ', size(fn%params)
				if (fn%variadic_min < 0 .and. size(fn%params) /= args%len) then

					span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
					call parser%diagnostics%push( &
						err_bad_arg_count(parser%context, &
						span, identifier%text, size(fn%params), args%len))
					return

				else if (args%len < size(fn%params) + fn%variadic_min) then

					span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
					call parser%diagnostics%push( &
						err_too_few_args(parser%context, &
						span, identifier%text, &
						size(fn%params) + fn%variadic_min, args%len))
					return

				end if

				do i = 1, args%len
					!print *, kind_name(args%v(i)%val%type)
					!print *, kind_name(fn%params(i)%type)

					! For variadic fns, check the argument type against the type
					! of the last required parameter.  This may need to change,
					! e.g. writeln(file) should write a blank line to a file,
					! but writeln(file, string1, string2), where string* is not
					! the same type as file?

					! TODO: re-test min/max arg count/type checking

					!! We want println() to just print an empty line
					!if (fn%variadic_min == 0) exit

					if (i <= size(fn%params)) then
						ptype = fn%params(i)%type
					else
						ptype = fn%variadic_type
					end if

					!j = i
					!if (fn%variadic_min > 0) j = fn%variadic_min
					!ptype = fn%params(j)%type

					types_match = &
						ptype == any_type .or. ptype == args%v(i)%val%type

					if (.not. types_match) then

						! TODO: get span of individual arg, not whole arg list
						span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
						call parser%diagnostics%push( &
							err_bad_arg_type(parser%context, &
							span, identifier%text, i, fn%params(i)%name, &
							kind_name(ptype), &
							kind_name(args%v(i)%val%type)))
						return

					end if

					! TODO: fns w/ variadic array params are not implemented
					if (fn%variadic_min >= 0 .and. i > size(fn%params)) cycle

					if (ptype == array_type) then
						atype = fn%params(i)%array_type
						types_match = &
							atype == any_type .or. &
							atype == args%v(i)%val%array%type
					end if

					if (.not. types_match) then

						span = new_span(lparen%pos, rparen%pos - lparen%pos + 1)
						param_type = kind_name( atype)
						arg_type   = kind_name(args%v(i)%val%array%type)

						call parser%diagnostics%push( &
							err_bad_array_arg_type(parser%context, &
							span, identifier%text, i, fn%params(i)%name, &
							param_type, arg_type))
						return

					end if

					if (ptype == array_type) then
						param_rank = fn%params(i)%rank
						arg_rank = args%v(i)%val%array%rank

						if (param_rank >= 0 .and. param_rank /= arg_rank) then

							span = new_span(lparen%pos, &
								rparen%pos - lparen%pos + 1)

							call parser%diagnostics%push( &
								err_bad_arg_rank(parser%context, &
								span, identifier%text, i, fn%params(i)%name, &
								param_rank, arg_rank))
							return

						end if

					end if

				end do

				expr%id_index = id_index

				call syntax_nodes_copy(expr%args, &
					args%v( 1: args%len ))

				!print *, 'done parsing fn_call_expr'

			end if

		case (f32_token)

			token = parser%match(f32_token)
			expr  = new_f32(token%val%f32)

		case (str_token)

			token = parser%match(str_token)
			expr  = new_str(token%val%str%s)

		case default

			token = parser%match(i32_token)
			expr  = new_i32(token%val%i32)

			if (debug > 1) print *, 'token = ', expr%val%to_str()

	end select

end function parse_primary_expr

!===============================================================================

function new_name_expr(identifier, val) result(expr)

	type(syntax_token_t), intent(in) :: identifier
	type(syntax_node_t) :: expr
	type(value_t) :: val

	expr%kind = name_expr
	expr%identifier = identifier
	expr%val = val

	!if (expr%val%type == array_type) then
	!	!if (.not. associated(expr%val%array)) allocate(expr%val%array)
	!	allocate(expr%val%array)
	!	!expr%val%array = right%val%array
	!	expr%val%array = val%array
	!end if

end function new_name_expr

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

function new_str(str) result(expr)

	character(len = *), intent(in) :: str
	type(syntax_node_t) :: expr

	expr%kind = literal_expr
	expr%val  = new_literal_value(str_type, str = str)

end function new_str

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
			! int + int casts to float
			get_binary_op_kind = f32_type
			return
		end if

		get_binary_op_kind = unknown_type

	end select

end function get_binary_op_kind

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

function match(parser, kind) result(token)

	class(parser_t) :: parser

	integer :: kind

	type(syntax_token_t) :: token

	!********

	integer :: len_text

	type(syntax_token_t) :: current
	type(text_span_t) :: span

	! If current_text() and current_pos() helper fns are added, this local var
	! current can be eliminated
	current = parser%current()

	if (parser%current_kind() == kind) then
		token = parser%next()
		!print *, 'returning parser expecting false'
		return
	end if

	!! A continued expression can commonly have several unmatched tokens.  The
	!! last one is usually a semicolon, or it could be a right brace.  The first
	!! one is more helpful for the user to know
	!print *, 'unmatched '//kind_name(kind)
	!print *, 'unmatched '//kind_token(kind)

	if (.not. parser%first_expecting) then
		parser%first_expected  = kind_token(kind)
		parser%first_expecting = .true.
	end if

	len_text = max(len(current%text), 1)
	span = new_span(current%pos, len_text)
	call parser%diagnostics%push( &
		err_unexpected_token(parser%context, span, current%text, &
		kind_name(parser%current_kind()), kind_name(kind)))

	! An unmatched char in the middle of the input is an error and should log
	! a diagnostic.  An unmatched char at the end means the interactive
	! interpreter should expect more lines
	if (parser%pos >= size(parser%tokens)) then
		parser%expecting = .true.
	end if

	token = new_token(kind, current%pos, null_char)

end function match

!===============================================================================

function current_token(parser)
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token
	current_token = parser%peek(0)
end function current_token

function parser_peek_token(parser, offset) result(token)

	class(parser_t) :: parser

	type(syntax_token_t) :: token

	integer, intent(in) :: offset

	!********

	integer :: pos

	pos = parser%pos + offset

	if (debug > 2) print *, 'token pos ', pos

	if (pos > size(parser%tokens)) then
		token = parser%tokens( size(parser%tokens) )
		return
	end if

	token = parser%tokens(pos)

end function parser_peek_token

!===============================================================================

function next_parser_token(parser) result(next)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next_parser_token

!===============================================================================

integer function subscript_eval(node, vars, fns, quietl) result(index)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index

	type(syntax_node_t) :: node
	type(vars_t) :: vars
	type(fns_t) :: fns
	logical, intent(in) :: quietl

	!******

	integer :: i, prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	! str scalar with single char subscript
	if (vars%vals(node%id_index)%type == str_type) then
		subscript = syntax_eval(node%subscripts(1), vars, fns, quietl)
		index = subscript%i32
		return
	end if

	!if (vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

	prod  = 1
	index = 0
	do i = 1, vars%vals(node%id_index)%array%rank
		!print *, 'i = ', i

		subscript = syntax_eval(node%subscripts(i), vars, fns, quietl)

		! TODO: bound checking? by default or enabled with cmd line flag?

		index = index + prod * subscript%i32
		prod  = prod * vars%vals(node%id_index)%array%size(i)

	end do

end function subscript_eval

!===============================================================================

recursive function syntax_eval(node, vars, fns, quiet) result(res)

	! TODO: encapsulate vars, fns, and quiet into a state struct.  Add
	! diagnostics to state for runtime errors (bounds overflow, rank mismatch,
	! etc.)

	type(syntax_node_t) :: node

	! I don't want to make this arg optional, because then it would require
	! copying a potentially large struct to a local var without fancy use of
	! move_alloc()
	type(vars_t) :: vars

	type(fns_t) :: fns

	logical, optional, intent(in) :: quiet

	type(value_t) :: res

	!********

	integer :: i, j, il, iu, io

	logical :: quietl

	real(kind = 4) :: f, fstep

	type(array_t) :: array
	type(value_t) :: left, right, condition, lbound, ubound, itr, elem, &
		step, len, arg, arg1, arg2, array_val

	!print *, 'starting syntax_eval()'

	quietl = .false.
	if (present(quiet)) quietl = quiet

	if (node%is_empty) then
		!print *, 'returning'
		return
	end if

	!********

	! I'm being a bit loose with consistency on select case indentation but
	! I don't want a gigantic diff

	select case (node%kind)

	case (literal_expr)
		! This handles both ints, bools, etc.
		res = node%val
		!print *, 'res = ', res%str()

	case (array_expr)

		!print *, 'evaluating array_expr'

		if (node%val%array%kind == impl_array .and. allocated(node%step)) then

			! step-based impl array
			!
			! TODO: make several more impl_*_array enum variations for each impl
			! array form instead of checking whether various syntax nodes are
			! allocated

			lbound = syntax_eval(node%lbound, vars, fns, quietl)
			step   = syntax_eval(node%step  , vars, fns, quietl)
			ubound = syntax_eval(node%ubound, vars, fns, quietl)

			array%type = node%val%array%type

			if (array%type == i32_type) then

				array%cap = (ubound%i32 - lbound%i32) / step%i32 + 1
				allocate(array%i32( array%cap ))

				j = 1
				i = lbound%i32
				if (lbound%i32 < ubound%i32 .neqv. 0 < step%i32) i = ubound%i32

				! Step may be negative
				do while ((i  < ubound%i32 .eqv. lbound%i32 < ubound%i32) &
				     .and. i /= ubound%i32)
					array%i32(j) = i
					i = i + step%i32
					j = j + 1
				end do
				array%len = j - 1

			else if (array%type == f32_type) then

				!print *, 'lbound, ubound = ', lbound%f32, ubound%f32
				!print *, 'step = ', step%f32

				array%cap = ceiling((ubound%f32 - lbound%f32) / step%f32) + 1
				allocate(array%f32( array%cap ))

				j = 1
				f = lbound%f32
				if (lbound%f32 < ubound%f32 .neqv. 0 < step%f32) f = ubound%f32

				do while ((f  < ubound%f32 .eqv. lbound%f32 < ubound%f32) &
				     .and. f /= ubound%f32)
					array%f32(j) = f
					f = f + step%f32
					j = j + 1
				end do
				array%len = j - 1

			else
				write(*,*) 'Error: step array type eval not implemented'
				call internal_error()
			end if

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len

			allocate(res%array)
			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array &
			.and. allocated(node%ubound) .and. allocated(node%len)) then

			! len-based impl arrays

			!print *, 'len array'
			lbound = syntax_eval(node%lbound, vars, fns, quietl)
			ubound = syntax_eval(node%ubound, vars, fns, quietl)
			len    = syntax_eval(node%len   , vars, fns, quietl)

			!array = new_array(node%val%array%type)
			!elem = lbound

			array%type = node%val%array%type
			array%len  = len%i32
			array%cap  = array%len

			if (array%type == f32_type) then

				allocate(array%f32( array%cap ))
				fstep = (ubound%f32 - lbound%f32) / (len%i32 - 1)

				do i = 0, len%i32 - 1
					array%f32(i+1) = lbound%f32 + i * fstep
					!elem%f32 = lbound%f32 + i * fstep
					!call array%push(elem)
				end do

			else
				write(*,*) 'Error: bound/len array type eval not implemented'
				call internal_error()
			end if

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len

			allocate(res%array)

			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array .and. allocated(node%len)) then

			! Initialize rank-2+ arrays
			if (allocated(node%size)) then

				array%rank = size( node%size )
				allocate(array%size( array%rank ))

				do i = 1, array%rank
					len = syntax_eval(node%size(i), vars, fns, quietl)
					array%size(i) = len%i32
					!print *, 'size['//str(i)//'] = ', array%size(i)
				end do

			end if

			! Constant-value impl arrays (i.e. every element has the same value
			! at initialization, but they are of course mutable)

			!print *, 'len array'
			lbound = syntax_eval(node%lbound, vars, fns, quietl)

			! Allocate in one shot without growing

			array%type = node%val%array%type
			array%len  = product(array%size)
			array%cap  = array%len

			!print *, 'array%len = ', array%len

			if      (array%type == i32_type) then
				allocate(array%i32( array%cap ))
				array%i32 = lbound%i32

			else if (array%type == f32_type) then
				allocate(array%f32( array%cap ))
				array%f32 = lbound%f32

			else if (array%type == str_type) then
				allocate(array%str( array%cap ))
				array%str = lbound%str

			else
				write(*,*) 'Error: len array type eval not implemented'
				call internal_error()
			end if

			allocate(res%array)

			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array) then
			!print *, 'impl_array'

			! Unit step impl i32 array

			! Expand impl_array to expl_array here on evaluation.  Consider
			! something like this:
			!
			!     let a = [0: 5];
			!     a[2] = -3;
			!
			! Even though a is initialized to an implicit array, the second
			! statement requires it to be explicit, so we might as well expand
			! at initialization

			lbound = syntax_eval(node%lbound, vars, fns, quietl)
			ubound = syntax_eval(node%ubound, vars, fns, quietl)

			!array = new_array(node%val%array%type)

			array%type = node%val%array%type

			if (array%type /= i32_type) then
				write(*,*) 'Error: unit step array type eval not implemented'
				call internal_error()
			end if

			array%len = ubound%i32 - lbound%i32
			array%cap = array%len

			allocate(array%i32( array%cap ))

			!print *, 'bounds in [', lbound%str(), ': ', ubound%str(), ']'
			!print *, 'node%val%array%type = ', node%val%array%type

			! TODO: i64 types

			!elem = lbound
			do i = lbound%i32, ubound%i32 - 1
				array%i32(i - lbound%i32 + 1) = i
				!elem%i32 = i
				!call array%push(elem)
			end do

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len

			allocate(res%array)

			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == expl_array .and. allocated(node%size)) then
			! Explicit rank-2+ arrays

			array = new_array(node%val%array%type, size(node%elems))

			do i = 1, size(node%elems)
				elem = syntax_eval(node%elems(i), vars, fns, quietl)
				!print *, 'elem['//str(i)//'] = ', elem%str()
				call array%push(elem)
			end do

			array%rank = size( node%size )
			allocate(array%size( array%rank ))
			!array%size = array%len
			do i = 1, array%rank
				len = syntax_eval(node%size(i), vars, fns, quietl)
				array%size(i) = len%i32
			end do

			!print *, 'copying array'
			allocate(res%array)
			res%type  = array_type
			res%array = array
			!print *, 'done'

		else if (node%val%array%kind == expl_array) then
			!print *, 'expl_array'

			! Explicit rank-1 arrays

			! TODO: allow empty arrays?  Sub type of empty array?  Empty arrays
			! can currently be created like [0: -1];
			array = new_array(node%val%array%type, size(node%elems))

			do i = 1, size(node%elems)
				elem = syntax_eval(node%elems(i), vars, fns, quietl)
				!print *, 'elem['//str(i)//'] = ', elem%str()
				call array%push(elem)
			end do

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len

			!print *, 'copying array'
			allocate(res%array)
			res%type  = array_type
			res%array = array
			!print *, 'done'

		else
			!TODO
			write(*,*) 'Error: unexpected array kind'
			call internal_error()
		end if

	case (for_statement)

		! TODO: this assumes for statement array range is i32 of the form [imin:
		! imax].  Generalize for other forms, maybe make an array%at() method
		! for shared use here for for_statement eval and above for array_expr
		! eval.  If possible, don't expand implicit arrays for for loops

		lbound = syntax_eval(node%array%lbound, vars, fns, quietl)
		ubound = syntax_eval(node%array%ubound, vars, fns, quietl)

		! push scope to make the loop iterator local
		call vars%push_scope()

		! Get the type of the loop iterator for future i64 compatibility but
		! throw an error since it's not supported yet
		itr%type = lbound%type
		!itr%type = vars%vals(node%id_index)%type  ! unset

		if (itr%type /= i32_type) then
			write(*,*) err_eval_i32_itr(node%identifier%text)
			call internal_error()
		end if

		do i = lbound%i32, ubound%i32 - 1
			itr%i32 = i

			! During evaluation, insert variables by array id_index instead of
			! dict lookup.  This is much faster and can be done during
			! evaluation now that we know all of the variable identifiers.
			! Parsing still needs to rely on dictionary lookups because it does
			! not know the entire list of variable identifiers ahead of time
			vars%vals(node%id_index) = itr

			res = syntax_eval(node%body, vars, fns, quietl)
		end do

		call vars%pop_scope()

	case (while_statement)

		condition = syntax_eval(node%condition, vars, fns, quietl)
		do while (condition%bool)
			res = syntax_eval(node%body, vars, fns, quietl)
			condition = syntax_eval(node%condition, vars, fns, quietl)
		end do

	case (if_statement)

		condition = syntax_eval(node%condition, vars, fns, quietl)
		!print *, 'condition = ', condition%str()

		if (condition%bool) then
			res = syntax_eval(node%if_clause, vars, fns, quietl)

		else if (allocated(node%else_clause)) then
			res = syntax_eval(node%else_clause, vars, fns, quietl)

		end if

	case (translation_unit)

		! TODO: do we want to globally push/pop scope for whole
		! translation_unit?  Will this have impacts on interpretting multiple
		! files, or allowing the user to override intrinsic fns?
		!call vars%push_scope()

		! The final statement of a unit returns the actual result.  Non-final
		! members only change the (vars) state or define fns
		do i = 1, size(node%members)

			! Only eval statements, not fns declarations.  TODO: cycle structs
			! too.
			!
			! TODO: is this where we should copy fn dict to array?
			if (node%members(i)%kind == fn_declaration) cycle

			res = syntax_eval(node%members(i), vars, fns, quietl)

			!print *, 'kind = ', node%members(i)%kind
			!print *, i, ' res = ', res%str()
			!print *, ''

			! HolyC feature: implicitly print name expression members.  I may
			! remove this after I implement an intrinsic print() fn.  May also
			! need to suppress this for void fn calls later
			if (node%members(i)%kind == name_expr .and. .not. quietl) then
				write(*,*) res%to_str()
			end if

		end do

		!call vars%pop_scope()

	case (block_statement)

		call vars%push_scope()

		! The final statement of a block returns the actual result.  Non-final
		! members only change the (vars) state.
		do i = 1, size(node%members)
			res = syntax_eval(node%members(i), vars, fns, quietl)

			!print *, 'kind = ', node%members(i)%kind
			!print *, i, ' res = ', res%str()
			!print *, ''

			! HolyC feature: implicitly print name expression members.  I may
			! remove this after I implement an intrinsic print() fn.  May also
			! need to suppress this for void fn calls later
			if (node%members(i)%kind == name_expr .and. .not. quietl) then
				write(*,*) res%to_str()
			end if

		end do

		call vars%pop_scope()

	case (assignment_expr)

		if (.not. allocated(node%subscripts)) then

			! Assign return value
			res = syntax_eval(node%right, vars, fns, quietl)

			! TODO: test int/float casting.  It should be an error during
			! parsing

			call compound_assign(vars%vals(node%id_index), res, node%op)

			! For compound assignment, ensure that the LHS is returned
			res = vars%vals(node%id_index)

			! The difference between let and assign is inserting into the
			! current scope (let) vs possibly searching parent scopes (assign).
			! During evaluation we don't need any extra logic for scoping.  The
			! parser has already assigned a separate id_index for each
			! identifier at each scope level

		else
			!print *, 'LHS array subscript assignment'
			!print *, 'LHS type = ', vars%vals(node%id_index)%type

			! Assign return value from RHS
			res = syntax_eval(node%right, vars, fns, quietl)

			!print *, 'RHS = ', res%to_str()

			i = subscript_eval(node, vars, fns, quietl)

			if (vars%vals(node%id_index)%type == str_type) then
				! TODO: ban compound character substring assignment
				vars%vals(node%id_index)%str%s(i+1: i+1) = res%str%s
			else

				!print *, 'LHS array type = ', &
				!	vars%vals(node%id_index)%array%type
				!print *, 'LHS array = ', vars%vals(node%id_index)%array%i32

				array_val = get_array_value_t(vars%vals(node%id_index)%array, i)
				call compound_assign(array_val, res, node%op)
				call set_array_value_t( &
					vars%vals(node%id_index)%array, i, array_val)
				res = array_val

			end if
		end if

	case (let_expr)

		! Assign return value
		res = syntax_eval(node%right, vars, fns, quietl)

		!print *, 'assigning identifier "', node%identifier%text, '"'
		vars%vals(node%id_index) = res

	case (fn_call_expr)

		!print *, 'eval fn_call_expr'
		!print *, 'fn identifier = ', node%identifier%text
		!print *, 'fn id_index   = ', node%id_index

		!res%type = fns%fns(node%id_index)%type
		res%type = node%val%type

		!print *, 'res type = ', res%type

		! Intrinsic fns
		select case (node%identifier%text)
		case ("exp")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			res%f32 = exp(arg1%f32)

		case ("min")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%i32 = arg%i32

			! Note that min/max/println etc. are variadic, so we loop to
			! size(node%args) instead of size(node%params)

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%i32 = min(res%i32, arg%i32)
			end do

		case ("max")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%i32 = arg%i32
			do i = 2, size(node%args)
				!print *, 'arg ', i
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%i32 = max(res%i32, arg%i32)
			end do

		case ("println")

			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(output_unit, '(a)', advance = 'no') arg%to_str()
			end do
			write(output_unit, *)

			!! TODO: what, if anything, should println return?
			!res%i32 = 0

		case ("str")

			res%str%s = ''
			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%str%s = res%str%s // arg%to_str()  ! TODO: use char_vector_t
			end do

		case ("open")

			arg = syntax_eval(node%args(1), vars, fns, quietl)

			open(newunit = res%file_%unit_, file = arg%str%s)
			!print *, 'opened unit ', res%file_%unit_
			res%file_%name_ = arg%str%s

		case ("readln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, "reading from unit", arg1%file_%unit_
			res%str%s = read_line(arg1%file_%unit_, io)

			! TODO:  set eof flag or crash for other non-zero io 
			if (io == iostat_end) then
				arg1%file_%eof = .true.
			end if
			print *, 'eof = ', arg1%file_%eof

		case ("writeln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, 'writing to unit ', arg1%file_%unit_
			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(arg1%file_%unit_, '(a)', advance = 'no') arg%to_str()
			end do
			write(arg1%file_%unit_, *)

		case ("close")
			arg = syntax_eval(node%args(1), vars, fns, quietl)
			!print *, 'closing unit ', arg%file_%unit_
			close(arg%file_%unit_)

		case ("size")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			arg2 = syntax_eval(node%args(2), vars, fns, quietl)

			if (arg2%i32 < 0 .or. arg2%i32 >= arg1%array%rank) then
				! TODO: this should be a runtime error (like bounds-checking),
				! not an internal_error.  I just don't have infrastructure for
				! runtime error handling yet
				write(*,*) 'Error: rank mismatch in size() call'
				call internal_error()
			end if

			res%i32 = arg1%array%size( arg2%i32 + 1 )

		case default
			! User-defined function

			if (.not. allocated(node%params)) then
				write(*,*) 'Error: unexpected fn'
				call internal_error()
			end if

			!print *, 'fn name = ', node%identifier%text
			!print *, 'fn idx  = ', node%id_index
			!print *, 'node type = ', node%val%type
			!print *, 'size params = ', size(node%params)
			!print *, 'param ids = ', node%params

			! TODO: Shared param scope is ok at first, but eventually target
			! recursive fns with scoped stack frames

			! Pass by value (for now, at least).  Arguments are evaluated and
			! their values are copied to the fn parameters

			do i = 1, size(node%params)
				!print *, 'copying param ', i
				vars%vals( node%params(i) ) = &
					syntax_eval(node%args(i), vars, fns, quietl)
			end do

			res = syntax_eval(node%body, vars, fns, quietl)

		end select

	case (name_expr)
		!print *, 'searching identifier ', node%identifier%text

		if (allocated(node%subscripts) .and. &
			vars%vals(node%id_index)%type == str_type) then
			!print *, 'string subscript RHS name expr'

			!print *, 'str type'
			res%type = vars%vals(node%id_index)%type

			select case (node%subscripts(1)%sub_kind)
			case (scalar_sub)
				i = subscript_eval(node, vars, fns, quietl)
				res%str%s = vars%vals(node%id_index)%str%s(i+1: i+1)

			case (range_sub)

				il = subscript_eval(node, vars, fns, quietl)
				!print *, 'il = ', il

				! This feels inconsistent and not easy to extend to higher ranks
				right = syntax_eval(node%usubscripts(1), vars, fns, quietl)
				iu = right%i32
				!print *, 'iu = ', iu

				! Not inclusive of upper bound
				res%str%s = vars%vals(node%id_index)%str%s(il+1: iu)

			case default
				write(*,*) 'Error: unexpected subscript kind'
				call internal_error()
			end select

		else if (allocated(node%subscripts)) then
			!print *, 'string subscript RHS name expr'

			if (vars%vals(node%id_index)%type /= array_type) then
				write(*,*) 'Error: bad type, expected array'
				call internal_error()
			end if

			i = subscript_eval(node, vars, fns, quietl)
			res = get_array_value_t(vars%vals(node%id_index)%array, i)

		else
			!print *, 'scalar name expr'
			res = vars%vals(node%id_index)

			! Deep copy if whole array instead of aliasing pointers
			if (res%type == array_type) then
				!print *, 'array  name_expr'

				allocate(res%array)
				res%type = array_type
				res%array = vars%vals(node%id_index)%array

			!else
			!	print *, 'scalar name_expr'
			end if

		end if

	case (unary_expr)

		right = syntax_eval(node%right, vars, fns, quietl)
		!print *, 'right = ', right

		res%type = right%type

		! TODO: add fallback type checking here

		select case (node%op%kind)
		case (plus_token)
			res      =  right

		case (minus_token)
			select case (right%type)
				case (i32_type)
					res%i32 = -right%i32
				case (f32_type)
					res%f32 = -right%f32
			end select

		case (not_keyword)
			res%bool = .not. right%bool

		case default
			write(*,*) err_eval_unary_op(node%op%text)
			call internal_error()
		end select

	case (binary_expr)

		left  = syntax_eval(node%left , vars, fns, quietl)
		right = syntax_eval(node%right, vars, fns, quietl)

		!print *, 'left  = ', left
		!print *, 'right = ', right

		res%type = get_binary_op_kind(left%type, node%op%kind, right%type)
		if (res%type == unknown_type) then
			write(*,*) err_eval_binary_types(node%op%text)
			call internal_error()
		end if

		select case (node%op%kind)
		case (plus_token)
			call add(left, right, res, node%op%text)

		case (minus_token)
			call subtract(left, right, res, node%op%text)

		case (star_token)
			call mul(left, right, res, node%op%text)

		case (sstar_token)
			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = left%i32 ** right%i32
			case        (magic * f32_type + f32_type)
				res%f32 = left%f32 ** right%f32
			case        (magic * f32_type + i32_type)
				res%f32 = left%f32 ** right%i32
			case        (magic * i32_type + f32_type)
				res%f32 = left%i32 ** right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (slash_token)
			call div(left, right, res, node%op%text)

		case (percent_token)

			! The Fortran mod() fn is consistent with the C operator `%`, while
			! modulo() works differently for negative args (it's actually a
			! remainder function, not a true modulo)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = mod(left%i32, right%i32)
			case        (magic * f32_type + f32_type)
				res%f32 = mod(left%f32, right%f32)
			case        (magic * f32_type + i32_type)
				res%f32 = mod(left%f32, real(right%i32))
			case        (magic * i32_type + f32_type)
				res%f32 = mod(real(left%i32), right%f32)
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (and_keyword)
			res%bool = left%bool .and. right%bool

		case (or_keyword)
			res%bool = left%bool .or.  right%bool

		case (eequals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 == right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 == right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 == right%i32
				! TODO: is this even possible or should I ban comparing ints and
				! floats?  Similarly for other comparisons
				!
				! GNU says Warning: Equality comparison for REAL(4) at (1)
				! [-Wcompare-reals]
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 == right%f32
			case        (magic * bool_type + bool_type)
				res%bool = left%bool .eqv. right%bool
			case        (magic * str_type + str_type)
				res%bool = left%str%s == right%str%s
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (bang_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 /= right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 /= right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 /= right%i32
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 /= right%f32
			case        (magic * bool_type + bool_type)
				res%bool = left%bool .neqv. right%bool
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (less_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 < right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 < right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 < right%i32
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 < right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (less_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 <= right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 <= right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 <= right%i32
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 <= right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (greater_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 > right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 > right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 > right%i32
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 > right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (greater_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%bool = left%i32 >= right%i32
			case        (magic * f32_type + f32_type)
				res%bool = left%f32 >= right%f32
			case        (magic * f32_type + i32_type)
				res%bool = left%f32 >= right%i32
			case        (magic * i32_type + f32_type)
				res%bool = left%i32 >= right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case default
			write(*,*) err_eval_binary_op(node%op%text)
			call internal_error()

		end select

	case default
		write(*,*) err_eval_node(kind_name(node%kind))
		call internal_error()

	end select

end function syntax_eval

!===============================================================================

subroutine compound_assign(lhs, rhs, op)
	! TODO: rename?  This also handles regular assignment

	! lhs += rhs;
	!   or
	! lhs *= rhs;
	!   etc.

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in) :: rhs

	type(syntax_token_t), intent(in) :: op

	select case (op%kind)
	case (equals_token)
		lhs = rhs  ! simply overwrite

	case (plus_equals_token)
		call add(lhs, rhs, lhs, op%text)

	case (minus_equals_token)
		call subtract(lhs, rhs, lhs, op%text)

	case (star_equals_token)
		call mul(lhs, rhs, lhs, op%text)

	case (slash_equals_token)
		call div(lhs, rhs, lhs, op%text)

	case default
		write(*,*) 'Error: unexpected assignment operator "', op%text, '"'
		call internal_error()
	end select

end subroutine compound_assign

!===============================================================================

function get_array_value_t(array, i) result(val)

	type(array_t), intent(in) :: array

	integer, intent(in) :: i

	type(value_t) :: val

	!print *, 'starting get_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)

	val%type = array%type
	select case (array%type)
		case (bool_type)
			val%bool = array%bool(i + 1)
		case (i32_type)
			val%i32 = array%i32(i + 1)
		case (f32_type)
			val%f32 = array%f32(i + 1)
		case (str_type)
			val%str = array%str(i + 1)
	end select

end function get_array_value_t

!===============================================================================

subroutine set_array_value_t(array, i, val)

	type(array_t), intent(inout) :: array

	integer, intent(in) :: i

	type(value_t), intent(in) :: val

	!print *, 'starting set_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)
	!print *, 'val%type   = ', kind_name(val%type)

	! array%type is already set
	select case (array%type)
		case (bool_type)
			array%bool(i + 1) = val%bool
		case (i32_type)
			array%i32(i + 1) = val%i32
		case (f32_type)
			array%f32(i + 1) = val%f32
		case (str_type)
			array%str(i + 1) = val%str
	end select

end subroutine set_array_value_t

!===============================================================================

subroutine add_value_t(left, right, res, op_text)

	type(value_t), intent(in)  :: left, right

	! For binary_expr, the type is set before calling this routine, so res is
	! inout
	type(value_t), intent(inout) :: res

	character(len = *), intent(in) :: op_text

	!********

	! Case selector must be a scalar expression, so use this nasty hack.
	! This will break if magic is smaller than the largest type enum
	! parameter
	select case (magic**2 * res%type + magic * left%type + right%type)

	!****
	case        (magic**2 * i32_type + magic * i32_type + i32_type)
		res%i32 = left%i32 + right%i32

	! Usually, adding f32 to i32 casts to an i32 result.  But for compound
	! assignment we may want to make it an i32, e.g. i += 3.1;

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%i32 = int(left%f32 + right%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%i32 = int(left%i32 + right%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%f32 = left%f32 + right%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%f32 = left%f32 + right%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%f32 = left%i32 + right%f32

	case        (magic**2 * str_type + magic * str_type + str_type)
		res%str%s = left%str%s // right%str%s

	case default
		! FIXME: other numeric types (i64, f64, etc.)
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
		res%i32 = left%i32 * right%i32

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%i32 = int(left%f32 * right%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%i32 = int(left%i32 * right%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%f32 = left%f32 * right%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%f32 = left%f32 * right%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%f32 = left%i32 * right%f32

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
		res%i32 = left%i32 / right%i32

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%i32 = int(left%f32 / right%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%i32 = int(left%i32 / right%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%f32 = left%f32 / right%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%f32 = left%f32 / right%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%f32 = left%i32 / right%f32

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
		res%i32 = left%i32 - right%i32

	case        (magic**2 * i32_type + magic * f32_type + i32_type)
		res%i32 = int(left%f32 - right%i32)

	case        (magic**2 * i32_type + magic * i32_type + f32_type)
		res%i32 = int(left%i32 - right%f32)

	!****
	case        (magic**2 * f32_type + magic * f32_type + f32_type)
		res%f32 = left%f32 - right%f32

	case        (magic**2 * f32_type + magic * f32_type + i32_type)
		res%f32 = left%f32 - right%i32

	case        (magic**2 * f32_type + magic * i32_type + f32_type)
		res%f32 = left%i32 - right%f32

	case default
		! FIXME: other numeric types (i64, f64, etc.)
		write(*,*) err_eval_binary_types(op_text)
		call internal_error()
	end select

end subroutine subtract_value_t

!===============================================================================

subroutine internal_error()

	write(*,*) fg_bold_bright_red//'Fatal error'//color_reset
	call exit(exit_failure)

end subroutine internal_error

!===============================================================================

subroutine log_diagnostics(node, ou)

	class(syntax_node_t), intent(in) :: node
	integer, optional   , intent(in) :: ou

	!********

	integer :: i, oul

	oul = output_unit
	if (present(ou)) oul = ou

	do i = 1, node%diagnostics%len
		write(oul, '(a)') node%diagnostics%v(i)%s
		write(oul,*)
	end do

end subroutine log_diagnostics

!===============================================================================

recursive function value_to_str(val) result(ans)

	class(value_t) :: val

	character(len = :), allocatable :: ans

	!********

	character(len = 16) :: buf16
	character(len = 32) :: buffer

	integer :: i, j, prod

	!type(string_vector_t) :: str_vec
	type(char_vector_t) :: str_vec

	select case (val%type)

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

		case (str_type)
			! TODO: wrap str in quotes for clarity, both scalars and str array
			! elements.  Update tests.
			ans = val%str%s

		case (file_type)
			ans = "{file_unit: "//str(val%file_%unit_)//", filename: """// &
				val%file_%name_//"""}"

		case (array_type)

			if (val%array%kind == impl_array) then
				ans = '['//val%array%lbound%to_str()//': ' &
				         //val%array%ubound%to_str()//']'
				return
			end if

			!print *, 'array type = ', val%array%type

			!! You would think that this would help
			!if (val%array%type == i32_type) then
			!	str_vec = new_char_vector( 12 * val%array%len )
			!else if (val%array%type == f32_type) then
			!	str_vec = new_char_vector( 16 * val%array%len )
			!end if
			str_vec = new_char_vector()

			call str_vec%push('[')
			if (val%array%rank > 1) call str_vec%push(line_feed)

			if (val%array%type == i32_type) then

				!! Recursive IO stalls execution
				!print *, 'size = ', val%array%size

				do i = 1, val%array%len

					call str_vec%push(str(val%array%i32(i)))
					if (i >= val%array%len) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == f32_type) then

				do i = 1, val%array%len

					!! Nice alignment, but breaks tests
					!write(buf16, '(es16.6)') val%array%f32(i)
					!call str_vec%push(buf16)

					! Trimmed string (not aligned)
					call str_vec%push(str(val%array%f32(i)))

					if (i >= val%array%len) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == bool_type) then

				do i = 1, val%array%len

					call str_vec%push(str(val%array%bool(i)))

					if (i >= val%array%len) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i, prod) == 0) call str_vec%push(line_feed)
						prod = prod * val%array%size(j)
					end do

				end do

			else if (val%array%type == str_type) then

				do i = 1, val%array%len

					call str_vec%push(val%array%str(i)%s)

					if (i >= val%array%len) cycle

					call str_vec%push(', ')

					! Products could be saved ahead of time outside of loop
					prod = val%array%size(1)
					do j = 2, val%array%rank
						if (mod(i, prod) == 0) call str_vec%push(line_feed)
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

			ans = str_vec%v( 1: str_vec%len )

		case default
			ans = err_prefix//"<invalid_value>"//color_reset

	end select

end function value_to_str

!===============================================================================

end module core_m

!===============================================================================

