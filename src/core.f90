
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
	integer, parameter :: debug = 2

	integer, parameter ::   &
		syntran_major =  0, &
		syntran_minor =  0, &
		syntran_patch =  9

	! TODO:
	!
	! Add:
	!  - error_type (or unknown_type) like Immo to prevent cascading errors
	!  - compound assignment: +=, -=, *=, etc.
	!    * Does any language have "**="? This will
	!  - ++, --
	!  - tetration operator ***? ints only? just for fun
	!  - functions
	!  - arrays
	!    * start with the way implicit arrays are handled as for loop iterators
	!  - % (mod/modulo (which? Fortran handles negatives differently in one))
	!  - strings.  are characters useful or can we just use strings of length 1?
	!  - structs
	!  - make syntax highlighting plugins for vim and TextMate (VSCode et al.)
	!  - enums
	!  - xor
	!  - bitwise operators
	!
	!****************************************

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
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

	type value_t
		integer :: type

		logical           :: bool
		integer(kind = 4) :: i32
		real   (kind = 4) :: f32

		! TODO: may need to manually define value_t copy assignment since array
		! is pointer/allocatable
		type(array_t), pointer :: array => null()

		contains
			procedure :: str => value_str

			!procedure, pass(dst) :: copy => value_copy
			!generic, public :: assignment(=) => copy

	end type value_t

	!********

	type array_t

		! The array type is i32_type, f32_type, etc. while the kind is
		! impl_array (bound-based) or expl_array (CSV list)
		integer :: type, kind

		!type(syntax_node_t), allocatable :: lbound, ubound
		!type(syntax_node_t), pointer :: lbound, ubound
		type(value_t), allocatable :: lbound, ubound

		!integer(kind = 4) :: lbound_i32, step_i32, ubound_i32
		!real   (kind = 4) :: lbound_f32, step_f32, ubound_f32

		! Note that these are arrays of primitive Fortran types, instead of
		! arrays of generic value_t.  This performs better since we can put
		! a type select/case outside of loops for processing arrays, as opposed
		! to inside of a loop for type selection of every element
		logical(kind = 1), allocatable :: bool(:)
		integer(kind = 4), allocatable ::  i32(:)
		real   (kind = 4), allocatable ::  f32(:)

		integer :: len, cap
		contains
			procedure :: push => push_array

			!procedure, pass(dst) :: copy => array_copy
			!generic, public :: assignment(=) => copy

	end type array_t

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

		type(syntax_node_t), allocatable :: left, right, statements(:), &
			condition, if_clause, else_clause, body

		! Array expression syntax nodes
		type(syntax_node_t), allocatable :: lbound, ubound, elems(:)

		! TODO: add an array subscript member.  Type syntax_node_t?  It could be
		! an int literal or an int expression (or later a CSV list of
		! subscripts)

		! TODO: make this an array for rank-2+ arrays
		type(syntax_node_t), allocatable :: subscript

		type(syntax_token_t) :: op, identifier
		integer :: id_index

		type(value_t) :: val
		!type(value_t), allocatable :: val

		type(string_vector_t) :: diagnostics

		! Only used to handle comment/whitespace lines for now
		logical :: is_empty = .false.

		! Is the parser expecting more input, e.g. from a continued line in the
		! interactive interpreter to match a semicolon, brace, etc.?
		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		contains
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

	! Fixed-size limit to the scope level for now, while I work on scoping
	integer, parameter :: scope_max = 64

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

		contains
			procedure :: match, tokens_str, current_kind, &
				current => current_token, next => next_parser_token, &
				peek => parser_peek_token, peek_kind, &
				parse_expr, parse_primary_expr, parse_expr_statement, &
				parse_statement, parse_block_statement, parse_if_statement, &
				current_pos, peek_pos, parse_for_statement, &
				parse_while_statement, parse_array_expr

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

	! TODO
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

function new_array(type) result(vector)

	integer, intent(in) :: type
	type(array_t) :: vector

	vector%len = 0
	vector%cap = 2  ! I think a small default makes sense here

	if      (type == i32_type) then
		allocate(vector%i32( vector%cap ))
	else if (type == f32_type) then
		allocate(vector%f32( vector%cap ))
	else if (type == bool_type) then
		allocate(vector%bool( vector%cap ))
	else
		print *, 'Error: array type not implemented'
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

	integer(kind = 4), allocatable :: tmp_i32(:)
	real   (kind = 4), allocatable :: tmp_f32(:)
	logical(kind = 1), allocatable :: tmp_bool(:)

	integer :: tmp_cap

	vector%len = vector%len + 1

	if (vector%len > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len

		if (vector%type == i32_type) then

			allocate(tmp_i32( tmp_cap ))
			tmp_i32(1: vector%cap) = vector%i32
			call move_alloc(tmp_i32, vector%i32)

		else
			print *, 'TODO: push_array type not implemented'
		end if

		vector%cap = tmp_cap

	end if

	if (vector%type == i32_type) then
		vector%i32( vector%len ) = val%i32
	else
		! TODO
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
			"comma_token         "  & ! 56
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
			"[                    ", & ! 40
			"]                    ", & ! 41
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
			",                    "  & ! 56
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

		do i = 1, size(node%statements)
			block = block // node%statements(i)%str(indentl//'    ')
		end do

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
		val   = indentl//'    val   = '//node%val%str()//line_feed
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

subroutine value_copy(dst, src)

	! TODO: is this required or does default assignment work?

	class(value_t), intent(inout) :: dst
	class(value_t), intent(in)    :: src

	!********

	print *, 'starting value_copy'

	dst%type = src%type
	print *, 'bool'
	dst%bool = src%bool
	print *, 'i32'
	dst%i32  = src%i32
	print *, 'f32'
	dst%f32  = src%f32

	print *, 'checking array'
	!if (allocated(src%array)) then
	if (associated(src%array)) then
		print *, 'copying array'
		if (.not. associated(dst%array)) allocate(dst%array)
		dst%array = src%array
	end if
	print *, 'done'

end subroutine value_copy

!===============================================================================

!recursive subroutine array_copy(dst, src)
subroutine array_copy(dst, src)

	! TODO: is this required or does default assignment work?

	! Deep copy

	class(array_t), intent(inout) :: dst
	class(array_t), intent(in)    :: src

	!********

	!integer :: i, n

	if (debug > 3) print *, 'starting array_copy()'

	dst%kind = src%kind
	dst%type = src%type
	dst%len  = src%len
	dst%cap  = src%cap

	if (allocated(src%lbound)) then
		if (.not. allocated(dst%lbound)) allocate(dst%lbound)
		dst%lbound = src%lbound
	end if

	if (allocated(src%ubound)) then
		if (.not. allocated(dst%ubound)) allocate(dst%ubound)
		dst%ubound = src%ubound
	end if

	if (allocated(src%bool)) then
		!if (.not. allocated(dst%bool)) allocate(dst%bool)
		dst%bool = src%bool
	end if

	if (allocated(src%i32)) then
		!if (.not. allocated(dst%i32)) allocate(dst%i32)
		dst%i32 = src%i32
	end if

	if (allocated(src%f32)) then
		!if (.not. allocated(dst%f32)) allocate(dst%f32)
		dst%f32 = src%f32
	end if

end subroutine array_copy

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

	integer :: i, n

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

	if (allocated(src%condition)) then
		if (.not. allocated(dst%condition)) allocate(dst%condition)
		dst%condition = src%condition
	end if

	if (allocated(src%body)) then
		if (.not. allocated(dst%body)) allocate(dst%body)
		dst%body = src%body
	end if

	if (allocated(src%lbound)) then
		if (.not. allocated(dst%lbound)) allocate(dst%lbound)
		dst%lbound = src%lbound
	end if

	if (allocated(src%ubound)) then
		if (.not. allocated(dst%ubound)) allocate(dst%ubound)
		dst%ubound = src%ubound
	end if

	if (allocated(src%elems)) then
		call syntax_nodes_copy(dst%elems, src%elems)
	end if

	if (allocated(src%subscript)) then
		if (.not. allocated(dst%subscript)) allocate(dst%subscript)
		dst%subscript = src%subscript
	end if

	if (allocated(src%if_clause)) then
		if (.not. allocated(dst%if_clause)) allocate(dst%if_clause)
		dst%if_clause = src%if_clause
	end if

	if (allocated(src%else_clause)) then
		if (.not. allocated(dst%else_clause)) allocate(dst%else_clause)
		dst%else_clause = src%else_clause
	end if

	if (allocated(src%statements)) then
		!print *, 'copying statements'

		call syntax_nodes_copy(dst%statements, src%statements)

		!n = size(src%statements)
		!print *, 'n = ', n

		!if (allocated(dst%statements)) deallocate(dst%statements)
		!allocate(dst%statements(n))

		!! This explicit loop is apparently required
		!do i = 1, n
		!	dst%statements(i) = src%statements(i)
		!end do
		!!print *, 'done loop'

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

	type(text_span_t) :: span
	type(value_t) :: val

	if (lexer%pos > len(lexer%text)) then
		token = new_token(eof_token, lexer%pos, null_char)
		return
	end if

	start = lexer%pos

	if (is_digit(lexer%current())) then

		float = .false.

		!do while (is_digit(lexer%current()))
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
			token = new_token(plus_token  , lexer%pos, lexer%current())

			! FIXME: prefix/postfix inc/dec operators (++, --)

		case ("-")
			token = new_token(minus_token , lexer%pos, lexer%current())

		case ("*")
			if (lexer%lookahead() == "*") then
				lexer%pos = lexer%pos + 1
				token = new_token(sstar_token  , lexer%pos, "**")
			else
				token = new_token(star_token  , lexer%pos, lexer%current())
			end if

		case ("/")
			if (lexer%lookahead() == "/") then

				call lexer%read_single_line_comment()

				! FIXME: make "trivia" token types instead of overloading
				! whitespace_token for comments.  This is what Immo did
				text = lexer%text(start: lexer%pos-1)
				token = new_token(whitespace_token, start, text)

			else
				token = new_token(slash_token , lexer%pos, lexer%current())
			end if

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
				token = new_token(equals_token , lexer%pos, lexer%current())
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

function new_literal_value(type, bool, i32, f32) result(val)

	integer, intent(in) :: type

	integer(kind = 4), intent(in), optional :: i32
	real   (kind = 4), intent(in), optional :: f32
	logical          , intent(in), optional :: bool

	type(value_t) :: val

	val%type = type
	if (present(bool)) val%bool = bool
	if (present(f32 )) val%f32  = f32
	if (present(i32 )) val%i32  = i32

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

function syntax_parse(str, vars, src_file, allow_continue) result(tree)

	character(len = *) :: str

	type(syntax_node_t) :: tree

	character(len = *), optional, intent(in)  :: src_file

	logical, intent(in), optional :: allow_continue

	!********

	character(len = :), allocatable :: src_filel
	integer :: i
	logical :: allow_continuel

	type(parser_t) :: parser

	type(syntax_token_t) :: token

	type(vars_t) :: vars, vars0

	if (debug > 0) print *, 'syntax_parse'
	if (debug > 1) print *, 'str = ', str

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
			!	print *, vars%vals(i)%str()
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

	!*******************************
	! Parse the tokens
	tree = parser%parse_statement()
	!*******************************

	tree%expecting       = parser%expecting
	tree%first_expecting = parser%first_expecting

	tree%first_expected = parser%first_expected

	if (debug > 1) print *, 'tree = ', tree%str()

	if (tree%expecting .and. allow_continuel) then

		! If expecting more input, don't push diagnostics yet.  Also undo any
		! variable declarations, since they will be re-declared when we continue
		! parsing the current stdin line from its start again.

		! TODO: also reset vars if not expecting but diagnostics exist?  This
		! should fix the following interpreter edge case:
		!
		!     let a = ;
		!     a = 5;
		!   //  ^ bad types

		if (allocated(vars0%dicts(1)%root)) then
			call move_alloc(vars0%dicts(1)%root, vars%dicts(1)%root)
			call move_alloc(vars0%vals         , vars%vals)
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

	! When parsing is finished, we are done with the variable dictionary
	! parser%vars%dicts.  Allocate an array for efficient evaluation without
	! dictionary lookups.  Indices in the array are already saved in each node's
	! id_index member

	!print *, 'parser%num_vars = ', parser%num_vars
	allocate(vars%vals( parser%num_vars ))

	if (allocated(vars0%vals)) then
		vars%vals( 1: size(vars0%vals) ) = vars0%vals
	end if

	if (debug > 0) print *, 'done syntax_parse'

end function syntax_parse

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

	integer :: bound_beg, bound_end

	type(syntax_node_t)  :: body, lbound, ubound
	type(syntax_token_t) :: for_token, in_token, lbracket, rbracket, colon, &
		identifier
	type(text_span_t) :: span

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
	!  * or use `=` instead of `in`?
	!  * or use `..` instead of `:` like rust?
	!  * For steps, rust has `for x in (1..10).step_by(2) {}`, which I hate

	for_token  = parser%match(for_keyword)

	call parser%vars%push_scope()

	identifier = parser%match(identifier_token)

	in_token   = parser%match(in_keyword)

	! TODO: just call the general parse_array_expr() method.  Do int
	! type-checking somehow, since parse_array_expr will parse floats too

	lbracket = parser%match(lbracket_token)

	! Check that bounds are i32_type types.  For an explicit comma-separated
	! array, we could iterate over bool values, but implicit range/bound-based
	! arrays require ints

	! Allow floats as loop bounds?

	bound_beg = parser%peek_pos(0)
	lbound    = parser%parse_expr()
	bound_end = parser%peek_pos(0) - 1
	if (lbound%val%type /= i32_type) then
		span = new_span(bound_beg, bound_end - bound_beg + 1)
		call parser%diagnostics%push(err_non_int_bound( &
			parser%context, span, parser%context%text(bound_beg: bound_end)))
	end if

	colon    = parser%match(colon_token)

	bound_beg = parser%peek_pos(0)
	ubound    = parser%parse_expr()
	bound_end = parser%peek_pos(0) - 1
	if (ubound%val%type /= i32_type) then
		span = new_span(bound_beg, bound_end - bound_beg + 1)
		call parser%diagnostics%push(err_non_int_bound( &
			parser%context, span, parser%context%text(bound_beg: bound_end)))
	end if

	rbracket = parser%match(rbracket_token)

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
	call parser%vars%insert(identifier%text, lbound%val, &
		statement%id_index)!, io, overwrite = .false.)

	body = parser%parse_statement()

	allocate(statement%lbound, statement%ubound, statement%body)

	statement%kind = for_statement

	statement%identifier = identifier
	statement%lbound     = lbound
	statement%ubound     = ubound
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

	type(syntax_node_vector_t) :: statements
	type(syntax_token_t) :: left, right, dummy

	integer :: i, pos0

	statements = new_syntax_node_vector()
	i = 0

	left  = parser%match(lbrace_token)

	call parser%vars%push_scope()

	do while ( &
		parser%current_kind() /= eof_token .and. &
		parser%current_kind() /= rbrace_token)

		pos0 = parser%pos
		i = i + 1
		!print *, '    statement ', i

		call statements%push(parser%parse_statement())

		! Avoid infinite loops on malformed blocks like this:
		!   {
		!     4) + 5;
		!   }
		if (parser%pos == pos0) dummy = parser%next()

	end do

	call parser%vars%pop_scope()

	right = parser%match(rbrace_token)

	block%kind = block_statement

	! Convert to standard array.  TODO: make a subroutine for this explicit loop
	! copy, since apparently its required for memory correctness

	call syntax_nodes_copy(block%statements, statements%v( 1: statements%len ))

	!if (allocated(block%statements)) deallocate(block%statements)
	!allocate(block%statements( statements%len ))
	!do i = 1, statements%len
	!	block%statements(i) = statements%v(i)
	!end do

end function parse_block_statement

!===============================================================================

subroutine syntax_nodes_copy(dst, src)

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

	integer :: io, type

	logical :: subscript_present

	type(syntax_node_t) :: right, subscript
	type(syntax_token_t) :: let, identifier, op, lbracket, rbracket
	type(text_span_t) :: span

	print *, 'starting parse_expr_statement()'

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
		expr%id_index = parser%num_vars

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

	! This is a bit of a hack for subscripts.  It assumes that `[` will
	! eventually be followed by `=`.  How many tokens later it is, is
	! indeterminate, because the subscript itself could be a single int token or
	! a multi-token expression
	!
	! Currently this prevents printing a single element at the end of a program:
	!
	!     let a = [0: 2];
	!     a[0];

	if  (parser%peek_kind(0) == identifier_token .and. &
	    (parser%peek_kind(1) == equals_token .or. &
	     parser%peek_kind(1) == lbracket_token)) then

		!print *, 'assign expr'

		identifier = parser%next()

		! Parse array subscript index if present

		! Subscript can appear in assignment expr but not let expr, because let
		! must initialize the whole array

		subscript_present = .false.
		if (parser%current_kind() == lbracket_token) then
			subscript_present = .true.
			print *, 'parsing subscript'

			lbracket  = parser%match(lbracket_token)
			subscript = parser%parse_expr()

			print *, 'subscript = ', subscript%str()

			! TODO: check subscript type is int

			print *, 'parsing rbracket'
			rbracket  = parser%match(rbracket_token)
			print *, 'done'

		end if

		!op         = parser%next()
		op         = parser%match(equals_token)

		right      = parser%parse_expr_statement()

		!expr = new_assignment_expr(identifier, op, right)

		expr%kind = assignment_expr

		allocate(expr%right)

		expr%identifier = identifier

		expr%op    = op
		expr%right = right

		print *, 'expr ident text = ', expr%identifier%text

		! Get the identifier's type and index from the dict and check that it
		! has been declared
		expr%val = parser%vars%search(identifier%text, expr%id_index, io)

		if (io /= exit_success) then
			span = new_span(identifier%pos, len(identifier%text))
			call parser%diagnostics%push( &
				err_undeclare_var(parser%context, &
				span, identifier%text))
		end if

		print *, 'type = ', kind_name(expr%val%type)

		print *, 'associated(expr%val%array) = ', associated(expr%val%array)

		type = expr%val%type

		if (subscript_present) then
			allocate(expr%subscript)
			expr%subscript = subscript
		end if

		!! TODO: only do this if the subscript is present.  Whole array
		!! assignment should be different
		!if (type == array_type) then
		!	print *, 'reassigning'
		!	print *, 'expr%val%array%type = ', expr%val%array%type
		!	type = expr%val%array%type
		!	print *, 'done'
		!end if

		! TODO: move this check inside of is_binary_op_allowed?  Need to pass
		! parser to it to push diagnostics
		if (type /= array_type .and. .not. is_binary_op_allowed( &
			type, op%kind, expr%right%val%type)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context, &
				span, op%text, &
				kind_name(expr%val%type), &
				kind_name(expr%right%val%type)))

		end if

		return

	end if

	expr = parser%parse_expr()
	!semi       = parser%match(semicolon_token)

end function parse_expr_statement

!===============================================================================

recursive function parse_expr(parser, parent_prec) result(expr)

	! In episode 3, Immo renamed this fn to "ParseBinaryExpression()", but
	! I consider that confusing because the result could be either unary or
	! binary

	class(parser_t) :: parser

	integer, optional, intent(in) :: parent_prec

	type(syntax_node_t) :: expr

	!********

	integer :: parent_precl, prec

	type(syntax_node_t) :: right
	type(syntax_token_t) :: op
	type(text_span_t) :: span

	if (debug > 1) print *, 'parse_expr'

	parent_precl = 0
	if (present(parent_prec)) parent_precl = parent_prec

	prec = get_unary_op_prec(parser%current_kind())
	if (prec /= 0 .and. prec >= parent_precl) then

		op    = parser%next()
		right = parser%parse_expr(prec)
		expr  = new_unary_expr(op, right)

		if (.not. is_unary_op_allowed(op%kind, right%val%type)) then

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

		if (.not. is_binary_op_allowed( &
			expr%left%val%type, op%kind, expr%right%val%type)) then

			span = new_span(op%pos, len(op%text))
			call parser%diagnostics%push( &
				err_binary_types(parser%context, &
				span, op%text, &
				kind_name(expr%left %val%type), &
				kind_name(expr%right%val%type)))

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

	select case (op)

		case (plus_token, minus_token, sstar_token, star_token, slash_token, &
				less_token   , less_equals_token, &
				greater_token, greater_equals_token)

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

		case (star_token, slash_token)
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

function parse_array_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: bound_beg, bound_end, elem_beg, elem_end

	type(syntax_node_t)  :: lbound, ubound, elem
	type(syntax_node_vector_t) :: elems
	type(syntax_token_t) :: lbracket, rbracket, colon, comma
	type(text_span_t) :: span

	print *, 'starting parse_array_expr()'

	! TODO: optionally match this lbound:ubound style, or lbound step and
	! ubound, or an explicit list of comma-separated array elements.  Rank-1 for
	! now, but eventually target higher rank arrays

	lbracket = parser%match(lbracket_token)

	bound_beg = parser%peek_pos(0)
	lbound    = parser%parse_expr()
	bound_end = parser%peek_pos(0) - 1

	! TODO: should type checking be done by caller, or should we pass an
	! expected type arg for the RHS of this check?

	! TODO: check if lbound%val is allocated, e.g. for assigning one array to
	! a cat of another:
	!
	!     let a = [0: 3];
	!     let b = [a, 5, 6];
	!              ^ segfault

	if (lbound%val%type /= i32_type) then
		span = new_span(bound_beg, bound_end - bound_beg + 1)
		call parser%diagnostics%push(err_non_int_bound( &
			parser%context, span, parser%context%text(bound_beg: bound_end)))
	end if

	!if (parser%peek_kind(1) == colon_token) then
	if (parser%current_kind() == colon_token) then

		! Implicit array form [lbound: ubound]

		colon    = parser%match(colon_token)

		bound_beg = parser%peek_pos(0)
		ubound    = parser%parse_expr()
		bound_end = parser%peek_pos(0) - 1

		! TODO: other types
		if (ubound%val%type /= i32_type) then
			span = new_span(bound_beg, bound_end - bound_beg + 1)
			call parser%diagnostics%push(err_non_int_bound( &
				parser%context, span, parser%context%text(bound_beg: bound_end)))
		end if

		rbracket = parser%match(rbracket_token)

		print *, 'lbound = ', lbound%str()
		print *, 'ubound = ', ubound%str()

		allocate(expr%val%array)

		!allocate(expr%val%array%lbound)
		!allocate(expr%val%array%ubound)
		allocate(expr%lbound)
		allocate(expr%ubound)

		expr%kind                 = array_expr
		expr%val%type             = array_type
		expr%val%array%type       = i32_type  ! TODO
		expr%val%array%kind       = impl_array

		!expr%val%array%lbound_i32 = lbound%val%i32
		!expr%val%array%ubound_i32 = ubound%val%i32

		!expr%val%array%lbound = lbound
		!expr%val%array%ubound = ubound
		expr%lbound = lbound
		expr%ubound = ubound

		return

	end if

	print *, 'elem ', lbound%val%str()

	elems = new_syntax_node_vector()
	call elems%push(lbound)

	! Explicit array form [elem0, elem1, elem2, ... ].  elem0 has already been
	! parsed as lbound above
	do while (parser%current_kind() /= rbracket_token)
		comma    = parser%match(comma_token)

		elem_beg = parser%peek_pos(0)
		elem     = parser%parse_expr()
		elem_end = parser%peek_pos(0) - 1

		print *, 'elem ', elem%val%str()

		! TODO: compare to first type, not hard-coded i32
		if (elem%val%type /= i32_type) then
			span = new_span(elem_beg, elem_end - elem_beg + 1)
			call parser%diagnostics%push(err_het_array( &
				parser%context, span, parser%context%text(elem_beg: elem_end)))
		end if

		call elems%push(elem)

	end do

	rbracket = parser%match(rbracket_token)

	allocate(expr%val%array)
	expr%kind           = array_expr
	expr%val%type       = array_type
	expr%val%array%type = i32_type ! TODO
	expr%val%array%kind = expl_array

	call syntax_nodes_copy(expr%elems, elems%v( 1: elems%len ))

	! TODO: allow arbitrarily combinations catting expl and impl arrays, e.g.
	!
	!        [0: 3   ,   5, 6,   10: 13    ]
	!     // [0, 1, 2,   5, 6,   10, 11, 12]

end function parse_array_expr

!===============================================================================

function parse_primary_expr(parser) result(expr)

	class(parser_t) :: parser

	type(syntax_node_t) :: expr

	!********

	integer :: io, id_index
	logical :: bool
	type(syntax_token_t) :: num, left, right, keyword, identifier
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

		case (true_keyword, false_keyword)

			keyword = parser%next()
			bool = keyword%kind == true_keyword
			expr = new_bool(bool)

			!print *, 'expr%val%bool = ', expr%val%bool

		case (identifier_token)

			identifier = parser%next()
			!print *, 'identifier = ', identifier%text

			! TODO: parse array subscript if present.  Can this be consolidated
			! with subscript parsing in parse_expr_statement?

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

		case (f32_token)

			num = parser%match(f32_token)
			expr = new_f32(num%val%f32)

		case default

			num = parser%match(i32_token)
			expr = new_i32(num%val%i32)

			if (debug > 1) print *, 'num = ', expr%val%str()

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
	expr%val%type = right%val%type

end function new_declaration_expr

!===============================================================================

!function new_assignment_expr(identifier, op, right) result(expr)
!
!	type(syntax_token_t), intent(in) :: identifier, op
!	type(syntax_node_t) , intent(in) :: right
!
!	type(syntax_node_t) :: expr
!
!	!********
!
!	expr%kind = assignment_expr
!
!	allocate(expr%right)
!
!	expr%identifier = identifier
!
!	expr%op    = op
!	expr%right = right
!
!	! The identifier has already been declared, so do not overwrite its type
!	! here
!
!end function new_assignment_expr

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

		! TODO: messaging.  stop w/o breaking bad syntax tests
		print *, 'Error: unreachable'
		!call internal_error()

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
	expr%val%type = right%val%type

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

recursive function syntax_eval(node, vars) result(res)

	type(syntax_node_t) :: node

	! I don't want to make this arg optional, because then it would require
	! copying a potentially large struct to a local var without fancy use of
	! move_alloc()
	type(vars_t) :: vars

	type(value_t) :: res

	!********

	integer :: i
	integer, parameter :: magic = 128
	type(array_t) :: array
	type(value_t) :: left, right, condition, lbound, ubound, itr, elem, &
		subscript

	!print *, 'starting syntax_eval()'

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

		print *, 'evaluating array_expr'

		! TODO: switch on impl_array vs expl_array cases

		if (node%val%array%kind == impl_array) then
			print *, 'impl_array'

			! TODO: expand impl_array to expl_array here on evaluation.  Consider
			! something like this:
			!
			!     let a = [0: 5];
			!     a[2] = -3;
			!
			! Even though a is initialized to an implicit array, the second
			! statement requires it to be explicit, so we might as well expand at
			! initialization

			!lbound = syntax_eval(node%val%array%lbound, vars)
			!ubound = syntax_eval(node%val%array%ubound, vars)
			lbound = syntax_eval(node%lbound, vars)
			ubound = syntax_eval(node%ubound, vars)

			print *, 'bounds in [', lbound%str(), ': ', ubound%str(), ']'

			! TODO: res should be the whole expanded array?
			res = node%val

			res%array%lbound = lbound
			res%array%ubound = ubound

		else if (node%val%array%kind == expl_array) then
			print *, 'expl_array'

			! TODO: allow empty arrays?
			array = new_array(node%val%array%type)

			do i = 1, size(node%elems)
				print *, 'i = ', i
				elem = syntax_eval(node%elems(i), vars)

				print *, 'elem = ', elem%str()
				print *, ''

				call array%push(elem)

			end do

			print *, 'copying array'
			allocate(res%array)
			res%type  = array_type
			res%array = array
			print *, 'done'

		else
			!TODO
			print *, 'Error: unexpected array kind'
			call internal_error()
		end if

	case (for_statement)

		! TODO: migrate to general array_t type.  If possible, don't expand
		! implicit arrays for for loops

		lbound = syntax_eval(node%lbound, vars)
		ubound = syntax_eval(node%ubound, vars)

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
			!call vars%insert(node%identifier%text, itr, node%id_index)

			res = syntax_eval(node%body, vars)
		end do

		call vars%pop_scope()

	case (while_statement)

		condition = syntax_eval(node%condition, vars)
		do while (condition%bool)
			res = syntax_eval(node%body, vars)
			condition = syntax_eval(node%condition, vars)
		end do

	case (if_statement)

		condition = syntax_eval(node%condition, vars)
		!print *, 'condition = ', condition%str()

		if (condition%bool) then
			res = syntax_eval(node%if_clause, vars)

		else if (allocated(node%else_clause)) then
			res = syntax_eval(node%else_clause, vars)

		end if

	case (block_statement)

		call vars%push_scope()

		! The final statement of a block returns the actual result.  Non-final
		! statements only change the (vars) state.
		do i = 1, size(node%statements)
			res = syntax_eval(node%statements(i), vars)
			!print *, i, ' res = ', res%str()
		end do

		call vars%pop_scope()

	case (assignment_expr)

		!if (node%left%val%type /= array_type) then
		!if (.not. allocated(node%left%subscript)) then
		if (.not. allocated(node%subscript)) then

			! Assign return value
			res = syntax_eval(node%right, vars)

			! TODO: test int/float casting.  It should be an error during
			! parsing

			! Assign res to LHS identifier variable as well.  This inserts the
			! value, while the insert call in the parser inserts the type
			vars%vals(node%id_index) = res
			!call vars%search_insert(node%identifier%text, res, node%id_index)

			! The difference between let and assign is inserting into the
			! current scope (let) vs possibly searching parent scopes (assign).
			! During evaluation we don't need any extra logic for scoping.  The
			! parser has already assigned a separate id_index for each
			! identifier at each scope level

		else
			print *, 'array subscript assignment'

			! Assign return value from RHS
			res = syntax_eval(node%right, vars)

			print *, 'RHS = ', res%str()

			subscript = syntax_eval(node%subscript, vars)

			print *, 'subscript = ', subscript%str()

			!print *, 'LHS array type = ', node%val%array%type
			print *, 'LHS array type = ', vars%vals(node%id_index)%array%type
			print *, 'LHS array = ', vars%vals(node%id_index)%array%i32

			! TODO: check types, at least in parser if not here as a fallback
			! too
			vars%vals(node%id_index)%array%i32(subscript%i32) = res%i32

		end if

	case (let_expr)

		! Assign return value
		res = syntax_eval(node%right, vars)

		!print *, 'assigning identifier "', node%identifier%text, '"'
		!call vars%insert(node%identifier%text, res, node%id_index)
		vars%vals(node%id_index) = res

	case (name_expr)
		!print *, 'searching identifier ', node%identifier%text
		!res = vars%search(node%identifier%text, node%id_index)
		res = vars%vals(node%id_index)

	case (unary_expr)

		right = syntax_eval(node%right, vars)
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

		left  = syntax_eval(node%left , vars)
		right = syntax_eval(node%right, vars)

		!print *, 'left  = ', left
		!print *, 'right = ', right

		res%type = get_binary_op_kind(left%type, node%op%kind, right%type)

		select case (node%op%kind)
		case (plus_token)

			! Case selector must be a scalar expression, so use this nasty hack.
			! This will break if magic is smaller than the largest type enum
			! parameter
			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = left%i32 + right%i32
			case        (magic * f32_type + f32_type)
				res%f32 = left%f32 + right%f32
			case        (magic * f32_type + i32_type)
				res%f32 = left%f32 + right%i32
			case        (magic * i32_type + f32_type)
				res%f32 = left%i32 + right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (minus_token)
			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = left%i32 - right%i32
			case        (magic * f32_type + f32_type)
				res%f32 = left%f32 - right%f32
			case        (magic * f32_type + i32_type)
				res%f32 = left%f32 - right%i32
			case        (magic * i32_type + f32_type)
				res%f32 = left%i32 - right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (star_token)
			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = left%i32 * right%i32
			case        (magic * f32_type + f32_type)
				res%f32 = left%f32 * right%f32
			case        (magic * f32_type + i32_type)
				res%f32 = left%f32 * right%i32
			case        (magic * i32_type + f32_type)
				res%f32 = left%i32 * right%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

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
			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%i32 = left%i32 / right%i32
			case        (magic * f32_type + f32_type)
				res%f32 = left%f32 / right%f32
			case        (magic * f32_type + i32_type)
				res%f32 = left%f32 / right%i32
			case        (magic * i32_type + f32_type)
				res%f32 = left%i32 / right%f32
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

function value_str(val) result(str)

	class(value_t) :: val

	character(len = :), allocatable :: str

	!********

	character(len = 16) :: buf16
	character(len = 32) :: buffer

	integer :: i

	select case (val%type)

		case (f32_type)
			write(buf16, '(es16.6)') val%f32
			!str = trim(buf16)
			str = buf16  ! no trim for alignment

		case (i32_type)
			write(buffer, '(i0)') val%i32
			str = trim(buffer)

		case (bool_type)
			! It might be helpful to have util fns for primitive str conversion
			if (val%bool) then
				str = "true"
			else
				str = "false"
			end if

		case (array_type)

			if (val%array%kind == impl_array) then
				str = '['//val%array%lbound%str()//': ' &
				         //val%array%ubound%str()//']'
				return
			end if

			! This will probably break for large arrays as we can't arbitrarily
			! cat huge strings
			str = '['
			do i = 1, val%array%len
				! TODO: other types
				str = str//int_str(val%array%i32(i))
				if (i < val%array%len) str = str//', '
			end do
			str = str//']'

		case default
			str = err_prefix//"<invalid_value>"//color_reset

	end select

end function

!===============================================================================

end module core_m

!===============================================================================

