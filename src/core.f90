
!===============================================================================

module syntran__core_m

	! This module contains private members.  For the public API, see syntran.f90

	use iso_fortran_env

	use syntran__consts_m
	use syntran__errors_m
	use syntran__parser_m
	use syntran__types_m
	use syntran__utils_m
	use syntran__value_m

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = 'syntran'

	integer, parameter ::   &
		syntran_major =  0, &
		syntran_minor =  0, &
		syntran_patch =  36

	! TODO:
	!  - document recent features:
	!    * array slicing
	!    * casting?
	!    * array comparison (wip)
	!  - add a workflow that tests gfortran version 8 (and/or older?).  older
	!    versions don't allow a user-defined type that references another type
	!    which is defined below it
	!    * added a matrix for gfortran 9 through 12
	!    * 8 isn't installed.  maybe i can install it in workflow?
	!  - pass by reference?  big boost to perf for array fns.  should be
	!    possible by swapping around some id_index values in vars%vals array.
	!    harder part is ensuring that only lvalues are passed by ref (not
	!    rvalues), e.g. `my_fn(x)` is allowed but `my_fn(x+1)` is not if arg is
	!    passed by ref
	!  - #(pragma)once  directive. #let var=val directive?
	!    * for #once include guards, insert filename path as key into a ternary
	!      tree w/ bool value true.  then when something is included, check if
	!      it's in the ternary dict first.
	!    * any use cases for #let? i probbaly don't want to get into general
	!      expression parsing like `#let x = 1 + 2;` during preprocessing
	!  - jumping control flow:
	!    * (sys) exit.  at least this basic form of error handling is needed
	!    * fn return statement. i like the cleanliness of rust but i still need
	!      a way to return early.  rust does have an explicit "return"
	!      statement, i guess it's just not the rust style to use it when it's
	!      not needed
	!    * cycle (continue), break ((loop) exit)
	!  - str comparison operations:
	!    * >, <, etc. via lexicographical ordering? careful w/ strs that have
	!      matching leading chars but diff lens
	!    * ==, !=:  done
	!  - negative for loop steps.  at least throw parser error
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
	!  - split doc into multiple README's, add TOC, cross-linking, etc.  Only
	!    include quick-start and links in top-level README?
	!    * github automatically includes a Table of Contents in a menu, so maybe
	!      it's better as-is
	!  - file reading
	!    * readln(), eof() done
	!    * also add a file_stat() fn which checks IO of previous file operation.
	!      this way I don't need to add structs, multiple return vals, or out
	!      args yet
	!  - arrays
	!    * add slice subscripts for LHS:
	!      > RHS slicing done
	!      > a[:]     -> a[0], a[1], a[2], ...
	!      > a[1:4]   -> a[1], a[2], a[3]  // already parsed bc of str feature, just can't eval yet
	!      > a[1:2:6] -> a[1], a[3], a[5]
	!      > higher rank:  a[:,1], a[2,:], a[2:4, 1], ...
	!      > in general, count the number of scalar subscripts.  that is the
	!        difference between the input rank and the output rank of the
	!        subscripting operation.  e.g. if every subscript is a slice, then
	!        the rank is unchanged, 1 scalar subscript decreases rank by 1, etc.
	!    * refactor the way implicit arrays are handled as for loop iterators
	!    * operations: vector comparisons, any(), all(), vector addition, dot product, scalar-vector mult, ...
	!  - compound assignment: logical &=, |=, etc.
	!    * +=, -=, *=, /=, %=, **= done
	!  - ++, --
	!  - tetration operator ***? ints only? just for fun
	!  - functions
	!    * check return value is correct type
	!    * intrinsic
	!      > abs, norm, dot
	!      > log
	!      > trig: sin, cos, tan, asin, ...
	!      > norm, sum, product
	!      > reshape
	!      > system: multiple out args? iostat and stdout
	!    * recursive user-defined fns
	!    * done:
	!      > exp  (non-variadic, non-polymorphic)
	!      > min, max (variadic but non-polymorphic)
	!      > size (non-variadic but polymorphic)
	!      > readln, writeln, println, open, close, str casting
	!      > len (of str)
	!      > non-recursive user-defined fns
	!  - structs
	!  - make syntax highlighting plugins for vim and TextMate (VSCode et al.)
	!    * using rust syntrax highlighting in neovim is not bad (except for "\" string)
	!  - enums
	!  - file reading
	!    * file_stat() fn: checks IO of previous file operation. this way I
	!      don't need to add structs, multiple return vals, or out args yet
	!    * readln(), eof() done
	!  - xor, xnor
	!    * xor (bool1, bool2) is just (bool1 != bool2)
	!    * xnor(bool1, bool2) is just (bool1 == bool2)
	!    * is there any value to having plain language versions of these
	!      operators, like `and` or `not` in syntran?
	!  - bitwise operators
	!  - split doc into multiple README's, add TOC, cross-linking, etc.  Only
	!    include quick-start and links in top-level README?
	!    * github automatically includes a Table of Contents in a menu, so maybe
	!      it's better as-is
	!
	!****************************************

	! A note on naming: Immo calls '==' equals_equals_token, but I think that
	! invites tab-completion mistakes so I went with eequals_token.  Same for
	! sstar_token (and upcoming pplus_token, mminus_token, etc.)

!===============================================================================

contains

!===============================================================================

function declare_intrinsic_fns() result(fns)

	type(fns_t) :: fns

	!********

	integer :: id_index, num_fns

	type(fn_t) :: exp_fn, min_fn, max_fn, println_fn, size_fn, open_fn, &
		close_fn, readln_fn, writeln_fn, str_fn, eof_fn, parse_i32_fn, len_fn, &
		i64_fn, parse_i64_fn, i32_fn, exit_fn

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

	! TODO: overload (?) i64 intrinsics or make that the default

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

	! TODO: update docs to use println() instead of old holyc implicit prints

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

	! TODO: return i64?

	len_fn%type = i32_type
	allocate(len_fn%params(1))
	len_fn%params(1)%type = str_type
	len_fn%params(1)%name = "str"

	id_index = id_index + 1
	call fns%insert("len", len_fn, id_index)

	!********

	! TODO: add fns for parsing str to other types (f32, bool, etc.)

	! Should this accept any type?  f32 can be converted implicitly so there
	! shouldn't be a need for other types

	parse_i32_fn%type = i32_type
	allocate(parse_i32_fn%params(1))
	parse_i32_fn%params(1)%type = str_type
	parse_i32_fn%params(1)%name = "str"

	id_index = id_index + 1
	call fns%insert("parse_i32", parse_i32_fn, id_index)

	!********

	parse_i64_fn%type = i64_type
	allocate(parse_i64_fn%params(1))
	parse_i64_fn%params(1)%type = str_type
	parse_i64_fn%params(1)%name = "str"

	id_index = id_index + 1
	call fns%insert("parse_i64", parse_i64_fn, id_index)

	!********

	i32_fn%type = i32_type
	allocate(i32_fn%params(1))

	i32_fn%params(1)%type = any_type

	i32_fn%params(1)%name = "a"

	id_index = id_index + 1
	call fns%insert("i32", i32_fn, id_index)

	!********

	i64_fn%type = i64_type
	allocate(i64_fn%params(1))

	! TODO: add a way to have a limited polymorphic parameter.  Numeric type to
	! i64 casting should be allowed, but bool to i64 is not allowed and str to
	! i64 should have a different fn name
	!
	! Currently anything funky will be caught during evaluation but it should
	! really be caught earlier by the type checker during parsing

	i64_fn%params(1)%type = any_type

	i64_fn%params(1)%name = "a"

	id_index = id_index + 1
	call fns%insert("i64", i64_fn, id_index)

	!********

	open_fn%type = file_type
	allocate(open_fn%params(1))
	open_fn%params(1)%type = str_type
	open_fn%params(1)%name = "filename"

	id_index = id_index + 1
	call fns%insert("open", open_fn, id_index)

	!********

	readln_fn%type = str_type
	allocate(readln_fn%params(1))
	readln_fn%params(1)%type = file_type
	readln_fn%params(1)%name = "file_handle"

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

	eof_fn%type = bool_type
	allocate(eof_fn%params(1))
	eof_fn%params(1)%type = file_type
	eof_fn%params(1)%name = "file_handle"

	id_index = id_index + 1
	call fns%insert("eof", eof_fn, id_index)

	!********

	close_fn%type = void_type
	allocate(close_fn%params(1))
	close_fn%params(1)%type = file_type
	close_fn%params(1)%name = "file_handle"

	id_index = id_index + 1
	call fns%insert("close", close_fn, id_index)

	!********

	exit_fn%type = void_type
	allocate(exit_fn%params(1))
	exit_fn%params(1)%type = i32_type
	exit_fn%params(1)%name = "exit_status"

	id_index = id_index + 1
	call fns%insert("exit", exit_fn, id_index)

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
			min_fn      , &
			max_fn      , &
			println_fn  , &
			str_fn      , &
			len_fn      , &
			parse_i32_fn, &
			parse_i64_fn, &
			i32_fn      , &
			i64_fn      , &
			open_fn     , &
			readln_fn   , &
			writeln_fn  , &
			eof_fn      , &
			close_fn    , &
			exit_fn     , &
			size_fn       &
		]

end function declare_intrinsic_fns

!===============================================================================

function new_array(type, cap) result(vector)

	! TODO: use or combine allocate_array()

	integer, intent(in) :: type
	integer, intent(in), optional :: cap
	type(array_t) :: vector

	vector%len_ = 0

	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2  ! I think a small default makes sense here
	end if

	if      (type == i32_type) then
		allocate(vector%i32 ( vector%cap ))
	else if (type == i64_type) then
		allocate(vector%i64 ( vector%cap ))
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

subroutine allocate_array(array, cap)

	type(array_t), intent(inout) :: array
	integer(kind = 8), intent(in) :: cap

	array%cap = cap

	select case (array%type)
	case (i32_type)
		allocate(array%i32( cap ))
	case (i64_type)
		allocate(array%i64( cap ))
	case (f32_type)
		allocate(array%f32( cap ))
	case (bool_type)
		allocate(array%bool( cap ))
	case (str_type)
		allocate(array%str( cap ))
	case default
		write(*,*) err_int_prefix//'cannot allocate array of type `' &
			//kind_name(array%type)//'`'//color_reset
		call internal_error()
	end select

end subroutine allocate_array

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

	integer :: unit_

	logical :: allow_continuel

	type(text_context_vector_t) :: contexts

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

	! TODO: unit_ is just synonymous with contexts%len_ in many places, so it
	! can be removed (but not in all places)
	contexts = new_context_vector()
	unit_ = 0

	parser = new_parser(str, src_filel, contexts, unit_)
	!print *, 'units = ', parser%tokens(:)%unit_

	! Do nothing for blank lines (or comments)
	if (parser%current_kind() == eof_token) then
		tree%is_empty = .true.
		tree%diagnostics = parser%diagnostics
		return
	end if

	! TODO: something funny is happening with vars backup/restore.  Run these
	! commands in interactive interpreter:
	!
	!     let x = 1;
	!     #include("src/tests/test-src/include/str.syntran");
	!     scan("1234", "3");
	!     // works as expected
	!
	! Then in a new syntran session, run these:
	!
	!     #include("src/tests/test-src/include/str.syntran");
	!     scan("1234", "3");
	!     // crashes

	! Point parser member to vars dict.  This could be done in the
	! constructor new_parser(), but it seems reasonable to do it here since it
	! has to be moved back later.  The dict vars0 comes from the
	! interactive interpreter's history, it has nothing to do with scoping

	!print *, 'moving vars'

	!print *, ''
	!print *, 'size(vars%vals) = ', size(vars%vals)

	!print *, 'allocated 1 = ', allocated(vars%dicts(1)%root)
	!print *, 'allocated 2 = ', allocated(vars%dicts(2)%root)
	!print *, 'allocated 3 = ', allocated(vars%dicts(3)%root)

	if (allocated(vars%dicts(1)%root)) then
	!if (any(allocated(vars%dicts(:)%root))) then

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

	!else if (parser%num_vars > 0) then
	!else if (size(vars%vals) > 0) then
	!else if (size(vars%vals) > 0 .and. allow_continuel) then
	else if (allocated(vars%vals) .and. allow_continuel) then
		if (size(vars%vals) > 0) then

		! This could probably be refactored but it breaks my brain to think this
		! through and test enough permutations in interactive interpreter

		!print *, 'backing up vars%vals to vars0%vals'
		vars0%vals = vars%vals
		parser%num_vars = size(vars%vals)
		!print *, '1'
		call move_alloc(vars%vals         , parser%vars%vals)
		!print *, '2'

		end if
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
		end if

		if (allocated(vars0%vals)) then
			!print *, 'restoring vars%vals from vars0%vals'
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

	!print *, 'size(parser%vars%vals) = ', size(parser%vars%vals)

	! Move back.  It's possible that vars were empty before this call but not
	! anymore
	if (allocated(parser%vars%dicts(1)%root)) then
	!if (parser%num_vars > 0) then
		call move_alloc(parser%vars%dicts(1)%root, vars%dicts(1)%root)
	end if

	! TODO: if num_fns instead?
	if (allocated(parser%fns%dicts(1)%root)) then
		call move_alloc(parser%fns%dicts(1)%root, fns%dicts(1)%root)
	end if

	! When parsing is finished, we are done with the variable dictionary
	! parser%vars%dicts.  Allocate a flat array for efficient evaluation without
	! dictionary lookups.  Indices in the array are already saved in each node's
	! id_index member

	!print *, 'parser%num_vars = ', parser%num_vars
	if (allocated(vars%vals)) deallocate(vars%vals)
	allocate(vars%vals( parser%num_vars ))

	if (allocated(vars0%vals)) then
		!print *, 'restoring vars%vals'
		vars%vals( 1: size(vars0%vals) ) = vars0%vals
		!vars%vals = vars0%vals
		!vars = vars0
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

function subscript_eval(node, vars, fns, quietl) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_

	type(syntax_node_t) :: node
	type(vars_t) :: vars
	type(fns_t) :: fns
	logical, intent(in) :: quietl

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	! str scalar with single char subscript
	if (vars%vals(node%id_index)%type == str_type) then
		subscript = syntax_eval(node%lsubscripts(1), vars, fns, quietl)
		index_ = subscript%sca%i32
		return
	end if

	!if (vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

	prod  = 1
	index_ = 0
	do i = 1, vars%vals(node%id_index)%array%rank
		!print *, 'i = ', i

		subscript = syntax_eval(node%lsubscripts(i), vars, fns, quietl)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%sca%i32
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

	character(len = :), allocatable :: color

	integer :: i, j, io, rank, rank_res, idim_, idim_res
	integer(kind = 8) :: il, iu, i8, index_, prod
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	logical :: quietl

	real(kind = 4) :: f, fstep

	! TODO: rename lbound/ubound to avoid intrinsic clash
	type(array_t) :: array
	type(value_t) :: left, right, condition, lbound, ubound, itr, elem, &
		step, len_, arg, arg1, arg2, array_val, lsubval, usubval

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

			! If any bound or step is i64, cast the others up to match
			if (any(i64_type == [lbound%type, step%type, ubound%type])) then

				!! this happens during parsing
				!array%type = i64_type

				call promote_i32_i64(lbound)
				call promote_i32_i64(step)
				call promote_i32_i64(ubound)
			end if

			!print *, 'lbound = ', lbound%sca%i64
			!print *, 'step32 = ', step  %sca%i32
			!print *, 'step64 = ', step  %sca%i64
			!print *, 'ubound = ', ubound%sca%i64

			if (array%type == i32_type) then

				array%cap = (ubound%sca%i32 - lbound%sca%i32) / step%sca%i32 + 1
				allocate(array%i32( array%cap ))

				j = 1
				i = lbound%sca%i32
				if (lbound%sca%i32 < ubound%sca%i32 .neqv. 0 < step%sca%i32) i = ubound%sca%i32

				! Step may be negative
				do while ((i  < ubound%sca%i32 .eqv. lbound%sca%i32 < ubound%sca%i32) &
				     .and. i /= ubound%sca%i32)
					array%i32(j) = i
					i = i + step%sca%i32
					j = j + 1
				end do
				array%len_ = j - 1

			else if (array%type == i64_type) then

				array%cap = int((ubound%sca%i64 - lbound%sca%i64) / step%sca%i64 + 1)
				allocate(array%i64( array%cap ))

				j = 1
				i8 = lbound%sca%i64
				if (lbound%sca%i64 < ubound%sca%i64 .neqv. 0 < step%sca%i64) then
					i8 = ubound%sca%i64
				end if

				! Step may be negative
				do while ((i8  < ubound%sca%i64 .eqv. lbound%sca%i64 < ubound%sca%i64) &
				     .and. i8 /= ubound%sca%i64)
					array%i64(j) = i8
					i8 = i8 + step%sca%i64
					j = j + 1
				end do
				array%len_ = j - 1

			else if (array%type == f32_type) then

				!print *, 'lbound, ubound = ', lbound%sca%f32, ubound%sca%f32
				!print *, 'step = ', step%sca%f32

				array%cap = ceiling((ubound%sca%f32 - lbound%sca%f32) / step%sca%f32) + 1
				allocate(array%f32( array%cap ))

				j = 1
				f = lbound%sca%f32
				if (lbound%sca%f32 < ubound%sca%f32 .neqv. 0 < step%sca%f32) f = ubound%sca%f32

				do while ((f  < ubound%sca%f32 .eqv. lbound%sca%f32 < ubound%sca%f32) &
				     .and. f /= ubound%sca%f32)
					array%f32(j) = f
					f = f + step%sca%f32
					j = j + 1
				end do
				array%len_ = j - 1

			else
				write(*,*) 'Error: step array type eval not implemented'
				call internal_error()
			end if

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len_

			allocate(res%array)
			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array &
			.and. allocated(node%ubound) .and. allocated(node%len_)) then

			! len-based impl arrays

			!print *, 'len array'
			lbound = syntax_eval(node%lbound, vars, fns, quietl)
			ubound = syntax_eval(node%ubound, vars, fns, quietl)
			len_   = syntax_eval(node%len_   , vars, fns, quietl)

			!array = new_array(node%val%array%type)
			!elem = lbound

			array%type = node%val%array%type
			array%len_  = len_%sca%i32
			array%cap  = array%len_

			if (array%type == f32_type) then

				allocate(array%f32( array%cap ))
				fstep = (ubound%sca%f32 - lbound%sca%f32) / (len_%sca%i32 - 1)

				do i = 0, len_%sca%i32 - 1
					array%f32(i+1) = lbound%sca%f32 + i * fstep
					!elem%sca%f32 = lbound%sca%f32 + i * fstep
					!call array%push(elem)
				end do

			else
				write(*,*) 'Error: bound/len array type eval not implemented'
				call internal_error()
			end if

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len_

			allocate(res%array)

			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array .and. allocated(node%len_)) then

			! Initialize rank-2+ arrays
			if (allocated(node%size)) then

				array%rank = size( node%size )
				allocate(array%size( array%rank ))

				do i = 1, array%rank
					len_ = syntax_eval(node%size(i), vars, fns, quietl)
					array%size(i) = len_%sca%i32
					!print *, 'size['//str(i)//'] = ', array%size(i)
				end do

			end if

			! Constant-value impl arrays (i.e. every element has the same value
			! at initialization, but they are of course mutable)

			!print *, 'len array'
			lbound = syntax_eval(node%lbound, vars, fns, quietl)

			! Allocate in one shot without growing

			array%type = node%val%array%type
			array%len_  = product(array%size)
			!print *, 'array%len_ = ', array%len_

			call allocate_array(array, array%len_)
			select case (array%type)
			case (i32_type)
				array%i32 = lbound%sca%i32
			case (i64_type)
				array%i64 = lbound%sca%i64
			case (f32_type)
				array%f32 = lbound%sca%f32
			case (bool_type)
				array%bool = lbound%sca%bool
			case (str_type)
				array%str = lbound%sca%str
			case default
				write(*,*) err_eval_len_array(kind_name(array%type))
				call internal_error()
			end select

			allocate(res%array)
			res%type  = array_type
			res%array = array

		else if (node%val%array%kind == impl_array) then
			!print *, 'impl_array'

			! Unit step impl integer array

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

			if (any(i64_type == [lbound%type, ubound%type])) then
				call promote_i32_i64(lbound)
				call promote_i32_i64(ubound)
			end if

			if (.not. any(array%type == [i32_type, i64_type])) then
				write(*,*) 'Error: unit step array type eval not implemented'
				call internal_error()
			end if

			if (array%type == i32_type) then
				array%len_ = ubound%sca%i32 - lbound%sca%i32
			else !if (array%type == i64_type) then
				array%len_ = ubound%sca%i64 - lbound%sca%i64
			end if

			call allocate_array(array, array%len_)

			!print *, 'bounds in [', lbound%str(), ': ', ubound%str(), ']'
			!print *, 'node%val%array%type = ', node%val%array%type

			if (array%type == i32_type) then
				do i = lbound%sca%i32, ubound%sca%i32 - 1
					array%i32(i - lbound%sca%i32 + 1) = i
				end do
			else !if (array%type == i64_type) then
				do i8 = lbound%sca%i64, ubound%sca%i64 - 1
					array%i64(i8 - lbound%sca%i64 + 1) = i8
				end do
			end if

			array%rank = 1
			allocate(array%size( array%rank ))
			array%size = array%len_

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
			!array%size = array%len_
			do i = 1, array%rank
				len_ = syntax_eval(node%size(i), vars, fns, quietl)
				array%size(i) = len_%sca%i32
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
			array%size = array%len_

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

		! TODO: i64 for loop iterators
		if (itr%type /= i32_type) then
			write(*,*) err_eval_i32_itr(node%identifier%text)
			call internal_error()
		end if

		do i = lbound%sca%i32, ubound%sca%i32 - 1
			itr%sca%i32 = i

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
		do while (condition%sca%bool)
			res = syntax_eval(node%body, vars, fns, quietl)
			condition = syntax_eval(node%condition, vars, fns, quietl)
		end do

	case (if_statement)

		condition = syntax_eval(node%condition, vars, fns, quietl)
		!print *, 'condition = ', condition%str()

		if (condition%sca%bool) then
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

		if (.not. allocated(node%lsubscripts)) then

			if (allocated(vars%vals)) then
			if (allocated(vars%vals(node%id_index)%array)) then
				!! TODO: necessary now that array is allocatable instead of pointable?
				!print *, "deallocating lhs array"
				deallocate(vars%vals(node%id_index)%array)
			end if
			end if

			! Assign return value
			!print *, 'eval and set res'
			res = syntax_eval(node%right, vars, fns, quietl)

			! TODO: test int/float casting.  It should be an error during
			! parsing

			!print *, 'compound assign'
			call compound_assign(vars%vals(node%id_index), res, node%op)

			! For compound assignment, ensure that the LHS is returned
			!print *, 'setting res again'
			res = vars%vals(node%id_index)
			!print *, 'done'

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

			i8 = subscript_eval(node, vars, fns, quietl)

			if (vars%vals(node%id_index)%type == str_type) then
				! TODO: ban compound character substring assignment
				vars%vals(node%id_index)%sca%str%s(i8+1: i8+1) = res%sca%str%s
			else

				!print *, 'LHS array type = ', &
				!	vars%vals(node%id_index)%array%type
				!print *, 'LHS array = ', vars%vals(node%id_index)%array%i32

				array_val = get_array_value_t(vars%vals(node%id_index)%array, i8)
				call compound_assign(array_val, res, node%op)
				call set_array_value_t( &
					vars%vals(node%id_index)%array, i8, array_val)
				res = array_val

			end if
		end if

	case (let_expr)

		! Assign return value
		res = syntax_eval(node%right, vars, fns, quietl)

		!print *, 'assigning identifier ', quote(node%identifier%text)
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
			res%sca%f32 = exp(arg1%sca%f32)

		case ("min")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%sca%i32

			! Note that min/max/println etc. are variadic, so we loop to
			! size(node%args) instead of size(node%params)

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i32 = min(res%sca%i32, arg%sca%i32)
			end do

		case ("max")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%sca%i32
			do i = 2, size(node%args)
				!print *, 'arg ', i
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i32 = max(res%sca%i32, arg%sca%i32)
			end do

		case ("println")

			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(output_unit, '(a)', advance = 'no') arg%to_str()
			end do
			write(output_unit, *)

			!! TODO: what, if anything, should println return?
			!res%sca%i32 = 0

		case ("str")

			res%sca%str%s = ''
			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%str%s = res%sca%str%s // arg%to_str()  ! TODO: use char_vector_t
			end do

		case ("len")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = len(arg%sca%str%s, 4)
			!res%sca%i32 = mylen( arg%sca%str%s )

		case ("parse_i32")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			read(arg%sca%str%s, *) res%sca%i32  ! TODO: catch iostat

		case ("parse_i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			read(arg%sca%str%s, *) res%sca%i64  ! TODO: catch iostat

		case ("i32")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%to_i32()

		case ("i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i64 = arg%to_i64()

		case ("open")

			arg = syntax_eval(node%args(1), vars, fns, quietl)

			! TODO: catch iostat, e.g. same file opened twice, folder doesn't
			! exist, etc.
			open(newunit = res%sca%file_%unit_, file = arg%sca%str%s)

			!print *, 'opened unit ', res%sca%file_%unit_
			res%sca%file_%name_ = arg%sca%str%s
			res%sca%file_%eof = .false.

		case ("readln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, "reading from unit", arg1%sca%file_%unit_
			res%sca%str%s = read_line(arg1%sca%file_%unit_, io)
			!print *, 'done reading'

			! This could be a very dangerous side effect!  The file argument of
			! readln() acts as an out-arg:  it's eof flag can be toggled on.  I
			! don't have out-args anywhere else so I may want to rethink this
			! :exploding-head:
			!
			! writeln() does not need to mess with the vars struct like this
			! because the file is the actual return value for that fn

			!!print *, 'ident = ', node%args(1)%identifier%text
			!!vars%vals(node%id_index) = res

			! TODO:  set eof flag or crash for other non-zero io
			if (io == iostat_end) then
			!if (io /= 0) then
				!arg1%sca%file_%eof = .true.
				vars%vals(node%args(1)%id_index)%sca%file_%eof = .true.
			end if
			!print *, 'eof   = ', arg1%sca%file_%eof

		case ("writeln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, 'writing to unit ', arg1%sca%file_%unit_
			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(arg1%sca%file_%unit_, '(a)', advance = 'no') arg%to_str()
			end do
			write(arg1%sca%file_%unit_, *)

		case ("eof")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, "checking eof for unit", arg1%sca%file_%unit_
			res%sca%bool = arg1%sca%file_%eof

			!print *, 'eof fn = ', arg1%sca%file_%eof

		case ("close")
			arg = syntax_eval(node%args(1), vars, fns, quietl)
			!print *, 'closing unit ', arg%sca%file_%unit_
			close(arg%sca%file_%unit_)

		case ("exit")

			arg = syntax_eval(node%args(1), vars, fns, quietl)

			io = arg%sca%i32
			if (io == 0) then
				color = fg_bright_green
			else
				color = fg_bold_bright_red
			end if

			write(*,*) color//'Exiting syntran with status '// &
				str(io)//color_reset

			call exit(io)

		case ("size")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			arg2 = syntax_eval(node%args(2), vars, fns, quietl)

			if (arg2%sca%i32 < 0 .or. arg2%sca%i32 >= arg1%array%rank) then
				! TODO: this should be a runtime error (like bounds-checking),
				! not an internal_error.  I just don't have infrastructure for
				! runtime error handling yet
				write(*,*) 'Error: rank mismatch in size() call'
				call internal_error()
			end if

			! TODO: return type?  Make separate size64() fn?
			res%sca%i32 = int(arg1%array%size( arg2%sca%i32 + 1 ))

			! TODO: if the array pointer is not deallocated here, this was
			! causing a memory leak which is especially bad when `size()` is
			! called in a loop.  For something that was affected by the mem
			! leak, see this Advent of Code solution:
			!
			!     https://github.com/JeffIrwin/aoc-syntran/blob/609ff26a1e4d4b7cc00fd4836f26b47d237aea71/2023/08/main-v3.syntran#L306
			!
			!
			! Might not be strictly necessary now that %array is allocatable
			! instead of pointable
			deallocate(arg1%array)

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

			do i = 1, size(node%params)
				if (allocated(vars%vals( node%params(i) )%array)) then
					!print *, 'deallocating node%params ... array'
					deallocate(vars%vals( node%params(i) )%array)
				end if
			end do

		end select

	case (name_expr)
		!print *, 'searching identifier ', node%identifier%text

		if (allocated(node%lsubscripts) .and. &
			vars%vals(node%id_index)%type == str_type) then
			!print *, 'string subscript RHS name expr'

			!print *, 'str type'
			res%type = vars%vals(node%id_index)%type

			select case (node%lsubscripts(1)%sub_kind)
			case (scalar_sub)
				i8 = subscript_eval(node, vars, fns, quietl)
				res%sca%str%s = vars%vals(node%id_index)%sca%str%s(i8+1: i8+1)

			case (range_sub)

				! TODO: str all_sub

				il = subscript_eval(node, vars, fns, quietl) + 1

				! This feels inconsistent and not easy to extend to higher ranks
				right = syntax_eval(node%usubscripts(1), vars, fns, quietl)
				iu = right%sca%i32 + 1

				!print *, ''
				!print *, 'identifier ', node%identifier%text
				!print *, 'il = ', il
				!print *, 'iu = ', iu
				!print *, 'str = ', vars%vals(node%id_index)%sca%str%s

				! Not inclusive of upper bound
				res%sca%str%s = vars%vals(node%id_index)%sca%str%s(il: iu-1)

			case default
				write(*,*) 'Error: unexpected subscript kind'
				call internal_error()
			end select

		else if (allocated(node%lsubscripts)) then

			if (vars%vals(node%id_index)%type /= array_type) then
				write(*,*) 'Error: bad type, expected array'
				call internal_error()
			end if

			!print *, 'sub kind 1 = ', kind_name(node%lsubscripts(1)%sub_kind)
			!print *, 'rank = ', node%val%array%rank

			if (all(node%lsubscripts%sub_kind == scalar_sub)) then

				! This could probably be lumped in with the range_sub case now
				! that I have it fully generalized
				i8 = subscript_eval(node, vars, fns, quietl)
				res = get_array_value_t(vars%vals(node%id_index)%array, i8)

			else

				rank = vars%vals(node%id_index)%array%rank
				allocate(lsubs(rank), usubs(rank))
				rank_res = 0
				do i = 1, rank

					if (node%lsubscripts(i)%sub_kind == all_sub) then
						lsubs(i) = 0
						!print *, 'lsubs(i) = ', lsubs(i)
					else
						lsubval = syntax_eval(node%lsubscripts(i), vars, fns, quietl)
						lsubs(i) = lsubval%sca%i32
					end if

					select case (node%lsubscripts(i)%sub_kind)
					case (all_sub)
						usubs(i) = vars%vals(node%id_index)%array%size(i)
						!print *, 'usubs(i) = ', usubs(i)

						rank_res = rank_res + 1

					case (range_sub)
						usubval = syntax_eval(node%usubscripts(i), vars, fns, quietl)
						usubs(i) = usubval%sca%i32

						rank_res = rank_res + 1

					case (scalar_sub)
						! Scalar subs are converted to a range-1 sub so we can
						! iterate later without further case logic
						usubs(i) = lsubs(i) + 1

					case default
						write(*,*) err_int_prefix//'cannot evaluate subscript kind'//color_reset
						call internal_error()

					end select

				end do
				!print *, 'lsubs = ', lsubs
				!print *, 'usubs = ', usubs
				!print *, 'rank_res = ', rank_res

				!print *, 'type = ', kind_name( node%val%array%type )

				!print *, 'type  = ', node%val%array%type
				!print *, 'rank  = ', node%val%array%rank
				!print *, 'size  = ', node%val%array%size
				!print *, 'len_  = ', node%val%array%len_
				!print *, 'cap   = ', node%val%array%cap

				allocate(res%array)
				res%type = array_type
				res%array%kind = expl_array
				res%array%type = node%val%array%type
				res%array%rank = rank_res

				allocate(res%array%size( rank_res ))
				idim_res = 1
				do idim_ = 1, rank
					select case (node%lsubscripts(idim_)%sub_kind)
					case (range_sub, all_sub)

						res%array%size(idim_res) = usubs(idim_) - lsubs(idim_)

						idim_res = idim_res + 1
					end select
				end do
				!print *, 'res size = ', res%array%size

				res%array%len_ = product(res%array%size)
				!print *, 'res len = ', res%array%len_

				call allocate_array(res%array, res%array%len_)

				! Iterate through all subscripts in range and copy to result
				! array
				subs = lsubs
				do i8 = 0, res%array%len_ - 1
					!print *, 'subs = ', int(subs, 4)

					! subscript_eval() inlined.  is there a way to copy a slice
					! without doing so much math?
					!
					! TODO: bound checking if enabled.  unlike subscript_eval(),
					! we can do it here outside the i8 loop
					prod  = 1
					index_ = 0
					do j = 1, rank
						!print *, 'j = ', j
						index_ = index_ + prod * subs(j)
						prod  = prod * vars%vals(node%id_index)%array%size(j)
					end do
					!print *, 'index_ = ', index_

					call set_array_value_t(res%array, i8, &
					     get_array_value_t(vars%vals(node%id_index)%array, index_))

					! get next subscript.  this is the bignum += 1 algorithm but
					! in an arbitrary mixed radix
					j = 1
					do while (j < rank .and. subs(j) == usubs(j) - 1)
						subs(j) = lsubs(j)
						j = j + 1
					end do
					subs(j) = subs(j) + 1

				end do
			end if

		else
			!print *, 'name expr'
			res = vars%vals(node%id_index)

			! Deep copy of whole array instead of aliasing pointers
			if (res%type == array_type) then
				!print *, 'array  name_expr'

				if (allocated(res%array)) deallocate(res%array)

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
					res%sca%i32 = -right%sca%i32
				case (i64_type)
					res%sca%i64 = -right%sca%i64
				case (f32_type)
					res%sca%f32 = -right%sca%f32
			end select

		case (not_keyword)
			res%sca%bool = .not. right%sca%bool

		case default
			write(*,*) err_eval_unary_op(node%op%text)
			call internal_error()
		end select

	case (binary_expr)

		left  = syntax_eval(node%left , vars, fns, quietl)
		right = syntax_eval(node%right, vars, fns, quietl)

		!print *, 'left  type = ', kind_name(left%type)
		!print *, 'right type = ', kind_name(right%type)

		res%type = get_binary_op_kind(left%type, node%op%kind, right%type)

		! TODO: DRY up with helper fn?
		select case (res%type)
		case (bool_array_type)
			!allocate(expr%val%array)
			!expr%val%array%type = bool_type
			res%type = array_type

		! TODO: other array sub types

		!case default
		!	expr%val%type = type_

		end select

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
			call pow(left, right, res, node%op%text)

		case (slash_token)
			call div(left, right, res, node%op%text)

		case (percent_token)
			call mod_(left, right, res, node%op%text)

		case (and_keyword)
			res%sca%bool = left%sca%bool .and. right%sca%bool

		case (or_keyword)
			res%sca%bool = left%sca%bool .or.  right%sca%bool

		case (eequals_token)

			! Is there a reason to make this a fn like add, mul, etc?  Those
			! save duplicated code for compound assignment, but I don't see a
			! use for === compound assignment (different from javascript ===)
			!
			! TODO: move to value.f90 anyway for consistency now that I've split
			! the source

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 == right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 == right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 == right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 == right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 == right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 == right%sca%i32
				! TODO: is this even possible or should I ban comparing ints and
				! floats?  Similarly for other comparisons
				!
				! GNU says Warning: Equality comparison for REAL(4) at (1)
				! [-Wcompare-reals]
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 == right%sca%f32
			case        (magic * bool_type + bool_type)
				res%sca%bool = left%sca%bool .eqv. right%sca%bool
			case        (magic * str_type + str_type)
				res%sca%bool = left%sca%str%s == right%sca%str%s

			case        (magic * array_type + i32_type)

				print *, 'left%type       = ', kind_name(left%type)
				print *, 'left array type = ', kind_name(left%array%type)

				!res%sca%bool = .false.
				!res%sca%bool = left%sca%str%s == right%sca%str%s

				select case (left%array%type)
				case (i32_type)

					allocate(res%array)
					res%type  = array_type
					res%array%bool = left%array%i32 == right%sca%i32
					res%array%type = bool_type
					print *, 'res = ', res%array%bool

					! TODO: helper fn to construct array meta-data
					res%array%kind = expl_array
					!res%array%type = node%val%array%type
					res%array%rank = left%array%rank

					! TODO: len_, cap, size
					res%array%len_ = left%array%len_
					res%array%cap  = left%array%cap
					res%array%size = left%array%size

				case default
					! TODO: refactor with below default?
					write(*,*) err_eval_binary_types(node%op%text)
					call internal_error()
				end select

			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (bang_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 /= right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 /= right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 /= right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 /= right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 /= right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 /= right%sca%i32
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 /= right%sca%f32
			case        (magic * bool_type + bool_type)
				res%sca%bool = left%sca%bool .neqv. right%sca%bool
			case        (magic * str_type + str_type)
				res%sca%bool = left%sca%str%s /= right%sca%str%s

			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (less_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 < right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 < right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 < right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 < right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 < right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 < right%sca%i32
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 < right%sca%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (less_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 <= right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 <= right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 <= right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 <= right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 <= right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 <= right%sca%i32
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 <= right%sca%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (greater_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 > right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 > right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 > right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 > right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 > right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 > right%sca%i32
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 > right%sca%f32
			case default
				! FIXME: other numeric types (i64, f64, etc.)
				write(*,*) err_eval_binary_types(node%op%text)
				call internal_error()
			end select

		case (greater_equals_token)

			select case (magic * left%type + right%type)
			case        (magic * i32_type + i32_type)
				res%sca%bool = left%sca%i32 >= right%sca%i32

			case        (magic * i64_type + i64_type)
				res%sca%bool = left%sca%i64 >= right%sca%i64

			case        (magic * i32_type + i64_type)
				res%sca%bool = left%sca%i32 >= right%sca%i64

			case        (magic * i64_type + i32_type)
				res%sca%bool = left%sca%i64 >= right%sca%i32

			case        (magic * f32_type + f32_type)
				res%sca%bool = left%sca%f32 >= right%sca%f32
			case        (magic * f32_type + i32_type)
				res%sca%bool = left%sca%f32 >= right%sca%i32
			case        (magic * i32_type + f32_type)
				res%sca%bool = left%sca%i32 >= right%sca%f32
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

subroutine promote_i32_i64(val)

	! If val is i32 type, change it to i64 and copy the values

	type(value_t), intent(inout) :: val

	if (val%type == i32_type) then
		val%type = i64_type
		val%sca%i64 = val%sca%i32
	end if

end subroutine promote_i32_i64

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

	case (sstar_equals_token)
		call pow(lhs, rhs, lhs, op%text)

	case (percent_equals_token)
		call mod_(lhs, rhs, lhs, op%text)

	case default
		write(*,*) 'Error: unexpected assignment operator ', quote(op%text)
		call internal_error()
	end select

end subroutine compound_assign

!===============================================================================

function get_array_value_t(array, i) result(val)

	type(array_t), intent(in) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t) :: val

	!print *, 'starting get_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)

	val%type = array%type
	select case (array%type)
		case (bool_type)
			val%sca%bool = array%bool(i + 1)

		case (i32_type)
			val%sca%i32 = array%i32(i + 1)
		case (i64_type)
			val%sca%i64 = array%i64(i + 1)

		case (f32_type)
			val%sca%f32 = array%f32(i + 1)

		case (str_type)
			val%sca%str = array%str(i + 1)

	end select

end function get_array_value_t

!===============================================================================

subroutine set_array_value_t(array, i, val)

	type(array_t), intent(inout) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(in) :: val

	!print *, 'starting set_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)
	!print *, 'val%type   = ', kind_name(val%type)

	! array%type is already set
	select case (array%type)
		case (bool_type)
			array%bool(i + 1) = val%sca%bool

		case (i32_type)
			array%i32(i + 1) = val%sca%i32

		case (i64_type)
			array%i64(i + 1) = val%sca%i64

		case (f32_type)
			array%f32(i + 1) = val%sca%f32

		case (str_type)
			array%str(i + 1) = val%sca%str

	end select

end subroutine set_array_value_t

!===============================================================================

end module syntran__core_m

!===============================================================================

