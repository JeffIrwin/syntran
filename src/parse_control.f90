
!===============================================================================

submodule (syntran__parse_m) syntran__parse_control

	use syntran__intr_fns_m

	implicit none

	! FIXME: remember to prepend routines like `module function` or `module
	! subroutine` when pasting them into a submodule.  gfortran doesn't care but
	! intel fortran will refuse to compile otherwise

!===============================================================================

contains

!===============================================================================

module function parse_return_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: right_beg, right_end

	type(syntax_token_t) :: return_token, semi
	type(text_span_t) :: span

	!print *, "starting parse_return_statement()"

	right_beg = parser%peek_pos(0)
	return_token = parser%match(return_keyword)
	parser%returned = .true.

	statement%kind = return_statement

	allocate(statement%right)

	if (parser%current_kind() == semicolon_token) then
		! Void return statement

		!! already matched below
		!semi = parser%match(semicolon_token)

		!right_end = parser%peek_pos(0) - 1
		right_end = parser%peek_pos(0)

		statement%right%val%type = void_type

	else
		! expr or statement?
		right_beg = parser%peek_pos(0)
		statement%right = parser%parse_expr()
		right_end = parser%peek_pos(0) - 1

	end if
	semi = parser%match(semicolon_token)

	! Check return type (unless we're at global level ifn == 1, in which case
	! %fn_type is any_type).  That's half the point of return statements
	!
	! There should also be a check that every branch of a fn has a return
	! statement, but that seems more difficult
	if (types_match(parser%fn_type, statement%right%val) /= TYPE_MATCH) then
	if (statement%right%val%type /= unknown_type) then  ! don't cascade
		span = new_span(right_beg, right_end - right_beg + 1)
		call parser%diagnostics%push( &
			err_bad_ret_type(parser%context(), &
			span, parser%fn_name, &
			type_name(parser%fn_type), &
			type_name(statement%right%val)))
	end if
	end if

end function parse_return_statement

!===============================================================================

module function parse_break_statement(parser) result(statement)

	class(parser_t) :: parser
	type(syntax_node_t) :: statement
	!********
	type(syntax_token_t) :: break_token, semi

	break_token = parser%match(break_keyword)
	statement%kind = break_statement
	semi = parser%match(semicolon_token)

end function parse_break_statement

!===============================================================================

module function parse_continue_statement(parser) result(statement)

	class(parser_t) :: parser
	type(syntax_node_t) :: statement
	!********
	type(syntax_token_t) :: continue_token, semi

	continue_token = parser%match(continue_keyword)
	statement%kind = continue_statement
	semi = parser%match(semicolon_token)

end function parse_continue_statement

!===============================================================================

module function parse_use_statement(parser) result(statement)

	use syntran__errors_m

	! Parse `use module;` or `use module::*;` or `use module::name;`
	!
	! - `use module;`         imports functions as module::fn (qualified access)
	! - `use module::*;`      imports all functions (unqualified access)
	! - `use module::name;`   imports specific function name (unqualified)
	! - `use path/to/module;  imports path/to/module.syntran, can be combined with qualified or unqualified forms above

	class(parser_t) :: parser
	type(syntax_node_t) :: statement
	!********
	character(len = :), allocatable :: module_name, import_name, module_path
	character(len = :), allocatable :: mod_filename, mod_text, src_dir, fn_name
	character(len = :), allocatable :: insert_name
	type(syntax_token_t) :: use_token, mod_identifier, double_colon, &
		name_identifier, semi, star, dummy
	type(text_span_t) :: span
	type(parser_t) :: mod_parser
	type(syntax_node_t) :: mod_unit
	type(text_context_vector_t) :: mod_contexts
	type(fn_t) :: fn
	integer :: i, io, iostat, mod_unit_
	logical :: qualified_import
	character(len = :), allocatable :: qualified_prefix

	use_token = parser%match(use_keyword)

	! Handle parent directory references (e.g., `use ../module;` or `use ../../module;`)
	! module_path includes "../" for file resolution, module_name is for namespacing
	module_path = ""
	do while (parser%current_kind() == dot_token .and. &
	          parser%peek_kind(1) == dot_token .and. &
	          parser%peek_kind(2) == slash_token)
		! Match ".." and "/"
		dummy = parser%match(dot_token)
		dummy = parser%match(dot_token)
		dummy = parser%match(slash_token)
		module_path = module_path // "../"
	end do

	! Handle current directory reference (e.g., `use ./module;`)
	if (parser%current_kind() == dot_token .and. &
	    parser%peek_kind(1) == slash_token) then
		dummy = parser%match(dot_token)
		dummy = parser%match(slash_token)
		module_path = module_path // "./"
	end if

	mod_identifier = parser%match(identifier_token)
	module_name = mod_identifier%text
	module_path = module_path // module_name

	! Handle module paths with slashes (e.g., `use math/vectors::*;`)
	do while (parser%current_kind() == slash_token)
		dummy = parser%match(slash_token)
		name_identifier = parser%match(identifier_token)
		module_name = module_name // "/" // name_identifier%text
		module_path = module_path // "/" // name_identifier%text
	end do

	! Check for `use module;` (qualified import) vs `use module::*;` (glob import)
	if (parser%current_kind() == double_colon_token) then
		double_colon = parser%match(double_colon_token)

		! Check for glob import (use module::*)
		if (parser%current_kind() == star_token) then
			star = parser%match(star_token)
			import_name = "*"
		else
			name_identifier = parser%match(identifier_token)
			import_name = name_identifier%text
		end if
		qualified_import = .false.
	else
		! `use module;` - qualified import
		import_name = ""
		qualified_import = .true.
	end if

	semi = parser%match(semicolon_token)

	! Return an empty statement (no-op)
	statement%kind = expr_statement
	statement%is_empty = .true.

	! For std::, all intrinsics are already globally available
	if (module_name == "std") return

	! Get the directory of the current source file
	src_dir = get_dir(parser%contexts%v(parser%current_unit())%src_file)
	mod_filename = src_dir // module_path // ".syntran"

	! Check if module file exists
	if (.not. exists(mod_filename)) then
		span = new_span(mod_identifier%pos, len(mod_identifier%text))
		call parser%diagnostics%push( &
			err_mod_404(parser%context(), span, mod_filename))
		return
	end if

	! Read the module file
	mod_text = read_file(mod_filename, iostat)
	if (iostat /= exit_success) then
		span = new_span(mod_identifier%pos, len(mod_identifier%text))
		call parser%diagnostics%push( &
			err_mod_read(parser%context(), span, mod_filename))
		return
	end if

	! Create a new parser for the module
	mod_contexts = new_context_vector()
	mod_unit_ = 0
	mod_parser = new_parser(mod_text, mod_filename, mod_contexts, mod_unit_)

	! Initialize intrinsic functions for the module parser.
	! We use declare_intr_fns instead of copying from the main parser's dict
	! to avoid copying previously imported module functions which would cause
	! redeclaration errors in pass 1.
	call declare_intr_fns(mod_parser%fns)
	mod_parser%num_fns = mod_parser%fns%num_intr_fns

	! Parse the module
	mod_unit = mod_parser%parse_unit()

	! Check for parsing errors in the module (only in first pass)
	if (parser%ipass == 0 .and. mod_parser%diagnostics%len_ > 0) then
		call parser%diagnostics%push( &
			err_prefix // "failed to parse module `" // module_name // "`:" // color_reset)
		do i = 1, mod_parser%diagnostics%len_
			call parser%diagnostics%push(mod_parser%diagnostics%v(i)%s)
		end do
		return
	end if

	! Copy parsed functions from module parser to current parser
	do i = 1, mod_parser%fn_names%len_
		fn_name = mod_parser%fn_names%v(i)%s

		! Look up the function in the module parser
		fn = mod_parser%fns%search(fn_name, io, iostat)
		if (iostat /= exit_success) cycle

		! Determine the name to insert: qualified (module::fn) or unqualified (fn)
		! For qualified imports, convert path separators to namespace separators
		! e.g., "math/vectors" -> "math::vectors::fn"
		if (qualified_import) then
			! Replace "/" with "::" in module_name for qualified prefix
			qualified_prefix = replace_all(module_name, "/", "::")
			insert_name = qualified_prefix // "::" // fn_name
		else
			insert_name = fn_name

			! Check if this would shadow an overloaded intrinsic function. Note:
			! we only check overloaded intrinsics here because non-overloaded
			! intrinsics are handled mostly like normal user-defined fns
			!
			! TODO: is this still needed since we already handle
			! err_redeclare_intr_fn() elsewhere?
			if (is_overloaded_intr(fn_name)) then
				span = new_span(mod_identifier%pos, len(mod_identifier%text))
				call parser%diagnostics%push( &
					err_redeclare_intr_fn(parser%context(), span, fn_name))
				cycle
			end if
		end if

		! Insert into current parser with new id_index
		parser%num_fns = parser%num_fns + 1
		call parser%fns%insert(insert_name, fn, parser%num_fns, io)

		! Only push to fn_names in the first pass (like parse_fn_declaration)
		if (parser%ipass == 0) call parser%fn_names%push(insert_name)
	end do

end function parse_use_statement

!===============================================================================

recursive module function parse_if_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end, type_

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

	! Check that condition type is bool.  If the condition depends on a fn which
	! is declared below, it may be unknown on pass 0
	type_ = condition%val%type
	if (type_ /= bool_type .and. type_ /= unknown_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context(), span, parser%text(cond_beg, cond_end), &
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

recursive module function parse_for_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: arr_beg, arr_end

	type(syntax_node_t)  :: array, body
	type(syntax_token_t) :: for_token, in_token, identifier

	type(text_span_t) :: span

	type(value_t) :: dummy

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
	call parser%locs%push_scope()

	identifier = parser%match(identifier_token)

	in_token   = parser%match(in_keyword)

	arr_beg  = parser%peek_pos(0)
	!array      = parser%parse_array_expr()
	array      = parser%parse_primary_expr()
	arr_end  = parser%peek_pos(0) - 1

	if (parser%is_loc) then
		parser%num_locs = parser%num_locs + 1
		statement%id_index = parser%num_locs
		statement%is_loc = .true.
	else
		parser%num_vars = parser%num_vars + 1
		statement%id_index = parser%num_vars
		statement%is_loc = .false.
	end if

	! Auto declare loop iterator in for statement (HolyC doesn't let you do
	! that!).  The 'let' keyword is not used:
	!
	!     for i in [lower, upper]
	!        {}

	! Insert the identifier's type into the dict. This is a local scope, so
	! there's no need to check io

	!print *, 'identifier%text = ', identifier%text
	!print *, 'allocated(array%lbound) = ', allocated(array%lbound)

	!if (allocated(array%lbound)) then
	if (allocated(array%val%array)) then

		!print *, 'array%val%type = ', kind_name(array%val%type) ! "array_type" :(
		!print *, 'array type = ', kind_name(array%val%array%type)

		! Pathological code like `for <EOF>` can crash the parser :(

		! Array iterator type could be i32 or i64, and lbound type might not
		! match ubound type!
		dummy%type = array%val%array%type
		if (parser%is_loc) then
			call parser%locs%insert(identifier%text, dummy, statement%id_index)
		else
			call parser%vars%insert(identifier%text, dummy, statement%id_index)
		end if

	else

		dummy%type = array%val%type
		if (parser%is_loc) then
			call parser%locs%insert(identifier%text, dummy, statement%id_index)
		else
			call parser%vars%insert(identifier%text, dummy, statement%id_index)
		end if

		! I guess we could allow a 1-loop iteration on a scalar if that's
		! worthwhile.  Eval would need some work

		if (array%val%type /= str_type) then
			span = new_span(arr_beg, arr_end - arr_beg + 1)
			call parser%diagnostics%push(err_non_array_loop( &
				parser%context(), span, parser%text(arr_beg, arr_end)))
		end if

	end if

	body = parser%parse_statement()

	allocate(statement%array)
	allocate(statement%body)

	statement%kind = for_statement

	statement%identifier = identifier
	statement%array      = array
	statement%body       = body

	call parser%vars%pop_scope()
	call parser%locs%pop_scope()

end function parse_for_statement

!===============================================================================

recursive module function parse_while_statement(parser) result(statement)

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: cond_beg, cond_end, type_

	type(syntax_node_t)  :: body, condition
	type(syntax_token_t) :: while_token
	type(text_span_t) :: span

	while_token  = parser%match(while_keyword)

	cond_beg  = parser%peek_pos(0)
	condition = parser%parse_expr()
	cond_end  = parser%peek_pos(0) - 1

	! Check that condition type is bool
	type_ = condition%val%type
	if (type_ /= bool_type .and. type_ /= unknown_type) then
		span = new_span(cond_beg, cond_end - cond_beg + 1)
		call parser%diagnostics%push(err_non_bool_condition( &
			parser%context(), span, parser%text(cond_beg, cond_end), &
			"while-loop"))
	end if

	body = parser%parse_statement()

	allocate(statement%condition, statement%body)

	statement%kind = while_statement

	statement%condition = condition
	statement%body      = body

end function parse_while_statement

!===============================================================================

recursive module function parse_block_statement(parser) result(block)

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
	call parser%locs%push_scope()

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
	call parser%locs%pop_scope()

	right = parser%match(rbrace_token)

	block%kind = block_statement

	! Convert to standard array
	call syntax_nodes_copy(block%members, members%v( 1: members%len_ ))

end function parse_block_statement

!===============================================================================

recursive module function parse_statement(parser) result(statement)

	use syntran__errors_m

	class(parser_t) :: parser

	type(syntax_node_t) :: statement

	!********

	integer :: pos_beg, pos_end

	type(syntax_token_t) :: semi

	type(text_span_t) :: span

	select case (parser%current_kind())
	case (lbrace_token)
		statement = parser%parse_block_statement()

	case (if_keyword)
		statement = parser%parse_if_statement()

	case (for_keyword)
		statement = parser%parse_for_statement()

	case (while_keyword)
		statement = parser%parse_while_statement()

	case (return_keyword)
		statement = parser%parse_return_statement()

	case (break_keyword)
		statement = parser%parse_break_statement()

	case (continue_keyword)
		statement = parser%parse_continue_statement()

	case (use_keyword)
		statement = parser%parse_use_statement()

	case default
		pos_beg   = parser%peek_pos(0)
		statement = parser%parse_expr_statement()
		pos_end   = parser%peek_pos(0)
		semi      = parser%match(semicolon_token)

		if (.not. parser%repl .and. parser%ipass > 0) then
			!print *, "statement kind = ", kind_name(statement%kind)

			! Ban expression statements.  I tried for a while to put this
			! logic inside of parse_expr_statement() but it is difficult to
			! get the recursive descent parsing logic correct, especially
			! considering that it is allowed in the REPL but not in script
			! files, and moreso with edge cases like nested assignment.
			! It's much easier to parse it unconditionally and then check it
			! afterwards here
			!
			! Many tests depend on the REPL style behavior where there is
			! just one statement, and the value is returned implicitly
			! without an explicit `return`

			select case (statement%kind)
			case (let_expr, assignment_expr)
				! Do nothing.  These kinds of expressions are allowed

			case (fn_call_expr, fn_call_intr_expr)
				! Only allow void fn call statements.  Don't allow
				! discarding fn return value

				!print *, "fn ret type = ", kind_name(statement%val%type)

				if (statement%val%type /= void_type) then
					span = new_span(pos_beg, pos_end - pos_beg + 1)
					call parser%diagnostics%push( &
						err_bad_expr(parser%context(), &
						span))
				end if

			case default
				span = new_span(pos_beg, pos_end - pos_beg + 1)
				call parser%diagnostics%push( &
					err_bad_expr(parser%context(), &
					span))
			end select

		end if

	end select

end function parse_statement

!===============================================================================

end submodule syntran__parse_control

!===============================================================================

