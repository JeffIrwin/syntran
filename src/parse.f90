
!===============================================================================

module syntran__parse_m

	! This module is almost entirely interfaces to routines implemented in
	! src/parse_*.f90.  The remaining fns implemented directly in this file are
	! so short that defining interfaces for them would add significantly more
	! lines of code

	use syntran__lex_m

	implicit none

	type parser_t

		! The parser takes a string of tokens (technically an array) and
		! constructs higher-level structures such as terms and expressions, like
		! constructing a phrase or sentence from words

		type(syntax_token_t), allocatable :: tokens(:)
		integer :: pos  ! token index position

		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		type(string_vector_t) :: diagnostics

		! Context for all src files (including include files).  Could convert to
		! standard array instead after size is known but I don't expect a
		! performance diff
		type(text_context_vector_t) :: contexts

		logical :: is_loc = .false.

		type(vars_t) :: vars, locs
		integer :: num_vars = 0
		integer :: num_locs = 0

		type(fns_t) :: fns
		integer :: num_fns = 0
		type(string_vector_t) :: fn_names
		type(string_vector_t) :: var_names    ! track module-level variable names
		type(string_vector_t) :: struct_names ! track module-level struct names

		type(structs_t) :: structs
		integer :: num_structs = 0

		! Set this to (the current) fn's return type.  Check that each return
		! statement matches while parsing.  This is redundant since the fn
		! syntax node also has the type, but it's easier to store it here than
		! to walk back up the syntax tree to do return type checking
		!
		! This won't work with nested fns but we don't allow that anyway
		type(value_t) :: fn_type
		character(len = :), allocatable :: fn_name
		logical :: returned

		! Method parsing context: set while parsing a method body
		logical :: in_method = .false.
		logical :: in_const_method = .false.
		integer :: self_loc_id = 0     ! loc slot index of "0self"
		type(struct_t) :: method_struct  ! struct whose method is being parsed

		! Pass index.  0 on first pass while getting fn signatures, then 1 on
		! final (second) pass
		integer :: ipass

		logical :: repl

		type(map_i32_t) :: import_stack
		type(map_i32_t) :: imported_modules

		contains
			procedure :: &
				context => current_context, &
				current => current_token, &
				current_kind, &
				current_pos, &
				current_unit, &
				match, &
				match_pre, &
				next => next_token, &
				parse_array_expr, &
				parse_block_statement, &
				parse_expr, &
				parse_expr_statement, &
				parse_fn_declaration, &
				parse_method_declaration, &
				parse_fn_call, &
				parse_qualified_expr, &
				parse_struct_declaration, &
				parse_struct_instance, &
				parse_for_statement, &
				parse_if_statement, &
				parse_return_statement, &
				parse_break_statement, &
				parse_continue_statement, &
				parse_use_statement, &
				parse_name_expr, &
				parse_primary_expr, &
				parse_size, &
				parse_statement, &
				parse_subscripts, &
				parse_dot, &
				parse_type, &
				parse_unit, &
				parse_while_statement, &
				peek => peek_token, &
				peek_kind, &
				peek_text, &
				current_text, &
				peek_pos, &
				peek_unit, &
				preprocess, &
				text => parser_text, &
				tokens_str

	end type parser_t

	!********

	interface
		! Implemented in parse_fn.f90

		module subroutine parse_fn_declaration(parser, decl)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: decl
		end subroutine parse_fn_declaration

		recursive module subroutine parse_fn_call(parser, module_prefix, identifier, fn_call)
			class(parser_t) :: parser
			character(len = *), intent(in), optional :: module_prefix
			type(syntax_token_t), intent(in), optional :: identifier
			type(syntax_node_t), intent(out) :: fn_call
		end subroutine parse_fn_call

		recursive module subroutine parse_qualified_expr(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_qualified_expr

		module subroutine parse_type(parser, type_text, type)
			class(parser_t) :: parser
			character(len = :), intent(out), allocatable :: type_text
			type(value_t), intent(out) :: type
		end subroutine parse_type

		! TODO: move struct stuff to another translation unit? parse_fn.f90 is a
		! very manageable ~1100 lines rn, so not much benefit to splitting
		module subroutine parse_struct_declaration(parser, decl)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: decl
		end subroutine parse_struct_declaration

		module subroutine parse_method_declaration(parser, decl, struct, is_const, struct_name)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: decl
			type(struct_t), intent(in) :: struct
			logical, intent(in) :: is_const
			character(len = *), intent(in) :: struct_name
		end subroutine parse_method_declaration

		recursive module subroutine parse_struct_instance(parser, inst, struct_name)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: inst
			character(len = *), intent(in), optional :: struct_name
		end subroutine parse_struct_instance

		module subroutine check_call_arg(parser, arg, call_is_ref_i, arg_span, &
				fn_name, i_0based, param_val, param_name, param_is_ref, param_is_const_ref)
			class(parser_t), intent(inout) :: parser
			type(syntax_node_t), intent(in) :: arg
			logical(kind = 1), intent(in) :: call_is_ref_i
			type(text_span_t), intent(in) :: arg_span
			character(len = *), intent(in) :: fn_name, param_name
			integer, intent(in) :: i_0based
			type(value_t), intent(in) :: param_val
			logical, intent(in) :: param_is_ref, param_is_const_ref
		end subroutine check_call_arg

	end interface

	!********

	interface
		! Implemented in parse_array.f90

		recursive module subroutine parse_array_expr(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_array_expr

		module subroutine parse_size(parser, size)
			class(parser_t) :: parser
			type(syntax_node_vector_t), intent(out) :: size
		end subroutine parse_size

		recursive module subroutine parse_subscripts(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(inout) :: expr
		end subroutine parse_subscripts

	end interface

	!********

	interface
		! Implemented in parse_control.f90

		recursive module subroutine parse_if_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_if_statement

		module subroutine parse_return_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_return_statement

		module subroutine parse_break_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_break_statement

		module subroutine parse_continue_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_continue_statement

		module subroutine parse_use_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_use_statement

		recursive module subroutine parse_for_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_for_statement

		recursive module subroutine parse_while_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_while_statement

		recursive module subroutine parse_block_statement(parser, block)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: block
		end subroutine parse_block_statement

		recursive module subroutine parse_statement(parser, statement)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: statement
		end subroutine parse_statement

	end interface

	!********

	interface
		! Implemented in parse_expr.f90

		recursive module subroutine parse_expr_statement(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_expr_statement

		recursive module subroutine parse_expr(parser, parent_prec, expr)
			class(parser_t) :: parser
			integer, optional, intent(in) :: parent_prec
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_expr

		recursive module subroutine parse_primary_expr(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_primary_expr

		recursive module subroutine parse_name_expr(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: expr
		end subroutine parse_name_expr

		recursive module subroutine parse_dot(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(inout) :: expr
		end subroutine parse_dot

	end interface

	!********

	interface
		! Implemented in parse_misc.f90

		module function tokens_str(parser) result(str_)
			class(parser_t) :: parser
			character(len = :), allocatable :: str_
		end function tokens_str

		module function match(parser, kind) result(token)
			class(parser_t) :: parser
			integer :: kind
			type(syntax_token_t) :: token
		end function match

		recursive module subroutine preprocess(parser, tokens_in, src_file, contexts, unit_)
			class(parser_t) :: parser
			type(syntax_token_t), intent(in) :: tokens_in(:)
			character(len = *), intent(in) :: src_file
			type(text_context_vector_t), intent(inout) :: contexts
			integer, intent(inout) :: unit_
		end subroutine preprocess

		module function match_pre(parser, kind, tokens, token_index, context) result(token)
			class(parser_t) :: parser
			integer :: kind
			type(syntax_token_t), intent(in) :: tokens(:)
			integer, intent(inout) :: token_index
			type(text_context_t) :: context
			type(syntax_token_t) :: token
		end function match_pre

		module subroutine parse_unit(parser, unit)
			class(parser_t) :: parser
			type(syntax_node_t), intent(out) :: unit
		end subroutine parse_unit

		recursive module function new_parser(str_, src_file, contexts, unit_) result(parser)
			character(len = *), intent(in) :: str_, src_file
			type(text_context_vector_t) :: contexts
			integer, intent(inout) :: unit_
			type(parser_t) :: parser
		end function new_parser

	end interface

!===============================================================================

contains

!===============================================================================

! You can't chain together member fn calls and their children like
! parser%current()%kind in Fortran, so use this helper fn instead

integer function current_kind(parser)
	class(parser_t) :: parser
	current_kind = parser%peek_kind(0)
end function current_kind

!********

integer function peek_kind(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_kind = peek%kind
end function peek_kind

!===============================================================================

function current_text(parser)
	character(len = :), allocatable :: current_text
	class(parser_t) :: parser
	current_text = parser%peek_text(0)
end function current_text

!********

function peek_text(parser, offset)
	character(len = :), allocatable :: peek_text
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_text = peek%text
end function peek_text

!===============================================================================

function current_token(parser)
	class(parser_t) :: parser
	type(syntax_token_t) :: current_token
	current_token = parser%peek(0)
end function current_token

!********

function peek_token(parser, offset) result(token)

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

end function peek_token

!===============================================================================

function next_token(parser) result(next)
	class(parser_t) :: parser
	type(syntax_token_t) :: next
	next = parser%current()
	parser%pos = parser%pos + 1
end function next_token

!===============================================================================

integer function current_pos(parser)

	! Get the current character index.  If you want the token index, use
	! parser%pos instead

	class(parser_t) :: parser

	current_pos = parser%peek_pos(0)

end function current_pos

!********

integer function peek_pos(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_pos = peek%pos
end function peek_pos

!********

integer function current_unit(parser)
	class(parser_t) :: parser
	current_unit = parser%peek_unit(0)
end function current_unit

!********

integer function peek_unit(parser, offset)
	class(parser_t) :: parser
	type(syntax_token_t) :: peek
	integer, intent(in) :: offset
	peek = parser%peek(offset)
	peek_unit = peek%unit_
end function peek_unit

!********

function current_context(parser) result(context)
	class(parser_t) :: parser
	type(text_context_t) :: context

	!parser%contexts%v(parser%current_unit()), span, type_text))
	context = parser%contexts%v( parser%current_unit() )

end function current_context

!********

function parser_text(parser, beg_, end_) result(text)
	class(parser_t) :: parser
	integer, intent(in) :: beg_, end_
	type(text_context_t) :: context
	character(len = :), allocatable :: text

	!parser%context(), span, parser%context%text(cond_beg: cond_end), &
	context = parser%context()
	text = context%text(beg_: end_)

end function parser_text

!===============================================================================

end module syntran__parse_m

!===============================================================================

