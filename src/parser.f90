
!===============================================================================

module syntran__parser_m

	! This module is almost entirely interfaces to routines implemented in
	! src/parse_*.f90.  The remaining fns implemented directly in this file are
	! so short that defining interfaces for them would add significantly more
	! lines of code

	!use syntran__errors_m
	use syntran__lexer_m

	!! types and utils are used indirectly through lexer, so it doesn't matter
	!! much if they're explicitly included here
	!use syntran__types_m
	!use syntran__utils_m

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

		type(vars_t) :: vars
		integer :: num_vars = 0

		type(fns_t) :: fns
		integer :: num_fns = 0

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
				parse_fn_call, &
				parse_fn_declaration, &
				parse_for_statement, &
				parse_if_statement, &
				parse_name_expr, &
				parse_primary_expr, &
				parse_size, &
				parse_statement, &
				parse_subscripts, &
				parse_type, &
				parse_unit, &
				parse_while_statement, &
				peek => peek_token, &
				peek_kind, &
				peek_pos, &
				peek_unit, &
				preprocess, &
				text => parser_text, &
				tokens_str

	end type parser_t

	!********

	interface
		! Implemented in parse_fn.f90

		module function parse_fn_call(parser) result(fn_call)
			class(parser_t) :: parser
			type(syntax_node_t) :: fn_call
		end function parse_fn_call

		module function parse_fn_declaration(parser) result(decl)
			class(parser_t) :: parser
			type(syntax_node_t) :: decl
		end function parse_fn_declaration

		module subroutine parse_type(parser, type_text, rank)
			class(parser_t) :: parser
			character(len = :), intent(out), allocatable :: type_text
			integer, intent(out) :: rank
		end subroutine parse_type

	end interface

	!********

	interface
		! Implemented in parse_array.f90

		module function parse_array_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function

		module function parse_size(parser) result(size)
			class(parser_t) :: parser
			type(syntax_node_vector_t) :: size
		end function parse_size

		module subroutine parse_subscripts(parser, expr)
			class(parser_t) :: parser
			type(syntax_node_t), intent(inout) :: expr
		end subroutine parse_subscripts

	end interface

	!********

	interface
		! Implemented in parse_control.f90

		module function parse_if_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_if_statement

		module function parse_for_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_for_statement

		module function parse_while_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_while_statement

		module function parse_block_statement(parser) result(block)
			class(parser_t) :: parser
			type(syntax_node_t) :: block
		end function parse_block_statement

		module function parse_statement(parser) result(statement)
			class(parser_t) :: parser
			type(syntax_node_t) :: statement
		end function parse_statement

	end interface

	!********

	interface
		! Implemented in parse_expr.f90

		recursive module function parse_expr_statement(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_expr_statement

		recursive module function parse_expr(parser, parent_prec) result(expr)
			class(parser_t) :: parser
			integer, optional, intent(in) :: parent_prec
			type(syntax_node_t) :: expr
		end function parse_expr

		module function parse_primary_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_primary_expr

		module function parse_name_expr(parser) result(expr)
			class(parser_t) :: parser
			type(syntax_node_t) :: expr
		end function parse_name_expr

	end interface

	!********

	interface
		! Implemented in parse_misc.f90

		module function tokens_str(parser) result(str)
			class(parser_t) :: parser
			character(len = :), allocatable :: str
		end function tokens_str

		module function match(parser, kind) result(token)
			class(parser_t) :: parser
			integer :: kind
			type(syntax_token_t) :: token
		end function match

		module subroutine preprocess(parser, tokens_in, src_file, contexts, unit_)
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

		module function parse_unit(parser) result(unit)
			class(parser_t) :: parser
			type(syntax_node_t) :: unit
		end function parse_unit

		recursive module function new_parser(str, src_file, contexts, unit_) result(parser)
			character(len = *), intent(in) :: str, src_file
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

end module syntran__parser_m

!===============================================================================

