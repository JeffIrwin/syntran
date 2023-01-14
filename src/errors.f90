
!===============================================================================

module errors_m

	use utils

	implicit none

	! This includes fg_bold at the end, so the rest of the error message after
	! the prefix must concatenate color_reset at its end
	character(len = *), parameter :: &
		err_prefix = fg_bold_bright_red//'Error'//fg_bold//': ', &
		err_int_prefix = fg_bold_bright_red//'Internal syntran error' &
			//fg_bold//': '

	! A text span indicates which characters to underline in a faulty line of
	! code
	type text_span_t
		integer :: start, length
	end type text_span_t

	! Span is different for each error.  Other things, like the src_file name,
	! text, and lines, stay the same (at least per parser invocation for now).
	! Those constants are in text_context_t, which is constructed within
	! new_parser() and new_lexer()
	type text_context_t

		! Text is the full text of the source code with filename src_file.  The
		! array lines(:) contains the character indices of the start of each
		! line
		character(len = :), allocatable :: text, src_file
		integer, allocatable :: lines(:)

	end type text_context_t

!===============================================================================

contains

!===============================================================================

function err_bad_int(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_prefix//'invalid i32 integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' invalid integer'//color_reset

end function err_bad_int

!===============================================================================

function err_unexpected_char(context, span, c) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: c
	err = err_prefix &
		//"unexpected character `"//c//"`"//underline(context, span) &
		//" unexpected character"//color_reset

end function err_unexpected_char

!===============================================================================

function err_unexpected_token(context, span, got, kind, expect) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: got, kind ,expect
	err = err_prefix &
		//'unexpected token `'//got//'` of kind `'//kind &
		//'`, expected `'//expect//'`'//underline(context, span) &
		//" unexpected token"//color_reset

end function err_unexpected_token

!===============================================================================

function err_redeclare_var(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_prefix &
		//'variable `'//var//'` has already been declared' &
		//underline(context, span)//" variable already declared"//color_reset

end function err_redeclare_var

!===============================================================================

function err_undeclare_var(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_prefix &
		//'variable `'//var//'` has not been declared' &
		//underline(context, span)//" variable undeclared"//color_reset

end function err_undeclare_var

!===============================================================================

function err_binary_types(context, span, op, left, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, left, right

	!print *, 'starting err_binary_types'

	err = err_prefix &
		//'binary operator `'//op//'` is not defined for types ' &
		//left//' and '//right//underline(context, span) &
		//" bad types for this binary operator"//color_reset

end function err_binary_types

!===============================================================================

function err_unary_types(context, span, op, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, right
	err = err_prefix &
		//'unary operator `'//op//'` is not defined for type ' &
		//right//underline(context, span) &
		//" bad type for this unary operator"//color_reset

end function err_unary_types

!===============================================================================

function err_non_bool_condition(context, span, condition, statement) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: condition, statement
	err = err_prefix &
		//'Condition `'//trimw(condition)//'` of '//statement//' is not bool' &
		//underline(context, span) &
		//" non-bool condition"//color_reset

end function err_non_bool_condition

!===============================================================================

function err_non_num_bound(context, span, bound) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: bound
	err = err_prefix &
		//'Bound `'//bound//'` of for-loop is not numeric' &
		//underline(context, span) &
		//" non-numeric bound"//color_reset

end function err_non_num_bound

!===============================================================================

function err_404(file) result(err)
	character(len = *), intent(in) :: file
	character(len = :), allocatable :: err
	err = err_prefix//'file `'//file//'` not found'//color_reset
end function err_404

!===============================================================================

function err_eval_binary_types(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'binary operator `'//op//'` is not defined for operand types ' &
		//color_reset

end function err_eval_binary_types

!===============================================================================

function err_eval_unary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected unary operator `'//op//'`'//color_reset

end function err_eval_unary_op

!===============================================================================

function err_eval_node(node_kind) result(err)
	character(len = *), intent(in) :: node_kind
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected node `'//node_kind//'`'//color_reset

end function err_eval_node

!===============================================================================

function err_eval_binary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_prefix &
		//'unexpected binary operator `'//op//'`'//color_reset

end function err_eval_binary_op

!===============================================================================

function new_span(start, length) result(span)

	integer, intent(in) :: start, length

	type(text_span_t) :: span

	span%start    = start
	span%length   = max(length, 1)

end function new_span

!===============================================================================

function new_context(text, src_file, lines) result(context)
	character(len = *), intent(in) :: text, src_file
	integer, intent(in) :: lines(:)
	type(text_context_t) :: context
	context%text = text
	context%src_file = src_file
	context%lines = lines
end function new_context

!===============================================================================

function underline(context, span)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: underline

	character(len = :), allocatable :: str_i, spaces, fg1, rst, col, text
	integer :: i1(1), i, j, str_i_len, start, last, length

	! Get line number.  Ideally use a binary search, but it's so nice to do
	! something with a Fortran intrinsic for a change
	i1 = maxloc(context%lines, context%lines <= span%start)
	i = min(size(context%lines)-1, max(1, i1(1)))

	str_i = str(i)
	str_i_len = len(str_i)

	!print *, 'line # = ', i

	! Pad spaces the same length as the line number string
	spaces = repeat(' ', str_i_len + 2)

	! First and last character indices of line
	start = context%lines(i)
	last  = context%lines(i+1) - 1

	! Trim whitespace from end of line.  Make sure interpreter looks ok with
	! errors at final character of line.  TODO: use is_whitespace()
	do while (context%text(last:last) == line_feed .or. &
	          context%text(last:last) == carriage_return)
		last = last - 1
	end do

	! Without substitution, this doesn't work if the line is indented with tabs,
	! because it depends on how wide the console displays a tab!
	text = tabs2spaces(context%text(start: last))

	! Length of error token(s) within the line
	length = span%length

	! Same idea as 'last' adjustment: trim whitespace from error tokens
	j = min(len(context%text), span%start + length - 1)
	!print *, 'char = "', context%text(j:j), '"'
	do while (is_whitespace(context%text(j:j)))
		length = length - 1
		j = min(len(context%text), span%start + length - 1)
	end do
	length = max(length, 1)

	! Here's an example of a rust error message, from which I'm stealing UX:
	!
	! """
	!
	!    Compiling skillet v0.4.0 (C:\git\skillet)
	! error[E0433]: failed to resolve: use of undeclared crate or module `st`
	!  --> src\main.rs:6:5
	!   |
	! 6 | use st::path::PathBuf;
	!   |     ^^ use of undeclared crate or module `st`
	!   |
	! help: there is a crate or module with a similar name
	!   |
	! 6 | use std::path::PathBuf;
	!   |     ~~~
	!
	! """

	col = str(span%start - context%lines(i) + 1)

	fg1 = fg_bright_cyan
	!fg1 = fg_bright_blue

	rst = color_reset

	underline = line_feed//fg1//spaces(2:)//"--> "//rst//context%src_file &
		//":"//str_i//":"//col//line_feed &
		//fg1//     spaces//"| "//line_feed &
		//fg1//" "//str_i//" | "//rst//text//line_feed &
		//fg1//     spaces//"| " &
		//repeat(' ', span%start - context%lines(i)) &
		//fg_bright_red//repeat('^', length)

end function underline

!===============================================================================

end module errors_m

!===============================================================================

