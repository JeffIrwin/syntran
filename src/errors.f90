
!===============================================================================

module errors_m

	use utils

	implicit none

	! This includes fg_bold at the end, so the rest of the error message after
	! the prefix must concatenate color_reset at its end
	character(len = *), parameter :: &
		err_prefix = fg_bold_bright_red//'Error'//color_reset//fg_bold//': '

	! A text span indicates which characters to underline in a faulty line of
	! code
	type text_span_t
		integer :: start, length
	end type text_span_t

!===============================================================================

contains

!===============================================================================

function err_bad_int(text, lines, span, num) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	! TODO: re-order error message *first* end the LOC with underline (and more
	! after the underline?)

	character(len = *), intent(in) :: num
	err = underline(text, lines, span)//err_prefix &
		//'invalid i32 integer '//num//color_reset

end function err_bad_int

!===============================================================================

function err_unexpected_char(text, lines, span, c) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: c
	err = underline(text, lines, span)//err_prefix &
		//"unexpected character '"//c//"'"//color_reset

end function err_unexpected_char

!===============================================================================

function err_unexpected_token(text, lines, span, got, kind, expect) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: got, kind ,expect
	err = underline(text, lines, span)//err_prefix &
		//'unexpected token "'//got//'" of kind <'//kind &
		//'>, expected <'//expect//'>'//color_reset

end function err_unexpected_token

!===============================================================================

function err_redeclare_var(text, lines, span, var) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = underline(text, lines, span)//err_prefix &
		//'variable "'//var//'" has already been declared'//color_reset

end function err_redeclare_var

!===============================================================================

function err_undeclare_var(text, lines, span, var) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = underline(text, lines, span)//err_prefix &
		//'variable "'//var//'" has not been declared'//color_reset

end function err_undeclare_var

!===============================================================================

function err_binary_types(text, lines, span, op, left, right) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, left, right

	!print *, 'starting err_binary_types'

	err = underline(text, lines, span)//err_prefix &
		//'binary operator "'//op//'" is not defined for types ' &
		//left//' and '//right//color_reset

end function err_binary_types

!===============================================================================

function err_unary_types(text, lines, span, op, right) result(err)
	character(len = *), intent(in) :: text
	integer, allocatable :: lines(:)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, right
	err = underline(text, lines, span)//err_prefix &
		//'unary operator "'//op//'" is not defined for type ' &
		//right//color_reset

end function err_unary_types

!===============================================================================

function new_text_span(start, length) result(span)

	integer :: start, length
	type(text_span_t) :: span

	span%start  = start
	span%length = length

end function new_text_span

!===============================================================================

function underline(text, lines, span)

	type(text_span_t), intent(in) :: span
	character(len = *), intent(in) :: text
	integer, allocatable, intent(in) :: lines(:)
	character(len = :), allocatable :: underline

	character(len = :), allocatable :: str_i, spaces, src_file, fg1, rst
	integer :: i1(1), i, str_i_len, start, last

	! Get line number.  Ideally use a binary search, but it's so nice to do
	! something with a Fortran intrinsic for a change
	i1 = maxloc(lines, lines < span%start)
	i = min(size(lines)-1, max(1, i1(1)))

	str_i = str(i)
	str_i_len = len(str_i)

	!print *, 'line # = ', i

	! TODO: this doesn't work if the line is indented with tabs, because it
	! depends on how wide the console displays a tab!
	!
	! To work around this, trim whitespace from beginning of line, count number
	! of trimmed characters, and then adjust the leading underline spaces

	! Pad spaces the same length as the line number string
	spaces = repeat(' ', str_i_len + 2)

	! First and last character indices of line
	start = lines(i)
	last  = lines(i+1) - 1

	! Trim whitespace from end of line.  Make sure interpretter looks ok when
	! forgetting semicolons
	do while (text(last:last) == line_feed .or. &
	          text(last:last) == carriage_return)
		last = last - 1
	end do

	! TODO: add filename, line number, and (start?) column too like rust:
	!
	!error[E0425]: cannot find value `string_number` in this scope
	!  --> src/main.rs:3:20
	!   |
	! 3 |    println!("{}", string_number);
	!   |                   ^^^^^^^^^^^^^ not found in this scope
	!

	! TODO: arg for src_file?  There's getting to be a lot of args.  Encapsulate
	! text, lines, and src_file name in a new "context" type (or
	! diagnostic_context if you're feeling verbose).  Set src_file to <stdin>
	! for interpretter

	src_file = "*.syntran"

	fg1 = fg_bright_cyan
	rst = color_reset

	underline = &
		  fg1//"  --> "//rst//src_file//":"//str_i//":"//str(span%start) &
		//line_feed &
		//fg1//     spaces//"| "//line_feed &
		//fg1//" "//str_i//" | "//rst//text(start: last)//line_feed &
		//fg1//     spaces//"| " &! //line_feed &
		//repeat(' ', span%start - lines(i)) &
		//fg_bright_red//repeat('^', span%length)//color_reset//line_feed

end function underline

!===============================================================================

end module errors_m

!===============================================================================

