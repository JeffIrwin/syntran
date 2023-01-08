
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

function err_bad_int(span, text) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: text
	err = underline(span)//err_prefix &
		//'invalid i32 integer '//text//color_reset

end function err_bad_int

!===============================================================================

function err_unexpected_char(span, text) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: text
	err = underline(span)//err_prefix &
		//"unexpected character '"//text//"'"//color_reset

end function err_unexpected_char

!===============================================================================

function err_unexpected_token(span, got, kind, expect) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: got, kind ,expect
	err = underline(span)//err_prefix &
		//'unexpected token "'//got//'" of kind <'//kind &
		//'>, expected <'//expect//'>'//color_reset

end function err_unexpected_token

!===============================================================================

function err_redeclare_var(span, text) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: text
	err = underline(span)//err_prefix &
		//'variable "'//text//'" has already been declared'//color_reset

end function err_redeclare_var

!===============================================================================

function err_undeclare_var(span, text) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: text
	err = underline(span)//err_prefix &
		//'variable "'//text//'" has not been declared'//color_reset

end function err_undeclare_var

!===============================================================================

function err_binary_types(text, lines, span, op, left, right) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	character(len = *), intent(in) :: text

	character(len = *), intent(in) :: op, left, right
	integer, allocatable :: lines(:)

	print *, 'starting err_binary_types'
	print *, 

	err = underline(span, text, lines)//err_prefix &
		//'binary operator "'//op//'" is not defined for types ' &
		//left//' and '//right//color_reset

end function err_binary_types

!===============================================================================

function err_unary_types(span, op, right) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, right
	err = underline(span)//err_prefix &
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

function underline(span, text, lines)

	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: underline

	! TODO: make not optional after modifying all err_*() fns, put in order
	! (text, span)
	character(len = *), intent(in), optional :: text
	integer, allocatable, optional :: lines(:)

	integer :: i1(1), i

	underline = ''

	if (present(text)) then

	! Get line number.  Ideally use a binary search, but it's so nice to do
	! something with a Fortran intrinsic for a change
	i1 = maxloc(lines, lines < span%start)
	i = i1(1)

	print *, 'line # = ', i

	! TODO: this doesn't work if the line is indented with tabs, because it
	! depends on how wide the console displays a tab!

	underline = text(lines(i): lines(i+1) - 1) &
		//repeat(' ', span%start - lines(i)) &
		//fg_bright_red//repeat('^', span%length) &
		//color_reset//line_feed

	!underline = text(lines(i): lines(i+1) - 2)//line_feed

	end if

	return
	! TODO

	underline = repeat(' ', span%start - 1) &
		//fg_bright_red//repeat('^', span%length) &
		//color_reset//line_feed

end function underline

!===============================================================================

end module errors_m

!===============================================================================

