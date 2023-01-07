
!===============================================================================

module syntran_errors_m

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

function new_text_span(start, length) result(span)

	integer :: start, length
	type(text_span_t) :: span

	span%start  = start
	span%length = length

end function new_text_span

!===============================================================================

function underline(span)

	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: underline

	underline = repeat(' ', span%start-1) &
		//fg_bright_red//repeat('^', span%length) &
		//color_reset//line_feed

end function underline

!===============================================================================

function err_bad_int(span, text) result(err)
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: text
	err = underline(span)//err_prefix &
		//'invalid int32 '//text//color_reset

end function err_bad_int

!===============================================================================

end module syntran_errors_m

!===============================================================================

