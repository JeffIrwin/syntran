
!===============================================================================

module utils

	use iso_fortran_env
	implicit none

	integer, parameter :: exit_success = 0, exit_failure = -1

	character, parameter :: &
			null_char       = char( 0), &
			tab             = char( 9), &
			line_feed       = char(10), &
			vert_tab        = char(11), &
			carriage_return = char(13), &
			esc             = char(27)

	! ANSI escape color codes
	!
	!     https://stackoverflow.com/a/54062826/4347028
	!
	! TODO: more colors
	character(len = *), parameter :: &
			fg_bold            = esc//'[;1m', &
			fg_bright_red      = esc//'[91m', &
			fg_bold_bright_red = esc//'[91;1m', &
			fg_bright_green    = esc//'[92m', &
			fg_bright_cyan     = esc//'[96m', &
			fg_bright_white    = esc//'[97m', &
			color_reset        = esc//'[0m'

	!********

	type string_t
		character(len = :), allocatable :: s
	end type string_t

	!********

	type string_view_t
		character(len = :), allocatable :: s
		integer :: pos
		contains
			procedure :: get_line => string_view_get_line
	end type string_view_t

	!********

	type string_vector_t
		type(string_t), allocatable :: v(:)
		integer :: len, cap
		contains
			procedure :: push     => push_string
			procedure :: push_all => push_all_string
	end type string_vector_t

	interface str
		module procedure int_str
	end interface str

!===============================================================================

contains

!===============================================================================

function new_string_vector() result(vector)

	type(string_vector_t) :: vector

	vector%len = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_string_vector

!===============================================================================

subroutine push_string(vector, val)

	class(string_vector_t) :: vector

	character(len = *), intent(in) :: val

	!********

	type(string_t) :: val_str
	type(string_t), allocatable :: tmp(:)

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

	val_str%s = val
	vector%v( vector%len ) = val_str

end subroutine push_string

!===============================================================================

subroutine push_all_string(vector, add)

	! Push all elements of add into vector
	!
	! This currently isn't used, since it's easier to just copy the whole vector
	! type to an initially empty type

	class(string_vector_t) :: vector

	type(string_vector_t), intent(in) :: add

	!********

	integer :: i

	do i = 1, add%len
		call vector%push( add%v(i)%s )
	end do

end subroutine push_all_string

!===============================================================================

function new_string_view(str) result(view)

	character(len = *), intent(in) :: str
	type(string_view_t) :: view

	view%s = str
	view%pos = 1

end function new_string_view

!===============================================================================

function string_view_get_line(sv, iostat) result(line)

	class(string_view_t), intent(inout) :: sv
	character(len = :), allocatable :: line
	integer, optional, intent(out) :: iostat

	!********

	integer :: length, io

	io = 0
	length = scan(sv%s( sv%pos: ), line_feed//carriage_return)
	if (length <= 0) then
		io = iostat_end
	end if

	!print *, 'string_view_get_line'
	!print *, 'pos, len = ', sv%pos, length

	line = sv%s( sv%pos: sv%pos + length  - 1 )
	sv%pos = sv%pos + length

	if (present(iostat)) iostat = io

end function string_view_get_line

!===============================================================================

function read_line(iu, iostat) result(str)

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c
	character(len = :), allocatable :: tmp

	integer :: i, io, str_cap, tmp_cap

	! Buffer string with some initial length
	str_cap = 64
	allocate(character(len = str_cap) :: str)

	! Read 1 character at a time until end
	i = 0
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit
		if (io == iostat_eor) exit
		!if (c == carriage_return) exit
		!if (c == line_feed) exit
		i = i + 1

		if (i > str_cap) then
			!print *, 'growing str'

			! Grow the buffer capacity.  What is the optimal growth factor?
			tmp_cap = 2 * str_cap
			allocate(character(len = tmp_cap) :: tmp)
			tmp(1: str_cap) = str

			call move_alloc(tmp, str)
			str_cap = tmp_cap

			!print *, 'str_cap  = ', str_cap
			!print *, 'len(str) = ', len(str)

		end if
		str(i:i) = c

	end do

	! Trim unused chars from buffer
	str = str(1:i)

	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

function read_file(file, iostat) result(str)

	! Read all lines of a file into str

	character(len = *), intent(in) :: file

	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c
	character(len = :), allocatable :: tmp

	integer :: i, io, str_cap, tmp_cap, iu

	open(file = file, newunit = iu, status = 'old', iostat = io)
	if (io /= exit_success) return

	! Buffer string with some initial length
	str_cap = 64
	allocate(character(len = str_cap) :: str)

	! Read 1 character at a time until end
	i = 0
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit

		!if (io == iostat_eor) exit
		if (io == iostat_eor) c = line_feed

		i = i + 1

		if (i > str_cap) then
			!print *, 'growing str'

			! Grow the buffer capacity.  What is the optimal growth factor?
			tmp_cap = 2 * str_cap
			allocate(character(len = tmp_cap) :: tmp)
			tmp(1: str_cap) = str

			call move_alloc(tmp, str)
			str_cap = tmp_cap

			!print *, 'str_cap  = ', str_cap
			!print *, 'len(str) = ', len(str)

		end if
		str(i:i) = c

	end do
	close(iu)

	! Trim unused chars from buffer
	str = str(1:i)

	!print *, 'str = '
	!print *, str

	if (present(iostat)) iostat = io

end function read_file

!===============================================================================

logical function is_digit(c)

	character, intent(in) :: c

	is_digit = '0' <= c .and. c <= '9'

end function is_digit

!===============================================================================

logical function is_letter(c)

	character, intent(in) :: c

	is_letter = ('a' <= c .and. c <= 'z') .or. ('A' <= c .and. c <= 'Z')

end function is_letter

!===============================================================================

logical function is_alphanum(c)

	character, intent(in) :: c

	is_alphanum = is_letter(c) .or. is_digit(c)

end function is_alphanum

!===============================================================================

logical function is_whitespace(c)

	character, intent(in) :: c

	is_whitespace = any(c == [tab, line_feed, vert_tab, carriage_return, ' '])

end function is_whitespace

!===============================================================================

function findlocl1(arr, val) result(loc)

	! findloc() is standard in Fortran 2008, but gfortran 8.1.0 doesn't have it
	! yet :(.  Here I implement it for logical rank-1 arrays only without
	! optional args

	logical, intent(in) :: arr(:), val

	integer :: loc(1)

	if (size(arr) == 0) then
		loc = 0
		return
	end if

	loc = 1
	do while (arr(loc(1)) .neqv. val)
		loc(1) = loc(1) + 1

		if (loc(1) > size(arr, 1)) then
			! not found
			loc = 0
			return
		end if

	end do

end function findlocl1

!===============================================================================

! Colors work by default in bash and Windows terminal
!
! For color use in cmd or powershell, set:
!
!    [HKEY_CURRENT_USER\Console]
!    "VirtualTerminalLevel"=dword:00000001
!
! Ref:
!
!     https://superuser.com/a/1300251
!

subroutine console_color(color)
	character(len = *), intent(in) :: color
	write(*, '(a)', advance = 'no') color
end subroutine console_color

subroutine console_color_reset()
	write(*, '(a)', advance = 'no') color_reset
end subroutine console_color_reset

!===============================================================================

function int_str(i) result(str)

	integer, intent(in) :: i

	character(len = :), allocatable :: str

	! Fine for default 4-byte ints.  May need more chars for bigger ints
	character(len = 16) :: buffer

	write(buffer, '(i0)') i
	str = trim(buffer)

end function int_str

!===============================================================================

end module utils

!===============================================================================

