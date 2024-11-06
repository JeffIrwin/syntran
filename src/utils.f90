
!===============================================================================

module syntran__utils_m

	use iso_fortran_env
	!use iso_c_binding

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
			fg_bright_blue     = esc//'[94m', &
			fg_bright_magenta  = esc//'[95m', &
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
		integer :: len_, cap
		contains
			procedure :: push     => push_string
			procedure :: push_all => push_all_string
	end type string_vector_t

	!********

	type char_vector_t
		! This is more of a "string builder".  c.f. ribbit
		character(len = :), allocatable :: v
		integer :: len_, cap
		contains
			procedure :: &
				push => push_char, &
				trim => trim_char_vector
	end type char_vector_t

	!********

	type integer_vector_t
		integer, allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push     => push_integer
	end type integer_vector_t

	!********

	type logical_vector_t
		logical(kind = 1), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push     => push_logical
	end type logical_vector_t

	!********

	interface str
		module procedure  i32_str
		module procedure  i32_vec_str
		module procedure  i64_str
		module procedure  f32_str
		module procedure  f64_str
		module procedure bool1_str
	end interface str

	!********

!	! I don't understand the magic of how these work, or if they work at all
!	! for executables in the PATH env var or for shell aliases.  If so, it may
!	! be useful if/when we want to have system standard include files loaded
!	! relative to the syntran exe
!
!	interface
!		! Ref: https://fortran-lang.discourse.group/t/getting-a-full-path-name/4137/12
!#if defined _WIN32
!        function fullpath_c(resolved_path, path, maxLength) result(ptr) bind(C, name="_fullpath")
!           import :: c_ptr, c_char, c_int
!           character(kind=c_char, len=1), intent(out) :: resolved_path(*)
!           character(kind=c_char, len=1), intent(in) :: path(*)
!           integer(c_int), value, intent(in) :: maxLength
!           type(c_ptr) :: ptr
!        end function
!#else
!        function realpath_c(path, resolved_path) result(ptr) bind(C, name="realpath")
!           import :: c_ptr, c_char
!           character(kind=c_char, len=1), intent(in) :: path(*)
!           character(kind=c_char, len=1), intent(out) :: resolved_path(*)
!           type(c_ptr) :: ptr
!        end function
!#endif
!	end interface

!===============================================================================

contains

!===============================================================================

function new_integer_vector() result(vector)

	type(integer_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_integer_vector

!===============================================================================

subroutine push_integer(vector, val)

	class(integer_vector_t) :: vector

	integer, intent(in) :: val

	!********

	integer, allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_integer

!===============================================================================

function new_logical_vector() result(vector)

	type(logical_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_logical_vector

!===============================================================================

subroutine push_logical(vector, val)

	class(logical_vector_t) :: vector

	logical, intent(in) :: val

	!********

	logical(kind = 1), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_logical

!===============================================================================

function new_string_vector() result(vector)

	type(string_vector_t) :: vector

	vector%len_ = 0
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

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	val_str%s = val
	vector%v( vector%len_ ) = val_str

end subroutine push_string

!===============================================================================

function new_char_vector(cap) result(vector)

	integer, intent(in), optional :: cap

	type(char_vector_t) :: vector

	vector%len_ = 0
	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2
	end if

	!allocate(vector%v( vector%cap ))
	allocate(character(len = vector%cap) :: vector%v)

end function new_char_vector

!===============================================================================

subroutine push_char(vector, val)

	class(char_vector_t) :: vector

	character(len = *), intent(in) :: val

	!********

	character(len = :), allocatable :: tmp

	integer :: tmp_cap

	vector%len_ = vector%len_ + len(val)

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(character(len = tmp_cap) :: tmp)
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ - len(val) + 1: vector%len_ ) = val

end subroutine push_char

!===============================================================================

function trim_char_vector(sb) result(str)

	class(char_vector_t), intent(in) :: sb
	character(len = :), allocatable :: str

	str = sb%v(1: sb%len_)

end function trim_char_vector

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

	do i = 1, add%len_
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

	io = exit_success
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

	integer :: io

	type(char_vector_t) :: sb  ! string builder

	!print *, 'starting read_line()'

	! Read 1 character at a time until end
	sb = new_char_vector()
	do
		read(iu, '(a)', advance = 'no', iostat = io) c

		if (io == iostat_end) exit
		if (io == iostat_eor) exit

		! In syntran, calling readln() one more time after the initial EOF
		! causes an infinite loop for some reason without this
		if (io /= 0) exit

		!if (c == carriage_return) exit
		!if (c == line_feed) exit

		call sb%push(c)

	end do
	str = sb%trim()

	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

logical function exists(filename)
	character(len = *), intent(in) :: filename
	inquire(file = filename, exist = exists)
end function exists

!===============================================================================

function read_file(file, iostat) result(str)

	! Read all lines of a file into str

	character(len = *), intent(in) :: file

	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str

	!********

	character :: c

	integer :: io, iu

	type(char_vector_t) :: sb  ! string builder

	open(file = file, newunit = iu, status = 'old', iostat = io)
	if (io /= exit_success) then
		if (present(iostat)) iostat = io
		return
	end if

	! Read 1 character at a time until end
	sb = new_char_vector()
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) then
			io = exit_success
			exit
		end if

		!if (io == iostat_eor) exit
		if (io == iostat_eor) c = line_feed

		call sb%push(c)

	end do
	close(iu)
	str = sb%trim()

	!print *, 'str = '
	!print *, str

	if (present(iostat)) iostat = io

end function read_file

!===============================================================================

!function fullpath(path) result(resolved_path)
!	! Ref: https://fortran-lang.discourse.group/t/getting-a-full-path-name/4137/12
!        character(*), intent(in) :: path
!        character(:), allocatable :: resolved_path
!        !private
!        type(c_ptr) :: ptr
!        character(1) :: tmp(1024)
!        integer :: idx
!
!        allocate(character(1024) :: resolved_path)
!!#ifndef _WIN32
!#if defined _WIN32
!        ptr = fullpath_c(tmp, path // null_char, 1024)
!#else
!        ptr = realpath_c(path // null_char, tmp)
!#endif
!        resolved_path = transfer(tmp, resolved_path)
!        idx = index(resolved_path, null_char)
!        resolved_path = resolved_path(:idx - 1)
!
!end function fullpath

!===============================================================================

!function get_basename(filename) result(basename)
!	! c.f. github.com/jeffirwin/cali/src/cali.f90
!	!
!	! not used (yet) in syntran
!	character(len = *), intent(in)  :: filename
!	character(len = :), allocatable :: basename
!	!********
!	integer :: beg_, end_, i
!
!	beg_ = 1
!	end_ = len(filename)
!
!	!print *, 'len = ', end_
!
!	i = scan(filename, '/\', .true.)
!	if (i /= 0) beg_ = i + 1
!
!	i = scan(filename(beg_:), '.')
!	if (i /= 0) end_ = beg_ + i - 2
!
!	basename = filename(beg_: end_)
!	!print *, 'beg_, end_ = ', beg_, end_
!
!end function get_basename

!===============================================================================

function get_base_with_ext(filename) result(basename)
	! Get basename plus extension
	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: basename
	!********
	integer :: beg_, end_, i

	beg_ = 1
	end_ = len(filename)

	!print *, 'len = ', end_

	i = scan(filename, '/\', .true.)
	if (i /= 0) beg_ = i + 1

	basename = filename(beg_: end_)
	!print *, 'beg_, end_ = ', beg_, end_

end function get_base_with_ext

!===============================================================================

function get_dir(filename) result(dir)
	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: dir
	!********
	character(len = :), allocatable :: path
	integer :: beg_, end_, i

	!! Return the absolute path dir
	!path = fullpath(filename)

	! Return relative path or absolute, whichever way input filename is given
	path = filename

	beg_ = 1
	!end_ = len(path)
	end_ = 0

	!print *, 'len = ', end_

	i = scan(path, '/\', .true.)
	if (i /= 0) end_ = i

	dir = path(beg_: end_)
	!print *, 'beg_, end_ = ', beg_, end_

end function get_dir

!===============================================================================

logical function is_digit(c)

	character, intent(in) :: c

	is_digit = '0' <= c .and. c <= '9'

end function is_digit

!===============================================================================

logical function is_hex(c)
	! This is only applicable to hex digits, not the "0x" prefix

	character, intent(in) :: c

	is_hex = &
		is_digit(c) .or. &
		('a' <= c .and. c <= 'f') .or. &
		('A' <= c .and. c <= 'F')

end function is_hex

!===============================================================================

logical function is_sign(c)

	character, intent(in) :: c

	is_sign = c == '+' .or. c == '-'

end function is_sign

!===============================================================================

logical function is_expo(c)

	character, intent(in) :: c

	is_expo = c == 'd' .or. c == 'e' .or. c == 'D' .or. c == 'E'

end function is_expo

!===============================================================================

logical function is_float(c)

	character, intent(in) :: c

	! Correctly tokenizing a float is actually tricky.  We can't just greedily
	! gobble up all the characters that match is_float().  We need to tokenize
	! this as a float:
	!
	!     1.234e+1
	!
	! But tokenize this as a binary expression adding two ints:
	!
	!     1+234
	!
	! The + or - can only appear immediately after d or e.  To complicate
	! matters, there could also be a variable identifier named "e".
	!
	! To correctly tokenize floats, the lexer uses is_float(), in conjunction
	! with is_sign() and is_expo() to ensure that sign characters within a float
	! token ONLY occur immediately after an exponent character. Note that sign
	! characters before a number are tokenized as a separate unary operator, not
	! as part of the number token.

	is_float = is_digit(c) .or. is_sign(c) .or. is_expo(c) .or. c == '.'

end function is_float

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

function tabs2spaces(str) result(str_out)

	! Replace each tab with a *single* space.  This is useful for alignment and
	! it makes allocation easy

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: str_out

	integer :: i

	allocate(character(len = len(str)) :: str_out)
	do i = 1, len(str)
		if (str(i:i) == tab) then
			str_out(i:i) = ' '
		else
			str_out(i:i) = str(i:i)
		end if
	end do

end function tabs2spaces

!===============================================================================

function trimw(str)

	! Trim whitespace, because the intrinsic trim() fn apparently doesn't trim
	! line breaks!?

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: trimw

	integer :: first, last

	first = 1
	do
		if (first > len(str)) exit
		if (.not. is_whitespace(str(first: first))) exit
		first = first + 1
	end do

	last = len(str)
	do
		if (last < first) exit
		if (.not. is_whitespace(str(last: last))) exit
		last = last - 1
	end do

	trimw = str(first: last)

end function trimw

!===============================================================================

function quote(str) result(wrapped)

	! Wrap a str in "double quotes".  Any quotes already contained are not
	! escaped

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: wrapped

	wrapped = '"'//str//'"'

end function quote

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

function i32_str(x) result(str)

	integer(kind = 4), intent(in) :: x

	character(len = :), allocatable :: str

	! Fine for default 4-byte ints.  May need more chars for bigger ints
	character(len = 16) :: buffer

	write(buffer, '(i0)') x
	str = trim(buffer)

end function i32_str

!===============================================================================

function i32_vec_str(x) result(str)

	integer(kind = 4), intent(in) :: x(:)

	character(len = :), allocatable :: str

	integer :: i
	str = "["
	do i = 1, size(x)
		str = str//i32_str(x(i))
		if (i < size(x)) str = str//", "
	end do
	str = str//"]"

end function i32_vec_str

!===============================================================================

function i64_str(x) result(str)

	integer(kind = 8), intent(in) :: x

	character(len = :), allocatable :: str

	! I think ~20 chars is the max actually, but let's round up to the next
	! multiple of 8
	character(len = 24) :: buffer

	write(buffer, '(i0)') x
	str = trim(buffer)

end function i64_str

!===============================================================================

function f32_str(x) result(str)

	real, intent(in) :: x

	character(len = :), allocatable :: str

	! Fine for default 4-byte type
	character(len = 16) :: buffer

	write(buffer, '(es16.6)') x
	str = trim(adjustl(buffer))

end function f32_str

!===============================================================================

function f64_str(x) result(str)

	real(kind = 8), intent(in) :: x

	character(len = :), allocatable :: str

	character(len = 28) :: buffer

	write(buffer, '(es25.15)') x
	str = trim(adjustl(buffer))

end function f64_str

!===============================================================================

function bool1_str(x) result(str)

	logical(kind = 1), intent(in) :: x

	character(len = :), allocatable :: str

	if (x) then
		str = 'true'
	else
		str = 'false'
	end if

end function bool1_str

!===============================================================================

end module syntran__utils_m

!===============================================================================

