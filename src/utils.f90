
!===============================================================================

module syntran__utils_m

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
			FG_BOLD_            = esc//'[;1m', &
			FG_BRIGHT_RED_      = esc//'[91m', &
			FG_BOLD_BRIGHT_RED_ = esc//'[91;1m', &
			FG_BRIGHT_GREEN_    = esc//'[92m', &
			FG_GREEN_           = esc//'[32m', &
			FG_BRIGHT_BLUE_     = esc//'[94m', &
			FG_BRIGHT_MAGENTA_  = esc//'[95m', &
			FG_BRIGHT_CYAN_     = esc//'[96m', &
			FG_BRIGHT_WHITE_    = esc//'[97m', &
			FG_BOLD_YELLOW_     = esc//'[1;33m', &
			COLOR_RESET_        = esc//'[0m'

	! Either copies of the above or empty strings, depending on `--color` arg
	character(len = :), allocatable :: &
			fg_bold, &
			fg_bright_red, &
			fg_bold_bright_red, &
			fg_bright_green, &
			fg_green, &
			fg_bright_blue, &
			fg_bright_magenta, &
			fg_bright_cyan, &
			fg_bright_white, &
			fg_bold_yellow, &
			color_reset

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

	!****

	type :: map_i32_entry_t
		character(:), allocatable :: key
		integer :: value
	end type map_i32_entry_t

	type :: map_i32_t
		type(map_i32_entry_t), allocatable :: table(:)
		integer :: capacity = 0
		integer :: count = 0
		real :: load_factor_threshold = 0.75
		contains
			procedure :: init      => map_i32_init
			procedure :: set       => map_i32_set
			procedure :: get       => map_i32_get
			procedure :: contains  => map_i32_contains
			procedure :: destroy   => map_i32_destroy
			procedure, private :: resize => map_i32_resize
	end type map_i32_t

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

	type i64_vector_t
		integer(kind = 8), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_i64
	end type i64_vector_t

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

function new_i64_vector() result(vector)

	type(i64_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end function new_i64_vector

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

subroutine push_i64(vector, val)

	class(i64_vector_t) :: vector

	integer(kind = 8), intent(in) :: val

	!********

	integer(kind = 8), allocatable :: tmp(:)

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

end subroutine push_i64

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

function trim_char_vector(sb) result(str_)

	class(char_vector_t), intent(in) :: sb
	character(len = :), allocatable :: str_

	str_ = sb%v(1: sb%len_)

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

function new_string_view(str_) result(view)

	character(len = *), intent(in) :: str_
	type(string_view_t) :: view

	view%s = str_
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

function read_line(iu, iostat) result(str_)

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too

	integer, intent(in) :: iu
	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str_

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
	str_ = sb%trim()

	if (present(iostat)) iostat = io

end function read_line

!===============================================================================

logical function exists(filename)
	character(len = *), intent(in) :: filename
	inquire(file = filename, exist = exists)
end function exists

!===============================================================================

function read_file(file, iostat) result(str_)

	! Read all lines of a file into str_

	character(len = *), intent(in) :: file

	integer, optional, intent(out) :: iostat

	character(len = :), allocatable :: str_

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
	str_ = sb%trim()

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

logical function is_abs_path(path)
	! Check if a path is absolute (starts with / on Unix or drive letter on Windows)

	character(len = *), intent(in) :: path

	is_abs_path = .false.
	if (len(path) == 0) return

	! Unix absolute path
	if (path(1:1) == '/') then
		is_abs_path = .true.
		return
	end if

	! Windows absolute path on current drive (e.g., \foo\bar)
	if (path(1:1) == '\') then
		is_abs_path = .true.
		return
	end if

	! Windows absolute path with drive letter (e.g., C:\, D:\)
	if (len(path) >= 2) then
		if (path(2:2) == ':') then
			is_abs_path = .true.
			return
		end if
	end if

end function is_abs_path

!===============================================================================

function resolve_path(src_dir, path) result(resolved)

	! Resolve a path relative to src_dir
	! If path is absolute, return it as-is
	! If path is relative and src_dir is non-empty, prepend src_dir

	character(len = *), intent(in) :: src_dir, path
	character(len = :), allocatable :: resolved

	if (is_abs_path(path)) then
		resolved = path
	else if (len(src_dir) > 0) then
		resolved = src_dir // path
	else
		resolved = path
	end if

end function resolve_path

!===============================================================================

logical function is_digit(c)

	character, intent(in) :: c

	is_digit = '0' <= c .and. c <= '9'

end function is_digit

!===============================================================================

logical function is_digit_under(c)

	character, intent(in) :: c

	is_digit_under = is_digit(c) .or. c == "_"

end function is_digit_under

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

logical function is_hex_under(c)
	! This is only applicable to hex digits, not the "0x" prefix

	character, intent(in) :: c

	is_hex_under = is_hex(c) .or. c == "_"

end function is_hex_under

!===============================================================================

logical function is_oct(c)
	! This is only applicable to octal digits, not the "0o" prefix

	character, intent(in) :: c

	is_oct = '0' <= c .and. c <= '7'

end function is_oct

!===============================================================================

logical function is_oct_under(c)
	character, intent(in) :: c

	is_oct_under = is_oct(c) .or. c == "_"

end function is_oct_under

!===============================================================================

logical function is_bin(c)
	! This is only applicable to binary digits, not the "0b" prefix

	character, intent(in) :: c

	is_bin = '0' <= c .and. c <= '1'

end function is_bin

!===============================================================================

logical function is_bin_under(c)
	character, intent(in) :: c

	is_bin_under = is_bin(c) .or. c == "_"

end function is_bin_under

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

logical function is_float_under(c)

	character, intent(in) :: c

	is_float_under = is_float(c) .or. c == "_"

end function is_float_under

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

logical function is_alphanum_under(c)

	character, intent(in) :: c

	is_alphanum_under = is_letter(c) .or. is_digit(c) .or. c == "_"

end function is_alphanum_under

!===============================================================================

logical function is_whitespace(c)

	character, intent(in) :: c

	is_whitespace = any(c == [tab, line_feed, vert_tab, carriage_return, ' '])

end function is_whitespace

!===============================================================================

function rm_char(str_, char) result(str_out)

	! Remove all occurences of character `char` from `str_`

	character(len = *), intent(in) :: str_
	character, intent(in) :: char

	character(len = :), allocatable :: str_out

	!********

	integer :: i
	type(char_vector_t) :: sb  ! string builder

	sb = new_char_vector()
	do i = 1, len(str_)
		if (str_(i:i) /= char) call sb%push(str_(i:i))
	end do
	str_out = sb%trim()

end function rm_char

!===============================================================================

function replace_all(str_, old, new) result(str_out)

	! Replace all occurrences of substring `old` with `new` in `str_`

	character(len = *), intent(in) :: str_, old, new

	character(len = :), allocatable :: str_out

	!********

	integer :: pos, start
	type(char_vector_t) :: sb  ! string builder

	if (len(old) == 0) then
		str_out = str_
		return
	end if

	sb = new_char_vector()
	start = 1
	pos = index(str_(start:), old)
	do while (pos > 0)
		! Push everything before the match
		call sb%push(str_(start: start + pos - 2))
		! Push the replacement
		call sb%push(new)
		! Move past the matched substring
		start = start + pos - 1 + len(old)
		! Find next match
		pos = index(str_(start:), old)
	end do
	! Push the remaining part
	call sb%push(str_(start:))
	str_out = sb%trim()

end function replace_all

!===============================================================================

function tabs2spaces(str_) result(str_out)

	! Replace each tab with a *single* space.  This is useful for alignment and
	! it makes allocation easy

	character(len = *), intent(in)  :: str_
	character(len = :), allocatable :: str_out

	integer :: i

	allocate(character(len = len(str_)) :: str_out)
	do i = 1, len(str_)
		if (str_(i:i) == tab) then
			str_out(i:i) = ' '
		else
			str_out(i:i) = str_(i:i)
		end if
	end do

end function tabs2spaces

!===============================================================================

function trimw(str_)

	! Trim whitespace, because the intrinsic trim() fn apparently doesn't trim
	! line breaks!?

	character(len = *), intent(in)  :: str_
	character(len = :), allocatable :: trimw

	integer :: first, last

	first = 1
	do
		if (first > len(str_)) exit
		if (.not. is_whitespace(str_(first: first))) exit
		first = first + 1
	end do

	last = len(str_)
	do
		if (last < first) exit
		if (.not. is_whitespace(str_(last: last))) exit
		last = last - 1
	end do

	trimw = str_(first: last)

end function trimw

!===============================================================================

function quote(str_) result(wrapped)

	! Wrap a str_ in "double quotes".  Any quotes already contained are not
	! escaped

	character(len = *), intent(in)  :: str_
	character(len = :), allocatable :: wrapped

	wrapped = '"'//str_//'"'

end function quote

!===============================================================================

logical function is_str_eq(a, b)
	! Fortran considers spaces as insignificant in str comparisons, but no sane
	! language would allow that
	!
	! I guess this is an artifact of fixed-length strings being common in older
	! fortran code
	!
	! Note that `is_ne()` is implemented as `.not. is_eq()`, which calls this
	! fn, so there is no need for a separate is_str_ne()

	character(len = *), intent(in) :: a, b

	!is_str_eq = a == b  ! not what you expect!

	is_str_eq = &
		len(a) == len(b) .and. &
		    a  ==     b

end function is_str_eq

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

function i32_str(x) result(str_)

	integer(kind = 4), intent(in) :: x

	character(len = :), allocatable :: str_

	! Fine for default 4-byte ints.  May need more chars for bigger ints
	character(len = 16) :: buffer

	write(buffer, '(i0)') x
	str_ = trim(buffer)

end function i32_str

!===============================================================================

function i32_vec_str(x) result(str_)

	integer(kind = 4), intent(in) :: x(:)

	character(len = :), allocatable :: str_

	integer :: i
	str_ = "["
	do i = 1, size(x)
		str_ = str_//i32_str(x(i))
		if (i < size(x)) str_ = str_//", "
	end do
	str_ = str_//"]"

end function i32_vec_str

!===============================================================================

function i64_str(x) result(str_)

	integer(kind = 8), intent(in) :: x

	character(len = :), allocatable :: str_

	! I think ~20 chars is the max actually, but let's round up to the next
	! multiple of 8
	character(len = 24) :: buffer

	write(buffer, '(i0)') x
	str_ = trim(buffer)

end function i64_str

!===============================================================================

function f32_str(x) result(str_)

	real, intent(in) :: x

	character(len = :), allocatable :: str_

	! Fine for default 4-byte type
	character(len = 16) :: buffer

	write(buffer, '(es16.6)') x
	str_ = trim(adjustl(buffer))

end function f32_str

!===============================================================================

function f64_str(x) result(str_)

	real(kind = 8), intent(in) :: x

	character(len = :), allocatable :: str_

	character(len = 28) :: buffer

	write(buffer, '(es25.15)') x
	str_ = trim(adjustl(buffer))

end function f64_str

!===============================================================================

function bool1_str(x) result(str_)

	logical(kind = 1), intent(in) :: x

	character(len = :), allocatable :: str_

	if (x) then
		str_ = 'true'
	else
		str_ = 'false'
	end if

end function bool1_str

!===============================================================================

pure function fnv_1a(input, seed) result(hash)
	character(*), intent(in) :: input
	integer(int64), intent(in), optional :: seed
	integer(int64) :: hash

	integer :: i
	integer(int64), parameter :: FNV_OFFSET_32 = 2166136261_int64
	integer(int64), parameter :: FNV_PRIME_32  = 16777619_int64

	if (present(seed)) then
		hash = seed
	else
		hash = FNV_OFFSET_32
	end if

	do i = 1, len(input)
		hash = iand( ieor(hash, iachar(input(i:i), int64)) * FNV_PRIME_32, &
		             int(z'FFFFFFFF', int64) )
	end do

end function fnv_1a

!===============================================================================

subroutine map_i32_init(self, capacity)
	! Consider making a `new_map_i32()` fn instead of init subroutine,
	! consistent with syntran src style
	class(map_i32_t), intent(inout) :: self
	integer, intent(in) :: capacity

	if (capacity <= 0) then
		error stop "map_i32_init: capacity must be positive"
	end if

	self%capacity = capacity
	self%count = 0
	allocate(self%table(capacity))
end subroutine map_i32_init

!===============================================================================

subroutine map_i32_set(self, key, value)
	class(map_i32_t), intent(inout) :: self
	character(len=*), intent(in) :: key
	integer, intent(in) :: value
	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx

	! Auto-resize if load factor exceeds threshold
	if (real(self%count) / real(self%capacity) >= self%load_factor_threshold) then
		call self%resize()
	end if

	! FNV-1a hash
	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(self%capacity, int64)) + 1)

	! Linear probing
	do probe = 0, self%capacity - 1
		idx = modulo(hash_idx + probe - 1, self%capacity) + 1

		if (.not. allocated(self%table(idx)%key)) then
			! Empty slot - insert new entry
			self%table(idx)%key = key
			self%table(idx)%value = value
			self%count = self%count + 1
			return
		else if (is_str_eq(self%table(idx)%key, key)) then
			! Key exists - update value
			self%table(idx)%value = value
			return
		end if
	end do

	! Should never reach here if resize works correctly
	error stop "map_i32_set: table full despite resize"
end subroutine map_i32_set

!===============================================================================

function map_i32_get(self, key, value) result(found)
	! Consider making this fn return `value` instead of `found`
	class(map_i32_t), intent(in) :: self
	character(len=*), intent(in) :: key
	integer, intent(out) :: value
	logical :: found
	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx

	found = .false.
	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(self%capacity, int64)) + 1)

	do probe = 0, self%capacity - 1
		idx = modulo(hash_idx + probe - 1, self%capacity) + 1

		if (.not. allocated(self%table(idx)%key)) then
			return  ! Not found
		else if (is_str_eq(self%table(idx)%key, key)) then
			value = self%table(idx)%value
			found = .true.
			return
		end if
	end do
end function map_i32_get

!===============================================================================

function map_i32_contains(self, key) result(found)
	class(map_i32_t), intent(in) :: self
	character(len=*), intent(in) :: key
	logical :: found
	integer(int64) :: hash_val
	integer :: hash_idx, probe, idx

	found = .false.
	hash_val = fnv_1a(key)
	hash_idx = int(modulo(hash_val, int(self%capacity, int64)) + 1)

	do probe = 0, self%capacity - 1
		idx = modulo(hash_idx + probe - 1, self%capacity) + 1

		if (.not. allocated(self%table(idx)%key)) then
			return  ! Not found
		else if (is_str_eq(self%table(idx)%key, key)) then
			found = .true.
			return
		end if
	end do
end function map_i32_contains

!===============================================================================

!===============================================================================

subroutine map_i32_destroy(self)
	class(map_i32_t), intent(inout) :: self
	if (allocated(self%table)) deallocate(self%table)
	self%capacity = 0
	self%count = 0
end subroutine map_i32_destroy

!===============================================================================

subroutine map_i32_resize(self)
	class(map_i32_t), intent(inout) :: self
	type(map_i32_entry_t), allocatable :: old_table(:)
	integer :: old_capacity, i, new_capacity

	! Save old table
	old_capacity = self%capacity
	call move_alloc(self%table, old_table)

	! Allocate new table with double capacity
	new_capacity = old_capacity * 2
	self%capacity = new_capacity
	self%count = 0
	allocate(self%table(new_capacity))

	! Rehash all entries from old table
	do i = 1, old_capacity
		if (allocated(old_table(i)%key)) then
			call self%set(old_table(i)%key, old_table(i)%value)
		end if
	end do

	! Old table automatically deallocated
end subroutine map_i32_resize

!===============================================================================

function to_lower(s) result(lower)

	! Return a copy of string `s` with ASCII upper-case letters converted to
	! lower-case.  Used for case-insensitive Levenshtein matching.

	character(len = *), intent(in) :: s
	character(len = :), allocatable :: lower

	!********

	integer :: i, ic

	lower = s
	do i = 1, len(lower)
		ic = iachar(lower(i:i))
		if (ic >= iachar('A') .and. ic <= iachar('Z')) then
			lower(i:i) = achar(ic + 32)
		end if
	end do

end function to_lower

!===============================================================================

function overload_display_name(name) result(display)

	! Translate an internal overloaded-intrinsic key (starts with "0") to its
	! user-facing name for spellcheck suggestions.  Names not starting with "0"
	! are returned unchanged.
	!
	! Internal names follow the pattern:  0<base>[_<kind>][_<rank>]
	! where <kind> is one of: i32, i64, f32, f64
	! and  <rank>  is one of: arr, sca
	!
	! Examples:
	!   "0tan_f32"     -> "tan"
	!   "0abs_f64_arr" -> "abs"
	!   "0i32_sca"     -> "i32"
	!   "0log2_f32"    -> "log2"
	!   "println"      -> "println"  (pass-through)

	character(len = *), intent(in) :: name
	character(len = :), allocatable :: display

	!********

	integer :: n

	if (len(name) < 1 .or. name(1:1) /= "0") then
		display = name
		return
	end if

	! Strip the leading "0"
	display = name(2:)

	! Strip trailing rank tag: _arr, _sca
	n = len(display)
	if (n > 4 .and. display(n-3:n) == "_arr") then
		display = display(1:n-4)
	else if (n > 4 .and. display(n-3:n) == "_sca") then
		display = display(1:n-4)
	end if

	! Strip trailing kind tag: _i32, _i64, _f32, _f64
	n = len(display)
	if (n > 4 .and. (display(n-3:n) == "_i32" .or. display(n-3:n) == "_i64" .or. &
	                 display(n-3:n) == "_f32" .or. display(n-3:n) == "_f64")) then
		display = display(1:n-4)
	end if

end function overload_display_name

!===============================================================================

integer function levenshtein(s, t)

	! Get the Levenshtein edit distance between strings `s` and `t`.
	! Adapted from the two-row DP implementation in ~/git/jsonf/src/jsonf.F90.

	character(len = *), intent(in) :: s, t

	!********

	integer :: m, n, i, j, deletion_cost, insertion_cost, substitution_cost
	integer, allocatable :: v0(:), v1(:), tmp(:)

	m = len(s)
	n = len(t)

	allocate(v0(n + 1))
	allocate(v1(n + 1))
	do j = 0, n
		v0(j + 1) = j
	end do

	do i = 1, m
		v1(1) = i
		do j = 1, n
			deletion_cost     = v0(j + 1) + 1
			insertion_cost    = v1(j) + 1
			if (s(i:i) == t(j:j)) then
				substitution_cost = v0(j)
			else
				substitution_cost = v0(j) + 1
			end if
			v1(j + 1) = min(deletion_cost, insertion_cost, substitution_cost)
		end do

		! Swap v0 and v1
		call move_alloc(v0, tmp)
		call move_alloc(v1, v0)
		call move_alloc(tmp, v1)
	end do

	levenshtein = v0(n + 1)

end function levenshtein

!===============================================================================

end module syntran__utils_m

!===============================================================================

