
!===============================================================================

submodule (syntran__utils_m) syntran__utils_impl_m

	implicit none

contains


!===============================================================================

module procedure new_integer_vector


	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end procedure new_integer_vector


!===============================================================================

module procedure push_integer



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

end procedure push_integer


!===============================================================================

module procedure push_i64



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

end procedure push_i64


!===============================================================================

module procedure new_logical_vector


	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end procedure new_logical_vector


!===============================================================================

module procedure push_logical



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

end procedure push_logical


!===============================================================================

module procedure new_string_vector


	vector%len_ = 0
	vector%cap = 2

	allocate(vector%v( vector%cap ))

end procedure new_string_vector


!===============================================================================

module procedure push_string



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

end procedure push_string


!===============================================================================

module procedure new_char_vector



	vector%len_ = 0
	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2
	end if

	!allocate(vector%v( vector%cap ))
	allocate(character(len = vector%cap) :: vector%v)

end procedure new_char_vector


!===============================================================================

module procedure push_char



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

end procedure push_char


!===============================================================================

module procedure trim_char_vector


	str_ = sb%v(1: sb%len_)

end procedure trim_char_vector


!===============================================================================

module procedure push_all_string

	! Push all elements of add into vector
	!
	! This currently isn't used, since it's easier to just copy the whole vector
	! type to an initially empty type



	!********

	integer :: i

	do i = 1, add%len_
		call vector%push( add%v(i)%s )
	end do

end procedure push_all_string


!===============================================================================

module procedure new_string_view


	view%s = str_
	view%pos = 1

end procedure new_string_view


!===============================================================================

module procedure string_view_get_line


	!********

	integer :: length, io

	io = exit_success
	length = scan(sv%s( sv%pos: ), line_feed//carriage_return)
	if (length <= 0) then
		io = iostat_end
	end if

	!print *, 'string_view_get_line'
	!print *, 'pos, len = ', sv%pos, length

	! `length` is the 1-based position of the line_feed/carriage_return
	! delimiter within sv%s(sv%pos:), so the line's content (excluding the
	! delimiter, to match read_line()'s contract in this same module) ends
	! one character before it, at sv%pos + length - 2.  sv%pos itself still
	! advances by the full `length` so the next call starts just past the
	! delimiter
	line = sv%s( sv%pos: sv%pos + length  - 2 )
	sv%pos = sv%pos + length

	if (present(iostat)) iostat = io

end procedure string_view_get_line


!===============================================================================

module procedure read_line

	! c.f. aoc-2022/utils.f90
	!
	! This version reads WITHOUT backspacing, so it works on stdin too



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

end procedure read_line


!===============================================================================

module procedure exists
	inquire(file = filename, exist = exists)
end procedure exists


!===============================================================================

module procedure is_dir

	! Check if filename is a directory.  There is no standard Fortran
	! intrinsic for this, so delegate to a stat()-based C helper (see
	! syntran_is_dir() in src/c/isocline_wrap.c).  This is more portable than
	! trying to detect directories from open()/read() error codes, which vary
	! between compiler runtimes (e.g. gfortran vs ifx), and more reliable
	! than an inquire()-based probe: an earlier version of this function
	! probed inquire(file = "<path>/."), which works on POSIX (stat() on
	! "<path>/." fails with ENOTDIR for regular files) but not on Windows,
	! where path normalization collapses "<file>/." to "<file>" and made
	! every regular file look like a directory


	is_dir = syntran_is_dir_c(trim(filename)//c_null_char) /= 0

end procedure is_dir


!===============================================================================

module procedure read_file

	! Read all lines of a file into str_




	!********

	character :: c

	integer :: io, iu

	type(char_vector_t) :: sb  ! string builder

	! str_ must be allocated on every return path.  Callers assign the result
	! directly (e.g. `source_text = read_file(file, iostat)`) and only check
	! iostat afterward, so an early return with str_ left unallocated would
	! make that assignment read an undefined allocatable -- harmless under
	! gfortran, but a segfault under ifort classic's debug runtime
	str_ = ''

	if (is_dir(file)) then
		if (present(iostat)) iostat = exit_failure
		return
	end if

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

end procedure read_file


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

module procedure get_dir
	!********
	character(len = :), allocatable :: path
	integer :: beg_, end_, i

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

end procedure get_dir


!===============================================================================

module procedure get_cwd

	! Get the current working directory.  There is no standard Fortran
	! intrinsic for this, so delegate to a getcwd()-based C helper (see
	! syntran_getcwd() in src/c/isocline_wrap.c)


	!********

	character(kind = c_char, len = 1) :: buf(4096)

	integer :: i, n

	cwd = ''
	if (syntran_getcwd_c(buf, size(buf)) /= 0) return

	n = 0
	do i = 1, size(buf)
		if (buf(i) == c_null_char) exit
		n = n + 1
	end do

	deallocate(cwd)
	allocate(character(len = n) :: cwd)
	do i = 1, n
		cwd(i:i) = buf(i)
	end do

end procedure get_cwd


!===============================================================================

module procedure is_abs_path
	! Check if a path is absolute (starts with / on Unix or drive letter on Windows)


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

end procedure is_abs_path


!===============================================================================

module procedure resolve_path

	! Resolve a path relative to src_dir
	! If path is absolute, return it as-is
	! If path is relative and src_dir is non-empty, prepend src_dir


	if (is_abs_path(path)) then
		resolved = path
	else if (len(src_dir) > 0) then
		resolved = src_dir // path
	else
		resolved = path
	end if

end procedure resolve_path


!===============================================================================

module procedure is_digit


	is_digit = '0' <= c .and. c <= '9'

end procedure is_digit


!===============================================================================

module procedure is_digit_under


	is_digit_under = is_digit(c) .or. c == "_"

end procedure is_digit_under


!===============================================================================

module procedure is_hex
	! This is only applicable to hex digits, not the "0x" prefix


	is_hex = &
		is_digit(c) .or. &
		('a' <= c .and. c <= 'f') .or. &
		('A' <= c .and. c <= 'F')

end procedure is_hex


!===============================================================================

module procedure is_hex_under
	! This is only applicable to hex digits, not the "0x" prefix


	is_hex_under = is_hex(c) .or. c == "_"

end procedure is_hex_under


!===============================================================================

module procedure is_oct
	! This is only applicable to octal digits, not the "0o" prefix


	is_oct = '0' <= c .and. c <= '7'

end procedure is_oct


!===============================================================================

module procedure is_oct_under

	is_oct_under = is_oct(c) .or. c == "_"

end procedure is_oct_under


!===============================================================================

module procedure is_bin
	! This is only applicable to binary digits, not the "0b" prefix


	is_bin = '0' <= c .and. c <= '1'

end procedure is_bin


!===============================================================================

module procedure is_bin_under

	is_bin_under = is_bin(c) .or. c == "_"

end procedure is_bin_under


!===============================================================================

module procedure is_sign


	is_sign = c == '+' .or. c == '-'

end procedure is_sign


!===============================================================================

module procedure is_expo


	is_expo = c == 'd' .or. c == 'e' .or. c == 'D' .or. c == 'E'

end procedure is_expo


!===============================================================================

module procedure is_float


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

end procedure is_float


!===============================================================================

module procedure is_float_under


	is_float_under = is_float(c) .or. c == "_"

end procedure is_float_under


!===============================================================================

module procedure is_letter


	is_letter = ('a' <= c .and. c <= 'z') .or. ('A' <= c .and. c <= 'Z')

end procedure is_letter


!===============================================================================

module procedure is_alphanum


	is_alphanum = is_letter(c) .or. is_digit(c)

end procedure is_alphanum


!===============================================================================

module procedure is_alphanum_under


	is_alphanum_under = is_letter(c) .or. is_digit(c) .or. c == "_"

end procedure is_alphanum_under


!===============================================================================

module procedure is_whitespace


	is_whitespace = any(c == [tab, line_feed, vert_tab, carriage_return, ' '])

end procedure is_whitespace


!===============================================================================

module procedure rm_char

	! Remove all occurences of character `char` from `str_`



	!********

	integer :: i
	type(char_vector_t) :: sb  ! string builder

	sb = new_char_vector()
	do i = 1, len(str_)
		if (str_(i:i) /= char) call sb%push(str_(i:i))
	end do
	str_out = sb%trim()

end procedure rm_char


!===============================================================================

module procedure rm_leading_zeros

	! Strip leading '0' characters from str_.  Used on BOZ literal digit
	! strings (after underscores have already been removed by rm_char())
	! so a fixed-width Z/O/B edit descriptor isn't fed more digits than it
	! can hold by zeros that don't affect the value.  Returns "0" rather
	! than "" if str_ is empty or all zeros, so there's still a digit for
	! the edit descriptor to read



	!********

	integer :: i

	i = 1
	do while (i < len(str_) .and. str_(i:i) == "0")
		i = i + 1
	end do
	str_out = str_(i:)

end procedure rm_leading_zeros


!===============================================================================

module procedure replace_all

	! Replace all occurrences of substring `old` with `new` in `str_`



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

end procedure replace_all


!===============================================================================

module procedure tabs2spaces

	! Replace each tab with a *single* space.  This is useful for alignment and
	! it makes allocation easy


	integer :: i

	allocate(character(len = len(str_)) :: str_out)
	do i = 1, len(str_)
		if (str_(i:i) == tab) then
			str_out(i:i) = ' '
		else
			str_out(i:i) = str_(i:i)
		end if
	end do

end procedure tabs2spaces


!===============================================================================

module procedure trimw

	! Trim whitespace, because the intrinsic trim() fn apparently doesn't trim
	! line breaks!?


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

end procedure trimw


!===============================================================================

module procedure quote

	! Wrap a str_ in "double quotes".  Any quotes already contained are not
	! escaped


	wrapped = '"'//str_//'"'

end procedure quote


!===============================================================================

module procedure quote_escape

	! Wrap a str_ in "double quotes" and escape any quotes already contained,
	! so the result is a valid syntran string literal that lexes back to
	! str_.  Syntran has no backslash escapes -- a literal quote inside a
	! string is written by *doubling* it (c.f. lex.f90's string lexer), so
	! that's the only substitution needed here


	integer :: i

	type(char_vector_t) :: vec

	vec = new_char_vector()
	call vec%push('"')
	do i = 1, len(str_)
		if (str_(i:i) == '"') call vec%push('"')
		call vec%push(str_(i:i))
	end do
	call vec%push('"')

	wrapped = vec%trim()

end procedure quote_escape


!===============================================================================

module procedure is_str_eq
	! Fortran considers spaces as insignificant in str comparisons, but no sane
	! language would allow that
	!
	! I guess this is an artifact of fixed-length strings being common in older
	! fortran code
	!
	! Note that `is_ne()` is implemented as `.not. is_eq()`, which calls this
	! fn, so there is no need for a separate is_str_ne()


	!is_str_eq = a == b  ! not what you expect!

	is_str_eq = &
		len(a) == len(b) .and. &
		    a  ==     b

end procedure is_str_eq


!===============================================================================

module procedure is_str_lt
	! Length-aware, lexicographic string less-than.  Fortran's `<` blank-pads
	! the shorter operand to the longer operand's length before comparing, so
	! e.g. `"a" < "a "` is `.false.` in raw Fortran even though the shorter
	! string should sort first.  Comparing one character at a time sidesteps
	! that (single-char slices have no padding to apply), and a common prefix
	! is broken by length, with the shorter string sorting first
	!
	! `a <= b`, `a > b`, and `a >= b` are all derived from this one fn:
	!
	!     a <  b  =        is_str_lt(a, b)
	!     a <= b  = .not.  is_str_lt(b, a)
	!     a >  b  =        is_str_lt(b, a)
	!     a >= b  = .not.  is_str_lt(a, b)


	integer :: i, n

	n = min(len(a), len(b))
	do i = 1, n
		if (a(i:i) /= b(i:i)) then
			is_str_lt = a(i:i) < b(i:i)
			return
		end if
	end do
	is_str_lt = len(a) < len(b)

end procedure is_str_lt


!===============================================================================

module procedure findlocl1

	! findloc() is standard in Fortran 2008, but gfortran 8.1.0 doesn't have it
	! yet :(.  Here I implement it for logical rank-1 arrays only without
	! optional args



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

end procedure findlocl1


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

module procedure console_color
	write(*, '(a)', advance = 'no') color
end procedure console_color


module procedure console_color_reset
	write(*, '(a)', advance = 'no') color_reset
end procedure console_color_reset


!===============================================================================

module procedure i32_str



	! Fine for default 4-byte ints.  May need more chars for bigger ints
	character(len = 16) :: buffer

	write(buffer, '(i0)') x
	str_ = trim(buffer)

end procedure i32_str


!===============================================================================

module procedure i32_vec_str



	integer :: i
	str_ = "["
	do i = 1, size(x)
		str_ = str_//i32_str(x(i))
		if (i < size(x)) str_ = str_//", "
	end do
	str_ = str_//"]"

end procedure i32_vec_str


!===============================================================================

module procedure i64_str



	! I think ~20 chars is the max actually, but let's round up to the next
	! multiple of 8
	character(len = 24) :: buffer

	write(buffer, '(i0)') x
	str_ = trim(buffer)

end procedure i64_str


!===============================================================================

module procedure f32_str



	! Fine for default 4-byte type
	character(len = 16) :: buffer

	write(buffer, '(es16.6)') x
	str_ = trim(adjustl(buffer))

end procedure f32_str


!===============================================================================

module procedure f64_str



	character(len = 28) :: buffer

	write(buffer, '(es25.15)') x
	str_ = trim(adjustl(buffer))

end procedure f64_str


!===============================================================================

module procedure bool1_str



	if (x) then
		str_ = 'true'
	else
		str_ = 'false'
	end if

end procedure bool1_str


!===============================================================================

module procedure fnv_1a

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

end procedure fnv_1a


!===============================================================================

module procedure map_i32_init
	! Consider making a `new_map_i32()` fn instead of init subroutine,
	! consistent with syntran src style

	if (capacity <= 0) then
		error stop "map_i32_init: capacity must be positive"
	end if

	self%capacity = capacity
	self%count = 0
	allocate(self%table(capacity))
end procedure map_i32_init


!===============================================================================

module procedure map_i32_set
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
end procedure map_i32_set


!===============================================================================

module procedure map_i32_get
	! Consider making this fn return `value` instead of `found`
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
end procedure map_i32_get


!===============================================================================

module procedure map_i32_contains
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
end procedure map_i32_contains


!===============================================================================

!===============================================================================

module procedure map_i32_destroy
	if (allocated(self%table)) deallocate(self%table)
	self%capacity = 0
	self%count = 0
end procedure map_i32_destroy


!===============================================================================

module procedure map_i32_resize
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
end procedure map_i32_resize


!===============================================================================

module procedure to_lower

	! Return a copy of string `s` with ASCII upper-case letters converted to
	! lower-case.  Used for case-insensitive Levenshtein matching.


	!********

	integer :: i, ic

	lower = s
	do i = 1, len(lower)
		ic = iachar(lower(i:i))
		if (ic >= iachar('A') .and. ic <= iachar('Z')) then
			lower(i:i) = achar(ic + 32)
		end if
	end do

end procedure to_lower


!===============================================================================

module procedure unqualified_name

	! Return the portion of `name` after the last "::" module separator, or
	! the whole name when it contains no "::".  Used so spellcheck suggestions
	! can rank module-qualified names (e.g. "poople::read_dict") by how close
	! their unqualified tail is to the typed identifier.


	!********

	integer :: p

	p = index(name, "::", back = .true.)
	if (p > 0) then
		unqual = name(p+2:)
	else
		unqual = name
	end if

end procedure unqualified_name


!===============================================================================

module procedure overload_display_name

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


	!********

	integer :: n

	if (len(name) < 1 .or. name(1:1) /= "0") then
		display = name
		return
	end if

	! Strip the leading "0"
	display = name(2:)

	! Strip trailing rank tag: _arr, _sca
	! Nested if (not compound .and.) so the substring access is never evaluated
	! when n < 4, regardless of short-circuit behaviour.
	n = len(display)
	if (n >= 4) then
		if (display(n-3:n) == "_arr" .or. display(n-3:n) == "_sca") then
			display = display(1:n-4)
		end if
	end if

	! Strip trailing kind tag: _i32, _i64, _f32, _f64
	n = len(display)
	if (n >= 4) then
		if (display(n-3:n) == "_i32" .or. display(n-3:n) == "_i64" .or. &
		    display(n-3:n) == "_f32" .or. display(n-3:n) == "_f64") then
			display = display(1:n-4)
		end if
	end if

end procedure overload_display_name


!===============================================================================

module procedure levenshtein

	! Get the Levenshtein edit distance between strings `s` and `t`.
	! Adapted from the two-row DP implementation in ~/git/jsonf/src/jsonf.F90.


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

end procedure levenshtein


!===============================================================================

end submodule syntran__utils_impl_m

!===============================================================================
