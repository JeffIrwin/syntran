
!===============================================================================

module syntran__utils_m

	use iso_fortran_env
	use iso_c_binding

	implicit none

	integer, parameter :: exit_success = 0, exit_failure = 1

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

	! C helpers used by is_dir()/get_cwd() below.  Implemented in
	! src/c/isocline_wrap.c, which is linked into every syntran target (FPM
	! and CMake)
	interface
		function syntran_is_dir_c(path) bind(c, name = "syntran_is_dir") result(res)
			import :: c_char, c_int
			character(kind = c_char), intent(in) :: path(*)
			integer(c_int) :: res
		end function syntran_is_dir_c

		function syntran_getcwd_c(buf, n) bind(c, name = "syntran_getcwd") result(res)
			import :: c_char, c_int
			character(kind = c_char), intent(out) :: buf(*)
			integer(c_int), value, intent(in) :: n
			integer(c_int) :: res
		end function syntran_getcwd_c
	end interface

!===============================================================================


	! Bodies live in utils_impl.f90 so that editing them does not
	! invalidate this module's .mod and rebuild the tree -- only the
	! .smod changes, which fpm's dependents don't recompile on.
	interface

		module function new_integer_vector() result(vector)
			type(integer_vector_t) :: vector
		end function new_integer_vector

		module subroutine push_integer(vector, val)
			class(integer_vector_t) :: vector
			integer, intent(in) :: val
		end subroutine push_integer

		module subroutine push_i64(vector, val)
			class(i64_vector_t) :: vector
			integer(kind = 8), intent(in) :: val
		end subroutine push_i64

		module function new_logical_vector() result(vector)
			type(logical_vector_t) :: vector
		end function new_logical_vector

		module subroutine push_logical(vector, val)
			class(logical_vector_t) :: vector
			logical, intent(in) :: val
		end subroutine push_logical

		module function new_string_vector() result(vector)
			type(string_vector_t) :: vector
		end function new_string_vector

		module subroutine push_string(vector, val)
			class(string_vector_t) :: vector
			character(len = *), intent(in) :: val
		end subroutine push_string

		module function new_char_vector(cap) result(vector)
			integer, intent(in), optional :: cap
			type(char_vector_t) :: vector
		end function new_char_vector

		module subroutine push_char(vector, val)
			class(char_vector_t) :: vector
			character(len = *), intent(in) :: val
		end subroutine push_char

		module function trim_char_vector(sb) result(str_)
			class(char_vector_t), intent(in) :: sb
			character(len = :), allocatable :: str_
		end function trim_char_vector

		module subroutine push_all_string(vector, add)
			class(string_vector_t) :: vector
			type(string_vector_t), intent(in) :: add
		end subroutine push_all_string

		module function new_string_view(str_) result(view)
			character(len = *), intent(in) :: str_
			type(string_view_t) :: view
		end function new_string_view

		module function string_view_get_line(sv, iostat) result(line)
			class(string_view_t), intent(inout) :: sv
			character(len = :), allocatable :: line
			integer, optional, intent(out) :: iostat
		end function string_view_get_line

		module function read_line(iu, iostat) result(str_)
			integer, intent(in) :: iu
			integer, optional, intent(out) :: iostat
			character(len = :), allocatable :: str_
		end function read_line

		module logical function exists(filename)
			character(len = *), intent(in) :: filename
		end function exists

		module logical function is_dir(filename)
			character(len = *), intent(in) :: filename
		end function is_dir

		module function read_file(file, iostat) result(str_)
			character(len = *), intent(in) :: file
			integer, optional, intent(out) :: iostat
			character(len = :), allocatable :: str_
		end function read_file

		module function get_dir(filename) result(dir)
			character(len = *), intent(in)  :: filename
			character(len = :), allocatable :: dir
		end function get_dir

		module function get_cwd() result(cwd)
			character(len = :), allocatable :: cwd
		end function get_cwd

		module logical function is_abs_path(path)
			character(len = *), intent(in) :: path
		end function is_abs_path

		module function resolve_path(src_dir, path) result(resolved)
			character(len = *), intent(in) :: src_dir, path
			character(len = :), allocatable :: resolved
		end function resolve_path

		module logical function is_digit(c)
			character, intent(in) :: c
		end function is_digit

		module logical function is_digit_under(c)
			character, intent(in) :: c
		end function is_digit_under

		module logical function is_hex(c)
			character, intent(in) :: c
		end function is_hex

		module logical function is_hex_under(c)
			character, intent(in) :: c
		end function is_hex_under

		module logical function is_oct(c)
			character, intent(in) :: c
		end function is_oct

		module logical function is_oct_under(c)
			character, intent(in) :: c
		end function is_oct_under

		module logical function is_bin(c)
			character, intent(in) :: c
		end function is_bin

		module logical function is_bin_under(c)
			character, intent(in) :: c
		end function is_bin_under

		module logical function is_sign(c)
			character, intent(in) :: c
		end function is_sign

		module logical function is_expo(c)
			character, intent(in) :: c
		end function is_expo

		module logical function is_float(c)
			character, intent(in) :: c
		end function is_float

		module logical function is_float_under(c)
			character, intent(in) :: c
		end function is_float_under

		module logical function is_letter(c)
			character, intent(in) :: c
		end function is_letter

		module logical function is_alphanum(c)
			character, intent(in) :: c
		end function is_alphanum

		module logical function is_alphanum_under(c)
			character, intent(in) :: c
		end function is_alphanum_under

		module logical function is_whitespace(c)
			character, intent(in) :: c
		end function is_whitespace

		module function rm_char(str_, char) result(str_out)
			character(len = *), intent(in) :: str_
			character, intent(in) :: char
			character(len = :), allocatable :: str_out
		end function rm_char

		module function rm_leading_zeros(str_) result(str_out)
			character(len = *), intent(in) :: str_
			character(len = :), allocatable :: str_out
		end function rm_leading_zeros

		module function replace_all(str_, old, new) result(str_out)
			character(len = *), intent(in) :: str_, old, new
			character(len = :), allocatable :: str_out
		end function replace_all

		module function tabs2spaces(str_) result(str_out)
			character(len = *), intent(in)  :: str_
			character(len = :), allocatable :: str_out
		end function tabs2spaces

		module function trimw(str_)
			character(len = *), intent(in)  :: str_
			character(len = :), allocatable :: trimw
		end function trimw

		module function quote(str_) result(wrapped)
			character(len = *), intent(in)  :: str_
			character(len = :), allocatable :: wrapped
		end function quote

		module function quote_escape(str_) result(wrapped)
			character(len = *), intent(in)  :: str_
			character(len = :), allocatable :: wrapped
		end function quote_escape

		module logical function is_str_eq(a, b)
			character(len = *), intent(in) :: a, b
		end function is_str_eq

		module logical function is_str_lt(a, b)
			character(len = *), intent(in) :: a, b
		end function is_str_lt

		module function findlocl1(arr, val) result(loc)
			logical, intent(in) :: arr(:), val
			integer :: loc(1)
		end function findlocl1

		module subroutine console_color(color)
			character(len = *), intent(in) :: color
		end subroutine console_color

		module subroutine console_color_reset()
		end subroutine console_color_reset

		module function i32_str(x) result(str_)
			integer(kind = 4), intent(in) :: x
			character(len = :), allocatable :: str_
		end function i32_str

		module function i32_vec_str(x) result(str_)
			integer(kind = 4), intent(in) :: x(:)
			character(len = :), allocatable :: str_
		end function i32_vec_str

		module function i64_str(x) result(str_)
			integer(kind = 8), intent(in) :: x
			character(len = :), allocatable :: str_
		end function i64_str

		module function f32_str(x) result(str_)
			real, intent(in) :: x
			character(len = :), allocatable :: str_
		end function f32_str

		module function f64_str(x) result(str_)
			real(kind = 8), intent(in) :: x
			character(len = :), allocatable :: str_
		end function f64_str

		module function bool1_str(x) result(str_)
			logical(kind = 1), intent(in) :: x
			character(len = :), allocatable :: str_
		end function bool1_str

		module pure function fnv_1a(input, seed) result(hash)
			character(*), intent(in) :: input
			integer(int64), intent(in), optional :: seed
			integer(int64) :: hash
		end function fnv_1a

		module subroutine map_i32_init(self, capacity)
			class(map_i32_t), intent(inout) :: self
			integer, intent(in) :: capacity
		end subroutine map_i32_init

		module recursive subroutine map_i32_set(self, key, value)
			class(map_i32_t), intent(inout) :: self
			character(len=*), intent(in) :: key
			integer, intent(in) :: value
		end subroutine map_i32_set

		module function map_i32_get(self, key, value) result(found)
			class(map_i32_t), intent(in) :: self
			character(len=*), intent(in) :: key
			integer, intent(out) :: value
			logical :: found
		end function map_i32_get

		module function map_i32_contains(self, key) result(found)
			class(map_i32_t), intent(in) :: self
			character(len=*), intent(in) :: key
			logical :: found
		end function map_i32_contains

		module subroutine map_i32_destroy(self)
			class(map_i32_t), intent(inout) :: self
		end subroutine map_i32_destroy

		module recursive subroutine map_i32_resize(self)
			class(map_i32_t), intent(inout) :: self
		end subroutine map_i32_resize

		module function to_lower(s) result(lower)
			character(len = *), intent(in) :: s
			character(len = :), allocatable :: lower
		end function to_lower

		module function unqualified_name(name) result(unqual)
			character(len = *), intent(in) :: name
			character(len = :), allocatable :: unqual
		end function unqualified_name

		module function overload_display_name(name) result(display)
			character(len = *), intent(in) :: name
			character(len = :), allocatable :: display
		end function overload_display_name

		module integer function levenshtein(s, t)
			character(len = *), intent(in) :: s, t
		end function levenshtein

	end interface

end module syntran__utils_m

!===============================================================================

