
!===============================================================================

module syntran__c_parse_m

	! Thread-safe numeric parsing using C standard library functions
	!
	! This module provides Fortran wrappers around C stdlib functions strtol,
	! strtoll, strtof, and strtod, which are thread-safe (reentrant) unlike
	! Fortran's internal I/O read() statements.
	!
	! Allegedly this might be fixed in later gfortran versions, but I tried
	! gfortran-12 and there were still problems:
	!
	!     https://fortran-lang.discourse.group/t/openmp-and-thread-safety-of-i-os-write-read/4567/19

	use iso_c_binding
	implicit none

	! TODO: move to consts? Otherwise, these are defined in multiple places or
	! there are circular dependencies between this and utils
	integer, parameter :: exit_success_ = 0, exit_failure_ = -1

	interface

		! long strtol(const char *str, char **endptr, int base)
		function c_strtol(str, endptr, base) bind(c, name='strtol')
			import :: c_ptr, c_char, c_int, c_long
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			integer(c_int), value :: base
			integer(c_long) :: c_strtol
		end function

		! long long strtoll(const char *str, char **endptr, int base)
		function c_strtoll(str, endptr, base) bind(c, name='strtoll')
			import :: c_ptr, c_char, c_int, c_long_long
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			integer(c_int), value :: base
			integer(c_long_long) :: c_strtoll
		end function

		! unsigned long strtoul(const char *str, char **endptr, int base)
		function c_strtoul(str, endptr, base) bind(c, name='strtoul')
			import :: c_ptr, c_char, c_int, c_long
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			integer(c_int), value :: base
			integer(c_long) :: c_strtoul
		end function

		! unsigned long long strtoull(const char *str, char **endptr, int base)
		function c_strtoull(str, endptr, base) bind(c, name='strtoull')
			import :: c_ptr, c_char, c_int, c_long_long
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			integer(c_int), value :: base
			integer(c_long_long) :: c_strtoull
		end function

		! float strtof(const char *str, char **endptr)
		function c_strtof(str, endptr) bind(c, name='strtof')
			import :: c_ptr, c_char, c_float
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			real(c_float) :: c_strtof
		end function

		! double strtod(const char *str, char **endptr)
		function c_strtod(str, endptr) bind(c, name='strtod')
			import :: c_ptr, c_char, c_double
			character(kind=c_char), intent(in) :: str(*)
			type(c_ptr), value :: endptr
			real(c_double) :: c_strtod
		end function

		! int* isinf(double x)
		function c_isinf(x) bind(c, name='isinf')
			import :: c_double, c_int
			real(c_double), value :: x
			integer(c_int) :: c_isinf
		end function

		! int* isnan(double x)
		function c_isnan(x) bind(c, name='isnan')
			import :: c_double, c_int
			real(c_double), value :: x
			integer(c_int) :: c_isnan
		end function

		! int snprintf(char *str, size_t size, const char *format, int val)
		function c_snprintf_i32(str, size, format, val) bind(c, name='snprintf')
			import :: c_ptr, c_char, c_int, c_size_t
			type(c_ptr), value :: str
			integer(c_size_t), value :: size
			character(kind=c_char), intent(in) :: format(*)
			integer(c_int), value :: val
			integer(c_int) :: c_snprintf_i32
		end function

		! int snprintf(char *str, size_t size, const char *format, long long val)
		function c_snprintf_i64(str, size, format, val) bind(c, name='snprintf')
			import :: c_ptr, c_char, c_int, c_size_t, c_long_long
			type(c_ptr), value :: str
			integer(c_size_t), value :: size
			character(kind=c_char), intent(in) :: format(*)
			integer(c_long_long), value :: val
			integer(c_int) :: c_snprintf_i64
		end function

		! int snprintf(char *str, size_t size, const char *format, double val)
		function c_snprintf_f32(str, size, format, val) bind(c, name='snprintf')
			import :: c_ptr, c_char, c_int, c_size_t, c_double
			type(c_ptr), value :: str
			integer(c_size_t), value :: size
			character(kind=c_char), intent(in) :: format(*)
			real(c_double), value :: val
			integer(c_int) :: c_snprintf_f32
		end function

		! int snprintf(char *str, size_t size, const char *format, double val)
		function c_snprintf_f64(str, size, format, val) bind(c, name='snprintf')
			import :: c_ptr, c_char, c_int, c_size_t, c_double
			type(c_ptr), value :: str
			integer(c_size_t), value :: size
			character(kind=c_char), intent(in) :: format(*)
			real(c_double), value :: val
			integer(c_int) :: c_snprintf_f64
		end function

	end interface

contains

!===============================================================================

subroutine parse_i32_base(str, val, iostat, base)
	! Parse 32-bit integer from string with specified base

	character(len=*), intent(in) :: str
	integer(4), intent(out) :: val
	integer, intent(out) :: iostat
	integer, intent(in) :: base

	character(len=:), allocatable :: cstr
	integer(c_long) :: result

	cstr = trim(str) // c_null_char
	result = c_strtol(cstr, c_null_ptr, int(base, c_int))

	! Check for overflow (c_long may be 64-bit, we want 32-bit)
	if (result > huge(val) .or. result < -huge(val)-1) then
		iostat = exit_failure_
		val = 0
	else
		iostat = exit_success_
		val = int(result, kind=4)
	end if

end subroutine parse_i32_base

!===============================================================================

subroutine parse_i64_base(str, val, iostat, base)
	! Parse 64-bit integer from string with specified base

	character(len=*), intent(in) :: str
	integer(8), intent(out) :: val
	integer, intent(out) :: iostat
	integer, intent(in) :: base

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result

	cstr = trim(str) // c_null_char
	result = c_strtoll(cstr, c_null_ptr, int(base, c_int))

	! Check for overflow
	if (result > huge(val) .or. result < -huge(val)-1_8) then
		iostat = exit_failure_
		val = 0_8
	else
		iostat = exit_success_
		val = int(result, kind=8)
	end if

end subroutine parse_i64_base

!===============================================================================

subroutine parse_f32(str, val, iostat)
	! Parse 32-bit float from string

	character(len=*), intent(in) :: str
	real(4), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	real(c_float) :: result

	cstr = trim(str) // c_null_char
	result = c_strtof(cstr, c_null_ptr)

	! Check for inf/nan (these indicate parse failure or overflow)
	if (c_isinf(real(result, c_double)) /= 0 .or. &
	    c_isnan(real(result, c_double)) /= 0) then
		iostat = exit_failure_
		val = 0.0
	else
		iostat = exit_success_
		val = real(result, kind=4)
	end if

end subroutine parse_f32

!===============================================================================

subroutine parse_f64(str, val, iostat)
	! Parse 64-bit float from string

	character(len=*), intent(in) :: str
	real(8), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	real(c_double) :: result

	cstr = trim(str) // c_null_char
	result = c_strtod(cstr, c_null_ptr)

	! Check for inf/nan (these indicate parse failure or overflow)
	if (c_isinf(result) /= 0 .or. c_isnan(result) /= 0) then
		iostat = exit_failure_
		val = 0.0d0
	else
		iostat = exit_success_
		val = real(result, kind=8)
	end if

end subroutine parse_f64

!===============================================================================

! Convenience wrappers for specific bases
! Hex/octal/binary use unsigned parsing to match Fortran's z/o/b format behavior

subroutine parse_i32_hex(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(4), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result
	integer(c_long_long), parameter :: max_u32 = int(z'FFFFFFFF', c_long_long)

	cstr = trim(str) // c_null_char
	! Use strtoull to avoid truncation on platforms where c_long is 32-bit
	result = c_strtoull(cstr, c_null_ptr, 16_c_int)

	! Check for overflow (result is unsigned, must fit in 32 bits)
	if (result > max_u32) then
		iostat = exit_failure_
		val = 0
	else
		iostat = exit_success_
		! Reinterpret bits as signed
		val = int(result, kind=4)
	end if
end subroutine parse_i32_hex

subroutine parse_i64_hex(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(8), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result

	cstr = trim(str) // c_null_char
	result = c_strtoull(cstr, c_null_ptr, 16_c_int)

	iostat = exit_success_
	! Reinterpret bits as signed
	val = int(result, kind=8)
end subroutine parse_i64_hex

subroutine parse_i32_oct(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(4), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result
	integer(c_long_long), parameter :: max_u32 = int(z'FFFFFFFF', c_long_long)

	cstr = trim(str) // c_null_char
	! Use strtoull to avoid truncation on platforms where c_long is 32-bit
	result = c_strtoull(cstr, c_null_ptr, 8_c_int)

	if (result > max_u32) then
		iostat = exit_failure_
		val = 0
	else
		iostat = exit_success_
		val = int(result, kind=4)
	end if
end subroutine parse_i32_oct

subroutine parse_i64_oct(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(8), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result

	cstr = trim(str) // c_null_char
	result = c_strtoull(cstr, c_null_ptr, 8_c_int)

	iostat = exit_success_
	val = int(result, kind=8)
end subroutine parse_i64_oct

subroutine parse_i32_bin(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(4), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result
	integer(c_long_long), parameter :: max_u32 = int(z'FFFFFFFF', c_long_long)

	cstr = trim(str) // c_null_char
	! Use strtoull to avoid truncation on platforms where c_long is 32-bit
	result = c_strtoull(cstr, c_null_ptr, 2_c_int)

	if (result > max_u32) then
		iostat = exit_failure_
		val = 0
	else
		iostat = exit_success_
		val = int(result, kind=4)
	end if
end subroutine parse_i32_bin

subroutine parse_i64_bin(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(8), intent(out) :: val
	integer, intent(out) :: iostat

	character(len=:), allocatable :: cstr
	integer(c_long_long) :: result

	cstr = trim(str) // c_null_char
	result = c_strtoull(cstr, c_null_ptr, 2_c_int)

	iostat = exit_success_
	val = int(result, kind=8)
end subroutine parse_i64_bin

subroutine parse_i32_dec(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(4), intent(out) :: val
	integer, intent(out) :: iostat
	call parse_i32_base(str, val, iostat, 10)
end subroutine parse_i32_dec

subroutine parse_i64_dec(str, val, iostat)
	character(len=*), intent(in) :: str
	integer(8), intent(out) :: val
	integer, intent(out) :: iostat
	call parse_i64_base(str, val, iostat, 10)
end subroutine parse_i64_dec

!===============================================================================

subroutine format_i32(val, str)
	! Format 32-bit integer to string
	integer(4), intent(in) :: val
	character(len=:), allocatable, intent(out) :: str

	character(len=16, kind=c_char), target :: buf
	integer(c_int) :: n

	buf = ''
	n = c_snprintf_i32(c_loc(buf), int(len(buf), c_size_t), &
	    '%d'//c_null_char, int(val, c_int))
	str = trim(buf(1:n))
end subroutine format_i32

!===============================================================================

subroutine format_i64(val, str)
	! Format 64-bit integer to string
	integer(8), intent(in) :: val
	character(len=:), allocatable, intent(out) :: str

	character(len=24, kind=c_char), target :: buf
	integer(c_int) :: n

	buf = ''
	n = c_snprintf_i64(c_loc(buf), int(len(buf), c_size_t), &
	    '%lld'//c_null_char, int(val, c_long_long))
	str = trim(buf(1:n))
end subroutine format_i64

!===============================================================================

subroutine format_f32(val, str)
	! Format 32-bit float to string
	real(4), intent(in) :: val
	character(len=:), allocatable, intent(out) :: str

	character(len=16, kind=c_char), target :: buf
	integer(c_int) :: n

	buf = ''
	n = c_snprintf_f32(c_loc(buf), int(len(buf), c_size_t), &
	    '%.6E'//c_null_char, real(val, c_double))
	str = trim(buf(1:n))
end subroutine format_f32

!===============================================================================

subroutine format_f64(val, str)
	! Format 64-bit float to string
	real(8), intent(in) :: val
	character(len=:), allocatable, intent(out) :: str

	character(len=28, kind=c_char), target :: buf
	integer(c_int) :: n

	buf = ''
	n = c_snprintf_f64(c_loc(buf), int(len(buf), c_size_t), &
	    '%.15E'//c_null_char, real(val, c_double))
	str = trim(buf(1:n))
end subroutine format_f64

!===============================================================================

end module syntran__c_parse_m

!===============================================================================

