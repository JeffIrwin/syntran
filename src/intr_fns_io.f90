
!===============================================================================

module syntran__intr_fns_io_m

	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine declare_io_fns(fns, id_index, fn_array)

	! Declare I/O and string intrinsic functions

	type(fns_t), intent(inout) :: fns
	integer, intent(inout) :: id_index
	type(fn_t), allocatable, intent(out) :: fn_array(:)

	!********

	type(fn_t) :: println_fn, str_fn, len_fn, repeat_fn, args_fn, &
		parse_i32_fn, parse_i64_fn, parse_f32_fn, parse_f64_fn, &
		char_fn, i32_sca_fn, i32_arr_fn, i64_sca_fn, i64_arr_fn, &
		open_fn, readln_fn, writeln_fn, eof_fn, close_fn, exit_fn

	!********

	println_fn%type%type = void_type

	allocate(println_fn%params(0))
	allocate(println_fn%param_names%v(0))
	!println_fn%param_names%v(1)%s = "a"

	println_fn%variadic_min  = 0
	println_fn%variadic_type = any_type
	println_fn%variadic_name = "value"

	call fns%insert("println", println_fn, id_index)

	!********

	str_fn%type%type = str_type

	allocate(str_fn%params(0))
	allocate(str_fn%param_names%v(0))
	!str_fn%param_names%v(1)%s = "a"

	str_fn%variadic_min  = 0
	str_fn%variadic_type = any_type
	str_fn%variadic_name = "value"

	call fns%insert("str", str_fn, id_index)

	!********

	len_fn%type%type = i64_type
	allocate(len_fn%params(1))
	allocate(len_fn%param_names%v(1))
	len_fn%params(1)%type = str_type
	len_fn%param_names%v(1)%s = "str"

	call fns%insert("len", len_fn, id_index)

	!********

	repeat_fn%type%type = str_type
	allocate(repeat_fn%params(2))
	allocate(repeat_fn%param_names%v(2))

	repeat_fn%params(1)%type = str_type
	repeat_fn%param_names%v(1)%s = "str"

	! TODO: overload for i32 and i64 ncopies? Not sure if you would need that many reps
	repeat_fn%params(2)%type = i32_type
	repeat_fn%param_names%v(2)%s = "ncopies"

	call fns%insert("repeat", repeat_fn, id_index)

	!********

	! std::args() returns an array of strings containing command-line arguments
	! passed after `--`.  This is an std-only function: it must be called as
	! std::args(), not just args()
	args_fn%type%type = array_type
	allocate(args_fn%type%array)
	args_fn%type%array%type = str_type
	args_fn%type%array%rank = 1

	allocate(args_fn%params(0))
	allocate(args_fn%param_names%v(0))

	call fns%insert("std::args", args_fn, id_index)

	!********

	! TODO: add fns for parsing str to other types (bool, etc.)

	! Should this accept any type?  f32 can be converted implicitly so there
	! shouldn't be a need for other types

	parse_i32_fn%type%type = i32_type
	allocate(parse_i32_fn%params(1))
	allocate(parse_i32_fn%param_names%v(1))
	parse_i32_fn%params(1)%type = str_type
	parse_i32_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_i32", parse_i32_fn, id_index)

	!********

	parse_i64_fn%type%type = i64_type
	allocate(parse_i64_fn%params(1))
	allocate(parse_i64_fn%param_names%v(1))
	parse_i64_fn%params(1)%type = str_type
	parse_i64_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_i64", parse_i64_fn, id_index)

	!********

	parse_f32_fn%type%type = f32_type
	allocate(parse_f32_fn%params(1))
	allocate(parse_f32_fn%param_names%v(1))
	parse_f32_fn%params(1)%type = str_type
	parse_f32_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_f32", parse_f32_fn, id_index)

	!********

	parse_f64_fn%type%type = f64_type
	allocate(parse_f64_fn%params(1))
	allocate(parse_f64_fn%param_names%v(1))
	parse_f64_fn%params(1)%type = str_type
	parse_f64_fn%param_names%v(1)%s = "str"

	call fns%insert("parse_f64", parse_f64_fn, id_index)

	!********

	char_fn%type%type = str_type
	allocate(char_fn%params(1))
	allocate(char_fn%param_names%v(1))

	char_fn%params(1)%type = i32_type

	char_fn%param_names%v(1)%s = "i"

	call fns%insert("char", char_fn, id_index)

	!********

	i32_sca_fn%type%type = i32_type
	allocate(i32_sca_fn%params(1))
	allocate(i32_sca_fn%param_names%v(1))

	i32_sca_fn%params(1)%type = any_type

	i32_sca_fn%param_names%v(1)%s = "a"

	call fns%insert("0i32_sca", i32_sca_fn, id_index)

	!********

	i32_arr_fn%type%type = array_type
	allocate(i32_arr_fn%type%array)
	i32_arr_fn%type%array%type = i32_type
	i32_arr_fn%type%array%rank = -1

	allocate(i32_arr_fn%params(1))
	allocate(i32_arr_fn%param_names%v(1))

	i32_arr_fn%params(1)%type = any_type

	i32_arr_fn%param_names%v(1)%s = "a"

	call fns%insert("0i32_arr", i32_arr_fn, id_index)

	!********

	! TODO: to f32 casting

	i64_sca_fn%type%type = i64_type
	allocate(i64_sca_fn%params(1))
	allocate(i64_sca_fn%param_names%v(1))

	i64_sca_fn%params(1)%type = any_type

	i64_sca_fn%param_names%v(1)%s = "a"

	call fns%insert("0i64_sca", i64_sca_fn, id_index)

	!********

	i64_arr_fn%type%type = array_type
	allocate(i64_arr_fn%type%array)
	i64_arr_fn%type%array%type = i64_type
	i64_arr_fn%type%array%rank = -1

	allocate(i64_arr_fn%params(1))
	allocate(i64_arr_fn%param_names%v(1))

	i64_arr_fn%params(1)%type = any_type

	i64_arr_fn%param_names%v(1)%s = "a"

	call fns%insert("0i64_arr", i64_arr_fn, id_index)

	!********

	open_fn%type%type = file_type
	allocate(open_fn%params(2))
	allocate(open_fn%param_names%v(2))

	open_fn%params(1)%type = str_type
	open_fn%param_names%v(1)%s = "filename"

	open_fn%params(2)%type = str_type
	open_fn%param_names%v(2)%s = "mode"

	call fns%insert("open", open_fn, id_index)

	!********

	readln_fn%type%type = str_type
	allocate(readln_fn%params(1))
	allocate(readln_fn%param_names%v(1))
	readln_fn%params(1)%type = file_type
	readln_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("readln", readln_fn, id_index)

	!********

	writeln_fn%type%type = void_type
	allocate(writeln_fn%params(1))
	allocate(writeln_fn%param_names%v(1))
	writeln_fn%params(1)%type = file_type
	writeln_fn%param_names%v(1)%s = "file_handle"

	writeln_fn%variadic_min  = 0
	!writeln_fn%variadic_min = 1
	writeln_fn%variadic_type = any_type
	writeln_fn%variadic_name = "value"

	call fns%insert("writeln", writeln_fn, id_index)

	!********

	eof_fn%type%type = bool_type
	allocate(eof_fn%params(1))
	allocate(eof_fn%param_names%v(1))
	eof_fn%params(1)%type = file_type
	eof_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("eof", eof_fn, id_index)

	!********

	close_fn%type%type = void_type
	allocate(close_fn%params(1))
	allocate(close_fn%param_names%v(1))
	close_fn%params(1)%type = file_type
	close_fn%param_names%v(1)%s = "file_handle"

	call fns%insert("close", close_fn, id_index)

	!********

	exit_fn%type%type = void_type
	allocate(exit_fn%params(1))
	allocate(exit_fn%param_names%v(1))
	exit_fn%params(1)%type = i32_type
	exit_fn%param_names%v(1)%s = "exit_status"

	call fns%insert("exit", exit_fn, id_index)

	!********

	! Return array of all functions declared in this module
	fn_array = &
		[ &
			println_fn, str_fn, len_fn, repeat_fn, args_fn, &
			parse_i32_fn, parse_i64_fn, parse_f32_fn, parse_f64_fn, &
			char_fn, i32_sca_fn, i32_arr_fn, i64_sca_fn, i64_arr_fn, &
			open_fn, readln_fn, writeln_fn, eof_fn, close_fn, exit_fn &
		]

end subroutine declare_io_fns

!===============================================================================

end module syntran__intr_fns_io_m

!===============================================================================

