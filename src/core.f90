
!===============================================================================

module syntran__core_m

	! This module contains private members.  For the public API, see syntran.f90

	use iso_fortran_env

	use syntran__compiler_m
	use syntran__consts_m
	use syntran__errors_m
	use syntran__eval_m
	use syntran__parse_m
	use syntran__types_m
	use syntran__utils_m
	use syntran__value_m

	implicit none

	! Syntax translator (think FORmula TRANslator)
	!
	! I mean what could she have?  Fungus?
	character(len = *), parameter :: lang_name = "syntran"

	integer, parameter ::   &
		syntran_major =  0, &
		syntran_minor =  0, &
		syntran_patch =  50

	! TODO:
	!  - syntran crashes when trying to interpret quine.f90 (fortran!) as
	!    syntran
	!    * d'oh, but shouldn't crash at least
	!  - add version summary as a text file to release packages
	!    * doc. readme? samples?
	!    * try -static-libgcc etc. on win/mac to ease packaging
	!    * ship syntran.a static lib? don't know who would want that
	!  - structs
	!    * post-merge TODO struct items:
	!      + update struct sample.  include struct/array combos, nesting, etc.
	!        maybe make separate simple and compound struct samples
	!      + note in docs that structs don't work in interactive interpreter
	!    * mvp done:
	!      + struct fn return values
	!      + nested structs
	!      + structs of arrays (and arrays of structs)
	!        * string indexing tbd. should be do-able
	!          + was done in a reverted commit before i re-focused on struct/array combos
	!        * slice indexing tbd (more difficult)
	!      + unary ops on dot exprs
	!        * i had broken and fixed binary ops at some point, but i think i
	!          changed things later which automatically fixed unary ops?
	!    * tbd:
	!      + remove unused vars per cmake warnings
	!      + improved to_str() conversion with labels of struct name and member names
	!  - jumping control flow:
	!    * fn return statement done
	!    * c continue (fortran cycle), c break (fortran loop exit)
	!    * (sys) exit done
	!      + should final return value be used as an implicit sys exit value?
	!        currently, default exit stat is 0, regardless of what syntran
	!        "main" returns
	!  - consider using subroutines with out-args instead of fn return vals for
	!    parse_*() fns?  i believe this is the source of segfaults for gfortran
	!    8 and maybe 13.  subroutines allow passing-by-reference instead of
	!    requiring a copy of a complex syntax_node_t type on return.  would this
	!    eliminate all node copies?  it could be a lot of work, gfortran 8 might
	!    still segfault, and code will be uglier with out-args instead of return
	!    vals. is copying syntax_node_t a perf bottleneck? i suspect that eval
	!    is bottleneck and not parsing, but i haven't actually benchmarked poor
	!    perf of intel compilers for AOC solution tests
	!  - hacker sdk:
	!    * bitwise operations: shift left and right, and, or, not, xor
	!    * hex, binary, (and octal) literals
	!    * these features are especially useful when implementing encryption,
	!      hashing, utf, and rng algorithms
	!  - add more tests for lhs slicing
	!    * str, bool, and i64 need testing
	!    * write another wave equation sample using slicing and array operations
	!  - block_statement eval had a bug which is now fixed by setting tmp =
	!    syntax_eval() in case of a block which ends with a null if_statement.
	!    check translation_unit and other eval case branches for similar bugs
	!    * maybe fixed after syntax_eval() refactor?  unknown_type is
	!      initialized inside the value_t struct declaration
	!  - cmd args
	!    * args would be useful for logo sample, e.g. image size and some
	!      control color options
	!    * related: environment variables
	!  - check assignment to void type? guard against things like
	!    `let x = println();`
	!    * did i allow this to stop cascading errors?  i think i used
	!      unknown_type for that
	!  - array concatenation:
	!        let arr1 = [0: 4];
	!        let arr2 = [4: 8];
	!        let cat  = [arr1, arr2]; // [0: 8]
	!  - rethink open() fn.  add a mode.  read mode should check if file exists
	!  - array operations:
	!    * element-wise add, sub, mul, div, pow, mod done
	!      + compound array assignment works but needs unit tests
	!    * vector dot product, matrix mul, ...
	!      + new operator tokens, something like `.*`?
	!      + maybe `:*`. I like that I can just hold shift while typing it
	!        unlike MATLAB's `.*` and it's shorter than R's `%*%`
	!    * optional `dim` argument for any() and all(). 1 arg versions done
	!    * comparisons done
	!    * array and, or, not, done
	!    * unary -, + done
	!  - document recent features:
	!    * casting?
	!    * array comparison
	!    * array arithmetic and boolean operations, test and doc array compound
	!      assignment
	!    * caveat about return val from nested slice compound assignemnt:
	!          `let u = (v[3: 7] += 7);`
	!    * any(), all(), count(), sum(), i32(array), exit(), polymorphic
	!      min/max intrinsics, parse_f32(), parse_i64() intrinsic
	!    * new generalized for loops
	!    * compound `**=` assignment, %=
	!    * --help arg
	!    * -c arg, shebang
	!    * --fmax-errors arg
	!    * ifx/ifort pass tests but perform order of magnitude slower
	!  - add a workflow that tests gfortran version 8 (and/or older?).  older
	!    versions don't allow a user-defined type that references another type
	!    which is defined below it
	!    * added a matrix for gfortran 9 through 12
	!    * 8 isn't installed.  maybe i can install it in workflow?
	!    * tried "setup-fortran" marketplace action but it can't install 8
	!      either
	!  - pass by reference?  big boost to perf for array fns.  should be
	!    possible by swapping around some id_index values in vars%vals array.
	!    harder part is ensuring that only lvalues are passed by ref (not
	!    rvalues), e.g. `my_fn(x)` is allowed but `my_fn(x+1)` is not if arg is
	!    passed by ref
	!  - #(pragma)once  directive. #let var=val directive?
	!    * for #once include guards, insert filename path as key into a ternary
	!      tree w/ bool value true.  then when something is included, check if
	!      it's in the ternary dict first.
	!    * any use cases for #let? i probbaly don't want to get into general
	!      expression parsing like `#let x = 1 + 2;` during preprocessing
	!  - str comparison operations:
	!    * >, <, etc. via lexicographical ordering? careful w/ strs that have
	!      matching leading chars but diff lens
	!    * ==, !=:  done
	!  - fuzz testing
	!  - substring indexing and slicing:
	!    * string arrays get an optional extra rank.  omitting the extra rank
	!      refers to the whole string at that position in the array:
	!      + str_vec[0] == str_vec[:,0]
	!      + str_mat[0,0] == str_mat[:,0,0]
	!      + etc.
	!    * first, single-character indexing
	!      + done
	!    * then, range-based slicing
	!      + done
	!  - file reading/writing
	!    * binary file i/o
	!    * vectorized writes (and reads) for arrays without syntran loops. c.f.
	!      vectorized wave sample (all math is vectorized but writes are not).
	!      need to strip bracket [] wrapping and comma delimiters
	!    * readln(), eof() done
	!    * also add a file_stat() fn which checks IO of previous file operation.
	!      this way I don't need to add structs, multiple return vals, or out
	!      args yet
	!  - compound assignment: logical &=, |=, etc.
	!    * +=, -=, *=, /=, %=, **= done
	!  - ++, --
	!  - tetration operator ***? ints only? just for fun
	!  - functions
	!    * check return value is correct type.  return statements could help
	!      with this
	!    * intrinsic
	!      + abs, norm, dot
	!      + log
	!      + trig: sin, cos, tan, asin, ...
	!      + norm, product
	!      + reshape
	!      + system: multiple out args? iostat and stdout
	!    * recursive user-defined fns
	!    * done:
	!      + exp  (non-variadic, non-polymorphic)
	!      + min, max, sum
	!      + size (non-variadic but polymorphic)
	!      + readln, writeln, println, open, close, str casting
	!      + len (of str)
	!      + non-recursive user-defined fns
	!  - use more submodules
	!    * types.f90 is long and close to leaves of dependency tree.  value.f90
	!      is also highly depended upon
	!  - make syntax highlighting plugins for vim and TextMate (VSCode et al.)
	!    * using rust syntrax highlighting in neovim is not bad (except for "\" string)
	!  - enums
	!  - file reading
	!    * file_stat() fn: checks IO of previous file operation. this way I
	!      don't need to add structs, multiple return vals, or out args yet
	!    * readln(), eof() done
	!  - casting fns should work with array args
	!    * f32() doesn't exist (you can mul by 1.0 as a workaround)
	!    * i32(), i64() done
	!  - xor, xnor
	!    * xor (bool1, bool2) is just (bool1 != bool2)
	!    * xnor(bool1, bool2) is just (bool1 == bool2)
	!    * is there any value to having plain language versions of these
	!      operators, like `and` or `not` in syntran?
	!  - split doc into multiple README's, add TOC, cross-linking, etc.  Only
	!    include quick-start and links in top-level README?
	!    * github automatically includes a Table of Contents in a menu, so maybe
	!      it's better as-is
	!
	!****************************************

	! A note on naming: Immo calls '==' equals_equals_token, but I think that
	! invites tab-completion mistakes so I went with eequals_token.  Same for
	! sstar_token (and upcoming pplus_token, mminus_token, etc.)

!===============================================================================

contains

!===============================================================================

subroutine declare_intrinsic_fns(fns)

	type(fns_t), intent(out) :: fns

	!********

	integer :: id_index, num_fns

	type(fn_t) :: exp_fn, min_i32_fn, max_i32_fn, println_fn, size_fn, open_fn, &
		close_fn, readln_fn, writeln_fn, str_fn, eof_fn, parse_i32_fn, len_fn, &
		i64_sca_fn, parse_i64_fn, i32_sca_fn, exit_fn, any_fn, all_fn, count_fn, &
		min_i64_fn, max_i64_fn, i32_arr_fn, i64_arr_fn, sum_i32_fn, &
		sum_f32_fn, sum_i64_fn, parse_f32_fn, min_f32_fn, max_f32_fn

	! Increment index for each fn and then set num_fns
	id_index = 0

	!********

	! TODO: polymorphic in f32, f64, etc.
	exp_fn%type%type = f32_type
	allocate(exp_fn%params(1))
	allocate(exp_fn%param_names%v(1))
	exp_fn%params(1)%type = f32_type
	exp_fn%param_names%v(1)%s = "x"

	! Insert the fn into the dict. These are global intrinsic fns, so there's no
	! need to check iostat

	call fns%insert("exp", exp_fn, id_index)

	!********

	! We could make max() and min() work with just 1 argument too.  I'm not sure
	! why you would want to be able to take the max of 1 number, but it seems
	! like an arbitrary limitation.  Anyway we follow the Fortran convention
	! here

	! TODO: add an array version min(array) as opposed to Fortran's minval()
	!
	! In Fortran, min() is polymorphic and variadic, but all args must be the
	! same type.  For example, min(1, 2) and min(1.1, 2.1) are allowed, but
	! min(1, 2.1) does not compile.  I think that's a reasonable restriction

	! TODO: min_f64_fn, max_f64_fn

	min_i32_fn%type%type = i32_type
	allocate(min_i32_fn%params(2))
	allocate(min_i32_fn%param_names%v(2))

	min_i32_fn%params(1)%type = i32_type
	min_i32_fn%param_names%v(1)%s = "a0"

	min_i32_fn%params(2)%type = i32_type
	min_i32_fn%param_names%v(2)%s = "a1"

	min_i32_fn%variadic_min  = 0
	min_i32_fn%variadic_type = i32_type

	! Internal overloaded name starts with a "0" because this would be illegal
	! for user-defined fn's, so there can never be a clash
	call fns%insert("0min_i32", min_i32_fn, id_index)

	!********

	min_i64_fn%type%type = i64_type
	allocate(min_i64_fn%params(2))
	allocate(min_i64_fn%param_names%v(2))

	min_i64_fn%params(1)%type = i64_type
	min_i64_fn%param_names%v(1)%s = "a0"

	min_i64_fn%params(2)%type = i64_type
	min_i64_fn%param_names%v(2)%s = "a1"

	min_i64_fn%variadic_min  = 0
	min_i64_fn%variadic_type = i64_type

	call fns%insert("0min_i64", min_i64_fn, id_index)

	!********

	min_f32_fn%type%type = f32_type
	allocate(min_f32_fn%params(2))
	allocate(min_f32_fn%param_names%v(2))

	min_f32_fn%params(1)%type = f32_type
	min_f32_fn%param_names%v(1)%s = "a0"

	min_f32_fn%params(2)%type = f32_type
	min_f32_fn%param_names%v(2)%s = "a1"

	min_f32_fn%variadic_min  = 0
	min_f32_fn%variadic_type = f32_type

	call fns%insert("0min_f32", min_f32_fn, id_index)

	!********

	max_i32_fn%type%type = i32_type
	allocate(max_i32_fn%params(2))
	allocate(max_i32_fn%param_names%v(2))

	max_i32_fn%params(1)%type = i32_type
	max_i32_fn%param_names%v(1)%s = "a0"

	max_i32_fn%params(2)%type = i32_type
	max_i32_fn%param_names%v(2)%s = "a1"

	max_i32_fn%variadic_min  = 0
	max_i32_fn%variadic_type = i32_type

	! Internal overloaded name starts with a "0" because this would be illegal
	! for user-defined fn's, so there can never be a clash
	call fns%insert("0max_i32", max_i32_fn, id_index)

	!********

	max_i64_fn%type%type = i64_type
	allocate(max_i64_fn%params(2))
	allocate(max_i64_fn%param_names%v(2))

	max_i64_fn%params(1)%type = i64_type
	max_i64_fn%param_names%v(1)%s = "a0"

	max_i64_fn%params(2)%type = i64_type
	max_i64_fn%param_names%v(2)%s = "a1"

	max_i64_fn%variadic_min  = 0
	max_i64_fn%variadic_type = i64_type

	call fns%insert("0max_i64", max_i64_fn, id_index)

	!********

	max_f32_fn%type%type = f32_type
	allocate(max_f32_fn%params(2))
	allocate(max_f32_fn%param_names%v(2))

	max_f32_fn%params(1)%type = f32_type
	max_f32_fn%param_names%v(1)%s = "a0"

	max_f32_fn%params(2)%type = f32_type
	max_f32_fn%param_names%v(2)%s = "a1"

	max_f32_fn%variadic_min  = 0
	max_f32_fn%variadic_type = f32_type

	call fns%insert("0max_f32", max_f32_fn, id_index)

	!********

	! TODO: update docs to use println() instead of old holyc implicit prints

	println_fn%type%type = void_type ! TODO?

	allocate(println_fn%params(0))
	allocate(println_fn%param_names%v(0))

	println_fn%variadic_min  = 0
	println_fn%variadic_type = any_type

	call fns%insert("println", println_fn, id_index)

	!********

	str_fn%type%type = str_type

	allocate(str_fn%params(0))
	allocate(str_fn%param_names%v(0))

	str_fn%variadic_min  = 0
	str_fn%variadic_type = any_type

	call fns%insert("str", str_fn, id_index)

	!********

	len_fn%type%type = i64_type
	allocate(len_fn%params(1))
	allocate(len_fn%param_names%v(1))
	len_fn%params(1)%type = str_type
	len_fn%param_names%v(1)%s = "str"

	call fns%insert("len", len_fn, id_index)

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
	allocate(open_fn%params(1))
	allocate(open_fn%param_names%v(1))
	open_fn%params(1)%type = str_type
	open_fn%param_names%v(1)%s = "filename"

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

	size_fn%type%type = i64_type
	allocate(size_fn%params(2))
	allocate(size_fn%param_names%v(2))

	size_fn%params(1)%type = array_type

	allocate(size_fn%params(1)%array)
	size_fn%params(1)%array%type = any_type
	size_fn%params(1)%array%rank = -1  ! negative means any rank

	size_fn%param_names%v(1)%s = "array"

	size_fn%params(2)%type = i32_type
	size_fn%param_names%v(2)%s = "dim"

	call fns%insert("size", size_fn, id_index)

	! It might also be useful to make size() variadic and have size(array)
	! return the product of each dimension's size.  It should just have a single
	! optional param though, not unlimited arity like min/max.

	!********

	count_fn%type%type = i64_type
	allocate(count_fn%params(1))
	allocate(count_fn%param_names%v(1))

	count_fn%params(1)%type = array_type

	allocate(count_fn%params(1)%array)
	count_fn%params(1)%array%type = bool_type
	count_fn%params(1)%array%rank = -1  ! negative means any rank

	count_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to count() like Fortran
	!count_fn%params(2)%type = i32_type
	!count_fn%param_names%v(2)%s = "dim"

	call fns%insert("count", count_fn, id_index)

	!********

	sum_i32_fn%type%type = i32_type
	allocate(sum_i32_fn%params(1))
	allocate(sum_i32_fn%param_names%v(1))

	sum_i32_fn%params(1)%type = array_type

	allocate(sum_i32_fn%params(1)%array)
	sum_i32_fn%params(1)%array%type = i32_type
	sum_i32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i32_fn%param_names%v(1)%s =  "array"

	!! TODO: add mask and dim args to sum() like Fortran.  Maybe overload
	!! several distinct internal fn's like 0min_i32 vs 0min_i64?  The return
	!! value is still the same so maybe there's an easier way
	!sum_i32_fn%params(2)%type = i32_type
	!sum_i32_fn%param_names%v(2)%s = "dim"

	call fns%insert("0sum_i32", sum_i32_fn, id_index)

	!********

	sum_i64_fn%type%type = i64_type
	allocate(sum_i64_fn%params(1))
	allocate(sum_i64_fn%param_names%v(1))

	sum_i64_fn%params(1)%type = array_type

	allocate(sum_i64_fn%params(1)%array)
	sum_i64_fn%params(1)%array%type = i64_type
	sum_i64_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_i64_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_i64", sum_i64_fn, id_index)

	!********

	sum_f32_fn%type%type = f32_type
	allocate(sum_f32_fn%params(1))
	allocate(sum_f32_fn%param_names%v(1))

	sum_f32_fn%params(1)%type = array_type

	allocate(sum_f32_fn%params(1)%array)
	sum_f32_fn%params(1)%array%type = f32_type
	sum_f32_fn%params(1)%array%rank = -1  ! negative means any rank

	sum_f32_fn%param_names%v(1)%s =  "array"

	call fns%insert("0sum_f32", sum_f32_fn, id_index)

	!********

	all_fn%type%type = bool_type
	allocate(all_fn%params(1))
	allocate(all_fn%param_names%v(1))

	all_fn%params(1)%type = array_type

	allocate(all_fn%params(1)%array)
	all_fn%params(1)%array%type = bool_type
	all_fn%params(1)%array%rank = -1  ! negative means any rank

	all_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to all() like Fortran
	!all_fn%params(2)%type = i32_type
	!all_fn%param_names%v(2)%s = "dim"

	call fns%insert("all", all_fn, id_index)

	!********

	any_fn%type%type = bool_type
	allocate(any_fn%params(1))
	allocate(any_fn%param_names%v(1))

	any_fn%params(1)%type = array_type

	allocate(any_fn%params(1)%array)
	any_fn%params(1)%array%type = bool_type
	any_fn%params(1)%array%rank = -1  ! negative means any rank

	any_fn%param_names%v(1)%s = "mask"

	!! TODO: add dim arg to any() like Fortran
	!any_fn%params(2)%type = i32_type
	!any_fn%param_names%v(2)%s = "dim"

	call fns%insert("any", any_fn, id_index)

	!********

	! FIXME: when adding new functions, remember to copy them into the
	! fns%fns(:) array below

	num_fns = id_index
	allocate(fns%fns(num_fns))

	fns%fns = &
		[ &
			min_i32_fn  , &
			min_i64_fn  , &
			min_f32_fn  , &
			max_i32_fn  , &
			max_i64_fn  , &
			max_f32_fn  , &
			println_fn  , &
			str_fn      , &
			len_fn      , &
			parse_i32_fn, &
			parse_i64_fn, &
			parse_f32_fn, &
			i32_sca_fn  , &
			i32_arr_fn  , &
			i64_sca_fn  , &
			i64_arr_fn  , &
			open_fn     , &
			readln_fn   , &
			writeln_fn  , &
			eof_fn      , &
			close_fn    , &
			exit_fn     , &
			size_fn     , &
			count_fn    , &
			sum_i32_fn  , &
			sum_i64_fn  , &
			sum_f32_fn  , &
			all_fn      , &
			any_fn        &
		]

end subroutine declare_intrinsic_fns

!===============================================================================

function syntax_parse(str, vars, fns, src_file, allow_continue) result(tree)

	! TODO: take state struct instead of separate vars and fns members?

	! TODO: take structs arg (like existing fns arg)

	character(len = *) :: str

	type(vars_t), intent(inout) :: vars

	type(fns_t), intent(inout) :: fns

	type(syntax_node_t) :: tree

	character(len = *), optional, intent(in)  :: src_file

	logical, intent(in), optional :: allow_continue

	!********

	character(len = :), allocatable :: src_filel

	integer :: unit_

	logical :: allow_continuel

	type(text_context_vector_t) :: contexts

	type(fns_t) :: fns0

	! This no longer seems to make a difference.  Previously, without `save`,
	! gfortran crashes when this goes out of scope.  Maybe I need to work on a
	! manual finalizer to deallocate ternary trees, not just for structs but for
	! the vars_t trees contained within
	type(parser_t) :: parser
	!type(parser_t), save :: parser

	type(syntax_token_t) :: token

	type(vars_t) :: vars0

	if (debug > 0) print *, 'syntax_parse'
	if (debug > 1) print *, 'str = ', str

	!! "exp"
	!print *, 'key = ', &
	!	fns%dicts(1)%root%split_char, &
	!	fns%dicts(1)%root%mid%split_char, &
	!	fns%dicts(1)%root%mid%mid%split_char

	src_filel = '<stdin>'
	if (present(src_file)) src_filel = src_file

	allow_continuel = .false.
	if (present(allow_continue)) allow_continuel = allow_continue

	! TODO: unit_ is just synonymous with contexts%len_ in many places, so it
	! can be removed (but not in all places)
	contexts = new_context_vector()
	unit_ = 0

	parser = new_parser(str, src_filel, contexts, unit_)
	!print *, 'units = ', parser%tokens(:)%unit_

	! The global scope can return any type.  This is initialized here and not
	! inside new_parser() in case you have half of a function body inside an
	! include file (!)
	parser%fn_type%type = any_type
	allocate(parser%fn_type%array)
	!parser%fn_type = any_type

	! Do nothing for blank lines (or comments)
	if (parser%current_kind() == eof_token) then
		tree%is_empty = .true.
		tree%diagnostics = parser%diagnostics
		return
	end if

	! Point parser member to vars dict.  This could be done in the
	! constructor new_parser(), but it seems reasonable to do it here since it
	! has to be moved back later.  The dict vars0 comes from the
	! interactive interpreter's history, it has nothing to do with scoping

	!print *, 'moving vars'

	!print *, ''
	!print *, 'size(vars%vals) = ', size(vars%vals)

	!print *, 'allocated 1 = ', allocated(vars%dicts(1)%root)
	!print *, 'allocated 2 = ', allocated(vars%dicts(2)%root)
	!print *, 'allocated 3 = ', allocated(vars%dicts(3)%root)

	if (allocated(vars%dicts(1)%root)) then
	!if (any(allocated(vars%dicts(:)%root))) then

		if (allow_continuel) then
			! Backup existing vars.  Only copy for interactive interpreter.
			! This logic is slightly redundant as allow_continuel should _only_
			! be set true for the interactive interpreter with stdin, which is
			! also the only case where vars%root will be allocated.
			! Calling syntran_interpret() on a multi-line string is deprecated,
			! since syntran_eval() can parse it all in one syntax_parse() call.

			! The root type has an overloaded assignment op, but the vars
			! type itself does not (and I don't want to expose or encourage
			! copying)
			allocate(vars0%dicts(1)%root)
			vars0%dicts(1)%root = vars%dicts(1)%root

			!print *, 'vars%vals = '
			!do i = 1, size(vars%vals)
			!	print *, vars%vals(i)%to_str()
			!end do

			! Backup vals array and set num_vars in parser object
			vars0%vals = vars%vals
			parser%num_vars = size(vars%vals)

		end if

		! Only the 1st scope level matters from interpreter.  It doesn't
		! evaluate until the block is finished
		call move_alloc(vars%dicts(1)%root, parser%vars%dicts(1)%root)
		call move_alloc(vars%vals         , parser%vars%vals)

	!else if (parser%num_vars > 0) then
	!else if (size(vars%vals) > 0) then
	!else if (size(vars%vals) > 0 .and. allow_continuel) then
	else if (allocated(vars%vals) .and. allow_continuel) then
		if (size(vars%vals) > 0) then

		! This could probably be refactored but it breaks my brain to think this
		! through and test enough permutations in interactive interpreter

		!print *, 'backing up vars%vals to vars0%vals'
		vars0%vals = vars%vals
		parser%num_vars = size(vars%vals)
		!print *, '1'
		call move_alloc(vars%vals         , parser%vars%vals)
		!print *, '2'

		end if
	end if

	!print *, 'moving fns'
	if (allocated(fns%dicts(1)%root)) then

		allocate(fns0%dicts(1)%root)
		fns0%dicts(1)%root = fns%dicts(1)%root

		!print *, 'fns%fns = '
		!do i = 1, size(fns%fns)
		!	print *, fns%fns(i)%to_str()
		!end do

		!! With intrinsic fns, this is always allocated
		!if (allocated(fns%fns)) then
			!print *, 'copy fns'
			fns0%fns = fns%fns
			parser%num_fns = size(fns%fns)
		!else
		!	parser%num_fns = 0
		!end if

		!print *, 'parser%num_fns = ', parser%num_fns

		! Only the 1st scope level matters from interpreter.  It doesn't
		! evaluate until the block is finished
		call move_alloc(fns%dicts(1)%root, parser%fns%dicts(1)%root)
		if (allocated(fns%fns)) call move_alloc(fns%fns          , parser%fns%fns)

	end if

	!*******************************
	! Parse the tokens
	tree = parser%parse_unit()

	!print *, ""
	!print *, "in core.f90:"
	!print *, "parser structs root     = ", parser%structs%dict%root%split_char
	!print *, "parser structs root mid = ", parser%structs%dict%root%mid%split_char

	!*******************************

	tree%expecting       = parser%expecting
	tree%first_expecting = parser%first_expecting

	if (allocated(parser%first_expected)) then
		tree%first_expected = parser%first_expected
	end if

	if (debug > 1) print *, 'tree = ', tree%str()

	if (tree%expecting .and. allow_continuel) then

		! If expecting more input, don't push diagnostics yet.  Also undo any
		! variable declarations, since they will be re-declared when we continue
		! parsing the current stdin line from its start again.

		if (allocated(vars0%dicts(1)%root)) then
			call move_alloc(vars0%dicts(1)%root, vars%dicts(1)%root)
		end if

		if (allocated(vars0%vals)) then
			!print *, 'restoring vars%vals from vars0%vals'
			call move_alloc(vars0%vals         , vars%vals)
		end if

		if (allocated(fns0%dicts(1)%root)) then
			call move_alloc(fns0%dicts(1)%root, fns%dicts(1)%root)
			call move_alloc(fns0%fns          , fns%fns)
		end if

		return

	end if

	if (debug > 1) print *, 'matching eof'
	token  = parser%match(eof_token)

	tree%diagnostics = parser%diagnostics

	!print *, 'size(parser%vars%vals) = ', size(parser%vars%vals)

	! Move back.  It's possible that vars were empty before this call but not
	! anymore
	if (allocated(parser%vars%dicts(1)%root)) then
	!if (parser%num_vars > 0) then
		call move_alloc(parser%vars%dicts(1)%root, vars%dicts(1)%root)
	end if

	! TODO: if num_fns instead?
	if (allocated(parser%fns%dicts(1)%root)) then
		call move_alloc(parser%fns%dicts(1)%root, fns%dicts(1)%root)
	end if

	! When parsing is finished, we are done with the variable dictionary
	! parser%vars%dicts.  Allocate a flat array for efficient evaluation without
	! dictionary lookups.  Indices in the array are already saved in each node's
	! id_index member

	!print *, 'parser%num_vars = ', parser%num_vars
	if (allocated(vars%vals)) deallocate(vars%vals)
	allocate(vars%vals( parser%num_vars ))

	if (allocated(vars0%vals)) then
		!print *, 'restoring vars%vals'
		vars%vals( 1: size(vars0%vals) ) = vars0%vals
		!vars%vals = vars0%vals
		!vars = vars0
	end if

	!print *, 'parser%num_fns = ', parser%num_fns
	if (allocated(fns%fns)) deallocate(fns%fns)
	allocate(fns%fns( parser%num_fns ))

	if (allocated(fns0%fns)) then
		fns%fns( 1: size(fns0%fns) ) = fns0%fns
	end if

	!if (allocated(parser%structs)) then
	!	! TODO: manually finalize recursively?
	!	deallocate(parser%structs)
	!end if
	!print *, "size = ", size(parser%structs%structs)
	!print *, "allocated = ", allocated(parser%structs%structs)
	!print *, "size = ", size(parser%structs%dicts)
	!print *, "allocated = ", allocated(parser%structs%dict%root)
	!deallocate(parser%structs%dict%root)
	!call struct_ternary_tree_final(parser%structs%dict%root)

	if (debug > 0) print *, 'done syntax_parse'

end function syntax_parse

!===============================================================================

end module syntran__core_m

!===============================================================================

