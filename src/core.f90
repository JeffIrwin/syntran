
!===============================================================================

module syntran__core_m

	! This module contains private members.  For the public API, see syntran.f90

	use iso_fortran_env

	use syntran__compiler_m
	use syntran__consts_m
	use syntran__errors_m
	use syntran__eval_m
	use syntran__intr_fns_m
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
		syntran_major =  1, &
		syntran_minor =  0, &
		syntran_patch =  0

	! TODO:
	!  - fn type checking bug:
	!
	!      // TODO:  something about myfn() being defined below and maybe abs()
	!      // being overloaded makes this fail the type checker:
	!      println(abs(myfn(7)));
	!      fn myfn(a: i32): i32
	!      {
	!      	return a + 1;
	!      }
	!
	!  - document recommendation of `ulimit -s unlimited`
	!    * aoc 2018 day 17 crashed without this. helps with large (~hundreds)
	!      recursion depth
	!  - built-in syntran update:
	!    * add checksum verification
	!    * currently ./syup.sh can do it
	!    * make this built-in to syntran binary, maybe invoke like
	!      `syntran --update`.  think carefully. don't want to break this by
	!      changing the name of the script or arg
	!    * this would only work on linux, since windows can't overwrite a
	!      running exe
	!    * apparently it's possible on windows too.  til:  https://stackoverflow.com/a/459860/4347028
	!  - post 1.0:
	!    * need to think about namespaces, at least for std fns so i can add
	!      intrinsic fns without breaking anyone's code who happened to already
	!      define a fn with the same name
	!      + example: define a `println` fn.  it won't parse
	!      + note that you *are* allowed to define a `sum` fn because the
	!        intrinsic sum fn is overloaded.  all of the actual intrinsic sums
	!        are named like `0sum_i32` or `0sum_f64`
	!        > could this be abused to add new secret fns starting with "0"
	!          without breaking compat? seems like a bad idea
	!        > it also seems bad that users can shadow define `sum` or any other
	!          overloaded intrinsic
	!  - enable plugging in to nvim linting.  doesn't seem hard from the way
	!    that gfortran nvim linting works.  just need to add a cmd arg like
	!    `--syntax-only` and print errors in 1 line per error, with filename,
	!    line, and column indices
	!  - test rocky 10 circa May 2025, that's when they're planning to release
	!    it
	!  - appimage?  some kind of binary packaging improvement
	!    * the current dependence on libquadmath.so (and sometimes
	!      libgfortran.so) is not ideal, especially considering that rocky is
	!      worse than ubuntu.  it would be nice if everything was truly
	!      statically bundled into one file
	!    * is appimage the standard tool for this?  how does fpm do it?
	!  - add recursive fibonacci sample to syntran-explorer
	!  - REPL styling
	!    * any other ideas from julia?  got their green prompt
	!    * could later extend with hint levels (off, semicolon-only, or fully on)
	!      set by an env var, but that isn't pressing
	!  - add tests that cover interactive interpreter REPL
	!    * added a couple basic tests in main.yml
	!    * fns should also be covered
	!    * should also cover options like `-i` (startup include file)
	!  - recursive data structs?
	!    * recursive fns are available, but not structs
	!  - callbacks, fn pointers, i.e. passing one function as an argument to
	!    another function
	!    * this could be a big change to the type system
	!  - minloc, maxloc, findloc fns
	!  - optional `dim` and/or `mask` args for intrn fns, e.g. sum, minval, any,
	!    etc.
	!    * just use `reshape` and call the fortran built-in.  no need for any
	!      slice logic on my end
	!    * "all" that needs to be done is resolve overloads based on which args
	!       are present, call fortran build-in with reshape to the appropriate
	!       size, and then set the result data along with size/rank meta-data
	!  - using `in` (a keyword) as a fn arg name crashes the parser
	!  - mention syntran explorer in readme
	!    * note it may not exist in ~6 months
	!  - print improvements:
	!    * hex format printing
	!    * formatted printf?
	!      + fully general formatting sounds hard
	!    * print (without newline)
	!      + can be worked around with str() to build a line and then println
	!    * maybe strhex() as a stopgap?
	!    * bin/oct too
	!  - installer packaging:
	!    * bin exists.  nest it in a `bin` folder
	!    * add build os to `--version` output, to distinguish between rocky vs
	!      ubuntu builds
	!    * add version summary as a text file (major.minor.patch, git hash,
	!      build date)
	!    * list sha256 checksums of binaries somewhere
	!    * doc. autogenerate pdf and/or html from markdown via pandoc or similar
	!      + see build-doc.sh
	!    * readme?
	!    * samples?
	!    * libsyntran.a and fortran sample?
	!    * try -static-libgcc etc. on win/mac to ease packaging
	!  - implicit upper bound slicing:
	!    * vec[3:] is equivalent to vec[3: size(vec,0)]
	!    * step sub too:  vec[3: 2: ] or vec[:-1 : -1] (reverse whole vec).
	!      lbound is implicit for negative step
	!    * negative bound indices to count from end, like python? imo this is
	!      bad for bounds checking (which doesn't exist yet outside of debug
	!      builds) and it could let bugs return garbage instead of crashing.  i
	!      think crashing is the correct behavior for index bugs
	!  - struct array slicing:
	!    * can't do my_struct.arr[1:4], currently have to loop with scalar index
	!  - struct member fns
	!    * it would be a nicer experience to be able to write
	!      `dict.set("key", val)` instead of `set_dict_i64(&dict, "key", val)`,
	!      effectively using the struct identifier as a namespace
	!    * inside member fns, you should be allowed to just write `var` instead
	!      of `this.var`
	!  - pass by reference for subscripted array name expressions and dot
	!    expressions
	!    * done for regular variable name expressions
	!    * needs documentation
	!    * samples could be updated, e.g. sorting in place would be a lot more
	!      efficient without copying large array vals in and out.  don't update
	!      tests though.  if anything, add tests, but don't remove coverage
	!  - raw string literals
	!    * easier to include quotes without doubling
	!    * follow rust style:
	!      + let str1 = r#" my raw str with "quotes" "#;
	!      + let str2 = r##" my raw str with "#quotes"# "##;
	!      + number of hashes at start matches end
	!      + any use for zero hashes?  r"raw str"
	!  - size() fn should optionally not need a 2nd argument for dim. in this
	!    case, return product of extents of all dims (useful especially for vecs)
	!  - type() or typeof() fn to get type name as str?  could be useful for
	!    debugging, but I don't want to encourage its use for actual program
	!    logic
	!  - complex number type(s)
	!    * basically required for FFT
	!  - f64
	!    * make a fn to cast f64 down to f32
	!    * casting from f32 up to f64 (or from int to float) is easy, just
	!      multiply by 1.0
	!  - special `ans` variable for REPL only, like matlab, julia, etc?
	!    * it would have to be dynamically typed, changing depending on whatever
	!      the type of the last statement is
	!    * it would also have to be reserved from overriding with a regular
	!      variable with the same name `ans`, at least within the REPL and maybe
	!      everywhere for compatibility of being able to paste code into the
	!      REPL
	!    * it would be very convenient.  you can always rlwrap and up arrow,
	!      then assign to a var if you need to save the last ans.  but this is
	!      inconvenient, and state changers are not always idempotent (meaning
	!      up arrow could give a different answer on 2nd execution).  and if a
	!      statement is expensive, you may not want to have to run it again
	!  - structs
	!    * post-merge TODO struct items:
	!      + update struct sample.  include struct/array combos, nesting, etc.
	!        maybe make separate simple and compound struct samples
	!    * tbd:
	!      + improved to_str() conversion with labels of struct name and member names
	!  - jumping control flow:
	!    * break and continue need documentation
	!    * goto: useful to break nested loops? or add break with loop label
	!    * done:
	!      > return, break, continue
	!      > (sys) exit done
	!        + should final return value be used as an implicit sys exit value?
	!          currently, default exit stat is 0, regardless of what syntran
	!          "main" returns.  what about non-int return vals?
	!  - consider using subroutines with out-args instead of fn return vals for
	!    parse_*() fns?  i believe this is the source of segfaults for gfortran
	!    8. subroutines allow passing-by-reference instead of requiring a copy
	!    of a complex syntax_node_t type on return.  would this eliminate all
	!    node copies?  it could be a lot of work, gfortran 8 might still
	!    segfault, and code will be uglier with out-args instead of return vals.
	!    is copying syntax_node_t a perf bottleneck? i suspect that eval is
	!    bottleneck and not parsing, but i haven't actually benchmarked poor
	!    perf of intel compilers for AOC solution tests
	!  - hacker sdk:
	!    * bitwise operations
	!      > done
	!      > according to c (and fortran), right operand of shift should be
	!        non-negative.  c says it's undefined behavior to shift negative.
	!        is there anything i should catch?  it would have to be a runtime
	!        check if any.  actual result seems to shift circularly
	!      > according to msvc, operands of bitwise and, xor, etc. can have
	!        different sizes. seems dangerous.  should it be allowed? what does
	!        rust do?
	!          https://learn.microsoft.com/en-us/cpp/c-language/c-bitwise-operators?view=msvc-170
	!    * reinterpret cast -- used in quake fast inverse sqrt algo
	!    * done:
	!      > shifting: << and >>
	!      > and: &
	!      > or : |
	!      > xor: ^
	!      > not: ! (rust) or ~ (c)?
	!        + i think c uses ~ because ! is already logical not, which can cast
	!          ints to bools.  i'll prefer the rust style operator
	!    * these features are especially useful when implementing encryption,
	!      hashing, utf, base64, and rng algorithms
	!  - add more tests for lhs slicing
	!    * str, bool, and i64 need testing
	!  - cmd args
	!    * args would be useful for logo sample, e.g. image size and some
	!      control color options
	!    * pass after a ` -- `?
	!    * related: environment variables
	!  - array operations:
	!    * done: element-wise add, sub, mul, div, pow, mod
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
	!    * hex/bin/oct literals
	!    * bitwise ops
	!    * array comparison
	!    * array arithmetic and boolean operations, test and doc array compound
	!      assignment
	!    * new generalized for loops
	!    * compound `**=` assignment, %=
	!    * -c arg, shebang
	!      + this is shown in `--help` output in readme but not further
	!        explained
	!    * --fmax-errors arg
	!    * ifx/ifort pass tests but perform order of magnitude slower
	!  - #(pragma)once  directive. #let var=val directive?
	!    * maybe have an `const` qualifier instead of #let or #define
	!    * for #once include guards, insert filename path as key into a ternary
	!      tree w/ bool value true.  then when something is included, check if
	!      it's in the ternary dict first.
	!    * any use cases for #let? i probably don't want to get into general
	!      expression parsing like `#let x = 1 + 2;` during preprocessing
	!  - str comparison operations:
	!    * >, <, etc. via lexicographical ordering? careful w/ strs that have
	!      matching leading chars but diff lens
	!    * also careful with fortran padding strs with spaces before
	!      comparisons. i had a long-standing bug where "a" == "a " was true
	!      because that's how fortran works :(
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
	!    * intrinsic
	!      + trig: atan2, (sec, cos, hyperbolic, ... ?)
	!      + bessel_jn
	!      + gamma, log_gamma?
	!      + reshape
	!      + system: multiple out args? iostat and stdout
	!    * done:
	!      + abs, sqrt
	!      + exp
	!        > need documentation for elemental array overloading
	!      + log, log10, log2
	!      + norm2, dot
	!        > boolean dot not implemented yet
	!      + sind, cosd, tand, asind, acosd, atand
	!      + sin, cos, tan, asin, acos, atan
	!        > needs documentation
	!      + sum, product
	!        > these need an optional `dim` arg, and so do any/all
	!      + min, max
	!      + size (non-variadic but polymorphic)
	!      + readln, writeln, println, open, close, str casting
	!      + len (of str)
	!      + recursive and non-recursive user-defined fns
	!  - use more submodules
	!    * types.f90 is long and close to leaves of dependency tree.  value.f90
	!      is also highly depended upon
	!  - make syntax highlighting plugins for vim and TextMate (VSCode et al.)
	!    * using rust syntrax highlighting in neovim is not bad (except for "\" string)
	!  - enums
	!  - casting fns should work with array args
	!    * f32() doesn't exist (you can mul by 1.0 as a workaround)
	!    * i32(), i64() done
	!  - char type?
	!    * there's not really anything that you can't do with str instead of
	!      char
	!    * could improve type-checking in situations where you have a fn that
	!      only really expects 1 char
	!    * strs have metadata (i think) in fortran, i.e. length.  for a large
	!      array where each element is a single char this is not efficient,
	!      probably 9x overhead if str/char is 1-byte and its length metadata is
	!      8-bytes
	!    * ascii only. utf out of scope
	!  - other types?
	!    * i16, i8
	!    * any diff between i8 and char?
	!    * unsigned? probably difficult without unsigneds existing in fortran
	!  - literals
	!    * done
	!    * some lex error messages might be improvable
	!    * hex float literals?  not worthwhile imo, but c(++) and golang have them
	!  - built-in dictionary or hash map type?
	!    * could have syntactic sugar like `dict["key"] = val`
	!    * would perform better than syntran-implemented dicts
	!    * how to handle many permutations of key/val types? templates?
	!  - split doc into multiple README's, add TOC, cross-linking, etc.  Only
	!    include quick-start and links in top-level README?
	!    * github automatically includes a Table of Contents in a menu, so maybe
	!      it's better as-is
	!  - minval, maxval fns
	!    * done
	!    * until now, i had delayed these over confusion about whether
	!      min/minval should be different
	!    * these *should* be different from min() and max() fns.  min and max
	!      should be elemental, while minval and maxval condense an array to a
	!      scalar (or a lower-rank array with the opt dim arg).  i now see the
	!      beauty of fortran's design here, while i previously wondered why min
	!      and minval weren't one fn
	!    * if min and minval were the same fn, what would they do with an array
	!      arg?
	!    * maybe you could auto invoke minval when there is only 1 arg, but this
	!      gets complicated with the opt dim/mask args.  you would have to very
	!      carefully resolve the overload based on type/rank of several args
	!  - recursive fns
	!    * done
	!    * could always use more tests.  recursive quicksort?  various aoc
	!      problems?  minheap or heapsort or something tree/graph related?
	!      + "src/tests/long/aoc/2017/12/" now covers recursive DFS, in addition
	!      to everything in "src/tests/test-src/recursion/"
	!    * i think include files can call fns from the includer.  this
	!      probably isn't desireable, but i'm not sure how big of a deal it is
	!      or how to fix it.  right now everything is one translation unit.
	!      might want to rethink scoping
	!  - file open() modes
	!    + done mvp
	!    + add more modes, like "b" for binary and "a" for append
	!    + python style.  open("file.txt", "r"), open("file.txt", "w"),
	!      open("file.txt", "rb"), etc.
	!      * default to read with an optional mode arg would be nice, but i
	!        don't have opt args.  should be possible to add as a later
	!        feature without breaking compat
	!    + should readln() take a ref?  the file is technically an in/out arg
	!      since it will set eof.  compat break.  close() also modifies its
	!      arg as an out arg, but not writeln().  on the other hand, i like
	!      keeping it simple and not making users think about references for
	!      built-in fns. in rust, writing surprisingly requires a mutable ref:
	!      `writeln!(&mut f, "{i}")?;`
	!
	!****************************************

	! A note on naming: Immo calls '==' equals_equals_token, but I think that
	! invites tab-completion mistakes so I went with eequals_token.  Same for
	! sstar_token (and upcoming pplus_token, mminus_token, etc.)

!===============================================================================

contains

!===============================================================================

function syntax_parse(str, vars, fns, src_file, allow_continue, repl) result(tree)

	! TODO: take state struct instead of separate vars and fns members

	! TODO: take structs arg (like existing fns arg)

	character(len = *) :: str

	type(vars_t), intent(inout) :: vars

	type(fns_t), intent(inout) :: fns

	type(syntax_node_t) :: tree

	character(len = *), optional, intent(in)  :: src_file

	logical, intent(in), optional :: allow_continue, repl

	!********

	character(len = :), allocatable :: src_filel, fn_name

	integer :: i, io, dummy, unit_

	logical :: allow_continuel, repll

	type(text_context_vector_t) :: contexts

	type(fn_t) :: fn
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
	!	fns%dict%root%split_char, &
	!	fns%dict%root%mid%split_char, &
	!	fns%dict%root%mid%mid%split_char

	src_filel = '<stdin>'
	if (present(src_file)) src_filel = src_file

	allow_continuel = .false.
	if (present(allow_continue)) allow_continuel = allow_continue

	repll = .true.
	if (present(repl)) repll = repl

	! TODO: unit_ is just synonymous with contexts%len_ in many places, so it
	! can be removed (but not in all places)
	contexts = new_context_vector()
	unit_ = 0

	parser = new_parser(str, src_filel, contexts, unit_)
	!print *, 'units = ', parser%tokens(:)%unit_

	parser%repl = repll

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

			allocate(vars0%dicts(1))
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
	if (allocated(fns%dict%root)) then

		allocate(fns0%dict%root)
		fns0%dict%root = fns%dict%root

		!print *, 'fns%fns = '
		!do i = 1, size(fns%fns)
		!	print *, fns%fns(i)%to_str()
		!end do

		!! With intrinsic fns, this is always allocated

		!if (allocated(fns%fns)) then
			!print *, 'copy fns'

			!fns0%fns = fns%fns
			allocate(fns0%fns( size(fns%fns) ))
			do i = 1, size(fns%fns)
				fns0%fns(i) = fns%fns(i)
			end do

			parser%num_fns = size(fns%fns)
		!else
		!	parser%num_fns = 0
		!end if

		!print *, 'parser%num_fns = ', parser%num_fns

		! Only the 1st scope level matters from interpreter.  It doesn't
		! evaluate until the block is finished
		call move_alloc(fns%dict%root, parser%fns%dict%root)
		if (allocated(fns%fns)) call move_alloc(fns%fns          , parser%fns%fns)

	end if

	!print *, "allocated parser%vars%dicts = ", allocated( parser%vars%dicts )
	!print *, "size parser%vars%dicts = ", size( parser%vars%dicts )

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

		if (allocated(vars0%dicts)) then
			if (allocated(vars0%dicts(1)%root)) then
				call move_alloc(vars0%dicts(1)%root, vars%dicts(1)%root)
			end if
		end if

		if (allocated(vars0%vals)) then
			!print *, 'restoring vars%vals from vars0%vals'
			call move_alloc(vars0%vals         , vars%vals)
		end if

		if (allocated(fns0%dict%root)) then
			call move_alloc(fns0%dict%root, fns%dict%root)
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
	if (allocated(parser%fns%dict%root)) then
		call move_alloc(parser%fns%dict%root, fns%dict%root)

		!! I tried adding this while working on recursive fn lookup but it's not
		!! the way
		!call move_alloc(parser%fns%fns          , fns%fns)

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

		!vars%vals( 1: size(vars0%vals) ) = vars0%vals
		do i = 1, size(vars0%vals)
			vars%vals(i) = vars0%vals(i)
		end do

		!vars%vals = vars0%vals
		!vars = vars0
	end if

	!print *, 'parser%num_fns = ', parser%num_fns
	if (allocated(fns%fns)) deallocate(fns%fns)
	allocate(fns%fns( parser%num_fns ))

	if (allocated(fns0%fns)) then

		!fns%fns( 1: size(fns0%fns) ) = fns0%fns
		do i = 1, size(fns0%fns)
			fns%fns(i) = fns0%fns(i)
		end do

	end if

	! Save flat fn array `fns%fns` with a one-time dict lookup.  There's not any
	! actual fns%fns%node info in what is set above

	!print *, "num intr fns = ", fns%num_intr_fns
	do i = 1, parser%fn_names%len_
		fn_name = parser%fn_names%v(i)%s
		!print *, "fn name = ", fn_name

		! User-defined fns are in the table after all of the intrinsic fns, so
		! shift its index by num_intr_fns
		fn = fns%search(fn_name, dummy, io)
		fns%fns( fns%num_intr_fns + i ) = fn

	end do
	!print *, "done looking up fns"

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

