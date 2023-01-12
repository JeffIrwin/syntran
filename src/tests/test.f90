
module test_m

	! Use short names for convenience
	use syntran, &
		interpret      => syntran_interpret, &
		interpret_file => syntran_interpret_file, &
		eval      => syntran_eval     , &
		eval_int  => syntran_eval_int

	use utils, only: fg_bright_red, fg_bright_green, line_feed, &
		findlocl1, console_color, console_color_reset

	implicit none

contains

!===============================================================================

subroutine unit_test_bin_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'binary arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval_int('1;') == 1, &
			eval_int('69;') == 69, &
			eval_int('420;') == 420, &
			eval_int('1337;') == 1337, &
			eval_int('1 + 2;') == 1 + 2, &
			eval_int('1 + 2 + 34;') == 1 + 2 + 34, &
			eval_int('1 + 2 * 3;') == 1 + 2 * 3, &
			eval_int('1 * 2 * 3 * 4;') == 1 * 2 * 3 * 4, &
			eval_int('73 - 48;') == 73 - 48, &
			eval_int('73 - 48 - 21;') == 73 - 48 - 21, &
			eval_int('24 / 6;') == 24 / 6, &
			eval_int('24 / 6 / 2;') == 24 / 6 / 2, &
			eval_int('2 ** 5;') == 2 ** 5, &
			eval_int('3 ** 4;') == 3 ** 4, &
			eval_int('343 - 87654345 / 27 + 76 * 234 - 65432 / 63;') &
			       == 343 - 87654345 / 27 + 76 * 234 - 65432 / 63 &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bin_arith

!===============================================================================

subroutine unit_test_paren_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'parenthesized arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! In the first 3 tests, the parens don't change the result, but they should
	! at least be parsed
	tests = &
		[   &
			eval_int('(69);') == (69), &
			eval_int('(1) + 2;') == (1) + 2, &
			eval_int('1 + (2 + 34);') == 1 + (2 + 34), &
			eval_int('(1 + 2) * 3;') == (1 + 2) * 3, &
			eval_int('1 * (2 * 3 * 4);') == 1 * (2 * 3 * 4), &
			eval_int('73 - (48);') == 73 - (48), &
			eval_int('73 - (48 - 21);') == 73 - (48 - 21), &
			eval_int('24 / (6 / 2);') == 24 / (6 / 2), &
			eval_int('343 - (87654345 / 27 + 76 * (234 - 65432)) / 63;') &
			   == 343 - (87654345 / 27 + 76 * (234 - 65432)) / 63,  &
			eval_int(  &
				'(1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9 ' &
				//' - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8) ' &
				//' - 9) * 8) + 1 - (2 * (3 + (4 - (5 * 6) / 2)  ' &
				//' + 7) - 8) * 9) + 1) * 42;')   &
				== &
				 (1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9   &
				    - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8)   &
				    - 9) * 8) + 1 - (2 * (3 + (4 - (5 * 6) / 2)    &
				    + 7) - 8) * 9) + 1) * 42     &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_paren_arith

!===============================================================================

subroutine unit_test_unary_arith(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'unary arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! I'm intentionally not testing things like '--69' or '-+-69' since those
	! will conflict with the prefix/postfix increment/decrement operators that
	! I'm planning.  Use parentheses if you want to write that bullshit.
	tests = &
		[   &
			eval_int('-(69);') == -(69), &
			eval_int('(-69);') == (-69), &
			eval_int('+(69);') == +(69), &
			eval_int('(+69);') == (+69), &
			eval_int('(-1) + 2;') == (-1) + 2, &
			eval_int('1 + (+2 + 34);') == 1 + (+2 + 34), &
			eval_int('(1 + 2) * -3;') == (1 + 2) * -3, &
			eval_int('-1 * (2 * -3 * -4);') == -1 * (2 * -3 * -4), &
			eval_int('-73 - (+48);') == -73 - (+48), &
			eval_int('24 / (-6 / 2);') == 24 / (-6 / 2), &
			eval_int('343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63;') &
			       == 343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_unary_arith

!===============================================================================

subroutine unit_test_bool(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'Boolean logic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('true;')  == 'true' , &
			eval('false;') == 'false', &
			eval('not true;')  == 'false', &
			eval('not false;')  == 'true',  &
			eval('not not true;')  == 'true',  &
			eval('not not false;')  == 'false',  &
			eval('false and false;')  == 'false',  &
			eval('true  and false;')  == 'false',  &
			eval('false and true ;')  == 'false',  &
			eval('true  and true ;')  == 'true' ,  &
			eval('false or  false;')  == 'false',  &
			eval('true  or  false;')  == 'true' ,  &
			eval('false or  true ;')  == 'true' ,  &
			eval('true  or  true ;')  == 'true' ,  &
			eval('not true or true  ;')  == 'true' ,  &
			eval('not (true or true);')  == 'false'   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bool

!===============================================================================

subroutine unit_test_comparisons(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'comparisons'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('true  == true ;')  == 'true' ,  &
			eval('false == false;')  == 'true' ,  &
			eval('42    == 1337 ;')  == 'false',  &
			eval('31415 == 31415;')  == 'true' ,  &
			eval('36+7  == 43   ;')  == 'true' ,  &
			eval('12    == 36/3 ;')  == 'true' ,  &
			eval('12    == 36*3 ;')  == 'false',  &
			eval('12    != 36*3 ;')  == 'true' ,  &
			eval('12    != 36/3 ;')  == 'false',  &
			eval('true  != true ;')  == 'false',  &
			eval('false != true ;')  == 'true' ,  &
			eval('true  == false;')  == 'false'   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comparisons

!===============================================================================

subroutine unit_test_assignment(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'assignment'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! We're getting into dangerous Fortran territory here.  I think there's
	! a limit on how many chars and lines can be in a statement, and this may be
	! pushing it

	tests = &
		[   &
			eval('let x = 1 ;') == '1' ,  &
			eval('let a =  let b = 42 ;') == '42' ,  &
			eval('let a = (let b = 42);') == '42' ,  &
			interpret('{ '// &
				'let a = 2;'// &
				'let b = a;}') == '2',     &
			interpret('{'// &
				'let a = 2;'// &
				'let b = a + 1;}') == '3', &
			interpret('{'// &
				'let a = 1;'// &
				'let b = a;'// &
				'a = a + 1;'// &
				'a == 2 and b == 1;}') == 'true', &
			interpret('{'// &
				'let a = 1;'// &
				'let aa = 2;'// &
				'let aaa = 3;'// &
				'let aaaa = 4;'// &
				'a == 1 and aa == 2 and aaa == 3 and aaaa == 4;}') == 'true', &
			interpret('{'// &
				'let aaaa = 4;'// &
				'let aaa = 3;'// &
				'let aa = 2;'// &
				'let a = 1;'// &
				'a == 1 and aa == 2 and aaa == 3 and aaaa == 4;}') == 'true', &
			interpret('{'// &
				'let aaa = 3;'// &
				'let aaaa = 4;'// &
				'let a = 1;'// &
				'let aa = 2;'// &
				'a == 1 and aa == 2 and aaa == 3 and aaaa == 4;}') == 'true', &
			interpret('{'// &
				'let a = 1;'// &
				'let b = 2;'// &
				'let c = 3;'// &
				'let d = 4;'// &
				'a == 1 and b == 2 and c == 3 and d == 4;}') == 'true', &
			interpret('{'// &
				'let d = 4;'// &
				'let c = 3;'// &
				'let b = 2;'// &
				'let a = 1;'// &
				'a == 1 and b == 2 and c == 3 and d == 4;}') == 'true', &
			interpret('{'// &
				'let c = 3;'// &
				'let d = 4;'// &
				'let a = 1;'// &
				'let b = 2;'// &
				'a == 1 and b == 2 and c == 3 and d == 4;}') == 'true', &
			interpret('{'// &
				'let p = true;'// &
				'let q = false;'// &
				'p and not q;}') == 'true', &
			eval('(let a = 10) * a;') == '100' ,  &
			interpret('{'// &
				'let b = (let a = 5) * a;'// &
				'let c = b - a;}') == '20', &
			interpret('{'// &
				'let b = (let a = 5) * a;'// &
				'let d = (let c = b - a) + c;}') == '40', &
			eval('let myVariable = 1337;')  == '1337'   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_assignment

!===============================================================================

subroutine unit_test_comments(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'comments'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret('//;') == '', &
			interpret('// ;')  == '', &
			interpret(' //   ;')  == '', &
			eval('{'//line_feed// &
				'let a = 2;'//line_feed// &
				'a     = 3;'//line_feed// &
				'//a   = 4;'//line_feed// &
				' // a = 5;'//line_feed// &
				'a;}') == '3',     &
			eval('let x = 1337; // set x to thirteen thirty seven')  == '1337' &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comments

!===============================================================================

subroutine unit_test_blocks(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'block statements'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret('{ let a = 0 ; (a = 10) * a ;}') == '100', &
			!
			eval('{ let a = 0 ; (a = 10) * a; }') == '100', &
			!  eval works the same as interpret for blocks
			!
			eval( &
				'{'// &
				'	let a = 0;'// &
				'	(a = 10) * a;'// &
				'}') == '100', &
			!  since we don't need statement delimiters in blocks, we can
			!  now eval multi-line strings
			!
			interpret('{ let a = 1 ; (a =  9) * a;}') == '81'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_blocks

!===============================================================================

subroutine unit_test_if_else(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'if/else statements'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests, and some bad syntax tests for interpret_file().
	! Organize syntran test src files into another level of folders
	!
	! Eventually, do something like Immo's AssertDiagnostic() for bad syntax

	tests = &
		[   &
			interpret_file(path//'test-01-blocks.syntran')  == 'true', &
			interpret_file(path//'test-02-if.syntran')      == '2' , &
			interpret_file(path//'test-03-if.syntran')      == '14', &
			interpret_file(path//'test-04-if.syntran')      == '12', &
			interpret_file(path//'test-05-else.syntran')    == '14', &
			interpret_file(path//'test-06-else.syntran')    == '16', &
			interpret_file(path//'test-07-else.syntran')    == '16', &
			interpret_file(path//'test-08-else.syntran')    == '15', &
			interpret_file(path//'test-09-else-if.syntran') == '20', &
			interpret_file(path//'test-10-else-if.syntran') == '18', &
			interpret_file(path//'test-11-else-if.syntran') == '16', &
			interpret_file(path//'test-12-else-if.syntran') == '14', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_if_else

!===============================================================================

subroutine unit_test_for(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'for loops'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests
	!
	! Organize syntran test src files into another level of folders

	tests = &
		[   &
			interpret_file(path//'test-01-for.syntran') == '0', &
			interpret_file(path//'test-02-for.syntran') == '5050', &
			interpret_file(path//'test-03-for.syntran') == '1836311903', &
			interpret_file(path//'test-04-for.syntran') == '97', &
			interpret_file(path//'test-05-for.syntran') == '25', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_for

!===============================================================================

subroutine unit_test_while(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'for loops'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests
	!
	! Organize syntran test src files into another level of folders

	tests = &
		[   &
			interpret_file(path//'test-01-while.syntran') == '0', &
			interpret_file(path//'test-02-while.syntran') == '1', &
			interpret_file(path//'test-03-while.syntran') == '5050', &
			interpret_file(path//'test-04-while.syntran') == '9973', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_while

!===============================================================================

subroutine unit_test_bad_syntax(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'bad syntax'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! Anything with a syntax error should evaluate to an empty string.  Set
	! quiet arg to not pollute the logs with error messages that I intend
	! to trigger

	! TODO: check edge cases of power operator with negative int args:
	!  + 0 **  0 ?
	!  + 2 ** -2 ? (real but not int)
	!  + etc.

	tests = &
		[   &
			eval('1 +* 2', quiet) == '', &
			eval('3 - 4 + ', quiet) == '', &
			eval('1 + 123456789123456789', quiet) == '', &
			eval('1 + $', quiet) == '', &
			eval('4) + 5', quiet) == '', &
			eval('true + 4', quiet) == '', &
			eval('+true', quiet) == '', &
			eval('not 1', quiet) == '', &
			eval('true == 1', quiet) == '', &
			eval('0 == false', quiet) == '', &
			eval('0 != false', quiet) == '', &
			eval('1 + (2 == 3)', quiet) == '', &
			interpret( &
				'let a = 2;'// &
				'let a = 3', quiet) == '',     &
			interpret( &
				'let aaa = 3;'// &
				'let aa = aaa - 1;'// &
				'let b = a - 1', quiet) == '',     &
			interpret( &
				'let a = b', quiet) == '',     &
			interpret( &
				'let a = 1;'// &
				'a = true', quiet) == '',     &
			interpret( &
				'let cute = 1;'// &
				'let cup = 2;'// &
				'let at = 3;'// &
				'let as = 4;'// &
				'let he = 5;'// &
				'let us = 6;'// &
				'let i = 7;'// &
				'a;', quiet) == '', &
			interpret( &
				'//let a = 1;'//line_feed// &
				'a;', quiet) == '',     &
			eval('let a + 1 = 2;', quiet) == '', &
			eval('let = 2;', quiet) == '', &
			eval('let a = 2 3;', quiet) == '', &
			eval('7 * false;', quiet) == '' &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bad_syntax

!===============================================================================

subroutine unit_test_coda(tests, label, npass, nfail)

	logical, intent(in) :: tests(:)
	character(len = *), intent(in) :: label
	integer, intent(inout) :: npass, nfail

	!********

	integer :: id(1)

	if (.not. all(tests)) then

		call console_color(fg_bright_red)
		write(*, '(a,i0,a)') '     Error: ', count(.not. tests), &
				' '//label//' test(s) failed'

		id = findlocl1(tests, .false.)
		write(*, '(a,i0,a)') '     Test ID ', id(1), ' was the first failure'
		call console_color_reset()

	end if

	npass = npass + count(tests)
	nfail = nfail + count(.not. tests)

end subroutine unit_test_coda

!===============================================================================

subroutine unit_tests(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 72)
	write(*,*) 'Running syntran unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_bin_arith  (npass, nfail)
	call unit_test_paren_arith(npass, nfail)
	call unit_test_unary_arith(npass, nfail)
	call unit_test_bool       (npass, nfail)
	call unit_test_comparisons(npass, nfail)
	call unit_test_bad_syntax (npass, nfail)
	call unit_test_assignment (npass, nfail)
	call unit_test_comments   (npass, nfail)
	call unit_test_blocks     (npass, nfail)
	call unit_test_if_else    (npass, nfail)
	call unit_test_for        (npass, nfail)
	call unit_test_while      (npass, nfail)

	write(*,*)
	write(*,*) repeat('+', 42)

	write(*, '(a)', advance = 'no') ' +'
	if (npass > 0) call console_color(fg_bright_green)
	write(*, '(i12,a)', advance = 'no') npass, ' total tests passed'
	call console_color_reset()
	write(*,*) '        +'

	write(*, '(a)', advance = 'no') ' +'
	if (nfail > 0) call console_color(fg_bright_red)
	write(*, '(i12,a)', advance = 'no') nfail, ' total tests failed'
	call console_color_reset()
	write(*,*) '        +'

	write(*,*) repeat('+', 42)
	write(*,*)
	write(*,*) repeat('=', 72)

	iostat = nfail

end subroutine unit_tests

!===============================================================================

end module test_m

!===============================================================================

program test

	use test_m
	implicit none

	integer :: io

	call unit_tests(io)
	call exit(io)

end program test

!===============================================================================

