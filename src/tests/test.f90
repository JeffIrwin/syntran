
module test_m

	! Use short names for convenience
	use syntran, &
		interpret      => syntran_interpret, &
		interpret_file => syntran_interpret_file, &
		eval      => syntran_eval     , &
		eval_i32  => syntran_eval_i32, &
		eval_f32  => syntran_eval_f32

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
			eval_i32('1;') == 1, &
			eval_i32('69;') == 69, &
			eval_i32('420;') == 420, &
			eval_i32('1337;') == 1337, &
			eval_i32('1 + 2;') == 1 + 2, &
			eval_i32('1 + 2 + 34;') == 1 + 2 + 34, &
			eval_i32('1+2+34;') == 1+2+34, &
			eval_i32('1-2-34;') == 1-2-34, &
			eval_i32('1 + 2 * 3;') == 1 + 2 * 3, &
			eval_i32('1 * 2 * 3 * 4;') == 1 * 2 * 3 * 4, &
			eval_i32('73 - 48;') == 73 - 48, &
			eval_i32('73 - 48 - 21;') == 73 - 48 - 21, &
			eval_i32('24 / 6;') == 24 / 6, &
			eval_i32('24 / 6 / 2;') == 24 / 6 / 2, &
			eval_i32('2 ** 5;') == 2 ** 5, &
			eval_i32('3 ** 4;') == 3 ** 4, &
			eval_i32('343 - 87654345 / 27 + 76 * 234 - 65432 / 63;') &
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
			eval_i32('(69);') == (69), &
			eval_i32('(1) + 2;') == (1) + 2, &
			eval_i32('1 + (2 + 34);') == 1 + (2 + 34), &
			eval_i32('(1 + 2) * 3;') == (1 + 2) * 3, &
			eval_i32('1 * (2 * 3 * 4);') == 1 * (2 * 3 * 4), &
			eval_i32('73 - (48);') == 73 - (48), &
			eval_i32('73 - (48 - 21);') == 73 - (48 - 21), &
			eval_i32('24 / (6 / 2);') == 24 / (6 / 2), &
			eval_i32('343 - (87654345 / 27 + 76 * (234 - 65432)) / 63;') &
			   == 343 - (87654345 / 27 + 76 * (234 - 65432)) / 63,  &
			eval_i32(  &
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
			eval_i32('-(69);') == -(69), &
			eval_i32('(-69);') == (-69), &
			eval_i32('+(69);') == +(69), &
			eval_i32('(+69);') == (+69), &
			eval_i32('(-1) + 2;') == (-1) + 2, &
			eval_i32('1 + (+2 + 34);') == 1 + (+2 + 34), &
			eval_i32('(1 + 2) * -3;') == (1 + 2) * -3, &
			eval_i32('-1 * (2 * -3 * -4);') == -1 * (2 * -3 * -4), &
			eval_i32('-73 - (+48);') == -73 - (+48), &
			eval_i32('24 / (-6 / 2);') == 24 / (-6 / 2), &
			eval_i32('343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63;') &
			       == 343 - (-87654345 / 27 + -76 * (+234 - 65432)) / -63   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_unary_arith

!===============================================================================

subroutine unit_test_bools(npass, nfail)

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

end subroutine unit_test_bools

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
			eval('1337  < 1338  ;')  == 'true' ,  &
			eval('1337  < 1337  ;')  == 'false',  &
			eval('1337  < 1336  ;')  == 'false',  &
			eval('1337 <= 1338  ;')  == 'true' ,  &
			eval('1337 <= 1337  ;')  == 'true' ,  &
			eval('1337 <= 1336  ;')  == 'false',  &
			eval('1337  > 1338  ;')  == 'false',  &
			eval('1337  > 1337  ;')  == 'false',  &
			eval('1337  > 1336  ;')  == 'true' ,  &
			eval('1337 >= 1338  ;')  == 'false',  &
			eval('1337 >= 1337  ;')  == 'true' ,  &
			eval('1337 >= 1336  ;')  == 'true' ,  &
			eval('true  == false;')  == 'false'   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comparisons

!===============================================================================

subroutine unit_test_comp_f32(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 comparisons'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('13.37 >  13.36 ;')  == 'true',  &
			eval('13.37 >  13.37 ;')  == 'false',  &
			eval('13.37 >  13.38 ;')  == 'false',  &
			eval('13.37 <  13.36 ;')  == 'false',  &
			eval('13.37 <  13.37 ;')  == 'false',  &
			eval('13.37 <  13.38 ;')  == 'true',  &
			eval('13.37 >= 13.36 ;')  == 'true',  &
			eval('13.37 >= 13.37 ;')  == 'true',  &
			eval('13.37 >= 13.38 ;')  == 'false',  &
			eval('13.37 <= 13.36 ;')  == 'false',  &
			eval('13.37 <= 13.37 ;')  == 'true',  &
			eval('13.37 <= 13.38 ;')  == 'true',  &
			eval('13.37 != 13.36 ;')  == 'true',  &
			eval('13.37 != 13.37 ;')  == 'false',  &
			eval('13.37 != 13.38 ;')  == 'true',  &
			eval('13.37 == 4.2   ;')  == 'false',  &
			eval('4.2   == 13.37 ;')  == 'false',  &
			eval('1.0   == 1.0   ;')  == 'true'    &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comp_f32

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
			eval('{'// &
				'let a = 42;'// &
				'let b = a = 1337;'// &
				'}') == '1337' ,  &
			eval('{'// &
				'let a = 42;'// &
				'let b = 69;'// &
				'b = a = 1337;'// &
				'}') == '1337' ,  &
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
	character(len = *), parameter :: &
		path = 'src/tests/test-src/if-else-statements/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests, and some bad syntax tests for interpret_file().
	!
	! Eventually, do something like Immo's AssertDiagnostic() for bad syntax.
	! However, I don't like the idea of having to update tests whenever I decide
	! to edit an error's wording, especially if there are multiple tests that
	! cover each error message.  It could be manageable if there is only 1 test
	! per message, but that limits how thorough the tests can be

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == 'true', &
			interpret_file(path//'test-02.syntran') == '2' , &
			interpret_file(path//'test-03.syntran') == '14', &
			interpret_file(path//'test-04.syntran') == '12', &
			interpret_file(path//'test-05.syntran') == '14', &
			interpret_file(path//'test-06.syntran') == '16', &
			interpret_file(path//'test-07.syntran') == '16', &
			interpret_file(path//'test-08.syntran') == '15', &
			interpret_file(path//'test-09.syntran') == '20', &
			interpret_file(path//'test-10.syntran') == '18', &
			interpret_file(path//'test-11.syntran') == '16', &
			interpret_file(path//'test-12.syntran') == '14', &
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
	character(len = *), parameter :: path = 'src/tests/test-src/for-loops/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == '0', &
			interpret_file(path//'test-02.syntran') == '5050', &
			interpret_file(path//'test-03.syntran') == '1836311903', &
			interpret_file(path//'test-04.syntran') == '97', &
			interpret_file(path//'test-05.syntran') == '25', &
			interpret_file(path//'test-06.syntran') == '25', &
			interpret_file(path//'test-07.syntran') == '1836311903', &
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

	character(len = *), parameter :: label = 'while loops'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/while-loops/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == '0', &
			interpret_file(path//'test-02.syntran') == '1', &
			interpret_file(path//'test-03.syntran') == '5050', &
			interpret_file(path//'test-04.syntran') == '9973', &
			interpret_file(path//'test-05.syntran') == '5050', &
			interpret_file(path//'test-06.syntran') == '0', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_while

!===============================================================================

subroutine unit_test_var_scopes(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'variable scoping'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/var-scopes/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == 'true', &
			interpret_file(path//'test-02.syntran') == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_var_scopes

!===============================================================================

subroutine unit_test_f32_1(npass, nfail)

	! Simple f32 float tests of arithmetic and comparisons with single-line
	! evaluations

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 arithmetic'

	logical, allocatable :: tests(:)

	real, parameter :: tol = 1.e-9

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			abs(eval_f32('1.0;') - 1) < tol, &
			abs(eval_f32('6.9e1;') - 6.9e1) < tol, &
			abs(eval_f32('+6.9e1;') - +6.9e1) < tol, &
			abs(eval_f32('-6.9e1;') - -6.9e1) < tol, &
			abs(eval_f32('0.333333333;') - 0.333333333) < tol, &
			abs(eval_f32('1.1 +  2  ;') - (1.1 +  2  )) < tol, &
			abs(eval_f32('1   +  2.1;') - (1   +  2.1)) < tol, &
			abs(eval_f32('1.1 +  2.1;') - (1.1 +  2.1)) < tol, &
			abs(eval_f32('1.1 -  2  ;') - (1.1 -  2  )) < tol, &
			abs(eval_f32('1   -  2.1;') - (1   -  2.1)) < tol, &
			abs(eval_f32('1.1 -  2.1;') - (1.1 -  2.1)) < tol, &
			abs(eval_f32('1.1 *  2  ;') - (1.1 *  2  )) < tol, &
			abs(eval_f32('1   *  2.1;') - (1   *  2.1)) < tol, &
			abs(eval_f32('1.1 *  2.1;') - (1.1 *  2.1)) < tol, &
			abs(eval_f32('1.1 /  2  ;') - (1.1 /  2  )) < tol, &
			abs(eval_f32('1   /  2.1;') - (1   /  2.1)) < tol, &
			abs(eval_f32('1.1 /  2.1;') - (1.1 /  2.1)) < tol, &
			abs(eval_f32('1.1 ** 2  ;') - (1.1 ** 2  )) < tol, &
			abs(eval_f32('1   ** 2.1;') - (1   ** 2.1)) < tol, &
			abs(eval_f32('1.1 ** 2.1;') - (1.1 ** 2.1)) < tol, &
			abs(eval_f32('1.2e-3 + 4.5e-3;') - (1.2e-3 + 4.5e-3)) < tol, &
			abs(eval_f32('1.2e-3+4.5e-3;') - (1.2e-3+4.5e-3)) < tol, &
			abs(eval_f32('1.2e-3-4.5e-3;') - (1.2e-3-4.5e-3)) < tol, &
			abs(eval_f32('1.2e+3-4.5e+3;') - (1.2e+3-4.5e+3)) < tol, &
			abs(eval_f32('1.1 + 2.2 + 34;') - (1.1 + 2.2 + 34)) < tol, &
			abs(eval_f32('1 + 2 * 3.3;') - (1 + 2 * 3.3)) < tol, &
			abs(eval_f32('1 * 2 * 3.6 * 4;') - (1 * 2 * 3.6 * 4)) < tol, &
			abs(eval_f32('73 - 48.0;') - (73 - 48.0)) < tol, &
			abs(eval_f32('73.1 - 48 - 21;') - (73.1 - 48 - 21)) < tol, &
			abs(eval_f32('24 / 6.3;') - (24 / 6.3)) < tol, &
			abs(eval_f32('24 / 6 / 2.1;') - (24 / 6 / 2.1)) < tol, &
			abs(eval_f32('2.0 ** 5;') - (2.0 ** 5)) < tol, &
			abs(eval_f32('3 ** 4.1;') - (3 ** 4.1)) < tol, &
			abs(eval_f32('3.43 - 87654345 / 27 + 76 * 234 - 65432 / 63;') &
			       - (3.43 - 87654345 / 27 + 76 * 234 - 65432 / 63)) < tol &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_f32_1

!===============================================================================

subroutine unit_test_array_i32_1(npass, nfail)

	! Simple i32 array tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'i32 arrays'

	logical, allocatable :: tests(:)

	real, parameter :: tol = 1.e-9

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! Because test evaluation results are tested by comparing strings, output
	! white space is significant!  Ints are formatted in min width, and array
	! elements are separated by a comma and a single space

	tests = &
		[   &
			eval('[42];') == '[42]', &
			eval('[-42,1337];') == '[-42, 1337]', &
			eval('[3, 2, 1];') == '[3, 2, 1]', &
			eval('[1: 4];') == '[1, 2, 3]', &
			eval('[2-3: 6/3 + 3];') == '[-1, 0, 1, 2, 3, 4]', &
			eval('let myArray = [2-3: 6/3 + 3];') == '[-1, 0, 1, 2, 3, 4]', &
			eval('[48-6, 13*100 + 37];') == '[42, 1337]'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_i32_1

!===============================================================================

subroutine unit_test_array_i32_2(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'i32 array scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/array-i32/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == '0', &
			interpret_file(path//'test-02.syntran') == '0', &
			interpret_file(path//'test-03.syntran') == '13', &
			interpret_file(path//'test-04.syntran') == '-3', &
			interpret_file(path//'test-05.syntran') == '16', &
			interpret_file(path//'test-06.syntran') == '16', &
			interpret_file(path//'test-07.syntran') == '13', &
			interpret_file(path//'test-08.syntran') == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_i32_2

!===============================================================================

!===============================================================================

subroutine unit_test_f32_2(npass, nfail)

	! More advanced f32 tests on longer syntran scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/f32/'

	logical, allocatable :: tests(:)

	write(*,*)
	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran') == 'true', &
			interpret_file(path//'test-02.syntran') == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_f32_2

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
			eval('0  < false', quiet) == '', &
			eval('0  > false', quiet) == '', &
			eval('0 <= false', quiet) == '', &
			eval('0 >= false', quiet) == '', &
			eval('true >= false', quiet) == '', &
			eval('1 + (2 == 3)', quiet) == '', &
			eval('1.3 >= false', quiet) == '', &
			eval('1.0 == 1', quiet) == '', &  ! I'm open to debate
			eval('1 == 1.0', quiet) == '', &
			eval('1.0 + true', quiet) == '', &
			eval('true - 1.0', quiet) == '', &
			eval('true and 1.0', quiet) == '', &
			eval('1.234e+1e+2', quiet) == '', &
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

subroutine log_test_summary(npass, nfail)

	implicit none

	integer, intent(in) :: npass, nfail

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

end subroutine log_test_summary

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
	call unit_test_bools      (npass, nfail)
	call unit_test_comparisons(npass, nfail)
	call unit_test_comp_f32   (npass, nfail)
	call unit_test_bad_syntax (npass, nfail)
	call unit_test_assignment (npass, nfail)
	call unit_test_comments   (npass, nfail)
	call unit_test_blocks     (npass, nfail)
	call unit_test_f32_1      (npass, nfail)
	call unit_test_if_else    (npass, nfail)
	call unit_test_for        (npass, nfail)
	call unit_test_while      (npass, nfail)
	call unit_test_var_scopes (npass, nfail)
	call unit_test_f32_2      (npass, nfail)
	call unit_test_array_i32_1(npass, nfail)
	call unit_test_array_i32_2(npass, nfail)

	! TODO: add tests that mock interpreting one line at a time (as opposed to
	! whole files)

	call log_test_summary(npass, nfail)
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

