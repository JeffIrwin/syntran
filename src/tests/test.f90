
module test_m

	use syntran__test_core_m

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
			eval_i32('13 % 4;') == mod(13, 4), &
			eval_i32('14 % 4;') == mod(14, 4), &
			eval_i32('15 % 4;') == mod(15, 4), &
			eval_i32('13 % -4;') == mod(13, -4), &
			eval_i32('14 % -4;') == mod(14, -4), &
			eval_i32('15 % -4;') == mod(15, -4), &
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
			eval_i32('1 * (2 % 3 * 4);') == 1 * (mod(2, 3) * 4), &
			eval_i32('73 - (48 % 3);') == 73 - (mod(48, 3)), &
			eval_i32('73 - (48 - 21 % 2);') == 73 - (48 - mod(21, 2)), &
			eval_i32('24 / (156 % 17 / 2);') == 24 / (mod(156, 17) / 2), &
			eval_i32('343 - (87654345 / 27 + 76 * (234 - 65432)) / 63;') &
			   == 343 - (87654345 / 27 + 76 * (234 - 65432)) / 63,  &
			eval_i32(  &
				'(1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9 ' &
				//' - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8) ' &
				//' - 9) * 8) + 1 - (2 * (3 + (4 - (5 % 6) / 2)  ' &
				//' + 7) - 8) * 9) + 1) * 42;')   &
				== &
				 (1 + (2 - (3 * (456789 / (5 + (6 - 7) * 8) + (9   &
				    - (0 + 1) - 2) * 3) / (4 + (5 - (6 * 7) + 8)   &
				    - 9) * 8) + 1 - (2 * (3 + (4 - (mod(5, 6)) / 2)    &
				    + 7) - 8) * 9) + 1) * 42,     &
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
			       == 343 - (-87654345 / 27 + -76 * (+234 - 65432)) / (-63)  &
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
			eval('"a"   == "a"  ;')  == 'true' ,  &
			eval('"a"   == "b"  ;')  == 'false',  &
			eval('"c"   == "a"  ;')  == 'false',  &
			eval('"aoe" == "aoe";')  == 'true' ,  &
			eval('"hoe" == "aoe";')  == 'false',  &
			eval('"ahe" == "aoe";')  == 'false',  &
			eval('"aoh" == "aoe";')  == 'false',  &
			eval('"aoe" == "hoe";')  == 'false',  &
			eval('"aoe" == "ahe";')  == 'false',  &
			eval('"aoe" == "aoh";')  == 'false',  &
			eval('" oe" == "aoe";')  == 'false',  &
			eval('"a e" == "aoe";')  == 'false',  &
			eval('"ao " == "aoe";')  == 'false',  &
			eval('"aoe" == " oe";')  == 'false',  &
			eval('"aoe" == "a e";')  == 'false',  &
			eval('"aoe" == "ao ";')  == 'false',  &
			eval('"aoe" != "aoe";')  == 'false',  &
			eval('"hoe" != "aoe";')  == 'true',  &
			eval('"ahe" != "aoe";')  == 'true',  &
			eval('"aoh" != "aoe";')  == 'true',  &
			eval('"aoe" != "hoe";')  == 'true',  &
			eval('"aoe" != "ahe";')  == 'true',  &
			eval('"aoe" != "aoh";')  == 'true',  &
			eval('" oe" != "aoe";')  == 'true',  &
			eval('"a e" != "aoe";')  == 'true',  &
			eval('"ao " != "aoe";')  == 'true',  &
			eval('"aoe" != " oe";')  == 'true',  &
			eval('"aoe" != "a e";')  == 'true',  &
			eval('"aoe" != "ao ";')  == 'true',  &
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
			eval('13.37f >  13.36f ;')  == 'true',  &
			eval('13.37f >  13.36f ;')  == 'true',  &
			eval('13.37f >  13.37f ;')  == 'false',  &
			eval('13.37f >  13.38f ;')  == 'false',  &
			eval('13.37f <  13.36f ;')  == 'false',  &
			eval('13.37f <  13.37f ;')  == 'false',  &
			eval('13.37f <  13.38f ;')  == 'true',  &
			eval('13.37f >= 13.36f ;')  == 'true',  &
			eval('13.37f >= 13.37f ;')  == 'true',  &
			eval('13.37f >= 13.38f ;')  == 'false',  &
			eval('13.37f <= 13.36f ;')  == 'false',  &
			eval('13.37f <= 13.37f ;')  == 'true',  &
			eval('13.37f <= 13.38f ;')  == 'true',  &
			eval('13.37f != 13.36f ;')  == 'true',  &
			eval('13.37f != 13.37f ;')  == 'false',  &
			eval('13.37f != 13.38f ;')  == 'true',  &
			eval('13.37f == 4.2f   ;')  == 'false',  &
			eval('4.2f   == 13.37f ;')  == 'false',  &
			eval('1.0f   == 1.0f   ;')  == 'true'    &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comp_f32

!===============================================================================

subroutine unit_test_comp_f64(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f64 comparisons'

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
			eval('1.0   == 1.0   ;')  == 'true',   &
			eval('13.37f >  13.36 ;')  == 'true',  &
			eval('13.37 >  13.36f ;')  == 'true',  &
			eval('13.37f >= 13.38 ;')  == 'false',  &
			eval('13.37 >= 13.38f ;')  == 'false',  &
			eval('13.37f <= 13.38 ;')  == 'true',  &
			.false.  &
		]

	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comp_f64

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

subroutine unit_test_comp_ass(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'compound assignment'

	logical, allocatable :: tests(:)
	logical, parameter :: quiet = .true.

	real, parameter :: tol = 1.e-9

	write(*,*) 'Unit testing '//label//' ...'

	! We're getting into dangerous Fortran territory here.  I think there's
	! a limit on how many chars and lines can be in a statement, and this may be
	! pushing it

	tests = &
		[   &
			eval('let j = 10; j += 3; j;', quiet) == '13', &
			abs(eval_f32('let f = 0.5f; f += 0.25f; f;', quiet) - 0.75) < tol, &
			abs(eval_f64('let f = 0.5; f += 0.25; f;', quiet) - 0.75d0) < tol, &
			eval('let s = "hello "; s += "world"; s;', quiet) == 'hello world', &
			abs(eval_f32('let f = 0.5f; f += 0.25f;', quiet) - 0.75) < tol, &
			abs(eval_f64('let f = 0.5; f += 0.25;', quiet) - 0.75d0) < tol, &
			eval('let s = "hello "; s += "world";', quiet) == 'hello world', &
			eval('let iv = [10; 3]; iv[0] += 5;', quiet) == '15', &
			eval('let iv = [10; 3]; iv[0] += 5; iv;', quiet) == '[15, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] += 5; iv;', quiet) == '[10, 15, 10]', &
			eval('let iv = [10; 3]; iv[1] += 5.1; iv;', quiet) == '[10, 15, 10]', &
			abs(eval_f32('let v = [10.0f; 3]; v[0] += 5.0f;', quiet) - 15) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[0] += 5.0;', quiet) - 15) < tol, &
			eval('let v = [10.0f; 3]; v[0] += 5.0f; v;', quiet) == '[1.500000E+01, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] += 5.0f; v;', quiet) == '[1.000000E+01, 1.500000E+01, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] += 5; v;', quiet) == '[1.000000E+01, 1.500000E+01, 1.000000E+01]', &
			abs(eval_f64('let v = [10.0; 3]; v[0] += 5.0; sum(v);', quiet) - 35) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] += 5.0; sum(v);', quiet) - 35) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] += 5; sum(v);', quiet) - 35) < tol, &
			eval('let i = 20; i += 5.1f;', quiet) == '25', &
			eval('let i = 20; i += 5.1;', quiet) == '25', &
			abs(eval_f32('let i = 20.1f; i += 5;', quiet) - 25.1) < tol, &
			abs(eval_f64('let i = 20.1; i += 5;', quiet) - 25.1d0) < tol, &
			eval('let j = 10; j -= 3; j;', quiet) == '7', &
			abs(eval_f32('let f = 0.75f; f -= 0.25f; f;', quiet) - 0.5) < tol, &
			abs(eval_f64('let f = 0.75; f -= 0.25; f;', quiet) - 0.5d0) < tol, &
			eval('let j = 10; j -= 3;', quiet) == '7', &
			abs(eval_f32('let f = 0.75f; f -= 0.25f;', quiet) - 0.5) < tol, &
			abs(eval_f64('let f = 0.75; f -= 0.25;', quiet) - 0.5d0) < tol, &
			eval('let iv = [10; 3]; iv[0] -= 4;', quiet) == '6', &
			eval('let iv = [10; 3]; iv[0] -= 4; iv;', quiet) == '[6, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] -= 4; iv;', quiet) == '[10, 6, 10]', &
			eval('let iv = [10; 3]; iv[1] -= 3.9; iv;', quiet) == '[10, 6, 10]', &
			abs(eval_f32('let v = [10.0f; 3]; v[0] -= 4.0f;', quiet) - 6) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[0] -= 4.0;', quiet) - 6) < tol, &
			eval('let v = [10.0f; 3]; v[0] -= 4.0f; v;', quiet) == '[6.000000E+00, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [20.0f; 3]; v[1] -= 4.0f; v;', quiet) == '[2.000000E+01, 1.600000E+01, 2.000000E+01]', &
			eval('let v = [20.0f; 3]; v[1] -= 4; v;', quiet) == '[2.000000E+01, 1.600000E+01, 2.000000E+01]', &
			abs(eval_f64('let v = [10.0; 3]; v[0] -= 4.0; sum(v);', quiet) - 26) < tol, &
			abs(eval_f64('let v = [20.0; 3]; v[1] -= 4.0; sum(v);', quiet) - 56) < tol, &
			abs(eval_f64('let v = [20.0; 3]; v[1] -= 4; sum(v);', quiet) - 56) < tol, &
			eval('let i = 20; i -= 5.1f;', quiet) == '14', &
			abs(eval_f32('let i = 20.1f; i -= 5;', quiet) - 15.1) < tol, &
			eval('let i = 20; i -= 3.1f;', quiet) == '16', &
			abs(eval_f32('let i = 20.1f; i -= 3;', quiet) - 17.1) < tol, &
			eval('let i = 20; i -= 5.1;', quiet) == '14', &
			abs(eval_f64('let i = 20.1; i -= 5;', quiet) - 15.1d0) < tol, &
			eval('let i = 20; i -= 3.1;', quiet) == '16', &
			abs(eval_f64('let i = 20.1; i -= 3;', quiet) - 17.1d0) < tol, &
			eval('let j =  7; j *= 6; j;', quiet) == '42', &
			eval('let j = 10; j *= 3; j;', quiet) == '30', &
			abs(eval_f32('let f = 0.5f; f *= 0.25f; f;', quiet) - 0.125) < tol, &
			abs(eval_f32('let f = 0.5f; f *= 0.25f;', quiet) - 0.125) < tol, &
			abs(eval_f64('let f = 0.5; f *= 0.25; f;', quiet) - 0.125d0) < tol, &
			abs(eval_f64('let f = 0.5; f *= 0.25;', quiet) - 0.125d0) < tol, &
			eval('let iv = [10; 3]; iv[0] *= 5;', quiet) == '50', &
			eval('let iv = [10; 3]; iv[0] *= 5; iv;', quiet) == '[50, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] *= 5; iv;', quiet) == '[10, 50, 10]', &
			eval('let iv = [10; 3]; iv[1] *= 5.101; iv;', quiet) == '[10, 51, 10]', &
			abs(eval_f32('let v = [10.0f; 3]; v[0] *= 5.0f;', quiet) - 50) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[0] *= 5.0;', quiet) - 50) < tol, &
			eval('let v = [10.0f; 3]; v[0] *= 5.0f; v;', quiet) == '[5.000000E+01, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] *= 5.0f; v;', quiet) == '[1.000000E+01, 5.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] *= 5; v;', quiet) == '[1.000000E+01, 5.000000E+01, 1.000000E+01]', &
			abs(eval_f64('let v = [10.0; 3]; v[0] *= 5.0; sum(v);', quiet) - 70) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] *= 5.0; sum(v);', quiet) - 70) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] *= 5; sum(v);', quiet) - 70) < tol, &
			eval('let i = 20; i *= 5.101f;', quiet) == '102', &
			abs(eval_f32('let i = 20.1f; i *= 5;', quiet) - 100.5) < tol, &
			eval('let j = 12; j /= 3; j;', quiet) == '4', &
			abs(eval_f32('let f = 0.5f; f /= 0.25f; f;', quiet) - 2.0) < tol, &
			abs(eval_f32('let f = 0.5f; f /= 0.25f;', quiet) - 2.0) < tol, &
			eval('let i = 20; i *= 5.101;', quiet) == '102', &
			abs(eval_f64('let i = 20.1; i *= 5;', quiet) - 100.5) < tol, &
			eval('let j = 12; j /= 3; j;', quiet) == '4', &
			abs(eval_f64('let f = 0.5; f /= 0.25; f;', quiet) - 2.0d0) < tol, &
			abs(eval_f64('let f = 0.5; f /= 0.25;', quiet) - 2.0d0) < tol, &
			eval('let iv = [10; 3]; iv[0] /= 5;', quiet) == '2', &
			eval('let iv = [10; 3]; iv[0] /= 5; iv;', quiet) == '[2, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] /= 5; iv;', quiet) == '[10, 2, 10]', &
			eval('let iv = [10; 3]; iv[1] /= 4.9; iv;', quiet) == '[10, 2, 10]', &
			abs(eval_f32('let v = [10.0f; 3]; v[0] /= 5.0f;', quiet) - 2) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[0] /= 5.0;', quiet) - 2) < tol, &
			eval('let v = [10.0f; 3]; v[0] /= 5.0f; v;', quiet) == '[2.000000E+00, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] /= 5.0f; v;', quiet) == '[1.000000E+01, 2.000000E+00, 1.000000E+01]', &
			eval('let v = [10.0f; 3]; v[1] /= 5; v;', quiet) == '[1.000000E+01, 2.000000E+00, 1.000000E+01]', &
			abs(eval_f64('let v = [10.0; 3]; v[0] /= 5.0; sum(v);', quiet) - 22) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] /= 5.0; sum(v);', quiet) - 22) < tol, &
			abs(eval_f64('let v = [10.0; 3]; v[1] /= 5; sum(v);', quiet) - 22) < tol, &
			eval('let i = 20; i /= 4.9;', quiet) == '4', &
			abs(eval_f32('let i = 20.5f; i /= 5;', quiet) - 4.1) < tol, &
			abs(eval_f64('let i = 20.5; i /= 5;', quiet) - 4.1d0) < tol, &
			eval('let j = 3; j **= 2; j;', quiet) == '9', &
			eval('let j = 4; j **= 2; j;', quiet) == '16', &
			eval('let j = 2; j **= 3; j;', quiet) == '8', &
			eval('let j = 3; j **= 4; j;', quiet) == '81', &
			eval('let j = 5; j %= 3; j;', quiet) == '2', &
			eval('let j = 4; j %= 3; j;', quiet) == '1', &
			eval('let j = 3; j %= 3; j;', quiet) == '0', &
			eval('let j = 5; j %= 4; j;', quiet) == '1', &
			eval('let j = 4; j %= 4; j;', quiet) == '0', &
			eval('let j = 3; j %= 4; j;', quiet) == '3', &
			eval('let j = i64(3); j += i64(4); j;', quiet) == '7', &
			eval('let j = 3; j += i64(4); j;', quiet) == '7', &
			eval('let j = i64(3); j += 4; j;', quiet) == '7', &
			eval('let j = 3; j -= i64(4); j;', quiet) == '-1', &
			eval('let j = i64(3); j -= 4; j;', quiet) == '-1', &
			eval('let j = 3; j *= i64(4); j;', quiet) == '12', &
			eval('let j = i64(3); j *= 4; j;', quiet) == '12', &
			eval('let j = 20; j /= i64(4); j;', quiet) == '5', &
			eval('let j = i64(24); j /= 4; j;', quiet) == '6', &
			eval('let j = 13; j %= i64(4); j;', quiet) == '1', &
			eval('let j = i64(14); j %= 4; j;', quiet) == '2', &
			eval('let j = 2; j **= i64(4); j;', quiet) == '16', &
			eval('let j = i64(2); j **= 5; j;', quiet) == '32', &
			eval('let j = 10; j += 3;', quiet) == '13'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_comp_ass

!===============================================================================

subroutine unit_test_intr_fns(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'intrinsic functions'
	character(len = :), allocatable :: spi

	integer :: i

	logical, allocatable :: tests(:)

	double precision, parameter :: pi = 4.d0 * atan(1.d0)
	real, parameter :: tol = 1.e-9, ftol = 1.e-5

	write(*,*) 'Unit testing '//label//' ...'

	spi = str(pi)

	! It's important to use ftol for f32 instead of (double) tol.  This has a
	! tendency to work for one compiler/os but not all, e.g. it may break but
	! only for ifx, or only in docker env, etc.

	tests = &
		[   &
			abs(eval_f32('abs(0.01f);') - abs(0.01)) < ftol,  &
			abs(eval_f32('abs(-0.1f);') - abs(-0.1)) < ftol,  &
			abs(eval_f64('abs(-0.01);') - abs(-0.01d0)) < tol,  &
			abs(eval_f64('abs(0.1);') - abs(0.1d0)) < tol,  &
			abs(eval_f32('sum(abs([0.01f, 0.1f]));') - sum(abs([0.01, 0.1])))   < ftol,  &
			abs(eval_f32('sum(abs([-0.5f, -0.1f]));') - sum(abs([-0.5, -0.1])))   < ftol,  &
			abs(eval_f32('sum(abs([0.2f, 0.1f]));') - sum(abs([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(abs([-0.01, -0.1]));') - sum(abs([-0.01d0, -0.1d0])))  < tol,  &
			abs(eval_f64('sum(abs([0.5, -0.1]));') - sum(abs([0.5d0, -0.1d0]))) < tol,  &
			abs(eval_f64('sum(abs([0.2, -0.1]));') - sum(abs([0.2d0, -0.1d0])))  < tol,  &
			eval('abs(1) == 1;') == 'true', &
			eval('abs(-1) == 1;') == 'true', &
			eval('all(abs([-1, 3, -2]) == [1, 3, 2]);') == 'true', &
			eval('abs(i64(-4)) == 4;') == 'true', &
			eval('all(abs(i64([-10, 30, -20])) == [10, 30, 20]);') == 'true', &
			abs(eval_f32('exp(0.0f);') - 1.0) < ftol,  &
			abs(eval_f32('exp(1.0f);') - exp(1.0)) < ftol,  &
			abs(eval_f64('exp(0.0);') - 1.0d0) < tol,  &
			abs(eval_f64('exp(1.0);') - exp(1.0d0)) < tol,  &
			abs(eval_f32('sum(exp([0.0f, 1.0f]));') - sum(exp([0.0, 1.0])))   < ftol,  &
			abs(eval_f32('sum(exp([0.5f, 1.0f]));') - sum(exp([0.5, 1.0])))   < ftol,  &
			abs(eval_f32('sum(exp([2.0f, 1.0f]));') - sum(exp([2.0, 1.0])))   < ftol,  &
			abs(eval_f64('sum(exp([0.0, 1.0]));') - sum(exp([0.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sum(exp([0.5, 1.0]));') - sum(exp([0.5d0, 1.0d0]))) < tol,  &
			abs(eval_f64('sum(exp([2.0, 1.0]));') - sum(exp([2.d0, 1.0d0])))  < tol,  &
			abs(eval_f32('log(0.01f);') - log(0.01)) < ftol,  &
			abs(eval_f32('log(0.1f);') - log(0.1)) < ftol,  &
			abs(eval_f64('log(0.01);') - log(0.01d0)) < tol,  &
			abs(eval_f64('log(0.1);') - log(0.1d0)) < tol,  &
			abs(eval_f32('sum(log([0.01f, 0.1f]));') - sum(log([0.01, 0.1])))   < ftol,  &
			abs(eval_f32('sum(log([0.5f, 0.1f]));') - sum(log([0.5, 0.1])))   < ftol,  &
			abs(eval_f32('sum(log([0.2f, 0.1f]));') - sum(log([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(log([0.01, 0.1]));') - sum(log([0.01d0, 0.1d0])))  < tol,  &
			abs(eval_f64('sum(log([0.5, 0.1]));') - sum(log([0.5d0, 0.1d0]))) < tol,  &
			abs(eval_f64('sum(log([0.2, 0.1]));') - sum(log([0.2d0, 0.1d0])))  < tol,  &
			abs(eval_f32('log10(0.01f);') - log10(0.01)) < ftol,  &
			abs(eval_f32('log10(0.1f);') - log10(0.1)) < ftol,  &
			abs(eval_f64('log10(0.01);') - log10(0.01d0)) < tol,  &
			abs(eval_f64('log10(0.1);') - log10(0.1d0)) < tol,  &
			abs(eval_f64('log10(10.0);') - 1.0) < tol,  &
			abs(eval_f64('log10(100.0);') - 2.0) < tol,  &
			abs(eval_f32('sum(log10([0.01f, 0.1f]));') - sum(log10([0.01, 0.1])))   < ftol,  &
			abs(eval_f32('sum(log10([0.5f, 0.1f]));') - sum(log10([0.5, 0.1])))   < ftol,  &
			abs(eval_f32('sum(log10([0.2f, 0.1f]));') - sum(log10([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(log10([0.01, 0.1]));') - sum(log10([0.01d0, 0.1d0])))  < tol,  &
			abs(eval_f64('sum(log10([0.5, 0.1]));') - sum(log10([0.5d0, 0.1d0]))) < tol,  &
			abs(eval_f64('sum(log10([0.2, 0.1]));') - sum(log10([0.2d0, 0.1d0])))  < tol,  &
			abs(eval_f32('log2(0.01f);') - log(0.01)/log(2.d0)) < ftol,  &
			abs(eval_f32('log2(0.1f);') - log(0.1)/log(2.d0)) < ftol,  &
			abs(eval_f64('log2(0.01);') - log(0.01d0)/log(2.d0)) < tol,  &
			abs(eval_f64('log2(0.1);') - log(0.1d0)/log(2.d0)) < tol,  &
			abs(eval_f64('log2(8.0);') - 3.0) < tol,  &
			abs(eval_f64('log2(32.0);') - 5.0) < tol,  &
			abs(eval_f32('sum(log2([0.01f, 0.1f]));') - sum(log([0.01, 0.1])/log(2.d0)))   < ftol,  &
			abs(eval_f32('sum(log2([0.5f, 0.1f]));') - sum(log([0.5, 0.1])/log(2.d0)))   < ftol,  &
			abs(eval_f32('sum(log2([0.2f, 0.1f]));') - sum(log([0.2, 0.1])/log(2.d0)))   < ftol,  &
			abs(eval_f64('sum(log2([0.01, 0.1]));') - sum(log([0.01d0, 0.1d0])/log(2.d0)))  < tol,  &
			abs(eval_f64('sum(log2([0.5, 0.1]));') - sum(log([0.5d0, 0.1d0])/log(2.d0))) < tol,  &
			abs(eval_f64('sum(log2([0.2, 0.1]));') - sum(log([0.2d0, 0.1d0])/log(2.d0)))  < tol,  &
			abs(eval_f32('sqrt(1.0f);') - sqrt(1.0)) < ftol,  &
			abs(eval_f64('sqrt(4.0);') - sqrt(4.0d0)) < tol,  &
			abs(eval_f32('sum(sqrt([0.2f, 0.1f]));') - sum(sqrt([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(sqrt([0.01, 0.1]));') - sum(sqrt([0.01d0, 0.1d0])))  < tol,  &
			abs(eval_f32('cos(0.0f);') - cos(0.0)) < ftol,  &
			abs(eval_f32('cos(1.0f);') - cos(1.0)) < ftol,  &
			abs(eval_f64('cos(0.0);') - cos(0.d0)) < tol,  &
			abs(eval_f64('cos(1.0);') - cos(1.0d0)) < tol,  &
			abs(eval_f32('sum(cos([0.0f, 1.0f]));') - sum(cos([0.0, 1.0])))   < ftol,  &
			abs(eval_f32('sum(cos([0.5f, 1.0f]));') - sum(cos([0.5, 1.0])))   < ftol,  &
			abs(eval_f32('sum(cos([2.0f, 1.0f]));') - sum(cos([2.0, 1.0])))   < ftol,  &
			abs(eval_f64('sum(cos([0.0, 1.0]));') - sum(cos([0.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sum(cos([0.5, 1.0]));') - sum(cos([0.5d0, 1.0d0]))) < tol,  &
			abs(eval_f64('sum(cos([2.0, 1.0]));') - sum(cos([2.d0, 1.0d0])))  < tol,  &
			abs(eval_f32('sin(0.0f);') - sin(0.0)) < ftol,  &
			abs(eval_f32('sin(1.0f);') - sin(1.0)) < ftol,  &
			abs(eval_f64('sin(0.0);') - sin(0.d0)) < tol,  &
			abs(eval_f64('sin(1.0);') - sin(1.0d0)) < tol,  &
			abs(eval_f32('sum(sin([0.0f, 1.0f]));') - sum(sin([0.0, 1.0])))   < ftol,  &
			abs(eval_f32('sum(sin([0.5f, 1.0f]));') - sum(sin([0.5, 1.0])))   < ftol,  &
			abs(eval_f32('sum(sin([2.0f, 1.0f]));') - sum(sin([2.0, 1.0])))   < ftol,  &
			abs(eval_f64('sum(sin([0.0, 1.0]));') - sum(sin([0.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sum(sin([0.5, 1.0]));') - sum(sin([0.5d0, 1.0d0]))) < tol,  &
			abs(eval_f64('sum(sin([2.0, 1.0]));') - sum(sin([2.d0, 1.0d0])))  < tol,  &
			abs(eval_f32('tan(0.0f);') - tan(0.0)) < ftol,  &
			abs(eval_f32('tan(1.0f);') - tan(1.0)) < ftol,  &
			abs(eval_f64('tan(0.0);') - tan(0.d0)) < tol,  &
			abs(eval_f64('tan(1.0);') - tan(1.0d0)) < tol,  &
			abs(eval_f32('sum(tan([0.0f, 1.0f]));') - sum(tan([0.0, 1.0])))   < ftol,  &
			abs(eval_f32('sum(tan([0.5f, 1.0f]));') - sum(tan([0.5, 1.0])))   < ftol,  &
			abs(eval_f32('sum(tan([2.0f, 1.0f]));') - sum(tan([2.0, 1.0])))   < ftol,  &
			abs(eval_f64('sum(tan([0.0, 1.0]));') - sum(tan([0.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sum(tan([0.5, 1.0]));') - sum(tan([0.5d0, 1.0d0]))) < tol,  &
			abs(eval_f64('sum(tan([2.0, 1.0]));') - sum(tan([2.d0, 1.0d0])))  < tol,  &
			abs(eval_f32('acos(0.0f);') - acos(0.0)) < ftol,  &
			abs(eval_f32('acos(1.0f);') - acos(1.0)) < ftol,  &
			abs(eval_f64('acos(0.0);') - acos(0.d0)) < tol,  &
			abs(eval_f64('acos(1.0);') - acos(1.0d0)) < tol,  &
			abs(eval_f32('sum(acos([0.0f, 0.1f]));') - sum(acos([0.0, 0.1])))   < ftol,  &
			abs(eval_f32('sum(acos([0.5f, 0.1f]));') - sum(acos([0.5, 0.1])))   < ftol,  &
			abs(eval_f32('sum(acos([0.2f, 0.1f]));') - sum(acos([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(acos([0.0, 0.1]));') - sum(acos([0.d0, 0.1d0])))  < tol,  &
			abs(eval_f64('sum(acos([0.5, 0.1]));') - sum(acos([0.5d0, 0.1d0]))) < tol,  &
			abs(eval_f64('sum(acos([0.2, 0.1]));') - sum(acos([0.2d0, 0.1d0])))  < tol,  &
			abs(eval_f32('asin(0.0f);') - asin(0.0)) < ftol,  &
			abs(eval_f32('asin(0.1f);') - asin(0.1)) < ftol,  &
			abs(eval_f64('asin(0.0);') - asin(0.d0)) < tol,  &
			abs(eval_f64('asin(0.1);') - asin(0.1d0)) < tol,  &
			abs(eval_f32('sum(asin([0.0f, 0.1f]));') - sum(asin([0.0, 0.1])))   < ftol,  &
			abs(eval_f32('sum(asin([0.5f, 0.1f]));') - sum(asin([0.5, 0.1])))   < ftol,  &
			abs(eval_f32('sum(asin([0.2f, 0.1f]));') - sum(asin([0.2, 0.1])))   < ftol,  &
			abs(eval_f64('sum(asin([0.0, 0.1]));') - sum(asin([0.d0, 0.1d0])))  < tol,  &
			abs(eval_f64('sum(asin([0.5, 0.1]));') - sum(asin([0.5d0, 0.1d0]))) < tol,  &
			abs(eval_f64('sum(asin([0.2, 0.1]));') - sum(asin([0.2d0, 0.1d0])))  < tol,  &
			abs(eval_f32('atan(0.0f);') - atan(0.0)) < ftol,  &
			abs(eval_f32('atan(1.0f);') - atan(1.0)) < ftol,  &
			abs(eval_f64('atan(0.0);') - atan(0.d0)) < tol,  &
			abs(eval_f64('atan(1.0);') - atan(1.0d0)) < tol,  &
			abs(eval_f32('sum(atan([0.0f, 1.0f]));') - sum(atan([0.0, 1.0])))   < ftol,  &
			abs(eval_f32('sum(atan([0.5f, 1.0f]));') - sum(atan([0.5, 1.0])))   < ftol,  &
			abs(eval_f32('sum(atan([2.0f, 1.0f]));') - sum(atan([2.0, 1.0])))   < ftol,  &
			abs(eval_f64('sum(atan([0.0, 1.0]));') - sum(atan([0.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sum(atan([0.5, 1.0]));') - sum(atan([0.5d0, 1.0d0]))) < tol,  &
			abs(eval_f64('sum(atan([2.0, 1.0]));') - sum(atan([2.d0, 1.0d0])))  < tol,  &
			abs(eval_f64('sin(0.0);') - 0.d0) < tol, &
			abs(eval_f64('sin('//spi//');') - 0.d0) < tol, &
			abs(eval_f64('sin('//spi//' / 2);') - 1.d0) < tol, &
			abs(eval_f64('sin(-'//spi//' / 2);') - -1.d0) < tol, &
			abs(eval_f64('sin('//spi//' / 6);') - 0.5d0) < tol, &
			abs(eval_f64('sin('//spi//' / 4);') - sqrt(2.d0)/2) < tol, &
			abs(eval_f64('sin('//spi//' / 3);') - sqrt(3.d0)/2) < tol, &
			abs(eval_f64('cos(0.0);')           - 1.0d0) < tol, &
			abs(eval_f64('cos('//spi//' / 6);') - sqrt(3.d0)/2) < tol, &
			abs(eval_f64('cos('//spi//' / 4);') - sqrt(2.d0)/2) < tol, &
			abs(eval_f64('cos('//spi//' / 3);') - 0.5d0       ) < tol, &
			abs(eval_f64('tan(0.0);')           - 0.0d0) < tol, &
			abs(eval_f64('tan('//spi//' / 6);') - sqrt(3.d0)/3) < tol, &
			abs(eval_f64('tan('//spi//' / 4);') - 1.d0        ) < tol, &
			abs(eval_f64('tan('//spi//' / 3);') - sqrt(3.d0)  ) < tol, &
			abs(eval_f64('asin(0.0);') - 0.d0) < tol, &
			abs(eval_f64('asin(0.5);') - pi/6) < tol, &
			abs(eval_f64('asin(2.0**0.5 / 2);') - pi/4) < tol, &
			abs(eval_f64('asin(3.0**0.5 / 2);') - pi/3) < tol, &
			abs(eval_f64('acos(1.0);')           - 0.d0) < tol, &
			abs(eval_f64('acos(3.0**0.5 / 2);') - pi/6) < tol, &
			abs(eval_f64('acos(2.0**0.5 / 2);') - pi/4) < tol, &
			abs(eval_f64('acos(0.5);') - pi/3) < tol, &
			abs(eval_f64('atan(0.0);')           - 0.d0) < tol, &
			abs(eval_f64('atan(3.0**0.5 / 3);') - pi/6) < tol, &
			abs(eval_f64('atan(1.0);') - pi/4) < tol, &
			abs(eval_f64('atan(3.0**0.5);') - pi/3) < tol, &
			abs(eval_f64('sind(0.0);') - 0.d0) < tol, &
			abs(eval_f64('sind(180.0);') - 0.d0) < tol, &
			abs(eval_f64('sind(180.0 / 2);') - 1.d0) < tol, &
			abs(eval_f64('sind(-180.0 / 2);') - -1.d0) < tol, &
			abs(eval_f64('sind(180.0 / 6);') - 0.5d0) < tol, &
			abs(eval_f64('sind(180.0 / 4);') - sqrt(2.d0)/2) < tol, &
			abs(eval_f64('sind(180.0 / 3);') - sqrt(3.d0)/2) < tol, &
			abs(eval_f64('cosd(0.0);')           - 1.0d0) < tol, &
			abs(eval_f64('cosd(180.0 / 6);') - sqrt(3.d0)/2) < tol, &
			abs(eval_f64('cosd(180.0 / 4);') - sqrt(2.d0)/2) < tol, &
			abs(eval_f64('cosd(180.0 / 3);') - 0.5d0       ) < tol, &
			abs(eval_f64('tand(0.0);')           - 0.0d0) < tol, &
			abs(eval_f64('tand(180.0 / 6);') - sqrt(3.d0)/3) < tol, &
			abs(eval_f64('tand(180.0 / 4);') - 1.d0        ) < tol, &
			abs(eval_f64('tand(180.0 / 3);') - sqrt(3.d0)  ) < tol, &
			abs(eval_f32('sum(cosd([0.2f, 0.8f]));') - sum(cosd([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(cosd([0.1, 0.9]));') - sum(cosd([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f32('sum(sind([0.2f, 0.8f]));') - sum(sind([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(sind([0.1, 0.9]));') - sum(sind([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f32('sum(tand([0.2f, 0.8f]));') - sum(tand([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(tand([0.1, 0.9]));') - sum(tand([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f32('sum(acosd([0.2f, 0.8f]));') - sum(acosd([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(acosd([0.1, 0.9]));') - sum(acosd([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f32('sum(asind([0.2f, 0.8f]));') - sum(asind([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(asind([0.1, 0.9]));') - sum(asind([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f32('sum(atand([0.2f, 0.8f]));') - sum(atand([0.2, 0.8])))   < ftol,  &
			abs(eval_f64('sum(atand([0.1, 0.9]));') - sum(atand([0.1d0, 0.9d0])))  < tol,  &
			abs(eval_f64('asind(0.0);') - 0.d0) < tol, &
			abs(eval_f64('asind(0.5);') - 180.d0/6) < tol, &
			abs(eval_f64('asind(2.0**0.5 / 2);') - 180.d0/4) < tol, &
			abs(eval_f64('asind(3.0**0.5 / 2);') - 180.d0/3) < tol, &
			abs(eval_f64('acosd(1.0);')           - 0.d0) < tol, &
			abs(eval_f64('acosd(3.0**0.5 / 2);') - 180.d0/6) < tol, &
			abs(eval_f64('acosd(2.0**0.5 / 2);') - 180.d0/4) < tol, &
			abs(eval_f64('acosd(0.5);') - 180.d0/3) < tol, &
			abs(eval_f64('atand(0.0);')           - 0.d0) < tol, &
			abs(eval_f64('atand(3.0**0.5 / 3);') - 180.d0/6) < tol, &
			abs(eval_f64('atand(1.0);') - 180.d0/4) < tol, &
			abs(eval_f64('atand(3.0**0.5);') - 180.d0/3) < tol, &
			eval_i32('min(3, 2);')  == 2,  &
			eval_i32('min(2, 2);')  == 2,  &
			eval_i32('min(2, 3, 4);')  == 2,  &
			eval_i32('min(2, 4, 3);')  == 2,  &
			eval_i32('min(3, 2, 4);')  == 2,  &
			eval_i32('min(3, 4, 2);')  == 2,  &
			eval_i32('min(4, 2, 3);')  == 2,  &
			eval_i32('min(4, 3, 2);')  == 2,  &
			eval_i32('min(3, 2, 2);')  == 2,  &
			eval_i32('min(2, 2, 2);')  == 2,  &
			eval_i32('min(2, 4, 3, 5);')  == 2,  &
			eval_i32('min(5, 2, 3, 4);')  == 2,  &
			eval_i32('min(5, 1, 3, 4);')  == 1,  &
			eval_i32('min(5, 0, 3, 4);')  == 0,  &
			eval('min(4000000000, 3000000000);')  == "3000000000",  &
			eval('min(4000000000, 5000000000);')  == "4000000000",  &
			abs(eval_f32('min(3.0f, 2.0f);') - 2.0) < tol, &
			abs(eval_f32('min(2.0f, 3.0f);') - 2.0) < tol, &
			abs(eval_f32('min(4.0f, 3.0f, 5.0f);') - 3.0) < tol, &
			abs(eval_f32('min(4.0f, 3.0f, -5.0f);') - -5.0) < tol, &
			abs(eval_f32('max(3.0f, 2.0f);') - 3.0) < tol, &
			abs(eval_f32('max(2.0f, 3.0f);') - 3.0) < tol, &
			abs(eval_f32('max(4.0f, 3.0f, 5.0f);') - 5.0) < tol, &
			abs(eval_f32('max(4.0f, 3.0f, -5.0f);') - 4.0) < tol, &
			abs(eval_f64('min(3.0, 2.0);') - 2.0) < tol, &
			abs(eval_f64('min(2.0, 3.0);') - 2.0) < tol, &
			eval_i32('max(3, 2);')  == 3,  &
			eval_i32('max(2, 2);')  == 2,  &
			eval_i32('max(2, 3, 4);')  == 4,  &
			eval_i32('max(2, 4, 3);')  == 4,  &
			eval_i32('max(3, 2, 4);')  == 4,  &
			eval_i32('max(3, 4, 2);')  == 4,  &
			eval_i32('max(4, 2, 3);')  == 4,  &
			eval_i32('max(4, 3, 2);')  == 4,  &
			eval_i32('max(3, 2, 2);')  == 3,  &
			eval_i32('max(2, 2, 2);')  == 2,  &
			eval_i32('max(2, 4, 3, 5);')  == 5,  &
			eval_i32('max(5, 2, 3, 4);')  == 5,  &
			eval('max(4000000000, 3000000000);')  == "4000000000",  &
			eval('max(4000000000, 5000000000);')  == "5000000000",  &
			eval_i64('size([0; 5], 0);')  == 5,  &
			eval_i64('size([0; 6, 7], 0);')  == 6,  &
			eval_i64('size([0; 6, 7], 1);')  == 7,  &
			eval_i64('size([0; 6, 7, 8], 2);')  == 8,  &
			eval_i64('size([0.0; 5], 0);')  == 5,  &
			eval_i64('size([0.0; 6, 7], 0);')  == 6,  &
			eval_i64('size([0.0; 6, 7], 1);')  == 7,  &
			eval_i64('size([0.0; 6, 7, 8], 2);')  == 8,  &
			eval_i32('parse_i32(    "0");')  ==     0,  &
			eval_i32('parse_i32(    "1");')  ==     1,  &
			eval_i32('parse_i32(    "2");')  ==     2,  &
			eval_i32('parse_i32(   "34");')  ==    34,  &
			eval_i32('parse_i32( "1337");')  ==  1337,  &
			eval_i32('parse_i32(   "-1");')  ==    -1,  &
			eval_i32('parse_i32(   "-2");')  ==    -2,  &
			eval_i32('parse_i32(  "-34");')  ==   -34,  &
			eval_i32('parse_i32("-1337");')  == -1337,  &
			eval('parse_i64(    "0");')  ==     "0",  &
			eval('parse_i64(    "1");')  ==     "1",  &
			eval('parse_i64(    "2");')  ==     "2",  &
			eval('parse_i64(   "34");')  ==    "34",  &
			eval('parse_i64( "1337");')  ==  "1337",  &
			eval('parse_i64(   "-1");')  ==    "-1",  &
			eval('parse_i64(   "-2");')  ==    "-2",  &
			eval('parse_i64(  "-34");')  ==   "-34",  &
			eval('parse_i64("-1337");')  == "-1337",  &
			eval('parse_i64("-9123123123");')  == "-9123123123",  &
			eval('parse_i64( "9123123123");')  ==  "9123123123",  &
			abs(eval_f32('parse_f32("6.000000E+00");') - 6.000000E+00) < tol, &
			abs(eval_f32('parse_f32("3.000000E+00");') - 3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("2.000000E+00");') - 2.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-3.000000E+00");') - -3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-2.000000E+00");') - -2.000000E+00) < tol, &
			abs(eval_f32('parse_f32("3.0");') - 3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("2.0");') - 2.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-3.0");') - -3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-2.0");') - -2.000000E+00) < tol, &
			abs(eval_f32('parse_f32("3");') - 3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("2");') - 2.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-3");') - -3.000000E+00) < tol, &
			abs(eval_f32('parse_f32("-2");') - -2.000000E+00) < tol, &
			abs(eval_f64('parse_f64("-3.000000E+00");') - (-3.000000d+00)) < tol, &
			abs(eval_f64('parse_f64("-2.000000E+00");') - (-2.000000d+00)) < tol, &
			abs(eval_f64('parse_f64("3.0");') - 3.000000d+00) < tol, &
			abs(eval_f64('parse_f64("2.0");') - 2.000000d+00) < tol, &
			eval_i64('len(     "");')  == 0,  &
			eval_i64('len(    " ");')  == 1,  &
			eval_i64('len(   "  ");')  == 2,  &
			eval_i64('len(  "   ");')  == 3,  &
			eval_i64('len( "    ");')  == 4,  &
			eval_i64('len("     ");')  == 5,  &
			eval_i64('len(    "h");')  == 1,  &
			eval_i64('len(   "ht");')  == 2,  &
			eval_i64('len(  "htn");')  == 3,  &
			eval_i64('len( "htns");')  == 4,  &
			eval_i64('len("htns-");')  == 5,  &
			eval('repeat("a", 2);')  == "aa",  &
			eval('repeat("aBcD ", 3);')  == "aBcD aBcD aBcD ",  &
			eval('repeat(" aBcD", 3);')  == " aBcD aBcD aBcD",  &
			eval('repeat(" ", 4);')  == "    ",  &
			eval('repeat("aBcD", 0);')  == "",  &  ! 0 is empty. negatives crash
			eval('i32( 0.0);') ==   "0", &
			eval('i32( 1.1);') ==   "1", &
			eval('i32(-1.1);') ==  "-1", &
			eval('i32( 0.0f);') ==   "0", &
			eval('i32( 1.1f);') ==   "1", &
			eval('i32(-1.1f);') ==  "-1", &
			eval('i32(  0);') ==   "0", &
			eval('i32(  1);') ==   "1", &
			eval('i32( -1);') ==  "-1", &
			eval('i32(123);') == "123", &
			eval('i32(432);') == "432", &
			eval('i32("c");') ==  "99", &
			eval('i32("d");') == "100", &
			eval('i32("C");') ==  "67", &
			eval('i32("D");') ==  "68", &
			eval('i32("1");') ==  "49", &
			eval('i32("2");') ==  "50", &
			eval('i32("(");') ==  "40", &
			eval('i32(")");') ==  "41", &
			eval('i32(" ");') ==  "32", &
			eval('i32([ 0.0, 2.2, 3.3]);') ==  "[0, 2, 3]", &
			eval('i32([ 1.1, 2.2, 3.3]);') ==  "[1, 2, 3]", &
			eval('i32([-1.1, 2.2, 3.3]);') ==  "[-1, 2, 3]", &
			eval('i32([i64( 0), i64(2), i64(3)]);') ==  "[0, 2, 3]", &
			eval('i32([i64( 1), i64(2), i64(3)]);') ==  "[1, 2, 3]", &
			eval('i32([i64(-1), i64(2), i64(3)]);') ==  "[-1, 2, 3]", &
			eval('i32([ 0, 2, 3]);') ==  "[0, 2, 3]", &
			eval('i32([ 1, 2, 3]);') ==  "[1, 2, 3]", &
			eval('i32([-1, 2, 3]);') ==  "[-1, 2, 3]", &
			eval('char(99);') ==  "c", &  ! char() is inverse of i32(), but i32() can also do float-to-int casting
			eval('char(100);') ==  "d", &
			eval('char(67);') ==  "C", &
			eval('char(68);') ==  "D", &
			eval('char(49);') ==  "1", &
			eval('char(50);') ==  "2", &
			eval('char(40);') ==  "(", &
			eval('char(41);') ==  ")", &
			eval('char(32);') ==  " ", &
			eval('i64([ 0.0f, 2.2f, 3.3f]);') ==  "[0, 2, 3]", &
			eval('i64([ 0.0, 2.2, 3.3]);') ==  "[0, 2, 3]", &
			eval('i64([ 1.1, 2.2, 3.3]);') ==  "[1, 2, 3]", &
			eval('i64([-1.1, 2.2, 3.3]);') ==  "[-1, 2, 3]", &
			eval('i64([i64( 0), i64(2), i64(3)]);') ==  "[0, 2, 3]", &
			eval('i64([i64( 1), i64(2), i64(3)]);') ==  "[1, 2, 3]", &
			eval('i64([i64(-1), i64(2), i64(3)]);') ==  "[-1, 2, 3]", &
			eval('i64([ 0, 2, 3]);') ==  "[0, 2, 3]", &
			eval('i64([ 1, 2, 3]);') ==  "[1, 2, 3]", &
			eval('i64([-1, 2, 3]);') ==  "[-1, 2, 3]", &
			eval('any([true]);') == "true", &
			eval('any([false]);') == "false", &
			eval('any([false, true]);') == "true", &
			eval('any([false, false]);') == "false", &
			eval('all([true]);') == "true", &
			eval('all([false]);') == "false", &
			eval('all([false, true]);') == "false", &
			eval('all([true, true]);') == "true", &
			eval('count([true]);') == "1", &
			eval('count([true, true]);') == "2", &
			eval('count([0: 10] <  4);') == "4", &
			eval('count([0: 10] <  7);') == "7", &
			eval('count([0: 10] < 15);') == "10", &
			eval('sum([1: 4]);') == "6", &
			eval('sum([2: 4]);') == "5", &
			eval('sum([1: 5]);') == "10", &
			eval('sum([1: 2: 6]);') == "9", &
			eval('sum([7, 3, 15, 1; 2, 2]);') == "26", &
			eval('[sum([0.0f: 3.0f; 4])];') == "[6.000000E+00]", &
			eval('i32([sum([0.0: 3.0; 4])]);') == "[6]", &
			eval('sum(i64([1: 4]));') == "6", &
			eval('product([1: 5]);') == "24", &
			eval('product([2: 5]);') == "24", &
			eval('product(i64([3: 5]));') == "12", &
			abs(eval_f64('product([1.0: 4.0; 4]);') - 24.d0) < tol, &
			abs(eval_f32('product([1.0f: 4.0f; 4]);') - 24.0) < ftol, &
			abs(eval_f64('norm2([3.0, 4.0]);') - 5.d0) < tol, &
			abs(eval_f64('norm2([3.0, 4.0, 12.0]);') - 13.d0) < tol, &
			abs(eval_f32('norm2([3.0f, 4.0f]);') - 5.0) < ftol, &
			abs(eval_f64('norm2([1.0: 6.0; 6]);') - norm2(1.d0 * [(i, i = 1, 6)])) < tol, &
			abs(eval_f64('norm2([1.0,2.0,  3.0,4.0,  5.0,6.0;  2,3]);') - norm2(1.d0 * [(i, i = 1, 6)])) < tol, &
			abs(eval_f32('dot([1.0f, 2.0f], [3.0f, 4.0f]);') - 11.0) < ftol, &
			abs(eval_f64('dot([1.0, 2.0], [3.0, 4.0]);') - 11.d0) < tol, &
			abs(eval_i32('dot([1, 2], [3, 4]);') - 11) == 0, &
			abs(eval_i64('dot(i64([1, 2]), i64([3, 4]));') - 11) == 0, &
			eval_i32('min(1, 2);')  == 1   &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_intr_fns

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
				'let b = a;}') == '3',     &
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

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

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
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == '2' , &
			interpret_file(path//'test-03.syntran', quiet) == '14', &
			interpret_file(path//'test-04.syntran', quiet) == '12', &
			interpret_file(path//'test-05.syntran', quiet) == '14', &
			interpret_file(path//'test-06.syntran', quiet) == '16', &
			interpret_file(path//'test-07.syntran', quiet) == '16', &
			interpret_file(path//'test-08.syntran', quiet) == '15', &
			interpret_file(path//'test-09.syntran', quiet) == '20', &
			interpret_file(path//'test-10.syntran', quiet) == '18', &
			interpret_file(path//'test-11.syntran', quiet) == '16', &
			interpret_file(path//'test-12.syntran', quiet) == '14', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_if_else

!===============================================================================

subroutine unit_test_for_1(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'short for loops'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('let sum_ = 0; for x in [0:5] sum_ += x; sum_;', quiet) == '10', &
			eval('let sum_ = 0; for x in [0:6] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = 0; for x in [10:16] sum_ += x; sum_;', quiet) == '75', &
			eval('let sum_ = 0; for x in [5:-1:0] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = 0; for x in [5:-1:-2] sum_ += x; sum_;', quiet) == '14', &
			eval('let sum_ = 0; for x in [0:2:6] sum_ += x; sum_;', quiet) == '6', &
			eval('let sum_ = 0; for x in [0:2:7] sum_ += x; sum_;', quiet) == '12', &
			eval('let sum_ = 0; for x in [0:2:8] sum_ += x; sum_;', quiet) == '12', &
			eval('let sum_ = 0; for x in [0:2:9] sum_ += x; sum_;', quiet) == '20', &
			eval('let sum_ = 0; for x in [5:-2:0] sum_ += x; sum_;', quiet) == '9', &
			eval('let sum_ = 0; for x in [6:-2:0] sum_ += x; sum_;', quiet) == '12', &
			eval('let sum_ = 0; for x in [7:-2:0] sum_ += x; sum_;', quiet) == '16', &
			eval('let sum_ = 0; for x in [8:-2:0] sum_ += x; sum_;', quiet) == '20', &
			eval('let sum_ = 0; for x in [5:-2:-1] sum_ += x; sum_;', quiet) == '9', &
			eval('let sum_ = 0; for x in [6:-2:-1] sum_ += x; sum_;', quiet) == '12', &
			eval('let sum_ = 0; for x in [7:-2:-1] sum_ += x; sum_;', quiet) == '16', &
			eval('let sum_ = 0; for x in [8:-2:-1] sum_ += x; sum_;', quiet) == '20', &
			eval('let sum_ = 0; for x in [5:-2:-2] sum_ += x; sum_;', quiet) == '8', &
			eval('let sum_ = 0; for x in [6:-2:-2] sum_ += x; sum_;', quiet) == '12', &
			eval('let sum_ = 0; for x in [7:-2:-2] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = 0; for x in [8:-2:-2] sum_ += x; sum_;', quiet) == '20', &
			eval('let sum_ = 0; for x in [5:-2:-3] sum_ += x; sum_;', quiet) == '8', &
			eval('let sum_ = 0; for x in [6:-2:-3] sum_ += x; sum_;', quiet) == '10', &
			eval('let sum_ = 0; for x in [7:-2:-3] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = 0; for x in [8:-2:-3] sum_ += x; sum_;', quiet) == '18', &
			eval('let sum_ = i64(0); for x in [0:5] sum_ += x; sum_;', quiet) == '10', &
			eval('let sum_ = i64(0); for x in [0:6] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = i64(0); for x in [10:16] sum_ += x; sum_;', quiet) == '75', &
			eval('let sum_ = i64(0); for x in [5:-1:0] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = i64(0); for x in [5:-1:-2] sum_ += x; sum_;', quiet) == '14', &
			eval('let sum_ = i64(0); for x in [0:2:6] sum_ += x; sum_;', quiet) == '6', &
			eval('let sum_ = i64(0); for x in [i64(0):5] sum_ += x; sum_;', quiet) == '10', &
			eval('let sum_ = i64(0); for x in [i64(0):6] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = i64(0); for x in [i64(10):16] sum_ += x; sum_;', quiet) == '75', &
			eval('let sum_ = i64(0); for x in [i64(5):-1:0] sum_ += x; sum_;', quiet) == '15', &
			eval('let sum_ = i64(0); for x in [i64(5):-1:-2] sum_ += x; sum_;', quiet) == '14', &
			eval('let sum_ = i64(0); for x in [i64(0):2:6] sum_ += x; sum_;', quiet) == '6', &
			eval('let sum_ = 0.0f; for x in [0.0f: 1.0f: 5.0f] sum_ += x; [sum_];', quiet) == '[1.000000E+01]', &  ! [] is poor man's trim()
			eval('let sum_ = 0.0f; for x in [0.0f:1.0f:6.0f] sum_ += x; [sum_];', quiet)   == '[1.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [10.0f:1.0f:16.0f] sum_ += x; [sum_];', quiet) == '[7.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [5.0f:-1.0f:0.0f] sum_ += x; [sum_];', quiet)  == '[1.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [5.0f:-1.0f:-2.0f] sum_ += x; [sum_];', quiet) == '[1.400000E+01]', &
			eval('let sum_ = 0.0f; for x in [0.0f:2.0f:6.0f] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let vec = [0:6]; let sum_ = 0; for x in vec sum_ += x; sum_;', quiet) == '15', &
			eval('let vec = [0.0f:1.0f:6.0f]; let sum_ = 0.0f; for x in vec sum_ += x; [sum_];', quiet) == '[1.500000E+01]', &
			eval('let mat = [1,2,3, 4,5,6; 3,2]; let sum_ = 0; for x in mat sum_ += x; sum_;', quiet) == '21', &
			eval('let sum_ = 0.0f; for x in [0.0f: 4.0f; 5] sum_ += x; [sum_];', quiet) == '[1.000000E+01]', &  ! [] is poor man's trim()
			eval('let sum_ = 0.0f; for x in [0.0f: 5.0f; 6] sum_ += x; [sum_];', quiet)   == '[1.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [10.0f: 15.0f; 6] sum_ += x; [sum_];', quiet) == '[7.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [5.0f: 1.0f; 5] sum_ += x; [sum_];', quiet)  == '[1.500000E+01]', &
			eval('let sum_ = 0.0f; for x in [5.0f: -1.0f; 7] sum_ += x; [sum_];', quiet) == '[1.400000E+01]', &
			eval('let sum_ = 0.0f; for x in [0.0f: 4.0f; 3] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let sum_ = 0.0f; for x in [0.0f, 4.0f, 2.0f] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let sum_ = 0; for x in [4, 0, 2] sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = 0; for x in i32([4, 0, 2]) sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = i64(0); for x in [i64(4), i64(0), i64(2)] sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = 0; for x in [4, 0, 3, 2; 2, 2] sum_ += x; sum_;', quiet)   == '9', &
			eval('let sum_ = i64(0); for x in [i64(4), i64(0), i64(3), i64(2); 2, 2] sum_ += x; sum_;', quiet)   == '9', &
			eval('let sum_ = 0.0f; for x in [4.0f, 0.0f, 3.0f, 2.0f; 2, 2] sum_ += x; [sum_];', quiet)   == '[9.000000E+00]', &
			eval('let sum_ = 0; for x in [42; 5] sum_ += x; sum_;', quiet)   == '210', &
			eval('let sum_ = i64(0); for x in [i64(42); 5] sum_ += x; sum_;', quiet)   == '210', &
			eval('let sum_ = 0.0f; for x in [42.0f; 5] sum_ += x; [sum_];', quiet)   == '[2.100000E+02]', &
			eval('let sum_ = 0; for x in [42; 2, 3] sum_ += x; sum_;', quiet)   == '252', &
			eval('let sum_ = 0; for x in [42; 2, 10, 3] sum_ += x; sum_;', quiet)   == '2520', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_for_1

!===============================================================================

subroutine unit_test_for(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'for loops'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/for-loops/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '0', &
			interpret_file(path//'test-02.syntran', quiet) == '5050', &
			interpret_file(path//'test-03.syntran', quiet) == '1836311903', &
			interpret_file(path//'test-04.syntran', quiet) == '97', &
			interpret_file(path//'test-05.syntran', quiet) == '25', &
			interpret_file(path//'test-06.syntran', quiet) == '25', &
			interpret_file(path//'test-07.syntran', quiet) == '1836311903', &
			interpret_file(path//'test-08.syntran', quiet) == '0', &
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

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '0', &
			interpret_file(path//'test-02.syntran', quiet) == '1', &
			interpret_file(path//'test-03.syntran', quiet) == '5050', &
			interpret_file(path//'test-04.syntran', quiet) == '9973', &
			interpret_file(path//'test-05.syntran', quiet) == '5050', &
			interpret_file(path//'test-06.syntran', quiet) == '0', &
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

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
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

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			abs(eval_f32('1.0f;') - 1) < tol, &
			abs(eval_f32('6.9e1f;') - 6.9e1) < tol, &
			abs(eval_f32('+6.9e1f;') - +6.9e1) < tol, &
			abs(eval_f32('-6.9e1f;') - -6.9e1) < tol, &
			abs(eval_f32('0.333333333f;') - 0.333333333) < tol, &
			abs(eval_f32('1.1f +  2  ;') - (1.1 +  2  )) < tol, &
			abs(eval_f32('1   +  2.1f;') - (1   +  2.1)) < tol, &
			abs(eval_f32('1.1f +  2.1f;') - (1.1 +  2.1)) < tol, &
			abs(eval_f32('1.1f -  2  ;') - (1.1 -  2  )) < tol, &
			abs(eval_f32('1   -  2.1f;') - (1   -  2.1)) < tol, &
			abs(eval_f32('1.1f -  2.1f;') - (1.1 -  2.1)) < tol, &
			abs(eval_f32('1.1f *  2  ;') - (1.1 *  2  )) < tol, &
			abs(eval_f32('1   *  2.1f;') - (1   *  2.1)) < tol, &
			abs(eval_f32('1.1f *  2.1f;') - (1.1 *  2.1)) < tol, &
			abs(eval_f32('1.1f /  2  ;') - (1.1 /  2  )) < tol, &
			abs(eval_f32('1   /  2.1f;') - (1   /  2.1)) < tol, &
			abs(eval_f32('1.1f /  2.1f;') - (1.1 /  2.1)) < tol, &
			abs(eval_f32('1.1f ** 2  ;') - (1.1 ** 2  )) < tol, &
			abs(eval_f32('1   ** 2.1f;') - (1   ** 2.1)) < tol, &
			abs(eval_f32('1.1f ** 2.1f;') - (1.1 ** 2.1)) < tol, &
			abs(eval_f32('1.2e-3f + 4.5e-3f;') - (1.2e-3 + 4.5e-3)) < tol, &
			abs(eval_f32('1.2e-3f+4.5e-3f;') - (1.2e-3+4.5e-3)) < tol, &
			abs(eval_f32('1.2e-3f-4.5e-3f;') - (1.2e-3-4.5e-3)) < tol, &
			abs(eval_f32('1.2e+3f-4.5e+3f;') - (1.2e+3-4.5e+3)) < tol, &
			abs(eval_f32('1.1f + 2.2f + 34;') - (1.1 + 2.2 + 34)) < tol, &
			abs(eval_f32('1 + 2 * 3.3f;') - (1 + 2 * 3.3)) < tol, &
			abs(eval_f32('1 * 2 * 3.6f * 4;') - (1 * 2 * 3.6 * 4)) < tol, &
			abs(eval_f32('73 - 48.0f;') - (73 - 48.0)) < tol, &
			abs(eval_f32('73.1f - 48 - 21;') - (73.1 - 48 - 21)) < tol, &
			abs(eval_f32('24 / 6.3f;') - (24 / 6.3)) < tol, &
			abs(eval_f32('24 / 6 / 2.1f;') - (24 / 6 / 2.1)) < tol, &
			abs(eval_f32('2.0f ** 5;') - (2.0 ** 5)) < tol, &
			abs(eval_f32('3 ** 4.1f;') - (3 ** 4.1)) < tol, &
			abs(eval_f32('1.1f + i64(2);') - (3.1)) < tol, &
			abs(eval_f32('i64(2) + 1.1f;') - (3.1)) < tol, &
			abs(eval_f32('3.43f - 87654345 / 27 + 76 * 234 - 65432 / 63;') &
			       - (3.43 - 87654345 / 27 + 76 * 234 - 65432 / 63)) < tol &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_f32_1

!===============================================================================

subroutine unit_test_f64_1(npass, nfail)

	! Simple f64 float tests of arithmetic and comparisons with single-line
	! evaluations

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f64 arithmetic'

	logical, allocatable :: tests(:)

	real, parameter :: tol = 1.e-12

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			abs(eval_f64('1.0;') - 1) < tol, &
			abs(eval_f64('6.9e1;') - 6.9d1) < tol, &
			abs(eval_f64('+6.9e1;') - +6.9d1) < tol, &
			abs(eval_f64('-6.9e1;') - -6.9d1) < tol, &
			abs(eval_f64('0.333333333;') - 0.333333333d0) < tol, &
			abs(eval_f64('1.1 +  2  ;') - (1.1d0 +  2  )) < tol, &
			abs(eval_f64('1   +  2.1;') - (1   +  2.1d0)) < tol, &
			abs(eval_f64('1.1 +  2.1;') - (1.1d0 +  2.1d0)) < tol, &
			abs(eval_f64('1.1 -  2  ;') - (1.1d0 -  2  )) < tol, &
			abs(eval_f64('1   -  2.1;') - (1   -  2.1d0)) < tol, &
			abs(eval_f64('1.1 -  2.1;') - (1.1d0 -  2.1d0)) < tol, &
			abs(eval_f64('1.1 *  2  ;') - (1.1d0 *  2  )) < tol, &
			abs(eval_f64('1   *  2.1;') - (1   *  2.1d0)) < tol, &
			abs(eval_f64('1.1 *  2.1;') - (1.1d0 *  2.1d0)) < tol, &
			abs(eval_f64('1.1 /  2  ;') - (1.1d0 /  2  )) < tol, &
			abs(eval_f64('1   /  2.1;') - (1   /  2.1d0)) < tol, &
			abs(eval_f64('1.1 /  2.1;') - (1.1d0 /  2.1d0)) < tol, &
			abs(eval_f64('1.1 ** 2  ;') - (1.1d0 ** 2  )) < tol, &
			abs(eval_f64('1   ** 2.1;') - (1   ** 2.1d0)) < tol, &
			abs(eval_f64('1.1 ** 2.1;') - (1.1d0 ** 2.1d0)) < tol, &
			abs(eval_f64('1.2e-3 + 4.5e-3;') - (1.2d-3 + 4.5d-3)) < tol, &
			abs(eval_f64('1.2e-3+4.5e-3;') - (1.2d-3+4.5d-3)) < tol, &
			abs(eval_f64('1.2e-3-4.5e-3;') - (1.2d-3-4.5d-3)) < tol, &
			abs(eval_f64('1.2e+3-4.5e+3;') - (1.2d+3-4.5d+3)) < tol, &
			abs(eval_f64('1.1 + 2.2 + 34;') - (1.1d0 + 2.2d0 + 34)) < tol, &
			abs(eval_f64('1 + 2 * 3.3;') - (1 + 2 * 3.3d0)) < tol, &
			abs(eval_f64('1 * 2 * 3.6 * 4;') - (1 * 2 * 3.6d0 * 4)) < tol, &
			abs(eval_f64('73 - 48.0;') - (73 - 48.0d0)) < tol, &
			abs(eval_f64('73.1 - 48 - 21;') - (73.1d0 - 48 - 21)) < tol, &
			abs(eval_f64('24 / 6.3;') - (24 / 6.3d0)) < tol, &
			abs(eval_f64('24 / 6 / 2.1;') - (24 / 6 / 2.1d0)) < tol, &
			abs(eval_f64('2.0 ** 5;') - (2.0d0 ** 5)) < tol, &
			abs(eval_f64('3 ** 4.1;') - (3 ** 4.1d0)) < tol, &
			abs(eval_f64('1.1 + i64(2);') - (3.1d0)) < tol, &
			abs(eval_f64('i64(2) + 1.1;') - (3.1d0)) < tol, &
			abs(eval_f64('3.43 - 87654345 / 27 + 76 * 234 - 65432 / 63;') &
			       - (3.43d0 - 87654345 / 27 + 76 * 234 - 65432 / 63)) < tol, &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_f64_1

!===============================================================================

subroutine unit_test_f64_mix(npass, nfail)

	! Simple f64 float tests of arithmetic and comparisons with single-line
	! evaluations

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f64 mixed arithmetic'

	logical, allocatable :: tests(:)

	real(kind = 8), parameter :: tol = 1.e-12, ftol = 1.e-9

	write(*,*) 'Unit testing '//label//' ...'

	! There's not much need to test other operators besides `+` because of the
	! way `-`, `*`, `/`, and `**` are generated from templates
	!
	! mod (%) and unary negation are still independent

	tests = &
		[   &
			abs(eval_f64('1.1 + 2.0;') - (3.1d0)) < tol, &
			abs(eval_f64('1.1 + 2  ;') - (3.1d0)) < tol, &
			abs(eval_f64('1.1 + i64(2);') - (3.1d0)) < tol, &
			abs(eval_f64('1   + 2.1;') - (3.1d0)) < tol, &
			abs(eval_f64('i64(1) + 2.1;') - (3.1d0)) < tol, &
			abs(eval_f64('1.2e-3 + 4.5e-3;') - (1.2d-3 + 4.5d-3)) < tol, &
			abs(eval_f64('1.2e-3f + 4.5e-3;') - (1.2e-3 + 4.5d-3)) < tol, &
			abs(eval_f64('1.2e-3 + 4.5e-3f;') - (1.2d-3 + 4.5e-3)) < tol, &
			abs(eval_f64('1.1 + 2.0f;') - (1.1d0 + 2.0e0)) < tol, &
			abs(eval_f64('1.1f + 2.0;') - (1.1e0 + 2.0d0)) < tol, &
			abs(eval_f64('sum([1.1] + [2.0]);') - (3.1d0)) < ftol, &
			abs(eval_f64('sum([1.1f] + [2.0]);') - (1.1e0 + 2.0d0)) < ftol, &
			abs(eval_f64('sum([1.1] + [2.0f]);') - (1.1d0 + 2.0e0)) < ftol, &
			abs(eval_f64('sum([1.1] + [2]);') - (3.1d0)) < ftol, &
			abs(eval_f64('sum([2] + [1.1]);') - (3.1d0)) < ftol, &
			abs(eval_f64('sum(1.1 + [2.0]);') - (3.1d0)) < ftol, &
			abs(eval_f64('sum([1.1] + 2.0);') - (3.1d0)) < ftol, &
			abs(eval_f64('sum(1.1f + [2.0]);') - (1.1e0 + 2.0d0)) < ftol, &
			abs(eval_f64('sum([1.1f] + 2.0);') - (1.1e0 + 2.0d0)) < ftol, &
			abs(eval_f64('sum(1.1 + [2.0f]);') - (1.1d0 + 2.0e0)) < ftol, &
			abs(eval_f64('sum([1.1] + 2.0f);') - (1.1d0 + 2.0e0)) < ftol, &
			abs(eval_f64('sum([1.1] + 2);') - (3.1d0)) < ftol, &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_f64_mix

!===============================================================================

subroutine unit_test_literals(npass, nfail)

	! Hex, octal, and binary literals, use of "_" as numeric separators,
	! explicit type "'" suffixes, ...

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = "literals"

	logical, allocatable :: tests(:)

	double precision, parameter :: tol = 1.e-9

	write(*,*) "Unit testing "//label//" ..."

	tests = &
		[   &
			eval_i32("0x0;") == 0, &
			eval_i32("0x1;") == 1, &
			eval_i32(  "10;") == 5*2, &
			eval_i32("0x10;") == 16, &
			eval_i32("0x1'i32;") == 1, &
			eval_i64("0x1'i64;") == 1, &
			eval_i32("1'i32;") == 1, &
			eval_i64("1'i64;") == 1, &
			abs(eval_f32("1'f32;") - 1.0) < tol, &
			abs(eval_f64("1'f64;") - 1.0d0) < tol, &
			eval_i32("0o0;") == 0, &
			eval_i32("0o1;") == 1, &
			eval_i32("0o10;") == 8, &
			eval_i32("0o10'i32;") == 8, &
			eval_i64("0o10'i64;") == 8, &
			eval_i32("0o377_7777_7777;") == -1, &
			eval_i32("0o377_7777_7776;") == -2, &
			eval    ("0o377_7777_7777'i64;") == "4294967295", &
			eval_i32("0o1234_5670;") == 2739128, &
			eval_i32("0b0;") == 0, &
			eval_i32("0b1;") == 1, &
			eval_i32("0b10;") == 2, &
			eval_i32("0b10'i32;") == 2, &
			eval_i64("0b10'i64;") == 2, &
			eval_i32("0b11111111111111111111111111111111;") == -1, &
			eval_i32("0b11111111111111111111111111111110;") == -2, &
			eval    ("0b11111111111111111111111111111111'i64;") == "4294967295", &
			eval_i32("0b1010_1010;") == 170, &
			eval_i32("0b1111_1111;") == 255, &
			eval_i32("0x_1_;") == 1, &
			eval_i32("0x__1___;") == 1, &
			eval_i32("0xff;") == 255, &
			eval_i32("0xffffffff;")     == -1, &  ! "overflow" is an intended feature
			eval_i32("0xffff_ffff'i32;") == -1, &
			eval_i64("0xffff_ffff'i64;") == int(2, 8) ** 32 - 1, &
			eval    ("0xffff_ffff'i64;") == "4294967295", &
			eval_i32("0xffff_fffe;") == -2, &
			eval_i32("0x10000;") == 65536, &
			eval_i32("0x1_0000;") == 65536, &
			eval_i32("0x1__0000;") == 65536, &
			eval_i32("0x1___0000;") == 65536, &
			eval_i32("0x1____0_0__0___0;") == 65536, &
			eval_i32("1_000_000;") == 1000000, &
			eval_i32("1_234_567;") == 1234567, &
			abs(eval_f64("1.234_567;") - 1.234567d0) < tol, &
			abs(eval_f64("1._234_567_e3;") - 1234.567d0) < tol, &
			eval_i64("0x1_0000_0000;") == int(2, 8) ** 32, &
			eval_i64("0x1_0000_0001;") == int(2, 8) ** 32 + 1, &
			eval_i32("0x0123;") ==   291, &
			eval_i32("0x4567;") == 17767, &
			eval_i32("0x89ab;") == 35243, &
			eval_i32("0xcdef;") == 52719, &
			eval_i32("0x89AB;") == 35243, &
			eval_i32("0xCDEF;") == 52719, &
			eval_i64("0x0123456789abcdef;") == int(z"0123456789abcdef", 8), &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_literals

!===============================================================================

subroutine unit_test_bitwise(npass, nfail)

	! Bitwise operators: shift, and, xor, etc.

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = "bitwise operators"

	logical, allocatable :: tests(:)

	write(*,*) "Unit testing "//label//" ..."

	tests = &
		[   &
			eval_i32("1 << 0;") == 1, &
			eval_i32("1 << 1;") == 2, &
			eval_i32("1 << 3;") == 8, &
			eval_i32("0x0f << 4;") == 15*16, &
			eval_i32("0x0f << 4;") == eval_i32("0xf0;"), &
			eval_i32("0x00ff << 8;") == eval_i32("0xff00;"), &
			eval_i32("0x00ff00 << 8;") == eval_i32("0xff0000;"), &
			eval_i32("0xff00_0000 << 1;") == eval_i32("0xfe00_0000;"), &
			eval_i32("0xff00_0000 << 4;") == eval_i32("0xf000_0000;"), &
			eval_i32("0xff00_0000 << 8;") == eval_i32("0x0000_0000;"), &
			eval_i32("0b00001011 << 0;") == eval_i32("0b00001011;"), &
			eval_i32("0b00001011 << 1;") == eval_i32("0b00010110;"), &
			eval_i32("0b00001011 << 2;") == eval_i32("0b00101100;"), &
			eval_i32("0b00001011 << 3;") == eval_i32("0b01011000;"), &
			eval_i32("0b00001011 << 4;") == eval_i32("0b10110000;"), &
			eval("1 << [0: 4];") == "[1, 2, 4, 8]", &
			eval("1'i64 << [1: 5];") == "[2, 4, 8, 16]", &
			eval("[0: 4] << 0;") == "[0, 1, 2, 3]", &
			eval("[0: 4] << 2;") == "[0, 4, 8, 12]", &
			eval("[0b1111, 0b0111, 0b0011, 0b0001] << [0: 4];") == "[15, 14, 12, 8]", &
			eval("i64([0b1111, 0b0111, 0b0011, 0b0001]) << [0: 4];") == "[15, 14, 12, 8]", &
			eval("[0b1111, 0b0111, 0b0011, 0b0001] << i64([0: 4]);") == "[15, 14, 12, 8]", &
			eval("i64([0b1111, 0b0111, 0b0011, 0b0001]) << i64([0: 4]);") == "[15, 14, 12, 8]", &
			eval_i32("1024 >> 0;") == 1024, &
			eval_i32("1024 >> 1;") == 512, &
			eval_i32("1024 >> 2;") == 256, &
			eval_i32("1024 >> 3;") == 128, &
			eval_i32("1 >> 1;") == 0, &
			eval_i32("0x00ff00 >> 8;") == eval_i32("0x0000ff;"), &
			eval_i32("0b10110000 >> 0;") == eval_i32("0b10110000;"), &
			eval_i32("0b10110000 >> 1;") == eval_i32("0b01011000;"), &
			eval_i32("0b10110000 >> 2;") == eval_i32("0b00101100;"), &
			eval_i32("0b10110000 >> 3;") == eval_i32("0b00010110;"), &
			eval_i32("0b10110000 >> 4;") == eval_i32("0b00001011;"), &
			eval("1024 >> [0: 4];") == "[1024, 512, 256, 128]", &
			eval("1024'i64 >> [0: 4];") == "[1024, 512, 256, 128]", &
			eval_i32("0 ^ 0;") == 0, &
			eval_i32("1 ^ 1;") == 0, &
			eval_i32("255 ^ 255;") == 0, &
			eval_i32("0xff00 ^ 0x00ff;") == eval_i32("0xffff;"), &
			eval("(0xff00 ^ 0x00ff) == 0xffff;") == "true", &
			eval("0xff00 ^ 0x00ff == 0xffff;") == "true", &  ! ^ vs == precedence matches rust, not c
			eval("(0xf00f ^ 0x0ff0) == 0xffff;") == "true", &
			eval("(0b1010 ^ 0b0101) == 0b1111;") == "true", &
			eval("(0b1011 ^ 0b0101) == 0b1110;") == "true", &
			eval("(0b1011'i64 ^ 0b0101'i64) == 0b1110;") == "true", &
			eval("(0b1010 ^ 0b0001) == 0b1011;") == "true", &
			eval("all(([0b1010] ^ 0b0001) == [0b1011]);") == "true", &
			eval("all(([0b1110, 0b1101] ^ 0b1111) == [0b0001, 0b0010]);") == "true", &
			eval("all((i64([0b1110, 0b1101]) ^ 0b1111'i64) == [0b0001, 0b0010]);") == "true", &
			eval("all(([0b1110, 0b1101, 0b1011, 0b0111] ^ 0b1111) == [0b0001, 0b0010, 0b0100, 0b1000]);") == "true", &
			eval("all((0b0000 ^ [0b1110, 0b1101, 0b1011, 0b0111]) == [0b1110, 0b1101, 0b1011, 0b0111]);") == "true", &
			eval("all(([0b1110, 0b1101] ^ [0b1111, 0b0000]) == [0b0001, 0b1101]);") == "true", &
			eval("0xff00 | 0x00ff == 0xffff;") == "true", &
			eval("0xffff | 0x00ff == 0xffff;") == "true", &
			eval("0xffed | 0x00ff == 0xffff;") == "true", &
			eval("0b0110 | 0b1011 == 0b1111;") == "true", &
			eval("0b0110 | 0b1010 == 0b1110;") == "true", &
			eval("0b0110'i64 | 0b1010'i64 == 0b1110;") == "true", &
			eval("all([0b0110] | 0b1010 == [0b1110]);") == "true", &
			eval("all(0b0110 | [0b1010, 0b0101] == [0b1110, 0b0111]);") == "true", &
			eval("all(0b0110'i64 | i64([0b1010, 0b0101]) == [0b1110, 0b0111]);") == "true", &
			eval("0xff00 & 0x00ff == 0x0000;") == "true", &
			eval("0xffff & 0x00ff == 0x00ff;") == "true", &
			eval("0xffed & 0x00ff == 0x00ed;") == "true", &
			eval("0b0110 & 0b1011 == 0b0010;") == "true", &
			eval("0b1110 & 0b1010 == 0b1010;") == "true", &
			eval("0b0110'i64 & 0b1010'i64 == 0b0010;") == "true", &
			eval("all([0b0110] & 0b1010 == [0b0010]);") == "true", &
			eval("all(0b0110 & [0b1010, 0b0101] == [0b0010, 0b0100]);") == "true", &
			eval("all(0b0110'i64 & i64([0b1010, 0b0101]) == [0b0010, 0b0100]);") == "true", &
			eval("!0x0000_f00f == 0xffff_0ff0;") == "true", &
			eval("!0x0f0f_000f_fff0_0fff == 0xf0f0_fff0_000f_f000;") == "true", &
			eval("!-2 ==  1;") == "true", &
			eval("!-1 ==  0;") == "true", &
			eval("!0  == -1;") == "true", &
			eval("!1  == -2;") == "true", &
			eval("!-2'i64 ==  1;") == "true", &
			eval("!-1'i64 ==  0;") == "true", &
			eval("!0'i64  == -1;") == "true", &
			eval("!1'i64  == -2;") == "true", &
			eval("all(![0: 4] == [-1, -2, -3, -4]);") == "true", &
			eval("all(!i64([0: 4]) == [-1, -2, -3, -4]);") == "true", &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bitwise

!===============================================================================

subroutine unit_test_bit_ass(npass, nfail)

	! Compound assignment with bitwise operators: shift, and, xor, etc.

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = "compound bitwise assignment"

	logical, allocatable :: tests(:)

	write(*,*) "Unit testing "//label//" ..."

	! TODO: test arrays too
	tests = &
		[   &
			eval_i32('let x = 0xff00; x |= 0x00ff; return x;') == 65535, &
			eval_i32('let x = 0xff00; x |= 0x00fe; return x;') == 65534, &
			eval('let x = 0xff00; x |= 0x00fe; return x;') == eval('0xfffe;'), &
			eval('let x = 0x00AD; x |= 0xDE00; return x;') == eval('0xDEAD;'), &
			eval('let x = 0xff00; x &= 0x00ff; return x;') == eval('0x0000;'), &
			eval('let x = 0xBABE; x &= 0xffff; return x;') == eval('0xbabe;'), &
			eval('let x = 0xFFFF; x &= 0xbeaf; return x;') == eval('0xBEAF;'), &
			eval('let x = 0xaaff; x &= 0xffaa; return x;') == eval('0xaaaa;'), &
			eval('let x = 0xff00; x ^= 0x00fe; return x;') == eval('0xfffe;'), &
			eval('let x = 0xff00; x ^= 0xfffe; return x;') == eval('0x00fe;'), &
			eval('let x = 0xffff; x >>= 4; return x;') == eval('0x0fff;'), &
			eval('let x = 0xffff; x <<= 4; return x;') == eval('0xffff0;'), &
			eval('let x = 0xffff; x >>= 8; return x;') == eval('0x00ff;'), &
			eval('let x = 0xffff; x <<= 8; return x;') == eval('0xffff00;'), &
			eval('let x = 0xCAFE; x <<= 16; x |= 0xBABE; return x;') == eval('0xCAFE_BABE;'), &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bit_ass

!===============================================================================

subroutine unit_test_i64(npass, nfail)

	! Simple i64 integer tests of arithmetic with single-line

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'i64 arithmetic'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	real, parameter :: tol = 1.e-9

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: add tests covering conversion of floats to ints using i32() and
	! i64()

	tests = &
		[   &
			eval('8123123123;') == '8123123123', &
			eval('8123123123 + 7123123123;') == '15246246246', &
			eval('7123123123 + 8123123123;') == '15246246246', &
			eval('8123123123 + 1;')          ==  '8123123124', &
			eval('1 + 8123123123;')          ==  '8123123124', &
			eval('8123123123 - 7123123123;') ==  '1000000000', &
			eval('7123123123 - 8123123123;') == '-1000000000', &
			eval('8123123123 - 1;')          ==  '8123123122', &
			eval('1 - 8123123123;')          == '-8123123122', &
			eval('3000000000 * 3000000000;') == '9000000000000000000', &  ! close to i64 limit. 4 billion squared will overflow
			eval('-3000000000 * 3000000000;') == '-9000000000000000000', &
			eval('3000000000 * -3000000000;') == '-9000000000000000000', &
			eval('3000000000 * 2;') == '6000000000', &
			eval('2 * 3000000000;') == '6000000000', &
			eval('9000000000 / 3000000000;') == '3', &
			eval('6000000000 / 3000000000;') == '2', &
			eval('9000000000 / 3;') == '3000000000', &
			eval('6000000000 / 3;') == '2000000000', &
			eval('3000000000 ** 2;') == '9000000000000000000', &  ! close to i64 limit. 4 billion squared will overflow
			eval('i64(200000) ** 2;') == '40000000000', &
			eval('i64(200001) ** 2;') == '40000400001', &
			eval('i64(200002) ** 2;') == '40000800004', &  ! verified in python
			eval('i64(200000) ** 3;') == '8000000000000000', &
			eval('i64(200001) ** 3;') == '8000120000600001', &
			eval('i64(200002) ** 3;') == '8000240002400008', &
			eval('9000000000 % 2000000000;') == '1000000000', &
			eval('20000000000 % 7000000000;') == '6000000000', &
			eval('20000000001 % 7000000000;') == '6000000001', &
			eval('20000000002 % 7000000000;') == '6000000002', &
			eval('20000000003 % 7000000000;') == '6000000003', &
			eval('20000000000 % 10;') == '0', &
			eval('20000000001 % 10;') == '1', &
			eval('20000000002 % 10;') == '2', &
			eval('20000000003 % 10;') == '3', &
			eval('8123123123 == 8123123123;') == 'true', &
			eval('8123123123 == 8123123124;') == 'false', &
			eval('8123123124 == 8123123123;') == 'false', &
			eval('8123123123 <  8123123124;') == 'true', &
			eval('8123123124 <  8123123123;') == 'false', &
			eval('8123123123 <  8123123123;') == 'false', &
			eval('8123123123 >  8123123124;') == 'false', &
			eval('8123123124 >  8123123123;') == 'true', &
			eval('8123123123 >  8123123123;') == 'false', &
			eval('8123123123 <= 8123123124;') == 'true', &
			eval('8123123124 <= 8123123123;') == 'false', &
			eval('8123123123 <= 8123123123;') == 'true', &
			eval('8123123123 >= 8123123124;') == 'false', &
			eval('8123123124 >= 8123123123;') == 'true', &
			eval('8123123123 >= 8123123123;') == 'true', &
			eval('i64(42   )  == 1337 ;')  == 'false',  &
			eval('i64(31415)  == 31415;')  == 'true' ,  &
			eval('i64(36+7 )  == 43   ;')  == 'true' ,  &
			eval('i64(12   )  == 36/3 ;')  == 'true' ,  &
			eval('i64(12   )  == 36*3 ;')  == 'false',  &
			eval('i64(12   )  != 36*3 ;')  == 'true' ,  &
			eval('i64(12   )  != 36/3 ;')  == 'false',  &
			eval('i64(1337 )  < 1338  ;')  == 'true' ,  &
			eval('i64(1337 )  < 1337  ;')  == 'false',  &
			eval('i64(1337 )  < 1336  ;')  == 'false',  &
			eval('i64(1337 ) <= 1338  ;')  == 'true' ,  &
			eval('i64(1337 ) <= 1337  ;')  == 'true' ,  &
			eval('i64(1337 ) <= 1336  ;')  == 'false',  &
			eval('i64(1337 )  > 1338  ;')  == 'false',  &
			eval('i64(1337 )  > 1337  ;')  == 'false',  &
			eval('i64(1337 )  > 1336  ;')  == 'true' ,  &
			eval('i64(1337 ) >= 1338  ;')  == 'false',  &
			eval('i64(1337 ) >= 1337  ;')  == 'true' ,  &
			eval('i64(1337 ) >= 1336  ;')  == 'true' ,  &
			eval('42    == i64( 1337 );')  == 'false',  &
			eval('31415 == i64( 31415);')  == 'true' ,  &
			eval('36+7  == i64( 43   );')  == 'true' ,  &
			eval('12    == i64( 36/3 );')  == 'true' ,  &
			eval('12    == i64( 36*3 );')  == 'false',  &
			eval('12    != i64( 36*3 );')  == 'true' ,  &
			eval('12    != i64( 36/3 );')  == 'false',  &
			eval('1337  <  i64(1338  );')  == 'true' ,  &
			eval('1337  <  i64(1337  );')  == 'false',  &
			eval('1337  <  i64(1336  );')  == 'false',  &
			eval('1337 <=  i64(1338  );')  == 'true' ,  &
			eval('1337 <=  i64(1337  );')  == 'true' ,  &
			eval('1337 <=  i64(1336  );')  == 'false',  &
			eval('1337  >  i64(1338  );')  == 'false',  &
			eval('1337  >  i64(1337  );')  == 'false',  &
			eval('1337  >  i64(1336  );')  == 'true' ,  &
			eval('1337 >=  i64(1338  );')  == 'false',  &
			eval('1337 >=  i64(1337  );')  == 'true' ,  &
			eval('1337 >=  i64(1336  );')  == 'true' ,  &
			abs(eval_f32('i64(2) * 3.0f;', quiet) - 6.0) < tol, &
			abs(eval_f32('3.0f * i64(2);', quiet) - 6.0) < tol, &
			abs(eval_f32('i64(2) + 3.0f;', quiet) - 5.0) < tol, &
			abs(eval_f32('3.0f + i64(2);', quiet) - 5.0) < tol, &
			abs(eval_f64('i64(2) * 3.0;', quiet) - 6.0d0) < tol, &
			abs(eval_f64('3.0 * i64(2);', quiet) - 6.0d0) < tol, &
			abs(eval_f64('i64(2) + 3.0;', quiet) - 5.0d0) < tol, &
			abs(eval_f64('3.0 + i64(2);', quiet) - 5.0d0) < tol, &
			eval('[8000000000; 3];') == '[8000000000, 8000000000, 8000000000]', &
			eval('[8000000000: 8000000003];') == '[8000000000, 8000000001, 8000000002]', &
			eval('[8000000000: 2: 8000000006];') == '[8000000000, 8000000002, 8000000004]', &
			eval('[1: 1000000000: 4000000000];') == '[1, 1000000001, 2000000001, 3000000001]', & ! span accross i32 limit
			eval('[2147483640: 2147483660];') == &
				'[2147483640, 2147483641, 2147483642, 2147483643, ' &
				//'2147483644, 2147483645, 2147483646, 2147483647, ' &
				//'2147483648, 2147483649, 2147483650, 2147483651, ' &
				//'2147483652, 2147483653, 2147483654, 2147483655, ' &
				//'2147483656, 2147483657, 2147483658, 2147483659]', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_i64

!===============================================================================

subroutine unit_test_str(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'strings'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! Fortran allows either ' or " for strings.  Syntran requires ".  Both have
	! the same rule that doubling a quote produces a single quote literal

	!print *, 'apostrophe: '''
	!print *, "apostrophe: '"
	!print *, 'quote: "'
	!print *, "quote: """

	! TODO: test array to str conversion, after deciding whether to include brackets or not
	!
	! Note that printing strings does not include quotes, so that's inconsistent
	! with the way that printed arrays currently have brackets

	tests = &
		[   &
			eval('"hello";') == 'hello', &
			eval('"hello ""name""";') == 'hello "name"', &
			eval('"""name"", hello";') == '"name", hello', &
			eval('"hello ""firstname"" ""lastname""";') == 'hello "firstname" "lastname"', &
			eval('"hello ""firstname"" lastname";') == 'hello "firstname" lastname', &
			eval('"two """"quotes""""";') == 'two ""quotes""', &
			eval('"apostrophe: ''";') == 'apostrophe: ''', &
			eval('"hello " + "world";') == 'hello world', &
			eval('"hello " + ("planet " + "earth");') == 'hello planet earth', &
			eval('"testing " + str(1);') == 'testing 1', &
			eval('"testing " + str(true) ;') == 'testing true', &
			eval('"testing " + str(false);') == 'testing false', &
			eval('"testing " + str(1.0f);') == 'testing     1.000000E+00', &
			eval('"testing testing " + str(1) + " " + str(2);') == 'testing testing 1 2', &
			eval('"testing " + str(1, " ", 2, " ", 1, " ", 2);') == 'testing 1 2 1 2', &
			trimw(eval('str(1.0);')) == '1.000000000000000E+00', &
			eval('" " == " "  ;') == 'true', &
			eval('"  " == "  ";') == 'true', &
			eval('"" != " "   ;') == 'true', &
			eval('" " != "	" ;') == 'true', &  ! space .ne. tab
			eval('" " != ""   ;') == 'true', &
			eval('" " != "  " ;') == 'true', &
			eval('"  " != " " ;') == 'true', &
			eval('" a " == " a ";')  == 'true', &
			eval('"a " != " a ";')  == 'true', &
			eval('" a" != " a ";')  == 'true', &
			eval('" a " != "a ";')  == 'true', &
			eval('" a " != " a";')  == 'true', &
			eval('" " == ["", " ", "  "];')  == '[false, true, false]', &
			eval('["", " ", "  "] == " ";')  == '[false, true, false]', &
			eval('["", " ", "  "] == [" ", " ", " "];')  == '[false, true, false]', &
			eval('"hello world";') == 'hello world'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_str

!===============================================================================

subroutine unit_test_substr(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'substrings'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('let s = "hello world"; s[0];', quiet) == 'h', &
			eval('let s = "hello world"; s[1];', quiet) == 'e', &
			eval('let s = "hello world"; s[2];', quiet) == 'l', &
			eval('let s = "hall*"; s[4] = "o"; s;', quiet) == 'hallo', &
			eval('let s = "h*lp"; s[1] = "e"; s;', quiet) == 'help', &
			eval('let s = "hello world"; s[0:2];', quiet) == 'he', &
			eval('let s = "hello world"; s[0:3];', quiet) == 'hel', &
			eval('let s = "hello world"; s[1:3];', quiet) == 'el', &
			eval('let s = "hello world"; s[1:4];', quiet) == 'ell', &
			eval('let s = "hello world"; s[1:5];', quiet) == 'ello', &
			eval('let s = "hello world"; s[1:6];', quiet) == 'ello ', &
			eval('let s = "hello world"; s[6:11];', quiet) == 'world', &
			eval('let s = "hello world"; s[3];', quiet) == 'l'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_substr

!===============================================================================

subroutine unit_test_array_i32_1(npass, nfail)

	! Simple i32 array tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'i32 arrays'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! Because test evaluation results are tested by comparing strings, output
	! white space is significant!  Ints are formatted in min width, and array
	! elements are separated by a comma and a single space

	! TODO: test empty arrays.  As of 0.0.13, empty literal arrays cannot be
	! assigned, but this can be worked around with ubound < lbound, e.g.:
	!
	!     let v = [0: -1];
	!     // []

	tests = &
		[   &
			eval('[42];') == '[42]', &
			eval('[-42,1337];') == '[-42, 1337]', &
			eval('[3, 2, 1];') == '[3, 2, 1]', &
			eval('[1: 4];') == '[1, 2, 3]', &
			eval('[1: 2: 5];') == '[1, 3]', &
			eval('[1: 2: 6];') == '[1, 3, 5]', &
			eval('[1: 2: 7];') == '[1, 3, 5]', &
			eval('[1: 2: 8];') == '[1, 3, 5, 7]', &
			eval('[4: 2: 8];') == '[4, 6]', &
			eval('[4: 2: 9];') == '[4, 6, 8]', &
			eval('[4: 2: 10];') == '[4, 6, 8]', &
			eval('[4: 2: 11];') == '[4, 6, 8, 10]', &
			eval('[11: -2: 4];') == '[11, 9, 7, 5]', &
			eval('[10: -2: 4];') == '[10, 8, 6]', &
			eval('[ 9: -2: 4];') == '[9, 7, 5]', &
			eval('[2-3: 6/3 + 3];') == '[-1, 0, 1, 2, 3, 4]', &
			eval('let myArray = [2-3: 6/3 + 3];') == '[-1, 0, 1, 2, 3, 4]', &
			eval('[42; 3];') == '[42, 42, 42]', &
			eval('[1337; 4];') == '[1337, 1337, 1337, 1337]', &
			eval('[[0:3], [10:12]];') == '[0, 1, 2, 10, 11]', &
			eval('[[10:13], [0:2]];') == '[10, 11, 12, 0, 1]', &
			eval('[[10:12], [0:3]];') == '[10, 11, 0, 1, 2]', &
			eval('[[0:3], [10]];') == '[0, 1, 2, 10]', &
			eval('[[5; 2], [0:3], [10]];') == '[5, 5, 0, 1, 2, 10]', &
			eval('[[5; 2], [0:2:6], [10]];') == '[5, 5, 0, 2, 4, 10]', &
			eval('[48-6, 13*100 + 37];') == '[42, 1337]'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_i32_1

!===============================================================================

subroutine unit_test_arr_comp(npass, nfail)

	! Maybe rename this to unit_test_array_cmp?  It's long enough without +, *,
	! etc.

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'array comparisons'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('[0, 0] == 0;') == '[true, true]', &
			eval('[0, 1] == 0;') == '[true, false]', &
			eval('[1, 0] == 0;') == '[false, true]', &
			eval('[1, 1] == 0;') == '[false, false]', &
			eval('0 == [0, 0];') == '[true, true]', &
			eval('0 == [0, 1];') == '[true, false]', &
			eval('0 == [1, 0];') == '[false, true]', &
			eval('0 == [1, 1];') == '[false, false]', &
			eval('[3, 3] == [3, 3];') == '[true, true]', &
			eval('[7, 8] == [7, 8];') == '[true, true]', &
			eval('[7, 8] == [8, 7];') == '[false, false]', &
			eval('[3, 2] == [3, 1];') == '[true, false]', &
			eval('[4, 3] == [1, 3];') == '[false, true]', &
			eval('[5, 6] == [1, 1];') == '[false, false]', &
			eval('"b" == ["a", "b", "c"];') == '[false, true, false]', &
			eval('["a", "b", "c"] == "c";') == '[false, false, true]', &
			eval('["a", "b", "c"] == ["d", "b", "c"];') == '[false, true, true]', &
			eval('[false, false] == false;') == '[true, true]', &
			eval('[false, true]  == false;') == '[true, false]', &
			eval('true == [false, false];') == '[false, false]', &
			eval('true == [false, true] ;') == '[false, true]', &
			eval('[false, false] == [false, true];') == '[true, false]', &
			eval('i64(2) == [i64(0), i64(2), i64(3)];') == '[false, true, false]', &
			eval('[i64(0), i64(2), i64(3)] == i64(3);') == '[false, false, true]', &
			eval('[i64(0), i64(2), i64(3)] == [i64(4), i64(2), i64(3)];') == '[false, true, true]', &
			eval('2.0 == [0.0, 2.0, 3.0];') == '[false, true, false]', &
			eval('[0.0, 2.0, 3.0] == 3.0;') == '[false, false, true]', &
			eval('[0.0, 2.0, 3.0] == [4.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2.0f == [0.0f, 2.0f, 3.0f];') == '[false, true, false]', &
			eval('[0.0f, 2.0f, 3.0f] == 3.0f;') == '[false, false, true]', &
			eval('[0.0f, 2.0f, 3.0f] == [4.0f, 2.0f, 3.0f];') == '[false, true, true]', &
			eval('2 == [i64(0), i64(2), i64(3)];') == '[false, true, false]', &
			eval('[0, 2, 3] == i64(3);') == '[false, false, true]', &
			eval('[0, 2, 3] == [i64(4), i64(2), i64(3)];') == '[false, true, true]', &
			eval('i64(2) == [0, 2, 3];') == '[false, true, false]', &
			eval('[i64(0), i64(2), i64(3)] == 3;') == '[false, false, true]', &
			eval('[i64(0), i64(2), i64(3)] == [4, 2, 3];') == '[false, true, true]', &
			eval('[0, 0] != 0;') == '[false, false]', &
			eval('[0, 1] != 0;') == '[false, true]', &
			eval('[1, 0] != 0;') == '[true, false]', &
			eval('[1, 1] != 0;') == '[true, true]', &
			eval('0 != [0, 0];') == '[false, false]', &
			eval('0 != [0, 1];') == '[false, true]', &
			eval('0 != [1, 0];') == '[true, false]', &
			eval('0 != [1, 1];') == '[true, true]', &
			eval('[3, 3] != [3, 3];') == '[false, false]', &
			eval('[7, 8] != [7, 8];') == '[false, false]', &
			eval('[7, 8] != [8, 7];') == '[true, true]', &
			eval('[3, 2] != [3, 1];') == '[false, true]', &
			eval('[4, 3] != [1, 3];') == '[true, false]', &
			eval('[5, 6] != [1, 1];') == '[true, true]', &
			eval('"b" != ["a", "b", "c"];') == '[true, false, true]', &
			eval('["a", "b", "c"] != "c";') == '[true, true, false]', &
			eval('["a", "b", "c"] != ["d", "b", "c"];') == '[true, false, false]', &
			eval('[false, false] != false;') == '[false, false]', &
			eval('[false, true]  != false;') == '[false, true]', &
			eval('true != [false, false];') == '[true, true]', &
			eval('true != [false, true] ;') == '[true, false]', &
			eval('[false, false] != [false, true];') == '[false, true]', &
			eval('i64(2) != [i64(0), i64(2), i64(3)];') == '[true, false, true]', &
			eval('[i64(0), i64(2), i64(3)] != i64(3);') == '[true, true, false]', &
			eval('[i64(0), i64(2), i64(3)] != [i64(4), i64(2), i64(3)];') == '[true, false, false]', &
			eval('2.0 != [0.0, 2.0, 3.0];') == '[true, false, true]', &
			eval('[0.0, 2.0, 3.0] != 3.0;') == '[true, true, false]', &
			eval('[0.0, 2.0, 3.0] != [4.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2 != [i64(0), i64(2), i64(3)];') == '[true, false, true]', &
			eval('[0, 2, 3] != i64(3);') == '[true, true, false]', &
			eval('[0, 2, 3] != [i64(4), i64(2), i64(3)];') == '[true, false, false]', &
			eval('i64(2) != [0, 2, 3];') == '[true, false, true]', &
			eval('[i64(0), i64(2), i64(3)] != 3;') == '[true, true, false]', &
			eval('[i64(0), i64(2), i64(3)] != [4, 2, 3];') == '[true, false, false]', &
			eval('any([0: 5] ==  0);') == 'true', &
			eval('any([0: 5] == -0);') == 'true', &
			eval('any([0: 5] ==  4);') == 'true', &
			eval('any([0: 5] == -1);') == 'false', &
			eval('any([0: 5] ==  5);') == 'false', &
			eval('all([0: 5] ==  0);') == 'false', &
			eval('all([0: 5] ==  4);') == 'false', &
			eval('all([0; 5] ==  0);') == 'true', &
			eval('all([42;5] == 42);') == 'true', &
			eval('all([7, 7, 7] ==  7);') == 'true', &
			eval('all([8, 7, 7] ==  7);') == 'false', &
			eval('all([7, 8, 7] ==  7);') == 'false', &
			eval('all([7, 7, 8] ==  7);') == 'false', &
			eval('[0, 0] > 0;') == '[false, false]', &
			eval('[0, 1] > 0;') == '[false, true]', &
			eval('[1, 0] > 0;') == '[true, false]', &
			eval('[1, 1] > 0;') == '[true, true]', &
			eval('0 > [0, 0];') == '[false, false]', &
			eval('0 > [0, 1];') == '[false, false]', &
			eval('1 > [1, 0];') == '[false, true]', &
			eval('1 > [0, 0];') == '[true, true]', &
			eval('[7, 8] > [8, 7];') == '[false, true]', &
			eval('[3, 2] > [3, 1];') == '[false, true]', &
			eval('[4, 3] > [1, 3];') == '[true, false]', &
			eval('[5, 6] > [1, 1];') == '[true, true]', &
			eval('i64(2) > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('[i64(0), i64(2), i64(3)] > i64(0);') == '[false, true, true]', &
			eval('[i64(4), i64(2), i64(3)] > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('2.0 > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] > 2.0;') == '[false, false, true]', &
			eval('[4.0, 2.0, 3.0] > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0f > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('[0.0f, 2.0f, 3.0f] > 2.0;') == '[false, false, true]', &
			eval('[4.0f, 2.0f, 3.0f] > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0 > [0.0f, 2.0f, 3.0f];') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] > 2.0f;') == '[false, false, true]', &
			eval('[4.0, 2.0, 3.0] > [0.0f, 2.0f, 3.0f];') == '[true, false, false]', &
			eval('2 > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('[0, 2, 3] > i64(2);') == '[false, false, true]', &
			eval('[4, 2, 3] > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('i64(2) > [0, 2, 3];') == '[true, false, false]', &
			eval('[i64(0), i64(2), i64(3)] > 2;') == '[false, false, true]', &
			eval('[i64(4), i64(2), i64(3)] > [0, 2, 3];') == '[true, false, false]', &
			eval('2 > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('[0, 2, 3] > 2.0;') == '[false, false, true]', &
			eval('[4, 2, 3] > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0 > [0, 2, 3];') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] > 2;') == '[false, false, true]', &
			eval('[4.0, 2.0, 3.0] > [0, 2, 3];') == '[true, false, false]', &
			eval('i64(2) > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('[i64(0), i64(2), i64(3)] > 2.0;') == '[false, false, true]', &
			eval('[i64(4), i64(2), i64(3)] > [0.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0 > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] > i64(2);') == '[false, false, true]', &
			eval('[4.0, 2.0, 3.0] > [i64(0), i64(2), i64(3)];') == '[true, false, false]', &
			eval('[0, 0] < 1;') == '[true, true]', &
			eval('[0, 1] < 1;') == '[true, false]', &
			eval('[1, 0] < 1;') == '[false, true]', &
			eval('[1, 1] < 1;') == '[false, false]', &
			eval('0 < [0, 0];') == '[false, false]', &
			eval('0 < [0, 1];') == '[false, true]', &
			eval('0 < [1, 0];') == '[true, false]', &
			eval('0 < [1, 1];') == '[true, true]', &
			eval('[7, 8] < [8, 7];') == '[true, false]', &
			eval('[3, 2] < [4, 2];') == '[true, false]', &
			eval('[4, 2] < [1, 3];') == '[false, true]', &
			eval('[5, 6] < [1, 1];') == '[false, false]', &
			eval('i64(2) < [i64(0), i64(2), i64(3)];') == '[false, false, true]', &
			eval('[i64(0), i64(2), i64(3)] < i64(2);') == '[true, false, false]', &
			eval('[i64(4), i64(1), i64(3)] < [i64(0), i64(2), i64(3)];') == '[false, true, false]', &
			eval('2.0 < [0.0, 2.0, 3.0];') == '[false, false, true]', &
			eval('[0.0, 2.0, 3.0] < 2.0;') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] < [0.0, 2.1, 3.1];') == '[false, true, true]', &
			eval('2 < [i64(0), i64(2), i64(3)];') == '[false, false, true]', &
			eval('[0, 2, 3] < i64(2);') == '[true, false, false]', &
			eval('[0, 2, 3] < [i64(4), i64(2), i64(3)];') == '[true, false, false]', &
			eval('i64(2) < [0, 2, 3];') == '[false, false, true]', &
			eval('[i64(0), i64(2), i64(3)] < 2;') == '[true, false, false]', &
			eval('[i64(0), i64(2), i64(3)] < [4, 2, 3];') == '[true, false, false]', &
			eval('2 < [0.0, 2.0, 3.0];') == '[false, false, true]', &
			eval('[0, 2, 3] < 2.0;') == '[true, false, false]', &
			eval('[0, 2, 3] < [4.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0 < [0, 2, 3];') == '[false, false, true]', &
			eval('[0.0, 2.0, 3.0] < 2;') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] < [4, 2, 3];') == '[true, false, false]', &
			eval('i64(2) < [0.0, 2.0, 3.0];') == '[false, false, true]', &
			eval('[i64(0), i64(2), i64(3)] < 2.0;') == '[true, false, false]', &
			eval('[i64(0), i64(2), i64(3)] < [4.0, 2.0, 3.0];') == '[true, false, false]', &
			eval('2.0 < [i64(0), i64(2), i64(3)];') == '[false, false, true]', &
			eval('[0.0, 2.0, 3.0] < i64(2);') == '[true, false, false]', &
			eval('[0.0, 2.0, 3.0] < [i64(4), i64(2), i64(3)];') == '[true, false, false]', &
			eval('[0, 0] >= 1;') == '[false, false]', &
			eval('[0, 1] >= 1;') == '[false, true]', &
			eval('[1, 0] >= 1;') == '[true, false]', &
			eval('[1, 1] >= 1;') == '[true, true]', &
			eval('0 >= [0, 0];') == '[true, true]', &
			eval('0 >= [0, 1];') == '[true, false]', &
			eval('0 >= [1, 0];') == '[false, true]', &
			eval('0 >= [1, 1];') == '[false, false]', &
			eval('[7, 8] >= [8, 7];') == '[false, true]', &
			eval('[3, 2] >= [4, 2];') == '[false, true]', &
			eval('[4, 2] >= [1, 3];') == '[true, false]', &
			eval('[5, 6] >= [1, 1];') == '[true, true]', &
			eval('i64(2) >= [i64(0), i64(2), i64(3)];') == '[true, true, false]', &
			eval('[i64(0), i64(2), i64(3)] >= i64(2);') == '[false, true, true]', &
			eval('[i64(4), i64(1), i64(3)] >= [i64(0), i64(2), i64(3)];') == '[true, false, true]', &
			eval('2.0 >= [0.0, 2.0, 3.0];') == '[true, true, false]', &
			eval('[0.0, 2.0, 3.0] >= 2.0;') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] >= [0.0, 2.1, 3.1];') == '[true, false, false]', &
			eval('2 >= [i64(0), i64(2), i64(3)];') == '[true, true, false]', &
			eval('[0, 2, 3] >= i64(2);') == '[false, true, true]', &
			eval('[0, 2, 3] >= [i64(4), i64(2), i64(3)];') == '[false, true, true]', &
			eval('i64(2) >= [0, 2, 3];') == '[true, true, false]', &
			eval('[i64(0), i64(2), i64(3)] >= 2;') == '[false, true, true]', &
			eval('[i64(0), i64(2), i64(3)] >= [4, 2, 3];') == '[false, true, true]', &
			eval('2 >= [0.0, 2.0, 3.0];') == '[true, true, false]', &
			eval('[0, 2, 3] >= 2.0;') == '[false, true, true]', &
			eval('[0, 2, 3] >= [4.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2.0 >= [0, 2, 3];') == '[true, true, false]', &
			eval('[0.0, 2.0, 3.0] >= 2;') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] >= [4, 2, 3];') == '[false, true, true]', &
			eval('i64(2) >= [0.0, 2.0, 3.0];') == '[true, true, false]', &
			eval('[i64(0), i64(2), i64(3)] >= 2.0;') == '[false, true, true]', &
			eval('[i64(0), i64(2), i64(3)] >= [4.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2.0 >= [i64(0), i64(2), i64(3)];') == '[true, true, false]', &
			eval('[0.0, 2.0, 3.0] >= i64(2);') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] >= [i64(4), i64(2), i64(3)];') == '[false, true, true]', &
			eval('[0, 0] <= 0;') == '[true, true]', &
			eval('[0, 1] <= 0;') == '[true, false]', &
			eval('[1, 0] <= 0;') == '[false, true]', &
			eval('[1, 1] <= 0;') == '[false, false]', &
			eval('0 <= [0, 0];') == '[true, true]', &
			eval('0 <= [0, 1];') == '[true, true]', &
			eval('1 <= [1, 0];') == '[true, false]', &
			eval('1 <= [0, 0];') == '[false, false]', &
			eval('[7, 8] <= [8, 7];') == '[true, false]', &
			eval('[3, 2] <= [3, 1];') == '[true, false]', &
			eval('[4, 3] <= [1, 3];') == '[false, true]', &
			eval('[5, 6] <= [1, 1];') == '[false, false]', &
			eval('i64(2) <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			eval('[i64(0), i64(2), i64(3)] <= i64(0);') == '[true, false, false]', &
			eval('[i64(4), i64(2), i64(3)] <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			eval('2.0 <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] <= 2.0;') == '[true, true, false]', &
			eval('[4.0, 2.0, 3.0] <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2 <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			eval('[0, 2, 3] <= i64(2);') == '[true, true, false]', &
			eval('[4, 2, 3] <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			eval('i64(2) <= [0, 2, 3];') == '[false, true, true]', &
			eval('[i64(0), i64(2), i64(3)] <= 2;') == '[true, true, false]', &
			eval('[i64(4), i64(2), i64(3)] <= [0, 2, 3];') == '[false, true, true]', &
			eval('2 <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('[0, 2, 3] <= 2.0;') == '[true, true, false]', &
			eval('[4, 2, 3] <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2.0 <= [0, 2, 3];') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] <= 2;') == '[true, true, false]', &
			eval('[4.0, 2.0, 3.0] <= [0, 2, 3];') == '[false, true, true]', &
			eval('i64(2) <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('[i64(0), i64(2), i64(3)] <= 2.0;') == '[true, true, false]', &
			eval('[i64(4), i64(2), i64(3)] <= [0.0, 2.0, 3.0];') == '[false, true, true]', &
			eval('2.0 <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			eval('[0.0, 2.0, 3.0] <= i64(2);') == '[true, true, false]', &
			eval('[4.0, 2.0, 3.0] <= [i64(0), i64(2), i64(3)];') == '[false, true, true]', &
			.false. &
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_arr_comp

!===============================================================================

subroutine unit_test_arr_op(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'array arithmetic'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('[0, 1] + 2;') == '[2, 3]', &
			eval('[0, 1, 2] + 3;') == '[3, 4, 5]', &
			eval('[0, 1, 2] + -1;') == '[-1, 0, 1]', &
			eval('[3: 9] + -2;') == '[1, 2, 3, 4, 5, 6]', &
			eval('[3: 2: 9] + -2;') == '[1, 3, 5]', &
			eval('2 + [0, 1];') == '[2, 3]', &
			eval('3 + [0, 1, 2];') == '[3, 4, 5]', &
			eval('-1 + [0, 1, 2];') == '[-1, 0, 1]', &
			eval('-2 + [3: 9];') == '[1, 2, 3, 4, 5, 6]', &
			eval('-2 + [3: 2: 9];') == '[1, 3, 5]', &
			eval('[0, 1] + [1, 2];') == '[1, 3]', &
			eval('[0, 1, 2] + [3, 4, 5];') == '[3, 5, 7]', &
			eval('[0, 1, 2] + [-1, 5, -3];') == '[-1, 6, -1]', &
			eval('[3: 9] + [-3: -1: -9];') == '[0, 0, 0, 0, 0, 0]', &
			eval('[3: 2: 9] + [-2, -3, -4];') == '[1, 2, 3]', &
			eval('all([0: 99] + [99: -1: 0] == 99);') == 'true', &
			eval('i32([0.0f, 1.0f] + 2.0);') == '[2, 3]', &
			eval('i32([0.0, 1.0] + 2.0f);') == '[2, 3]', &
			eval('[0.0f, 1.0f] + 2.0f;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + 3.0f;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + -1.0f;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3.0f: 1.0f: 5.1f] + -2.0f;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3.0f: 2.0f: 7.1f] + -2.0f;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2.0f + [0.0f, 1.0f];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3.0f + [0.0f, 1.0f, 2.0f];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1.0f + [0.0f, 1.0f, 2.0f];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2.0f + [3.0f: 1.0f: 5.1f];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2.0f + [3.0f: 2.0f: 7.1f];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0.0f, 1.0f] + [1.0f, 2.0f];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + [3.0f, 4.0f, 5.0f];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + [-1.0f, 5.0f, -3.0f];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3.0f: 1.0f: 5.1f] + [-3.0f: -1.0f: -5.1f];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3.0f: 2.0f: 7.1f] + [-2.0f, -3.0f, -4.0f];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0.0: 1.0: 99.1] + [99.0: -1.0: -0.1] == 9.900000E+01);') == 'true', &
			eval('all([0.0f: 1.0f: 99.1f] + [99.0f: -1.0f: -0.1f] == 9.900000E+01f);') == 'true', &
			eval('[0, 1] + 2.0f;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0, 1, 2] + 3.0f;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0, 1, 2] + -1.0f;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3: 1: 6] + -2.0f;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3: 2: 8] + -2.0f;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2 + [0.0f, 1.0f];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3 + [0.0f, 1.0f, 2.0f];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1 + [0.0f, 1.0f, 2.0f];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2 + [3.0f: 1.0f: 5.1f];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2 + [3.0f: 2.0f: 7.1f];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0, 1] + [1.0f, 2.0f];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1, 2] + [3.0f, 4.0f, 5.0f];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0, 1, 2] + [-1.0f, 5.0f, -3.0f];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3: 1: 6] + [-3.0f: -1.0f: -5.1f];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3: 2: 8] + [-2.0f, -3.0f, -4.0f];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0: 1: 100] + [99.0f: -1.0f: -0.1f] == 9.900000E+01f);') == 'true', &
			eval('[0.0f, 1.0f] + 2;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + 3;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + -1;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3.0f: 1.0f: 5.1f] + -2;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3.0f: 2.0f: 7.1f] + -2;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2.0f + [0, 1];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3.0f + [0, 1, 2];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1.0f + [0, 1, 2];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2.0f + [3: 1: 6];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2.0f + [3: 2: 8];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0.0f, 1.0f] + [1, 2];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + [3, 4, 5];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0.0f, 1.0f, 2.0f] + [-1, 5, -3];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3.0f: 1.0f: 5.1f] + [-3: -1: -6];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3.0f: 2.0f: 7.1f] + [-2, -3, -4];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0.0f: 1.0f: 99.1f] + [99: -1: -1] == 9.900000E+01f);') == 'true', &
			eval('[i64(0), i64(1)] + i64(2);') == '[2, 3]', &
			eval('i64(2) + [i64(0), i64(1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] + 2;') == '[2, 3]', &
			eval('i64(2) + [0, 1];') == '[2, 3]', &
			eval('[0, 1] + i64(2);') == '[2, 3]', &
			eval('2 + [i64(0), i64(1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] + 2.0f;') == '[2.000000E+00, 3.000000E+00]', &
			eval('i64(2) + [0.0f, 1.0f];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f] + i64(2);') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0f + [i64(0), i64(1)];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] + [i64(1), i64(2)];') == '[1, 3]', &
			eval('[0, 1] + [i64(1), i64(2)];') == '[1, 3]', &
			eval('[i64(0), i64(1)] + [1, 2];') == '[1, 3]', &
			eval('[0.0f, 1.0f] + [i64(1), i64(2)];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] + [1.0f, 2.0f];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1] - [-1, -2];') == '[1, 3]', &
			eval('[0, 1] - -2;') == '[2, 3]', &
			eval('2 - [-0, -1];') == '[2, 3]', &
			eval('[0, 1] - -2.0f;') == '[2.000000E+00, 3.000000E+00]', &
			eval('2 - [-0.0f, -1.0f];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f] - -2;') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0f - [-0, -1];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f] - [-1, -2];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1] - [-1.0f, -2.0f];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - i64(-2);') == '[2, 3]', &
			eval('i64(2) - [i64(-0), i64(-1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] - -2;') == '[2, 3]', &
			eval('i64(2) - [-0, -1];') == '[2, 3]', &
			eval('[0, 1] - i64(-2);') == '[2, 3]', &
			eval('2 - [i64(-0), i64(-1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] - -2.0f;') == '[2.000000E+00, 3.000000E+00]', &
			eval('i64(2) - [-0.0f, -1.0f];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0f, 1.0f] - i64(-2);') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0f - [i64(0), i64(-1)];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - [i64(-1), i64(-2)];') == '[1, 3]', &
			eval('[0, 1] - [i64(-1), i64(-2)];') == '[1, 3]', &
			eval('[i64(0), i64(1)] - [-1, -2];') == '[1, 3]', &
			eval('[0.0f, 1.0f] - [i64(-1), i64(-2)];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - [-1.0f, -2.0f];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[2, 3] * [1, 2];') == '[2, 6]', &
			eval('[2, 3] * 2;') == '[4, 6]', &
			eval('2 * [1, 2];') == '[2, 4]', &
			eval('[1, 2] * 2.0f;') == '[2.000000E+00, 4.000000E+00]', &
			eval('2 * [1.0f, 2.0f];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0f, 2.0f] * 2;') == '[2.000000E+00, 4.000000E+00]', &
			eval('2.0f * [1, 2];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0f, 2.0f] * [1, 2];') == '[1.000000E+00, 4.000000E+00]', &
			eval('[2, 3] * [1.0f, 2.0f];') == '[2.000000E+00, 6.000000E+00]', &
			eval('[i64(2), i64(3)] * i64(2);') == '[4, 6]', &
			eval('i64(2) * [i64(2), i64(3)];') == '[4, 6]', &
			eval('[i64(2), i64(3)] * 2;') == '[4, 6]', &
			eval('i64(2) * [2, 3];') == '[4, 6]', &
			eval('[2, 4] * i64(2);') == '[4, 8]', &
			eval('2 * [i64(3), i64(4)];') == '[6, 8]', &
			eval('[i64(2), i64(3)] * 2.0f;') == '[4.000000E+00, 6.000000E+00]', &
			eval('i64(2) * [2.0f, 3.0f];') == '[4.000000E+00, 6.000000E+00]', &
			eval('[2.0f, 3.0f] * i64(2);') == '[4.000000E+00, 6.000000E+00]', &
			eval('2.0f * [i64(2), i64(3)];') == '[4.000000E+00, 6.000000E+00]', &
			eval('[i64(3), i64(4)] * [i64(1), i64(2)];') == '[3, 8]', &
			eval('[3, 4] * [i64(1), i64(2)];') == '[3, 8]', &
			eval('[i64(3), i64(4)] * [1, 2];') == '[3, 8]', &
			eval('[5.0f, 4.0f] * [i64(1), i64(2)];') == '[5.000000E+00, 8.000000E+00]', &
			eval('[i64(5), i64(4)] * [1.0f, 2.0f];') == '[5.000000E+00, 8.000000E+00]', &
			eval('[2, 3] ** [1, 2];') == '[2, 9]', &
			eval('[2, 3] ** 2;') == '[4, 9]', &
			eval('2 ** [1, 2];') == '[2, 4]', &
			eval('[1, 2] ** 2.0f;') == '[1.000000E+00, 4.000000E+00]', &
			eval('2 ** [1.0f, 2.0f];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0f, 2.0f] ** 2;') == '[1.000000E+00, 4.000000E+00]', &
			eval('2.0f ** [1, 2];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0f, 2.0f] ** [1, 2];') == '[1.000000E+00, 4.000000E+00]', &
			eval('[2, 3] ** [1.0f, 2.0f];') == '[2.000000E+00, 9.000000E+00]', &
			eval('[i64(2), i64(3)] ** i64(2);') == '[4, 9]', &
			eval('i64(2) ** [i64(2), i64(3)];') == '[4, 8]', &
			eval('[i64(2), i64(3)] ** 2;') == '[4, 9]', &
			eval('i64(2) ** [2, 3];') == '[4, 8]', &
			eval('[2, 4] ** i64(2);') == '[4, 16]', &
			eval('2 ** [i64(3), i64(4)];') == '[8, 16]', &
			eval('[i64(2), i64(3)] ** 2.0f;') == '[4.000000E+00, 9.000000E+00]', &
			eval('i64(2) ** [2.0f, 3.0f];') == '[4.000000E+00, 8.000000E+00]', &
			eval('[2.0f, 3.0f] ** i64(2);') == '[4.000000E+00, 9.000000E+00]', &
			eval('2.0f ** [i64(2), i64(3)];') == '[4.000000E+00, 8.000000E+00]', &
			eval('[i64(3), i64(4)] ** [i64(1), i64(2)];') == '[3, 16]', &
			eval('[3, 4] ** [i64(1), i64(2)];') == '[3, 16]', &
			eval('[i64(3), i64(4)] ** [1, 2];') == '[3, 16]', &
			eval('[5.0f, 4.0f] ** [i64(1), i64(2)];') == '[5.000000E+00, 1.600000E+01]', &
			eval('[i64(5), i64(4)] ** [1.0f, 2.0f];') == '[5.000000E+00, 1.600000E+01]', &
			eval('[2, 4] / [1, 2];') == '[2, 2]', &
			eval('[2, 4] / 2;') == '[1, 2]', &
			eval('4 / [1, 2];') == '[4, 2]', &
			eval('[1, 2] / 2.0f;') == '[5.000000E-01, 1.000000E+00]', &
			eval('2 / [1.0f, 2.0f];') == '[2.000000E+00, 1.000000E+00]', &
			eval('[1.0f, 2.0f] / 2;') == '[5.000000E-01, 1.000000E+00]', &
			eval('2.0f / [1, 2];') == '[2.000000E+00, 1.000000E+00]', &
			eval('[1.0f, 2.0f] / [1, 2];') == '[1.000000E+00, 1.000000E+00]', &
			eval('[2, 3] / [1.0f, 2.0f];') == '[2.000000E+00, 1.500000E+00]', &
			eval('[i64(2), i64(4)] / i64(2);') == '[1, 2]', &
			eval('i64(6) / [i64(2), i64(3)];') == '[3, 2]', &
			eval('[i64(2), i64(4)] / 2;') == '[1, 2]', &
			eval('i64(6) / [2, 3];') == '[3, 2]', &
			eval('[2, 4] / i64(2);') == '[1, 2]', &
			eval('2 / [i64(1), i64(2)];') == '[2, 1]', &
			eval('[i64(2), i64(3)] / 2.0f;') == '[1.000000E+00, 1.500000E+00]', &
			eval('i64(3) / [2.0f, 3.0f];') == '[1.500000E+00, 1.000000E+00]', &
			eval('[2.0f, 3.0f] / i64(2);') == '[1.000000E+00, 1.500000E+00]', &
			eval('3.0f / [i64(2), i64(3)];') == '[1.500000E+00, 1.000000E+00]', &
			eval('[i64(3), i64(4)] / [i64(1), i64(2)];') == '[3, 2]', &
			eval('[3, 4] / [i64(1), i64(2)];') == '[3, 2]', &
			eval('[i64(3), i64(4)] / [1, 2];') == '[3, 2]', &
			eval('[5.0f, 4.0f] / [i64(1), i64(2)];') == '[5.000000E+00, 2.000000E+00]', &
			eval('[i64(5), i64(4)] / [1.0f, 2.0f];') == '[5.000000E+00, 2.000000E+00]', &
			eval('[2, 4] % [1, 2];') == '[0, 0]', &
			eval('[2, 4] % 3;') == '[2, 1]', &
			eval('4 % [2, 3];') == '[0, 1]', &
			eval('[1, 2] % 2.0f;') == '[1.000000E+00, 0.000000E+00]', &
			eval('2 % [3.0f, 2.0f];') == '[2.000000E+00, 0.000000E+00]', &
			eval('[4.0f, 5.0f] % 3;') == '[1.000000E+00, 2.000000E+00]', &
			eval('[4.0f, 5.0f] % 3.0f;') == '[1.000000E+00, 2.000000E+00]', &
			eval('i32([4.0f, 5.0f] % 3.0f);') == '[1, 2]', &
			eval('i32([4.0f, 5.0f] % 3.0);') == '[1, 2]', &
			eval('i32([4.0, 5.0] % 3.0f);') == '[1, 2]', &
			eval('i32([3.0f, 4.0f] % [2.0, 3.0]);') == '[1, 1]', &
			eval('i32([3.0, 4.0] % [2.0f, 3.0f]);') == '[1, 1]', &
			eval('i32(3.0 % [2.0f, 3.0f]);') == '[1, 0]', &
			eval('i32(3.0f % [2.0, 3.0]);') == '[1, 0]', &
			eval('2.0f % [2, 3];') == '[0.000000E+00, 2.000000E+00]', &
			eval('[3.0f, 4.0f] % [2, 3];') == '[1.000000E+00, 1.000000E+00]', &
			eval('[2, 3] % [1.0f, 2.0f];') == '[0.000000E+00, 1.000000E+00]', &
			eval('[i64(3), i64(4)] % i64(2);') == '[1, 0]', &
			eval('i64(5) % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[i64(3), i64(4)] % 2;') == '[1, 0]', &
			eval('i64(5) % [2, 3];') == '[1, 2]', &
			eval('[3, 4] % i64(2);') == '[1, 0]', &
			eval('3 % [i64(2), i64(3)];') == '[1, 0]', &
			eval('[i64(2), i64(3)] % 2.0f;') == '[0.000000E+00, 1.000000E+00]', &
			eval('i64(3) % [2.0f, 3.0f];') == '[1.000000E+00, 0.000000E+00]', &
			eval('[2.0f, 3.0f] % i64(2);') == '[0.000000E+00, 1.000000E+00]', &
			eval('3.0f % [i64(2), i64(3)];') == '[1.000000E+00, 0.000000E+00]', &
			eval('[i64(3), i64(5)] % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[3, 5] % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[i64(3), i64(5)] % [2, 3];') == '[1, 2]', &
			eval('[3.0f, 5.0f] % [i64(2), i64(3)];') == '[1.000000E+00, 2.000000E+00]', &
			eval('[i64(3), i64(5)] % [2.0f, 3.0f];') == '[1.000000E+00, 2.000000E+00]', &
			eval('[true, false] and [true, true];') == '[true, false]', &
			eval('[true, false] and true;') == '[true, false]', &
			eval('true and [false, true];') == '[false, true]', &
			eval('[true, false] or [true, false];') == '[true, false]', &
			eval('[true, false] or true;') == '[true, true]', &
			eval('false or [false, true];') == '[false, true]', &
			eval('not [true, false];') == '[false, true]', &
			eval('not [false, true];') == '[true, false]', &
			eval('-[1.0f, 2.0f];') == '[-1.000000E+00, -2.000000E+00]', &
			eval('-[0, 1];') == '[0, -1]', &
			eval('-[i64(0), i64(1)];') == '[0, -1]', &
			eval('[0.0f, 1.0f];') == '[0.000000E+00, 1.000000E+00]', &
			eval('[0, 1];') == '[0, 1]', &
			eval('[i64(0), i64(1)];') == '[0, 1]', &
			.false. &
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_arr_op

!===============================================================================

subroutine unit_test_rhs_slc_1(npass, nfail)

	! Simple array slicing tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'rhs array slicing'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('let v = [0: 10]; v[0: 4];', quiet) == '[0, 1, 2, 3]', &
			eval('let v = [0: 10]; v[2: 5];', quiet) == '[2, 3, 4]', &
			eval('let v = [0: 10]; let u = v[2: 5]; u[0];', quiet) == '2', &
			eval('let v = [0: 10]; let u = v[2: 5]; u;', quiet) == '[2, 3, 4]', &
			eval('let v = [0: 10]; let u = v[0:2:10]; u;', quiet) == '[0, 2, 4, 6, 8]', &
			eval('let v = [0: 10]; let u = v[1:2:10]; u;', quiet) == '[1, 3, 5, 7, 9]', &
			eval('let v = [0: 10]; let u = v[2:2:10]; u;', quiet) == '[2, 4, 6, 8]', &
			eval('let v = [0: 10]; let u = v[0:3:10]; u;', quiet) == '[0, 3, 6, 9]', &
			eval('let v = [0: 10]; let u = v[1:3:10]; u;', quiet) == '[1, 4, 7]', &
			eval('let v = [0: 10]; let u = v[2:3:10]; u;', quiet) == '[2, 5, 8]', &
			eval('let v = [0: 10]; let u = v[3:3:10]; u;', quiet) == '[3, 6, 9]', &
			eval('let v = [0: 10]; let u = v[4:3:10]; u;', quiet) == '[4, 7]', &
			eval('let v = [0: 5]; let u = v[4: -1: -1]; u;', quiet) == '[4, 3, 2, 1, 0]', &
			eval('let v = [0: 10]; let u = v[9:-2:-1]; u;', quiet) == '[9, 7, 5, 3, 1]', &
			eval('let v = [0: 10]; let u = v[8:-2:-1]; u;', quiet) == '[8, 6, 4, 2, 0]', &
			eval('let v = [0: 10]; let u = v[9:-3:-1]; u;', quiet) == '[9, 6, 3, 0]', &
			eval('let v = [0: 10]; let u = v[8:-3:-1]; u;', quiet) == '[8, 5, 2]', &
			eval('let v = [0: 10]; let u = v[7:-3:-1]; u;', quiet) == '[7, 4, 1]', &
			eval('let v = [0: 10]; let u = v[6:-3:-1]; u;', quiet) == '[6, 3, 0]', &
			eval('let v = [10: 20]; let u = v[[0, 1, 3, 6]]; u;', quiet) == '[10, 11, 13, 16]', &
			eval('let v = [10: 20]; let u = v[[0, 8, 4, 2]]; u;', quiet) == '[10, 18, 14, 12]', &
			eval('let v = [10: 20]; let u = v[[0, 0, 4, 2]]; u;', quiet) == '[10, 10, 14, 12]', &
			eval('let v = [10: 20]; let u = v[[0, 8, 4, 4]]; u;', quiet) == '[10, 18, 14, 14]', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; size(n,0);', quiet) == '3', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; size(n,1);', quiet) == '2', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; sum(n);', quiet) == '20', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[0,0];', quiet) == '0', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[1,0];', quiet) == '1', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[2,0];', quiet) == '3', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[0,1];', quiet) == '4', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[1,1];', quiet) == '5', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2]; n[2,1];', quiet) == '7', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], 0:2:3]; sum(n);', quiet) == '32', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,1,3], [0,2]]; sum(n);', quiet) == '32', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11; 4,3]; let n = m[[0,2,3], [0,2]]; sum(n);', quiet) == '34', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11;  3,4]; let n = m[0:2, [0,1,3]]; sum(n);', quiet) == '27', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11;  3,4]; let n = m[0:2, [0,3,1]]; sum(n);', quiet) == '27', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11;  3,4]; let n = m[0:2, [0,2,1]]; sum(n);', quiet) == '21', &
			eval('let m = [0,1,2,3,4,5,6,7,8,9,10,11;  3,4]; let n = m[0:2, [2,2,0]]; sum(n);', quiet) == '27', &
			eval('let m = [0, 1, 2, 3; 2, 2]; m[0, 0:2];', quiet) == '[0, 2]', &
			eval('let m = [0, 1, 2, 3; 2, 2]; m[1, 0:2];', quiet) == '[1, 3]', &
			eval('let m = [0, 1, 2, 3; 2, 2]; m[0:2, 0];', quiet) == '[0, 1]', &
			eval('let m = [0, 1, 2, 3; 2, 2]; m[0:2, 1];', quiet) == '[2, 3]', &
			eval('let m = [0, 1, 2, 3, 4, 5; 2, 3]; m[0, 0:3];', quiet) == '[0, 2, 4]', &
			eval('let m = [0, 1, 2, 3, 4, 5; 2, 3]; m[1, 0:3];', quiet) == '[1, 3, 5]', &
			eval('let m = [0, 1, 2, 3, 4, 5; 2, 3]; m[0:2, 1];', quiet) == '[2, 3]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0, 0:3];', quiet) == '[0, 3, 6]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[2, 0:3];', quiet) == '[2, 5, 8]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0:3, 0];', quiet) == '[0, 1, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0:3, 2];', quiet) == '[6, 7, 8]', &
			eval('let v = ["a", "b", "c", "d", "e", "f"]; v[0: 4];', quiet) == '[a, b, c, d]', &
			eval('let v = ["a", "b", "c", "d", "e", "f"]; v[1: 5];', quiet) == '[b, c, d, e]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[0:2, 0, 0];', quiet) == '[0, 1]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[0, 0:2, 0];', quiet) == '[0, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[0, 0, 0:2];', quiet) == '[0, 4]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[0:2, 1, 1];', quiet) == '[6, 7]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[1, 0:2, 1];', quiet) == '[5, 7]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[1, 1, 0:2];', quiet) == '[3, 7]', &
			eval('let m = ["a", "b", "c", "d", "e", "f"; 3, 2]; m[0:3, 0];', quiet) == '[a, b, c]', &
			eval('let m = ["a", "b", "c", "d", "e", "f"; 3, 2]; m[0:3, 1];', quiet) == '[d, e, f]', &
			eval('let m = ["a", "b", "c", "d", "e", "f"; 3, 2]; m[0, 0:2];', quiet) == '[a, d]', &
			eval('let m = ["a", "b", "c", "d", "e", "f"; 3, 2]; m[2, 0:2];', quiet) == '[c, f]', &
			eval('let m = [true; 2, 2]; m[0, 0:2];', quiet) == '[true, true]', &
			eval('let m = [i64(42); 2, 2]; m[0:2, 1];', quiet) == '[42, 42]', &
			eval('let m = [1.0f; 2, 2]; m[0:2, 1];', quiet) == '[1.000000E+00, 1.000000E+00]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0, :];', quiet) == '[0, 3, 6]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[2, :];', quiet) == '[2, 5, 8]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[:, 0];', quiet) == '[0, 1, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[:, 2];', quiet) == '[6, 7, 8]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[0:2, 0:2]);', quiet) == '8', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[0:3, 0:2]);', quiet) == '15', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[0:2, 0:3]);', quiet) == '21', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[1:3, 1:3]);', quiet) == '24', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[0:3, 1:3]);', quiet) == '33', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; sum(m[1:3, 0:3]);', quiet) == '27', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11; 3, 4]; sum(m[1:3, 0:3]);', quiet) == '27', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11; 3, 4]; sum(m[1:3, 0:4]);', quiet) == '48', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; sum(m[:,:,0]);', quiet) == '6', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; sum(m[:,:,1]);', quiet) == '22', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; sum(m[:,:,:]);', quiet) == '28', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; sum(m[0,:,:]);', quiet) == '12', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; sum(m[:,0,:]);', quiet) == '10', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	!     [
	!     0, 1,
	!     2, 3,
	!
	!     4, 5,
	!     6, 7
	!     ]

	!     0,  1,  2,
	!     3,  4,  5,
	!     6,  7,  8,
	!     9, 10, 11,

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_rhs_slc_1

!===============================================================================

subroutine unit_test_lhs_slc_1(npass, nfail)

	! Simple array slicing tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'lhs array slicing'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: test i64, bool, and str lhs slicing

	tests = &
		[   &
			eval('let v = [0: 5]; v[1: 4] = [3:-1:0]; v;', quiet) == '[0, 3, 2, 1, 4]', &
			eval('let v = [0: 5]; v[1: 4] += [10; 3]; v;', quiet) == '[0, 11, 12, 13, 4]', &
			eval('let v = [0: 5]; v[1: 4] += [10; 3]; sum(v);', quiet) == '40', &
			eval('let v = [0: 5]; v[1: 4] += 10; v;', quiet) == '[0, 11, 12, 13, 4]', &
			eval('let v = [0: 5]; v[1: 4] = 10; v;', quiet) == '[0, 10, 10, 10, 4]', &
			eval('let v = [0.0f: 4.0f; 5]; v[1: 4]  = 10.0f; [sum(v)];', quiet) == '[3.400000E+01]', &
			eval('let v = [0.0f: 4.0f; 5]; v[1: 4] += 10.0f; [sum(v)];', quiet) == '[4.000000E+01]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0,:] += 1; m[0,:];', quiet) == '[1, 4, 7]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0,:] += [1,2,3]; m[0,:];', quiet) == '[1, 5, 9]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0,:] += 1; m[:,0];', quiet) == '[1, 1, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[1,:] += 1; m[1,:];', quiet) == '[2, 5, 8]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11; 3, 4]; m[2,:] += 1; m[2,:];', quiet) == '[3, 6, 9, 12]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11; 3, 4]; m[:,2] += 1; m[:,2];', quiet) == '[7, 8, 9]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[:,:,0] += 1; m[:,0,0];', quiet) == '[1, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[:,:,0] += 1; m[:,1,0];', quiet) == '[3, 4]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7; 2, 2, 2]; m[:,:,0] += 1; m[0,0,:];', quiet) == '[1, 4]', &

			!! This used to behave differently wrt the walrus operator
			!eval('let v = [0: 5]; v[1: 4] += [10; 3];', quiet) == '[0, 11, 12, 13, 4]', &
			!eval('let v = [0: 5]; v[1: 4] += [10; 3];', quiet) == '[10, 10, 10]', &
			eval('let v = [0: 5]; v[1: 4] += [10; 3];', quiet) == '[11, 12, 13]', &  ! this option makes the most sense
			eval('let v = [0: 5]; return (v[1: 4] += [10; 3]);', quiet) == '[11, 12, 13]', &  ! this option makes the most sense

			eval('let v = [0: 4]; v[0:2:4]   = 9; v;', quiet) == '[9, 1, 9, 3]', &
			eval('let v = [0: 4]; v[1:2:4]   = 9; v;', quiet) == '[0, 9, 2, 9]', &
			eval('let v = [0: 4]; v[3:-1:1]  = 9; v;', quiet) == '[0, 1, 9, 9]', &
			eval('let v = [0: 4]; v[3:-2:-1] = 9; v;', quiet) == '[0, 9, 2, 9]', &
			eval('let v = [0: 4]; v[2:-2:-1] = 9; v;', quiet) == '[9, 1, 9, 3]', &
			eval('let v = [0: 4]; v[[0,1]]   = 9; v;', quiet) == '[9, 9, 2, 3]', &
			eval('let v = [0: 4]; v[[0,3]]   = 9; v;', quiet) == '[9, 1, 2, 9]', &
			eval('let v = [0: 4]; v[[1,3]]   = 9; v;', quiet) == '[0, 9, 2, 9]', &
			eval('let v = [0: 4]; v[[0,1,3]]   = 9; v;', quiet) == '[9, 9, 2, 9]', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	!     [
	!     0, 1,
	!     2, 3,
	!
	!     4, 5,
	!     6, 7
	!     ]
	!
	!     0,  1,  2,
	!     3,  4,  5,
	!     6,  7,  8,
	!     9, 10, 11,

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_lhs_slc_1

!===============================================================================

subroutine unit_test_array_f32_1(npass, nfail)

	! Simple f32 array tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 arrays'

	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! Because test evaluation results are tested by comparing strings, output
	! white space is significant!  Ints are formatted in min width, and array
	! elements are separated by a comma and a single space

	! TODO: how portable is this string comparison with f32 rounding?  It will
	! at least be a pain to update if I change number of sig figs or something

	tests = &
		[   &
			eval('[42.0f];') == '[4.200000E+01]',  &
			eval('[-42.f,1337.f];') == '[-4.200000E+01, 1.337000E+03]', &
			eval('[3.f, 2.f, 1.f];') == '[3.000000E+00, 2.000000E+00, 1.000000E+00]', &
			eval('[3.f: 1.f; 3];') == '[3.000000E+00, 2.000000E+00, 1.000000E+00]', &
			eval('[3.f: 1.f; 5];') == '[3.000000E+00, 2.500000E+00, 2.000000E+00, 1.500000E+00, 1.000000E+00]', &
			eval('[1.f: 3.f; 3];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[1.f: 3.f; 5];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00]', &
			eval('[1.f: 0.5f: 2.9f];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00]', &
			eval('[1.f: 0.5f: 3.4f];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00]', &
			eval('[1.f: 0.5f: 3.9f];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00, 3.500000E+00]', &
			eval('[4.f: 0.5f: 5.4f];') == '[4.000000E+00, 4.500000E+00, 5.000000E+00]', &
			eval('[5.4f: -0.5f: 4.0f];') == '[5.400000E+00, 4.900000E+00, 4.400000E+00]', &
			eval('[2.f-3.f: 1.1f: 6.f/3.f + 3.f];') &
				== '[-1.000000E+00, 1.000000E-01, 1.200000E+00, 2.300000E+00, 3.400000E+00, 4.500000E+00]', &
			eval('[42.0f; 3];') == '[4.200000E+01, 4.200000E+01, 4.200000E+01]',  &
			eval('[42.0f; 4];') == '[4.200000E+01, 4.200000E+01, 4.200000E+01, 4.200000E+01]',  &
			eval('let myArray = [2.f-3.f: 1.1f: 6.f/3.f + 3.f];') &
				== '[-1.000000E+00, 1.000000E-01, 1.200000E+00, 2.300000E+00, 3.400000E+00, 4.500000E+00]', &
			eval('[48.f-6.f, 13.f*100.f + 37.f];') == '[4.200000E+01, 1.337000E+03]'  &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_f32_1

!===============================================================================

subroutine unit_test_array_i32_2(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'i32 array scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/array-i32/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '0', &
			interpret_file(path//'test-02.syntran', quiet) == '0', &
			interpret_file(path//'test-03.syntran', quiet) == '13', &
			interpret_file(path//'test-04.syntran', quiet) == '-3', &
			interpret_file(path//'test-05.syntran', quiet) == '16', &
			interpret_file(path//'test-06.syntran', quiet) == '16', &
			interpret_file(path//'test-07.syntran', quiet) == '13', &
			interpret_file(path//'test-08.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_i32_2

!===============================================================================

subroutine unit_test_nd_i32(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'rank-2+ i32 arrays'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/nd-i32/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_nd_i32

!===============================================================================

subroutine unit_test_fns(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'user-defined functions'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/fns/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '3', &
			interpret_file(path//'test-02.syntran', quiet) == '2', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == '7', &
			interpret_file(path//'test-05.syntran', quiet) == '10', &
			interpret_file(path//'test-06.syntran', quiet) == '0', &
			interpret_file(path//'test-07.syntran', quiet) == '0', &
			interpret_file(path//'test-08.syntran', quiet) == 'true', &
			interpret_file(path//'test-09.syntran', quiet) == '15', &
			interpret_file(path//'test-10.syntran', quiet) == '1.500000E+01', &
			interpret_file(path//'test-11.syntran', quiet) == '300', &
			interpret_file(path//'test-12.syntran', quiet) == '[4, 4, 4]', &
			interpret_file(path//'test-13.syntran', quiet) == '[2, 3, 4]', &
			interpret_file(path//'test-14.syntran', quiet) == '[4.000000E+00, 4.000000E+00, 4.000000E+00]', &
			interpret_file(path//'test-15.syntran', quiet) == '1.300000E+01', &
			interpret_file(path//'test-16.syntran', quiet) == '0.000000E+00', &
			interpret_file(path//'test-17.syntran', quiet) == '0', &
			interpret_file(path//'test-18.syntran', quiet) == '0', &
			interpret_file(path//'test-19.syntran', quiet) == '0', &
			interpret_file(path//'test-20.syntran', quiet) == '0', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_fns

!===============================================================================

subroutine unit_test_linalg_fns(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'user-defined linear algebra functions'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/linear-algebra/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '0.000000E+00', &
			interpret_file(path//'test-02.syntran', quiet) == '[-3.844404E+01, 2.821099E+01, -9.842391E+01]', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == 'true', &
			interpret_file(path//'test-05.syntran', quiet) == '[1.000000E+00, -3.000000E+00, 2.000000E+00]', &
			interpret_file(path//'test-06.syntran', quiet) == '[-1.000000E+00, -2.000000E+00, 3.000000E+00]', &
			interpret_file(path//'test-07.syntran', quiet) == '[-1.000000E+00, -2.000000E+00, 3.000000E+00]', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_linalg_fns

!===============================================================================

subroutine unit_test_array_f32_2(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 array scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/array-f32/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '1.666667E+00', &
			interpret_file(path//'test-02.syntran', quiet) == '1.000000E+00', &
			interpret_file(path//'test-03.syntran', quiet) == '1.300000E+01', &
			interpret_file(path//'test-04.syntran', quiet) == '-3.000000E+00', &
			interpret_file(path//'test-05.syntran', quiet) == '1.600000E+01', &
			interpret_file(path//'test-06.syntran', quiet) == '1.600000E+01', &
			interpret_file(path//'test-07.syntran', quiet) == '1.150000E+01', &
			interpret_file(path//'test-08.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_f32_2

!===============================================================================

subroutine unit_test_array_str(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'str array scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/array-str/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('let sv = ["hello"; 3];', quiet) == '[hello, hello, hello]', &
			interpret_file(path//'test-01.syntran', quiet) == 'hello world', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_str

!===============================================================================

subroutine unit_test_io(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'io scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/io/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'line test', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == 'line final', &
			interpret_file(path//'test-05.syntran', quiet) == 'true', &
			interpret_file(path//'test-06.syntran', quiet) == 'true', &
			interpret_file(path//'test-07.syntran', quiet) == '1337', &
			interpret_file(path//'test-08.syntran', quiet) == 'true', &
			interpret_file(path//'test-09.syntran', quiet) == '[1337, 42, 16384]', &
			interpret_file(path//'test-10.syntran', quiet) == '0', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_io

!===============================================================================

subroutine unit_test_include(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = '#include scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/include/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '3', &
			interpret_file(path//'test-02.syntran', quiet) == '0', &
			interpret_file(path//'test-03.syntran', quiet) == '7', &
			interpret_file(path//'test-04.syntran', quiet) == '7', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_include

!===============================================================================

subroutine unit_test_control(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'control flow'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/control/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == 'true', &
			interpret_file(path//'test-05.syntran', quiet) == 'true', &
			interpret_file(path//'test-06.syntran', quiet) == 'true', &
			interpret_file(path//'test-07.syntran', quiet) == 'true', &
			interpret_file(path//'test-08.syntran', quiet) == 'true', &
			interpret_file(path//'test-09.syntran', quiet) == 'true', &
			interpret_file(path//'test-10.syntran', quiet) == 'true', &
			interpret_file(path//'test-11.syntran', quiet) == 'true', &
			interpret_file(path//'test-12.syntran', quiet) == 'true', &
			interpret_file(path//'test-13.syntran', quiet) == 'true', &
			interpret_file(path//'test-14.syntran', quiet) == 'true', &
			interpret_file(path//'test-15.syntran', quiet) == 'true', &
			interpret_file(path//'test-16.syntran', quiet) == 'true', &
			interpret_file(path//'test-17.syntran', quiet) == 'true', &
			interpret_file(path//'test-18.syntran', quiet) == 'true', &
			interpret_file(path//'test-19.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_control

!===============================================================================

subroutine unit_test_struct(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'structs'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests1(:), tests2(:), tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests1 = &
		[   &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 1
				//'let d = D{y=i64(1912), m="Apr", d=14};' &
				//'d.y;', quiet) == '1912', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 2
				//'let d = D{y=i64(1912), m="Apr", d=14};' &
				//'d.m;', quiet) == 'Apr', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 3
				//'let d = D{y=i64(1912), m="Apr", d=14};' &
				//'d.d;', quiet) == '14', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 4
				//'let d = D{y=i64(1900)+12, m="Apr", d=14};' &
				//'d.y;', quiet) == '1912', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 5
				//'let d = D{y=i64(1912), m="Apr", d=7*2};' &
				//'let e=d; e.d;', quiet) == '14', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 6
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'let e=d;' &
			    //'e = D{y=i64(1945), m="May", d=5*3};' &
			    //'e.d;', quiet) == '15', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 7
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'let e=d;' &
			    //'e = D{y=i64(1945), m="May", d=5*3};' &
			    //'d.d;', quiet) == '14', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 8
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'let x = 42;' &
			    //'x = d.d;' &
			    //'x;', quiet) == '14', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 9
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'let x = d.y + 5;' &
			    //'x;', quiet) == '1917', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 10
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'let x = 6 + d.y;' &
			    //'x;', quiet) == '1918', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 11
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'d.d = 18;' &
			    //'d.d;', quiet) == '18', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 12
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'d.d += 3;' &
			    //'d.d;', quiet) == '17', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 13
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'d.m + "il";', quiet) == 'April', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 14
			    //'let d = D{y=i64(1912), m="Apr", d=7*2};' &
			    //'"month " + d.m;', quiet) == 'month Apr', &
			eval( 'struct C{r:i32, g:i32, b:i32}' &           ! 15
			    //'let c = C{r = 32, g = 64, b = 128};' &
			    //'max(c.r, c.g);', quiet) == '64', &
			eval( 'struct C{r:i32, g:i32, b:i32}' &           ! 16
			    //'let c = C{r = 32, g = 64, b = 128};' &
			    //'c.r + c.g + c.b;', quiet) == '224', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 17
			    //'fn x_year(dd: D): i64 {return dd.y;}' &
			    //'let d = D{y=i64(1884), m="Apr", d=7*2};' &
			    //'x_year(d);', quiet) == '1884', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 18
			    //'fn x_year(dd: D): i64 {return dd.y;}' &
			    //'let d = D{y=i64(1776), m="Jul", d=3+1};' &
			    //'x_year(d);', quiet) == '1776', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 19
			    //'fn x_day(dd: D): i32 {return dd.d;}' &
			    //'let d = D{y=i64(1884), m="Apr", d=7*2};' &
			    //'x_day(d);', quiet) == '14', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 20
			    //'fn x_day(dd: D): i32 {return dd.d;}' &
			    //'let d = D{y=i64(1776), m="Jul", d=3+1};' &
			    //'x_day(d);', quiet) == '4', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 21
			    //'fn x_mon(dd: D): str {return dd.m;}' &
			    //'let d = D{y=i64(1884), m="Apr", d=7*2};' &
			    //'x_mon(d);', quiet) == 'Apr', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 22
			    //'fn x_mon(dd: D): str {return dd.m;}' &
			    //'let d = D{y=i64(1776), m="Jul", d=3+1};' &
			    //'x_mon(d);', quiet) == 'Jul', &
			eval( 'struct D{y:i64, m:str, d:i32}' &           ! 23
			    //'let glbl = D{y=i64(1883), m="Apr", d=7*2};' &
			    //'fn x_year(): i64 {return glbl.y;}' &
			    //'x_year();', quiet) == '1883', &
			eval( 'struct P{x:i32, y:i32,}' &                 ! 24
			    //'let p1 = P{x=6, y=13};' &
			    //'let p2 = P{x=5, y=p1.y};' &
			    //'p2.x;', quiet) == '5', &
			eval( 'struct P{x:i32, y:i32,}' &                 ! 25
			    //'let p1 = P{x=6, y=13};' &
			    //'let p2 = P{x=5, y=p1.y};' &
			    //'p1.x;', quiet) == '6', &
			eval( 'struct P{x:i32, y:i32,}' &                 ! 26
			    //'let p1 = P{x=6, y=13};' &
			    //'let p2 = P{x=5, y=p1.y};' &
			    //'p2.y;', quiet) == '13', &
			eval( 'struct P{x:i32, y:i32,}' &                 ! 27
			    //'let p1 = P{x=6, y=13};' &
			    //'let p2 = P{x=5, y=p1.y};' &
			    //'p1.y;', quiet) == '13', &
			eval(''                         &                 ! 28
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let po = r1.bl;' &
				//'let yo = po.x;' &
				, quiet) == '6', &
			eval(''                         &                 ! 29
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let po = r1.bl;' &
				//'let yo = po.y;' &
				, quiet) == '13', &
			eval(''                         &                 ! 30
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let po = r1.tr;' &
				//'let yo = po.x;' &
				, quiet) == '9', &
			eval(''                         &                 ! 31
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let po = r1.tr;' &
				//'let yo = po.y;' &
				, quiet) == '17', &
			.false. &
		]
	tests2 = &
		[   &

			eval(''                         &                 ! 32
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'return v1.name;' &
				, quiet) == 'myvec1', &
			eval(''                         &                 ! 33
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'return v1.v;' &
				, quiet) == '[6, 2, 5]', &
			eval(''                         &                 ! 34
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'v1.v = [3, 1, 2];' &
				//'return v1.v;' &
				, quiet) == '[3, 1, 2]', &
			eval(''                         &                 ! 35
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'let v2 = v1.v;' &
				//'return v2;' &
				, quiet) == '[6, 2, 5]', &
			eval(''                         &                 ! 36
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'let v2 = v1.v;' &
				//'return v2[0];' &
				, quiet) == '6', &
			eval(''                         &                 ! 37
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'let v2 = v1.v;' &
				//'return v2[1];' &
				, quiet) == '2', &
			eval(''                         &                 ! 38
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'let v2 = [3, 1, 2];' &
				//'v1.v = v2;' &
				//'return v1.v;' &
				, quiet) == '[3, 1, 2]', &
			eval(''                         &                 ! 39
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let o = r1.bl.x;' &
				, quiet) == '6', &
			eval(''                         &                 ! 40
				//'struct P{x:i32, y:i32,}' &
				//'struct R{bl: P, tr: P}' &
				//'let p1 = P{x=6, y=13};' &
				//'let p2 = P{x=9, y=17};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let o = r1.tr.y;' &
				, quiet) == '17', &
			eval(''                         &                 ! 41
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'let o = y1.ba.tr.x;' &
				, quiet) == '9', &
			eval(''                         &                 ! 42
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'let o = y1.ba.bl.y;' &
				, quiet) == '13', &
			eval(''                         &                 ! 43
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'let o = y1.p.z;' &
				, quiet) == '5', &
			eval(''                         &                 ! 44
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'y1.ba.tr.x = 10;' &
				//'return y1.ba.tr.x;' &
				, quiet) == '10', &
			eval(''                         &                 ! 45
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'y1.ba.bl.y += 1;' &
				//'return y1.ba.bl.y;' &
				, quiet) == '14', &
			eval(''                         &                 ! 46
				//'struct P{x:i32, y:i32, z:i32}' &  ! point
				//'struct R{bl: P, tr: P}' &         ! rectangle with bottom-left and top-right points
				//'struct Y{ba: R, p: P}' &          ! pyramid with base rect and a point
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'y1.p.z -= y1.ba.tr.x;' &
				//'return y1.p.z;' &
				, quiet) == '-4', &
			eval(''                         &                 ! 47
				//'struct P{z:i32, x:i32, y:i32}' &  ! point with members in weird order
				//'struct R{bl: P, tr: P}' &
				//'struct Y{ba: R, p: P}' &
				//'let p1 = P{x=6, y=13, z=0};' &
				//'let p2 = P{x=9, y=17, z=0};' &
				//'let p3 = P{x=7, y=15, z=5};' &
				//'let r1 = R{bl=p1, tr=p2};' &
				//'let y1 = Y{ba=r1,  p=p3};' &
				//'y1.p.z -= y1.ba.tr.x;' &
				//'return y1.p.z;' &
				, quiet) == '-4', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 48
			    //'fn get_d(): D {return D{y=2024, m="Sep", d=21};}' &
			    //'let d0 = get_d();' &
			    //'return d0.m;' &
				, quiet) == 'Sep', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests1 = tests1(1: size(tests1) - 1)
	tests2 = tests2(1: size(tests2) - 1)
	!print *, "number of "//label//" tests = ", size(tests)

	! Cat sub arrays
	tests = [tests1, tests2]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct

!===============================================================================

subroutine unit_test_struct_arr1(npass, nfail)

	! TODO: this is split into multiple parts to avoid compiler warnings about
	! >255 continuation lines (which both gfort and ifort have no real issue
	! with).  For logging, it would be cleaner to split the array but not the
	! subroutine.  Concatenate the bool arrays before calling unit_test_coda()
	! all within one subroutine, as in unit_test_struct()

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	! This tests both structs of arrays and arrays of structs.  There are
	! limited structs of arrays covered in unit_test_struct(), but more complex
	! cases are covered here
	character(len = *), parameter :: label = 'structs/arrays part 1'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval(''                         &                 ! 1
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'return v1.v[0];' &
				, quiet) == '6', &
			eval(''                         &                 ! 2
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'return v1.v[1];' &
				, quiet) == '2', &
			eval(''                         &                 ! 3
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'return v1.v[1] + 1;' &
				, quiet) == '3', &
			eval(''                         &                 ! 4
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'v1.v = [3, 1, 2];' &
				//'return v1.v[0] + v1.v[1];' &
				, quiet) == '4', &
			eval(''                         &                 ! 5
				//'struct V{v:[i32;:], name:str,}' &
				//'let v1 = V{v=[6,2,5], name="myvec1"};' &
				//'let x = v1.v[1] + [3, 2, 1];' &
				, quiet) == '[5, 4, 3]', &
			eval(''                         &                 ! 6
				//'struct V{v:[i32;:], name:str,}' &  ! vector
				//'struct L{s:V, e:V}'             &  ! line with start and end vectors
				//'let v1 = V{v=[3,2,1], name="myvec1"};' &
				//'let v2 = V{v=[6,2,5], name="myvec2"};' &
				//'let l1 = L{s=v1, e=v2};' &
				//'return l1.s.v[0];' &
				, quiet) == '3', &
			eval(''                         &                 ! 7
				//'struct V{v:[i32;:], name:str,}' &  ! vector
				//'struct L{s:V, e:V}'             &  ! line with start and end vectors
				//'let v1 = V{v=[3,2,1], name="myvec1"};' &
				//'let v2 = V{v=[6,2,5], name="myvec2"};' &
				//'let l1 = L{s=v1, e=v2};' &
				//'return l1.e.v[1];' &
				, quiet) == '2', &
			eval(''                         &                 ! 8
				//'struct V{v:[i32;:], name:str,}' &  ! vector
				//'struct L{s:V, e:V}'             &  ! line with start and end vectors
				//'let v1 = V{v=[3,2,1], name="myvec1"};' &
				//'let v2 = V{v=[6,2,5], name="myvec2"};' &
				//'let l1 = L{s=v1, e=v2};' &
				//'return l1.e.name;' &
				, quiet) == 'myvec2', &
			eval(''                         &                 ! 9
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let ps = [p1; 2];' &
				//'let po = ps[0];' &
				//'return po.x;' &
				, quiet) == '6', &
			eval(''                         &                 ! 10
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let ps = [p1; 2];' &        ! ps is an array of 2 copies of p1
				//'let po = ps[1];' &
				//'return po.y;' &
				, quiet) == '13', &
			eval(''                         &                 ! 11
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let ps = [p1; 2];' &        ! ps is an array of 2 copies of p1
				//'return ps[0].x;' &
				, quiet) == '6', &
			eval(''                         &                 ! 12
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let ps = [p1; 2];' &        ! ps is an array of 2 copies of p1
				//'return ps[1].y;' &
				, quiet) == '13', &
			eval(''                         &                 ! 13
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let p2 = P{x=4, y=15,};' &
				//'let ps = [p1, p2];' &
				//'return ps[0].x;' &
				, quiet) == '6', &
			eval(''                         &                 ! 14
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let p2 = P{x=4, y=15,};' &
				//'let ps = [p1, p2];' &
				//'return ps[1].y;' &
				, quiet) == '15', &
			eval(''                         &                 ! 15
				//'struct P{y:i32, x:i32,}' &  ! point in weird order
				//'let p1 = P{x=6, y=13,};' &
				//'let p2 = P{x=4, y=15,};' &
				//'let ps = [p1, p2];' &
				//'return ps[1].y;' &
				, quiet) == '15', &
			eval(''                         &                 ! 16
				//'struct P{x:i32, y:i32,}' &  ! point
				//'struct L{s:P, e:P}'      &  ! line
				//'let p1 = P{x= 6, y=13,};' &
				//'let p2 = P{x= 4, y=15,};' &
				//'let p3 = P{x=16, y=18,};' &
				//'let p4 = P{x=14, y=20,};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'return ls[0].e.x;' &
				, quiet) == '4', &
			eval(''                         &                 ! 17
				//'struct P{x:i32, y:i32,}' &  ! point
				//'struct L{s:P, e:P}'      &  ! line
				//'let p1 = P{x= 6, y=13,};' &
				//'let p2 = P{x= 4, y=15,};' &
				//'let p3 = P{x=16, y=18,};' &
				//'let p4 = P{x=14, y=20,};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'return ls[1].s.y;' &
				, quiet) == '18', &
			eval(''                         &                 ! 18
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'let p1 = P{v=[6, 13], s="pt1"};' &
				//'let p2 = P{v=[4, 15], s="pt2"};' &
				//'let ps = [p1, p2];' &
				//'return ps[0].v[1];' &
				, quiet) == '13', &
			eval(''                         &                 ! 19
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'let p1 = P{v=[6, 13], s="pt1"};' &
				//'let p2 = P{v=[4, 15], s="pt2"};' &
				//'let ps = [p1, p2];' &
				//'return ps[1].v[0];' &
				, quiet) == '4', &
			eval(''                         &                 ! 20
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'let p1 = P{v=[6, 13], s="pt1"};' &
				//'let p2 = P{v=[4, 15], s="pt2"};' &
				//'let ps = [p1, p2];' &
				//'return ps[1].s;' &
				, quiet) == 'pt2', &
			eval(''                         &                 ! 21
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'struct L{s:P, e:P}'      &      ! line
				//'let p1 = P{v=[ 6, 13], s="pt1"};' &
				//'let p2 = P{v=[ 4, 15], s="pt2"};' &
				//'let p3 = P{v=[16, 18], s="pt3"};' &
				//'let p4 = P{v=[14, 20], s="pt4"};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'return ls[1].s.v[1];' &
				, quiet) == '18', &
			eval(''                         &                 ! 22
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'let p1 = P{v=[6, 13], s="pt1"};' &
				//'let p2 = P{v=[4, 15], s="pt2"};' &
				//'let ps = [p1, p2];' &
				//'ps[1].v[0] = 3;' &     ! dot/subscript expr on lhs
				//'return ps[1].v[0];' &
				, quiet) == '3', &
			eval(''                         &                 ! 23
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'let p1 = P{v=[6, 13], s="pt1"};' &
				//'let p2 = P{v=[4, 15], s="pt2"};' &
				//'let ps = [p1, p2];' &
				//'ps[1].v[0] -= 1;' &
				//'return ps[1].v[0];' &
				, quiet) == '3', &
			eval(''                         &                 ! 24
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'struct L{s:P, e:P}'      &      ! line
				//'let p1 = P{v=[ 6, 13], s="pt1"};' &
				//'let p2 = P{v=[ 4, 15], s="pt2"};' &
				//'let p3 = P{v=[16, 18], s="pt3"};' &
				//'let p4 = P{v=[14, 20], s="pt4"};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'ls[1].s.v[1] = 19;' &
				//'return ls[1].s.v[1];' &
				, quiet) == '19', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! I developed LHS dot exprs almost all at once, so I might have missed some
	! cases. For RHS dot exprs I did it piece-by-piece and added tests as I went

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)
	!print *, "number of "//label//" tests = ", size(tests)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct_arr1
subroutine unit_test_struct_arr2(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	! This tests both structs of arrays and arrays of structs.  There are
	! limited structs of arrays covered in unit_test_struct(), but more complex
	! cases are covered here
	character(len = *), parameter :: label = 'structs/arrays part 2'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval(''                         &                 ! 25
				//'struct P{v:[i32;:], s:str,}' &  ! point
				//'struct L{s:P, e:P}'      &      ! line
				//'let p1 = P{v=[ 6, 13], s="pt1"};' &
				//'let p2 = P{v=[ 4, 15], s="pt2"};' &
				//'let p3 = P{v=[16, 18], s="pt3"};' &
				//'let p4 = P{v=[14, 20], s="pt4"};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'ls[1].s.v[1] += 1;' &
				//'return ls[1].s.v[1];' &
				, quiet) == '19', &
			eval(''                         &                 ! 26
				//'struct P{x:i32, y:i32,}' &  ! point
				//'struct L{s:P, e:P}'      &  ! line
				//'let p1 = P{x= 6, y=13,};' &
				//'let p2 = P{x= 4, y=15,};' &
				//'let p3 = P{x=16, y=18,};' &
				//'let p4 = P{x=14, y=20,};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'ls[1].s.y = 21;' &
				//'return ls[1].s.y;' &
				, quiet) == '21', &
			eval(''                         &                 ! 27
				//'struct P{x:i32, y:i32,}' &  ! point
				//'struct L{s:P, e:P}'      &  ! line
				//'let p1 = P{x= 6, y=13,};' &
				//'let p2 = P{x= 4, y=15,};' &
				//'let p3 = P{x=16, y=18,};' &
				//'let p4 = P{x=14, y=20,};' &
				//'let l1 = L{s = p1, e = p2};' &
				//'let l2 = L{s = p3, e = p4};' &
				//'let ls = [l1, l2];' &
				//'ls[1].s.y += p1.x / 2;' &
				//'return ls[1].s.y;' &
				, quiet) == '21', &
			eval(''                         &                 ! 28
				//'struct P{v:[i32; :], s:str,}' &  ! point
				//'struct G{x:[P  ; :], s:str,}' &  ! polyGon of points
				//'let p0 = P{v=[6, 13], s="pta"};' &
				//'let p1 = P{v=[4, 15], s="ptb"};' &
				//'let p2 = P{v=[3, 17], s="ptc"};' &
				//'let g0 = G{x=[p0, p1, p2], s="tri"};' &
				//'return g0.x[0].v[0];' &
				, quiet) == '6', &
			eval(''                         &                 ! 29
				//'struct P{v:[i32; :], s:str,}' &  ! point
				//'struct G{x:[P  ; :], s:str,}' &  ! polyGon of points
				//'let p0 = P{v=[6, 13], s="pta"};' &
				//'let p1 = P{v=[4, 15], s="ptb"};' &
				//'let p2 = P{v=[3, 17], s="ptc"};' &
				//'let g0 = G{x=[p0, p1, p2], s="tri"};' &
				//'return g0.x[2].v[1];' &
				, quiet) == '17', &
			eval(''                         &                 ! 30
				//'struct P{v:[i32; :], s:str,}' &  ! point
				//'struct G{x:[P  ; :], s:str,}' &  ! polyGon of points
				//'let p0 = P{v=[6, 13], s="pta"};' &
				//'let p1 = P{v=[4, 15], s="ptb"};' &
				//'let p2 = P{v=[3, 17], s="ptc"};' &
				//'let g0 = G{x=[p0, p1, p2], s="tri"};' &
				//'g0.x[0].v[0] = 7;' &
				//'return g0.x[0].v[0];' &
				, quiet) == '7', &
			eval(''                         &                 ! 31
				//'struct P{v:[i32; :], s:str,}' &  ! point
				//'struct G{x:[P  ; :], s:str,}' &  ! polyGon of points
				//'let p0 = P{v=[6, 13], s="pta"};' &
				//'let p1 = P{v=[4, 15], s="ptb"};' &
				//'let p2 = P{v=[3, 17], s="ptc"};' &
				//'let g0 = G{x=[p0, p1, p2], s="tri"};' &
				//'g0.x[2].v[1] += 1;' &
				//'return g0.x[2].v[1];' &
				, quiet) == '18', &
			eval(''                         &                 ! 32
				//'struct P{v:[i32; :], s:str,}' &  ! point
				//'struct G{x:[P  ; :], s:str,}' &  ! polyGon of points
				//'let p0 = P{v=[6, 13], s="pta"};' &
				//'let p1 = P{v=[4, 15], s="ptb"};' &
				//'let p2 = P{v=[3, 17], s="ptc"};' &
				//'let g0 = G{x=[p0, p1, p2], s="tri"};' &
				//'g0.x[2].v = [2, 19];' &
				//'return g0.x[2].v;' &
				, quiet) == '[2, 19]', &
			eval('' &                                         ! 33
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'return e.e.d.c.b.a;' &
				, quiet) == '42', &
			eval('' &                                         ! 34
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: [B;:]}' &  ! middle array
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'return e.e.d.c[0].b.a;' &
				, quiet) == '42', &
			eval('' &                                         ! 35
				//'struct A{a: [i32;:]}' & ! inner-most array
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &       ! order-5 struct
				//'let a = A{a = [42]};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'return e.e.d.c.b.a[0];' &
				, quiet) == '42', &
			eval('' &                                         ! 36
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct, outer-most array
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = [E{e = d}];' &
				//'return e[0].e.d.c.b.a;' &
				, quiet) == '42', &
			eval('' &                                         ! 37
				//'struct A{a: [i32;:]}' &
				//'struct B{b: [A;:]}' &
				//'struct C{c: [B;:]}' &
				//'struct D{d: [C;:]}' &
				//'struct E{e: [D;:]}' &      ! order-5 struct, all arrays
				//'let a = A{a = [42]};' &
				//'let b = B{b = [a]};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = [c]};' &
				//'let e = [E{e = [d]}];' &
				//'return e[0].e[0].d[0].c[0].b[0].a[0];' &
				, quiet) == '42', &
			eval('' &                                         ! 38
				//'struct A{a: i32}' &
				//'struct B{b: [A;:]}' &
				//'struct C{c: B}' &
				//'struct D{d: [C;:]}' &
				//'struct E{e: D}' &      ! order-5 struct, alternating arrays
				//'let a = A{a = 1337};' &
				//'let b = B{b = [a]};' &
				//'let c = C{c = b};' &
				//'let d = D{d = [c]};' &
				//'let e = [E{e = d}];' &
				//'return e[0].e.d[0].c.b[0].a;' &
				, quiet) == '1337', &
			eval('' &                                         ! 39
				//'struct A{a: [i32;:]}' &
				//'struct B{b: A}' &
				//'struct C{c: [B;:]}' &
				//'struct D{d: C}' &
				//'struct E{e: [D;:]}' &      ! order-5 struct, alternating arrays the other way
				//'let a = A{a = [42]};' &
				//'let b = B{b = a};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = c};' &
				//'let e = E{e = [d]};' &
				//'return e.e[0].d.c[0].b.a[0];' &
				, quiet) == '42', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! I developed LHS dot exprs almost all at once, so I might have missed some
	! cases. For RHS dot exprs I did it piece-by-piece and added tests as I went

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)
	!print *, "number of "//label//" tests = ", size(tests)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct_arr2
subroutine unit_test_struct_arr3(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	! This tests both structs of arrays and arrays of structs.  There are
	! limited structs of arrays covered in unit_test_struct(), but more complex
	! cases are covered here
	character(len = *), parameter :: label = 'structs/arrays part 3'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('' &                                         ! 40
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'e.e.d.c.b.a = 69;' &
				//'return e.e.d.c.b.a;' &
				, quiet) == '69', &
			eval('' &                                         ! 41
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: [B;:]}' &  ! middle array
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'e.e.d.c[0].b.a = 69;' &
				//'return e.e.d.c[0].b.a;' &
				, quiet) == '69', &
			eval('' &                                         ! 42
				//'struct A{a: [i32;:]}' & ! inner-most array
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &       ! order-5 struct
				//'let a = A{a = [42]};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'e.e.d.c.b.a[0] = 69;' &
				//'return e.e.d.c.b.a[0];' &
				, quiet) == '69', &
			eval('' &                                         ! 43
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct, outer-most array
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = [E{e = d}];' &
				//'e[0].e.d.c.b.a = 69;' &
				//'return e[0].e.d.c.b.a;' &
				, quiet) == '69', &
			eval('' &                                         ! 44
				//'struct A{a: [i32;:]}' &
				//'struct B{b: [A;:]}' &
				//'struct C{c: [B;:]}' &
				//'struct D{d: [C;:]}' &
				//'struct E{e: [D;:]}' &      ! order-5 struct, all arrays
				//'let a = A{a = [42]};' &
				//'let b = B{b = [a]};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = [c]};' &
				//'let e = [E{e = [d]}];' &
				//'e[0].e[0].d[0].c[0].b[0].a[0] = 69;' &
				//'return e[0].e[0].d[0].c[0].b[0].a[0];' &
				, quiet) == '69', &
			eval('' &                                         ! 45
				//'struct A{a: i32}' &
				//'struct B{b: [A;:]}' &
				//'struct C{c: B}' &
				//'struct D{d: [C;:]}' &
				//'struct E{e: D}' &      ! order-5 struct, alternating arrays
				//'let a = A{a = 1337};' &
				//'let b = B{b = [a]};' &
				//'let c = C{c = b};' &
				//'let d = D{d = [c]};' &
				//'let e = [E{e = d}];' &
				//'e[0].e.d[0].c.b[0].a = 420;' &
				//'return e[0].e.d[0].c.b[0].a;' &
				, quiet) == '420', &
			eval('' &                                         ! 46
				//'struct A{a: [i32;:]}' &
				//'struct B{b: A}' &
				//'struct C{c: [B;:]}' &
				//'struct D{d: C}' &
				//'struct E{e: [D;:]}' &      ! order-5 struct, alternating arrays the other way
				//'let a = A{a = [42]};' &
				//'let b = B{b = a};' &
				//'let c = C{c = [b]};' &
				//'let d = D{d = c};' &
				//'let e = E{e = [d]};' &
				//'e.e[0].d.c[0].b.a[0] += 27;' &
				//'return e.e[0].d.c[0].b.a[0];' &
				, quiet) == '69', &
			eval(''                         &                 ! 47
				//'struct P{v:[i32; :],}' &  ! point
				//'let p0 = P{v=[6, 13]};' &
				//'let p1 = P{v=[4, 15]};' &
				//'let p2 = P{v=[3, 17]};' &
				//'let ps = [p0, p1];' &
				//'ps[0] = p2;' &
				//'return ps[1].v;' &
				, quiet) == '[4, 15]', &
			eval(''                         &                 ! 48
				//'struct P{v:[i32; :],}' &  ! point
				//'let p0 = P{v=[6, 13]};' &
				//'let p1 = P{v=[4, 15]};' &
				//'let p2 = P{v=[3, 17]};' &
				//'let ps = [p0, p1];' &
				//'ps[0] = p2;' &
				//'return ps[0].v;' &
				, quiet) == '[3, 17]', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 49
			    //'fn get_ds(): [D;:] {'&
				//'    let d0 = D{y=2024, m="Sep", d=21};' &
				//'    let d1 = D{y=1990, m="Aug", d=2};' &
				//'    return [d0, d1];' &
				//'}' &
			    //'let ds = get_ds();' &
			    //'return ds[1].m;' &
				, quiet) == 'Aug', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 50
			    //'fn get_ds(): [D;:] {'&
				//'    let d0 = D{y=2024, m="Sep", d=21};' &
				//'    let d1 = D{y=1990, m="Aug", d=2};' &
				//'    return [d0, d1];' &
				//'}' &
			    //'let ds = get_ds();' &
			    //'return ds[0].d;' &
				, quiet) == '21', &
			eval('' &                                         ! 51
				//'struct A{a: i32}' &
				//'struct B{b: A}' &
				//'struct C{c: B}' &
				//'struct D{d: C}' &
				//'struct E{e: D}' &      ! order-5 struct
				//'fn extract_a(x: E): i32 { return x.e.d.c.b.a; }' &
				//'let a = A{a = 42};' &
				//'let b = B{b = a};' &
				//'let c = C{c = b};' &
				//'let d = D{d = c};' &
				//'let e = E{e = d};' &
				//'return extract_a(e);' &
				, quiet = .false.) == '42', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 52
				//'let d0 = D{y=2024, m="Sep", d=21};' &
			    //'let c = d0.m[0];' &
			    //'return c;' &
				, quiet) == 'S', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 53
				//'let d0 = D{y=2024, m="Sep", d=21};' &
			    //'let c = d0.m[1];' &
			    //'return c;' &
				, quiet) == 'e', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 54
				//'let d0 = D{y=2024, m="Sep", d=21};' &
			    //'d0.m[0] = "P";' &
			    //'return d0.m;' &
				, quiet) == 'Pep', &
			eval('' &
				//'struct D{y:i32, m:str, d:i32}' &           ! 55
				//'let d0 = D{y=2024, m="Sep", d=21};' &
			    //'d0.m[2] = "a";' &
			    //'return d0.m;' &
				, quiet) == 'Sea', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! I developed LHS dot exprs almost all at once, so I might have missed some
	! cases. For RHS dot exprs I did it piece-by-piece and added tests as I went

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)
	!print *, "number of "//label//" tests = ", size(tests)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct_arr3

!===============================================================================

subroutine unit_test_struct_str(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'struct str formatting'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! This is a separate test from other struct stuff because struct to string
	! conversion is subject to change, especially if I figure out how to label
	! each member with its name
	!
	! TODO: update documentation to reflect any struct-to-str changes here

	tests = &
		[   &
			eval( 'struct P{x:i32, y:i32,}' &                 ! 1
			    //'let p1 = P{x=6, y=13};' &
			    //'p1;', quiet) == 'P{6, 13}', &
			eval(''                         &                 ! 2
				//'struct P{x:i32, y:i32,}' &  ! point
				//'let p1 = P{x=6, y=13,};' &
				//'let ps = [p1; 2];' &        ! ps is an array of 2 copies of p1
				//'return ps[0];' &
				, quiet) == 'P{6, 13}', &
			eval(''                         &                 ! 3
				//'struct P{x:i32, y:i32,}' &
				//'let p1 = P{x=6, y=13,};' &
				//'let p2 = P{x=4, y=15,};' &
				//'let ps = [p1, p2];' &
				//'return ps;' &
				, quiet) == '[P{6, 13}, P{4, 15}]', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)
	!print *, "number of "//label//" tests = ", size(tests)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct_str

!===============================================================================

subroutine unit_test_struct_long(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'struct scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/struct/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_struct_long

!===============================================================================

subroutine unit_test_bitwise_2(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'bitwise scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/bitwise/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '0', &
			interpret_file(path//'test-02.syntran', quiet) == '0', &
			interpret_file(path//'test-03.syntran', quiet) == '0', &
			interpret_file(path//'test-04.syntran', quiet) == '0', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bitwise_2

!===============================================================================

subroutine unit_test_ref(npass, nfail)

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'pass-by-reference'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/ref/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == '43', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == '0', &
			interpret_file(path//'test-05.syntran', quiet) == '0', &
			interpret_file(path//'test-06.syntran', quiet) == '0', &
			interpret_file(path//'test-07.syntran', quiet) == '0', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_ref

!===============================================================================

subroutine unit_test_array_bool(npass, nfail)

	! More advanced tests on longer scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'bool array scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/array-bool/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_array_bool

!===============================================================================

subroutine unit_test_f32_2(npass, nfail)

	! More advanced f32 tests on longer syntran scripts

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'f32 scripts'

	! Path to syntran test files from root of repo
	character(len = *), parameter :: path = 'src/tests/test-src/f32/'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	! TODO: more tests

	tests = &
		[   &
			interpret_file(path//'test-01.syntran', quiet) == 'true', &
			interpret_file(path//'test-02.syntran', quiet) == 'true', &
			interpret_file(path//'test-03.syntran', quiet) == 'true', &
			interpret_file(path//'test-04.syntran', quiet) == 'true', &
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
			eval('max(2);', quiet)  == '',  & ! arguable.  see comments in core.f90
			eval('size([0; 6, 7, 8], 2, 3);', quiet)  == '',  &
			eval('7 * false;', quiet) == '' &
		]

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_bad_syntax

!===============================================================================

subroutine unit_tests(iostat)

	implicit none

	integer, intent(out) :: iostat

	!********

	integer :: npass, nfail

	write(*,*) repeat('=', 60)
	write(*,*) 'Running syntran unit tests ...'
	write(*,*)

	npass = 0
	nfail = 0

	call unit_test_bin_arith  (npass, nfail)
	call unit_test_paren_arith(npass, nfail)
	call unit_test_unary_arith(npass, nfail)
	call unit_test_bool       (npass, nfail)
	call unit_test_comparisons(npass, nfail)
	call unit_test_comp_f32   (npass, nfail)
	call unit_test_comp_f64   (npass, nfail)
	call unit_test_bad_syntax (npass, nfail)
	call unit_test_assignment (npass, nfail)
	call unit_test_comments   (npass, nfail)
	call unit_test_blocks     (npass, nfail)
	call unit_test_f32_1      (npass, nfail)
	call unit_test_f64_1      (npass, nfail)
	call unit_test_str        (npass, nfail)
	call unit_test_substr     (npass, nfail)
	call unit_test_if_else    (npass, nfail)
	call unit_test_for_1      (npass, nfail)
	call unit_test_for        (npass, nfail)
	call unit_test_while      (npass, nfail)
	call unit_test_var_scopes (npass, nfail)
	call unit_test_f32_2      (npass, nfail)
	call unit_test_array_i32_1(npass, nfail)
	call unit_test_array_i32_2(npass, nfail)
	call unit_test_array_f32_1(npass, nfail)
	call unit_test_array_f32_2(npass, nfail)
	call unit_test_array_str  (npass, nfail)
	call unit_test_array_bool (npass, nfail)
	call unit_test_nd_i32     (npass, nfail)
	call unit_test_intr_fns   (npass, nfail)
	call unit_test_fns        (npass, nfail)
	call unit_test_linalg_fns (npass, nfail)
	call unit_test_comp_ass   (npass, nfail)
	call unit_test_io         (npass, nfail)
	call unit_test_i64        (npass, nfail)
	call unit_test_include    (npass, nfail)
	call unit_test_rhs_slc_1  (npass, nfail)
	call unit_test_arr_comp   (npass, nfail)
	call unit_test_arr_op     (npass, nfail)
	call unit_test_lhs_slc_1  (npass, nfail)
	call unit_test_control    (npass, nfail)
	call unit_test_struct     (npass, nfail)
	call unit_test_struct_arr1(npass, nfail)
	call unit_test_struct_arr2(npass, nfail)
	call unit_test_struct_arr3(npass, nfail)
	call unit_test_struct_str (npass, nfail)
	call unit_test_struct_long(npass, nfail)
	call unit_test_f64_mix    (npass, nfail)
	call unit_test_literals   (npass, nfail)
	call unit_test_bitwise    (npass, nfail)
	call unit_test_bit_ass    (npass, nfail)
	call unit_test_bitwise_2  (npass, nfail)
	call unit_test_ref        (npass, nfail)

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

