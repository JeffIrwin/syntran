
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
			abs(eval_f32('let f = 0.5; f += 0.25; f;', quiet) - 0.75) < tol, &
			eval('let s = "hello "; s += "world"; s;', quiet) == 'hello world', &
			abs(eval_f32('let f = 0.5; f += 0.25;', quiet) - 0.75) < tol, &
			eval('let s = "hello "; s += "world";', quiet) == 'hello world', &
			eval('let iv = [10; 3]; iv[0] += 5;', quiet) == '15', &
			eval('let iv = [10; 3]; iv[0] += 5; iv;', quiet) == '[15, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] += 5; iv;', quiet) == '[10, 15, 10]', &
			eval('let iv = [10; 3]; iv[1] += 5.1; iv;', quiet) == '[10, 15, 10]', &
			abs(eval_f32('let v = [10.0; 3]; v[0] += 5.0;', quiet) - 15) < tol, &
			eval('let v = [10.0; 3]; v[0] += 5.0; v;', quiet) == '[1.500000E+01, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] += 5.0; v;', quiet) == '[1.000000E+01, 1.500000E+01, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] += 5; v;', quiet) == '[1.000000E+01, 1.500000E+01, 1.000000E+01]', &
			eval('let i = 20; i += 5.1;', quiet) == '25', &
			abs(eval_f32('let i = 20.1; i += 5;', quiet) - 25.1) < tol, &
			eval('let j = 10; j -= 3; j;', quiet) == '7', &
			abs(eval_f32('let f = 0.75; f -= 0.25; f;', quiet) - 0.5) < tol, &
			eval('let j = 10; j -= 3;', quiet) == '7', &
			abs(eval_f32('let f = 0.75; f -= 0.25;', quiet) - 0.5) < tol, &
			eval('let iv = [10; 3]; iv[0] -= 4;', quiet) == '6', &
			eval('let iv = [10; 3]; iv[0] -= 4; iv;', quiet) == '[6, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] -= 4; iv;', quiet) == '[10, 6, 10]', &
			eval('let iv = [10; 3]; iv[1] -= 3.9; iv;', quiet) == '[10, 6, 10]', &
			abs(eval_f32('let v = [10.0; 3]; v[0] -= 4.0;', quiet) - 6) < tol, &
			eval('let v = [10.0; 3]; v[0] -= 4.0; v;', quiet) == '[6.000000E+00, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [20.0; 3]; v[1] -= 4.0; v;', quiet) == '[2.000000E+01, 1.600000E+01, 2.000000E+01]', &
			eval('let v = [20.0; 3]; v[1] -= 4; v;', quiet) == '[2.000000E+01, 1.600000E+01, 2.000000E+01]', &
			eval('let i = 20; i -= 5.1;', quiet) == '14', &
			abs(eval_f32('let i = 20.1; i -= 5;', quiet) - 15.1) < tol, &
			eval('let i = 20; i -= 3.1;', quiet) == '16', &
			abs(eval_f32('let i = 20.1; i -= 3;', quiet) - 17.1) < tol, &
			eval('let j =  7; j *= 6; j;', quiet) == '42', &
			eval('let j = 10; j *= 3; j;', quiet) == '30', &
			abs(eval_f32('let f = 0.5; f *= 0.25; f;', quiet) - 0.125) < tol, &
			abs(eval_f32('let f = 0.5; f *= 0.25;', quiet) - 0.125) < tol, &
			eval('let iv = [10; 3]; iv[0] *= 5;', quiet) == '50', &
			eval('let iv = [10; 3]; iv[0] *= 5; iv;', quiet) == '[50, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] *= 5; iv;', quiet) == '[10, 50, 10]', &
			eval('let iv = [10; 3]; iv[1] *= 5.101; iv;', quiet) == '[10, 51, 10]', &
			abs(eval_f32('let v = [10.0; 3]; v[0] *= 5.0;', quiet) - 50) < tol, &
			eval('let v = [10.0; 3]; v[0] *= 5.0; v;', quiet) == '[5.000000E+01, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] *= 5.0; v;', quiet) == '[1.000000E+01, 5.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] *= 5; v;', quiet) == '[1.000000E+01, 5.000000E+01, 1.000000E+01]', &
			eval('let i = 20; i *= 5.101;', quiet) == '102', &
			abs(eval_f32('let i = 20.1; i *= 5;', quiet) - 100.5) < tol, &
			eval('let j = 12; j /= 3; j;', quiet) == '4', &
			abs(eval_f32('let f = 0.5; f /= 0.25; f;', quiet) - 2.0) < tol, &
			abs(eval_f32('let f = 0.5; f /= 0.25;', quiet) - 2.0) < tol, &
			eval('let iv = [10; 3]; iv[0] /= 5;', quiet) == '2', &
			eval('let iv = [10; 3]; iv[0] /= 5; iv;', quiet) == '[2, 10, 10]', &
			eval('let iv = [10; 3]; iv[1] /= 5; iv;', quiet) == '[10, 2, 10]', &
			eval('let iv = [10; 3]; iv[1] /= 4.9; iv;', quiet) == '[10, 2, 10]', &
			abs(eval_f32('let v = [10.0; 3]; v[0] /= 5.0;', quiet) - 2) < tol, &
			eval('let v = [10.0; 3]; v[0] /= 5.0; v;', quiet) == '[2.000000E+00, 1.000000E+01, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] /= 5.0; v;', quiet) == '[1.000000E+01, 2.000000E+00, 1.000000E+01]', &
			eval('let v = [10.0; 3]; v[1] /= 5; v;', quiet) == '[1.000000E+01, 2.000000E+00, 1.000000E+01]', &
			eval('let i = 20; i /= 4.9;', quiet) == '4', &
			abs(eval_f32('let i = 20.5; i /= 5;', quiet) - 4.1) < tol, &
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

	logical, allocatable :: tests(:)

	real, parameter :: tol = 1.e-9

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			abs(eval_f32('exp(0.0);') - 1.0) < tol,  &
			abs(eval_f32('exp(1.0);') - exp(1.0)) < tol,  &
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
			eval_i32('len(     "");')  == 0,  &
			eval_i32('len(    " ");')  == 1,  &
			eval_i32('len(   "  ");')  == 2,  &
			eval_i32('len(  "   ");')  == 3,  &
			eval_i32('len( "    ");')  == 4,  &
			eval_i32('len("     ");')  == 5,  &
			eval_i32('len(    "h");')  == 1,  &
			eval_i32('len(   "ht");')  == 2,  &
			eval_i32('len(  "htn");')  == 3,  &
			eval_i32('len( "htns");')  == 4,  &
			eval_i32('len("htns-");')  == 5,  &
			eval('i32( 0.0);') ==   "0", &
			eval('i32( 1.1);') ==   "1", &
			eval('i32(-1.1);') ==  "-1", &
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

	real, parameter :: tol = 1.e-9

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
			eval('let sum_ = 0.0; for x in [0.0: 1.0: 5.0] sum_ += x; [sum_];', quiet) == '[1.000000E+01]', &  ! [] is poor man's trim()
			eval('let sum_ = 0.0; for x in [0.0:1.0:6.0] sum_ += x; [sum_];', quiet)   == '[1.500000E+01]', &
			eval('let sum_ = 0.0; for x in [10.0:1.0:16.0] sum_ += x; [sum_];', quiet) == '[7.500000E+01]', &
			eval('let sum_ = 0.0; for x in [5.0:-1.0:0.0] sum_ += x; [sum_];', quiet)  == '[1.500000E+01]', &
			eval('let sum_ = 0.0; for x in [5.0:-1.0:-2.0] sum_ += x; [sum_];', quiet) == '[1.400000E+01]', &
			eval('let sum_ = 0.0; for x in [0.0:2.0:6.0] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let vec = [0:6]; let sum_ = 0; for x in vec sum_ += x; sum_;', quiet) == '15', &
			eval('let vec = [0.0:1.0:6.0]; let sum_ = 0.0; for x in vec sum_ += x; [sum_];', quiet) == '[1.500000E+01]', &
			eval('let mat = [1,2,3, 4,5,6; 3,2]; let sum_ = 0; for x in mat sum_ += x; sum_;', quiet) == '21', &
			eval('let sum_ = 0.0; for x in [0.0: 4.0; 5] sum_ += x; [sum_];', quiet) == '[1.000000E+01]', &  ! [] is poor man's trim()
			eval('let sum_ = 0.0; for x in [0.0: 5.0; 6] sum_ += x; [sum_];', quiet)   == '[1.500000E+01]', &
			eval('let sum_ = 0.0; for x in [10.0: 15.0; 6] sum_ += x; [sum_];', quiet) == '[7.500000E+01]', &
			eval('let sum_ = 0.0; for x in [5.0: 1.0; 5] sum_ += x; [sum_];', quiet)  == '[1.500000E+01]', &
			eval('let sum_ = 0.0; for x in [5.0: -1.0; 7] sum_ += x; [sum_];', quiet) == '[1.400000E+01]', &
			eval('let sum_ = 0.0; for x in [0.0: 4.0; 3] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let sum_ = 0.0; for x in [0.0, 4.0, 2.0] sum_ += x; [sum_];', quiet)   == '[6.000000E+00]', &
			eval('let sum_ = 0; for x in [4, 0, 2] sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = 0; for x in i32([4, 0, 2]) sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = i64(0); for x in [i64(4), i64(0), i64(2)] sum_ += x; sum_;', quiet)   == '6', &
			eval('let sum_ = 0; for x in [4, 0, 3, 2; 2, 2] sum_ += x; sum_;', quiet)   == '9', &
			eval('let sum_ = i64(0); for x in [i64(4), i64(0), i64(3), i64(2); 2, 2] sum_ += x; sum_;', quiet)   == '9', &
			eval('let sum_ = 0.0; for x in [4.0, 0.0, 3.0, 2.0; 2, 2] sum_ += x; [sum_];', quiet)   == '[9.000000E+00]', &
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
			abs(eval_f32('i64(2) * 3.0;', quiet) - 6.0) < tol, &
			abs(eval_f32('3.0 * i64(2);', quiet) - 6.0) < tol, &
			abs(eval_f32('i64(2) + 3.0;', quiet) - 5.0) < tol, &
			abs(eval_f32('3.0 + i64(2);', quiet) - 5.0) < tol, &
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
			eval('"testing " + str(1.0);') == 'testing     1.000000E+00', &
			eval('"testing testing " + str(1) + " " + str(2);') == 'testing testing 1 2', &
			eval('"testing " + str(1, " ", 2, " ", 1, " ", 2);') == 'testing 1 2 1 2', &
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
			eval('[0.0, 1.0] + 2.0;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + 3.0;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + -1.0;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3.0: 1.0: 5.1] + -2.0;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3.0: 2.0: 7.1] + -2.0;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2.0 + [0.0, 1.0];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3.0 + [0.0, 1.0, 2.0];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1.0 + [0.0, 1.0, 2.0];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2.0 + [3.0: 1.0: 5.1];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2.0 + [3.0: 2.0: 7.1];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0.0, 1.0] + [1.0, 2.0];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + [3.0, 4.0, 5.0];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + [-1.0, 5.0, -3.0];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3.0: 1.0: 5.1] + [-3.0: -1.0: -5.1];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3.0: 2.0: 7.1] + [-2.0, -3.0, -4.0];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0.0: 1.0: 99.1] + [99.0: -1.0: -0.1] == 9.900000E+01);') == 'true', &
			eval('[0, 1] + 2.0;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0, 1, 2] + 3.0;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0, 1, 2] + -1.0;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3: 1: 6] + -2.0;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3: 2: 8] + -2.0;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2 + [0.0, 1.0];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3 + [0.0, 1.0, 2.0];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1 + [0.0, 1.0, 2.0];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2 + [3.0: 1.0: 5.1];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2 + [3.0: 2.0: 7.1];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0, 1] + [1.0, 2.0];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1, 2] + [3.0, 4.0, 5.0];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0, 1, 2] + [-1.0, 5.0, -3.0];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3: 1: 6] + [-3.0: -1.0: -5.1];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3: 2: 8] + [-2.0, -3.0, -4.0];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0: 1: 100] + [99.0: -1.0: -0.1] == 9.900000E+01);') == 'true', &
			eval('[0.0, 1.0] + 2;') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + 3;') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + -1;') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('[3.0: 1.0: 5.1] + -2;') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[3.0: 2.0: 7.1] + -2;') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('2.0 + [0, 1];') == '[2.000000E+00, 3.000000E+00]', &
			eval('3.0 + [0, 1, 2];') == '[3.000000E+00, 4.000000E+00, 5.000000E+00]', &
			eval('-1.0 + [0, 1, 2];') == '[-1.000000E+00, 0.000000E+00, 1.000000E+00]', &
			eval('-2.0 + [3: 1: 6];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('-2.0 + [3: 2: 8];') == '[1.000000E+00, 3.000000E+00, 5.000000E+00]', &
			eval('[0.0, 1.0] + [1, 2];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + [3, 4, 5];') == '[3.000000E+00, 5.000000E+00, 7.000000E+00]', &
			eval('[0.0, 1.0, 2.0] + [-1, 5, -3];') == '[-1.000000E+00, 6.000000E+00, -1.000000E+00]', &
			eval('[3.0: 1.0: 5.1] + [-3: -1: -6];') == '[0.000000E+00, 0.000000E+00, 0.000000E+00]', &
			eval('[3.0: 2.0: 7.1] + [-2, -3, -4];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('all([0.0: 1.0: 99.1] + [99: -1: -1] == 9.900000E+01);') == 'true', &
			eval('[i64(0), i64(1)] + i64(2);') == '[2, 3]', &
			eval('i64(2) + [i64(0), i64(1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] + 2;') == '[2, 3]', &
			eval('i64(2) + [0, 1];') == '[2, 3]', &
			eval('[0, 1] + i64(2);') == '[2, 3]', &
			eval('2 + [i64(0), i64(1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] + 2.0;') == '[2.000000E+00, 3.000000E+00]', &
			eval('i64(2) + [0.0, 1.0];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0] + i64(2);') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0 + [i64(0), i64(1)];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] + [i64(1), i64(2)];') == '[1, 3]', &
			eval('[0, 1] + [i64(1), i64(2)];') == '[1, 3]', &
			eval('[i64(0), i64(1)] + [1, 2];') == '[1, 3]', &
			eval('[0.0, 1.0] + [i64(1), i64(2)];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] + [1.0, 2.0];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1] - [-1, -2];') == '[1, 3]', &
			eval('[0, 1] - -2;') == '[2, 3]', &
			eval('2 - [-0, -1];') == '[2, 3]', &
			eval('[0, 1] - -2.0;') == '[2.000000E+00, 3.000000E+00]', &
			eval('2 - [-0.0, -1.0];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0] - -2;') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0 - [-0, -1];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0] - [-1, -2];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[0, 1] - [-1.0, -2.0];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - i64(-2);') == '[2, 3]', &
			eval('i64(2) - [i64(-0), i64(-1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] - -2;') == '[2, 3]', &
			eval('i64(2) - [-0, -1];') == '[2, 3]', &
			eval('[0, 1] - i64(-2);') == '[2, 3]', &
			eval('2 - [i64(-0), i64(-1)];') == '[2, 3]', &
			eval('[i64(0), i64(1)] - -2.0;') == '[2.000000E+00, 3.000000E+00]', &
			eval('i64(2) - [-0.0, -1.0];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[0.0, 1.0] - i64(-2);') == '[2.000000E+00, 3.000000E+00]', &
			eval('2.0 - [i64(0), i64(-1)];') == '[2.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - [i64(-1), i64(-2)];') == '[1, 3]', &
			eval('[0, 1] - [i64(-1), i64(-2)];') == '[1, 3]', &
			eval('[i64(0), i64(1)] - [-1, -2];') == '[1, 3]', &
			eval('[0.0, 1.0] - [i64(-1), i64(-2)];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[i64(0), i64(1)] - [-1.0, -2.0];') == '[1.000000E+00, 3.000000E+00]', &
			eval('[2, 3] * [1, 2];') == '[2, 6]', &
			eval('[2, 3] * 2;') == '[4, 6]', &
			eval('2 * [1, 2];') == '[2, 4]', &
			eval('[1, 2] * 2.0;') == '[2.000000E+00, 4.000000E+00]', &
			eval('2 * [1.0, 2.0];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0, 2.0] * 2;') == '[2.000000E+00, 4.000000E+00]', &
			eval('2.0 * [1, 2];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0, 2.0] * [1, 2];') == '[1.000000E+00, 4.000000E+00]', &
			eval('[2, 3] * [1.0, 2.0];') == '[2.000000E+00, 6.000000E+00]', &
			eval('[i64(2), i64(3)] * i64(2);') == '[4, 6]', &
			eval('i64(2) * [i64(2), i64(3)];') == '[4, 6]', &
			eval('[i64(2), i64(3)] * 2;') == '[4, 6]', &
			eval('i64(2) * [2, 3];') == '[4, 6]', &
			eval('[2, 4] * i64(2);') == '[4, 8]', &
			eval('2 * [i64(3), i64(4)];') == '[6, 8]', &
			eval('[i64(2), i64(3)] * 2.0;') == '[4.000000E+00, 6.000000E+00]', &
			eval('i64(2) * [2.0, 3.0];') == '[4.000000E+00, 6.000000E+00]', &
			eval('[2.0, 3.0] * i64(2);') == '[4.000000E+00, 6.000000E+00]', &
			eval('2.0 * [i64(2), i64(3)];') == '[4.000000E+00, 6.000000E+00]', &
			eval('[i64(3), i64(4)] * [i64(1), i64(2)];') == '[3, 8]', &
			eval('[3, 4] * [i64(1), i64(2)];') == '[3, 8]', &
			eval('[i64(3), i64(4)] * [1, 2];') == '[3, 8]', &
			eval('[5.0, 4.0] * [i64(1), i64(2)];') == '[5.000000E+00, 8.000000E+00]', &
			eval('[i64(5), i64(4)] * [1.0, 2.0];') == '[5.000000E+00, 8.000000E+00]', &
			eval('[2, 3] ** [1, 2];') == '[2, 9]', &
			eval('[2, 3] ** 2;') == '[4, 9]', &
			eval('2 ** [1, 2];') == '[2, 4]', &
			eval('[1, 2] ** 2.0;') == '[1.000000E+00, 4.000000E+00]', &
			eval('2 ** [1.0, 2.0];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0, 2.0] ** 2;') == '[1.000000E+00, 4.000000E+00]', &
			eval('2.0 ** [1, 2];') == '[2.000000E+00, 4.000000E+00]', &
			eval('[1.0, 2.0] ** [1, 2];') == '[1.000000E+00, 4.000000E+00]', &
			eval('[2, 3] ** [1.0, 2.0];') == '[2.000000E+00, 9.000000E+00]', &
			eval('[i64(2), i64(3)] ** i64(2);') == '[4, 9]', &
			eval('i64(2) ** [i64(2), i64(3)];') == '[4, 8]', &
			eval('[i64(2), i64(3)] ** 2;') == '[4, 9]', &
			eval('i64(2) ** [2, 3];') == '[4, 8]', &
			eval('[2, 4] ** i64(2);') == '[4, 16]', &
			eval('2 ** [i64(3), i64(4)];') == '[8, 16]', &
			eval('[i64(2), i64(3)] ** 2.0;') == '[4.000000E+00, 9.000000E+00]', &
			eval('i64(2) ** [2.0, 3.0];') == '[4.000000E+00, 8.000000E+00]', &
			eval('[2.0, 3.0] ** i64(2);') == '[4.000000E+00, 9.000000E+00]', &
			eval('2.0 ** [i64(2), i64(3)];') == '[4.000000E+00, 8.000000E+00]', &
			eval('[i64(3), i64(4)] ** [i64(1), i64(2)];') == '[3, 16]', &
			eval('[3, 4] ** [i64(1), i64(2)];') == '[3, 16]', &
			eval('[i64(3), i64(4)] ** [1, 2];') == '[3, 16]', &
			eval('[5.0, 4.0] ** [i64(1), i64(2)];') == '[5.000000E+00, 1.600000E+01]', &
			eval('[i64(5), i64(4)] ** [1.0, 2.0];') == '[5.000000E+00, 1.600000E+01]', &
			eval('[2, 4] / [1, 2];') == '[2, 2]', &
			eval('[2, 4] / 2;') == '[1, 2]', &
			eval('4 / [1, 2];') == '[4, 2]', &
			eval('[1, 2] / 2.0;') == '[5.000000E-01, 1.000000E+00]', &
			eval('2 / [1.0, 2.0];') == '[2.000000E+00, 1.000000E+00]', &
			eval('[1.0, 2.0] / 2;') == '[5.000000E-01, 1.000000E+00]', &
			eval('2.0 / [1, 2];') == '[2.000000E+00, 1.000000E+00]', &
			eval('[1.0, 2.0] / [1, 2];') == '[1.000000E+00, 1.000000E+00]', &
			eval('[2, 3] / [1.0, 2.0];') == '[2.000000E+00, 1.500000E+00]', &
			eval('[i64(2), i64(4)] / i64(2);') == '[1, 2]', &
			eval('i64(6) / [i64(2), i64(3)];') == '[3, 2]', &
			eval('[i64(2), i64(4)] / 2;') == '[1, 2]', &
			eval('i64(6) / [2, 3];') == '[3, 2]', &
			eval('[2, 4] / i64(2);') == '[1, 2]', &
			eval('2 / [i64(1), i64(2)];') == '[2, 1]', &
			eval('[i64(2), i64(3)] / 2.0;') == '[1.000000E+00, 1.500000E+00]', &
			eval('i64(3) / [2.0, 3.0];') == '[1.500000E+00, 1.000000E+00]', &
			eval('[2.0, 3.0] / i64(2);') == '[1.000000E+00, 1.500000E+00]', &
			eval('3.0 / [i64(2), i64(3)];') == '[1.500000E+00, 1.000000E+00]', &
			eval('[i64(3), i64(4)] / [i64(1), i64(2)];') == '[3, 2]', &
			eval('[3, 4] / [i64(1), i64(2)];') == '[3, 2]', &
			eval('[i64(3), i64(4)] / [1, 2];') == '[3, 2]', &
			eval('[5.0, 4.0] / [i64(1), i64(2)];') == '[5.000000E+00, 2.000000E+00]', &
			eval('[i64(5), i64(4)] / [1.0, 2.0];') == '[5.000000E+00, 2.000000E+00]', &
			eval('[2, 4] % [1, 2];') == '[0, 0]', &
			eval('[2, 4] % 3;') == '[2, 1]', &
			eval('4 % [2, 3];') == '[0, 1]', &
			eval('[1, 2] % 2.0;') == '[1.000000E+00, 0.000000E+00]', &
			eval('2 % [3.0, 2.0];') == '[2.000000E+00, 0.000000E+00]', &
			eval('[4.0, 5.0] % 3;') == '[1.000000E+00, 2.000000E+00]', &
			eval('2.0 % [2, 3];') == '[0.000000E+00, 2.000000E+00]', &
			eval('[3.0, 4.0] % [2, 3];') == '[1.000000E+00, 1.000000E+00]', &
			eval('[2, 3] % [1.0, 2.0];') == '[0.000000E+00, 1.000000E+00]', &
			eval('[i64(3), i64(4)] % i64(2);') == '[1, 0]', &
			eval('i64(5) % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[i64(3), i64(4)] % 2;') == '[1, 0]', &
			eval('i64(5) % [2, 3];') == '[1, 2]', &
			eval('[3, 4] % i64(2);') == '[1, 0]', &
			eval('3 % [i64(2), i64(3)];') == '[1, 0]', &
			eval('[i64(2), i64(3)] % 2.0;') == '[0.000000E+00, 1.000000E+00]', &
			eval('i64(3) % [2.0, 3.0];') == '[1.000000E+00, 0.000000E+00]', &
			eval('[2.0, 3.0] % i64(2);') == '[0.000000E+00, 1.000000E+00]', &
			eval('3.0 % [i64(2), i64(3)];') == '[1.000000E+00, 0.000000E+00]', &
			eval('[i64(3), i64(5)] % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[3, 5] % [i64(2), i64(3)];') == '[1, 2]', &
			eval('[i64(3), i64(5)] % [2, 3];') == '[1, 2]', &
			eval('[3.0, 5.0] % [i64(2), i64(3)];') == '[1.000000E+00, 2.000000E+00]', &
			eval('[i64(3), i64(5)] % [2.0, 3.0];') == '[1.000000E+00, 2.000000E+00]', &
			.false. &
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_arr_op

!===============================================================================

subroutine unit_test_slice_1(npass, nfail)

	! Simple array slicing tests

	implicit none

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: label = 'array slicing'

	logical, parameter :: quiet = .true.
	logical, allocatable :: tests(:)

	write(*,*) 'Unit testing '//label//' ...'

	tests = &
		[   &
			eval('let v = [0: 10]; v[0: 4];', quiet) == '[0, 1, 2, 3]', &
			eval('let v = [0: 10]; v[2: 5];', quiet) == '[2, 3, 4]', &
			eval('let v = [0: 10]; let u = v[2: 5]; u[0];', quiet) == '2', &
			eval('let v = [0: 10]; let u = v[2: 5]; u;', quiet) == '[2, 3, 4]', &
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
			eval('let m = [1.0; 2, 2]; m[0:2, 1];', quiet) == '[1.000000E+00, 1.000000E+00]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[0, :];', quiet) == '[0, 3, 6]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[2, :];', quiet) == '[2, 5, 8]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[:, 0];', quiet) == '[0, 1, 2]', &
			eval('let m = [0, 1, 2, 3, 4, 5, 6, 7, 8; 3, 3]; m[:, 2];', quiet) == '[6, 7, 8]', &
			.false.  & ! so I don't have to bother w/ trailing commas
		]

	! Trim dummy false element
	tests = tests(1: size(tests) - 1)

	call unit_test_coda(tests, label, npass, nfail)

end subroutine unit_test_slice_1

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
			eval('[42.0];') == '[4.200000E+01]',  &
			eval('[-42.,1337.];') == '[-4.200000E+01, 1.337000E+03]', &
			eval('[3., 2., 1.];') == '[3.000000E+00, 2.000000E+00, 1.000000E+00]', &
			eval('[3.: 1.; 3];') == '[3.000000E+00, 2.000000E+00, 1.000000E+00]', &
			eval('[3.: 1.; 5];') == '[3.000000E+00, 2.500000E+00, 2.000000E+00, 1.500000E+00, 1.000000E+00]', &
			eval('[1.: 3.; 3];') == '[1.000000E+00, 2.000000E+00, 3.000000E+00]', &
			eval('[1.: 3.; 5];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00]', &
			eval('[1.: 0.5: 2.9];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00]', &
			eval('[1.: 0.5: 3.4];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00]', &
			eval('[1.: 0.5: 3.9];') == '[1.000000E+00, 1.500000E+00, 2.000000E+00, 2.500000E+00, 3.000000E+00, 3.500000E+00]', &
			eval('[4.: 0.5: 5.4];') == '[4.000000E+00, 4.500000E+00, 5.000000E+00]', &
			eval('[5.4: -0.5: 4.0];') == '[5.400000E+00, 4.900000E+00, 4.400000E+00]', &
			eval('[2.-3.: 1.1: 6./3. + 3.];') == '[-1.000000E+00, 1.000000E-01, 1.200000E+00, 2.300000E+00, 3.400000E+00, 4.500000E+00]', &
			eval('[42.0; 3];') == '[4.200000E+01, 4.200000E+01, 4.200000E+01]',  &
			eval('[42.0; 4];') == '[4.200000E+01, 4.200000E+01, 4.200000E+01, 4.200000E+01]',  &
			eval('let myArray = [2.-3.: 1.1: 6./3. + 3.];') &
				== '[-1.000000E+00, 1.000000E-01, 1.200000E+00, 2.300000E+00, 3.400000E+00, 4.500000E+00]', &
			eval('[48.-6., 13.*100. + 37.];') == '[4.200000E+01, 1.337000E+03]'  &
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
	call unit_test_comp_f32   (npass, nfail)
	call unit_test_bad_syntax (npass, nfail)
	call unit_test_assignment (npass, nfail)
	call unit_test_comments   (npass, nfail)
	call unit_test_blocks     (npass, nfail)
	call unit_test_f32_1      (npass, nfail)
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
	call unit_test_slice_1    (npass, nfail)
	call unit_test_arr_comp   (npass, nfail)
	call unit_test_arr_op     (npass, nfail)

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

