
!===============================================================================

submodule (syntran__transpile_m) syntran__transpile_stmt

	! Fortran backend: statements, fns, and assembling the whole program.
	!
	! syntran's scopes are flattened.  Every binding has a unique slot (see
	! transpile.f90), so all of a procedure's variables are simply declared at
	! its top, and a `let` becomes a plain assignment where it stood.  Never emit
	! an initializer in a declaration: that would imply `save`, which breaks
	! recursion and re-entering a loop body

	implicit none

	type node_ptr_t
		type(syntax_node_t), pointer :: p => null()
	end type node_ptr_t

	! Longest line to emit before wrapping.  gfortran's default free-form limit
	! is 132
	integer, parameter :: MAX_LINE = 100

!===============================================================================

contains

!===============================================================================

function new_tmp_of(em, val, prefix) result(name)

	! A hidden temporary variable of the type of `val`, which may be an array

	type(emitter_t), intent(inout) :: em
	type(value_t), intent(in) :: val
	character(len = *), intent(in) :: prefix
	character(len = :), allocatable :: name

	logical :: ok

	em%tmp_count = em%tmp_count + 1
	name = prefix//'_t'//str(em%tmp_count)

	call em%decls%push(decl_line(val, name, ok))
	if (.not. ok) call em_unsupported(em, 'a temporary of type `'//kind_name(val%type)//'`')

end function new_tmp_of

!===============================================================================

subroutine emit_result_str(em, s)

	! Print the program's result, given a Fortran string expression for it

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: s

	if (.not. em%print_result) return

	if (em%trim_result) then
		call em_line(em, 'call rt_result(trim(adjustl('//s//')))')
	else
		call em_line(em, 'call rt_result('//s//')')
	end if

end subroutine emit_result_str

!===============================================================================

subroutine emit_result_invalid(em)

	! What the interpreter prints for a program whose last statement doesn't
	! have a value, like a loop

	type(emitter_t), intent(inout) :: em

	call emit_result_str(em, "'Error: <invalid_value>'")

end subroutine emit_result_invalid

!===============================================================================

function str_of(val, s) result(r)

	! A Fortran expression for the string that syntran makes from a value, given
	! the Fortran expression `s` for the value itself

	type(value_t), intent(in) :: val
	character(len = *), intent(in) :: s
	character(len = :), allocatable :: r

	if (is_arr(val)) then
		r = 'rt_str_a('//s//')'
	else if (val%type == str_type) then
		r = s
	else
		r = 'rt_str('//s//')'
	end if

end function str_of

!===============================================================================

recursive subroutine emit_print(em, node)

	! println(a, b, ...) writes each argument, without a separator, and then
	! ends the line

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	integer :: i

	if (allocated(node%args)) then
		do i = 1, size(node%args)
			call em_line(em, 'call rt_print('// &
				str_of(node%args(i)%val, emit_expr(em, node%args(i)))//')')
		end do
	end if

	call em_line(em, 'call rt_endl()')

end subroutine emit_print

!===============================================================================

recursive subroutine emit_call_stmt(em, node)

	! A call whose value, if any, is discarded

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: tmp

	if (node%kind == fn_call_intr_expr) then
		select case (node%identifier%text)
		case ('println')
			call emit_print(em, node)
			return

		case ('exit')
			call em_line(em, 'call rt_exit('// &
				convert(emit_expr(em, node%args(1)), elem_type(node%args(1)%val), &
				i32_type)//')')
			return

		end select
	end if

	if (node%val%type == void_type) then
		if (node%kind == fn_call_expr) then
			call em_line(em, 'call '//emit_expr(em, node))
		else
			call em_unsupported(em, 'the intrinsic fn `'// &
				node%identifier%text//'`', node%identifier%pos)
		end if

	else
		! Evaluate for its side effects
		tmp = new_tmp_of(em, node%val, 'discard')
		call em_line(em, tmp//' = '//emit_expr(em, node))

	end if

end subroutine emit_call_stmt

!===============================================================================

recursive subroutine emit_let(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: rhs

	rhs = emit_expr(em, node%right)
	call declare_var(em, node, node%val)
	call em_line(em, var_name(node)//' = '// &
		unparen(convert(rhs, elem_type(node%right%val), elem_type(node%val))))

end subroutine emit_let

!===============================================================================

recursive module subroutine emit_assign(em, node)

	! Plain and compound assignment, to a variable, an element or a slice of
	! one, or a character of a string

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: lhs, rhs, op_str, fn_str

	integer :: ct, lt, rt

	logical :: compound, lhs_arr, rhs_arr

	if (allocated(node%member)) then
		call em_unsupported(em, 'assignment to a struct member')
		return
	end if

	lt = elem_type(node%val)
	rt = elem_type(node%right%val)
	lhs_arr = is_arr(node%val)
	rhs_arr = is_arr(node%right%val)
	compound = node%op%kind /= equals_token

	! A compound assignment names its target twice, so anything with side
	! effects in a subscript is only evaluated once, up front
	lhs = emit_name_ref(em, node, hoist = compound, target = .true.)
	rhs = emit_expr(em, node%right)

	if (lt == str_type) then
		! An array of strings is an array of wrappers, so a lone string needs
		! wrapping to be assigned to (a slice of) one
		if (lhs_arr .and. .not. rhs_arr) rhs = 'rt_str_t_of('//rhs//')'

		if (.not. compound) then
			call em_line(em, lhs//' = '//rhs)
		else if (node%op%kind == plus_equals_token) then
			if (lhs_arr) then
				call em_line(em, lhs//' = rt_cat('//lhs//', '//rhs//')')
			else
				call em_line(em, lhs//' = '//lhs//' // '//rhs)
			end if
		else
			call em_unsupported(em, 'the `'//node%op%text//'` operator')
		end if
		return
	end if

	if (.not. compound) then
		call em_line(em, lhs//' = '//unparen(convert(rhs, rt, lt)))
		return
	end if

	! Compound assignment.  Do the operation in the wider of the two types, like
	! the equivalent binary operation does, then store in the variable's type
	ct = lt
	if (is_numeric_type(lt) .and. is_numeric_type(rt)) ct = wider_type(lt, rt)

	op_str = ''
	fn_str = ''

	select case (node%op%kind)
	case (plus_equals_token)
		op_str = ' + '
	case (minus_equals_token)
		op_str = ' - '
	case (star_equals_token)
		op_str = ' * '
	case (slash_equals_token)
		op_str = ' / '
	case (sstar_equals_token)
		op_str = ' ** '
	case (percent_equals_token)
		fn_str = 'mod'
	case (amp_equals_token)
		fn_str = 'iand'
	case (pipe_equals_token)
		fn_str = 'ior'
	case (caret_equals_token)
		fn_str = 'ieor'
	case (lless_equals_token)
		fn_str = 'shiftl'
	case (ggreater_equals_token)
		fn_str = 'shiftr'
	case default
		call em_unsupported(em, 'the `'//node%op%text//'` operator')
		return
	end select

	if (node%op%kind == lless_equals_token .or. node%op%kind == ggreater_equals_token) then
		! Shifted in the width of the target.  The count can have any integer
		! type
		call em_line(em, lhs//' = '//fn_str//'('//lhs//', '//rhs//')')

	else if (len(fn_str) > 0) then
		call em_line(em, lhs//' = '//convert(fn_str//'('//convert(lhs, lt, ct)// &
			', '//convert(rhs, rt, ct)//')', ct, lt))

	else
		call em_line(em, lhs//' = '//unparen(convert('('//convert(lhs, lt, ct)// &
			op_str//convert(rhs, rt, ct)//')', ct, lt)))
	end if

end subroutine emit_assign

!===============================================================================

recursive function has_loopless_break(node) result(found)

	! Does this statement have a `break` that isn't inside of a loop?  Such a
	! break leaves the outermost block.  Loops are not searched, because their
	! breaks leave the loop instead

	type(syntax_node_t), intent(in) :: node
	logical :: found

	integer :: i

	found = .false.

	select case (node%kind)
	case (break_statement)
		found = .true.

	case (block_statement)
		if (allocated(node%members)) then
			do i = 1, size(node%members)
				if (has_loopless_break(node%members(i))) then
					found = .true.
					return
				end if
			end do
		end if

	case (if_statement)
		if (allocated(node%if_clause)) found = has_loopless_break(node%if_clause)
		if (found) return
		if (allocated(node%else_clause)) found = has_loopless_break(node%else_clause)

	case (switch_statement)
		! The arms of a switch behave like the clauses of an `if`
		if (allocated(node%members)) then
			do i = 1, size(node%members)
				if (.not. allocated(node%members(i)%body)) cycle
				if (has_loopless_break(node%members(i)%body)) then
					found = .true.
					return
				end if
			end do
		end if
		if (allocated(node%else_clause)) found = has_loopless_break(node%else_clause)

	end select

end function has_loopless_break

!===============================================================================

recursive subroutine emit_block(em, node, is_result)

	! The statements of a block.  If is_result, the last one's value is the
	! program's result.
	!
	! A `break` outside of a loop jumps to the end of the outermost block.  That
	! one is wrapped in a named Fortran `block` construct to `exit` from

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	character(len = :), allocatable :: label, saved_label

	integer :: i, n

	logical :: wrap

	wrap = .false.
	if (em%loop_depth == 0) then
		wrap = .true.
		if (allocated(em%break_label)) wrap = len(em%break_label) == 0
		if (wrap) wrap = has_loopless_break(node)
	end if

	if (wrap) then
		em%tmp_count = em%tmp_count + 1
		label = 'brk_t'//str(em%tmp_count)

		if (allocated(em%break_label)) then
			saved_label = em%break_label
		else
			saved_label = ''
		end if
		em%break_label = label

		call em_line(em, label//': block')
		em%indent = em%indent + 1
	end if

	n = 0
	if (allocated(node%members)) n = size(node%members)

	if (is_result .and. n == 0) then
		call emit_result_invalid(em)
	else
		do i = 1, n
			if (is_result .and. i == n) then
				call emit_result_stmt(em, node%members(i))
			else
				call emit_stmt(em, node%members(i))
			end if
		end do
	end if

	if (wrap) then
		em%indent = em%indent - 1
		call em_line(em, 'end block '//label)
		em%break_label = saved_label
	end if

end subroutine emit_block

!===============================================================================

recursive subroutine emit_if(em, node, is_result)

	! if / else if / else chain.  If is_result, the value of the taken branch's
	! last statement is the program's result

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	call em_line(em, 'if ('//unparen(emit_expr(em, node%condition))//') then')

	call emit_branch(node%if_clause)

	! Flatten `else if` chains instead of nesting each one
	if (allocated(node%else_clause)) then
		call emit_else(node%else_clause)
	else if (is_result) then
		call em_line(em, 'else')
		em%indent = em%indent + 1
		call emit_result_invalid(em)
		em%indent = em%indent - 1
	end if

	call em_line(em, 'end if')

contains

	recursive subroutine emit_branch(clause)
		type(syntax_node_t), intent(in) :: clause
		em%indent = em%indent + 1
		if (is_result) then
			call emit_result_stmt(em, clause)
		else
			call emit_stmt(em, clause)
		end if
		em%indent = em%indent - 1
	end subroutine emit_branch

	recursive subroutine emit_else(clause)
		type(syntax_node_t), intent(in) :: clause
		character(len = :), allocatable :: cond
		if (clause%kind == if_statement) then
			! This condition isn't reached on every pass through the statement, so
			! nothing can be hoisted ahead of the whole `if`
			em%in_cond = .true.
			cond = emit_expr(em, clause%condition)
			em%in_cond = .false.
			call em_line(em, 'else if ('//unparen(cond)//') then')
			call emit_branch(clause%if_clause)
			if (allocated(clause%else_clause)) then
				call emit_else(clause%else_clause)
			else if (is_result) then
				call em_line(em, 'else')
				em%indent = em%indent + 1
				call emit_result_invalid(em)
				em%indent = em%indent - 1
			end if
		else
			call em_line(em, 'else')
			call emit_branch(clause)
		end if
	end subroutine emit_else

end subroutine emit_if

!===============================================================================

recursive function is_pure_case(node) result(pure_)

	! Can the match value (or range) of a `case` be evaluated whether or not an
	! earlier value of its arm matched, because it has no side effects and nothing
	! has to be hoisted out of it?  Otherwise the values have to be tested in
	! order, stopping at the first match, like the interpreter does

	type(syntax_node_t), intent(in) :: node
	logical :: pure_

	! An array has to be stored in a hidden variable first
	if (node%kind /= case_range) then
		if (is_arr(node%val)) then
			pure_ = .false.
			return
		end if
	end if

	select case (node%kind)
	case (literal_expr)
		pure_ = .true.
	case (name_expr)
		pure_ = .not. allocated(node%lsubscripts)
	case (unary_expr)
		pure_ = .false.
		if (allocated(node%right)) pure_ = is_pure_case(node%right)
	case (case_range)
		pure_ = is_pure_case(node%lbound_) .and. is_pure_case(node%ubound_)
	case default
		pure_ = .false.
	end select

end function is_pure_case

!===============================================================================

recursive function case_cmp(em, subj, sval, v, op) result(s)

	! Fortran expression for `<subject> == v` if `op` is eequals_token, or for
	! `<subject> < v` if it is less_token.  `subj` is the hidden variable that the
	! subject was stored in and `sval` is its value, for the type

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: subj
	type(value_t), intent(in) :: sval
	type(syntax_node_t), intent(in) :: v
	integer, intent(in) :: op
	character(len = :), allocatable :: s

	!********

	character(len = :), allocatable :: r, tmp

	integer :: st, vt, ct

	r  = emit_expr(em, v)
	st = elem_type(sval)
	vt = elem_type(v%val)

	if (is_arr(sval)) then
		! Whole-array equality.  Ranges of arrays aren't allowed by the parser.  The
		! value is stored in a hidden variable first, so that its shape and its
		! elements can both be read from it
		tmp = new_tmp_of(em, v%val, 'cv')
		call em_line(em, tmp//' = '//unparen(r))

		ct = st
		if (is_numeric_type(st) .and. is_numeric_type(vt)) ct = wider_type(st, vt)

		s = 'rt_arr_eq(shape('//subj//'), '// &
			convert('reshape('//subj//', [size('//subj//')])', st, ct)//', '// &
			'shape('//tmp//'), '// &
			convert('reshape('//tmp//', [size('//tmp//')])', vt, ct)//')'

	else if (st == str_type) then
		if (op == less_token) then
			s = 'rt_str_lt('//subj//', '//r//')'
		else
			s = 'rt_str_eq('//subj//', '//r//')'
		end if

	else if (st == bool_type) then
		s = '('//subj//' .eqv. '//r//')'

	else if (is_numeric_type(st) .and. is_numeric_type(vt)) then
		ct = wider_type(st, vt)
		if (op == less_token) then
			s = '('//convert(subj, st, ct)//' < '//convert(r, vt, ct)//')'
		else
			s = '('//convert(subj, st, ct)//' == '//convert(r, vt, ct)//')'
		end if

	else
		call em_unsupported(em, 'a `switch` on type `'//kind_name(st)//'`')
		s = '.false.'

	end if

end function case_cmp

!===============================================================================

recursive function case_test(em, subj, sval, v) result(s)

	! Fortran condition for whether a case value or range matches the subject.
	! A range is half-open like the interpreter's: `lo <= subj < hi`, tested as
	! "not (subj < lo) and subj < hi"

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: subj
	type(value_t), intent(in) :: sval
	type(syntax_node_t), intent(in) :: v
	character(len = :), allocatable :: s

	if (v%kind == case_range) then
		s = '(.not. '//case_cmp(em, subj, sval, v%lbound_, less_token)//' .and. '// &
			case_cmp(em, subj, sval, v%ubound_, less_token)//')'
	else
		s = case_cmp(em, subj, sval, v, eequals_token)
	end if

end function case_test

!===============================================================================

recursive subroutine emit_switch(em, node, is_result)

	! A switch statement is a chain of arms inside a named Fortran `block`.  The
	! subject is stored once in a hidden variable.  An arm whose value (or one of
	! its values) matches and whose guard holds runs its body and then leaves
	! the block, so that a guard which is false falls through to the next arm, like
	! the interpreter's.  The `default` arm is last, and its position in the
	! source doesn't matter
	!
	!   sw_t3 = <subject>
	!   sw_t3_blk: block
	!       if (<match 1>) then
	!           if (<guard>) then
	!               <body>
	!               exit sw_t3_blk
	!           end if
	!       end if
	!       ...
	!       <default body>
	!   end block sw_t3_blk
	!
	! The values of an arm are tested in order, and only up to the first that
	! matches, because a value can be a call with side effects.  An arm whose
	! values can't have any is tested with a single condition
	!
	! If is_result, the value of the body that ran is the program's result, and a
	! switch that matched nothing has none

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node
	logical, intent(in) :: is_result

	!********

	character(len = :), allocatable :: label, subj, matched, test

	integer :: i, j, nelems, narms, depth

	logical :: all_pure, has_guard

	em%tmp_count = em%tmp_count + 1
	label = 'sw_blk'//str(em%tmp_count)

	test = emit_expr(em, node%condition)
	subj = new_tmp_of(em, node%condition%val, 'sw')
	call em_line(em, subj//' = '//unparen(test))

	call em_line(em, label//': block')
	em%indent = em%indent + 1

	narms = 0
	if (allocated(node%members)) narms = size(node%members)

	do i = 1, narms

		depth = 0

		nelems = 0
		if (allocated(node%members(i)%elems)) nelems = size(node%members(i)%elems)

		if (nelems > 0) then

			all_pure = .true.
			do j = 1, nelems
				if (.not. is_pure_case(node%members(i)%elems(j))) all_pure = .false.
			end do

			if (all_pure) then
				test = ''
				do j = 1, nelems
					if (j > 1) test = test//' .or. '
					test = test//case_test(em, subj, node%condition%val, &
						node%members(i)%elems(j))
				end do
				call em_line(em, 'if ('//unparen(test)//') then')
				em%indent = em%indent + 1
				depth = depth + 1

			else
				! Stop at the first value that matches
				matched = new_tmp(em, 'logical', 'm')
				call em_line(em, matched//' = .false.')

				do j = 1, nelems
					call em_line(em, 'if (.not. '//matched//') then')
					em%indent = em%indent + 1

					associate (v => node%members(i)%elems(j))
						if (v%kind == case_range) then
							! The upper bound isn't evaluated for a subject below the
							! lower bound
							test = case_cmp(em, subj, node%condition%val, v%lbound_, &
								less_token)
							call em_line(em, 'if (.not. '//test//') then')
							em%indent = em%indent + 1
							test = case_cmp(em, subj, node%condition%val, v%ubound_, &
								less_token)
							call em_line(em, matched//' = '//unparen(test))
							em%indent = em%indent - 1
							call em_line(em, 'end if')
						else
							test = case_cmp(em, subj, node%condition%val, v, eequals_token)
							call em_line(em, matched//' = '//unparen(test))
						end if
					end associate

					em%indent = em%indent - 1
					call em_line(em, 'end if')
				end do

				call em_line(em, 'if ('//matched//') then')
				em%indent = em%indent + 1
				depth = depth + 1

			end if
		end if

		! The guard is only evaluated once a value has matched, or at once for an
		! arm that is nothing but a guard
		has_guard = allocated(node%members(i)%condition)
		if (has_guard) then
			test = emit_expr(em, node%members(i)%condition)
			call em_line(em, 'if ('//unparen(test)//') then')
			em%indent = em%indent + 1
			depth = depth + 1
		end if

		call emit_arm_body(node%members(i)%body)
		call em_line(em, 'exit '//label)

		do j = 1, depth
			em%indent = em%indent - 1
			call em_line(em, 'end if')
		end do

	end do

	if (allocated(node%else_clause)) then
		call emit_arm_body(node%else_clause)
	else if (is_result) then
		call emit_result_invalid(em)
	end if

	em%indent = em%indent - 1
	call em_line(em, 'end block '//label)

contains

	recursive subroutine emit_arm_body(body)
		type(syntax_node_t), intent(in) :: body
		if (is_result) then
			call emit_result_stmt(em, body)
		else
			call emit_stmt(em, body)
		end if
	end subroutine emit_arm_body

end subroutine emit_switch

!===============================================================================

recursive subroutine emit_while(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	character(len = :), allocatable :: cond

	em%in_cond = .true.
	cond = emit_expr(em, node%condition)
	em%in_cond = .false.

	call em_line(em, 'do while ('//unparen(cond)//')')
	em%indent = em%indent + 1
	em%loop_depth = em%loop_depth + 1
	call emit_stmt(em, node%body)
	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1
	call em_line(em, 'end do')

end subroutine emit_while

!===============================================================================

recursive subroutine emit_for(em, node)

	! syntran evaluates the iterable once on entering the loop, and the body may
	! assign to the loop variable without affecting the iteration.  So the loop
	! is driven by a hidden counter, and the variable is assigned from it
	!
	! An integer range is a Fortran do loop of its own.  Anything else, i.e. an
	! array or a string, is first stored in a temporary which is then iterated

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: it, tmp, len_str, elem_str

	type(value_t) :: var_val

	if (node%array%kind == array_expr) then
		if (node%array%val%array%kind == bound_array .or. &
				node%array%val%array%kind == step_array) then
			if (any(node%array%val%array%type == [i32_type, i64_type])) then
				call emit_for_range(em, node)
				return
			end if
		end if
	end if

	!********
	! A string or array

	if (node%array%val%type == str_type) then

		tmp = new_tmp(em, 'character(len = :), allocatable', 'str')
		call em_line(em, tmp//' = '//emit_expr(em, node%array))
		var_val%type = str_type
		len_str = 'len('//tmp//')'
		elem_str = tmp//'(KK:KK)'

	else if (node%array%val%type == array_type) then

		if (node%array%val%array%rank /= 1) then
			call em_unsupported(em, 'a `for` loop over an array that is not rank 1')
			return
		end if

		tmp = new_tmp_of(em, node%array%val, 'arr')
		call em_line(em, tmp//' = '//emit_expr(em, node%array))

		var_val%type = node%array%val%array%type
		len_str = 'size('//tmp//')'
		if (var_val%type == str_type) then
			elem_str = tmp//'(KK)%s'
		else
			elem_str = tmp//'(KK)'
		end if

	else
		call em_unsupported(em, 'a `for` loop over a value of type `'// &
			kind_name(node%array%val%type)//'`')
		return
	end if

	it = new_tmp(em, type_spec(i32_type), 'it')

	call declare_var(em, node, var_val)

	call em_line(em, 'do '//it//' = 1, '//len_str)

	em%indent = em%indent + 1
	em%loop_depth = em%loop_depth + 1

	! Substitute the counter into the element designator
	call em_line(em, var_name(node)//' = '//replace_kk(elem_str, it))
	call emit_stmt(em, node%body)

	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1

	call em_line(em, 'end do')

end subroutine emit_for

!===============================================================================

function replace_kk(s, it) result(r)

	! Replace each `KK` placeholder in s by the loop counter's name

	character(len = *), intent(in) :: s, it
	character(len = :), allocatable :: r

	integer :: i

	r = ''
	i = 1
	do while (i <= len(s))
		if (i < len(s)) then
			if (s(i:i+1) == 'KK') then
				r = r//it
				i = i + 2
				cycle
			end if
		end if
		r = r//s(i:i)
		i = i + 1
	end do

end function replace_kk

!===============================================================================

recursive subroutine emit_for_range(em, node)

	! A for loop over an integer range `[a: b]` or `[a: step: b]` is a Fortran
	! do loop.  Its bounds are evaluated once on entry, just like syntran's

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	character(len = :), allocatable :: it, lb, ub, step, tmp
	character(len = 2) :: kindstr

	integer :: elem

	type(value_t) :: var_val

	elem = node%array%val%array%type
	kindstr = merge('32', '64', elem == i32_type)

	associate (arr => node%array)

		lb = convert(emit_expr(em, arr%lbound_), elem_type(arr%lbound_%val), elem)
		ub = convert(emit_expr(em, arr%ubound_), elem_type(arr%ubound_%val), elem)

		it = new_tmp(em, type_spec(elem), 'it')

		if (arr%val%array%kind == step_array) then
			step = convert(emit_expr(em, arr%step), elem_type(arr%step%val), elem)
			if (.not. is_simple(arr%step)) then
				! Used twice below
				tmp = new_tmp(em, type_spec(elem), 'step')
				call em_line(em, tmp//' = '//step)
				step = tmp
			end if
			! The upper bound is exclusive, in either direction
			call em_line(em, 'do '//it//' = '//lb//', '//ub//' - sign(1_int'// &
				kindstr//', '//step//'), '//step)
		else
			call em_line(em, 'do '//it//' = '//lb//', '//ub//' - 1')
		end if

	end associate

	var_val%type = elem
	call declare_var(em, node, var_val)

	em%indent = em%indent + 1
	em%loop_depth = em%loop_depth + 1
	call em_line(em, var_name(node)//' = '//it)
	call emit_stmt(em, node%body)
	em%loop_depth = em%loop_depth - 1
	em%indent = em%indent - 1

	call em_line(em, 'end do')

end subroutine emit_for_range

!===============================================================================

recursive subroutine emit_return(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	logical :: is_void

	is_void = node%right%val%type == void_type

	if (em%top_level) then
		! Returning from the program prints the returned value, which is what the
		! interpreter does with it
		if (is_void) then
			call emit_result_invalid(em)
		else if (em%print_result) then
			call emit_result_str(em, str_of(node%right%val, emit_expr(em, node%right)))
		else
			call emit_discard(em, node%right)
		end if
		call em_line(em, 'return')

	else
		if (.not. is_void) then
			call em_line(em, 'res_ = '//unparen(emit_expr(em, node%right)))
		end if
		call em_line(em, 'return')

	end if

end subroutine emit_return

!===============================================================================

recursive subroutine emit_discard(em, node)

	! Evaluate an expression only for its side effects

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	select case (node%kind)
	case (fn_call_expr, fn_call_intr_expr)
		call emit_call_stmt(em, node)
	case default
		! A pure expression has no side effects
	end select

end subroutine emit_discard

!===============================================================================

recursive subroutine emit_result_stmt(em, node)

	! Emit a statement such that the program's result is the value of the last
	! statement that gets executed.  This is what the interpreter does.  It
	! recurses into the branch of an `if` and into the end of a block.  A loop, a
	! bare `return`, an `if` that doesn't run, or a void call has no value

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
	end if

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .true.)

	case (if_statement)
		call emit_if(em, node, .true.)

	case (while_statement, for_statement, break_statement, continue_statement)
		call emit_stmt(em, node)
		call emit_result_invalid(em)

	case (return_statement)
		call emit_stmt(em, node)

	case (switch_statement)
		call emit_switch(em, node, .true.)

	case (struct_declaration, enum_declaration)
		call emit_stmt(em, node)

	case (use_statement)
		! Its value is nothing
		call emit_stmt(em, node)
		call emit_result_invalid(em)

	case (let_expr)
		call emit_stmt(em, node)
		call emit_result_str(em, str_of(node%val, var_name(node)))

	case (assignment_expr)
		call emit_stmt(em, node)
		if (allocated(node%member)) then
			! Already reported as unsupported by emit_stmt()
		else
			! The target, e.g. a whole array or just the element that was assigned
			call emit_result_str(em, str_of(node%val, emit_name_ref(em, node)))
		end if

	case (fn_call_expr, fn_call_intr_expr)
		if (node%val%type == void_type) then
			call emit_call_stmt(em, node)
			! println() has an empty result while other void fns have none.  exit()
			! never gets to have one
			if (node%kind == fn_call_intr_expr) then
				if (node%identifier%text /= 'exit') call emit_result_str(em, "''")
			else
				call emit_result_invalid(em)
			end if
		else
			call emit_result_expr(em, node)
		end if

	case default
		call emit_result_expr(em, node)

	end select

end subroutine emit_result_stmt

!===============================================================================

recursive subroutine emit_result_expr(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	if (node%val%type == unknown_type .or. node%val%type == void_type) then
		call emit_result_invalid(em)
	else if (em%print_result) then
		call emit_result_str(em, str_of(node%val, emit_expr(em, node)))
	else
		call emit_discard(em, node)
	end if

end subroutine emit_result_expr

!===============================================================================

recursive subroutine emit_use(em, node)

	! A module's own top-level statements, like `let count = 0;`, run where it is
	! imported.  Its fns are emitted with the rest of them (c.f. emit_module_fns()).
	! The parser numbered the module's variables and fns together with the
	! importer's, so they are named like any other global and need no mapping

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	integer :: i

	if (.not. allocated(node%member)) return
	if (.not. allocated(node%member%members)) return

	do i = 1, size(node%member%members)
		if (node%member%members(i)%kind == fn_declaration) cycle
		call emit_stmt(em, node%member%members(i))
	end do

end subroutine emit_use

!===============================================================================

recursive subroutine emit_module_fns(em, unit, done)

	! Emit the fns of an imported module, and of the modules that it imports.
	! `done` has the ids of the fns that are already emitted, which a module that
	! is imported from two places isn't to do again

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: unit
	type(integer_vector_t), intent(inout) :: done

	integer :: i, j
	logical :: found

	if (.not. allocated(unit%members)) return

	do i = 1, size(unit%members)
		if (unit%members(i)%kind /= use_statement) cycle
		if (.not. allocated(unit%members(i)%member)) cycle
		call emit_module_fns(em, unit%members(i)%member, done)
	end do

	do i = 1, size(unit%members)
		if (unit%members(i)%kind /= fn_declaration) cycle

		found = .false.
		do j = 1, done%len_
			if (done%v(j) == unit%members(i)%id_index) found = .true.
		end do
		if (found) cycle

		call done%push(unit%members(i)%id_index)
		call emit_fn(em, unit%members(i))
	end do

end subroutine emit_module_fns

!===============================================================================

recursive module subroutine emit_stmt(em, node)

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: node

	!********

	if (node%src_id > 0) then
		em%cur_src_id  = node%src_id
		em%cur_src_pos = node%src_pos
	end if

	if (node%is_empty) return

	select case (node%kind)

	case (block_statement)
		call emit_block(em, node, .false.)

	case (let_expr)
		call emit_let(em, node)

	case (assignment_expr)
		call emit_assign(em, node)

	case (if_statement)
		call emit_if(em, node, .false.)

	case (while_statement)
		call emit_while(em, node)

	case (for_statement)
		call emit_for(em, node)

	case (break_statement)
		if (em%loop_depth > 0) then
			call em_line(em, 'exit')
		else if (allocated(em%break_label)) then
			if (len(em%break_label) > 0) call em_line(em, 'exit '//em%break_label)
		end if
		! Otherwise there is nothing to break out of, which is a no-op

	case (continue_statement)
		if (em%loop_depth > 0) call em_line(em, 'cycle')

	case (return_statement)
		call emit_return(em, node)

	case (fn_call_expr, fn_call_intr_expr)
		call emit_call_stmt(em, node)

	case (fn_declaration)
		! Emitted separately, since a fn is its own Fortran procedure

	case (struct_declaration)
		call em_unsupported(em, 'a struct declaration')

	case (enum_declaration)
		call em_unsupported(em, 'an enum declaration')

	case (switch_statement)
		call emit_switch(em, node, .false.)

	case (use_statement)
		call emit_use(em, node)

	case default
		! A bare expression statement, which at most has side effects from calls
		call emit_discard(em, node)

	end select

end subroutine emit_stmt

!===============================================================================

subroutine push_wrapped(v, line)

	! Push a source line onto `v`, wrapped with Fortran continuation markers if
	! it is too long.  A line is only broken at a blank outside of a string
	! literal, which is only ever next to an operator or a comma in generated
	! code, so a break there can't change the meaning

	type(string_vector_t), intent(inout) :: v
	character(len = *), intent(in) :: line

	!********

	character(len = :), allocatable :: rest, pad

	integer :: i, k, lead, rest_lead
	logical :: in_quote

	lead = verify(line, ' ') - 1
	if (lead < 0) lead = 0

	rest = line
	pad  = repeat(' ', lead + 8)

	do while (len(rest) > MAX_LINE)

		! Where the text starts, after the indentation of the first line or the
		! padding of a continuation
		rest_lead = verify(rest, ' ') - 1
		if (rest_lead < 0) exit

		! Don't touch comments
		if (rest(rest_lead+1: rest_lead+1) == '!') exit

		k = 0
		in_quote = .false.
		do i = 1, MAX_LINE - 2
			if (rest(i:i) == "'") in_quote = .not. in_quote
			if (i > rest_lead + 1 .and. rest(i:i) == ' ' .and. .not. in_quote) k = i
		end do

		! Nothing to break at within the limit, e.g. a deep nest of parentheses
		! before the first operator.  Take the first blank after the limit
		if (k <= len(pad) + 1) then
			in_quote = .false.
			do i = 1, len(rest)
				if (rest(i:i) == "'") in_quote = .not. in_quote
				if (i > MAX_LINE - 2 .and. rest(i:i) == ' ' .and. .not. in_quote) then
					k = i
					exit
				end if
			end do
		end if

		! Nowhere to break it, so leave it long.  A break has to be past the
		! padding that a continuation gets, otherwise it would never get shorter
		if (k <= len(pad) + 1) exit

		call v%push(rest(1: k-1)//' &')
		rest = pad//rest(k+1:)
	end do

	call v%push(rest)

end subroutine push_wrapped

!===============================================================================

subroutine push_proc(em, header, footer, ret_decls)

	! Move the body and declarations that have been accumulated for one
	! procedure into em%procs, wrapped in its header and footer

	type(emitter_t), intent(inout) :: em
	character(len = *), intent(in) :: header, footer
	type(string_vector_t), intent(in) :: ret_decls

	integer :: i

	call push_wrapped(em%procs, '    '//header)
	do i = 1, ret_decls%len_
		call push_wrapped(em%procs, '        '//ret_decls%v(i)%s)
	end do
	do i = 1, em%decls%len_
		call push_wrapped(em%procs, '        '//em%decls%v(i)%s)
	end do
	do i = 1, em%body%len_
		call push_wrapped(em%procs, '    '//em%body%v(i)%s)
	end do
	call em%procs%push('    '//footer)
	call em%procs%push('')

end subroutine push_proc

!===============================================================================

subroutine get_children(node, kids, n)

	! Pointers to every child node of a node, whichever of its many optional
	! components they are in

	type(syntax_node_t), intent(in), target :: node
	type(node_ptr_t), allocatable, intent(out) :: kids(:)
	integer, intent(out) :: n

	integer :: i, cap

	cap = 12
	if (allocated(node%members)) cap = cap + size(node%members)
	if (allocated(node%elems)) cap = cap + size(node%elems)
	if (allocated(node%lsubscripts)) cap = cap + size(node%lsubscripts)
	if (allocated(node%size_)) cap = cap + size(node%size_)
	if (allocated(node%args)) cap = cap + size(node%args)
	if (allocated(node%usubscripts)) cap = cap + size(node%usubscripts)
	if (allocated(node%ssubscripts)) cap = cap + size(node%ssubscripts)
	allocate(kids(cap))
	n = 0

	if (allocated(node%left)) call add(node%left)
	if (allocated(node%right)) call add(node%right)
	if (allocated(node%condition)) call add(node%condition)
	if (allocated(node%if_clause)) call add(node%if_clause)
	if (allocated(node%else_clause)) call add(node%else_clause)
	if (allocated(node%body)) call add(node%body)
	if (allocated(node%array)) call add(node%array)
	if (allocated(node%member)) call add(node%member)
	if (allocated(node%lbound_)) call add(node%lbound_)
	if (allocated(node%step)) call add(node%step)
	if (allocated(node%ubound_)) call add(node%ubound_)
	if (allocated(node%len_)) call add(node%len_)

	if (allocated(node%members)) then
		do i = 1, size(node%members)
			call add(node%members(i))
		end do
	end if
	if (allocated(node%elems)) then
		do i = 1, size(node%elems)
			call add(node%elems(i))
		end do
	end if
	if (allocated(node%lsubscripts)) then
		do i = 1, size(node%lsubscripts)
			call add(node%lsubscripts(i))
		end do
	end if
	if (allocated(node%size_)) then
		do i = 1, size(node%size_)
			call add(node%size_(i))
		end do
	end if
	if (allocated(node%args)) then
		do i = 1, size(node%args)
			call add(node%args(i))
		end do
	end if
	if (allocated(node%usubscripts)) then
		do i = 1, size(node%usubscripts)
			call add(node%usubscripts(i))
		end do
	end if
	if (allocated(node%ssubscripts)) then
		do i = 1, size(node%ssubscripts)
			call add(node%ssubscripts(i))
		end do
	end if

contains

	subroutine add(child)
		type(syntax_node_t), intent(in), target :: child
		n = n + 1
		kids(n)%p => child
	end subroutine add

end subroutine get_children

!===============================================================================

recursive function modifies_slot(node, slot) result(found)

	! Is the local variable in `slot` assigned to anywhere in this tree, or
	! passed by reference to a fn which could assign to it?

	type(syntax_node_t), intent(in), target :: node
	integer, intent(in) :: slot
	logical :: found

	integer :: i, n
	type(node_ptr_t), allocatable :: kids(:)

	found = .false.

	select case (node%kind)

	case (assignment_expr)
		! The target is the node itself.  That includes assignment to an
		! element or slice of the variable
		if (node%is_loc .and. node%id_index == slot) then
			found = .true.
			return
		end if

	case (fn_call_expr, method_call_expr)
		! An argument passed by reference is another way for it to be assigned.
		! A `&const` parameter is only read, but we don't know that here, so
		! assume the worst
		if (allocated(node%is_ref) .and. allocated(node%args)) then
			do i = 1, size(node%args)
				if (.not. node%is_ref(i)) cycle
				if (node%args(i)%is_loc .and. node%args(i)%id_index == slot .and. &
						node%args(i)%kind == name_expr) then
					found = .true.
					return
				end if
			end do
		end if

	end select

	call get_children(node, kids, n)
	do i = 1, n
		if (modifies_slot(kids(i)%p, slot)) then
			found = .true.
			return
		end if
	end do

end function modifies_slot

!===============================================================================

recursive function alias_hazard(node, fn_id, ref_slots) result(found)

	! Could something in this tree change a variable that a by-value argument of
	! the fn `fn_id` is aliasing, while it is running?
	!
	! Fortran passes arguments by reference, so a dummy argument which isn't
	! copied is the caller's own variable.  That's only the same as passing by
	! value if nothing else assigns to that variable during the call, which could
	! be through a global, a by-reference parameter, or any fn that does either.
	! So this is conservative: any assignment to a global or by-reference
	! parameter counts, as does calling any user fn other than the fn itself

	type(syntax_node_t), intent(in), target :: node
	integer, intent(in) :: fn_id
	integer, intent(in) :: ref_slots(:)
	logical :: found

	integer :: i, n
	type(node_ptr_t), allocatable :: kids(:)

	found = .false.

	select case (node%kind)

	case (assignment_expr)
		if (.not. node%is_loc) then
			found = .true.
			return
		end if
		if (any(ref_slots == node%id_index)) then
			found = .true.
			return
		end if

	case (fn_call_expr, method_call_expr)
		if (node%id_index /= fn_id) then
			found = .true.
			return
		end if

		! The fn itself can also pass a global or by-reference parameter as
		! its own by-reference argument
		if (allocated(node%is_ref) .and. allocated(node%args)) then
			do i = 1, size(node%args)
				if (.not. node%is_ref(i)) cycle
				if (node%args(i)%kind /= name_expr) cycle
				if (.not. node%args(i)%is_loc .or. &
						any(ref_slots == node%args(i)%id_index)) then
					found = .true.
					return
				end if
			end do
		end if

	end select

	call get_children(node, kids, n)
	do i = 1, n
		if (alias_hazard(kids(i)%p, fn_id, ref_slots)) then
			found = .true.
			return
		end if
	end do

end function alias_hazard

!===============================================================================

function dims_of(rank_) result(s)

	! `(:,:)` for rank 2

	integer, intent(in) :: rank_
	character(len = :), allocatable :: s

	integer :: i

	s = '('
	do i = 1, rank_
		if (i > 1) s = s//','
		s = s//':'
	end do
	s = s//')'

end function dims_of

!===============================================================================

function elem_spec(val) result(s)

	! Type spec of a scalar's type, or of an array's elements, for a dummy
	! argument

	type(value_t), intent(in) :: val
	character(len = :), allocatable :: s

	integer :: t

	t = elem_type(val)
	if (t == str_type) then
		if (is_arr(val)) then
			s = 'type(rt_str_t)'
		else
			s = 'character(len = *)'
		end if
	else
		s = type_spec(t)
	end if

end function elem_spec

!===============================================================================

recursive subroutine emit_fn(em, decl)

	! Emit a user fn as a Fortran procedure.  It is always `recursive`, which
	! also makes all of its local variables automatic (not `save`)
	!
	! By-value parameters are copied into a local at entry, so that the body is
	! free to assign to them.  By-reference parameters are `intent(inout)`, and
	! allocatable if they are arrays or strings so that they can be resized

	type(emitter_t), intent(inout) :: em
	type(syntax_node_t), intent(in) :: decl

	!********

	character(len = :), allocatable :: name, header, dummy_names, pname, local, dims, line

	integer :: i, slot

	integer, allocatable :: ref_slots(:)

	logical :: is_void, ok, is_const, use_direct

	type(fn_t), pointer :: fn
	type(string_vector_t) :: ret_decls, entry_lines
	type(syntax_node_t) :: pnode

	if (decl%src_id > 0) then
		em%cur_src_id  = decl%src_id
		em%cur_src_pos = decl%src_pos
	end if

	fn => em%fns%fns(decl%id_index)

	! Fresh procedure state
	em%body  = new_string_vector()
	em%decls = new_string_vector()
	em%seen_local = new_integer_vector()
	if (allocated(em%local_slots)) deallocate(em%local_slots)
	em%top_level = .false.
	em%loop_depth = 0
	em%break_label = ''
	em%indent = 1

	ret_decls   = new_string_vector()
	entry_lines = new_string_vector()

	is_void = fn%type%type == void_type

	name = fn_name(decl%identifier%text, decl%id_index)

	! Slots of the parameters passed by reference, which the body can assign to
	allocate(ref_slots(0))
	if (allocated(decl%params) .and. allocated(decl%is_ref)) then
		do i = 1, size(decl%params)
			if (decl%is_ref(i)) ref_slots = [ref_slots, decl%params(i)]
		end do
	end if

	dummy_names = ''
	if (allocated(decl%params)) then
		do i = 1, size(decl%params)
			slot  = decl%params(i)
			pname = fn%param_names%v(i)%s

			! A node that stands for the parameter, to name and declare it
			pnode%is_loc = .true.
			pnode%id_index = slot
			pnode%identifier%text = pname

			local = var_name(pnode)

			dims = ''
			if (is_arr(fn%params(i))) dims = dims_of(fn%params(i)%array%rank)

			if (len(dummy_names) > 0) dummy_names = dummy_names//', '

			is_const = .false.
			if (allocated(decl%is_const_ref)) is_const = decl%is_const_ref(i)

			if (is_const) then
				! `&const`: passed by reference but read-only.  The dummy argument
				! is the variable itself, with no copy of it needed
				dummy_names = dummy_names//local
				call ret_decls%push(elem_spec(fn%params(i))//', intent(in) :: '// &
					local//dims)
				call em%seen_local%push(2 * slot + 1)
				call record_slot(em, .true., slot, fn%params(i))
				cycle
			end if

			if (allocated(decl%is_ref)) then
				if (decl%is_ref(i)) then
					! Passed by reference: the dummy argument is the variable
					dummy_names = dummy_names//local
					line = decl_line(fn%params(i), local, ok)
					if (.not. ok) then
						call em_unsupported(em, 'a parameter of type `'// &
							kind_name(fn%params(i)%type)//'`')
					else
						! Declare as an intent(inout) dummy: insert the intent
						! after the type spec
						line = insert_intent(line, 'intent(inout)')
						call ret_decls%push(line)
					end if
					call em%seen_local%push(2 * slot + 1)
					call record_slot(em, .true., slot, fn%params(i))
					cycle
				end if
			end if

			use_direct = .false.
			if (is_arr(fn%params(i)) .or. fn%params(i)%type == str_type) then
				! Copying a scalar is free.  An array or string is only worth not
				! copying if it can't be affected by anything else
				use_direct = .not. modifies_slot(decl%body, slot)
				if (use_direct) use_direct = .not. alias_hazard(decl%body, &
					decl%id_index, ref_slots)
			end if

			if (use_direct) then
				! Passed by value, and never assigned to, so the dummy argument can
				! be used directly instead of copying it
				dummy_names = dummy_names//local
				call ret_decls%push(elem_spec(fn%params(i))//', intent(in) :: '// &
					local//dims)
				call em%seen_local%push(2 * slot + 1)
				call record_slot(em, .true., slot, fn%params(i))
				cycle
			end if

			! Passed by value, and assigned to: a read-only dummy argument, copied
			! into the local
			dummy_names = dummy_names//local//'_a'
			call ret_decls%push(elem_spec(fn%params(i))//', intent(in) :: '// &
				local//'_a'//dims)
			call declare_var(em, pnode, fn%params(i))
			call entry_lines%push(local//' = '//local//'_a')
		end do
	end if

	do i = 1, entry_lines%len_
		call em_line(em, entry_lines%v(i)%s)
	end do

	if (is_void) then
		header = 'recursive subroutine '//name//'('//dummy_names//')'
	else
		header = 'recursive function '//name//'('//dummy_names//') result(res_)'

		line = decl_line(fn%type, 'res_', ok)
		if (.not. ok) then
			call em_unsupported(em, 'a fn that returns type `'//kind_name(fn%type%type)//'`')
		else
			call ret_decls%push(line)
		end if
	end if

	call emit_stmt(em, decl%body)

	if (is_void) then
		call push_proc(em, header, 'end subroutine '//name, ret_decls)
	else
		call push_proc(em, header, 'end function '//name, ret_decls)
	end if

	em%top_level = .true.

end subroutine emit_fn

!===============================================================================

function insert_intent(line, intent_str) result(r)

	! Add an attribute to a declaration like `real(real32) :: x` or
	! `integer(int32), allocatable :: x(:)`, before its `::`

	character(len = *), intent(in) :: line, intent_str
	character(len = :), allocatable :: r

	integer :: i

	i = index(line, ' :: ')
	if (i == 0) then
		r = line
	else
		r = line(1: i-1)//', '//intent_str//line(i:)
	end if

end function insert_intent

!===============================================================================

module subroutine transpile_tree(tree, state, t, diags)

	type(syntax_node_t), intent(in) :: tree
	type(state_t), intent(in), target :: state
	type(transpile_t), intent(inout) :: t
	type(string_vector_t), intent(out) :: diags

	!********

	integer :: i, last
	logical :: no_diags

	type(emitter_t) :: em
	type(integer_vector_t) :: done
	type(string_vector_t) :: rt, main_decls, src

	em%fns => state%fns
	em%print_result = t%print_result
	em%trim_result  = t%trim_result

	em%body  = new_string_vector()
	em%decls = new_string_vector()
	em%gdecls = new_string_vector()
	em%procs = new_string_vector()
	em%diags = new_string_vector()
	em%seen_local  = new_integer_vector()
	em%seen_global = new_integer_vector()
	em%break_label = ''

	if (tree%kind /= translation_unit .or. .not. allocated(tree%members)) then
		call em_unsupported(em, 'this kind of input')
	end if

	! Top-level statements go in the main procedure.  The value of the last one is
	! the program's result.  This comes before the fns so that the types of the
	! global variables, which fns can use, are known by then
	em%top_level = .true.
	em%indent = 1

	last = 0
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			! Declarations aren't statements that run, so they have no value
			select case (tree%members(i)%kind)
			case (fn_declaration, struct_declaration, enum_declaration)
			case default
				last = i
			end select
		end do
	end if

	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind == fn_declaration) cycle
			if (i == last) then
				call emit_result_stmt(em, tree%members(i))
			else
				call emit_stmt(em, tree%members(i))
			end if
		end do
	end if
	if (last == 0) call emit_result_invalid(em)

	main_decls = new_string_vector()
	call push_proc(em, 'subroutine syntran_main()', 'end subroutine syntran_main', main_decls)

	! Fns have their own state, so they don't interleave with the above.  Those of
	! the modules come with their own diagnostics, in the module's source
	done = new_integer_vector()
	if (allocated(tree%members)) then
		do i = 1, size(tree%members)
			if (tree%members(i)%kind /= fn_declaration) cycle
			call done%push(tree%members(i)%id_index)
			call emit_fn(em, tree%members(i))
		end do
		do i = 1, size(tree%members)
			if (tree%members(i)%kind /= use_statement) cycle
			if (.not. allocated(tree%members(i)%member)) cycle
			call emit_module_fns(em, tree%members(i)%member, done)
		end do
	end if

	!********
	! Assemble the program

	diags = em%diags
	no_diags = diags%len_ == 0

	src = new_string_vector()

	rt = transpile_rt_src()
	call src%push_all(rt)

	call src%push('')
	call src%push('!'//repeat('=', 79))
	call src%push('')
	call src%push('! Generated by syntran --transpile')
	call src%push('')
	call src%push('module syntran_prog')
	call src%push('')
	call src%push('    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64')
	call src%push('    use syntran_rt')
	call src%push('    implicit none')
	call src%push('')
	do i = 1, em%gdecls%len_
		call push_wrapped(src, '    '//em%gdecls%v(i)%s)
	end do
	if (em%gdecls%len_ > 0) call src%push('')
	call src%push('contains')
	call src%push('')
	call src%push_all(em%procs)
	call src%push('end module syntran_prog')
	call src%push('')
	call src%push('program syntran_program')
	call src%push('    use syntran_prog, only: syntran_main')
	call src%push('    implicit none')
	call src%push('    call syntran_main()')
	call src%push('end program syntran_program')
	call src%push('')

	if (no_diags) t%src = src

end subroutine transpile_tree

!===============================================================================

end submodule syntran__transpile_stmt

!===============================================================================

