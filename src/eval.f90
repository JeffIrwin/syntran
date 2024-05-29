
!===============================================================================

module syntran__eval_m

	use iso_fortran_env

	use syntran__bool_m
	use syntran__math_m
	use syntran__types_m

	implicit none

	!********

	type state_t
		! Run time state

		logical :: quiet

		type(fns_t) :: fns

		type(vars_t) :: vars

		! Function nesting index.  Each function call increments, each return
		! decrements
		!
		! TODO: does returned need to be an array?  I think we can just use one
		! scalar
		integer :: ifn
		type(logical_vector_t) :: returned

	end type state_t

!===============================================================================

contains

!===============================================================================

recursive function syntax_eval(node, state) result(res)

	! TODO: add diagnostics to state for runtime errors (bounds overflow, rank
	! mismatch, etc.)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	!print *, 'starting syntax_eval()'

	! if_statement and while_statement may return an uninitialized type
	! otherwise if their conditions are false
	!
	! TODO: setting here should be unnecessary now that type is initialized
	! inside the value_t declaration
	res%type = unknown_type

	if (node%is_empty) then
		!print *, 'returning'
		return
	end if

	!********

	! I'm being a bit loose with consistency on select case indentation but
	! I don't want a gigantic diff

	select case (node%kind)

	case (literal_expr)
		res = node%val  ! this handles ints, bools, etc.

	case (array_expr)
		res = eval_array_expr(node, state)

	case (for_statement)
		res = eval_for_statement(node, state)

	case (while_statement)
		res = eval_while_statement(node, state)

	case (if_statement)
		res = eval_if_statement(node, state)

	case (return_statement)
		res = eval_return_statement(node, state)

	case (translation_unit)
		res = eval_translation_unit(node, state)

	case (block_statement)
		res = eval_block_statement(node, state)

	case (assignment_expr)
		res = eval_assignment_expr(node, state)

	case (let_expr)

		! Assign return value
		res = syntax_eval(node%right, state)

		!print *, 'assigning identifier ', quote(node%identifier%text)
		state%vars%vals(node%id_index) = res

	case (fn_call_expr)
		res = eval_fn_call(node, state)

	case (name_expr)
		res = eval_name_expr(node, state)

	case (unary_expr)
		res = eval_unary_expr(node, state)

	case (binary_expr)
		res = eval_binary_expr(node, state)

	case default
		write(*,*) err_eval_node(kind_name(node%kind))
		call internal_error()

	end select

end function syntax_eval

!===============================================================================

function eval_binary_expr(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: larrtype, rarrtype

	type(value_t) :: left, right

	left  = syntax_eval(node%left , state)
	right = syntax_eval(node%right, state)

	!print *, 'left  type = ', kind_name(left%type)
	!print *, 'right type = ', kind_name(right%type)

	larrtype = unknown_type
	rarrtype = unknown_type
	if (left %type == array_type) larrtype = left %array%type
	if (right%type == array_type) rarrtype = right%array%type

	res%type = get_binary_op_kind(left%type, node%op%kind, right%type, &
		larrtype, rarrtype)
	select case (res%type)
	case (bool_array_type, f32_array_type, i32_array_type, i64_array_type, &
		str_array_type)
		res%type = array_type
	end select

	if (res%type == unknown_type) then
		write(*,*) err_eval_binary_types(node%op%text)
		call internal_error()
	end if

	!print *, 'op = ', node%op%text

	select case (node%op%kind)
	case (plus_token)
		call add(left, right, res, node%op%text)

	case (minus_token)
		call subtract(left, right, res, node%op%text)

	case (star_token)
		call mul(left, right, res, node%op%text)

	case (sstar_token)
		call pow(left, right, res, node%op%text)

	case (slash_token)
		call div(left, right, res, node%op%text)

	case (percent_token)
		call mod_(left, right, res, node%op%text)

	case (and_keyword)
		call and_(left, right, res, node%op%text)

	case (or_keyword)
		call or_(left, right, res, node%op%text)

	case (eequals_token)
		call is_eq(left, right, res, node%op%text)

	case (bang_equals_token)
		call is_ne(left, right, res, node%op%text)

	case (less_token)
		call is_lt(left, right, res, node%op%text)

	case (less_equals_token)
		call is_le(left, right, res, node%op%text)

	case (greater_token)
		call is_gt(left, right, res, node%op%text)

	case (greater_equals_token)
		call is_ge(left, right, res, node%op%text)

	case default
		write(*,*) err_eval_binary_op(node%op%text)
		call internal_error()

	end select

end function eval_binary_expr

!===============================================================================

function eval_name_expr(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: rank_res, idim_, idim_res
	integer(kind = 8) :: il, iu, i8, index_
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	type(value_t) :: right

	!print *, "starting eval_name_expr()"
	!print *, 'searching identifier ', node%identifier%text

	if (allocated(node%lsubscripts) .and. &
		state%vars%vals(node%id_index)%type == str_type) then
		!print *, 'string subscript RHS name expr'

		!print *, 'str type'
		res%type = state%vars%vals(node%id_index)%type

		select case (node%lsubscripts(1)%sub_kind)
		case (scalar_sub)
			i8 = subscript_eval(node, state)
			!print *, 'i8 = ', i8
			res%sca%str%s = state%vars%vals(node%id_index)%sca%str%s(i8+1: i8+1)

		case (range_sub)

			! TODO: str all_sub

			il = subscript_eval(node, state) + 1

			! This feels inconsistent and not easy to extend to higher ranks
			right = syntax_eval(node%usubscripts(1), state)
			iu = right%to_i64() + 1

			!print *, ''
			!print *, 'identifier ', node%identifier%text
			!print *, 'il = ', il
			!print *, 'iu = ', iu
			!print *, 'str = ', state%vars%vals(node%id_index)%sca%str%s

			! Not inclusive of upper bound
			res%sca%str%s = state%vars%vals(node%id_index)%sca%str%s(il: iu-1)

		case default
			write(*,*) err_int_prefix//'unexpected subscript kind'//color_reset
			call internal_error()
		end select

	else if (allocated(node%lsubscripts)) then

		if (state%vars%vals(node%id_index)%type /= array_type) then
			write(*,*) err_int_prefix//'bad type, expected array'//color_reset
			call internal_error()
		end if

		!print *, 'sub kind 1 = ', kind_name(node%lsubscripts(1)%sub_kind)
		!print *, 'rank = ', node%val%array%rank

		if (all(node%lsubscripts%sub_kind == scalar_sub)) then

			! This could probably be lumped in with the range_sub case now
			! that I have it fully generalized
			i8 = subscript_eval(node, state)
			!print *, 'i8 = ', i8
			res = get_array_value_t(state%vars%vals(node%id_index)%array, i8)

		else

			call get_subscript_range(node, state, lsubs, usubs, rank_res)

			!print *, 'type = ', kind_name( node%val%array%type )

			!print *, 'type  = ', node%val%array%type
			!print *, 'rank  = ', node%val%array%rank
			!print *, 'size  = ', node%val%array%size
			!print *, 'len_  = ', node%val%array%len_
			!print *, 'cap   = ', node%val%array%cap

			allocate(res%array)
			res%type = array_type
			res%array%kind = expl_array
			res%array%type = node%val%array%type
			res%array%rank = rank_res

			allocate(res%array%size( rank_res ))
			idim_res = 1
			do idim_ = 1, size(lsubs)
				select case (node%lsubscripts(idim_)%sub_kind)
				case (range_sub, all_sub)

					res%array%size(idim_res) = usubs(idim_) - lsubs(idim_)

					idim_res = idim_res + 1
				end select
			end do
			!print *, 'res size = ', res%array%size

			res%array%len_ = product(res%array%size)
			!print *, 'res len = ', res%array%len_

			call allocate_array(res%array, res%array%len_)

			! Iterate through all subscripts in range and copy to result
			! array
			subs = lsubs
			do i8 = 0, res%array%len_ - 1
				!print *, 'subs = ', int(subs, 4)

				index_ = subscript_i32_eval(subs, state%vars%vals(node%id_index)%array)
				call set_array_value_t(res%array, i8, &
				     get_array_value_t(state%vars%vals(node%id_index)%array, index_))

				call get_next_subscript(lsubs, usubs, subs)
			end do
		end if

	else
		!print *, "name expr without subscripts"
		res = state%vars%vals(node%id_index)

		! Deep copy of whole array instead of aliasing pointers
		!
		! I suspect that value_t now has a deep copy problem like syntax_node_t
		! does, and this may be why samples/array-fns.syntran doesn't work.  May
		! need to convert return-by-value to subroutine out-arg as reference (or
		! override the copy operator, but that hasn't worked out so well for
		! syntax_node_t)
		if (res%type == array_type) then
			!print *, 'array  name_expr'

			if (allocated(res%array)) deallocate(res%array)

			allocate(res%array)
			res%type = array_type
			res%array = state%vars%vals(node%id_index)%array

			!res%array%rank = state%vars%vals(node%id_index)%array%rank
			!!print *, "allocated(size j) = ", allocated(state%vars%vals(node%id_index)%array%size)
			!res%array%size = state%vars%vals(node%id_index)%array%size
			!print *, "rank = ", res%array%rank, state%vars%vals(node%id_index)%array%rank

		!else
		!	print *, 'scalar name_expr'
		end if

	end if

end function eval_name_expr

!===============================================================================

function eval_fn_call(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	character(len = :), allocatable :: color

	integer :: i, io

	type(value_t) :: arg, arg1, arg2

	!print *, 'eval fn_call_expr'
	!print *, 'fn identifier = ', node%identifier%text
	!print *, 'fn id_index   = ', node%id_index

	state%ifn = state%ifn + 1  ! push
	call state%returned%push(.false.)

	res%type = node%val%type

	!print *, 'res type = ', res%type

	! Intrinsic fns
	select case (node%identifier%text)
	case ("exp")

		arg1 = syntax_eval(node%args(1), state)
		res%sca%f32 = exp(arg1%sca%f32)
		state%returned%v( state%ifn ) = .true.

	case ("0min_i32")

		arg = syntax_eval(node%args(1), state)
		res%sca%i32 = arg%sca%i32

		! Note that min/max/println etc. are variadic, so we loop to
		! size(node%args) instead of size(node%params)

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%i32 = min(res%sca%i32, arg%sca%i32)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("0min_i64")

		arg = syntax_eval(node%args(1), state)
		res%sca%i64 = arg%sca%i64

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%i64 = min(res%sca%i64, arg%sca%i64)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("0min_f32")

		arg = syntax_eval(node%args(1), state)
		res%sca%f32 = arg%sca%f32

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%f32 = min(res%sca%f32, arg%sca%f32)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("0max_i32")

		arg = syntax_eval(node%args(1), state)
		res%sca%i32 = arg%sca%i32

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%i32 = max(res%sca%i32, arg%sca%i32)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("0max_i64")

		arg = syntax_eval(node%args(1), state)
		res%sca%i64 = arg%sca%i64

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%i64 = max(res%sca%i64, arg%sca%i64)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("0max_f32")

		arg = syntax_eval(node%args(1), state)
		res%sca%f32 = arg%sca%f32

		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%f32 = max(res%sca%f32, arg%sca%f32)
		end do
		state%returned%v( state%ifn ) = .true.

	case ("println")

		do i = 1, size(node%args)
			arg = syntax_eval(node%args(i), state)
			write(output_unit, '(a)', advance = 'no') arg%to_str()
		end do
		write(output_unit, *)

		!! TODO: what, if anything, should println return?
		!res%sca%i32 = 0
		state%returned%v( state%ifn ) = .true.

	case ("str")

		res%sca%str%s = ''
		do i = 1, size(node%args)
			arg = syntax_eval(node%args(i), state)
			res%sca%str%s = res%sca%str%s // arg%to_str()  ! TODO: use char_vector_t
		end do
		state%returned%v( state%ifn ) = .true.

	case ("len")

		arg = syntax_eval(node%args(1), state)
		res%sca%i64 = len(arg%sca%str%s, 8)
		state%returned%v( state%ifn ) = .true.

	case ("parse_i32")

		arg = syntax_eval(node%args(1), state)
		read(arg%sca%str%s, *) res%sca%i32  ! TODO: catch iostat
		state%returned%v( state%ifn ) = .true.

	case ("parse_i64")

		arg = syntax_eval(node%args(1), state)
		read(arg%sca%str%s, *) res%sca%i64  ! TODO: catch iostat
		state%returned%v( state%ifn ) = .true.

	case ("parse_f32")

		arg = syntax_eval(node%args(1), state)
		read(arg%sca%str%s, *) res%sca%f32  ! TODO: catch iostat
		state%returned%v( state%ifn ) = .true.

	case ("0i32_sca")

		arg = syntax_eval(node%args(1), state)
		res%sca%i32 = arg%to_i32()
		state%returned%v( state%ifn ) = .true.

	case ("0i32_arr")

		arg = syntax_eval(node%args(1), state)
		res%array = arg%to_i32_array()
		state%returned%v( state%ifn ) = .true.

	case ("0i64_sca")

		arg = syntax_eval(node%args(1), state)
		res%sca%i64 = arg%to_i64()
		state%returned%v( state%ifn ) = .true.

	case ("0i64_arr")

		arg = syntax_eval(node%args(1), state)
		res%array = arg%to_i64_array()
		state%returned%v( state%ifn ) = .true.

	case ("open")

		arg = syntax_eval(node%args(1), state)

		! TODO: catch iostat, e.g. same file opened twice, folder doesn't
		! exist, etc.
		open(newunit = res%sca%file_%unit_, file = arg%sca%str%s)

		!print *, 'opened unit ', res%sca%file_%unit_
		res%sca%file_%name_ = arg%sca%str%s
		res%sca%file_%eof = .false.
		state%returned%v( state%ifn ) = .true.

	case ("readln")

		arg1 = syntax_eval(node%args(1), state)

		!print *, "reading from unit", arg1%sca%file_%unit_
		res%sca%str%s = read_line(arg1%sca%file_%unit_, io)
		!print *, 'done reading'

		! This could be a very dangerous side effect!  The file argument of
		! readln() acts as an out-arg:  it's eof flag can be toggled on.  I
		! don't have out-args anywhere else so I may want to rethink this
		! :exploding-head:
		!
		! writeln() does not need to mess with the vars struct like this
		! because the file is the actual return value for that fn

		!!print *, 'ident = ', node%args(1)%identifier%text
		!!state%vars%vals(node%id_index) = res

		! TODO:  set eof flag or crash for other non-zero io
		if (io == iostat_end) then
		!if (io /= 0) then
			!arg1%sca%file_%eof = .true.
			state%vars%vals(node%args(1)%id_index)%sca%file_%eof = .true.
		end if
		!print *, 'eof   = ', arg1%sca%file_%eof
		state%returned%v( state%ifn ) = .true.

	case ("writeln")

		arg1 = syntax_eval(node%args(1), state)

		!print *, 'writing to unit ', arg1%sca%file_%unit_
		do i = 2, size(node%args)
			arg = syntax_eval(node%args(i), state)
			write(arg1%sca%file_%unit_, '(a)', advance = 'no') arg%to_str()
		end do
		write(arg1%sca%file_%unit_, *)
		state%returned%v( state%ifn ) = .true.

	case ("eof")

		arg1 = syntax_eval(node%args(1), state)

		!print *, "checking eof for unit", arg1%sca%file_%unit_
		res%sca%bool = arg1%sca%file_%eof

		!print *, 'eof fn = ', arg1%sca%file_%eof
		state%returned%v( state%ifn ) = .true.

	case ("close")
		arg = syntax_eval(node%args(1), state)
		!print *, 'closing unit ', arg%sca%file_%unit_
		close(arg%sca%file_%unit_)
		state%returned%v( state%ifn ) = .true.

	case ("exit")

		arg = syntax_eval(node%args(1), state)

		io = arg%sca%i32
		if (io == 0) then
			color = fg_bright_green
		else
			color = fg_bold_bright_red
		end if

		write(*,*) color//'Exiting syntran with status '// &
			str(io)//color_reset

		call exit(io)
		state%returned%v( state%ifn ) = .true.  ! no-op

	case ("size")

		arg1 = syntax_eval(node%args(1), state)
		arg2 = syntax_eval(node%args(2), state)

		if (arg2%sca%i32 < 0 .or. arg2%sca%i32 >= arg1%array%rank) then
			! TODO: re-think runtime errors.  A different prefix here
			! besides err_int_prefix helps, but context should be given if
			! possible like for parser/lexer error diagnostics
			write(*,*) err_rt_prefix//"rank mismatch in size() call"//color_reset
			!print *, "rank     = ", arg1%array%rank
			!print *, "size arg = ", arg2%sca%i32
			call internal_error()
		end if

		!print *, "allocated(size) = ", allocated(arg1%array%size)
		res%sca%i64 = int(arg1%array%size( arg2%sca%i32 + 1 ))
		state%returned%v( state%ifn ) = .true.

	case ("count")

		arg1 = syntax_eval(node%args(1), state)
		res%sca%i64 = count(arg1%array%bool)
		state%returned%v( state%ifn ) = .true.

	case ("0sum_i32")
		arg1 = syntax_eval(node%args(1), state)
		res%sca%i32 = sum(arg1%array%i32)
		state%returned%v( state%ifn ) = .true.

	case ("0sum_i64")
		arg1 = syntax_eval(node%args(1), state)
		res%sca%i64 = sum(arg1%array%i64)
		state%returned%v( state%ifn ) = .true.

	case ("0sum_f32")
		arg1 = syntax_eval(node%args(1), state)
		res%sca%f32 = sum(arg1%array%f32)
		state%returned%v( state%ifn ) = .true.

	case ("all")

		arg1 = syntax_eval(node%args(1), state)
		res%sca%bool = all(arg1%array%bool)
		state%returned%v( state%ifn ) = .true.

		! Might not be strictly necessary now that %array is allocatable
		! instead of pointable
		!deallocate(arg1%array)

	case ("any")

		arg1 = syntax_eval(node%args(1), state)
		res%sca%bool = any(arg1%array%bool)
		state%returned%v( state%ifn ) = .true.

	case default
		! User-defined function

		if (.not. allocated(node%params)) then
			write(*,*) err_int_prefix//'unexpected fn'//color_reset
			call internal_error()
		end if

		!print *, 'fn name = ', node%identifier%text
		!print *, 'fn idx  = ', node%id_index
		!print *, 'node type = ', node%val%type
		!print *, 'size params = ', size(node%params)
		!print *, 'param ids = ', node%params

		! TODO: Shared param scope is ok at first, but eventually target
		! recursive fns with scoped stack frames

		! Pass by value (for now, at least).  Arguments are evaluated and
		! their values are copied to the fn parameters

		do i = 1, size(node%params)
			!print *, 'copying param ', i

			state%vars%vals( node%params(i) ) = &
				syntax_eval(node%args(i), state)

			!print *, "param rank = ", state%vars%vals( node%params(i) )%array%rank
			!print *, "param size = ", state%vars%vals( node%params(i) )%array%size

			!print *, 'done'
			!print *, ''
		end do

		res = syntax_eval(node%body, state)
		!print *, "res rank = ", res%array%rank
		!print *, 'res = ', res%to_str()

	end select

	! This is a runtime stopgap check that every fn returns, until (?) i can
	! figure out parse-time return branch checking.  Checking for unreachable
	! statements after returns also seems hard
	if (.not. state%returned%v( state%ifn )) then
		write(*,*) err_int_prefix//"reached end of function `", &
			node%identifier%text, "` without a return statement"//color_reset
		call internal_error()
	end if

	state%ifn = state%ifn - 1  ! pop
	state%returned%len_ = state%returned%len_ - 1

end function eval_fn_call

!===============================================================================

function eval_for_statement(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: i, rank, for_kind
	integer(kind = 8) :: i8, len8

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, itr, &
		step, len_, tmp

	! Evaluate all of these ahead of loop, but only if they are allocated!
	if (allocated(node%array%lbound)) lbound_ = syntax_eval(node%array%lbound, state)
	if (allocated(node%array%step  )) step    = syntax_eval(node%array%step  , state)
	if (allocated(node%array%ubound)) ubound_ = syntax_eval(node%array%ubound, state)
	if (allocated(node%array%len_  )) len_    = syntax_eval(node%array%len_  , state)

	!print *, 'lbound_ = ', lbound_%to_i64()
	!print *, 'ubound_ = ', ubound_%to_i64()
	!print *, 'lbound type = ', kind_name(lbound_%type)
	!print *, 'ubound type = ', kind_name(ubound_%type)
	!print *, 'node%array%type = ', kind_name(node%array%val%array%type)

	!print *, 'node array kind = ', kind_name(node%array%kind)
	!print *, 'array kind      = ', kind_name(node%array%val%array%kind)

	select case (node%array%kind)
	case (array_expr)

		! Primary array exprs are evaluated lazily without wasting memory
		for_kind = node%array%val%array%kind

		select case (node%array%val%array%kind)
		case (bound_array)

			! Do promotion once before loop
			if (any(i64_type == [lbound_%type, ubound_%type])) then
				!print *, 'promoting'
				call promote_i32_i64(lbound_)
				call promote_i32_i64(ubound_)
				itr%type = i64_type
			else
				itr%type = i32_type
			end if

			if (.not. any(itr%type == [i32_type, i64_type])) then
				write(*,*) err_int_prefix//'unit step array type eval not implemented'//color_reset
				call internal_error()
			end if

			len8 = ubound_%to_i64() - lbound_%to_i64()

		case (step_array)

			! If any bound or step is i64, cast the others up to match
			if (any(i64_type == [lbound_%type, step%type, ubound_%type])) then
				call promote_i32_i64(lbound_)
				call promote_i32_i64(step)
				call promote_i32_i64(ubound_)
				itr%type = i64_type
			else
				itr%type = lbound_%type
			end if

			select case (itr%type)
			case (i32_type)
				len8 = (ubound_%sca%i32 - lbound_%sca%i32 &
					+ step%sca%i32 - sign(1,step%sca%i32)) / step%sca%i32

			case (i64_type)
				len8 = (ubound_%sca%i64 - lbound_%sca%i64 &
					+ step%sca%i64 - sign(int(1,8),step%sca%i64)) / step%sca%i64

			case (f32_type)
				len8 = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32)

			case default
				write(*,*) err_int_prefix//'step array type eval not implemented'//color_reset
				call internal_error()
			end select

		case (len_array)

			itr%type = node%array%val%array%type

			select case (itr%type)
			case (f32_type)
				len8 = len_%to_i64()
			case default
				write(*,*) err_int_prefix//'bound/len array type eval not implemented'//color_reset
				call internal_error()
			end select

		case (expl_array)
			len8 = node%array%val%array%len_

		case (size_array)

			rank = size( node%array%size )
			len8 = 1
			do i = 1, rank
				len_ = syntax_eval(node%array%size(i), state)
				len8 = len8 * len_%to_i64()
			end do

			if (size(node%array%elems) /= len8) then
				write(*,*) err_rt_prefix//"size of explicit array "// &
					"does not match number of elements"//color_reset
				call internal_error()
			end if

		case (unif_array)

			rank = size(node%array%size)
			!print *, 'rank = ', rank
			len8 = 1
			do i = 1, rank
				len_ = syntax_eval(node%array%size(i), state)
				len8 = len8 * len_%to_i64()
			end do
			!print *, 'len8 = ', len8

		case default
			write(*,*) err_int_prefix//'for loop not implemented for this array kind'//color_reset
			call internal_error()
		end select

	case default
		!print *, 'non-primary array expression'

		! Any non-primitive array needs to be evaluated before iterating
		! over it.  Parser guarantees that this is an array
		!
		! Unlike step_array, itr%type does not need to be set here because
		! it is set in array_at() (via get_array_value_t())
		for_kind = array_expr

		tmp = syntax_eval(node%array, state)
		array = tmp%array

		len8 = array%len_
		!print *, 'len8 = ', len8

	end select

	!print *, 'itr%type = ', kind_name(itr%type)

	! Push scope to make the loop iterator local
	call state%vars%push_scope()
	do i8 = 1, len8

		call array_at(itr, for_kind, i8, lbound_, step, ubound_, &
			len_, array, node%array%elems, state)

		!print *, 'itr = ', itr%to_str()

		! During evaluation, insert variables by array id_index instead of
		! dict lookup.  This is much faster and can be done during
		! evaluation now that we know all of the variable identifiers.
		! Parsing still needs to rely on dictionary lookups because it does
		! not know the entire list of variable identifiers ahead of time
		state%vars%vals(node%id_index) = itr

		res = syntax_eval(node%body, state)

		if (state%returned%v( state%ifn )) exit

	end do
	call state%vars%pop_scope()

end function eval_for_statement

!===============================================================================

function eval_assignment_expr(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: rank_res
	integer(kind = 8) :: i8, index_, len8
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	type(value_t) :: array_val, tmp

	if (.not. allocated(node%lsubscripts)) then

		!! This deallocation will cause a crash when an array appears on both
		!! the LHS and RHS of fn_call assignment, e.g. `dv = diff_(dv, i)` in
		!! AOC 2023/09
		!if (allocated(state%vars%vals)) then
		!if (allocated(state%vars%vals(node%id_index)%array)) then
		!	!print *, "deallocating lhs array"
		!	deallocate(state%vars%vals(node%id_index)%array)
		!end if
		!end if

		! Assign return value
		!print *, 'eval and set res'
		res = syntax_eval(node%right, state)

		! TODO: test int/float casting.  It should be an error during
		! parsing

		!print *, 'compound assign'
		!print *, 'lhs type = ', kind_name( state%vars%vals(node%id_index)%type )

		call compound_assign(state%vars%vals(node%id_index), res, node%op)

		! For compound assignment, ensure that the LHS is returned
		!print *, 'setting res again'
		res = state%vars%vals(node%id_index)
		!print *, 'done'

		! The difference between let and assign is inserting into the
		! current scope (let) vs possibly searching parent scopes (assign).
		! During evaluation we don't need any extra logic for scoping.  The
		! parser has already assigned a separate id_index for each
		! identifier at each scope level

	else
		!print *, 'LHS array subscript assignment'
		!print *, 'LHS type = ', kind_name(state%vars%vals(node%id_index)%array%type)  ! not alloc for str

		! Assign return value from RHS
		res = syntax_eval(node%right, state)

		!print *, 'RHS = ', res%to_str()

		if (state%vars%vals(node%id_index)%type == str_type) then
			!print *, 'str_type'

			! TODO: ban compound character substring assignment
			i8 = subscript_eval(node, state)
			state%vars%vals(node%id_index)%sca%str%s(i8+1: i8+1) = res%sca%str%s

		else if (all(node%lsubscripts%sub_kind == scalar_sub)) then

			!print *, 'non str_type scalar subscript'
			!print *, 'LHS array type = ', &
			!	state%vars%vals(node%id_index)%array%type
			!print *, 'LHS array = ', state%vars%vals(node%id_index)%array%i32

			i8 = subscript_eval(node, state)
			array_val = get_array_value_t(state%vars%vals(node%id_index)%array, i8)
			call compound_assign(array_val, res, node%op)
			call set_array_value_t( &
				state%vars%vals(node%id_index)%array, i8, array_val)
			res = array_val

		else

			!print *, 'lhs slice assignment'

			call get_subscript_range(node, state, lsubs, usubs, rank_res)
			len8 = product(usubs - lsubs)
			!print *, 'len8 = ', len8

			! TODO: some size/shape checking might be needed here between
			! LHS and RHS

			! Scalar rhs
			if (res%type /= array_type) array_val = res

			! Iterate through all subscripts in range and copy to result
			! array
			subs = lsubs
			do i8 = 0, len8 - 1

				!print *, 'subs = ', int(subs, 4)

				! This is confusing.  Maybe rename array_val -> rhs_val and
				! tmp -> lhs_val or something

				if (res%type == array_type) then
					array_val = get_array_value_t(res%array, i8)
				end if

				index_ = subscript_i32_eval(subs, state%vars%vals(node%id_index)%array)
				tmp  = get_array_value_t(state%vars%vals(node%id_index)%array, index_)
				call compound_assign(tmp, array_val, node%op)
				call set_array_value_t(state%vars%vals(node%id_index)%array, index_, tmp)

				!! move conditions out of loop for perf?
				!if (res%type == array_type) then
				!	call set_array_value_t(res%array, i8, tmp)
				!else

				!	! this makes the res return value a scalar.  Maybe
				!	! not correct for fn return values or paren exprs, at
				!	! least it's not consistent with the way that array rhs
				!	! vals work.  Maybe I will make a breaking change on the
				!	! return value here because copying res val can also
				!	! have a large perf overhead.
				!	res = tmp

				!	! This is illegal in python numpy:
				!	!
				!	! >>> import numpy as np
				!	! >>> a = np.arange(1, 6)
				!	! >>> b = (a[1:4] := 3)
				!	!   File "<stdin>", line 1
				!	!     b = (a[1:4] := 3)
				!	!          ^^^^^^
				!	! SyntaxError: cannot use assignment expressions with subscript
				!	! >>>
				!	!
				!	! Of course, such an assignment is legal as its own
				!	! statement without the "walrus" operator `:=` :
				!	!
				!	! >>> a[1:4] = 3
				!	! >>> a
				!	!     # [1, 3, 3, 3, 5]
				!	!
				!	! I believe it is illegal in python because of the
				!	! ambiguity of what should `b` be if it is assigned.
				!	! Should `b` be the whole `a` array as in syntran, or
				!	! just the slice `a[1:4]`, or just the scalar `3`?
				!	!
				!	! I think there's a good case to be made that it should
				!	! be the slice `a[1:4]`, but the implementation is more
				!	! simple by setting `b` to the whole array `a`.

				!end if

				call get_next_subscript(lsubs, usubs, subs)
			end do

			! set res (whole array (slice?)) for return val in case of
			! compound assignment.  see note above re walrus operator
			res = state%vars%vals(node%id_index)

		end if
	end if

end function eval_assignment_expr

!===============================================================================

function eval_translation_unit(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: i

	! TODO: do we want to globally push/pop scope for whole
	! translation_unit?  Will this have impacts on interpretting multiple
	! files, or allowing the user to override intrinsic fns?
	!call vars%push_scope()

	! The final statement of a unit returns the actual result.  Non-final
	! members only change the (vars) state or define fns
	do i = 1, size(node%members)

		! Only eval statements, not fns declarations.  TODO: cycle structs
		! too.
		!
		! TODO: is this where we should copy fn dict to array?
		if (node%members(i)%kind == fn_declaration) cycle

		res = syntax_eval(node%members(i), state)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' res = ', res%to_str()
		!print *, ''

		! HolyC feature: implicitly print name expression members.  I may
		! remove this after I implement an intrinsic print() fn.  May also
		! need to suppress this for void fn calls later
		if (node%members(i)%kind == name_expr .and. .not. state%quiet) then
			write(*,*) res%to_str()
		end if

		if (state%returned%v( state%ifn )) exit

	end do

	!call state%vars%pop_scope()

end function eval_translation_unit

!===============================================================================

function eval_array_expr(node, state) result(res)

	type(syntax_node_t), intent(in) :: node

	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: i, j
	integer(kind = 8) :: i8

	real(kind = 4) :: f, fstep

	type(array_t) :: array
	type(value_t) :: lbound_, ubound_, elem, &
		step, len_

	!print *, "starting eval_array_expr()"
	!print *, 'identifier = ', node%identifier%text

	if (node%val%array%kind == step_array) then

		lbound_ = syntax_eval(node%lbound, state)
		step    = syntax_eval(node%step  , state)
		ubound_ = syntax_eval(node%ubound, state)

		array%type = node%val%array%type

		! If any bound or step is i64, cast the others up to match
		if (any(i64_type == [lbound_%type, step%type, ubound_%type])) then

			!! this happens during parsing
			!array%type = i64_type

			call promote_i32_i64(lbound_)
			call promote_i32_i64(step)
			call promote_i32_i64(ubound_)
		end if

		!print *, 'lbound_ = ', lbound_%sca%i64
		!print *, 'step32 = ', step  %sca%i32
		!print *, 'step64 = ', step  %sca%i64
		!print *, 'ubound_ = ', ubound_%sca%i64

		if (array%type == i32_type) then

			array%cap = (ubound_%sca%i32 - lbound_%sca%i32 &
				+ step%sca%i32 - sign(1,step%sca%i32)) / step%sca%i32

			!print *, 'cap = ', array%cap
			allocate(array%i32( array%cap ))

			j = 1
			i = lbound_%sca%i32
			if (lbound_%sca%i32 < ubound_%sca%i32 .neqv. 0 < step%sca%i32) i = ubound_%sca%i32

			! Step may be negative
			do while ((i  < ubound_%sca%i32 .eqv. lbound_%sca%i32 < ubound_%sca%i32) &
			     .and. i /= ubound_%sca%i32)
				array%i32(j) = i
				i = i + step%sca%i32
				j = j + 1
			end do
			array%len_ = j - 1

		else if (array%type == i64_type) then

			array%cap = (ubound_%sca%i64 - lbound_%sca%i64 &
				+ step%sca%i64 - sign(int(1,8),step%sca%i64)) / step%sca%i64

			allocate(array%i64( array%cap ))

			j = 1
			i8 = lbound_%sca%i64
			if (lbound_%sca%i64 < ubound_%sca%i64 .neqv. 0 < step%sca%i64) then
				i8 = ubound_%sca%i64
			end if

			! Step may be negative
			do while ((i8  < ubound_%sca%i64 .eqv. lbound_%sca%i64 < ubound_%sca%i64) &
			     .and. i8 /= ubound_%sca%i64)
				array%i64(j) = i8
				i8 = i8 + step%sca%i64
				j = j + 1
			end do
			array%len_ = j - 1

		else if (array%type == f32_type) then

			!print *, 'lbound_, ubound_ = ', lbound_%sca%f32, ubound_%sca%f32
			!print *, 'step = ', step%sca%f32

			array%cap = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32)
			allocate(array%f32( array%cap ))

			j = 1
			f = lbound_%sca%f32
			if (lbound_%sca%f32 < ubound_%sca%f32 .neqv. 0 < step%sca%f32) f = ubound_%sca%f32

			do while ((f  < ubound_%sca%f32 .eqv. lbound_%sca%f32 < ubound_%sca%f32) &
			     .and. f /= ubound_%sca%f32)
				array%f32(j) = f

				! Using only addition here seems more efficient, but
				! rounding errors accumulate differently.  Compare
				! `[0.0: 0.1: 0.9];` with both methods.  First ends in
				! 0.800001, while second method (with multiplication) ends
				! in 0.8

				!f = f + step%sca%f32
				f = lbound_%sca%f32 + j * step%sca%f32

				j = j + 1
			end do
			array%len_ = j - 1
			!array%len_ = array%cap

		else
			write(*,*) err_int_prefix//'step array type eval not implemented'//color_reset
			call internal_error()
		end if

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		allocate(res%array)
		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == len_array) then

		!print *, 'len array'
		lbound_ = syntax_eval(node%lbound, state)
		ubound_ = syntax_eval(node%ubound, state)
		len_    = syntax_eval(node%len_  , state)

		array%type = node%val%array%type
		array%len_  = len_%to_i64()
		array%cap  = array%len_

		if (array%type == f32_type) then

			allocate(array%f32( array%cap ))
			fstep = (ubound_%sca%f32 - lbound_%sca%f32) &
				/ real((len_%to_i64() - 1))

			do i = 0, len_%to_i32() - 1
				array%f32(i+1) = lbound_%sca%f32 + i * fstep
			end do

		else
			write(*,*) err_int_prefix//'bound/len array type eval not implemented'//color_reset
			call internal_error()
		end if

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		allocate(res%array)

		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == unif_array) then

		array%rank = size( node%size )
		allocate(array%size( array%rank ))

		do i = 1, array%rank
			len_ = syntax_eval(node%size(i), state)
			array%size(i) = len_%to_i64()
			!print *, 'size['//str(i)//'] = ', array%size(i)
		end do

		! Uniform-value impl arrays (every element has the same value at
		! initialization, and you could say "constant" but they are of
		! course mutable)

		!print *, 'len array'
		lbound_ = syntax_eval(node%lbound, state)

		! Allocate in one shot without growing

		array%type = node%val%array%type
		array%len_  = product(array%size)
		!print *, 'array%len_ = ', array%len_

		call allocate_array(array, array%len_)
		select case (array%type)
		case (i32_type)
			array%i32 = lbound_%sca%i32
		case (i64_type)
			array%i64 = lbound_%sca%i64
		case (f32_type)
			array%f32 = lbound_%sca%f32
		case (bool_type)
			array%bool = lbound_%sca%bool
		case (str_type)
			array%str = lbound_%sca%str
		case default
			write(*,*) err_eval_len_array(kind_name(array%type))
			call internal_error()
		end select

		allocate(res%array)
		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == bound_array) then
		!print *, 'impl_array'

		! Expand implicit array kinds here on evaluation.  Consider
		! something like this:
		!
		!     let a = [0: 5];
		!     a[2] = -3;
		!
		! Even though a is initialized to an implicit array, the second
		! statement requires it to be explicit, so we might as well expand
		! at initialization

		lbound_ = syntax_eval(node%lbound, state)
		ubound_ = syntax_eval(node%ubound, state)

		!array = new_array(node%val%array%type)

		array%type = node%val%array%type

		if (any(i64_type == [lbound_%type, ubound_%type])) then
			call promote_i32_i64(lbound_)
			call promote_i32_i64(ubound_)
		end if

		if (.not. any(array%type == [i32_type, i64_type])) then
			write(*,*) err_int_prefix//'unit step array type eval not implemented'//color_reset
			call internal_error()
		end if

		if (array%type == i32_type) then
			array%len_ = ubound_%sca%i32 - lbound_%sca%i32
		else !if (array%type == i64_type) then
			array%len_ = ubound_%sca%i64 - lbound_%sca%i64
		end if

		call allocate_array(array, array%len_)

		!print *, 'bounds in [', lbound_%str(), ': ', ubound_%str(), ']'
		!print *, 'node%val%array%type = ', node%val%array%type

		if (array%type == i32_type) then
			do i = lbound_%sca%i32, ubound_%sca%i32 - 1
				array%i32(i - lbound_%sca%i32 + 1) = i
			end do
		else !if (array%type == i64_type) then
			do i8 = lbound_%sca%i64, ubound_%sca%i64 - 1
				array%i64(i8 - lbound_%sca%i64 + 1) = i8
			end do
		end if

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		allocate(res%array)

		res%type  = array_type
		res%array = array

	else if (node%val%array%kind == size_array) then

		! Explicit array with size

		array = new_array(node%val%array%type, size(node%elems))

		do i = 1, size(node%elems)
			elem = syntax_eval(node%elems(i), state)
			!print *, 'elem['//str(i)//'] = ', elem%str()
			call array%push(elem)
		end do

		array%rank = size( node%size )
		allocate(array%size( array%rank ))
		do i = 1, array%rank
			len_ = syntax_eval(node%size(i), state)
			array%size(i) = len_%to_i64()
		end do

		if (size(node%elems) /= product(array%size)) then
			write(*,*) err_rt_prefix//"size of explicit array "// &
				"does not match number of elements"//color_reset
			call internal_error()
		end if

		!print *, 'copying array'
		allocate(res%array)
		res%type  = array_type
		res%array = array
		!print *, 'done'

		!print *, "size_array"
		!print *, "size = ", array%size

	else if (node%val%array%kind == expl_array) then
		!print *, 'expl_array'

		! Explicit rank-1 arrays

		! TODO: allow empty arrays?  Sub type of empty array?  Empty arrays
		! can currently be created like [0: -1];
		array = new_array(node%val%array%type, size(node%elems))

		do i = 1, size(node%elems)
			elem = syntax_eval(node%elems(i), state)
			!print *, 'elem['//str(i)//'] = ', elem%str()
			call array%push(elem)
		end do

		array%rank = 1
		allocate(array%size( array%rank ))
		array%size = array%len_

		!print *, 'copying array'
		allocate(res%array)
		res%type  = array_type
		res%array = array
		!print *, 'done'

	else
		write(*,*) err_int_prefix//'unexpected array kind'//color_reset
		call internal_error()
	end if

end function eval_array_expr

!===============================================================================

function eval_while_statement(node, state) result(res)

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	type(value_t) :: res

	!********

	type(value_t) :: condition

	condition = syntax_eval(node%condition, state)
	do while (condition%sca%bool)
		res = syntax_eval(node%body, state)
		condition = syntax_eval(node%condition, state)
		if (state%returned%v( state%ifn )) exit
	end do

end function eval_while_statement

!===============================================================================

function eval_if_statement(node, state) result(res)

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	type(value_t) :: res

	!********

	type(value_t) :: condition

	condition = syntax_eval(node%condition, state)
	!print *, 'condition = ', condition%str()

	if (condition%sca%bool) then
		!print *, 'if'
		res = syntax_eval(node%if_clause, state)

	else if (allocated(node%else_clause)) then
		!print *, 'else'
		res = syntax_eval(node%else_clause, state)

	end if

end function eval_if_statement

!===============================================================================

function eval_return_statement(node, state) result(res)

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	type(value_t) :: res

	!********

	!print *, "starting eval_return_statement"

	state%returned%v( state%ifn ) = .true.

	if (node%right%val%type == void_type) then
		!res%type = unknown_type
		return
	end if

	res = syntax_eval(node%right, state)

	!print *, "ending eval_return_statement"

end function eval_return_statement

!===============================================================================

function eval_block_statement(node, state) result(res)

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	type(value_t) :: res

	!********

	integer :: i

	type(value_t) :: tmp

	call state%vars%push_scope()

	! The final statement of a block returns the actual result.  Non-final
	! members only change the (vars) state.
	do i = 1, size(node%members)
		tmp = syntax_eval(node%members(i), state)

		!print *, 'kind = ', node%members(i)%kind
		!print *, i, ' tmp = ', tmp%to_str()
		!print *, 'type = ', tmp%type, kind_name(tmp%type)
		!print *, ''

		! In case of no-op if statements and while loops
		if (tmp%type /= unknown_type) res = tmp

		! HolyC feature: implicitly print name expression members.  I may
		! remove this after I implement an intrinsic print() fn.  May also
		! need to suppress this for void fn calls later
		if (node%members(i)%kind == name_expr .and. .not. state%quiet) then
			write(*,*) tmp%to_str()
		end if

		if (state%returned%v( state%ifn )) exit

	end do

	call state%vars%pop_scope()

end function eval_block_statement

!===============================================================================

function eval_unary_expr(node, state) result(res)

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	type(value_t) :: res

	!********

	type(value_t) :: right

	right = syntax_eval(node%right, state)
	!print *, 'right = ', right

	res%type = right%type

	select case (node%op%kind)
	case (plus_token)
		res = right

	case (minus_token)
		call negate(right, res, node%op%text)

	case (not_keyword)
		call not_(right, res, node%op%text)

	case default
		write(*,*) err_eval_unary_op(node%op%text)
		call internal_error()
	end select

end function eval_unary_expr

!===============================================================================

subroutine promote_i32_i64(val)

	! If val is i32 type, change it to i64 and copy the values
	!
	! TODO: consider refactoring with to_i64()? at least add internal error.
	! to_i64() is a fn which returns a new val, whereas this is a subroutine
	! that changes the type of a given value

	type(value_t), intent(inout) :: val

	if (val%type == i32_type) then
		val%type = i64_type
		val%sca%i64 = val%sca%i32
	end if

end subroutine promote_i32_i64

!===============================================================================

subroutine allocate_array(array, cap)

	type(array_t), intent(inout) :: array
	integer(kind = 8), intent(in) :: cap

	array%cap = cap

	select case (array%type)
	case (i32_type)
		allocate(array%i32( cap ))
	case (i64_type)
		allocate(array%i64( cap ))
	case (f32_type)
		allocate(array%f32( cap ))
	case (bool_type)
		allocate(array%bool( cap ))
	case (str_type)
		allocate(array%str( cap ))
	case default
		write(*,*) err_int_prefix//'cannot allocate array of type `' &
			//kind_name(array%type)//'`'//color_reset
		call internal_error()
	end select

end subroutine allocate_array

!===============================================================================

function new_array(type, cap) result(vector)

	! TODO: use or combine allocate_array()

	integer, intent(in) :: type
	integer, intent(in), optional :: cap
	type(array_t) :: vector

	vector%len_ = 0

	if (present(cap)) then
		vector%cap = cap
	else
		vector%cap = 2  ! I think a small default makes sense here
	end if

	if      (type == i32_type) then
		allocate(vector%i32 ( vector%cap ))
	else if (type == i64_type) then
		allocate(vector%i64 ( vector%cap ))
	else if (type == f32_type) then
		allocate(vector%f32 ( vector%cap ))
	else if (type == bool_type) then
		allocate(vector%bool( vector%cap ))
	else if (type == str_type) then
		allocate(vector%str ( vector%cap ))
	else
		write(*,*) err_int_prefix//'array type not implemented'//color_reset
		call internal_error()
	end if

	vector%type = type

end function new_array

!===============================================================================

subroutine compound_assign(lhs, rhs, op)
	! TODO: rename?  This also handles regular assignment

	! lhs += rhs;
	!   or
	! lhs *= rhs;
	!   etc.

	type(value_t), intent(inout) :: lhs
	type(value_t), intent(in) :: rhs

	type(syntax_token_t), intent(in) :: op

	!******

	type(value_t) :: tmp  ! necessary for arrays

	if (op%kind /= equals_token) tmp = lhs

	select case (op%kind)
	case (equals_token)
		!print *, 'assign'
		!lhs = rhs  ! simply overwrite
		call assign_(lhs, rhs, op%text)

	case (plus_equals_token)
		call add(tmp, rhs, lhs, op%text)

	case (minus_equals_token)
		call subtract(tmp, rhs, lhs, op%text)

	case (star_equals_token)
		call mul(tmp, rhs, lhs, op%text)

	case (slash_equals_token)
		call div(tmp, rhs, lhs, op%text)

	case (sstar_equals_token)
		call pow(tmp, rhs, lhs, op%text)

	case (percent_equals_token)
		call mod_(tmp, rhs, lhs, op%text)

	case default
		write(*,*) err_int_prefix//'unexpected assignment operator ', quote(op%text)//color_reset
		call internal_error()
	end select

end subroutine compound_assign

!===============================================================================

subroutine get_subscript_range(node, state, lsubs, usubs, rank_res)

	! Evaluate the lower- and upper-bounds of each range of a subscripted array
	! slice
	!
	! TODO: `rank_res` is a misnomer.  For LHS slicing it's the rank of the LHS
	! *after* being sliced

	type(syntax_node_t), intent(in) :: node
	type(state_t) :: state

	integer(kind = 8), allocatable, intent(out) :: lsubs(:), usubs(:)
	integer, intent(out) :: rank_res

	!********

	integer :: i, rank_

	type(value_t) :: lsubval, usubval

	rank_ = state%vars%vals(node%id_index)%array%rank
	allocate(lsubs(rank_), usubs(rank_))
	rank_res = 0
	do i = 1, rank_

		if (node%lsubscripts(i)%sub_kind == all_sub) then
			lsubs(i) = 0
			!print *, 'lsubs(i) = ', lsubs(i)
		else
			lsubval = syntax_eval(node%lsubscripts(i), state)
			lsubs(i) = lsubval%sca%i32
		end if

		select case (node%lsubscripts(i)%sub_kind)
		case (all_sub)
			usubs(i) = state%vars%vals(node%id_index)%array%size(i)
			!print *, 'usubs(i) = ', usubs(i)

			rank_res = rank_res + 1

		case (range_sub)
			usubval = syntax_eval(node%usubscripts(i), state)
			!usubs(i) = usubval%sca%i32
			usubs(i) = usubval%to_i64()

			rank_res = rank_res + 1

		case (scalar_sub)
			! Scalar subs are converted to a range-1 sub so we can
			! iterate later without further case logic
			usubs(i) = lsubs(i) + 1

		case default
			write(*,*) err_int_prefix//'cannot evaluate subscript kind'//color_reset
			call internal_error()

		end select

	end do
	!print *, 'lsubs = ', lsubs
	!print *, 'usubs = ', usubs
	!print *, 'rank_res = ', rank_res

end subroutine get_subscript_range

!===============================================================================

subroutine get_next_subscript(lsubs, usubs, subs)

	! This is like a bignum += 1 algorithm but in an arbitrary mixed radix

	integer(kind = 8), intent(in) :: lsubs(:), usubs(:)
	integer(kind = 8), intent(inout) :: subs(:)

	!********

	integer :: j

	j = 1
	do while (j < size(subs) .and. subs(j) == usubs(j) - 1)
		subs(j) = lsubs(j)
		j = j + 1
	end do
	subs(j) = subs(j) + 1

end subroutine get_next_subscript

!===============================================================================

function subscript_i32_eval(subs, array) result(index_)

	! subscript_eval() but with a primitive subs int array
	!
	! Is there a way to copy a slice without doing so much math?
	!
	! TODO: bound checking if enabled.  unlike subscript_eval(),
	! we can do it here outside the i8 loop

	integer(kind = 8), intent(in) :: subs(:)
	type(array_t) :: array

	integer(kind = 8) :: index_

	!********

	integer :: j
	integer(kind = 8) :: prod

	prod  = 1
	index_ = 0
	do j = 1, array%rank
		!print *, 'j = ', j
		index_ = index_ + prod * subs(j)
		prod  = prod * array%size(j)
	end do
	!print *, 'index_ = ', index_

end function subscript_i32_eval

!===============================================================================

function subscript_eval(node, state) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_

	type(syntax_node_t) :: node
	type(state_t) :: state

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	! str scalar with single char subscript
	if (state%vars%vals(node%id_index)%type == str_type) then
		subscript = syntax_eval(node%lsubscripts(1), state)
		index_ = subscript%to_i64()
		return
	end if

	!if (state%vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

	! This could be refactored to run syntax_eval() on each subscript first, and
	! then call subscript_i32_eval() after the loop.  There would be a small
	! memory overhead to save i32 sub array but probably no significant time or
	! space perf difference
	prod  = 1
	index_ = 0
	do i = 1, state%vars%vals(node%id_index)%array%rank
		!print *, 'i = ', i

		subscript = syntax_eval(node%lsubscripts(i), state)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%to_i64()
		prod  = prod * state%vars%vals(node%id_index)%array%size(i)

	end do

end function subscript_eval

!===============================================================================

subroutine array_at(val, kind_, i, lbound_, step, ubound_, len_, array, &
		elems, state)

	! This lazily gets an array value at an index i without expanding the whole
	! implicit array in memory.  Used for for loops
	!
	! TODO: way too many args.  Bundle lbound_, step, ubound_, len_, array, and
	! elems into a new struct named `array_parts`.
	!
	! It's also worth considering whether the existence of an array_at() fn is
	! the right abstraction at all.  It only gets called in one place.  Is the
	! memory saving worthwhile?

	type(value_t), intent(inout) :: val

	integer, intent(in) :: kind_

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(in) :: lbound_, step, ubound_, len_

	type(array_t), intent(in) :: array

	type(syntax_node_t), allocatable :: elems(:)

	type(state_t) :: state

	!*********

	select case (kind_)
	case (bound_array)

		if (val%type == i32_type) then
			val%sca%i32 = lbound_%sca%i32 + int(i) - 1
		else !if (val%type == i64_type) then
			val%sca%i64 = lbound_%sca%i64 + i - 1
		end if

	case (step_array)

		select case (val%type)
		case (i32_type)
			val%sca%i32 = lbound_%sca%i32 + int(i - 1) * step%sca%i32

		case (i64_type)
			val%sca%i64 = lbound_%sca%i64 + (i - 1) * step%sca%i64

		case (f32_type)
			val%sca%f32 = lbound_%sca%f32 + real(i - 1) * step%sca%f32

		end select

	case (len_array)

		val%sca%f32 = lbound_%sca%f32 + real(i - 1) * &
			(ubound_%sca%f32 - lbound_%sca%f32) / real((len_%to_i64() - 1))

	case (expl_array, size_array)
		val = syntax_eval(elems(i), state)

	case (unif_array)
		val = lbound_

	case (array_expr)
		! Non-primary array expr
		val = get_array_value_t(array, i - 1)

	case default
		write(*,*) err_int_prefix//'for loop not implemented for this array kind'//color_reset
		call internal_error()
	end select

end subroutine array_at

!===============================================================================

function get_array_value_t(array, i) result(val)

	type(array_t), intent(in) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t) :: val

	!print *, 'starting get_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)

	val%type = array%type
	select case (array%type)
		case (bool_type)
			val%sca%bool = array%bool(i + 1)

		case (i32_type)
			val%sca%i32 = array%i32(i + 1)

		case (i64_type)
			val%sca%i64 = array%i64(i + 1)

		case (f32_type)
			val%sca%f32 = array%f32(i + 1)

		case (str_type)
			val%sca%str = array%str(i + 1)

	end select

end function get_array_value_t

!===============================================================================

subroutine set_array_value_t(array, i, val)

	type(array_t), intent(inout) :: array

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(in) :: val

	!print *, 'starting set_array_value_t()'
	!print *, 'array%type = ', kind_name(array%type)
	!print *, 'val%type   = ', kind_name(val%type)

	! array%type is already set
	select case (array%type)
		case (bool_type)
			array%bool(i + 1) = val%sca%bool

		case (i32_type)
			array%i32(i + 1) = val%to_i32()

		case (i64_type)
			array%i64(i + 1) = val%to_i64()

		case (f32_type)
			array%f32(i + 1) = val%to_f32()

		case (str_type)
			array%str(i + 1) = val%sca%str

	end select

end subroutine set_array_value_t

!===============================================================================

end module syntran__eval_m

!===============================================================================

