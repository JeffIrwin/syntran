
!===============================================================================

module syntran__eval_m

	use iso_fortran_env

	use syntran__bool_m
	use syntran__math_m
	use syntran__types_m

	implicit none

!===============================================================================

contains

!===============================================================================

recursive function syntax_eval(node, vars, fns, quiet) result(res)

	! TODO: encapsulate vars, fns, and quiet into a state struct.  Add
	! diagnostics to state for runtime errors (bounds overflow, rank mismatch,
	! etc.)

	type(syntax_node_t) :: node

	! I don't want to make this arg optional, because then it would require
	! copying a potentially large struct to a local var without fancy use of
	! move_alloc()
	type(vars_t) :: vars

	type(fns_t) :: fns

	logical, optional, intent(in) :: quiet

	type(value_t) :: res

	!********

	character(len = :), allocatable :: color

	integer :: i, j, io, rank, rank_res, idim_, idim_res, larrtype, rarrtype
	integer(kind = 8) :: il, iu, i8, index_, prod, len8
	integer(kind = 8), allocatable :: lsubs(:), usubs(:), subs(:)

	logical :: quietl

	real(kind = 4) :: f, fstep

	type(array_t) :: array
	type(value_t) :: left, right, condition, lbound_, ubound_, itr, elem, &
		step, len_, arg, arg1, arg2, array_val, lsubval, usubval, tmp

	!print *, 'starting syntax_eval()'

	quietl = .false.
	if (present(quiet)) quietl = quiet

	! if_statement and while_statement may return an uninitialized type
	! otherwise if their conditions are false
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
		! This handles ints, bools, etc.
		res = node%val
		!print *, 'res = ', res%to_str()

	case (array_expr)

		!print *, 'evaluating array_expr'
		!print *, 'identifier = ', node%identifier%text

		if (node%val%array%kind == step_array) then

			lbound_ = syntax_eval(node%lbound, vars, fns, quietl)
			step    = syntax_eval(node%step  , vars, fns, quietl)
			ubound_ = syntax_eval(node%ubound, vars, fns, quietl)

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

				array%cap = (ubound_%sca%i32 - lbound_%sca%i32) / step%sca%i32 + 1
				!array%cap = (ubound_%sca%i32 - lbound_%sca%i32) / step%sca%i32
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

				! TODO: can cap be calculated correctly at the start, without
				! breaking tests? Find out which test crashes without commented
				! `+1` in above cap calculation
				if (array%len_ < array%cap) then
					array%cap = array%len_
					array%i32 = array%i32(1: array%len_)
				end if

			else if (array%type == i64_type) then

				array%cap = int((ubound_%sca%i64 - lbound_%sca%i64) / step%sca%i64 + 1)
				!array%cap = int((ubound_%sca%i64 - lbound_%sca%i64) / step%sca%i64)
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

				! TODO: can cap be calculated correctly at the start, without
				! breakign tests? Find out which test crashes without commented
				! `+1` in above cap calculation
				if (array%len_ < array%cap) then
					array%cap = array%len_
					array%i64 = array%i64(1: array%len_)
				end if

			else if (array%type == f32_type) then

				!print *, 'lbound_, ubound_ = ', lbound_%sca%f32, ubound_%sca%f32
				!print *, 'step = ', step%sca%f32

				array%cap = ceiling((ubound_%sca%f32 - lbound_%sca%f32) / step%sca%f32) + 1
				allocate(array%f32( array%cap ))

				j = 1
				f = lbound_%sca%f32
				if (lbound_%sca%f32 < ubound_%sca%f32 .neqv. 0 < step%sca%f32) f = ubound_%sca%f32

				do while ((f  < ubound_%sca%f32 .eqv. lbound_%sca%f32 < ubound_%sca%f32) &
				     .and. f /= ubound_%sca%f32)
					array%f32(j) = f
					f = f + step%sca%f32
					j = j + 1
				end do
				array%len_ = j - 1

				! TODO: can cap be calculated correctly at the start, without
				! breakign tests? Find out which test crashes without commented
				! `+1` in above cap calculation
				if (array%len_ < array%cap) then
					array%cap = array%len_
					array%f32 = array%f32(1: array%len_)
				end if

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
			lbound_ = syntax_eval(node%lbound, vars, fns, quietl)
			ubound_ = syntax_eval(node%ubound, vars, fns, quietl)
			len_   = syntax_eval(node%len_   , vars, fns, quietl)

			!array = new_array(node%val%array%type)
			!elem = lbound_

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

			! Initialize rank-2+ arrays
			if (allocated(node%size)) then

				array%rank = size( node%size )
				allocate(array%size( array%rank ))

				do i = 1, array%rank
					len_ = syntax_eval(node%size(i), vars, fns, quietl)
					array%size(i) = len_%to_i64()
					!print *, 'size['//str(i)//'] = ', array%size(i)
				end do

			end if

			! Constant-value impl arrays (i.e. every element has the same value
			! at initialization, but they are of course mutable)

			!print *, 'len array'
			lbound_ = syntax_eval(node%lbound, vars, fns, quietl)

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

			lbound_ = syntax_eval(node%lbound, vars, fns, quietl)
			ubound_ = syntax_eval(node%ubound, vars, fns, quietl)

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
				elem = syntax_eval(node%elems(i), vars, fns, quietl)
				!print *, 'elem['//str(i)//'] = ', elem%str()
				call array%push(elem)
			end do

			array%rank = size( node%size )
			allocate(array%size( array%rank ))
			!array%size = array%len_
			do i = 1, array%rank
				len_ = syntax_eval(node%size(i), vars, fns, quietl)
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

		else if (node%val%array%kind == expl_array) then
			!print *, 'expl_array'

			! Explicit rank-1 arrays

			! TODO: allow empty arrays?  Sub type of empty array?  Empty arrays
			! can currently be created like [0: -1];
			array = new_array(node%val%array%type, size(node%elems))

			do i = 1, size(node%elems)
				elem = syntax_eval(node%elems(i), vars, fns, quietl)
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
			!TODO
			write(*,*) err_int_prefix//'unexpected array kind'//color_reset
			call internal_error()
		end if

	case (for_statement)

		! TODO: this assumes for statement array range is i32/64 of the form
		! [imin: imax].  Generalize for other forms, maybe make an array%at()
		! method for shared use here for for_statement eval and above for
		! array_expr eval.  If possible, don't expand implicit arrays for for
		! loops

		! TODO: evaluate all of these ahead of loop, but only if they are
		! allocated!  Also eval array step, size, etc.
		if (allocated(node%array%lbound)) lbound_ = syntax_eval(node%array%lbound, vars, fns, quietl)
		if (allocated(node%array%step  )) step    = syntax_eval(node%array%step  , vars, fns, quietl)
		if (allocated(node%array%ubound)) ubound_ = syntax_eval(node%array%ubound, vars, fns, quietl)

		!print *, 'lbound type = ', kind_name(lbound_%type)
		!print *, 'ubound type = ', kind_name(ubound_%type)

		! push scope to make the loop iterator local
		call vars%push_scope()

		!if (any([lbound_%type, ubound_%type] == i64_type)) then
		!	itr%type = i64_type
		!else
		!	itr%type = i32_type
		!end if
		!!print *, 'itr%type = ', kind_name(itr%type)

		!if (.not. any (lbound_%type == [i32_type, i64_type])) then
		!	write(*,*) err_eval_i32_itr(node%identifier%text)
		!	call internal_error()
		!end if
		!if (.not. any (ubound_%type == [i32_type, i64_type])) then
		!	write(*,*) err_eval_i32_itr(node%identifier%text)
		!	call internal_error()
		!end if

		!print *, 'lbound_ = ', lbound_%to_i64()
		!print *, 'ubound_ = ', ubound_%to_i64()

		!type_ = node%val%array%type
		!val%type = type_

		!print *, 'node%array%type = ', kind_name(node%array%val%array%type)

		! Do promotion once before loop
		!print *, 'lbound type = ', kind_name(lbound_%type)
		!print *, 'ubound type = ', kind_name(ubound_%type)
		if (any(i64_type == [lbound_%type, ubound_%type])) then
			!print *, 'promoting'
			call promote_i32_i64(lbound_)
			call promote_i32_i64(ubound_)
			itr%type = i64_type
		else
			itr%type = i32_type

		! TODO: f32

		end if

		!print *, 'itr%type = ', kind_name(itr%type)

		!print *, 'lbound type = ', kind_name(lbound_%type)
		!print *, 'val%type    = ', kind_name(val%type)

		if (.not. any(itr%type == [i32_type, i64_type])) then
			write(*,*) err_int_prefix//'unit step array type eval not implemented'//color_reset
			call internal_error()
		end if

		len8 = ubound_%to_i64() - lbound_%to_i64()

		do i8 = 1, len8

			!itr = array_at(node%array, i8, lbound_, ubound_)
			call array_at(itr, node%array, i8, lbound_, ubound_)
			!print *, 'itr = ', itr%to_str()

			! During evaluation, insert variables by array id_index instead of
			! dict lookup.  This is much faster and can be done during
			! evaluation now that we know all of the variable identifiers.
			! Parsing still needs to rely on dictionary lookups because it does
			! not know the entire list of variable identifiers ahead of time
			vars%vals(node%id_index) = itr

			res = syntax_eval(node%body, vars, fns, quietl)

		end do

		call vars%pop_scope()

	case (while_statement)

		condition = syntax_eval(node%condition, vars, fns, quietl)
		do while (condition%sca%bool)
			res = syntax_eval(node%body, vars, fns, quietl)
			condition = syntax_eval(node%condition, vars, fns, quietl)
		end do

	case (if_statement)

		condition = syntax_eval(node%condition, vars, fns, quietl)
		!print *, 'condition = ', condition%str()

		if (condition%sca%bool) then
			!print *, 'if'
			res = syntax_eval(node%if_clause, vars, fns, quietl)

		else if (allocated(node%else_clause)) then
			!print *, 'else'
			res = syntax_eval(node%else_clause, vars, fns, quietl)

		end if

	case (translation_unit)

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

			res = syntax_eval(node%members(i), vars, fns, quietl)

			!print *, 'kind = ', node%members(i)%kind
			!print *, i, ' res = ', res%to_str()
			!print *, ''

			! HolyC feature: implicitly print name expression members.  I may
			! remove this after I implement an intrinsic print() fn.  May also
			! need to suppress this for void fn calls later
			if (node%members(i)%kind == name_expr .and. .not. quietl) then
				write(*,*) res%to_str()
			end if

		end do

		!call vars%pop_scope()

	case (block_statement)

		call vars%push_scope()

		! The final statement of a block returns the actual result.  Non-final
		! members only change the (vars) state.
		do i = 1, size(node%members)
			tmp = syntax_eval(node%members(i), vars, fns, quietl)

			!print *, 'kind = ', node%members(i)%kind
			!print *, i, ' tmp = ', tmp%to_str()
			!print *, 'type = ', tmp%type, kind_name(tmp%type)
			!print *, ''

			! In case of no-op if statements and while loops
			if (tmp%type /= unknown_type) res = tmp

			! HolyC feature: implicitly print name expression members.  I may
			! remove this after I implement an intrinsic print() fn.  May also
			! need to suppress this for void fn calls later
			if (node%members(i)%kind == name_expr .and. .not. quietl) then
				write(*,*) tmp%to_str()
			end if

		end do

		call vars%pop_scope()

	case (assignment_expr)

		if (.not. allocated(node%lsubscripts)) then

			!! This deallocation will cause a crash when an array appears on both
			!! the LHS and RHS of fn_call assignment, e.g. `dv = diff_(dv, i)` in
			!! AOC 2023/09
			!if (allocated(vars%vals)) then
			!if (allocated(vars%vals(node%id_index)%array)) then
			!	!print *, "deallocating lhs array"
			!	deallocate(vars%vals(node%id_index)%array)
			!end if
			!end if

			! Assign return value
			!print *, 'eval and set res'
			res = syntax_eval(node%right, vars, fns, quietl)

			! TODO: test int/float casting.  It should be an error during
			! parsing

			!print *, 'compound assign'
			!print *, 'lhs type = ', kind_name( vars%vals(node%id_index)%type )

			call compound_assign(vars%vals(node%id_index), res, node%op)

			! For compound assignment, ensure that the LHS is returned
			!print *, 'setting res again'
			res = vars%vals(node%id_index)
			!print *, 'done'

			! The difference between let and assign is inserting into the
			! current scope (let) vs possibly searching parent scopes (assign).
			! During evaluation we don't need any extra logic for scoping.  The
			! parser has already assigned a separate id_index for each
			! identifier at each scope level

		else
			!print *, 'LHS array subscript assignment'
			!print *, 'LHS type = ', kind_name(vars%vals(node%id_index)%array%type)  ! not alloc for str

			! Assign return value from RHS
			res = syntax_eval(node%right, vars, fns, quietl)

			!print *, 'RHS = ', res%to_str()

			i8 = subscript_eval(node, vars, fns, quietl)

			if (vars%vals(node%id_index)%type == str_type) then
				! TODO: ban compound character substring assignment
				vars%vals(node%id_index)%sca%str%s(i8+1: i8+1) = res%sca%str%s
			else

				!print *, 'LHS array type = ', &
				!	vars%vals(node%id_index)%array%type
				!print *, 'LHS array = ', vars%vals(node%id_index)%array%i32

				array_val = get_array_value_t(vars%vals(node%id_index)%array, i8)
				call compound_assign(array_val, res, node%op)
				call set_array_value_t( &
					vars%vals(node%id_index)%array, i8, array_val)
				res = array_val

			end if
		end if

	case (let_expr)

		! Assign return value
		res = syntax_eval(node%right, vars, fns, quietl)

		!print *, 'assigning identifier ', quote(node%identifier%text)
		vars%vals(node%id_index) = res

	case (fn_call_expr)

		!print *, 'eval fn_call_expr'
		!print *, 'fn identifier = ', node%identifier%text
		!print *, 'fn id_index   = ', node%id_index

		!res%type = fns%fns(node%id_index)%type
		res%type = node%val%type

		!print *, 'res type = ', res%type

		! Intrinsic fns
		select case (node%identifier%text)
		case ("exp")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%f32 = exp(arg1%sca%f32)

		case ("0min_i32")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%sca%i32

			! Note that min/max/println etc. are variadic, so we loop to
			! size(node%args) instead of size(node%params)

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i32 = min(res%sca%i32, arg%sca%i32)
			end do

		case ("0min_i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i64 = arg%sca%i64

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i64 = min(res%sca%i64, arg%sca%i64)
			end do

		case ("0max_i32")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%sca%i32

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i32 = max(res%sca%i32, arg%sca%i32)
			end do

		case ("0max_i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i64 = arg%sca%i64

			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%i64 = max(res%sca%i64, arg%sca%i64)
			end do

		case ("println")

			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(output_unit, '(a)', advance = 'no') arg%to_str()
			end do
			write(output_unit, *)

			!! TODO: what, if anything, should println return?
			!res%sca%i32 = 0

		case ("str")

			res%sca%str%s = ''
			do i = 1, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				res%sca%str%s = res%sca%str%s // arg%to_str()  ! TODO: use char_vector_t
			end do

		case ("len")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = len(arg%sca%str%s, 4)
			!res%sca%i32 = mylen( arg%sca%str%s )

		case ("parse_i32")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			read(arg%sca%str%s, *) res%sca%i32  ! TODO: catch iostat

		case ("parse_i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			read(arg%sca%str%s, *) res%sca%i64  ! TODO: catch iostat

		case ("0i32_sca")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = arg%to_i32()

		case ("0i32_arr")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			!res%sca%i32 = arg%to_i32()
			res%array = arg%to_i32_array()

		case ("i64")

			arg = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i64 = arg%to_i64()

		case ("open")

			arg = syntax_eval(node%args(1), vars, fns, quietl)

			! TODO: catch iostat, e.g. same file opened twice, folder doesn't
			! exist, etc.
			open(newunit = res%sca%file_%unit_, file = arg%sca%str%s)

			!print *, 'opened unit ', res%sca%file_%unit_
			res%sca%file_%name_ = arg%sca%str%s
			res%sca%file_%eof = .false.

		case ("readln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

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
			!!vars%vals(node%id_index) = res

			! TODO:  set eof flag or crash for other non-zero io
			if (io == iostat_end) then
			!if (io /= 0) then
				!arg1%sca%file_%eof = .true.
				vars%vals(node%args(1)%id_index)%sca%file_%eof = .true.
			end if
			!print *, 'eof   = ', arg1%sca%file_%eof

		case ("writeln")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, 'writing to unit ', arg1%sca%file_%unit_
			do i = 2, size(node%args)
				arg = syntax_eval(node%args(i), vars, fns, quietl)
				write(arg1%sca%file_%unit_, '(a)', advance = 'no') arg%to_str()
			end do
			write(arg1%sca%file_%unit_, *)

		case ("eof")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)

			!print *, "checking eof for unit", arg1%sca%file_%unit_
			res%sca%bool = arg1%sca%file_%eof

			!print *, 'eof fn = ', arg1%sca%file_%eof

		case ("close")
			arg = syntax_eval(node%args(1), vars, fns, quietl)
			!print *, 'closing unit ', arg%sca%file_%unit_
			close(arg%sca%file_%unit_)

		case ("exit")

			arg = syntax_eval(node%args(1), vars, fns, quietl)

			io = arg%sca%i32
			if (io == 0) then
				color = fg_bright_green
			else
				color = fg_bold_bright_red
			end if

			write(*,*) color//'Exiting syntran with status '// &
				str(io)//color_reset

			call exit(io)

		case ("size")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			arg2 = syntax_eval(node%args(2), vars, fns, quietl)

			if (arg2%sca%i32 < 0 .or. arg2%sca%i32 >= arg1%array%rank) then
				! TODO: re-think runtime errors.  A different prefix here
				! besides err_int_prefix helps, but context should be given if
				! possible like for parser/lexer error diagnostics
				write(*,*) err_rt_prefix//'rank mismatch in size() call'//color_reset
				call internal_error()
			end if

			res%sca%i64 = int(arg1%array%size( arg2%sca%i32 + 1 ))

		case ("count")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%i32 = count(arg1%array%bool)
			!deallocate(arg1%array)

		case ("all")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%bool = all(arg1%array%bool)

			! Might not be strictly necessary now that %array is allocatable
			! instead of pointable
			!deallocate(arg1%array)

		case ("any")

			arg1 = syntax_eval(node%args(1), vars, fns, quietl)
			res%sca%bool = any(arg1%array%bool)
			!deallocate(arg1%array)

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

				vars%vals( node%params(i) ) = &
					syntax_eval(node%args(i), vars, fns, quietl)

				!!print *, 'set tmp'
				!tmp = syntax_eval(node%args(i), vars, fns, quietl)
				!!print *, 'set param var'
				!vars%vals( node%params(i) ) = tmp

				!print *, 'done'
				!print *, ''
			end do

			res = syntax_eval(node%body, vars, fns, quietl)
			!print *, 'res = ', res%to_str()

			!do i = 1, size(node%params)
			!	if (allocated(vars%vals( node%params(i) )%array)) then
			!		!print *, 'deallocating node%params ... array'
			!		deallocate(vars%vals( node%params(i) )%array)
			!	end if
			!end do

		end select

	case (name_expr)
		!print *, 'searching identifier ', node%identifier%text

		if (allocated(node%lsubscripts) .and. &
			vars%vals(node%id_index)%type == str_type) then
			!print *, 'string subscript RHS name expr'

			!print *, 'str type'
			res%type = vars%vals(node%id_index)%type

			select case (node%lsubscripts(1)%sub_kind)
			case (scalar_sub)
				i8 = subscript_eval(node, vars, fns, quietl)
				!print *, 'i8 = ', i8
				res%sca%str%s = vars%vals(node%id_index)%sca%str%s(i8+1: i8+1)

			case (range_sub)

				! TODO: str all_sub

				il = subscript_eval(node, vars, fns, quietl) + 1

				! This feels inconsistent and not easy to extend to higher ranks
				right = syntax_eval(node%usubscripts(1), vars, fns, quietl)
				iu = right%sca%i32 + 1

				!print *, ''
				!print *, 'identifier ', node%identifier%text
				!print *, 'il = ', il
				!print *, 'iu = ', iu
				!print *, 'str = ', vars%vals(node%id_index)%sca%str%s

				! Not inclusive of upper bound
				res%sca%str%s = vars%vals(node%id_index)%sca%str%s(il: iu-1)

			case default
				write(*,*) err_int_prefix//'unexpected subscript kind'//color_reset
				call internal_error()
			end select

		else if (allocated(node%lsubscripts)) then

			if (vars%vals(node%id_index)%type /= array_type) then
				write(*,*) err_int_prefix//'bad type, expected array'//color_reset
				call internal_error()
			end if

			!print *, 'sub kind 1 = ', kind_name(node%lsubscripts(1)%sub_kind)
			!print *, 'rank = ', node%val%array%rank

			if (all(node%lsubscripts%sub_kind == scalar_sub)) then

				! This could probably be lumped in with the range_sub case now
				! that I have it fully generalized
				i8 = subscript_eval(node, vars, fns, quietl)
				!print *, 'i8 = ', i8
				res = get_array_value_t(vars%vals(node%id_index)%array, i8)

			else

				rank = vars%vals(node%id_index)%array%rank
				allocate(lsubs(rank), usubs(rank))
				rank_res = 0
				do i = 1, rank

					if (node%lsubscripts(i)%sub_kind == all_sub) then
						lsubs(i) = 0
						!print *, 'lsubs(i) = ', lsubs(i)
					else
						lsubval = syntax_eval(node%lsubscripts(i), vars, fns, quietl)
						lsubs(i) = lsubval%sca%i32
					end if

					select case (node%lsubscripts(i)%sub_kind)
					case (all_sub)
						usubs(i) = vars%vals(node%id_index)%array%size(i)
						!print *, 'usubs(i) = ', usubs(i)

						rank_res = rank_res + 1

					case (range_sub)
						usubval = syntax_eval(node%usubscripts(i), vars, fns, quietl)
						usubs(i) = usubval%sca%i32

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
				do idim_ = 1, rank
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

					! subscript_eval() inlined.  is there a way to copy a slice
					! without doing so much math?
					!
					! TODO: bound checking if enabled.  unlike subscript_eval(),
					! we can do it here outside the i8 loop
					prod  = 1
					index_ = 0
					do j = 1, rank
						!print *, 'j = ', j
						index_ = index_ + prod * subs(j)
						prod  = prod * vars%vals(node%id_index)%array%size(j)
					end do
					!print *, 'index_ = ', index_

					call set_array_value_t(res%array, i8, &
					     get_array_value_t(vars%vals(node%id_index)%array, index_))

					! get next subscript.  this is the bignum += 1 algorithm but
					! in an arbitrary mixed radix
					j = 1
					do while (j < rank .and. subs(j) == usubs(j) - 1)
						subs(j) = lsubs(j)
						j = j + 1
					end do
					subs(j) = subs(j) + 1

				end do
			end if

		else
			!print *, 'name expr'
			res = vars%vals(node%id_index)

			! Deep copy of whole array instead of aliasing pointers
			if (res%type == array_type) then
				!print *, 'array  name_expr'

				if (allocated(res%array)) deallocate(res%array)

				allocate(res%array)
				res%type = array_type
				res%array = vars%vals(node%id_index)%array

			!else
			!	print *, 'scalar name_expr'
			end if

		end if

	case (unary_expr)

		right = syntax_eval(node%right, vars, fns, quietl)
		!print *, 'right = ', right

		res%type = right%type

		! TODO: add fallback type checking here

		select case (node%op%kind)
		case (plus_token)
			res = right

		case (minus_token)
			select case (right%type)
				case (i32_type)
					res%sca%i32 = -right%sca%i32
				case (i64_type)
					res%sca%i64 = -right%sca%i64
				case (f32_type)
					res%sca%f32 = -right%sca%f32
			end select

		case (not_keyword)
			res%sca%bool = .not. right%sca%bool

		case default
			write(*,*) err_eval_unary_op(node%op%text)
			call internal_error()
		end select

	case (binary_expr)

		left  = syntax_eval(node%left , vars, fns, quietl)
		right = syntax_eval(node%right, vars, fns, quietl)

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
			res%sca%bool = left%sca%bool .and. right%sca%bool

		case (or_keyword)
			res%sca%bool = left%sca%bool .or.  right%sca%bool

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

	case default
		write(*,*) err_eval_node(kind_name(node%kind))
		call internal_error()

	end select

end function syntax_eval

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
		lhs = rhs  ! simply overwrite

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

function subscript_eval(node, vars, fns, quietl) result(index_)

	! Evaluate subscript indices and convert a multi-rank subscript to a rank-1
	! subscript index_

	type(syntax_node_t) :: node
	type(vars_t) :: vars
	type(fns_t) :: fns
	logical, intent(in) :: quietl

	integer(kind = 8) :: index_

	!******

	integer :: i
	integer(kind = 8) :: prod
	type(value_t) :: subscript

	!print *, 'starting subscript_eval()'

	! str scalar with single char subscript
	if (vars%vals(node%id_index)%type == str_type) then
		subscript = syntax_eval(node%lsubscripts(1), vars, fns, quietl)
		index_ = subscript%to_i64()
		return
	end if

	!if (vars%vals(node%id_index)%type /= array_type) then
	!	! internal_error?
	!end if

	prod  = 1
	index_ = 0
	do i = 1, vars%vals(node%id_index)%array%rank
		!print *, 'i = ', i

		subscript = syntax_eval(node%lsubscripts(i), vars, fns, quietl)

		! TODO: bound checking? by default or enabled with cmd line flag?
		!
		! I think the only way to do it without killing perf is by having bound
		! checking turned off in release, and setting a compiler macro
		! definition to enable it only in debug

		index_ = index_ + prod * subscript%to_i64()
		prod  = prod * vars%vals(node%id_index)%array%size(i)

	end do

end function subscript_eval

!===============================================================================

subroutine array_at(val, node, i, lbound_, ubound_)

	type(value_t), intent(inout) :: val

	type(syntax_node_t), intent(in) :: node

	integer(kind = 8), intent(in) :: i

	type(value_t), intent(inout) :: lbound_, ubound_

	!*********

	!integer :: type_

	if (val%type == i32_type) then
		val%sca%i32 = lbound_%sca%i32 + i - 1
	else !if (val%type == i64_type) then
		val%sca%i64 = lbound_%sca%i64 + i - 1

	! TODO: f32

	end if

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

