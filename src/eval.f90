
!===============================================================================

module syntran__eval_m

	use iso_fortran_env

	use syntran__bool_m
	use syntran__math_m

	! consider grouping/encapsulating in a math bitwise module?
	use syntran__math_left_shift_m
	use syntran__math_right_shift_m
	use syntran__math_bit_xor_m
	use syntran__math_bit_or_m
	use syntran__math_bit_and_m
	use syntran__math_bit_not_m

	use syntran__types_m

	implicit none

	!********

	type state_t
		! Run time (eval time) state

		logical :: quiet

		type(fns_t) :: fns

		!type(structs_t) :: structs

		type(vars_t) :: vars, locs

		! Grammatically, "breaked" should be "broke", but it's going to be a
		! nightmare if you grep for "break" and don't find "broke"
		logical :: returned, breaked, continued

		! Script arguments passed after `--` on the command line
		type(string_vector_t) :: script_args

	end type state_t

	!********

	interface
		! Implemented in eval_expr.f90

		recursive module subroutine eval_binary_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_name_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		module subroutine eval_dot_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_unary_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		module subroutine promote_i32_i64(val)
			type(value_t), intent(inout) :: val
		end subroutine

	end interface

	interface
		! Implemented in eval_array.f90

		recursive module subroutine set_val(node, var, state, val, index_)
			type(syntax_node_t), intent(in) :: node
			type(value_t), intent(inout) :: var
			type(state_t), intent(inout) :: state
			type(value_t), intent(in) :: val
			integer(kind = 8), optional, intent(in) :: index_
		end subroutine

		recursive module subroutine get_val(node, var, state, res, index_)
			type(syntax_node_t), intent(in) :: node
			type(value_t), intent(in) :: var
			type(state_t), intent(inout) :: state
			integer(kind = 8), optional, intent(in) :: index_
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_struct_instance(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		module subroutine allocate_array(val, cap)
			type(value_t), intent(inout) :: val
			integer(kind = 8), intent(in) :: cap
		end subroutine

		module function new_array(type, cap) result(vector)
			integer, intent(in) :: type
			integer, intent(in), optional :: cap
			type(array_t) :: vector
		end function

		module subroutine compound_assign(lhs, rhs, op)
			type(value_t), intent(inout) :: lhs
			type(value_t), intent(in) :: rhs
			type(syntax_token_t), intent(in) :: op
		end subroutine

		module subroutine get_subscript_range(node, state, asubs, lsubs, ssubs, usubs, rank_res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(i64_vector_t), allocatable, intent(out) :: asubs(:)
			integer(kind = 8), allocatable, intent(out) :: lsubs(:), ssubs(:), usubs(:)
			integer, intent(out) :: rank_res
		end subroutine

		module subroutine get_next_subscript(asubs, lsubs, ssubs, usubs, subs)
			type(i64_vector_t), intent(in), allocatable :: asubs(:)
			integer(kind = 8), intent(in) :: lsubs(:), ssubs(:)
			integer(kind = 8), intent(inout) :: usubs(:), subs(:)
		end subroutine

		module function subscript_i32_eval(subs, array) result(index_)
			integer(kind = 8), intent(in) :: subs(:)
			type(array_t) :: array
			integer(kind = 8) :: index_
		end function

		module function sub_eval(node, var, state) result(index_)
			type(syntax_node_t) :: node
			type(value_t) :: var
			type(state_t), intent(inout) :: state
			integer(kind = 8) :: index_
		end function

		recursive module function subscript_eval(node, state) result(index_)
			type(syntax_node_t) :: node
			type(state_t), intent(inout) :: state
			integer(kind = 8) :: index_
		end function

		module subroutine array_at(val, kind_, i, lbound_, step, ubound_, len_, array, &
				elems, str_, state)
			type(value_t), intent(inout) :: val
			integer, intent(in) :: kind_
			integer(kind = 8), intent(in) :: i
			type(value_t), intent(in) :: lbound_, step, ubound_, len_
			type(array_t), intent(in) :: array
			type(syntax_node_t), allocatable :: elems(:)
			type(value_t), intent(in) :: str_
			type(state_t), intent(inout) :: state
		end subroutine

		module subroutine get_array_val(array, i, val)
			type(array_t), intent(in) :: array
			integer(kind = 8), intent(in) :: i
			type(value_t), intent(out) :: val
		end subroutine

		module subroutine set_array_val(array, i, val)
			type(array_t), intent(inout) :: array
			integer(kind = 8), intent(in) :: i
			type(value_t), intent(in) :: val
		end subroutine

	end interface

	interface
		! Implemented in eval_fn.f90

		recursive module subroutine eval_fn_call(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_fn_call_intr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

	end interface

	interface
		! Implemented in eval_control.f90

		recursive module subroutine eval_for_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_assignment_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		module subroutine eval_translation_unit(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_array_expr(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_while_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_if_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_return_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		recursive module subroutine eval_block_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		module subroutine eval_use_statement(node, state, res)
			type(syntax_node_t), intent(in) :: node
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

	end interface

!===============================================================================

contains

!===============================================================================

!function divceil(num, den) result(res)
elemental function divceil(num, den) result(res)

	! Integer division ceiling
	!
	! I initially made this elemental so I could call product() on a vector
	! result, but I need to loop and select case for index arr_sub anyway so
	! just a scalar fn would've sufficed
	!
	! TODO: move to utils?

	integer(kind = 8), intent(in) :: num, den
	integer(kind = 8) :: res

	! I basically have to divide integers and take the ceiling (not floor) here.
	! There are methods that work for positive ints but fail for negatives.  In
	! C you can do it by casting bools to ints (ew)

	res = num / den
	if (mod(num, den) /= 0) res = res + 1  ! TODO: sign? -1 if negative? tests seem ok

	!!!if (num < 0 .and. den

	!print *, "num, den, ceil(num/den) = ", num, den, res

end function divceil

!===============================================================================

recursive subroutine syntax_eval(node, state, res)

	! TODO: add diagnostics to state for runtime errors (bounds overflow, rank
	! mismatch, etc.)

	type(syntax_node_t), intent(in) :: node

	type(state_t), intent(inout) :: state

	! I experimented with making res intent(inout) in commit 993345ad, but it
	! had a negative imact on perf, making gfortran about twice as slow on aoc
	! tests, likely due to the extra work of checking `if allocated(...)
	! deallocate` in lots of places
	type(value_t), intent(out) :: res

	!********

	integer :: id

	!print *, "starting syntax_eval()"
	!print *, "node kind = ", kind_name(node%kind)

	if (node%is_empty) return

	!********

	! I'm being a bit loose with consistency on select case indentation but
	! I don't want a gigantic diff

	select case (node%kind)

	case (literal_expr)
		res = node%val  ! this handles ints, bools, etc.

	case (array_expr)
		call eval_array_expr(node, state, res)

	case (for_statement)
		call eval_for_statement(node, state, res)

	case (while_statement)
		call eval_while_statement(node, state, res)

	case (if_statement)
		call eval_if_statement(node, state, res)

	case (return_statement)
		call eval_return_statement(node, state, res)

	case (break_statement)
		state%breaked = .true.
		!call eval_break_statement(node, state, res)

	case (continue_statement)
		! No need for a subroutine (with 2 unused args) for 1 line
		state%continued = .true.
		!call eval_continue_statement(node, state, res)

	case (translation_unit)
		call eval_translation_unit(node, state, res)

	case (use_statement)
		call eval_use_statement(node, state, res)

	case (block_statement)
		call eval_block_statement(node, state, res)

	case (assignment_expr)
		call eval_assignment_expr(node, state, res)

	case (let_expr)

		! Assign return value
		call syntax_eval(node%right, state, res)

		!print *, 'assigning identifier ', quote(node%identifier%text)
		!print *, "is_loc = ", node%is_loc

		id = node%id_index
		if (node%is_loc) then
			state%locs%vals(id) = res
		else
			state%vars%vals(id) = res
		end if

		!print *, "res type = ", kind_name(res%type)
		!print *, "allocated(struct) = ", allocated(res%struct)
		!if (res%type == struct_type) then
		!	print *, "size struct = ", size(res%struct)
		!	print *, "size struct = ", size( state%vars%vals(id)%struct )
		!	do i = 1, size(res%struct)
		!		print *, "struct[", str(i), "] = ", res%struct(i)%to_str()
		!		print *, "struct[", str(i), "] = ", state%vars%vals(id)%struct(i)%to_str()
		!	end do
		!end if

	case (fn_call_expr)  ! user-defined
		call eval_fn_call(node, state, res)

	case (fn_call_intr_expr)
		call eval_fn_call_intr(node, state, res)

	case (struct_instance_expr)
		call eval_struct_instance(node, state, res)

	case (name_expr)
		!print *, "name_expr"
		call eval_name_expr(node, state, res)

	case (dot_expr)
		call eval_dot_expr(node, state, res)

	case (unary_expr)
		call eval_unary_expr(node, state, res)

	case (binary_expr)
		call eval_binary_expr(node, state, res)

	case default
		write(*,*) err_eval_node(kind_name(node%kind))
		call internal_error()

	end select

end subroutine syntax_eval

!===============================================================================

end module syntran__eval_m

!===============================================================================
