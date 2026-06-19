
!===============================================================================

module syntran__vm_m

	! Stack-based bytecode virtual machine.
	!
	! Module + submodule layout mirrors eval.f90 + eval_*.f90.
	! Submodules:
	!   vm_exec.f90  - main dispatch loop (vm_run)

	use syntran__bytecode_m
	use syntran__eval_m

	implicit none

!===============================================================================

	interface

		! Implemented in vm_exec.f90

		module subroutine vm_run(prog, state, res)
			type(program_t), intent(in) :: prog
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

		! Implemented in vm_intr.f90
		module subroutine vm_call_intr(intr_id, nargs, args, state, res)
			integer, intent(in) :: intr_id, nargs
			type(value_t), intent(in) :: args(:)
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

	end interface

!===============================================================================

end module syntran__vm_m

!===============================================================================
