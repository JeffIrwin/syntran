
!===============================================================================

module syntran__vm_m

	! Stack-based bytecode virtual machine.
	!
	! Module + submodule layout mirrors runtime.f90 + runtime_*.f90.
	! Submodules:
	!   vm_exec.f90  - main dispatch loop (vm_run)

	use syntran__bytecode_m
	use syntran__runtime_m

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

		! Implemented in vm_intr.f90.  format() with its pre-split format string
		! (program_t%fmts), which vm_call_intr doesn't receive
		module subroutine vm_format(fmt, args, res)
			type(fmt_t), intent(in) :: fmt
			type(value_t), intent(in) :: args(:)
			type(value_t), intent(out) :: res
		end subroutine

		! Implemented in vm_intr.f90.  Dispatch for intrinsics with by-ref args
		! (fn_t%ref_params), which unlike vm_call_intr may modify args(:).  The
		! caller moves them back into their variables afterwards
		module subroutine vm_call_intr_ref(intr_id, nargs, args, state, res)
			integer, intent(in) :: intr_id, nargs
			type(value_t), intent(inout) :: args(:)
			type(state_t), intent(inout) :: state
			type(value_t), intent(out) :: res
		end subroutine

	end interface

!===============================================================================

end module syntran__vm_m

!===============================================================================
