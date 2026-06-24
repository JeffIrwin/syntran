
!===============================================================================

module syntran__intr_vars_m

	use syntran__types_m

	implicit none

	! Number of pre-declared std:: constants.  Slots 1..NUM_INTR_VARS are
	! reserved in the vars array before any user variable declarations.
	integer, parameter :: NUM_INTR_VARS = 1

!===============================================================================

contains

!===============================================================================

subroutine declare_intr_vars(vars)

	! Insert std:: constants into the vars dict for type-checking during parsing.
	! Only type info matters here; runtime values come from populate_intr_vars().
	! id_index 1 is always reserved for std::PI.

	type(vars_t), intent(inout) :: vars

	!********

	type(value_t) :: val
	integer :: io

	val%type = f64_type
	call vars%insert("std::PI", val, 1, io)

end subroutine declare_intr_vars

!===============================================================================

subroutine populate_intr_vars(vals)

	! Set std:: constant runtime values in the flat eval array.
	! This is the single source of truth for the actual constant values.
	! Idempotent — safe to call after REPL vars restore.

	type(value_t), intent(inout) :: vals(:)

	vals(1)%type    = f64_type
	vals(1)%sca%f64 = 3.14159265358979323846_8

end subroutine populate_intr_vars

!===============================================================================

end module syntran__intr_vars_m

!===============================================================================
