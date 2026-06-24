
!===============================================================================

module syntran__intr_vars_m

	use syntran__types_m
	use iso_fortran_env, only: input_unit, output_unit, error_unit

	implicit none

	! Number of pre-declared std:: constants.  Slots 1..NUM_INTR_VARS are
	! reserved in the vars array before any user variable declarations.
	! Slot 1 = std::PI, slots 2-4 = std::IN/OUT/ERR
	integer, parameter :: NUM_INTR_VARS = 4

!===============================================================================

contains

!===============================================================================

subroutine declare_intr_vars(vars)

	! Insert std:: constants into the vars dict for type-checking during parsing.
	! Only type info matters here; runtime values come from populate_intr_vars().
	! Slot 1 = std::PI, slots 2-4 = std::IN/OUT/ERR.

	type(vars_t), intent(inout) :: vars

	!********

	type(value_t) :: val
	integer :: io

	val%type = f64_type
	call vars%insert("std::PI", val, 1, io)

	val%type = file_type
	call vars%insert("std::IN",  val, 2, io)
	call vars%insert("std::OUT", val, 3, io)
	call vars%insert("std::ERR", val, 4, io)

end subroutine declare_intr_vars

!===============================================================================

subroutine populate_intr_vars(vals)

	! Set std:: constant runtime values in the flat eval array.
	! This is the single source of truth for the actual constant values.
	! Idempotent — safe to call after REPL vars restore.

	type(value_t), intent(inout) :: vals(:)

	vals(1)%type    = f64_type
	vals(1)%sca%f64 = 3.14159265358979323846_8

	vals(2)%type = file_type
	if (.not. allocated(vals(2)%file_)) allocate(vals(2)%file_)
	vals(2)%file_%name_       = "stdin"
	vals(2)%file_%unit_       = input_unit
	vals(2)%file_%mode_read   = .true.
	vals(2)%file_%mode_write  = .false.
	vals(2)%file_%is_open     = .true.
	vals(2)%file_%eof         = .false.
	vals(2)%file_%is_std      = .true.

	vals(3)%type = file_type
	if (.not. allocated(vals(3)%file_)) allocate(vals(3)%file_)
	vals(3)%file_%name_       = "stdout"
	vals(3)%file_%unit_       = output_unit
	vals(3)%file_%mode_read   = .false.
	vals(3)%file_%mode_write  = .true.
	vals(3)%file_%is_open     = .true.
	vals(3)%file_%eof         = .false.
	vals(3)%file_%is_std      = .true.

	vals(4)%type = file_type
	if (.not. allocated(vals(4)%file_)) allocate(vals(4)%file_)
	vals(4)%file_%name_       = "stderr"
	vals(4)%file_%unit_       = error_unit
	vals(4)%file_%mode_read   = .false.
	vals(4)%file_%mode_write  = .true.
	vals(4)%file_%is_open     = .true.
	vals(4)%file_%eof         = .false.
	vals(4)%file_%is_std      = .true.

end subroutine populate_intr_vars

!===============================================================================

end module syntran__intr_vars_m

!===============================================================================
