
!===============================================================================

module syntran

	! This module contains the public API of syntran

	implicit none

!===============================================================================

contains

!===============================================================================

function syntran_interpret(str, quiet) result(res_str)

	! This is the interpreter shell
	!
	! Interpret stdin by default, or interpret the multi-line string str if it
	! is given.  The return value res_str is the result of the final expression
	! (like how Rust doesn't have a return statement, but fns just return the
	! final expression in their body)
	!
	! TODO: add quiet arg for bad syntax testing
	!
	! TODO: another optional arg for iu as stdin vs another file:
	!   - enable input echo for file input (not for stdin)
	!   - write file name and line num for diagnostics
	!   - echo inputs w/o "syntran$" prompt and print outputs after a comment,
	!     for ease of updating documentation with consistent styling

	use core_m
	use utils

	character(len = *), intent(in), optional :: str
	logical, intent(in), optional :: quiet

	character(len = :), allocatable :: res_str

	!********

	character(len = :), allocatable :: line
	character(len = *), parameter :: prompt = lang_name//'$ '

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	logical :: quietl, show_tree = .false.

	type(string_view_t) :: sv

	type(syntax_node_t) :: tree
	type(value_t) :: res
	type(variable_dictionary_t) :: variables

	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	if (present(str)) then
		! Append a trailing line feed in case it does not exist
		sv = new_string_view(str//line_feed)
	end if

	quietl = .false.
	if (present(quiet)) quietl = quiet

	! Read-eval-print-loop
	do

		if (present(str)) then

			! TODO: I don't have an end-of-statement token yet (;), so interpret
			! multi-line strings one line at a time for now.  Whatever I end up
			! doing has to work with both strings and stdin, so I may need
			! a continue iostat for syntax_parse to continue parsing the same
			! tree through multiple input lines
			!
			! Better yet, wrap entire str in a global {block} and just do
			! a single syntax_parse() call.  Continue logic is still needed for
			! stdin interpreter.

			line = sv%get_line(iostat = io)

		else
			write(ou, '(a)', advance = 'no') prompt
			line = read_line(iu, iostat = io)
		end if

		!print *, 'line = <', line, '>'
		!print *, 'io = ', io

		!! Echo input?
		!write(ou, '(a)') line

		if (io == iostat_end) exit

		! TODO:
		!
		! More directives:
		!   - #help
		!   - #reset or #clear to clear variables

		if (line == '#tree') then
			show_tree = .not. show_tree
			cycle
		end if

		res_str = ''
		tree = syntax_parse(line, variables)

		if (tree%is_empty) cycle

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0 .or. show_tree) print *, 'tree = ', tree%str()

		if (.not. quietl) call tree%log_diagnostics(line, ou)

		! Don't try to evaluate with errors
		if (tree%diagnostics%len > 0) cycle

		res  = syntax_eval(tree, variables)

		! Consider MATLAB-style "ans = " log?
		res_str = res%str()
		if (.not. present(str)) write(ou, '(a)') res_str

	end do

end function syntran_interpret

!===============================================================================

subroutine syntran_banner()

	use core_m

	character(len = :), allocatable :: version
	character(len = 16) :: major, minor, patch

	write(major, '(i0)') syntran_major
	write(minor, '(i0)') syntran_minor
	write(patch, '(i0)') syntran_patch

	version = &
		str(syntran_major)//'.'// &
		str(syntran_minor)//'.'// &
		str(syntran_patch)

	write(*,*)
	write(*,*) lang_name//' '//version
	write(*,*) 'https://github.com/JeffIrwin/syntran'
	write(*,*)
	write(*,*) 'Usage:'
	write(*,*) tab//'#tree to toggle tree display'
	write(*,*) tab//'Ctrl+C to exit'
	write(*,*)

	! TODO: add #help directive and arg for more in depth info

end subroutine syntran_banner

!===============================================================================

integer function syntran_eval_int(str) result(eval_int)

	use core_m

	character(len = *), intent(in) :: str

	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(variable_dictionary_t) :: variables

	tree = syntax_parse(str, variables)
	call tree%log_diagnostics(str)

	if (tree%diagnostics%len > 0) then
		! TODO: iostat
		eval_int = 0
		return
	end if

	val = syntax_eval(tree, variables)

	! TODO: check kind, add optional iostat arg
	eval_int = val%ival

end function syntran_eval_int

!===============================================================================

function syntran_eval(str, quiet) result(res)

	use core_m

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet

	!********

	logical :: quietl

	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(variable_dictionary_t) :: variables

	quietl = .false.
	if (present(quiet)) quietl = quiet

	!! One-liner, but no error handling.  This can crash unit tests without
	!! reporting failures
	!eval = syntax_eval(syntax_parse(str, variables), variables)

	! TODO: make a helper fn here that all the eval_* fns use

	tree = syntax_parse(str, variables)
	if (.not. quietl) call tree%log_diagnostics(str)

	if (tree%diagnostics%len > 0) then
		res = ''
		return
	end if

	val = syntax_eval(tree, variables)
	res = val%str()

end function syntran_eval

!===============================================================================

end module syntran

!===============================================================================

