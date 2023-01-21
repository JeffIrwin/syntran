
!===============================================================================

module syntran

	! This module contains the public API of syntran

	implicit none

!===============================================================================

contains

!===============================================================================

function syntran_interpret(str, quiet) result(res_str)

	! This is the interactive interpreter shell
	!
	! To interpret a whole file all at once, use syntran_interpret_file()
	! instead
	!
	! Interpret stdin by default, or interpret the multi-line string str if it
	! is given.  The return value res_str is the result of the final expression
	! (like how Rust doesn't have a return statement, but fns just return the
	! final expression in their body)
	!
	! Using the str arg is deprecated here.  Prefer eval() or interpret_file().
	! However, it's still useful for testing to have something that evals 1 line
	! at a time, so that we can have automatic test coverage of weird
	! interactive interpreter edge cases

	use core_m
	use utils

	character(len = *), intent(in), optional :: str
	logical, intent(in), optional :: quiet

	character(len = :), allocatable :: res_str

	!********

	character(len = :), allocatable :: line, src_file
	character(len = *), parameter :: prompt = lang_name//'$ '

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	logical :: quietl, cont, show_tree
	logical, parameter :: allow_cont = .true.

	type(string_view_t) :: sv

	type(fns_t) :: fns
	type(syntax_node_t) :: compilation
	type(value_t) :: res
	type(vars_t) :: vars

	!print *, 'starting syntran_interpret()'
	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	src_file = '<stdin>'
	cont = .false.
	show_tree = .false.

	if (present(str)) then
		! Append a trailing line feed in case it does not exist
		sv = new_string_view(str//line_feed)
		src_file = '<string>'
	end if

	quietl = .false.
	if (present(quiet)) quietl = quiet

	fns = declare_intrinsic_fns()

	! Read-eval-print-loop
	do

		if (present(str)) then

			! Interpret multi-line strings one line at a time to mock the
			! interpreter getting continued stdin lines.  If you know your whole
			! string ahead of time, just use syntran_eval() instead
			line = sv%get_line(iostat = io)

		else

			if (cont) then

				! If expecting more characters, re-parse the whole line from the
				! beginning.  An alternative implementation would not append but
				! only pass the new characters, but also pass the previous
				! compilation tree to append to the tree instead of appending
				! characters.  This way seemed easier :shrug:

				! TODO: add a directive option to hide expected char hint

				! Bash uses `$` for the inital prompt and `>` for continued
				! prompts.  So do we
				write(ou, '(a)', advance = 'no') &
					'[Hint `'//compilation%first_expected//'`]> '

				!write(ou, '(a)', advance = 'no') '> '

				line = line//line_feed//read_line(iu, iostat = io)

			else
				write(ou, '(a)', advance = 'no') prompt
				line = read_line(iu, iostat = io)
			end if

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
		!   - #reset or #clear to clear vars
		!   - #hint to toggle hint

		if (line == '#tree') then
			show_tree = .not. show_tree
			cycle
		end if

		res_str = ' '
		compilation = syntax_parse(line, vars, fns, src_file, allow_cont)
		!print *, 'in interpreter'

		!print *, 'compilation%expecting = ', compilation%expecting

		!print *, 'allocated(vars%dicts(1)%root) = ', &
		!	allocated(vars%dicts(1)%root)

		! Continue current parse with next line since more chars are expected
		cont = compilation%expecting

		!if (cont .and. present(str)) exit
		if (cont) cycle

		if (compilation%is_empty) cycle

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0 .or. show_tree) print *, 'tree = ', compilation%str()

		if (.not. quietl) call compilation%log_diagnostics(ou)

		! Don't try to evaluate with errors
		if (compilation%diagnostics%len > 0) cycle

		res  = syntax_eval(compilation, vars, fns)

		! Consider MATLAB-style "ans = " log?
		res_str = res%str()
		if (.not. present(str)) write(ou, '(a)') res_str

	end do

	!print *, 'done syntran_interpret()'

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

integer function syntran_eval_i32(str) result(eval_i32)

	use core_m

	character(len = *), intent(in) :: str

	type(fns_t) :: fns
	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(vars_t) :: vars

	fns = declare_intrinsic_fns()
	tree = syntax_parse(str, vars, fns)
	call tree%log_diagnostics()

	if (tree%diagnostics%len > 0) then
		! TODO: iostat
		eval_i32 = 0
		return
	end if

	val = syntax_eval(tree, vars, fns)

	! TODO: check kind, add optional iostat arg
	eval_i32 = val%i32

end function syntran_eval_i32

!===============================================================================

real(kind = 4) function syntran_eval_f32(str) result(eval_f32)

	use core_m

	character(len = *), intent(in) :: str

	type(fns_t) :: fns
	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(vars_t) :: vars

	fns = declare_intrinsic_fns()
	tree = syntax_parse(str, vars, fns)
	call tree%log_diagnostics()

	if (tree%diagnostics%len > 0) then
		! TODO: iostat
		eval_f32 = 0
		return
	end if

	val = syntax_eval(tree, vars, fns)

	! TODO: check kind, add optional iostat arg
	eval_f32 = val%f32
	!print *, 'eval_f32 = ', eval_f32

end function syntran_eval_f32

!===============================================================================

function syntran_eval(str, quiet, src_file) result(res)

	use core_m

	character(len = *), intent(in)  :: str
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet
	character(len = *), optional, intent(in)  :: src_file

	!********

	character(len = :), allocatable :: src_filel

	logical :: quietl

	type(fns_t) :: fns
	type(syntax_node_t) :: tree
	type(value_t) :: val
	type(vars_t) :: vars

	quietl = .false.
	if (present(quiet)) quietl = quiet

	src_filel = '<stdin>'
	if (present(src_file)) src_filel = src_file

	! TODO: make a helper fn here that all the eval_* fns use

	fns = declare_intrinsic_fns()
	tree = syntax_parse(str, vars, fns, src_filel)
	if (.not. quietl) call tree%log_diagnostics()

	if (tree%diagnostics%len > 0) then
		res = ''
		return
	end if

	val = syntax_eval(tree, vars, fns, quietl)
	res = val%str()

end function syntran_eval

!===============================================================================

function syntran_interpret_file(file, quiet) result(res)

	! TODO:
	!   - enable input echo for file input (not for stdin)
	!   - echo inputs w/o "syntran$" prompt and print outputs after a comment,
	!     for ease of updating documentation with consistent styling

	use core_m

	character(len = *), intent(in)  :: file
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet

	!********

	character(len = :), allocatable :: source_text
	integer :: iostat
	logical :: quietl

	quietl = .false.
	if (present(quiet)) quietl = quiet

	if (.not. quietl) write(*,*) 'Interpreting file "'//file//'"'

	source_text = read_file(file, iostat)
	if (iostat /= exit_success) then
		if (.not. quietl) write(*,*) err_404(file)
		return
	end if

	res = trim(adjustl(syntran_eval(source_text, quiet, file)))

end function syntran_interpret_file

!===============================================================================

end module syntran

!===============================================================================

