
!===============================================================================

module syntran__repl_m

	! REPL-only `#`-directives (#help, #tree, #hint, #cancel, #clear).  Split
	! out of syntran.f90's syntran_interpret() so the directive text and the
	! REPL loop's control flow don't have to live in the same big function.
	!
	! repl_directive() only reads state (for `#help fns`/`#help vars`) and
	! toggles the two logicals it owns by reference.  It cannot itself tear
	! down and reinitialize state_t for `#clear`, because init_state() lives
	! in syntran.f90 (module `syntran`), which sits *above* this module in
	! the dependency order -- so `#clear` is signaled back to the caller via
	! the DIR_CLEAR action code instead, and syntran_interpret() performs the
	! actual reset

	use syntran__core_m
	use syntran__line_edit_m

	implicit none

	! repl_directive() return codes
	integer, parameter :: &
		DIR_NONE   = 0, & ! chunk was not a directive; parse it normally
		DIR_HANDLED = 1, & ! directive fully handled here (#help, #tree, #hint)
		DIR_CANCEL  = 2, & ! #cancel: caller should abandon any continuation
		DIR_CLEAR   = 3    ! #clear: caller should reset state_t

!===============================================================================

contains

!===============================================================================

function repl_directive(chunk, state, show_tree, show_hint, ou) result(action)

	! Check whether `chunk` (the text just read from the current prompt, not
	! yet joined onto any earlier continuation lines) is a REPL directive.
	! Matching against `chunk` rather than the accumulated multi-line buffer
	! is what lets a directive fire from the `>` / `[Hint ...]>` continuation
	! prompts too, not just a fresh `syntran$` prompt

	character(len = *), intent(in) :: chunk
	type(state_t), intent(in) :: state
	logical, intent(inout) :: show_tree, show_hint
	integer, intent(in) :: ou

	integer :: action

	!********

	character(len = :), allocatable :: cmd, topic

	action = DIR_NONE
	cmd = trim(adjustl(chunk))

	if (cmd == "#tree") then
		show_tree = .not. show_tree
		action = DIR_HANDLED

	else if (cmd == "#hint") then
		show_hint = .not. show_hint
		action = DIR_HANDLED

	else if (cmd == "#cancel") then
		action = DIR_CANCEL

	else if (cmd == "#clear") then
		action = DIR_CLEAR

	else if (cmd == "#help") then
		call repl_help("", state, ou)
		action = DIR_HANDLED

	else if (len(cmd) > 6) then
		if (cmd(1:6) == "#help ") then
			topic = trim(adjustl(cmd(7:)))
			call repl_help(topic, state, ou)
			action = DIR_HANDLED
		end if
	end if

end function repl_directive

!===============================================================================

recursive subroutine repl_help(topic, state, ou)

	! Print the `#help` index, or detail for one of its topics

	character(len = *), intent(in) :: topic
	type(state_t), intent(in) :: state
	integer, intent(in) :: ou

	!********

	character(len = :), allocatable :: version

	select case (topic)
	case ("")
		version = str(syntran_major)//"."//str(syntran_minor)//"."//str(syntran_patch)

		write(ou, "(a)") lang_name//" "//version
		write(ou, "(a)") "https://github.com/JeffIrwin/syntran"
		write(ou, *)
		write(ou, "(a)") "Directives:"
		write(ou, "(a)") tab//"#help [<topic>]     Show this help, or detail on <topic>"
		write(ou, "(a)") tab//"#tree               Toggle syntax tree display"
		write(ou, "(a)") tab//"#hint               Toggle the `;` continuation hint"
		write(ou, "(a)") tab//"#cancel             Abandon a partly-typed multi-line statement"
		write(ou, "(a)") tab//"#clear              Delete all variables and user-defined functions"
		write(ou, *)
		write(ou, "(a)") "Topics:"
		write(ou, "(a)") tab//"#help keys          Line editing and history"
		write(ou, "(a)") tab//"#help prompts       What `syntran$`, `>` and `[Hint]` mean"
		write(ou, "(a)") tab//"#help fns           Intrinsic function names"
		write(ou, "(a)") tab//"#help vars          Your variables and functions"
		write(ou, *)
		write(ou, "(a)") "Exit with `exit(0);` or Ctrl+D.  `syntran --help` lists command-line options."

	case ("keys")
		call repl_help_keys(ou)

	case ("prompts")
		call repl_help_prompts(ou)

	case ("fns")
		call repl_help_fns(state, ou)

	case ("vars")
		call repl_help_vars(state, ou)

	case default
		write(ou, "(a)") err_prefix//"unknown `#help` topic `"//topic//"`"//color_reset
		call repl_help("", state, ou)

	end select

end subroutine repl_help

!===============================================================================

subroutine repl_help_keys(ou)

	integer, intent(in) :: ou

	!********

	character(len = :), allocatable :: hist_path

	write(ou, "(a)") "Line editing:"
	write(ou, "(a)") tab//"Up / Down    Recall the previous / next history entry"
	write(ou, "(a)") tab//"Ctrl+R       Reverse-search history"
	write(ou, "(a)") tab//"Ctrl+D       Exit, on an empty line"
	write(ou, "(a)") tab//"Ctrl+C       Clear the current line"
	write(ou, "(a)") tab//"Tab          Not bound -- there is no completion yet"
	write(ou, *)

	hist_path = line_edit_history_path()
	if (len(hist_path) > 0) then
		write(ou, "(a)") "History is saved to "//hist_path//"."
	else
		write(ou, "(a)") "No home directory was found, so history is kept for this session only."
	end if
	write(ou, "(a)") "A multi-line statement is stored as a single history entry."
	write(ou, *)
	write(ou, "(a)") "On MSYS2/Cygwin/Git Bash (MinTTY), history and arrow-key editing are"
	write(ou, "(a)") "not available -- run via winpty, cmd.exe, or PowerShell instead."

end subroutine repl_help_keys

!===============================================================================

subroutine repl_help_prompts(ou)

	integer, intent(in) :: ou

	write(ou, "(a)") "Prompts:"
	write(ou, "(a)") tab//"syntran$        Ready for a new statement"
	write(ou, "(a)") tab//">               The statement is unfinished -- keep typing"
	write(ou, "(a)") tab//"[Hint `;`]>     A `;` is expected to finish the statement"
	write(ou, *)
	write(ou, "(a)") "A statement is only parsed once it looks complete, so it can span"
	write(ou, "(a)") "multiple lines (e.g. a multi-line `fn` or `if` body)."
	write(ou, "(a)") "Use `#cancel` to abandon a partly-typed statement, or `#hint` to"
	write(ou, "(a)") "toggle the `[Hint ...]` prompt."

end subroutine repl_help_prompts

!===============================================================================

subroutine repl_help_fns(state, ou)

	! List the user-visible intrinsic fn names, wrapped to fit the terminal.
	! Built from intr_fn_names() (intr_fns.f90) instead of hardcoding a list
	! here, so this can't silently drift from the actual fn registry

	type(state_t), intent(in) :: state
	integer, intent(in) :: ou

	!********

	integer, parameter :: width = 64

	type(string_vector_t) :: names
	character(len = :), allocatable :: fn_line
	integer :: i

	names = intr_fn_names(state%fns)

	write(ou, "(a)") "Intrinsic functions ("//str(names%len_)//"):"

	fn_line = tab
	do i = 1, names%len_
		if (len(fn_line) > len(tab) .and. &
				len(fn_line) + 1 + len(names%v(i)%s) > width) then
			write(ou, "(a)") fn_line
			fn_line = tab
		end if
		if (len(fn_line) > len(tab)) fn_line = fn_line//" "
		fn_line = fn_line//names%v(i)%s
	end do
	if (len(fn_line) > len(tab)) write(ou, "(a)") fn_line

	write(ou, *)
	write(ou, "(a)") "See doc/README.md for signatures."

end subroutine repl_help_fns

!===============================================================================

subroutine repl_help_vars(state, ou)

	! List the caller's currently-declared variables (with their types) and
	! user-defined functions.  Walks state%vars%dicts(:)/state%fns%table(:)
	! directly -- the same open-addressing-table scan var_closest() and
	! fn_closest() use in types_dict.f90 -- rather than keeping a separate
	! name list in sync

	type(state_t), intent(in) :: state
	integer, intent(in) :: ou

	!********

	integer :: i, j
	logical :: any_vars, any_fns

	write(ou, "(a)") "Your variables:"
	any_vars = .false.
	do i = 1, state%vars%scope
		do j = 1, state%vars%dicts(i)%capacity
			if (.not. allocated(state%vars%dicts(i)%table(j)%key)) cycle
			! Slots 1..NUM_INTR_VARS are the pre-declared std:: constants
			! (declare_intr_vars(), intr_vars.f90); skip them here since
			! they aren't anything the user declared
			if (state%vars%dicts(i)%table(j)%id_index <= NUM_INTR_VARS) cycle
			any_vars = .true.
			write(ou, "(a)") tab//state%vars%dicts(i)%table(j)%key// &
				": "//type_name(state%vars%dicts(i)%table(j)%val)
		end do
	end do
	if (.not. any_vars) write(ou, "(a)") tab//"(none)"

	write(ou, *)
	write(ou, "(a)") "Your functions:"
	any_fns = .false.
	do i = 1, state%fns%capacity
		if (.not. allocated(state%fns%table(i)%key)) cycle
		if (state%fns%table(i)%val%is_intr) cycle
		any_fns = .true.
		write(ou, "(a)") tab//state%fns%table(i)%key
	end do
	if (.not. any_fns) write(ou, "(a)") tab//"(none)"

end subroutine repl_help_vars

!===============================================================================

end module syntran__repl_m

!===============================================================================
