
!===============================================================================

module syntran

	! This module contains the "public" API of syntran.  I don't actually use
	! any private statements, so you just have to be careful if you use the
	! syntran library as an API (as opposed to using the syntran CLI)

	use syntran__core_m
	use syntran__compile_m
	use syntran__vm_m
	use syntran__line_edit_m
	use syntran__repl_m

	implicit none

!===============================================================================

contains

!===============================================================================

subroutine eval_dispatch(tree, state, res)

	! Compile to bytecode and run it on the VM.
	!
	! If evaluation throws a runtime error (state%rt_halt), surface it here:
	! non-quiet callers (CLI, REPL, file interpretation) get the legacy
	! print-and-exit behavior, while quiet callers (e.g. unit tests) leave
	! state%rt_diags for syntran_eval() to copy into its `diags` out-arg
	! instead of exiting the process

	type(syntax_node_t), intent(in) :: tree
	type(state_t), intent(inout) :: state
	type(value_t), intent(out) :: res

	!*******

	type(program_t) :: prog

	call compile_tree(tree, prog, state%fns)
	call vm_run(prog, state, res)

	if (state%rt_halt .and. .not. state%quiet) then
		call log_rt_diags(state%rt_diags)
		call exit(exit_failure)
	end if

end subroutine eval_dispatch

!===============================================================================

subroutine log_rt_diags(diags)

	! Print runtime-error diagnostics to stdout.  In practice there's at most
	! one entry (evaluation halts on the first runtime error), but loop just
	! in case that ever changes

	type(string_vector_t), intent(in) :: diags

	integer :: i

	do i = 1, diags%len_
		write(*,*) diags%v(i)%s
	end do

end subroutine log_rt_diags

!===============================================================================

function syntran_interpret(str_, quiet, startup_file, script_args) result(res_str)

	! This is the interactive interpreter shell REPL
	!
	! To interpret a whole file all at once, use syntran_interpret_file()
	! instead
	!
	! Interpret stdin by default, or interpret the multi-line string str_ if it
	! is given.  The return value res_str is the result of the final expression
	! (like how Rust doesn't have a return statement, but fns just return the
	! final expression in their body)
	!
	! Using the str_ arg is deprecated here.  Prefer eval() or interpret_file().
	! However, it's still useful for testing to have something that evals 1 line
	! at a time, so that we can have automatic test coverage of weird
	! interactive interpreter edge cases

	character(len = *), intent(in), optional :: str_
	logical, intent(in), optional :: quiet
	character(len = *), intent(in), optional :: startup_file
	type(string_vector_t), optional, intent(in) :: script_args

	character(len = :), allocatable :: res_str

	!********

	character(len = :), allocatable :: line, chunk, src_file, source_text, &
		prompt, prompt_color

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io, dir_action

	logical :: continue_, show_tree, show_hint, interactive, saved_quiet
	logical, parameter :: allow_cont = .true.

	type(string_view_t) :: sv

	type(state_t) :: state
	type(syntax_node_t) :: compilation
	type(value_t) :: res

	!print *, 'starting syntran_interpret()'
	!print *, 'len(" ") = ', len(' ')
	!print *, 'len(line_feed) = ', len(line_feed)

	prompt_color = fg_bold//fg_green
	prompt = prompt_color//lang_name//'$ '//color_reset

	src_file = '<stdin>'
	continue_ = .false.
	show_tree = .false.
	show_hint = .true.

	! Only use interactive line editing (history, arrow keys) when reading a
	! real terminal.  Piped stdin, `-c` strings, and file interpretation all
	! keep using the plain read_line() path in utils.f90
	interactive = .not. present(str_) .and. is_tty()
	if (interactive) call line_edit_init()

	if (present(str_)) then
		! Append a trailing line feed in case it does not exist
		sv = new_string_view(str_//line_feed)
		src_file = '<string>'
	end if
	!print *, "src_file = ", src_file

	call init_state(state, script_args)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	if (.not. interactive .and. .not. present(str_) .and. &
			.not. state%quiet .and. is_mintty()) then
		write(*,*) 'Note: interactive line editing (history, arrow keys) is'// &
			' not available in Git Bash / MinTTY.'
		write(*,*) 'For full REPL editing use: winpty syntran'// &
			'  (or cmd.exe / PowerShell)'
	end if

	if (present(startup_file)) then
		!print *, "startup_file = ", startup_file

		source_text = read_file(startup_file, io)

		if (io /= exit_success) then
			if (.not. state%quiet) write(*,*) err_404(startup_file)
			call state_destroy(state)
			return
		end if

		compilation = syntax_parse(source_text, state, startup_file)
		if (.not. state%quiet) call compilation%log_diagnostics()

		if (compilation%diagnostics%len_ > 0) then
			res_str = ''
			call state_destroy(state)
			return
		end if

		! TODO: chdir option?
		call eval_dispatch(compilation, state, res)
		if (state%rt_halt) then
			! eval_dispatch() already printed and exited for non-quiet
			! callers, so reaching here means quiet was true
			res_str = ''
			call state_destroy(state)
			return
		end if
		res_str = res%to_str()
		write(*,*) '    '//res_str

	end if

	! Read-eval-print-loop
	do

		if (present(str_)) then

			! Interpret multi-line strings one line at a time to mock the
			! interpreter getting continued stdin lines.  If you know your whole
			! string ahead of time, just use syntran_eval() instead
			chunk = sv%get_line(iostat = io)

		else

			if (continue_) then

				! If expecting more characters, re-parse the whole line from the
				! beginning.  An alternative implementation would not append but
				! only pass the new characters, but also pass the previous
				! compilation tree to append to the tree instead of appending
				! characters.  This way seemed easier :shrug:

				! Bash uses `$` for the inital prompt and `>` for continued
				! prompts.  So do we

				if (show_hint .and. compilation%first_expected == ";") then

					! Other chars could be hinted, e.g. unmatched parens, but it
					! is generally noisy, less helpful, and should be more
					! obvious to the user
					!
					! I hint semicolons because it could be a stumbling block
					! for users coming from python or similar languages

					if (interactive) then
						chunk = read_line_interactive( &
							'[Hint `'//compilation%first_expected//'`]> ', io, &
							use_color = len(prompt_color) > 0)
					else
						write(ou, '(a)', advance = 'no') prompt_color// &
							'[Hint `'//compilation%first_expected//'`]> '//color_reset
						chunk = read_line(iu, iostat = io)
					end if

				else
					if (interactive) then
						chunk = read_line_interactive('> ', io, &
							use_color = len(prompt_color) > 0)
					else
						write(ou, '(a)', advance = 'no') prompt_color//'> '//color_reset
						chunk = read_line(iu, iostat = io)
					end if
				end if

			else
				if (interactive) then
					chunk = read_line_interactive(lang_name//'$ ', io, &
						use_color = len(prompt_color) > 0)
				else
					write(ou, '(a)', advance = 'no') prompt
					chunk = read_line(iu, iostat = io)
				end if
			end if

		end if

		!print *, 'chunk = <', chunk, '>'
		!print *, 'io = ', io

		!! Echo input?
		!write(ou, '(a)') chunk

		if (io == iostat_end) exit

		! Directives are matched against `chunk` (just what was read at this
		! prompt), not the accumulated `line`, so they work both at a fresh
		! `syntran$` prompt and mid-continuation -- e.g. `#cancel` can only
		! abandon a stuck multi-line statement because of this
		dir_action = repl_directive(chunk, state, show_tree, show_hint, ou)

		if (dir_action == DIR_CANCEL) then
			continue_ = .false.
			cycle
		end if

		if (dir_action == DIR_CLEAR) then
			! Tear down and reinitialize state_t, preserving what the REPL
			! was started with
			saved_quiet    = state%quiet
			call state_destroy(state)
			call init_state(state, script_args)
			state%quiet = saved_quiet
			continue_ = .false.
			cycle
		end if

		if (dir_action == DIR_HANDLED) cycle

		if (continue_) then
			! Mirror the previous per-branch behavior: keep accumulating
			! onto whatever the statement had so far
			line = line//line_feed//chunk
		else
			line = chunk
		end if

		res_str = ' '
		compilation = syntax_parse(line, state, src_file, allow_cont)
		!print *, 'in interpreter'

		!print *, 'compilation%expecting = ', compilation%expecting

		!print *, 'allocated(state%vars%dicts(1)%root) = ', &
		!	allocated(state%vars%dicts(1)%root)

		! Continue current parse with next line since more chars are expected
		continue_ = compilation%expecting

		!if (continue_ .and. present(str_)) exit
		if (continue_) cycle

		if (compilation%is_empty) cycle

		! The line is now a complete statement (possibly joined from several
		! continuation lines).  read_line_interactive() already undid isocline's
		! own per-call history push, so this is the one place a full logical
		! statement becomes a single recallable history entry
		if (interactive) call line_edit_add_history(line)

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0 .or. show_tree) print *, 'tree = ', compilation%to_str()

		if (.not. state%quiet) call compilation%log_diagnostics(ou)

		! Don't try to evaluate with errors
		if (compilation%diagnostics%len_ > 0) cycle

		call eval_dispatch(compilation, state, res)

		if (state%rt_halt) then
			! eval_dispatch() already printed and exited for non-quiet
			! callers, so reaching here means quiet was true.  Stop the REPL
			! loop rather than risk reading a stale/unset res
			exit
		end if

		!print *, "res type = ", kind_name(res%type)
		if (res%type == void_type   ) cycle
		if (res%type == unknown_type) cycle

		res_str = res%to_str()
		if (.not. present(str_)) write(ou, '(a)') res_str

	end do

	!print *, 'done syntran_interpret()'

	call state_destroy(state)

end function syntran_interpret

!===============================================================================

subroutine syntran_eval_value(str_, val, quiet, want_type, src_file, chdir_, &
		script_args, diags, syntax_only, io)

	! Shared front end for syntran_eval() and the typed syntran_eval_i32() /
	! _i64() / _f32() / _f64() / _bool() / _str() wrappers below.  Parses and
	! evaluates str_, and hands back the raw value_t so callers whose result
	! type has no typed wrapper (structs, enums, arrays, ...) can still get
	! at it
	!
	! Note that this chdir_ optional arg is a str_, while the chdir_ optional arg
	! for syntran_interpret_file() is boolean

	character(len = *), intent(in) :: str_
	type(value_t), intent(out) :: val

	logical, optional, intent(in) :: quiet

	! Expected result type (i32_type, bool_type, str_type, ...).  When
	! present and the expression evaluates to a different type, this is
	! treated as an error: `io` is set to exit_failure and `val` is left
	! unset (intent(out) default: type == unknown_type)
	integer, optional, intent(in) :: want_type

	character(len = *), optional, intent(in) :: src_file
	character(len = *), optional, intent(in) :: chdir_
	type(string_vector_t), optional, intent(in) :: script_args

	! Diagnostic messages (one error per element), unset on success.  Lets
	! callers (e.g. unit tests) inspect error codes/text without parsing
	! stdout
	type(string_vector_t), optional, intent(out) :: diags

	! Parse and type-check only, without evaluating.  Diagnostics are still
	! logged and copied to `diags`, but nothing runs: `use` module-level
	! statements, file I/O, and even bytecode compilation are all skipped.
	! Backs the `--syntax-only` CLI option
	logical, optional, intent(in) :: syntax_only

	! exit_success/exit_failure status for callers.  Failure means the
	! expression did not run to completion (parser diagnostics, or a runtime
	! halt for quiet callers -- non-quiet callers exit the process from
	! eval_dispatch() instead), or that it ran but produced a type other than
	! want_type
	integer, optional, intent(out) :: io

	!********

	character(len = :), allocatable :: src_filel, dir

	logical :: repl, syntax_onlyl

	type(state_t) :: state
	type(syntax_node_t) :: tree

	! Determine source directory first
	dir = ''
	if (present(chdir_)) then
		dir = chdir_
	end if

	call init_state(state, script_args, dir)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	syntax_onlyl = .false.
	if (present(syntax_only)) syntax_onlyl = syntax_only

	if (present(io)) io = exit_success

	src_filel = '<stdin>'
	repl = .true.
	if (present(src_file)) then
		src_filel = src_file
		repl = .false.
	end if

	tree = syntax_parse(str_, state, src_filel, repl = repl)

	if (.not. state%quiet) call tree%log_diagnostics()

	if (present(diags)) diags = tree%diagnostics

	if (tree%diagnostics%len_ > 0) then
		if (present(io)) io = exit_failure
		call state_destroy(state)
		return
	end if

	! No chdir() needed - src_dir is now in state and will be used by open()

	if (syntax_onlyl) then
		! Parse and type check succeeded.  Stop before eval_dispatch() so that
		! nothing is compiled or executed -- in particular, module-level
		! statements pulled in by `use` (see parse_use_statement()) must not run
		return
	end if

	call eval_dispatch(tree, state, val)

	! A runtime error halted evaluation.  eval_dispatch() already printed and
	! exited for non-quiet callers, so reaching here means state%quiet is
	! true: append the runtime diagnostic(s) to the diags out-arg (alongside
	! any parser diagnostics, of which there are none here since we already
	! returned above if tree%diagnostics were non-empty) and skip using val
	! since it may not have been fully populated when evaluation halted
	if (state%rt_halt) then
		if (present(diags)) call diags%push_all(state%rt_diags)
		if (present(io)) io = exit_failure
		call state_destroy(state)
		return
	end if

	if (present(want_type)) then
		if (val%type /= want_type) then
			if (.not. state%quiet) write(*,*) err_prefix// &
				'syntran_eval_value() expected type `'//kind_name(want_type)// &
				'` but the expression has type `'//kind_name(val%type)//'`'//color_reset
			if (present(io)) io = exit_failure
			call state_destroy(state)
			return
		end if
	end if

	call state_destroy(state)

end subroutine syntran_eval_value

!===============================================================================

integer function syntran_eval_i32(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io

	!*******

	integer :: iol
	type(value_t) :: val

	res = 0
	call syntran_eval_value(str_, val, quiet, i32_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	res = val%sca%i32

	if (debug >= 1) print *, 'eval_i32 = ', res

end function syntran_eval_i32

!===============================================================================

integer(kind = 8) function syntran_eval_i64(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io

	!*******

	integer :: iol
	type(value_t) :: val

	res = 0
	call syntran_eval_value(str_, val, quiet, i64_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	res = val%sca%i64

end function syntran_eval_i64

!===============================================================================

real(kind = 4) function syntran_eval_f32(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io

	!*******

	integer :: iol
	type(value_t) :: val

	res = 0
	call syntran_eval_value(str_, val, quiet, f32_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	res = val%sca%f32
	!print *, 'eval_f32 = ', res

end function syntran_eval_f32

!===============================================================================

real(kind = 8) function syntran_eval_f64(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io

	!*******

	integer :: iol
	type(value_t) :: val

	res = 0
	call syntran_eval_value(str_, val, quiet, f64_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	res = val%sca%f64
	!print *, 'eval_f64 = ', res

end function syntran_eval_f64

!===============================================================================

logical function syntran_eval_bool(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io

	!*******

	integer :: iol
	type(value_t) :: val

	res = .false.
	call syntran_eval_value(str_, val, quiet, bool_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	res = val%sca%bool

end function syntran_eval_bool

!===============================================================================

function syntran_eval_str(str_, quiet, io) result(res)

	character(len = *), intent(in) :: str_
	logical, optional, intent(in) :: quiet
	integer, optional, intent(out) :: io
	character(len = :), allocatable :: res

	!*******

	integer :: iol
	type(value_t) :: val

	res = ''
	call syntran_eval_value(str_, val, quiet, str_type, io = iol)
	if (present(io)) io = iol
	if (iol /= exit_success) return

	if (allocated(val%str)) res = val%str%s

end function syntran_eval_str

!===============================================================================

subroutine init_state(state, script_args, src_dir)

	! This sets everything but state%quiet, since some routines have that as an
	! optional argument

	type(state_t), intent(inout) :: state
	type(string_vector_t), intent(in), optional :: script_args
	character(len = *), intent(in), optional :: src_dir

	!*******

	call declare_intr_fns(state%fns)

	state%rt_halt  = .false.
	state%rt_diags = new_string_vector()

	! Is it safe to initialize these arrays both here and in new_parser?  Test
	! interactive interp

	state%vars%scope_cap = SCOPE_CAP_INIT
	allocate(state%vars%dicts( state%vars%scope_cap) )

	state%locs%scope_cap = SCOPE_CAP_INIT
	allocate(state%locs%dicts( state%locs%scope_cap) )

	! Script arguments passed after `--`
	if (present(script_args)) then
		state%script_args = script_args
	else
		state%script_args = new_string_vector()
	end if

	! Source directory for resolving relative file paths
	if (present(src_dir)) then
		state%src_dir = src_dir
	else
		state%src_dir = ''
	end if

	!print *, "init size fns = ", size(state%fns%fns)

end subroutine init_state

!===============================================================================

function syntran_eval(str_, quiet, src_file, chdir_, script_args, diags, &
		syntax_only, io) result(res)

	! Note that this chdir_ optional arg is a str_, while the chdir_ optional arg
	! for syntran_interpret_file() is boolean

	character(len = *), intent(in)  :: str_
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet
	character(len = *), optional, intent(in) :: src_file
	character(len = *), optional, intent(in) :: chdir_
	type(string_vector_t), optional, intent(in) :: script_args

	! Diagnostic messages (one error per element), unset on success.  Lets
	! callers (e.g. unit tests) inspect error codes/text without parsing
	! stdout
	type(string_vector_t), optional, intent(out) :: diags

	! Parse and type-check only, without evaluating.  Diagnostics are still
	! logged and copied to `diags`, but nothing runs: `use` module-level
	! statements, file I/O, and even bytecode compilation are all skipped.
	! Backs the `--syntax-only` CLI option
	logical, optional, intent(in) :: syntax_only

	! exit_success/exit_failure status for non-interactive callers (the CLI in
	! file or `-c` mode).  Failure means the program did not run to
	! completion: parser diagnostics, or a runtime halt for quiet callers
	! (non-quiet callers exit the process from eval_dispatch() instead)
	integer, optional, intent(out) :: io

	!********

	integer :: iol

	type(value_t) :: val

	call syntran_eval_value(str_, val, quiet, src_file = src_file, chdir_ = chdir_, &
		script_args = script_args, diags = diags, &
		syntax_only = syntax_only, io = iol)

	if (present(io)) io = iol

	res = ''
	if (iol /= exit_success) return

	if (present(syntax_only)) then
		! Parse and type check succeeded, but nothing was evaluated (see
		! syntran_eval_value()); val is unset
		if (syntax_only) return
	end if

	res = val%to_str()
	!print *, 'res = ', res

end function syntran_eval

!===============================================================================

function syntran_interpret_file(filename, quiet, quiet_info, chdir_, script_args, diags, &
		syntax_only, io) result(res)

	! TODO:
	!   - enable input echo for file input (not for stdin)
	!   - echo inputs w/o "syntran$" prompt and print outputs after a comment,
	!     for ease of updating documentation with consistent styling

	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: res

	! TODO: refactor verbosity as an int for silent, error, warn, info, debug, etc.
	! Maybe not all those options yet, but at least silent, error, and info.
	! Debug/trace could have an impact on perf so maybe that should still be a
	! compile-time const:
	!
	!     SILENT = -20
	!     ERROR  = -10
	!     WARN   =   0
	!     INFO   =  10
	!     DEBUG  =  20
	!     TRACE  =  30
	!
	logical, optional, intent(in) :: quiet, quiet_info

	logical, optional, intent(in) :: chdir_

	type(string_vector_t), optional, intent(in) :: script_args

	! Diagnostic messages (one error per element), unset on success.  Mirrors
	! the diags out-arg of syntran_eval(), including the EC_404 case below
	! which happens before any parser/diagnostics object exists
	type(string_vector_t), optional, intent(out) :: diags

	! Parse and type-check only, without evaluating.  See syntran_eval()
	logical, optional, intent(in) :: syntax_only

	! exit_success/exit_failure status.  See syntran_eval()
	integer, optional, intent(out) :: io

	!********

	character(len = :), allocatable :: source_text

	integer :: iostat

	logical :: chdirl, quiet_infol, syntax_onlyl

	type(state_t) :: state

	state%quiet = .false.
	quiet_infol = .false.
	if (present(quiet     )) state%quiet = quiet
	if (present(quiet_info)) quiet_infol = quiet_info

	chdirl = .false.
	if (present(chdir_)) chdirl = chdir_

	syntax_onlyl = .false.
	if (present(syntax_only)) syntax_onlyl = syntax_only

	! Syntax checking is silent on success, so suppress the info line too
	if (syntax_onlyl) quiet_infol = .true.

	if (present(io)) io = exit_success

	if (.not. state%quiet .and. .not. quiet_infol) then
		write(*,*) 'Interpreting file "'//filename//'"'
	end if
	!if (.true. .or. .not. state%quiet) write(*,*) 'Interpreting file "'//filename//'"'

	source_text = read_file(filename, iostat)

	if (iostat /= exit_success) then
		if (.not. state%quiet) write(*,*) err_404(filename)
		if (present(diags)) then
			diags = new_string_vector()
			call diags%push(err_404(filename))
		end if
		if (present(io)) io = exit_failure
		res = ''
		return
	end if

	if (chdirl) then
		res = trim(adjustl(syntran_eval(source_text, state%quiet, filename, &
			chdir_ = get_dir(filename), script_args = script_args, diags = diags, &
			syntax_only = syntax_only, io = io)))
	else
		res = trim(adjustl(syntran_eval(source_text, state%quiet, filename, &
			script_args = script_args, diags = diags, &
			syntax_only = syntax_only, io = io)))
	end if

end function syntran_interpret_file

!===============================================================================

end module syntran

!===============================================================================

