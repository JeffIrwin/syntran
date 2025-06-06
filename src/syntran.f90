
!===============================================================================

module syntran

	! This module contains the "public" API of syntran.  I don't actually use
	! any private statements, so you just have to be careful if you use the
	! syntran library as an API (as opposed to using the syntran CLI)

	use syntran__core_m

	implicit none

!===============================================================================

contains

!===============================================================================

function syntran_interpret(str_, quiet, startup_file) result(res_str)

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

	character(len = :), allocatable :: res_str

	!********

	character(len = :), allocatable :: line, src_file, source_text, prompt, &
		prompt_color

	integer, parameter :: iu = input_unit, ou = output_unit
	integer :: io

	logical :: continue_, show_tree
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

	if (present(str_)) then
		! Append a trailing line feed in case it does not exist
		sv = new_string_view(str_//line_feed)
		src_file = '<string>'
	end if
	!print *, "src_file = ", src_file

	call init_state(state)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	if (present(startup_file)) then
		!print *, "startup_file = ", startup_file

		source_text = read_file(startup_file, io)

		if (io /= exit_success) then
			if (.not. state%quiet) write(*,*) err_404(startup_file)
			return
		end if

		compilation = syntax_parse(source_text, state%vars, state%fns, startup_file)
		if (.not. state%quiet) call compilation%log_diagnostics()

		if (compilation%diagnostics%len_ > 0) then
			res_str = ''
			return
		end if

		! TODO: chdir option?
		call syntax_eval(compilation, state, res)
		res_str = res%to_str()
		write(*,*) '    '//res_str

	end if

	! Read-eval-print-loop
	do

		if (present(str_)) then

			! Interpret multi-line strings one line at a time to mock the
			! interpreter getting continued stdin lines.  If you know your whole
			! string ahead of time, just use syntran_eval() instead
			line = sv%get_line(iostat = io)

		else

			if (continue_) then

				! If expecting more characters, re-parse the whole line from the
				! beginning.  An alternative implementation would not append but
				! only pass the new characters, but also pass the previous
				! compilation tree to append to the tree instead of appending
				! characters.  This way seemed easier :shrug:

				! TODO: add a directive option to hide expected char hint

				! Bash uses `$` for the inital prompt and `>` for continued
				! prompts.  So do we

				if (compilation%first_expected == ";") then

					! Other chars could be hinted, e.g. unmatched parens, but it
					! is generally noisy, less helpful, and should be more
					! obvious to the user
					!
					! I hint semicolons because it could be a stumbling block
					! for users coming from python or similar languages
					write(ou, '(a)', advance = 'no') prompt_color// &
						'[Hint `'//compilation%first_expected//'`]> '//color_reset

				else
					write(ou, '(a)', advance = 'no') prompt_color//'> '//color_reset
				end if

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
		compilation = syntax_parse(line, state%vars, state%fns, src_file, allow_cont)
		!print *, 'in interpreter'

		!print *, 'compilation%expecting = ', compilation%expecting

		!print *, 'allocated(state%vars%dicts(1)%root) = ', &
		!	allocated(state%vars%dicts(1)%root)

		! Continue current parse with next line since more chars are expected
		continue_ = compilation%expecting

		!if (continue_ .and. present(str_)) exit
		if (continue_) cycle

		if (compilation%is_empty) cycle

		! I'm skipping the the binder that Immo implemented at this point in
		! episode 2.  I guess I'll find out later if that's a stupid decision on
		! my end.  I think I can just do type checking in the parser

		if (debug > 0 .or. show_tree) print *, 'tree = ', compilation%str()

		if (.not. state%quiet) call compilation%log_diagnostics(ou)

		! Don't try to evaluate with errors
		if (compilation%diagnostics%len_ > 0) cycle

		call syntax_eval(compilation, state, res )

		!print *, "res type = ", kind_name(res%type)
		if (res%type == void_type   ) cycle
		if (res%type == unknown_type) cycle

		res_str = res%to_str()
		if (.not. present(str_)) write(ou, '(a)') res_str

	end do

	!print *, 'done syntran_interpret()'

end function syntran_interpret

!===============================================================================

integer function syntran_eval_i32(str_) result(eval_i32)

	character(len = *), intent(in) :: str_

	type(state_t) :: state
	type(syntax_node_t) :: tree
	type(value_t) :: val

	call init_state(state)
	state%quiet = .false.

	tree = syntax_parse(str_, state%vars, state%fns)
	call tree%log_diagnostics()

	if (tree%diagnostics%len_ > 0) then
		! TODO: iostat
		eval_i32 = 0
		return
	end if

	call syntax_eval(tree, state, val)

	! TODO: check kind, add optional iostat arg
	eval_i32 = val%sca%i32

	if (debug >= 1) print *, 'eval_i32 = ', eval_i32

end function syntran_eval_i32

!===============================================================================

integer(kind = 8) function syntran_eval_i64(str_) result(val_)

	character(len = *), intent(in) :: str_

	type(state_t) :: state
	type(syntax_node_t) :: tree
	type(value_t) :: val

	call init_state(state)
	state%quiet = .false.

	tree = syntax_parse(str_, state%vars, state%fns)
	call tree%log_diagnostics()

	if (tree%diagnostics%len_ > 0) then
		! TODO: iostat
		val_ = 0
		return
	end if

	call syntax_eval(tree, state, val)

	! TODO: check kind, add optional iostat arg
	val_ = val%sca%i64

end function syntran_eval_i64

!===============================================================================

real(kind = 4) function syntran_eval_f32(str_, quiet) result(eval_f32)

	character(len = *), intent(in) :: str_

	logical, optional, intent(in) :: quiet

	!*******

	type(state_t) :: state
	type(syntax_node_t) :: tree
	type(value_t) :: val

	call init_state(state)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	tree = syntax_parse(str_, state%vars, state%fns)
	if (.not. state%quiet) call tree%log_diagnostics()

	if (tree%diagnostics%len_ > 0) then
		! TODO: iostat
		eval_f32 = 0
		return
	end if

	call syntax_eval(tree, state, val)

	! TODO: check kind, add optional iostat arg
	eval_f32 = val%sca%f32
	!print *, 'eval_f32 = ', eval_f32

end function syntran_eval_f32

!===============================================================================

real(kind = 8) function syntran_eval_f64(str_, quiet) result(eval_f64)

	character(len = *), intent(in) :: str_

	logical, optional, intent(in) :: quiet

	!*******

	type(state_t) :: state
	type(syntax_node_t) :: tree
	type(value_t) :: val

	call init_state(state)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	tree = syntax_parse(str_, state%vars, state%fns)
	if (.not. state%quiet) call tree%log_diagnostics()

	if (tree%diagnostics%len_ > 0) then
		! TODO: iostat
		eval_f64 = 0
		return
	end if

	call syntax_eval(tree, state, val)

	! TODO: check kind, add optional iostat arg
	eval_f64 = val%sca%f64
	!print *, 'eval_f64 = ', eval_f64

end function syntran_eval_f64

!===============================================================================

subroutine init_state(state)

	! TODO: move to eval.f90

	! This sets everything but state%quiet, since some routines have that as an
	! optional argument
	!
	! Maybe the state_t definition should be moved to types.f90, and then this
	! could be a class-bound procedure

	type(state_t), intent(inout) :: state

	call declare_intr_fns(state%fns)

	state%returned  = .false.
	state%breaked   = .false.
	state%continued = .false.

	! Is it safe to initialize these arrays both here and in new_parser?  Test
	! interactive interp

	state%vars%scope_cap = SCOPE_CAP_INIT
	allocate(state%vars%dicts( state%vars%scope_cap) )

	state%locs%scope_cap = SCOPE_CAP_INIT
	allocate(state%locs%dicts( state%locs%scope_cap) )

	!print *, "init size fns = ", size(state%fns%fns)

end subroutine init_state

!===============================================================================

function syntran_eval(str_, quiet, src_file, chdir_) result(res)

	! Note that this chdir_ optional arg is a str_, while the chdir_ optional arg
	! for syntran_interpret_file() is boolean
	!
	! TODO: add optional io arg in case of errors.  Especially for "-c" cmd arg.
	! See note below

	character(len = *), intent(in)  :: str_
	character(len = :), allocatable :: res

	logical, optional, intent(in) :: quiet
	character(len = *), optional, intent(in) :: src_file
	character(len = *), optional, intent(in) :: chdir_

	!********

	character(len = 1024) :: buffer
	character(len = :), allocatable :: src_filel, dir, cwd

	logical :: chdirl, repl

	type(state_t) :: state
	type(syntax_node_t) :: tree
	type(value_t) :: val

	!print *, ''
	!print *, 'str_ = ', str_

	call init_state(state)
	state%quiet = .false.
	if (present(quiet)) state%quiet = quiet

	src_filel = '<stdin>'
	repl = .true.
	if (present(src_file)) then
		src_filel = src_file
		repl = .false.
	end if

	!print * , "src_filel = """, src_filel, """"

	chdirl = .false.
	if (present(chdir_)) then
		chdirl = .true.
		dir = chdir_
	end if

	! TODO: make a helper fn that all the eval_* fns use

	!print *, "parsing"
	tree = syntax_parse(str_, state%vars, state%fns, src_filel, repl = repl)
	!print *, "done"
	!print *, "size fns = ", size(state%fns%fns)

	!print *, tree%str()  ! `#tree` or `show_tree` equivalent for interpreting a file

	if (.not. state%quiet) call tree%log_diagnostics()

	if (tree%diagnostics%len_ > 0) then
		! TODO: set io
		res = ''
		return
	end if

	if (chdirl) then

		! chdir *after* syntax_parse() so that #include paths are correct, but
		! *before* syntax_eval() so that runtime open() paths are correct
		!
		! I've only implemented this chdir option so that I can copy/paste my
		! AOC solutions to unit tests in subdirs with minimal changes

		! pushd
		call getcwd(buffer)
		cwd = trim(buffer)
		call chdir(dir)

	end if

	!print *, "evaling "
	call syntax_eval(tree, state, val)
	!print *, "done"
	res = val%to_str()
	!print *, 'res = ', res

	! popd
	if (chdirl) call chdir(cwd)
	!print *, "done syntran_eval()"

end function syntran_eval

!===============================================================================

function syntran_interpret_file(filename, quiet, quiet_info, chdir_) result(res)

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

	!********

	character(len = :), allocatable :: source_text

	integer :: iostat

	logical :: chdirl, quiet_infol

	type(state_t) :: state

	state%quiet = .false.
	quiet_infol = .false.
	if (present(quiet     )) state%quiet = quiet
	if (present(quiet_info)) quiet_infol = quiet_info

	chdirl = .false.
	if (present(chdir_)) chdirl = chdir_

	if (.not. state%quiet .and. .not. quiet_infol) then
		write(*,*) 'Interpreting file "'//filename//'"'
	end if
	!if (.true. .or. .not. state%quiet) write(*,*) 'Interpreting file "'//filename//'"'

	source_text = read_file(filename, iostat)

	if (iostat /= exit_success) then
		if (.not. state%quiet) write(*,*) err_404(filename)
		return
	end if

	if (chdirl) then
		res = trim(adjustl(syntran_eval(source_text, state%quiet, filename, &
			chdir_ = get_dir(filename))))
	else
		res = trim(adjustl(syntran_eval(source_text, state%quiet, filename)))
	end if

end function syntran_interpret_file

!===============================================================================

end module syntran

!===============================================================================

