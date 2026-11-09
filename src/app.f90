
!===============================================================================

module syntran__app_m

	use syntran
	use syntran__core_m
	use syntran__errors_m
	use syntran__linalg_m, only: blas_vendor
	use syntran__utils_m
	use syntran__line_edit_m, only: enable_vt_processing, is_stdout_tty

	implicit none

	character(len = *), parameter :: &
		COLOR_AUTO = "auto", &
		COLOR_ON   = "on", &
		COLOR_OFF  = "off"

	type args_t

		character(len = :), allocatable :: syntran_file, command, color, transpile_out, native_out

		integer :: maxerr

		logical :: &
			chdir              = .false., &
			command_arg        = .false., &
			interactive        = .false., &
			jit                = .false., &
			keep_unused_fns    = .false., &
			lex                = .false., &
			native             = .false., &
			permissive_return  = .false., &
			quiet              = .false., &
			syntax_only        = .false., &
			syntran_file_arg   = .false., &
			transpile          = .false., &
			version            = .false., &
			help               = .false.

		! Script arguments passed after `--`
		type(string_vector_t) :: script_args

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	!********
	character(len = :), allocatable, save :: argv0
	character(len = 1024) :: buffer
	integer, parameter :: STAT_TRUNC = -1
	integer :: io, argc
	logical, save :: first = .true.

	if (first) then
		first = .false.
		call get_command_argument(0, buffer)
		argv0 = trim(buffer)
	end if

	i = i + 1
	argc = command_argument_count()
	if (i > argc) then
		write(*,*) err_prefix//"missing required argument after """//argv0//""""
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(i, buffer, status = io)
	if (io == STAT_TRUNC) then
		! Could make buffer allocatable and automatically try resizing
		write(*,*) err_prefix//"command argument too long after """//argv0//""""
		call exit(EXIT_FAILURE)

	else if (io /= EXIT_SUCCESS) then
		write(*,*) err_prefix//"cannot get command argument after """//argv0//""""
		call exit(EXIT_FAILURE)

	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

	argv0 = argv

end subroutine get_next_arg

!===============================================================================

subroutine set_ansi_colors(is_color_in)

	! Should this be in error.f90 or utils.f90?  This is the only reason
	! test.f90 needs to use app.f90

	logical, intent(in), optional :: is_color_in
	logical :: is_color

	!print *, "starting set_ansi_colors()"
	if (present(is_color_in)) then
		is_color = is_color_in
		!print *, "is_color_in = ", is_color_in
	else
		! If stdout is a TTY, default to color on.  If stdout is redirected to a
		! log (not TTY), default to color off because most text editors will not
		! render ANSI escape sequences
		is_color = is_stdout_tty()
		!print *, "is_color_in not present"
	end if

	if (is_color) then
		! Under winpty, stdout is a hidden Windows console without ANSI escape
		! interpretation enabled, so raw color codes would otherwise render as
		! literal glyphs (e.g. ^[[95m) once winpty scrapes and re-emits them.
		! Enabling VT processing here, before any colored output is printed,
		! fixes that; it's a no-op on non-Windows and on non-console handles
		call enable_vt_processing()

		fg_bold            = FG_BOLD_
		fg_bright_red      = FG_BRIGHT_RED_
		fg_bold_bright_red = FG_BOLD_BRIGHT_RED_
		fg_bold_bright_green = FG_BOLD_BRIGHT_GREEN_
		fg_bright_green    = FG_BRIGHT_GREEN_
		fg_green           = FG_GREEN_
		fg_bright_blue     = FG_BRIGHT_BLUE_
		fg_bright_magenta  = FG_BRIGHT_MAGENTA_
		fg_bright_cyan     = FG_BRIGHT_CYAN_
		fg_bold_bright_cyan = FG_BOLD_BRIGHT_CYAN_
		fg_bright_white    = FG_BRIGHT_WHITE_
		fg_bold_yellow     = FG_BOLD_YELLOW_
		color_reset        = COLOR_RESET_
	else
		fg_bold            = ""
		fg_bright_red      = ""
		fg_bold_bright_red = ""
		fg_bold_bright_green = ""
		fg_bright_green    = ""
		fg_green           = ""
		fg_bright_blue     = ""
		fg_bright_magenta  = ""
		fg_bright_cyan     = ""
		fg_bold_bright_cyan = ""
		fg_bright_white    = ""
		fg_bold_yellow     = ""
		color_reset        = ""
	end if

	! These include fg_bold at the end, so the rest of the error message after
	! the prefix must concatenate color_reset at its end
	err_prefix     = fg_bold_bright_red//'Error'//fg_bold//': '
	err_int_prefix = fg_bold_bright_red//'Internal syntran error'//fg_bold//': '
	err_rt_prefix  = fg_bold_bright_red//'Runtime error'//fg_bold//': '
	warn_prefix    = fg_bold_yellow//'Warning'//fg_bold//': '

end subroutine set_ansi_colors

!===============================================================================

function compiler_str() result(s)

	! The Fortran compiler that built syntran, like "gfortran 15.2.0"

	character(len = :), allocatable :: s

	integer :: i

	s = fort_compiler
	do i = 1, size(fort_vers)
		s = s//merge(" ", ".", i == 1)//str(fort_vers(i))
	end do

end function compiler_str

!===============================================================================

subroutine print_version_row(label, val)

	! One detail of `--version`.  The values line up in a column

	character(len = *), intent(in) :: label, val

	integer, parameter :: width = 14

	write(*,*) "  "//fg_bright_magenta//label//color_reset// &
		repeat(" ", max(width - len(label), 1))//trim(val)

end subroutine print_version_row

!===============================================================================

subroutine print_help_opt(short, long, arg, desc, desc2)

	! One option of `--help`, in the style of `cargo -h`.  The flags are on one
	! line and the description is indented below.  `short` is blank for an option
	! without a short flag, which keeps the long flags aligned

	character(len = *), intent(in) :: short, long, arg, desc
	character(len = *), intent(in), optional :: desc2

	character(len = :), allocatable :: line

	if (len(short) > 0) then
		line = "  "//fg_bold_bright_cyan//short//color_reset//", "
	else
		line = repeat(" ", 6)
	end if
	line = line//fg_bold_bright_cyan//long//color_reset
	if (len(arg) > 0) line = line//" "//fg_bright_cyan//arg//color_reset

	write(*,*) line
	write(*,*) repeat(" ", 10)//desc
	if (present(desc2)) write(*,*) repeat(" ", 10)//desc2

end subroutine print_help_opt

!===============================================================================

function parse_args() result(args)

	! This argument parser is based on http://docopt.org/
	!
	! c.f. github.com/jeffirwin/cali

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, str_, url, version

	integer :: i, io, argc, ipos

	logical :: error = .false., interactive

	! Defaults
	args%maxerr = maxerr_def
	args%color  = COLOR_AUTO
	args%script_args = new_string_vector()

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		call get_next_arg(i, argv)

		select case (argv)
		case ("-h", "--help", "-help")
			args%help    = .true.

		case ("--fmax-errors")
			call get_next_arg(i, str_)

			read(str_, *, iostat = io) args%maxerr
			if (io /= exit_success) then
				write(*,*) err_prefix//"--fmax-errors "//str_ &
					//" is not a valid integer"
				error = .true.
			end if

		case ("--color")
			call get_next_arg(i, str_)
			args%color = str_

			select case (args%color)
			case (COLOR_AUTO)
				! Note that set_ansii_colors() is also called in main() to
				! initially default to auto, in case of errors during
				! parse_args()
				call set_ansi_colors()

			case (COLOR_ON, COLOR_OFF)
				call set_ansi_colors(args%color == COLOR_ON)

			case default
				write(*,*) err_prefix//"bad --color argument"
				error = .true.
			end select

		case ("-c", "--command")
			args%command_arg = .true.
			call get_next_arg(i, args%command)

		case ("--permissive-return")
			args%permissive_return = .true.

		case ("-q", "--quiet")
			args%quiet = .true.

		case ("-i", "--interactive")
			args%interactive = .true.

		case ("--cd")
			args%chdir = .true.

		case ("-s", "--syntax-only")
			args%syntax_only = .true.

		case ("-t", "--transpile")
			args%transpile = .true.
			call get_next_arg(i, args%transpile_out)

		case ("-o", "--native")
			args%native = .true.
			call get_next_arg(i, args%native_out)

		case ("-j", "--jit")
			args%jit = .true.

		case ("--keep-unused-fns")
			args%keep_unused_fns = .true.

		case ("--lex")
			! Hidden, so it's not in the help.  Dumps the lexer's tokens, to test
			! the syntran port of the lexer in compiler/lex.syntran
			args%lex = .true.

		case ("--version")
			args%version = .true.

		case ("--")
			! Capture all remaining arguments as script arguments
			do while (i < argc)
				call get_next_arg(i, str_)
				call args%script_args%push(str_)
			end do
			exit

		case default

			! `syntran help`, like `-h`.  Only as the first positional arg, so
			! a file named `help` has to be run as `./help`
			if (ipos == 0 .and. argv == "help") then
				args%help = .true.
				cycle
			end if

			! Positional arg
			ipos = ipos + 1

			if (ipos == 1) then
				args%syntran_file_arg = .true.
				args%syntran_file = argv

			else
				write(*,*) err_prefix//"unknown argument `"//argv//"`"
				error = .true.

			end if

		end select

	end do

	! --syntax-only has nothing to check without a file or a `-c` command.
	! Don't complain if the user just wants help or the version
	if (args%syntax_only .and. .not. args%help .and. .not. args%version .and. &
		.not. (args%syntran_file_arg .or. args%command_arg)) then
		write(*,*) err_prefix//"--syntax-only requires a <file.syntran> "// &
			"or -c <cmd> argument"
		error = .true.
	end if

	! Same for --transpile
	if (args%transpile .and. .not. args%help .and. .not. args%version .and. &
		.not. (args%syntran_file_arg .or. args%command_arg)) then
		write(*,*) err_prefix//"--transpile requires a <file.syntran> "// &
			"or -c <cmd> argument"
		error = .true.
	end if

	! `syntran -t a.syntran a.syntran` would replace the program with its translation
	if (args%transpile .and. args%syntran_file_arg .and. .not. args%help .and. &
		.not. args%version) then
		if (allocated(args%transpile_out) .and. allocated(args%syntran_file)) then
			if (args%transpile_out == args%syntran_file) then
				write(*,*) err_prefix//"--transpile output file `"//args%transpile_out// &
					"` is also the input file"
				error = .true.
			end if
		end if
	end if

	! Both of them mean not running the program, so one would silently win
	if (args%transpile .and. args%syntax_only .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--transpile and --syntax-only cannot be used together"
		error = .true.
	end if

	! --interactive would be ignored by these, which don't run the program in the REPL
	if (args%transpile .and. args%interactive .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--transpile cannot be used with --interactive"
		error = .true.
	end if

	if (args%syntax_only .and. args%interactive .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--syntax-only cannot be used with --interactive"
		error = .true.
	end if

	! Same for --native, which also needs a program and a syntran with LLVM
	if (args%native .and. .not. args%help .and. .not. args%version) then
		if (.not. (args%syntran_file_arg .or. args%command_arg)) then
			write(*,*) err_prefix//"--native requires a <file.syntran> "// &
				"or -c <cmd> argument"
			error = .true.
		end if
		if (args%transpile .or. args%syntax_only .or. args%interactive) then
			write(*,*) err_prefix//"--native cannot be used with --transpile, "// &
				"--syntax-only, or --interactive"
			error = .true.
		end if
		if (.not. native_available) then
			write(*,*) err_prefix//"--native requires a syntran that is built "// &
				"with LLVM, i.e. with SYNTRAN_LLVM"
			error = .true.
		end if
	end if

	! `syntran --native a.syntran a.syntran` would replace the program with its binary
	if (args%native .and. args%syntran_file_arg .and. .not. args%help .and. &
		.not. args%version) then
		if (allocated(args%native_out) .and. allocated(args%syntran_file)) then
			if (args%native_out == args%syntran_file) then
				write(*,*) err_prefix//"--native output file `"//args%native_out// &
					"` is also the input file"
				error = .true.
			end if
		end if
	end if

	! Unlike --transpile, "-" is not stdout, and would be a file named "-"
	if (args%native .and. .not. args%help .and. .not. args%version) then
		if (allocated(args%native_out)) then
			if (args%native_out == "-") then
				write(*,*) err_prefix//"--native needs an output file name, " &
					//"it cannot write to stdout"
				error = .true.
			end if
		end if
	end if

	! --jit compiles with LLVM like --native, but runs the program instead of
	! writing it
	if (args%jit .and. .not. args%help .and. .not. args%version) then
		if (.not. (args%syntran_file_arg .or. args%command_arg)) then
			write(*,*) err_prefix//"--jit requires a <file.syntran> "// &
				"or -c <cmd> argument"
			error = .true.
		end if
		if (args%native .or. args%transpile .or. args%syntax_only .or. &
			args%interactive) then
			write(*,*) err_prefix//"--jit cannot be used with --native, "// &
				"--transpile, --syntax-only, or --interactive"
			error = .true.
		end if
		if (.not. native_available) then
			write(*,*) err_prefix//"--jit requires a syntran that is built "// &
				"with LLVM, i.e. with SYNTRAN_LLVM"
			error = .true.
		end if
	end if

	! The program is either the file or the command, and the one that is not used
	! would be silently dropped
	if (args%command_arg .and. args%syntran_file_arg .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--command cannot be used with a <file.syntran>"
		error = .true.
	end if

	if (args%command_arg .and. args%interactive .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--command cannot be used with --interactive"
		error = .true.
	end if

	! The hidden --lex needs a file, and it's an alternative to running the program
	if (args%lex .and. .not. args%help .and. .not. args%version) then
		if (.not. args%syntran_file_arg) then
			write(*,*) err_prefix//"--lex requires a <file.syntran> argument"
			error = .true.
		end if
		if (args%command_arg .or. args%transpile .or. args%native .or. &
			args%jit .or. args%syntax_only .or. args%interactive) then
			write(*,*) err_prefix//"--lex cannot be used with --command, "// &
				"--transpile, --native, --jit, --syntax-only, or --interactive"
			error = .true.
		end if
	end if

	! It's an option of the transpiler, so it does nothing otherwise
	if (args%keep_unused_fns .and. .not. args%transpile .and. .not. args%help .and. &
		.not. args%version) then
		write(*,*) err_prefix//"--keep-unused-fns requires --transpile"
		error = .true.
	end if

	url = 'https://github.com/JeffIrwin/syntran'

	version = &
		str(syntran_major)//'.'// &
		str(syntran_minor)//'.'// &
		str(syntran_patch)

	if (.not. error .and. .not. args%quiet) then

		interactive = .not. &
			( &
				args%command_arg      .or. &
				args%syntax_only      .or. &
				args%lex              .or. &
				args%transpile        .or. &
				args%native           .or. &
				args%jit              .or. &
				args%version          .or. &
				args%syntran_file_arg .or. &
				args%help                  &
			)

		if (.not. args%command_arg .and. .not. args%syntax_only .and. &
			.not. args%lex .and. &
			.not. args%transpile .and. .not. args%native .and. .not. args%jit) then
			write(*,*)
			write(*,*) fg_bright_magenta//lang_name//' '//version//color_reset
			write(*,*) fg_bright_magenta//url//color_reset
			if (args%version) then
				write(*,*)
				call print_version_row("commit"      , git_commit)
				call print_version_row("built"       , build_date)
				call print_version_row("compiler"    , compiler_str())
				call print_version_row("bounds check", merge("on ", "off", bounds_check))
				call print_version_row("blas"        , blas_vendor())
				call print_version_row("llvm"        , merge("linked    ", "not linked", &
					native_available))
			end if
			write(*,*)
		end if

		if (interactive) then
			write(*,*) 'Usage:'
			write(*,*) tab//'`#help` for interactive help'
			write(*,*) tab//'`exit(0);` or Ctrl+D to exit'
			write(*,*)
		end if
	end if

	if (error .or. args%help) then

		write(*,*) fg_bold_bright_green//"Usage:"//color_reset//" "// &
			fg_bold_bright_cyan//"syntran"//color_reset//" "// &
			fg_bright_cyan//"[OPTIONS] [FILE] [-- <ARGS>...]"//color_reset
		write(*,*) "       "// &
			fg_bold_bright_cyan//"syntran"//color_reset//" "// &
			fg_bright_cyan//"[OPTIONS]"//color_reset//" "// &
			fg_bold_bright_cyan//"-c"//color_reset//" "// &
			fg_bright_cyan//"<CMD>"//color_reset
		write(*,*) "       "// &
			fg_bold_bright_cyan//"syntran help"//color_reset
		write(*,*)
		write(*,*) fg_bold_bright_green//"Options:"//color_reset
		call print_help_opt("-h", "--help", "", "Show this help")
		call print_help_opt("", "--version", "", "Show version and build details")
		call print_help_opt("-c", "--command", "<CMD>", "Run program passed in as string")
		call print_help_opt("", "--color", "<WHEN>", &
			"Set ANSI text color [possible values: auto, on, off] [default: auto]")
		call print_help_opt("", "--fmax-errors", "<N>", &
			"Limit max error messages to <N> [default: "//str(maxerr_def)//"]")
		call print_help_opt("-i", "--interactive", "", &
			"Interpret a file then start an interactive shell")
		call print_help_opt("-q", "--quiet", "", &
			"Don't print the banner, only errors and println calls")
		call print_help_opt("", "--permissive-return", "", &
			"Downgrade missing-return errors to warnings")
		call print_help_opt("", "--cd", "", &
			"Resolve the script's relative file paths against its own directory")
		call print_help_opt("-s", "--syntax-only", "", &
			"Parse and type check without running the program")
		call print_help_opt("-t", "--transpile", "<FILE>", &
			"Write the program as Fortran source to <FILE> ('-' for stdout)", &
			"instead of running it")
		call print_help_opt("", "--keep-unused-fns", "", &
			"With --transpile, also write fns that nothing calls")
		call print_help_opt("-o", "--native", "<FILE>", &
			"Compile the program with LLVM to an executable <FILE>", &
			"(or an object <FILE>.o, or LLVM IR <FILE>.ll)")
		call print_help_opt("-j", "--jit", "", &
			"Compile the program with LLVM and run it right away")
		call print_help_opt("", "--", "<ARGS>...", &
			"Pass remaining arguments to script via std::args()")
		write(*,*)

		if (.not. args%help) call exit(EXIT_FAILURE)
	end if

end function parse_args

!===============================================================================

end module syntran__app_m

!===============================================================================

