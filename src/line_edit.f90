
!===============================================================================

module syntran__line_edit_m

	! Fortran binding to the vendored isocline line-editing library (git
	! submodule at external/isocline, glued in via src/c/isocline_wrap.c).
	!
	! This gives the interactive REPL history + arrow-key editing without
	! requiring users to install `rlwrap` themselves.  See read_line() in
	! utils.f90 for the plain, non-interactive line reader used for piped
	! stdin, `-c` strings, and files -- that path is unaffected by this module

	use iso_c_binding
	use iso_fortran_env, only: iostat_end

	implicit none

	interface

		function ic_readline_c(prompt_text) bind(c, name = "ic_readline") result(res)
			import :: c_ptr, c_char
			character(kind = c_char), intent(in) :: prompt_text(*)
			type(c_ptr) :: res
		end function ic_readline_c

		subroutine ic_free_c(p) bind(c, name = "ic_free")
			import :: c_ptr
			type(c_ptr), value :: p
		end subroutine ic_free_c

		subroutine ic_set_history_c(fname, max_entries) bind(c, name = "ic_set_history")
			import :: c_ptr, c_long
			type(c_ptr), value :: fname
			integer(c_long), value :: max_entries
		end subroutine ic_set_history_c

		subroutine ic_history_add_c(entry) bind(c, name = "ic_history_add")
			import :: c_char
			character(kind = c_char), intent(in) :: entry(*)
		end subroutine ic_history_add_c

		subroutine ic_history_remove_last_c() bind(c, name = "ic_history_remove_last")
		end subroutine ic_history_remove_last_c

		subroutine ic_set_prompt_marker_c(marker, cont_marker) &
			bind(c, name = "ic_set_prompt_marker")
			import :: c_char
			character(kind = c_char), intent(in) :: marker(*)
			character(kind = c_char), intent(in) :: cont_marker(*)
		end subroutine ic_set_prompt_marker_c

		function syntran_isatty_c() bind(c, name = "syntran_isatty") result(res)
			import :: c_int
			integer(c_int) :: res
		end function syntran_isatty_c

		function syntran_history_path_c() bind(c, name = "syntran_history_path") result(res)
			import :: c_ptr
			type(c_ptr) :: res
		end function syntran_history_path_c

		function ic_enable_brace_insertion_c(enable) &
			bind(c, name = "ic_enable_brace_insertion") result(was_enabled)
			import :: c_bool
			logical(c_bool), value :: enable
			logical(c_bool) :: was_enabled
		end function ic_enable_brace_insertion_c

		function c_strlen(s) bind(c, name = "strlen") result(res)
			import :: c_ptr, c_size_t
			type(c_ptr), value :: s
			integer(c_size_t) :: res
		end function c_strlen

	end interface

contains

!===============================================================================

logical function is_tty()
	! Is stdin a real terminal (vs. a pipe or redirected file)?  Line editing
	! only makes sense when it is
	is_tty = syntran_isatty_c() /= 0
end function is_tty

!===============================================================================

subroutine line_edit_init()

	! Enable persistent history.  The history file path (platform-specific:
	! $USERPROFILE on Windows, $HOME elsewhere) is resolved in the C shim.  If
	! no home dir is set, history is kept in-memory for the session only

	type(c_ptr) :: path_ptr
	logical(c_bool) :: was_enabled

	path_ptr = syntran_history_path_c()
	call ic_set_history_c(path_ptr, -1_c_long)

	! Isocline appends its own prompt marker ("> " by default) after whatever
	! prompt text we pass to ic_readline().  Syntran manages its own prompt
	! text (including continuation prompts), so disable isocline's marker to
	! avoid a doubled-up "syntran$> "
	call ic_set_prompt_marker_c(c_null_char, c_null_char)

	! Isocline auto-inserts a closing bracket/paren/quote whenever the user
	! types an opening one (enabled by default).  That's surprising for users
	! coming from a plain terminal, so turn it off
	was_enabled = ic_enable_brace_insertion_c(logical(.false., c_bool))

end subroutine line_edit_init

!===============================================================================

function read_line_interactive(prompt, iostat) result(str_)

	! Read one line of input with history + arrow-key editing.  Mirrors
	! read_line() in utils.f90: returns iostat = iostat_end on EOF (ctrl-D on
	! an empty line)

	character(len = *), intent(in) :: prompt
	integer, intent(out) :: iostat

	character(len = :), allocatable :: str_

	!********

	type(c_ptr) :: res_ptr

	! prompt is assumed-length, so it already matches the actual argument's
	! length exactly (no trailing blank padding to strip) -- trim() here would
	! wrongly strip an intentional trailing space, e.g. "syntran$ "
	res_ptr = ic_readline_c(bbcode_escape(prompt)//c_null_char)

	if (.not. c_associated(res_ptr)) then
		! EOF (ctrl-D on an empty line).  Isocline's own internal cleanup
		! (editline.c) already pushes then immediately removes its placeholder
		! for this case, netting to no change, so there is nothing here to undo
		str_ = ""
		iostat = iostat_end
		return
	end if

	str_ = c_str_to_f_str(res_ptr)
	call ic_free_c(res_ptr)
	iostat = 0

	! ic_readline() unconditionally pushes whatever the user just typed onto
	! isocline's history (and saves it to the history file) before returning,
	! UNLESS the result is empty or a single character (e.g. ctrl-C, which
	! clears the line and returns "" rather than NULL) -- in that case isocline
	! has already undone its own placeholder push internally, same as the EOF
	! case above.  We want full control over what becomes a history entry
	! (e.g. joining multi-line continuations into a single entry), so undo the
	! auto-push here -- but only when isocline actually left one in place --
	! and let the caller opt in via line_edit_add_history()
	if (len(str_) > 1) call ic_history_remove_last_c()

end function read_line_interactive

!===============================================================================

function bbcode_escape(s) result(esc)

	! Isocline renders every prompt string it's given through its own bbcode
	! markup parser (see external/isocline/src/editline.c's edit_write_prompt(),
	! which calls bbcode_print() on the prompt text).  Bbcode treats "[...]" as
	! a formatting tag, so a plain prompt like "[Hint `;`]> " gets parsed as an
	! unrecognized tag and silently dropped instead of printed.
	!
	! Escape the chars that are special to bbcode so prompt text always renders
	! literally.  Per bbcode_append() in external/isocline/src/bbcode.c, only
	! '[' and '\' are ever treated specially -- a lone ']' is never parsed as
	! part of a tag unless a preceding unescaped '[' opened one, so escaping
	! ']' is unnecessary and would actually backfire: "\]" isn't a recognized
	! escape sequence (only "\[" and "\\" are), so it would render as a stray
	! literal backslash followed by "]" instead of just "]"

	character(len = *), intent(in) :: s
	character(len = :), allocatable :: esc

	!********

	integer :: i

	esc = ""
	do i = 1, len(s)
		select case (s(i:i))
			case ("\", "[")
				esc = esc//"\"//s(i:i)
			case default
				esc = esc//s(i:i)
		end select
	end do

end function bbcode_escape

!===============================================================================

subroutine line_edit_add_history(entry)
	character(len = *), intent(in) :: entry
	call ic_history_add_c(trim(entry)//c_null_char)
end subroutine line_edit_add_history

!===============================================================================

function c_str_to_f_str(cptr) result(str_)

	! Copy a null-terminated C string into an allocatable Fortran string

	type(c_ptr), intent(in) :: cptr
	character(len = :), allocatable :: str_

	!********

	character(kind = c_char), pointer :: chars(:)
	integer(c_size_t) :: n, i

	n = c_strlen(cptr)
	if (n == 0) then
		str_ = ""
		return
	end if

	call c_f_pointer(cptr, chars, [n])

	allocate(character(len = n) :: str_)
	do i = 1, n
		str_(i:i) = chars(i)
	end do

end function c_str_to_f_str

!===============================================================================

end module syntran__line_edit_m

!===============================================================================

