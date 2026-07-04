/* isocline_wrap.c
 *
 * Thin C shim gluing the vendored isocline line-editing library (git
 * submodule at external/isocline) into the syntran build under both FPM and
 * CMake.
 *
 * Isocline builds as a single translation unit: its own src/isocline.c
 * #includes all of its sibling .c files.  We #include that file here (via a
 * quoted, relative path) so both build systems only need to know about this
 * one shim file rather than isocline's whole file list.
 *
 * This file also provides two small platform-specific helpers that syntran's
 * REPL needs and that don't belong in Fortran:
 *   - syntran_isatty():       are stdin AND stdout real terminals?
 *   - syntran_history_path(): where should REPL history persist?
 * Both differ by platform in ways that are simplest to resolve here, where
 * the C compiler predefines _WIN32 automatically (no build-system flag
 * needed on either FPM or CMake).
 */

#include "../../external/isocline/src/isocline.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

/* Isocline should only take over the terminal when BOTH stdin and stdout are
 * real terminals.  If stdout is redirected (e.g. `syntran | tee log`),
 * isocline's cursor-control and redraw escape sequences would otherwise leak
 * into the pipe even though stdin is still an interactive tty.
 */
int syntran_isatty(void)
{
#ifdef _WIN32
	return _isatty(_fileno(stdin)) && _isatty(_fileno(stdout));
#else
	return isatty(fileno(stdin)) && isatty(fileno(stdout));
#endif
}

/* Non-zero when stdin is an MSYS2/Cygwin/MinTTY interactive pty pipe.
 *
 * A native _WIN32 exe cannot use isocline's POSIX termios path in MinTTY
 * because stdin is an MSYS named pipe rather than a real Windows console
 * handle, so _isatty() returns 0 and we fall back to the plain read path.
 * This helper detects that case so the REPL can print a "use winpty" hint.
 *
 * Detection uses the stdin handle's NT object name: MSYS2/Cygwin pty master
 * pipes have names of the form \msys-<hash>-ptyN-... or \cygwin-<hash>-ptyN-...
 * That distinguishes an interactive MinTTY session from a regular shell pipe
 * (echo 'x;' | syntran), which would have an ordinary anonymous pipe name.
 *
 * Returns 0 on non-Windows builds.
 */
int syntran_is_mintty(void)
{
#ifdef _WIN32
	HANDLE h;
	DWORD ftype;
	/* FILE_NAME_INFO: DWORD FileNameLength + WCHAR FileName[] */
	struct { DWORD len; WCHAR name[512]; } info;
	char narrow[512];
	int n;

	h = (HANDLE)_get_osfhandle(_fileno(stdin));
	if (h == INVALID_HANDLE_VALUE) return 0;

	ftype = GetFileType(h);
	if (ftype != FILE_TYPE_PIPE) return 0;

	if (!GetFileInformationByHandleEx(h, FileNameInfo, &info, sizeof(info)))
		return 0;

	n = WideCharToMultiByte(CP_UTF8, 0, info.name,
	                        info.len / sizeof(WCHAR),
	                        narrow, (int)sizeof(narrow) - 1, NULL, NULL);
	if (n <= 0) return 0;
	narrow[n] = '\0';

	return (strstr(narrow, "msys-") != NULL || strstr(narrow, "cygwin-") != NULL)
	       && strstr(narrow, "-pty") != NULL;
#else
	return 0;
#endif
}

/* Platform-correct history file path, or NULL if no home dir is set (in
 * which case history is kept in-memory only for the session).
 *
 * HOME is normally unset on Windows, so USERPROFILE is used there instead.
 */
const char *syntran_history_path(void)
{
	static char path[1024];

	const char *home =
#ifdef _WIN32
		getenv("USERPROFILE");
#else
		getenv("HOME");
#endif

	if (home == NULL || home[0] == '\0') return NULL;

	snprintf(path, sizeof(path), "%s/%s", home, ".syntran_history");
	return path;
}
