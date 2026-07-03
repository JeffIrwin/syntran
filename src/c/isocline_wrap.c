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
 *   - syntran_isatty():       is stdin a real terminal (vs. a pipe/file)?
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

int syntran_isatty(void)
{
#ifdef _WIN32
	return _isatty(_fileno(stdin));
#else
	return isatty(fileno(stdin));
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
