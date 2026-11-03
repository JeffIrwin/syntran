/* blas_info.c
 *
 * Which BLAS did this syntran link?  It is only reported by `syntran --version`, since
 * that is the only way to tell for a static binary, and the difference matters: an
 * optimized BLAS (OpenBLAS, Accelerate) is much faster than the reference BLAS for `@`.
 *
 * Nothing here names a BLAS routine as a symbol, so this file builds the same with or
 * without SYNTRAN_NO_BLAS.  Fortran only calls it when BLAS is linked.
 *
 * Reference BLAS has no symbol of its own, so it can only be told by elimination.  Anything
 * that is not recognized is reported as such, instead of claiming that it is the reference
 * BLAS, because it could be MKL or BLIS, etc.
 *
 * Use spaces, not tabs, and keep lines under 100 chars.
 */

#include <stddef.h>
#include <string.h>

#ifdef __APPLE__

/* Mach-O fails to link a weak reference to a symbol that no dylib defines, so look
 * symbols up at run time.  dlopen() and friends are in libSystem */
#include <dlfcn.h>

static void set_vendor(char *buf, size_t n, const char *s)
{
    strncpy(buf, s, n - 1);
    buf[n - 1] = '\0';
}

void syntran_blas_vendor(char *buf, int n)
{
    const char *(*get_config)(void);
    void *p;
    Dl_info info;

    if (n <= 0) return;

    /* A dynamic OpenBLAS, e.g. from Homebrew.  openblas_get_config() is its public API */
    *(void **)&get_config = dlsym(RTLD_DEFAULT, "openblas_get_config");
    if (get_config != NULL) {
        set_vendor(buf, (size_t)n, get_config());
        return;
    }

    /* Accelerate, which `-lblas` resolves to without any install */
    p = dlsym(RTLD_DEFAULT, "dgemm_");
    if (p != NULL && dladdr(p, &info) && info.dli_fname != NULL &&
        (strstr(info.dli_fname, "Accelerate") != NULL ||
         strstr(info.dli_fname, "vecLib") != NULL)) {
        set_vendor(buf, (size_t)n, "Accelerate");
        return;
    }

    set_vendor(buf, (size_t)n, "reference or unrecognized");
}

#else

/* In a PIE, an undefined weak symbol is still bound at load time, so a dynamic OpenBLAS is
 * found even when the BLAS was swapped for another after syntran was built.  Weak
 * references also need no -ldl on old glibc */

/* The public API of OpenBLAS.  A dynamic OpenBLAS always exports it.  A static one only
 * has it if something else pulled that archive member in, so it can't be relied on there */
extern const char *openblas_get_config(void) __attribute__((weak));

/* Called by OpenBLAS's gemm interface to get its work buffer, so the archive member that
 * defines it is always extracted along with dgemm_ when OpenBLAS is linked statically.  A
 * weak reference doesn't extract an archive member by itself.  Reference BLAS has none */
extern void *blas_memory_alloc(int) __attribute__((weak));

static void set_vendor(char *buf, size_t n, const char *s)
{
    strncpy(buf, s, n - 1);
    buf[n - 1] = '\0';
}

void syntran_blas_vendor(char *buf, int n)
{
    if (n <= 0) return;

    if (openblas_get_config != NULL) {
        set_vendor(buf, (size_t)n, openblas_get_config());
    } else if (blas_memory_alloc != NULL) {
        set_vendor(buf, (size_t)n, "OpenBLAS");
    } else {
        set_vendor(buf, (size_t)n, "reference or unrecognized");
    }
}

#endif
