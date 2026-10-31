/*
 * syntran_crt.c -- C runtime of the native backend (`syntran --native`)
 *
 * Every program that the native backend compiles is linked with this file.  It
 * is embedded in the syntran executable by src/gen_native_rt.sh, so a compiled
 * program needs nothing but a C compiler and libm to be linked.  It is also
 * compiled into the syntran library, so that it is checked on every build and
 * the unit tests can compare its number formatting to the Fortran runtime's
 * (src/rt/syntran_rt.f90).
 *
 * Use spaces, not tabs, and keep lines under 100 chars (see the generator).
 * Everything is prefixed with `sy_`.
 *
 * A str is a heap object, which the generated code passes around by pointer.
 * A literal is a constant in the program, marked immortal, so that freeing it
 * is a no-op.
 */

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int64_t len;
    int64_t imm;    /* nonzero for a literal, which is never freed */
    char data[];
} sy_str;

/*============================================================================*/

/* Write x like the Fortran edit descriptor `ESw.d` does, which is what the
 * interpreter uses for a float (es25.15 for f64, es16.6 for f32).  That is
 * `d + 1` significant digits, right-justified in `w` columns.  When the
 * exponent has 3 digits Fortran leaves out the `E` (1.000000000000000+100).
 * Returns the length, which is w.  buf needs room for w + 1 chars */
static int sy_fmt_es(double x, int d, int w, char *buf)
{
    char tmp[64];
    char *e;
    int n;

    if (isnan(x)) {
        strcpy(tmp, "NaN");
    } else if (isinf(x)) {
        strcpy(tmp, x < 0 ? "-Infinity" : "Infinity");
    } else {
        snprintf(tmp, sizeof tmp, "%.*E", d, x);
        e = strchr(tmp, 'E');
        if (e != NULL && strlen(e + 2) > 2) {
            /* Shift the sign and the 3 exponent digits left over the E */
            memmove(e, e + 1, strlen(e + 1) + 1);
        }
    }

    n = (int)strlen(tmp);
    if (n > w) {
        memset(buf, '*', (size_t)w);
    } else {
        memset(buf, ' ', (size_t)(w - n));
        memcpy(buf + (w - n), tmp, (size_t)n);
    }
    buf[w] = '\0';
    return w;
}

/* The Fortran runtime writes the 25 columns of an f64 to a buffer of 28, so
 * the interpreter's string of one has 3 blanks after it.  A compiled program
 * must have them too, or a line that ends with a float differs.  buf needs 29 */
int sy_fmt_f64(double x, char *buf)
{
    sy_fmt_es(x, 15, 25, buf);
    memcpy(buf + 25, "   ", 4);
    return 28;
}

int sy_fmt_f32(float x, char *buf)
{
    return sy_fmt_es((double)x, 6, 16, buf);
}

/*============================================================================*/

void sy_fatal(const char *msg)
{
    fflush(stdout);
    fprintf(stderr, "Runtime error: %s\n", msg);
    exit(1);
}

static sy_str *sy_str_alloc(int64_t len)
{
    sy_str *s = (sy_str *)malloc(sizeof(sy_str) + (size_t)len + 1);
    if (s == NULL) sy_fatal("out of memory");
    s->len = len;
    s->imm = 0;
    s->data[len] = '\0';
    return s;
}

static sy_str *sy_str_from(const char *p, int64_t len)
{
    sy_str *s = sy_str_alloc(len);
    memcpy(s->data, p, (size_t)len);
    return s;
}

void sy_str_free(sy_str *s)
{
    if (s != NULL && !s->imm) free(s);
}

/* A copy that can be owned.  A literal is immortal, so it can be shared */
sy_str *sy_str_dup(sy_str *s)
{
    if (s->imm) return s;
    return sy_str_from(s->data, s->len);
}

sy_str *sy_str_cat(const sy_str *a, const sy_str *b)
{
    sy_str *s = sy_str_alloc(a->len + b->len);
    memcpy(s->data, a->data, (size_t)a->len);
    memcpy(s->data + a->len, b->data, (size_t)b->len);
    return s;
}

/* Equality and ordering of two strings.  Unlike Fortran's operators, these
 * don't pad the shorter string with blanks, so "a" is not "a ".  A string that
 * is a strict prefix of another sorts first, and characters compare as unsigned
 * bytes, which is their order in ASCII */
int8_t sy_str_eq(const sy_str *a, const sy_str *b)
{
    return a->len == b->len && memcmp(a->data, b->data, (size_t)a->len) == 0;
}

int8_t sy_str_lt(const sy_str *a, const sy_str *b)
{
    int64_t n = a->len < b->len ? a->len : b->len;
    int c = memcmp(a->data, b->data, (size_t)n);

    if (c != 0) return c < 0;
    return a->len < b->len;
}

/* repeat(s, n) */
sy_str *sy_str_repeat(const sy_str *s, int64_t n)
{
    sy_str *r;
    int64_t i;

    if (n < 0) n = 0;
    r = sy_str_alloc(s->len * n);
    for (i = 0; i < n; i++) memcpy(r->data + i * s->len, s->data, (size_t)s->len);
    return r;
}

/* char(c), the string of one character */
sy_str *sy_str_char(int32_t c)
{
    char ch = (char)c;

    return sy_str_from(&ch, 1);
}

/* i32(s) of a string, the code of its first character */
int32_t sy_str_code(const sy_str *s)
{
    return s->len > 0 ? (int32_t)(unsigned char)s->data[0] : 0;
}

/*============================================================================*/

sy_str *sy_str_bool(int8_t x)
{
    return x ? sy_str_from("true", 4) : sy_str_from("false", 5);
}

sy_str *sy_str_i32(int32_t x)
{
    char buf[16];
    return sy_str_from(buf, snprintf(buf, sizeof buf, "%d", (int)x));
}

sy_str *sy_str_i64(int64_t x)
{
    char buf[24];
    return sy_str_from(buf, snprintf(buf, sizeof buf, "%lld", (long long)x));
}

sy_str *sy_str_f32(float x)
{
    char buf[32];
    return sy_str_from(buf, sy_fmt_f32(x, buf));
}

sy_str *sy_str_f64(double x)
{
    char buf[32];
    return sy_str_from(buf, sy_fmt_f64(x, buf));
}

/*============================================================================*/

/* println() writes all of its arguments, which are already strings, as one
 * line */
void sy_println(sy_str *const *parts, int32_t n)
{
    int32_t i;

    for (i = 0; i < n; i++) fwrite(parts[i]->data, 1, (size_t)parts[i]->len, stdout);
    fputc('\n', stdout);
}

/* print() is println() without ending the line.  It flushes, so that a prompt
 * is shown before a readln() blocks */
void sy_print(sy_str *const *parts, int32_t n)
{
    int32_t i;

    for (i = 0; i < n; i++) fwrite(parts[i]->data, 1, (size_t)parts[i]->len, stdout);
    fflush(stdout);
}

/* The value of the program's last statement, which the CLI prints after it
 * finishes: indented with 4 spaces, and a 5th from list-directed output in
 * the Fortran backends.  If `trim`, like the CLI does for a file, it has no
 * leading or trailing blanks */
void sy_result(const sy_str *s, int32_t trim)
{
    const char *p = s->data;
    int64_t n = s->len;

    if (trim) {
        while (n > 0 && *p == ' ') {
            p++;
            n--;
        }
        while (n > 0 && p[n - 1] == ' ') n--;
    }
    fputs("     ", stdout);
    fwrite(p, 1, (size_t)n, stdout);
    fputc('\n', stdout);
}

void sy_exit(int32_t code)
{
    printf(" Exiting syntran with status %d\n", (int)code);
    fflush(stdout);
    exit((int)code);
}

/*============================================================================*/

/* Integer powers, with the semantics of Fortran's `**`, which the interpreter
 * uses.  A negative exponent is 1 / n ** -m in integers, which is 0 unless n
 * is 1 or -1 */
int32_t sy_ipow_i32(int32_t n, int32_t m)
{
    uint32_t r = 1, b = (uint32_t)n;
    uint32_t e;

    if (m < 0) {
        if (n == 1) return 1;
        if (n == -1) return (m & 1) ? -1 : 1;
        return 0;
    }
    for (e = (uint32_t)m; e > 0; e >>= 1) {
        if (e & 1) r *= b;
        b *= b;
    }
    return (int32_t)r;
}

int64_t sy_ipow_i64(int64_t n, int64_t m)
{
    uint64_t r = 1, b = (uint64_t)n;
    uint64_t e;

    if (m < 0) {
        if (n == 1) return 1;
        if (n == -1) return (m & 1) ? -1 : 1;
        return 0;
    }
    for (e = (uint64_t)m; e > 0; e >>= 1) {
        if (e & 1) r *= b;
        b *= b;
    }
    return (int64_t)r;
}

/*============================================================================*/
