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

#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

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

/* parse_i32() and friends.  A string that isn't a number is a fatal error.  Like
 * Fortran's list-directed read, which the interpreter uses, the number may have
 * blanks around it, and may be followed by a comma or slash and anything */
static const char *sy_blanks(const char *p)
{
    while (*p == ' ' || *p == '\t') p++;
    return p;
}

static int sy_end_ok(const char *p)
{
    p = sy_blanks(p);
    return *p == '\0' || *p == ',' || *p == '/';
}

static void sy_parse_fail(const char *fn, const sy_str *s)
{
    char msg[512];

    snprintf(msg, sizeof msg, " cannot %s() for argument `%.400s`", fn, s->data);
    sy_fatal(msg);
}

static int64_t sy_parse_int(const char *fn, const sy_str *s, int64_t lo, int64_t hi)
{
    char *end;
    long long x;

    errno = 0;
    x = strtoll(sy_blanks(s->data), &end, 10);
    if (end == sy_blanks(s->data) || errno != 0 || x < lo || x > hi || !sy_end_ok(end)) {
        sy_parse_fail(fn, s);
    }
    return (int64_t)x;
}

int32_t sy_parse_i32(const sy_str *s)
{
    return (int32_t)sy_parse_int("parse_i32", s, INT32_MIN, INT32_MAX);
}

int64_t sy_parse_i64(const sy_str *s)
{
    return sy_parse_int("parse_i64", s, INT64_MIN, INT64_MAX);
}

/* A Fortran exponent can be introduced by d or D */
static double sy_parse_real(const char *fn, const sy_str *s)
{
    char buf[512], *end, *q;
    const char *p = sy_blanks(s->data);
    double x;

    snprintf(buf, sizeof buf, "%.500s", p);
    for (q = buf; *q != '\0'; q++) {
        if (*q == 'd' || *q == 'D') *q = 'e';
    }
    x = strtod(buf, &end);
    if (end == buf || !sy_end_ok(end)) sy_parse_fail(fn, s);
    return x;
}

float sy_parse_f32(const sy_str *s)
{
    return (float)sy_parse_real("parse_f32", s);
}

double sy_parse_f64(const sy_str *s)
{
    return sy_parse_real("parse_f64", s);
}

int8_t sy_parse_bool(const sy_str *s)
{
    const char *p = sy_blanks(s->data);
    size_t n = strlen(p);

    while (n > 0 && p[n - 1] == ' ') n--;
    if (n == 4 && strncmp(p, "true", 4) == 0) return 1;
    if (n == 5 && strncmp(p, "false", 5) == 0) return 0;
    sy_parse_fail("parse_bool", s);
    return 0;
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

/* An array is a heap object like a str, which the generated code passes around
 * by pointer.  Its size is fixed once it's made.  The fields before `size` are
 * at fixed offsets that the generated code reads directly (len 0, rank 8, elem
 * 12, data 16, size 24), so don't reorder them.  Elements are stored in
 * column-major order, like the interpreter's, and a str element is an owned
 * pointer which is never NULL */
enum { SY_I32, SY_I64, SY_F32, SY_F64, SY_BOOL, SY_STR, SY_ARR, SY_STRUCT, SY_ENUM, SY_FN };

/* A struct is defined below the arrays, which own them when they are elements */
typedef struct sy_struct sy_struct;
void sy_struct_free(sy_struct *s);
sy_struct *sy_struct_dup(const sy_struct *s);

typedef struct {
    int64_t len;
    int32_t rank;
    int32_t elem;
    char *data;
    int64_t size[];
} sy_arr;

/* The empty string that a new array of strs is full of.  It's immortal */
static struct { int64_t len; int64_t imm; char data[1]; } sy_empty_s = {0, 1, {0}};
#define SY_EMPTY ((sy_str *)&sy_empty_s)

static size_t sy_esize(int32_t e)
{
    switch (e) {
    case SY_I32:
    case SY_F32:
        return 4;
    case SY_BOOL:
        return 1;
    default:
        return 8;
    }
}

/* Does an element own a heap object, i.e. a str or a struct? */
static int sy_eptr(int32_t e)
{
    return e == SY_STR || e == SY_STRUCT;
}

static void sy_eptr_free(int32_t e, void *p)
{
    if (e == SY_STR) sy_str_free((sy_str *)p);
    else sy_struct_free((sy_struct *)p);
}

static void *sy_eptr_dup(int32_t e, void *p)
{
    if (p == NULL) return NULL;
    if (e == SY_STR) return sy_str_dup((sy_str *)p);
    return sy_struct_dup((const sy_struct *)p);
}

/* A zeroed array of the given shape */
sy_arr *sy_arr_new(int32_t elem, int32_t rank, const int64_t *sizes)
{
    int64_t len = 1, i;
    sy_arr *a = (sy_arr *)malloc(sizeof(sy_arr) + (size_t)rank * sizeof(int64_t));

    if (a == NULL) sy_fatal("out of memory");
    for (i = 0; i < rank; i++) {
        a->size[i] = sizes[i] > 0 ? sizes[i] : 0;
        len *= a->size[i];
    }
    a->len = len;
    a->rank = rank;
    a->elem = elem;
    a->data = (char *)calloc(len > 0 ? (size_t)len : 1, sy_esize(elem));
    if (a->data == NULL) sy_fatal("out of memory");
    if (elem == SY_STR) {
        for (i = 0; i < len; i++) ((sy_str **)a->data)[i] = SY_EMPTY;
    }
    return a;
}

static sy_arr *sy_arr_new1(int32_t elem, int64_t n)
{
    return sy_arr_new(elem, 1, &n);
}

void sy_arr_free(sy_arr *a)
{
    int64_t i;

    if (a == NULL) return;
    if (sy_eptr(a->elem)) {
        for (i = 0; i < a->len; i++) sy_eptr_free(a->elem, ((void **)a->data)[i]);
    }
    free(a->data);
    free(a);
}

/* A deep copy that can be owned */
sy_arr *sy_arr_dup(const sy_arr *a)
{
    sy_arr *r;
    int64_t i;

    if (a == NULL) return NULL;
    r = sy_arr_new(a->elem, a->rank, a->size);
    if (sy_eptr(a->elem)) {
        for (i = 0; i < a->len; i++) {
            sy_eptr_free(a->elem, ((void **)r->data)[i]);
            ((void **)r->data)[i] = sy_eptr_dup(a->elem, ((void **)a->data)[i]);
        }
    } else {
        memcpy(r->data, a->data, (size_t)a->len * sy_esize(a->elem));
    }
    return r;
}

/* a == b for the switch statement: the same shape and the same elements */
int8_t sy_arr_eq(const sy_arr *a, const sy_arr *b)
{
    int32_t k;
    int64_t i;

    if (a == NULL || b == NULL) return a == b;
    if (a->rank != b->rank || a->len != b->len) return 0;
    for (k = 0; k < a->rank; k++) {
        if (a->size[k] != b->size[k]) return 0;
    }
    if (a->elem != b->elem) {
        /* An i32 array can be equal to an i64 one */
        for (i = 0; i < a->len; i++) {
            int64_t x = a->elem == SY_I64 ? ((int64_t *)a->data)[i] : ((int32_t *)a->data)[i];
            int64_t y = b->elem == SY_I64 ? ((int64_t *)b->data)[i] : ((int32_t *)b->data)[i];

            if (x != y) return 0;
        }
        return 1;
    }
    for (i = 0; i < a->len; i++) {
        switch (a->elem) {
        case SY_I32:
            if (((int32_t *)a->data)[i] != ((int32_t *)b->data)[i]) return 0;
            break;
        case SY_I64:
            if (((int64_t *)a->data)[i] != ((int64_t *)b->data)[i]) return 0;
            break;
        case SY_F32:
            if (((float *)a->data)[i] != ((float *)b->data)[i]) return 0;
            break;
        case SY_F64:
            if (((double *)a->data)[i] != ((double *)b->data)[i]) return 0;
            break;
        case SY_BOOL:
            if (a->data[i] != b->data[i]) return 0;
            break;
        default:
            if (!sy_str_eq(((sy_str **)a->data)[i], ((sy_str **)b->data)[i])) return 0;
            break;
        }
    }
    return 1;
}

/* A zeroed array of the shape of `a`, of another element type */
sy_arr *sy_arr_like(const sy_arr *a, int32_t elem)
{
    return sy_arr_new(elem, a->rank, a->size);
}

/* The same elements in another shape.  This takes `a` and gives it a new header */
sy_arr *sy_arr_reshape(sy_arr *a, int32_t rank, const int64_t *sizes)
{
    int64_t i;
    sy_arr *r = (sy_arr *)malloc(sizeof(sy_arr) + (size_t)rank * sizeof(int64_t));

    if (r == NULL) sy_fatal("out of memory");
    r->len = a->len;
    r->rank = rank;
    r->elem = a->elem;
    r->data = a->data;
    for (i = 0; i < rank; i++) r->size[i] = sizes[i];
    free(a);
    return r;
}

/* [v; n, m].  `val` points to a value of the element type.  A str is copied */
sy_arr *sy_arr_fill(int32_t elem, int32_t rank, const int64_t *sizes, const void *val)
{
    sy_arr *a = sy_arr_new(elem, rank, sizes);
    size_t es = sy_esize(elem);
    int64_t i;

    for (i = 0; i < a->len; i++) {
        if (sy_eptr(elem)) {
            sy_eptr_free(elem, ((void **)a->data)[i]);
            ((void **)a->data)[i] = sy_eptr_dup(elem, *(void *const *)val);
        } else {
            memcpy(a->data + (size_t)i * es, val, es);
        }
    }
    return a;
}

/* Copy all of `src` into `dst`, starting at the element `at` */
void sy_arr_copy_into(sy_arr *dst, int64_t at, const sy_arr *src)
{
    int64_t i;

    if (src == NULL) return;
    if (sy_eptr(dst->elem)) {
        for (i = 0; i < src->len; i++) {
            sy_eptr_free(dst->elem, ((void **)dst->data)[at + i]);
            ((void **)dst->data)[at + i] = sy_eptr_dup(dst->elem, ((void **)src->data)[i]);
        }
    } else {
        memcpy(dst->data + (size_t)at * sy_esize(dst->elem), src->data,
            (size_t)src->len * sy_esize(src->elem));
    }
}

/*============================================================================*/

/* [lb: ub], which is empty if ub <= lb */
sy_arr *sy_arr_range_i32(int32_t lb, int32_t ub)
{
    int64_t n = (int64_t)ub - (int64_t)lb, i;
    sy_arr *a;

    if (n < 0) n = 0;
    a = sy_arr_new1(SY_I32, n);
    for (i = 0; i < n; i++) ((int32_t *)a->data)[i] = (int32_t)((int64_t)lb + i);
    return a;
}

sy_arr *sy_arr_range_i64(int64_t lb, int64_t ub)
{
    int64_t n = ub > lb ? ub - lb : 0, i;
    sy_arr *a = sy_arr_new1(SY_I64, n);

    for (i = 0; i < n; i++) ((int64_t *)a->data)[i] = lb + i;
    return a;
}

/* [lb: step: ub].  The bound is exclusive and the step may be negative.  This is
 * the same algorithm as the interpreter's, including its loop condition */
sy_arr *sy_arr_step_i32(int32_t lb, int32_t step, int32_t ub)
{
    int64_t cap, j = 0;
    int32_t i;
    sy_arr *a;

    if (step == 0) sy_fatal("array step is 0");
    cap = ((int64_t)ub - lb + step - (step < 0 ? -1 : 1)) / step;
    a = sy_arr_new1(SY_I32, cap > 0 ? cap : 0);
    i = lb;
    if ((lb < ub) != (0 < step)) i = ub;
    while (((i < ub) == (lb < ub)) && i != ub) {
        ((int32_t *)a->data)[j++] = i;
        i += step;
    }
    a->len = a->size[0] = j;
    return a;
}

sy_arr *sy_arr_step_i64(int64_t lb, int64_t step, int64_t ub)
{
    int64_t cap, j = 0, i;
    sy_arr *a;

    if (step == 0) sy_fatal("array step is 0");
    cap = (ub - lb + step - (step < 0 ? -1 : 1)) / step;
    a = sy_arr_new1(SY_I64, cap > 0 ? cap : 0);
    i = lb;
    if ((lb < ub) != (0 < step)) i = ub;
    while (((i < ub) == (lb < ub)) && i != ub) {
        ((int64_t *)a->data)[j++] = i;
        i += step;
    }
    a->len = a->size[0] = j;
    return a;
}

/* A float step multiplies instead of repeatedly adding, so that rounding errors
 * don't accumulate, e.g. [0.0: 0.1: 0.9] */
sy_arr *sy_arr_step_f32(float lb, float step, float ub)
{
    int64_t cap, j = 0;
    float f;
    sy_arr *a;

    if (!(step > 0.0f || step < 0.0f)) sy_fatal("array step is 0.0");
    cap = (int64_t)ceilf((ub - lb) / step);
    a = sy_arr_new1(SY_F32, cap > 0 ? cap : 0);
    f = lb;
    if ((lb < ub) != (0 < step)) f = ub;
    while (((f < ub) == (lb < ub)) && (f < ub || f > ub)) {
        ((float *)a->data)[j] = f;
        j++;
        f = lb + (float)j * step;
    }
    a->len = a->size[0] = j;
    return a;
}

sy_arr *sy_arr_step_f64(double lb, double step, double ub)
{
    int64_t cap, j = 0;
    double f;
    sy_arr *a;

    if (!(step > 0.0 || step < 0.0)) sy_fatal("array step is 0.0");
    cap = (int64_t)ceil((ub - lb) / step);
    a = sy_arr_new1(SY_F64, cap > 0 ? cap : 0);
    f = lb;
    if ((lb < ub) != (0 < step)) f = ub;
    while (((f < ub) == (lb < ub)) && (f < ub || f > ub)) {
        ((double *)a->data)[j] = f;
        j++;
        f = lb + (double)j * step;
    }
    a->len = a->size[0] = j;
    return a;
}

/* [lb: ub; n], n evenly spaced values including both ends */
sy_arr *sy_arr_linspace_f32(float lb, float ub, int64_t n)
{
    sy_arr *a = sy_arr_new1(SY_F32, n);
    float dx = (ub - lb) / (float)(n - 1);
    int64_t i;

    for (i = 0; i < a->len; i++) ((float *)a->data)[i] = lb + (float)i * dx;
    return a;
}

sy_arr *sy_arr_linspace_f64(double lb, double ub, int64_t n)
{
    sy_arr *a = sy_arr_new1(SY_F64, n);
    double dx = (ub - lb) / (double)(n - 1);
    int64_t i;

    for (i = 0; i < a->len; i++) ((double *)a->data)[i] = lb + (double)i * dx;
    return a;
}

/*============================================================================*/

/* One subscript of an array or a string, as the generated code builds it: what
 * kind it is, its bounds and step, which bounds were left out (bit 0 for the
 * lower, bit 1 for the upper), and an array of indices if it is a vector */
enum { SUB_SCALAR, SUB_RANGE, SUB_ALL, SUB_STEP, SUB_VEC };

typedef struct {
    int64_t kind, lo, hi, st, omit;
    const sy_arr *vec;
} sy_sub;

#define SY_MAX_RANK 16

/* The zero-based indices that a subscript selects out of a dimension that has
 * the extent n, in a list that the caller frees.  A range's upper bound is
 * exclusive.  An omitted bound of a stepped subscript depends on the sign of
 * the step, and the loop is the one of sy_arr_step_i64() */
static int64_t *sy_sub_idx(const sy_sub *s, int64_t n, int64_t *count)
{
    int64_t lo, hi, st, i, j = 0, cnt;
    int64_t *idx;

    switch (s->kind) {
    case SUB_SCALAR:
        idx = (int64_t *)malloc(sizeof(int64_t));
        idx[0] = s->lo;
        *count = 1;
        return idx;

    case SUB_RANGE:
    case SUB_ALL:
        lo = (s->kind == SUB_ALL || (s->omit & 1)) ? 0 : s->lo;
        hi = (s->kind == SUB_ALL || (s->omit & 2)) ? n : s->hi;
        cnt = hi > lo ? hi - lo : 0;
        idx = (int64_t *)malloc((size_t)(cnt > 0 ? cnt : 1) * sizeof(int64_t));
        for (i = 0; i < cnt; i++) idx[i] = lo + i;
        *count = cnt;
        return idx;

    case SUB_STEP:
        st = s->st;
        if (st == 0) sy_fatal("subscript step is 0");
        lo = (s->omit & 1) ? (st < 0 ? n - 1 : 0) : s->lo;
        hi = (s->omit & 2) ? (st < 0 ? -1 : n) : s->hi;
        cnt = (hi - lo + st - (st < 0 ? -1 : 1)) / st;
        idx = (int64_t *)malloc((size_t)(cnt > 0 ? cnt : 1) * sizeof(int64_t));
        i = lo;
        if ((lo < hi) != (0 < st)) i = hi;
        while (((i < hi) == (lo < hi)) && i != hi) {
            idx[j++] = i;
            i += st;
        }
        *count = j;
        return idx;

    default:
        cnt = s->vec != NULL ? s->vec->len : 0;
        idx = (int64_t *)malloc((size_t)(cnt > 0 ? cnt : 1) * sizeof(int64_t));
        for (i = 0; i < cnt; i++) {
            idx[i] = s->vec->elem == SY_I64 ? ((int64_t *)s->vec->data)[i]
                : (int64_t)((int32_t *)s->vec->data)[i];
        }
        *count = cnt;
        return idx;
    }
}

/* Walks the elements that a subscript of each dimension of an array selects, in
 * column-major order.  The position of the t-th one in the array is `lin` */
typedef struct {
    int32_t rank;
    int64_t *idx[SY_MAX_RANK];
    int64_t cnt[SY_MAX_RANK], pos[SY_MAX_RANK], ext[SY_MAX_RANK];
    int64_t total;
} sy_walk;

static void sy_walk_init(sy_walk *w, const sy_arr *a, const sy_sub *subs)
{
    int32_t k;

    if (a->rank > SY_MAX_RANK) sy_fatal("array rank is too big");
    w->rank = a->rank;
    w->total = 1;
    for (k = 0; k < a->rank; k++) {
        w->idx[k] = sy_sub_idx(&subs[k], a->size[k], &w->cnt[k]);
        w->ext[k] = a->size[k];
        w->pos[k] = 0;
        w->total *= w->cnt[k];
    }
}

static int64_t sy_walk_next(sy_walk *w)
{
    int32_t k;
    int64_t lin = 0, mult = 1;

    for (k = 0; k < w->rank; k++) {
        lin += w->idx[k][w->pos[k]] * mult;
        mult *= w->ext[k];
    }
    for (k = 0; k < w->rank; k++) {
        if (++w->pos[k] < w->cnt[k]) break;
        w->pos[k] = 0;
    }
    return lin;
}

static void sy_walk_free(sy_walk *w)
{
    int32_t k;

    for (k = 0; k < w->rank; k++) free(w->idx[k]);
}

/* a[subs], where at least one subscript is not a single element.  The
 * dimensions of single elements are dropped from the shape of the result */
sy_arr *sy_arr_slice(const sy_arr *a, const sy_sub *subs)
{
    sy_walk w;
    int64_t sizes[SY_MAX_RANK], t, lin;
    int32_t k, rr = 0;
    size_t es = sy_esize(a->elem);
    sy_arr *out;

    sy_walk_init(&w, a, subs);
    for (k = 0; k < a->rank; k++) {
        if (subs[k].kind != SUB_SCALAR) sizes[rr++] = w.cnt[k];
    }
    out = sy_arr_new(a->elem, rr, sizes);

    for (t = 0; t < out->len; t++) {
        lin = sy_walk_next(&w);
        if (sy_eptr(a->elem)) {
            sy_eptr_free(a->elem, ((void **)out->data)[t]);
            ((void **)out->data)[t] = sy_eptr_dup(a->elem, ((void **)a->data)[lin]);
        } else {
            memcpy(out->data + (size_t)t * es, a->data + (size_t)lin * es, es);
        }
    }
    sy_walk_free(&w);
    return out;
}

/* a[subs] = rhs, or a[subs] = *scalar if rhs is NULL, where `scalar` points to
 * a value of the element type */
void sy_arr_slice_set(sy_arr *a, const sy_sub *subs, const sy_arr *rhs, const void *scalar)
{
    sy_walk w;
    int64_t t, lin;
    size_t es = sy_esize(a->elem);
    sy_arr *tmp = NULL;

    sy_walk_init(&w, a, subs);
    if (rhs != NULL && rhs->len != w.total) {
        sy_walk_free(&w);
        sy_fatal("size of RHS does not match size of LHS slice");
    }
    if (rhs == a) rhs = tmp = sy_arr_dup(a);

    for (t = 0; t < w.total; t++) {
        lin = sy_walk_next(&w);
        if (sy_eptr(a->elem)) {
            void *src = rhs != NULL ? ((void **)rhs->data)[t] : *(void *const *)scalar;

            sy_eptr_free(a->elem, ((void **)a->data)[lin]);
            ((void **)a->data)[lin] = sy_eptr_dup(a->elem, src);
        } else {
            memcpy(a->data + (size_t)lin * es,
                rhs != NULL ? rhs->data + (size_t)t * es : (const char *)scalar, es);
        }
    }
    sy_arr_free(tmp);
    sy_walk_free(&w);
}

/* The i-th character of s, for iterating over it */
sy_str *sy_str_at(const sy_str *s, int64_t i)
{
    return sy_str_from(s->data + i, 1);
}

/* s[sub], a character or a substring */
sy_str *sy_str_sub(const sy_str *s, const sy_sub *sub)
{
    int64_t cnt, i;
    int64_t *idx = sy_sub_idx(sub, s->len, &cnt);
    sy_str *r = sy_str_alloc(cnt);

    for (i = 0; i < cnt; i++) r->data[i] = s->data[idx[i]];
    free(idx);
    return r;
}

/* s[sub] = rhs, where `slot` is the variable that holds s.  A string is only
 * changed in place if it isn't a literal, otherwise the variable gets a copy.
 * The rhs of a single character or a range is truncated or padded with blanks
 * to the length of the substring, like an assignment to a substring is in
 * Fortran.  Otherwise it must be as long as the number of characters */
void sy_str_set(sy_str **slot, const sy_sub *sub, const sy_str *rhs)
{
    sy_str *s = *slot;
    int64_t cnt, i;
    int64_t *idx = sy_sub_idx(sub, s->len, &cnt);
    int fill = sub->kind == SUB_SCALAR || sub->kind == SUB_RANGE || sub->kind == SUB_ALL;

    if (!fill && rhs->len != cnt) {
        free(idx);
        sy_fatal("size of RHS does not match size of LHS slice");
    }
    if (s->imm) s = *slot = sy_str_from(s->data, s->len);
    for (i = 0; i < cnt; i++) s->data[idx[i]] = i < rhs->len ? rhs->data[i] : ' ';
    free(idx);
}

/* a[subs], where a is an array of strs, and the last of the rank + 1 subscripts is
 * a substring of each of the elements that the others select.  The result has
 * the shape of a slice, which is a single element if they are all scalars */
sy_arr *sy_arr_str_sub(const sy_arr *a, const sy_sub *subs)
{
    sy_walk w;
    int64_t sizes[SY_MAX_RANK], t, lin;
    int32_t k, rr = 0;
    sy_arr *out;

    sy_walk_init(&w, a, subs);
    for (k = 0; k < a->rank; k++) {
        if (subs[k].kind != SUB_SCALAR) sizes[rr++] = w.cnt[k];
    }
    out = sy_arr_new(SY_STR, rr, sizes);

    for (t = 0; t < out->len; t++) {
        lin = sy_walk_next(&w);
        sy_str_free(((sy_str **)out->data)[t]);
        ((sy_str **)out->data)[t] = sy_str_sub(((sy_str **)a->data)[lin], &subs[a->rank]);
    }
    sy_walk_free(&w);
    return out;
}

/* a[subs] = rhs, for the same subscripts */
void sy_arr_str_set(sy_arr *a, const sy_sub *subs, const sy_str *rhs)
{
    sy_walk w;
    int64_t t;

    sy_walk_init(&w, a, subs);
    for (t = 0; t < w.total; t++) {
        sy_str_set(&((sy_str **)a->data)[sy_walk_next(&w)], &subs[a->rank], rhs);
    }
    sy_walk_free(&w);
}

/*============================================================================*/

typedef struct {
    char *p;
    size_t n, cap;
} sy_buf;

static void sy_buf_put(sy_buf *b, const char *s, size_t n)
{
    if (b->n + n + 1 > b->cap) {
        b->cap = 2 * (b->n + n + 1);
        b->p = (char *)realloc(b->p, b->cap);
        if (b->p == NULL) sy_fatal("out of memory");
    }
    memcpy(b->p + b->n, s, n);
    b->n += n;
}

static void sy_arr_put(sy_buf *b, const sy_arr *a, int rt, const void *aux);
static void sy_struct_put(sy_buf *b, const sy_struct *s);

/* One element of an array, or one member of a struct, from the address that it's
 * stored at.  `rt` is the round-trip form that a struct uses for its members,
 * which quotes a str and gives an i64 or an f32 a suffix, so that it reads back
 * as a literal.  The elements of a float array or struct are trimmed, unlike a
 * scalar's.  An enum is the index of its variant, and `aux` is the table of the
 * names of the variants, which is also that of an array of them */
static void sy_elem_put(sy_buf *b, int32_t kind, const void *p, int rt, const void *aux)
{
    char tmp[40];
    const char *q = tmp;
    const sy_str *s;
    size_t n, i;

    switch (kind) {
    case SY_I32:
        n = (size_t)snprintf(tmp, sizeof tmp, "%d", (int)*(const int32_t *)p);
        break;
    case SY_I64:
        n = (size_t)snprintf(tmp, sizeof tmp, "%lld", (long long)*(const int64_t *)p);
        break;
    case SY_F32:
    case SY_F64:
        n = (size_t)(kind == SY_F32 ? sy_fmt_f32(*(const float *)p, tmp)
            : sy_fmt_f64(*(const double *)p, tmp));
        while (n > 0 && tmp[n - 1] == ' ') n--;
        while (n > 0 && *q == ' ') {
            q++;
            n--;
        }
        break;
    case SY_BOOL:
        q = *(const int8_t *)p ? "true" : "false";
        n = strlen(q);
        break;
    case SY_ENUM:
        s = ((sy_str *const *)aux)[*(const int32_t *)p];
        sy_buf_put(b, s->data, (size_t)s->len);
        return;
    case SY_FN:
        /* A fn pointer is written as its type, which is `aux` */
        sy_buf_put(b, (const char *)aux, strlen((const char *)aux));
        return;
    case SY_ARR:
        sy_arr_put(b, *(sy_arr *const *)p, 1, aux);
        return;
    case SY_STRUCT:
        sy_struct_put(b, *(sy_struct *const *)p);
        return;
    default:
        s = *(sy_str *const *)p;
        if (rt) {
            sy_buf_put(b, "\"", 1);
            for (i = 0; i < (size_t)s->len; i++) {
                if (s->data[i] == '"') sy_buf_put(b, "\"", 1);
                sy_buf_put(b, s->data + i, 1);
            }
            sy_buf_put(b, "\"", 1);
            return;
        }
        q = s->data;
        n = (size_t)s->len;
        break;
    }
    sy_buf_put(b, q, n);
    if (rt && kind == SY_I64) sy_buf_put(b, "'i64", 4);
    if (rt && kind == SY_F32) sy_buf_put(b, "'f32", 4);
}

/* str(array), which is rt_join() of the Fortran runtime.  The elements are
 * separated by ", ".  A rank > 1 array starts and ends with a line break, and
 * has one after every complete row (and plane, etc.), except if its elements
 * are structs */
static void sy_arr_put(sy_buf *b, const sy_arr *a, int rt, const void *aux)
{
    int64_t i, prod;
    int32_t j, kind;
    size_t es;

    sy_buf_put(b, "[", 1);
    if (a != NULL && a->rank > 1) sy_buf_put(b, "\n", 1);

    es = a != NULL ? sy_esize(a->elem) : 0;
    kind = a != NULL && a->elem == SY_I32 && aux != NULL ? SY_ENUM : a != NULL ? a->elem : 0;
    for (i = 0; a != NULL && i < a->len; i++) {
        sy_elem_put(b, kind, a->data + (size_t)i * es, rt, aux);
        if (i >= a->len - 1) continue;
        sy_buf_put(b, ", ", 2);
        if (a->elem == SY_STRUCT) continue;

        prod = a->size[0];
        for (j = 1; j < a->rank; j++) {
            if ((i + 1) % prod == 0) sy_buf_put(b, "\n", 1);
            prod *= a->size[j];
        }
    }

    if (a != NULL && a->rank > 1) sy_buf_put(b, "\n", 1);
    sy_buf_put(b, "]", 1);
}

/* str(array), where the elements are enums, whose variants have these names */
sy_str *sy_arr_str_names(const sy_arr *a, const sy_str *const *names)
{
    sy_buf b = {NULL, 0, 0};
    sy_str *s;

    sy_arr_put(&b, a, 0, names);
    s = sy_str_from(b.p, (int64_t)b.n);
    free(b.p);
    return s;
}

sy_str *sy_arr_str(const sy_arr *a)
{
    return sy_arr_str_names(a, NULL);
}

/* The index of the first variant of an enum that has the backing value v, from
 * the values of all of the `n` variants, for a cast like `Suit(2)` */
int32_t sy_enum_of(const int32_t *vals, int32_t n, int32_t v)
{
    int32_t i;

    for (i = 0; i < n; i++) {
        if (vals[i] == v) return i;
    }
    sy_fatal("enum has no variant with this value");
    return 0;
}

/*============================================================================*/

/*============================================================================*/

/* A struct is a heap object too.  `desc` is a constant of the program, which
 * says what the members are.  Member i is stored in the 8 bytes at offset
 * 8 + 8 * i, as the type that it is (a bool is a byte), and a str, array, or
 * struct member is an owned pointer.  A NULL struct is `null`, which
 * everything here accepts */
typedef struct {
    const char *name;
    int32_t kind;
    int32_t pad;
    const void *aux;    /* for an enum, or an array of them: the names of the variants */
} sy_mdesc;

typedef struct {
    const char *name;
    int64_t n;
    const sy_mdesc *m;
} sy_sdesc;

struct sy_struct {
    const sy_sdesc *desc;
    int64_t slot[];
};

sy_struct *sy_struct_new(const sy_sdesc *d)
{
    sy_struct *s = (sy_struct *)calloc(1, sizeof(sy_struct) + (size_t)d->n * sizeof(int64_t));

    if (s == NULL) sy_fatal("out of memory");
    s->desc = d;
    return s;
}

void sy_struct_free(sy_struct *s)
{
    int64_t i;

    if (s == NULL) return;
    for (i = 0; i < s->desc->n; i++) {
        switch (s->desc->m[i].kind) {
        case SY_STR:
            sy_str_free((sy_str *)s->slot[i]);
            break;
        case SY_ARR:
            sy_arr_free((sy_arr *)s->slot[i]);
            break;
        case SY_STRUCT:
            sy_struct_free((sy_struct *)s->slot[i]);
            break;
        default:
            break;
        }
    }
    free(s);
}

/* A deep copy that can be owned */
sy_struct *sy_struct_dup(const sy_struct *s)
{
    int64_t i;
    sy_struct *r;

    if (s == NULL) return NULL;
    r = sy_struct_new(s->desc);
    for (i = 0; i < s->desc->n; i++) {
        r->slot[i] = s->slot[i];
        if (s->slot[i] == 0) continue;
        switch (s->desc->m[i].kind) {
        case SY_STR:
            r->slot[i] = (int64_t)(intptr_t)sy_str_dup((sy_str *)(intptr_t)s->slot[i]);
            break;
        case SY_ARR:
            r->slot[i] = (int64_t)(intptr_t)sy_arr_dup((const sy_arr *)(intptr_t)s->slot[i]);
            break;
        case SY_STRUCT:
            r->slot[i] = (int64_t)(intptr_t)sy_struct_dup((const sy_struct *)(intptr_t)s->slot[i]);
            break;
        default:
            break;
        }
    }
    return r;
}

/* The members are named, and rendered in the form that reads back as a
 * literal, like `P{x = 1, s = "hi"}` */
static void sy_struct_put(sy_buf *b, const sy_struct *s)
{
    int64_t i;

    if (s == NULL) {
        sy_buf_put(b, "null", 4);
        return;
    }
    sy_buf_put(b, s->desc->name, strlen(s->desc->name));
    sy_buf_put(b, "{", 1);
    for (i = 0; i < s->desc->n; i++) {
        sy_buf_put(b, s->desc->m[i].name, strlen(s->desc->m[i].name));
        sy_buf_put(b, " = ", 3);
        sy_elem_put(b, s->desc->m[i].kind, &s->slot[i], 1, s->desc->m[i].aux);
        if (i < s->desc->n - 1) sy_buf_put(b, ", ", 2);
    }
    sy_buf_put(b, "}", 1);
}

sy_str *sy_struct_str(const sy_struct *s)
{
    sy_buf b = {NULL, 0, 0};
    sy_str *r;

    sy_struct_put(&b, s);
    r = sy_str_from(b.p, (int64_t)b.n);
    free(b.p);
    return r;
}

/*============================================================================*/

/* Files.  A file handle is a value, like in the interpreter: copying one copies
 * its flags, so that reading from a handle, or closing it, does not change the
 * flags of a copy.  The FILE itself is shared, and closing it through one handle
 * closes it for all of them, but not their flags.  The variable that holds a
 * handle owns it, and readln(), eof(), and close() change that one in place */
typedef struct {
    FILE *fp;
    int refs;
} sy_unit;

typedef struct {
    sy_unit *u;
    sy_str *name;
    int8_t rd, wr, app, bin, is_open, eof, is_std;
} sy_file;

/* Did readln() or eof() hit the end of stdin?  It is not on the handle */
static int8_t sy_stdin_eof = 0;

/* The runtime error of the interpreter.  Unlike its message, which goes to
 * stdout with the source line and a stack trace, this goes to stderr */
static void sy_rt_error(const char *code, const char *fmt, ...)
{
    va_list ap;

    fflush(stdout);
    fprintf(stderr, "Runtime error[%s]: ", code);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    exit(1);
}

/* The arguments of the program, which main() hands over.  std::args() is all of
 * them, after the name of the program */
static int sy_argc = 0;
static char **sy_argv = NULL;

void sy_set_args(int32_t argc, char **argv)
{
    sy_argc = argc;
    sy_argv = argv;
}

sy_arr *sy_args(void)
{
    int64_t n = sy_argc > 1 ? sy_argc - 1 : 0, i;
    sy_arr *a = sy_arr_new1(SY_STR, n);

    for (i = 0; i < n; i++) {
        ((sy_str **)a->data)[i] = sy_str_from(sy_argv[i + 1], (int64_t)strlen(sy_argv[i + 1]));
    }
    return a;
}

/* std::getenv() is an error if the variable is not set, but std::hasenv() isn't */
sy_str *sy_getenv(const sy_str *name)
{
    const char *v = getenv(name->data);

    if (v == NULL)
        sy_rt_error("R29", "getenv() environment variable \"%s\" is not set", name->data);
    return sy_str_from(v, (int64_t)strlen(v));
}

int8_t sy_hasenv(const sy_str *name)
{
    return getenv(name->data) != NULL;
}

/* Reductions: sum, product, minval, maxval, count, all, and any of an array along
 * the zero-based dimension `dim`, or of all of it if `dim` is negative, and only of
 * the elements where `mask` is true, if there is one.  The result has the other
 * dimensions, so it has rank 0, which is a single element, for the whole array.
 * The elements of a lane are reduced in order, as Fortran does */
enum { SY_RED_SUM, SY_RED_PRODUCT, SY_RED_MIN, SY_RED_MAX, SY_RED_COUNT, SY_RED_ALL,
       SY_RED_ANY };

static const char *sy_red_name(int32_t op)
{
    switch (op) {
    case SY_RED_SUM: return "sum";
    case SY_RED_PRODUCT: return "product";
    case SY_RED_MIN: return "minval";
    default: return "maxval";
    }
}

#define SY_RED_CASE(ID, T, U) \
    case ID: { \
        const T *src = (const T *)a->data; \
        T *dst = (T *)r->data; \
        for (o = 0; o < outer; o++) { \
            for (i = 0; i < inner; i++) { \
                T acc = (T)(op == SY_RED_PRODUCT); \
                int64_t cnt = 0; \
                for (j = 0; j < n; j++) { \
                    int64_t q = i + inner * (j + n * o); \
                    T x = src[q]; \
                    if (m != NULL && !m[q]) continue; \
                    if (op == SY_RED_SUM) acc = (T)((U)acc + (U)x); \
                    else if (op == SY_RED_PRODUCT) acc = (T)((U)acc * (U)x); \
                    else if (cnt == 0) acc = x; \
                    else if (op == SY_RED_MIN) acc = x < acc ? x : acc; \
                    else acc = x > acc ? x : acc; \
                    cnt++; \
                } \
                if (cnt == 0 && (op == SY_RED_MIN || op == SY_RED_MAX)) \
                    sy_rt_error("R36", "%s() of an empty array or mask selection", \
                                sy_red_name(op)); \
                dst[i + inner * o] = acc; \
            } \
        } \
        break; \
    }

sy_arr *sy_arr_reduce(const sy_arr *a, int64_t dim, int32_t op, const sy_arr *mask)
{
    int64_t inner = 1, n, outer = 1, i, j, o, k;
    int32_t rr = 0, relem;
    int64_t sizes[16];
    const int8_t *m = NULL;
    sy_arr *r;

    if (dim >= a->rank)
        sy_rt_error("R34", "dim %d is out of range for a rank-%d array in reduction call",
                    (int)dim, (int)a->rank);

    if (mask != NULL) {
        int same = mask->rank == a->rank;
        for (k = 0; same && k < a->rank; k++) same = mask->size[k] == a->size[k];
        if (!same)
            sy_rt_error("R35", "mask shape does not match array shape in %s() call",
                        sy_red_name(op));
        m = (const int8_t *)mask->data;
    }

    if (dim < 0) {
        n = a->len;
    } else {
        for (k = 0; k < dim; k++) inner *= a->size[k];
        n = a->size[dim];
        for (k = dim + 1; k < a->rank; k++) outer *= a->size[k];
        for (k = 0; k < a->rank; k++) {
            if (k != dim) sizes[rr++] = a->size[k];
        }
    }

    relem = op == SY_RED_COUNT ? SY_I64 : a->elem;
    r = sy_arr_new(relem, rr, sizes);

    if (op == SY_RED_COUNT || op == SY_RED_ALL || op == SY_RED_ANY) {
        const int8_t *src = (const int8_t *)a->data;
        for (o = 0; o < outer; o++) {
            for (i = 0; i < inner; i++) {
                int64_t cnt = 0;
                for (j = 0; j < n; j++) cnt += src[i + inner * (j + n * o)] != 0;
                if (op == SY_RED_COUNT) ((int64_t *)r->data)[i + inner * o] = cnt;
                else ((int8_t *)r->data)[i + inner * o] =
                         op == SY_RED_ALL ? cnt == n : cnt > 0;
            }
        }
        return r;
    }

    switch (a->elem) {
    SY_RED_CASE(SY_I32, int32_t, uint32_t)
    SY_RED_CASE(SY_I64, int64_t, uint64_t)
    SY_RED_CASE(SY_F32, float, float)
    SY_RED_CASE(SY_F64, double, double)
    default:
        sy_fatal("reduction of an array that is not numeric");
    }
    return r;
}

static sy_file *sy_file_blank(const char *name, int64_t len)
{
    sy_file *f = (sy_file *)calloc(1, sizeof(sy_file));

    if (f == NULL) sy_fatal("out of memory");
    f->u = (sy_unit *)calloc(1, sizeof(sy_unit));
    if (f->u == NULL) sy_fatal("out of memory");
    f->u->refs = 1;
    f->name = sy_str_from(name, len);
    return f;
}

void sy_file_free(sy_file *f)
{
    if (f == NULL) return;
    if (--f->u->refs == 0) free(f->u);
    sy_str_free(f->name);
    free(f);
}

sy_file *sy_file_dup(const sy_file *f)
{
    sy_file *r;

    if (f == NULL) return NULL;
    r = sy_file_blank(f->name->data, f->name->len);
    free(r->u);
    r->u = f->u;
    r->u->refs++;
    r->rd = f->rd;
    r->wr = f->wr;
    r->app = f->app;
    r->bin = f->bin;
    r->is_open = f->is_open;
    r->eof = f->eof;
    r->is_std = f->is_std;
    return r;
}

/* std::IN, std::OUT, or std::ERR, for k = 0, 1, or 2 */
sy_file *sy_file_std(int32_t k)
{
    static const char *const names[] = {"stdin", "stdout", "stderr"};
    sy_file *f = sy_file_blank(names[k], (int64_t)strlen(names[k]));

    f->u->fp = k == 0 ? stdin : k == 1 ? stdout : stderr;
    f->rd = k == 0;
    f->wr = k != 0;
    f->is_open = 1;
    f->is_std = 1;
    return f;
}

/* open(), or std::try_open() if not `must`, which makes a closed handle where
 * open() would fail.  The mode is any of r, w, a (which writes at the end), and
 * b (binary).  Unlike Fortran, "w" truncates a file when it's opened, not when
 * it is first written to */
sy_file *sy_file_open(const sy_str *name, const sy_str *mode, int32_t must)
{
    char fm[4];
    int64_t i;
    sy_file *f = sy_file_blank(name->data, name->len);

    for (i = 0; i < mode->len; i++) {
        switch (mode->data[i]) {
        case 'r':
            f->rd = 1;
            break;
        case 'w':
            f->wr = 1;
            break;
        case 'a':
            f->wr = 1;
            f->app = 1;
            break;
        case 'b':
            f->bin = 1;
            break;
        default:
            sy_rt_error("R6", "bad file mode character \"%c\"", mode->data[i]);
        }
    }
    if (f->rd && f->wr) {
        sy_rt_error("R7", "cannot open file \"%s\" in combined read/write mode \"%s\"",
            name->data, mode->data);
    }

    strcpy(fm, f->rd ? "r" : f->app ? "a" : f->wr ? "w" : "a+");
    if (f->bin) strcat(fm, "b");
    f->u->fp = fopen(name->data, fm);
    if (f->u->fp == NULL) {
        if (must) sy_rt_error("R8", "cannot open file \"%s\" (iostat = %d)", name->data, errno);
        return f;
    }
    f->is_open = 1;
    return f;
}

int8_t sy_exists(const sy_str *path)
{
    struct stat st;

    if (path->len == 0) return 0;
    return stat(path->data, &st) == 0;
}

void sy_file_close(sy_file *f)
{
    if (f->is_std) {
        sy_rt_error("R28", "close() cannot be called on standard file handle \"%s\"",
            f->name->data);
    }
    if (!f->is_open) {
        sy_rt_error("R16", "close() was called for file \"%s\" which is not open",
            f->name->data);
    }
    f->is_open = 0;
    /* Another handle of the file may have closed it already */
    if (f->u->fp != NULL && fclose(f->u->fp) != 0) {
        sy_rt_error("R31", "cannot close() file \"%s\" (iostat = %d)", f->name->data, errno);
    }
    f->u->fp = NULL;
}

/* The members f.is_open, f.eof, and f.name */
int8_t sy_file_is_open(const sy_file *f)
{
    return f->is_open;
}

static int sy_file_at_eof(const sy_file *f)
{
    return f->u->fp == stdin ? sy_stdin_eof : f->eof;
}

int8_t sy_file_eof_member(const sy_file *f)
{
    return (int8_t)sy_file_at_eof(f);
}

sy_str *sy_file_name(const sy_file *f)
{
    return sy_str_from(f->name->data, f->name->len);
}

/*----------------------------------------------------------------------------*/

/* The line that is read next, without its line break (a CR before the LF is
 * dropped too).  If the file ends before a line break, `*hit_eof` is set, and
 * the line is what came before the end, which is "" at the end of a file that
 * does end with a line break.  Like the interpreter's, a read that hits the end
 * is not an error, the next one is */
static sy_str *sy_read_line(FILE *fp, int *hit_eof)
{
    int c;
    size_t n = 0, cap = 128;
    char *buf = (char *)malloc(cap);
    sy_str *s;

    if (buf == NULL) sy_fatal("out of memory");
    while ((c = getc(fp)) != EOF && c != '\n') {
        if (n + 1 > cap) {
            cap *= 2;
            buf = (char *)realloc(buf, cap);
            if (buf == NULL) sy_fatal("out of memory");
        }
        buf[n++] = (char)c;
    }
    *hit_eof = c == EOF;
    if (c == '\n' && n > 0 && buf[n - 1] == '\r') n--;
    s = sy_str_from(buf, (int64_t)n);
    free(buf);
    return s;
}

/* The checks that readln(), eof(), and std::read_<type>() make of the file they
 * read from: that it is open (code `c_open`), that it is open for reading
 * (`c_mode`), and for the first two that it isn't binary */
static void sy_file_rcheck(const sy_file *f, const char *fn, const char *c_open,
    const char *c_mode, const char *binary_hint)
{
    if (!f->is_open || f->u->fp == NULL) {
        sy_rt_error(c_open, "%s() was called for file \"%s\" which is not open", fn,
            f->name->data);
    }
    if (!f->rd) {
        sy_rt_error(c_mode, "%s() was called for file \"%s\" which was not opened in read "
            "mode \"r\"", fn, f->name->data);
    }
    if (binary_hint != NULL && f->bin) {
        sy_rt_error("R43", "%s() was called for file \"%s\" which was opened in binary "
            "mode \"b\"%s", fn, f->name->data, binary_hint);
    }
}

sy_str *sy_file_readln(sy_file *f)
{
    int hit;
    sy_str *s;

    sy_file_rcheck(f, "readln", "R9", "R10",
        ".  Use std::read_i32() etc. instead");
    if (sy_file_at_eof(f)) {
        sy_rt_error("R11", "cannot readln() from file \"%s\" past end of file",
            f->name->data);
    }
    s = sy_read_line(f->u->fp, &hit);
    if (hit) {
        if (f->u->fp == stdin) sy_stdin_eof = 1;
        else f->eof = 1;
    }
    return s;
}

/* readln() of stdin */
sy_str *sy_stdin_readln(void)
{
    int hit;
    sy_str *s;

    if (sy_stdin_eof) sy_rt_error("R11", "cannot readln() from stdin past end of input");
    s = sy_read_line(stdin, &hit);
    if (hit) sy_stdin_eof = 1;
    return s;
}

int8_t sy_file_eof(const sy_file *f)
{
    sy_file_rcheck(f, "eof", "R14", "R15", "");
    return (int8_t)sy_file_at_eof(f);
}

int8_t sy_stdin_at_eof(void)
{
    return sy_stdin_eof;
}

/*----------------------------------------------------------------------------*/

/* Writing.  Each value of a call is written on its own, in the order that they
 * are given.  In a text file, that is what str() of it is, and in a binary file
 * it is the bytes that the value is stored as.  A value that has no bytes, like
 * a struct, can't be written to a binary file */
void sy_file_wcheck(const sy_file *f, int32_t is_writeln)
{
    const char *fn = is_writeln ? "writeln" : "write";

    if (!f->is_open || f->u->fp == NULL) {
        sy_rt_error("R12", "%s() was called for file \"%s\" which is not open", fn,
            f->name->data);
    }
    if (!f->wr) {
        sy_rt_error("R13", "%s() was called for file \"%s\" which was not opened in write "
            "mode \"w\"", fn, f->name->data);
    }
    if (is_writeln && f->bin) {
        sy_rt_error("R43", "writeln() was called for file \"%s\" which was opened in binary "
            "mode \"b\".  Use write() instead", f->name->data);
    }
}

static void sy_file_put(sy_file *f, const void *p, size_t n)
{
    if (n > 0 && fwrite(p, 1, n, f->u->fp) != n) {
        sy_rt_error("R30", "cannot write() to file \"%s\" (iostat = %d)", f->name->data, errno);
    }
}

static void sy_file_put_str(sy_file *f, sy_str *s)
{
    sy_file_put(f, s->data, (size_t)s->len);
    sy_str_free(s);
}

static void sy_file_bad_type(const sy_file *f, const char *tname)
{
    sy_rt_error("R45", "write() can not write a value of type `%s` to file \"%s\" which "
        "was opened in binary mode \"b\"", tname, f->name->data);
}

void sy_file_wnl(sy_file *f)
{
    sy_file_put(f, "\n", 1);
}

void sy_file_wstr(sy_file *f, const sy_str *s)
{
    sy_file_put(f, s->data, (size_t)s->len);
}

void sy_file_wi32(sy_file *f, int32_t x)
{
    if (f->bin) sy_file_put(f, &x, sizeof x);
    else sy_file_put_str(f, sy_str_i32(x));
}

void sy_file_wi64(sy_file *f, int64_t x)
{
    if (f->bin) sy_file_put(f, &x, sizeof x);
    else sy_file_put_str(f, sy_str_i64(x));
}

void sy_file_wf32(sy_file *f, float x)
{
    if (f->bin) sy_file_put(f, &x, sizeof x);
    else sy_file_put_str(f, sy_str_f32(x));
}

void sy_file_wf64(sy_file *f, double x)
{
    if (f->bin) sy_file_put(f, &x, sizeof x);
    else sy_file_put_str(f, sy_str_f64(x));
}

void sy_file_wbool(sy_file *f, int8_t x)
{
    if (f->bin) sy_file_put(f, &x, sizeof x);
    else sy_file_put_str(f, sy_str_bool(x));
}

/* An array, whose elements are enums if `names` is not NULL */
void sy_file_warr(sy_file *f, const sy_arr *a, const sy_str *const *names, const char *tname)
{
    if (!f->bin) {
        sy_file_put_str(f, names != NULL ? sy_arr_str_names(a, names) : sy_arr_str(a));
        return;
    }
    if (names != NULL || a->elem == SY_STR || a->elem == SY_ARR || a->elem == SY_STRUCT) {
        sy_file_bad_type(f, tname);
    }
    sy_file_put(f, a->data, (size_t)a->len * sy_esize(a->elem));
}

void sy_file_wstruct(sy_file *f, const sy_struct *s, const char *tname)
{
    if (f->bin) sy_file_bad_type(f, tname);
    sy_file_put_str(f, sy_struct_str(s));
}

void sy_file_wenum(sy_file *f, int32_t v, const sy_str *const *names, const char *tname)
{
    if (f->bin) sy_file_bad_type(f, tname);
    sy_file_put(f, names[v]->data, (size_t)names[v]->len);
}

/*----------------------------------------------------------------------------*/

/* std::read_<type>().  In a text file, values are separated by blanks, tabs, or
 * commas and can be on different lines.  The rest of the line of the last value
 * is skipped.  Unlike a list-directed read, `/`, `*`, and quotes are not special
 * and an empty field like the one in `1,,3` is not a null value: they are all
 * errors, so that a bad value is never skipped or repeated.  A binary file is
 * read as the bytes the values are stored as.  The values go to `out`, which has
 * room for `n` of the type `elem` */
static void sy_read_fail(const sy_file *f, const char *fn, const char *what)
{
    sy_rt_error("R44", "cannot %s() from file \"%s\" %s", fn, f->name->data, what);
}

static void sy_read_bad(const sy_file *f, const char *fn, const char *tok)
{
    char what[256];

    if (tok[0] == '\0') snprintf(what, sizeof what, "(empty value)");
    else snprintf(what, sizeof what, "(invalid value \"%.200s\")", tok);
    sy_read_fail(f, fn, what);
}

static int sy_is_sep(int c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == ',';
}

/* Fortran's list-directed read takes a `d` for an exponent too */
static int sy_tok_value(int32_t elem, const char *tok, void *out)
{
    char *end, tmp[128];
    size_t i, n = strlen(tok);
    long long iv;
    double dv;

    if (n == 0 || n >= sizeof tmp) return 0;
    errno = 0;
    if (elem == SY_I32 || elem == SY_I64) {
        iv = strtoll(tok, &end, 10);
        if (*end != '\0' || errno != 0) return 0;
        if (elem == SY_I32) {
            if (iv < -2147483648LL || iv > 2147483647LL) return 0;
            *(int32_t *)out = (int32_t)iv;
        } else {
            *(int64_t *)out = (int64_t)iv;
        }
        return 1;
    }
    for (i = 0; i <= n; i++) tmp[i] = (tok[i] == 'd' || tok[i] == 'D') ? 'e' : tok[i];
    dv = strtod(tmp, &end);
    if (*end != '\0' || end == tmp) return 0;
    if (elem == SY_F32) *(float *)out = (float)dv;
    else *(double *)out = dv;
    return 1;
}

static void sy_read_values(sy_file *f, int32_t elem, int64_t n, void *out)
{
    static const char *const fns[] = {"read_i32", "read_i64", "read_f32", "read_f64"};
    const char *fn = fns[elem];
    size_t es = sy_esize(elem);
    int64_t k = 0, i, j;
    int hit, after_tok = 0;
    char tok[256];
    sy_str *line;

    sy_file_rcheck(f, fn, "R9", "R10", NULL);
    if (sy_file_at_eof(f)) sy_read_fail(f, fn, "past the end of the file");

    if (f->bin) {
        if (n > 0 && fread(out, es, (size_t)n, f->u->fp) != (size_t)n) {
            sy_read_fail(f, fn, "past the end of the file");
        }
        return;
    }

    while (k < n) {
        line = sy_read_line(f->u->fp, &hit);
        if (hit && line->len == 0) sy_read_fail(f, fn, "past the end of the file");
        i = 0;
        while (i < line->len && k < n) {
            char c = line->data[i];

            if (c == ' ' || c == '\t' || c == '\r') {
                i++;
            } else if (c == ',') {
                if (!after_tok) sy_read_bad(f, fn, "");
                after_tok = 0;
                i++;
            } else {
                for (j = i; j < line->len && !sy_is_sep(line->data[j]); j++) {}
                if (j - i >= (int64_t)sizeof tok) sy_read_bad(f, fn, "");
                memcpy(tok, line->data + i, (size_t)(j - i));
                tok[j - i] = '\0';
                if (strpbrk(tok, "/*'\"") != NULL ||
                    !sy_tok_value(elem, tok, (char *)out + (size_t)k * es)) {
                    sy_read_bad(f, fn, tok);
                }
                k++;
                after_tok = 1;
                i = j;
            }
        }
        sy_str_free(line);
    }
}

int32_t sy_file_read_i32(sy_file *f)
{
    int32_t x = 0;

    sy_read_values(f, SY_I32, 1, &x);
    return x;
}

int64_t sy_file_read_i64(sy_file *f)
{
    int64_t x = 0;

    sy_read_values(f, SY_I64, 1, &x);
    return x;
}

float sy_file_read_f32(sy_file *f)
{
    float x = 0;

    sy_read_values(f, SY_F32, 1, &x);
    return x;
}

double sy_file_read_f64(sy_file *f)
{
    double x = 0;

    sy_read_values(f, SY_F64, 1, &x);
    return x;
}

/* A rank 1 array of n values.  The type of the elements is SY_I32, etc. */
sy_arr *sy_file_read_arr(sy_file *f, int32_t elem, int64_t n)
{
    sy_arr *a;

    if (n < 0) {
        sy_rt_error("R44", "cannot read_%s() a negative number of values (n = %lld)",
            elem == SY_I32 ? "i32" : elem == SY_I64 ? "i64" : elem == SY_F32 ? "f32" : "f64",
            (long long)n);
    }
    a = sy_arr_new(elem, 1, &n);
    sy_read_values(f, elem, n, a->data);
    return a;
}
