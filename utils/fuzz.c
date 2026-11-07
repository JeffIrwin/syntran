/*
 * fuzz.c -- a black-box, corpus-mutation fuzzer for the syntran interpreter.
 *
 * The contract being tested: any input either runs, or stops with a coded
 * E##/R## diagnostic.  A run is a "crash" if it dies from a signal, times out,
 * prints an internal error (I##) or a Fortran runtime error, or trips a
 * sanitizer.
 *
 * It mutates the .syntran files under src/tests/test-src, samples and
 * src/tests/long, with token-aware mutations so that most mutants get past the
 * lexer, then runs `syntran -q case.syntran` in a forked child.  One reproducer
 * per distinct crash signature is saved to <out>/crashes/<signature>.syntran.
 *
 * This is not part of the fpm build.  Build and run it by hand:
 *
 *     cc -O2 -Wall -o build/fuzz/fuzz utils/fuzz.c
 *     build/fuzz/fuzz --bin build/fuzz/bin/syntran --time 300 --jobs 4
 *     build/fuzz/fuzz --bin build/fuzz/bin/syntran --reduce build/fuzz/crashes/X.syntran
 *
 * See CLAUDE.md, "Fuzzing".  POSIX only (Linux and macOS).
 */

#define _GNU_SOURCE
#define _DARWIN_C_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

#define MAXFILE   (256 * 1024)
#define MAXTOK    300000
#define MAXCAP    (64 * 1024)    /* captured output per run */
#define SIGLEN    100

/*============================================================================*/
/* Small utilities */

typedef struct { char *p; size_t n, cap; } buf_t;

static void die(const char *msg)
{
    fprintf(stderr, "fuzz: %s\n", msg);
    exit(1);
}

static void buf_reserve(buf_t *b, size_t need)
{
    if (need + 1 <= b->cap) return;
    size_t cap = b->cap ? b->cap : 1024;
    while (cap < need + 1) cap *= 2;
    b->p = realloc(b->p, cap);
    if (!b->p) die("out of memory");
    b->cap = cap;
}

static void buf_set(buf_t *b, const char *s, size_t n)
{
    buf_reserve(b, n);
    memcpy(b->p, s, n);
    b->n = n;
    b->p[n] = 0;
}

/* Replace `del` bytes at `at` with `nins` bytes from `ins` */
static void buf_splice(buf_t *b, size_t at, size_t del, const char *ins, size_t nins)
{
    if (at > b->n) at = b->n;
    if (at + del > b->n) del = b->n - at;
    if (b->n - del + nins > MAXFILE) return;
    buf_reserve(b, b->n - del + nins);
    memmove(b->p + at + nins, b->p + at + del, b->n - at - del);
    if (nins) memcpy(b->p + at, ins, nins);
    b->n = b->n - del + nins;
    b->p[b->n] = 0;
}

static double now(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + 1e-9 * ts.tv_nsec;
}

static uint64_t rs = 1;

static uint64_t rnd64(void)
{
    rs ^= rs >> 12;
    rs ^= rs << 25;
    rs ^= rs >> 27;
    return rs * 0x2545F4914F6CDD1DULL;
}

static size_t rnd(size_t n)
{
    return n ? (size_t)(rnd64() >> 11) % n : 0;
}

static void mkdir_p(const char *path)
{
    char tmp[PATH_MAX];
    snprintf(tmp, sizeof tmp, "%s", path);
    for (char *q = tmp + 1; *q; q++) {
        if (*q != '/') continue;
        *q = 0;
        mkdir(tmp, 0777);
        *q = '/';
    }
    mkdir(tmp, 0777);
}

static int read_file(const char *path, buf_t *b)
{
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    b->n = 0;
    buf_reserve(b, MAXFILE);
    b->n = fread(b->p, 1, MAXFILE, f);
    b->p[b->n] = 0;
    fclose(f);
    return 0;
}

static int write_file(const char *path, const char *p, size_t n, int excl)
{
    int fd = open(path, O_WRONLY | O_CREAT | (excl ? O_EXCL : O_TRUNC), 0666);
    if (fd < 0) return -1;
    size_t off = 0;
    while (off < n) {
        ssize_t w = write(fd, p + off, n - off);
        if (w <= 0) break;
        off += (size_t)w;
    }
    close(fd);
    return 0;
}

/*============================================================================*/
/* Running one case and classifying the outcome */

enum { RES_OK, RES_DIAG, RES_CRASH };

/* Turn a message into a short file-name-safe token */
static void sanitize_keep(char *dst, size_t cap, const char *s, size_t n, int keep_digits)
{
    size_t k = strlen(dst);
    int last_us = (k > 0 && dst[k - 1] == '_');
    for (size_t i = 0; i < n && s[i] && k + 1 < cap; i++) {
        char c = s[i];
        if (!keep_digits && isdigit((unsigned char)c)) c = 'N';
        if (!isalnum((unsigned char)c) && c != '-') c = '_';
        if (c == '_' && last_us) continue;
        last_us = (c == '_');
        dst[k++] = c;
    }
    dst[k] = 0;
}

static void sanitize_into(char *dst, size_t cap, const char *s, size_t n)
{
    sanitize_keep(dst, cap, s, n, 0);
}

/* The innermost user-code frame of a gfortran or sanitizer backtrace, as a
 * function name (no line numbers, so that the signature is stable) */
static void first_frame(const char *out, char *sig, size_t cap)
{
    static const char *skip[] = {"gfortran", "sigtramp", "restore_rt", "asan", "interceptor",
        "libsystem", "backtrace", "sanitizer", "ubsan", "pthread", "_sigaction", NULL};
    for (const char *line = out; line && *line; ) {
        const char *eol = strchr(line, '\n');
        size_t len = eol ? (size_t)(eol - line) : strlen(line);
        const char *q = line;
        while (q < line + len && (*q == ' ' || *q == '\t')) q++;
        if (q < line + len && *q == '#') {
            const char *in = NULL;
            for (const char *r = q; r + 4 <= line + len; r++)
                if (!strncmp(r, " in ", 4)) { in = r + 4; break; }
            if (in) {
                size_t m = 0;
                while (in + m < line + len && in[m] != ' ' && in[m] != '\r') m++;
                int bad = 0;
                for (int s = 0; skip[s]; s++) {
                    for (size_t i = 0; i + strlen(skip[s]) <= m; i++)
                        if (!strncmp(in + i, skip[s], strlen(skip[s]))) bad = 1;
                }
                if (!bad && m > 0) {
                    strncat(sig, "-", cap - strlen(sig) - 1);
                    sanitize_into(sig, cap, in, m);
                    return;
                }
            }
        }
        line = eol ? eol + 1 : NULL;
    }
    /* No symbol names (macOS), so use the low 12 bits of the addresses of the faulting
     * frame and its caller.  ASLR slides the image by whole pages, so those bits are
     * the same on every run.  Frames 0-2 are the signal handler's own */
    int found = 0;
    for (const char *line = out; line && *line && found < 2; ) {
        const char *eol = strchr(line, '\n');
        int idx;
        unsigned long long addr;
        if (sscanf(line, "#%d 0x%llx", &idx, &addr) == 2 && (idx == 3 || idx == 4)) {
            char hex[16];
            snprintf(hex, sizeof hex, "-%03llx", addr & 0xfff);
            strncat(sig, hex, cap - strlen(sig) - 1);
            found++;
        }
        line = eol ? eol + 1 : NULL;
    }
    if (!found) strncat(sig, "-noframe", cap - strlen(sig) - 1);
}

/* gfortran's "At line 617 of file ././src/parse_expr.f90" is a better key than a frame */
static int at_line(const char *out, char *sig, size_t cap)
{
    const char *m = strstr(out, "At line ");
    if (!m) return 0;
    int ln;
    char file[256];
    if (sscanf(m, "At line %d of file %255s", &ln, file) != 2) return 0;
    const char *base = strrchr(file, '/');
    base = base ? base + 1 : file;
    char tail[300];
    snprintf(tail, sizeof tail, "-%s-L%d", base, ln);
    sanitize_keep(sig, cap, tail, strlen(tail), 1);
    return 1;
}

/* strstr(), but not in a source line that a diagnostic echoes, like ` 2 | <source>`.  A
 * mutant can contain the text of an error message, e.g. in a comment */
static const char *find_msg(const char *out, const char *needle)
{
    const char *m = out;
    while ((m = strstr(m, needle))) {
        const char *s = m;
        while (s > out && s[-1] != '\n') s--;
        while (*s == ' ' || (*s >= '0' && *s <= '9')) s++;
        if (*s != '|' || s > m) return m;
        m += strlen(needle);
    }
    return NULL;
}

static int classify(const char *out, int timed_out, int status, char *sig, size_t cap)
{
    sig[0] = 0;
    const char *m;

    if (timed_out) {
        snprintf(sig, cap, "TIMEOUT");
        return RES_CRASH;
    }
    if ((m = strstr(out, "AddressSanitizer"))) {
        snprintf(sig, cap, "ASAN-");
        const char *k = strstr(m, ": ");
        if (k) {
            k += 2;
            size_t n = strcspn(k, " \n");
            sanitize_into(sig, cap, k, n);
        }
        first_frame(out, sig, cap);
        return RES_CRASH;
    }
    if ((m = strstr(out, ": runtime error: "))) {
        /* UBSan; syntran's own are capitalized, with a code in brackets */
        snprintf(sig, cap, "UBSAN-");
        const char *k = m + strlen(": runtime error: ");
        sanitize_into(sig, cap, k, strcspn(k, "\n") < 40 ? strcspn(k, "\n") : 40);
        return RES_CRASH;
    }
    if ((m = find_msg(out, "Internal syntran error["))) {
        snprintf(sig, cap, "INTERNAL-");
        m += strlen("Internal syntran error[");
        sanitize_into(sig, cap, m, strcspn(m, "]"));
        return RES_CRASH;
    }
    if ((m = find_msg(out, "Fortran runtime error"))) {
        snprintf(sig, cap, "FORTRAN-");
        const char *k = strchr(m, ':');
        k = k ? k + 1 : m;
        size_t n = strcspn(k, "\n");
        sanitize_into(sig, cap, k, n < 50 ? n : 50);
        if (!at_line(out, sig, cap)) first_frame(out, sig, cap);
        return RES_CRASH;
    }
    if ((m = find_msg(out, "Fatal error"))) {
        /* syntran's internal_error(), which is preceded by a message line */
        snprintf(sig, cap, "FATAL-");
        const char *e = m;
        while (e > out && (e[-1] == '\n' || e[-1] == ' ')) e--;
        const char *s = e;
        while (s > out && s[-1] != '\n') s--;
        size_t n = (size_t)(e - s);
        sanitize_into(sig, cap, s, n < 50 ? n : 50);
        return RES_CRASH;
    }
    if (WIFSIGNALED(status)) {
        snprintf(sig, cap, "SIG%d", WTERMSIG(status));
        first_frame(out, sig, cap);
        return RES_CRASH;
    }
    if (strstr(out, "Error[") || strstr(out, "error[")) return RES_DIAG;
    return RES_OK;
}

/* Runs `bin -q case.syntran` in `workdir` and classifies the result.  `out` gets
 * the (truncated) combined stdout and stderr, and must be MAXCAP + 1 long */
static int run_case(const char *bin, const char *workdir, int timeout_s, char *sig,
    size_t sigcap, char *out)
{
    int pfd[2];
    if (pipe(pfd) < 0) die("pipe() failed");

    pid_t pid = fork();
    if (pid < 0) die("fork() failed");
    if (pid == 0) {
        if (chdir(workdir) < 0) _exit(126);
        int dn = open("/dev/null", O_RDONLY);
        dup2(dn, 0);
        dup2(pfd[1], 1);
        dup2(pfd[1], 2);
        close(pfd[0]);
        close(pfd[1]);
        struct rlimit rl = {(rlim_t)timeout_s + 2, (rlim_t)timeout_s + 2};
        setrlimit(RLIMIT_CPU, &rl);
        rl.rlim_cur = rl.rlim_max = 0;
        setrlimit(RLIMIT_CORE, &rl);
        setenv("ASAN_OPTIONS", "detect_leaks=0", 1);
        setenv("UBSAN_OPTIONS", "print_stacktrace=1", 1);
        char *argv[] = {(char *)bin, "-q", "case.syntran", NULL};
        execv(bin, argv);
        _exit(127);
    }
    close(pfd[1]);
    fcntl(pfd[0], F_SETFL, fcntl(pfd[0], F_GETFL) | O_NONBLOCK);

    double deadline = now() + timeout_s;
    size_t n = 0;
    int timed_out = 0, status = 0, reaped = 0, eof = 0;
    char scratch[4096];
    while (!reaped) {
        if (!eof) {
            struct pollfd p = {pfd[0], POLLIN, 0};
            poll(&p, 1, 20);
            for (;;) {
                size_t room = MAXCAP - n;
                ssize_t r = read(pfd[0], room ? out + n : scratch,
                    room ? room : sizeof scratch);
                if (r > 0) { if (room) n += (size_t)r; continue; }
                if (r == 0) eof = 1;
                break;
            }
        } else {
            usleep(2000);
        }
        if (waitpid(pid, &status, WNOHANG) == pid) reaped = 1;
        else if (now() > deadline) {
            timed_out = 1;
            kill(pid, SIGKILL);
            waitpid(pid, &status, 0);
            reaped = 1;
        }
    }
    /* Whatever was left in the pipe when the child exited */
    for (;;) {
        size_t room = MAXCAP - n;
        if (!room) break;
        ssize_t r = read(pfd[0], out + n, room);
        if (r <= 0) break;
        n += (size_t)r;
    }
    close(pfd[0]);
    out[n] = 0;
    return classify(out, timed_out, status, sig, sigcap);
}

/*============================================================================*/
/* Lexing and mutation */

enum { T_WS, T_COMMENT, T_IDENT, T_NUM, T_STR, T_OP, T_OPEN, T_CLOSE };
typedef struct { size_t s, n; int cls; } tok_t;

static tok_t toks[MAXTOK];
static size_t nonws[MAXTOK];

/* Only token boundaries and a coarse class matter, so this is not syntran's lexer */
static size_t lex(const char *p, size_t n, tok_t *t, size_t max)
{
    size_t i = 0, k = 0;
    while (i < n && k < max) {
        size_t s = i;
        char c = p[i];
        int cls;
        if (isspace((unsigned char)c)) {
            while (i < n && isspace((unsigned char)p[i])) i++;
            cls = T_WS;
        } else if (c == '/' && i + 1 < n && p[i + 1] == '/') {
            while (i < n && p[i] != '\n') i++;
            cls = T_COMMENT;
        } else if (c == '/' && i + 1 < n && p[i + 1] == '*') {
            i += 2;
            while (i + 1 < n && !(p[i] == '*' && p[i + 1] == '/')) i++;
            i = i + 2 < n ? i + 2 : n;
            cls = T_COMMENT;
        } else if (isalpha((unsigned char)c) || c == '_') {
            while (i < n && (isalnum((unsigned char)p[i]) || p[i] == '_')) i++;
            cls = T_IDENT;
        } else if (isdigit((unsigned char)c)) {
            while (i < n && (isalnum((unsigned char)p[i]) || p[i] == '_' ||
                (p[i] == '.' && i + 1 < n && isdigit((unsigned char)p[i + 1])))) i++;
            cls = T_NUM;
        } else if (c == '"') {
            i++;
            while (i < n && p[i] != '"' && p[i] != '\n') {
                if (p[i] == '\\' && i + 1 < n) i++;
                i++;
            }
            if (i < n && p[i] == '"') i++;
            cls = T_STR;
        } else if (c == '(' || c == '[' || c == '{') {
            i++;
            cls = T_OPEN;
        } else if (c == ')' || c == ']' || c == '}') {
            i++;
            cls = T_CLOSE;
        } else {
            i++;
            cls = T_OP;
        }
        t[k].s = s;
        t[k].n = i - s;
        t[k].cls = cls;
        k++;
    }
    return k;
}

static const char *KEYWORDS[] = {"fn", "let", "if", "else", "while", "for", "in", "return",
    "struct", "enum", "use", "break", "continue", "true", "false", "mut", NULL};
static const char *TYPES[] = {"i32", "i64", "f32", "f64", "bool", "str", "file", NULL};
static const char *EDGE_NUMS[] = {"0", "1", "-1", "2", "2147483647", "-2147483648",
    "2147483648", "9223372036854775807", "-9223372036854775808", "99999999999999999999",
    "4294967296", "0.0", "1e308", "1e-308", "1e400", "1.5", "0x7fffffff", "1_000", NULL};
static const char *STRS[] = {"\"\"", "\"a\"", "\"%d\"", "\"{}\"", "\"\\n\"", "\"\\\"", NULL};
static const char *OPS[] = {"+", "-", "*", "/", "%", "**", "@", "=", "==", "!=", "<", ">",
    "<=", ">=", "&&", "||", "!", "~", "&", "|", "^", ":", ";", ",", ".", "::", "..", "+=",
    "-=", "*=", "/=", "<<", ">>", "?", "#", "$", "->", "=>", NULL};
static const char *BRACKETS[] = {"(", ")", "[", "]", "{", "}", NULL};

static const char *pick(const char **pool)
{
    size_t n = 0;
    while (pool[n]) n++;
    return pool[rnd(n)];
}

typedef struct { char *path; buf_t src; } seed_t;
static seed_t *corpus;
static size_t ncorpus;

/* A random word or punctuation char from a random seed file */
static size_t foreign_token(char *dst, size_t cap)
{
    for (int tries = 0; tries < 8; tries++) {
        buf_t *s = &corpus[rnd(ncorpus)].src;
        if (s->n == 0) continue;
        size_t a = rnd(s->n), b;
        unsigned char c = (unsigned char)s->p[a];
        if (isspace(c)) continue;
        b = a + 1;
        if (isalnum(c) || c == '_') {
            while (a > 0 && (isalnum((unsigned char)s->p[a - 1]) || s->p[a - 1] == '_')) a--;
            while (b < s->n && (isalnum((unsigned char)s->p[b]) || s->p[b] == '_')) b++;
        }
        if (b - a >= cap) continue;
        memcpy(dst, s->p + a, b - a);
        dst[b - a] = 0;
        return b - a;
    }
    dst[0] = ';';
    dst[1] = 0;
    return 1;
}

static void put(buf_t *b, size_t at, size_t del, const char *s)
{
    buf_splice(b, at, del, s, strlen(s));
}

static void line_bounds(const buf_t *b, size_t off, size_t *a, size_t *e)
{
    size_t s = off < b->n ? off : b->n;
    while (s > 0 && b->p[s - 1] != '\n') s--;
    size_t t = s;
    while (t < b->n && b->p[t] != '\n') t++;
    if (t < b->n) t++;
    *a = s;
    *e = t;
}

static void mutate_once(buf_t *b)
{
    size_t nt = lex(b->p, b->n, toks, MAXTOK);
    size_t nw = 0;
    for (size_t i = 0; i < nt; i++)
        if (toks[i].cls != T_WS && toks[i].cls != T_COMMENT) nonws[nw++] = i;

    size_t roll = rnd(100);
    char tmp[256];

    if (nw == 0) {
        tmp[0] = (char)(32 + rnd(95));
        buf_splice(b, rnd(b->n + 1), 0, tmp, 1);
        return;
    }
    size_t wi = rnd(nw);
    tok_t t = toks[nonws[wi]];

    if (roll < 10) {                    /* delete a token */
        buf_splice(b, t.s, t.n, "", 0);
    } else if (roll < 20) {             /* duplicate a token */
        memcpy(tmp, b->p + t.s, t.n < 200 ? t.n : 200);
        tmp[t.n < 200 ? t.n : 200] = 0;
        char ins[300];
        snprintf(ins, sizeof ins, " %s", tmp);
        buf_splice(b, t.s + t.n, 0, ins, strlen(ins));
    } else if (roll < 30) {             /* swap with a nearby token */
        size_t wj = wi + 1 + rnd(3);
        if (wj >= nw) return;
        tok_t u = toks[nonws[wj]];
        if (t.n > 200 || u.n > 200) return;
        char a[256], c[256];
        memcpy(a, b->p + t.s, t.n);
        a[t.n] = 0;
        memcpy(c, b->p + u.s, u.n);
        c[u.n] = 0;
        put(b, u.s, u.n, a);
        put(b, t.s, t.n, c);
    } else if (roll < 50) {             /* same-class replacement */
        const char *r;
        switch (t.cls) {
        case T_IDENT:
            if (rnd(3) == 0) { foreign_token(tmp, sizeof tmp); r = tmp; }
            else r = rnd(2) ? pick(KEYWORDS) : pick(TYPES);
            break;
        case T_NUM: r = pick(EDGE_NUMS); break;
        case T_STR: r = pick(STRS); break;
        case T_OPEN: case T_CLOSE: r = pick(BRACKETS); break;
        default: r = pick(OPS); break;
        }
        put(b, t.s, t.n, r);
    } else if (roll < 60) {             /* insert a bracket, operator, or separator */
        const char *r = rnd(2) ? pick(BRACKETS) : pick(OPS);
        char ins[16];
        snprintf(ins, sizeof ins, " %s ", r);
        put(b, t.s, 0, ins);
    } else if (roll < 68) {             /* delete a line */
        size_t a, e;
        line_bounds(b, t.s, &a, &e);
        buf_splice(b, a, e - a, "", 0);
    } else if (roll < 73) {             /* duplicate a line */
        size_t a, e;
        line_bounds(b, t.s, &a, &e);
        if (e - a > 2000) return;
        char *cp = malloc(e - a + 1);
        memcpy(cp, b->p + a, e - a);
        buf_splice(b, a, 0, cp, e - a);
        free(cp);
    } else if (roll < 80) {             /* splice in a line from another seed */
        buf_t *s = &corpus[rnd(ncorpus)].src;
        if (s->n == 0) return;
        size_t a, e;
        line_bounds(s, rnd(s->n), &a, &e);
        if (e - a > 2000) return;
        size_t at, ae;
        line_bounds(b, t.s, &at, &ae);
        buf_splice(b, at, 0, s->p + a, e - a);
    } else if (roll < 88) {             /* replace with a token from another seed */
        size_t n = foreign_token(tmp, sizeof tmp);
        buf_splice(b, t.s, t.n, tmp, n);
    } else if (roll < 92) {             /* truncate */
        buf_splice(b, t.s, b->n - t.s, "", 0);
    } else if (roll < 96) {             /* wrap a token range in parens */
        size_t wj = wi + rnd(8);
        if (wj >= nw) wj = nw - 1;
        tok_t u = toks[nonws[wj]];
        put(b, u.s + u.n, 0, ")");
        put(b, t.s, 0, "(");
    } else {                            /* byte noise */
        size_t at = rnd(b->n);
        if (rnd(2)) b->p[at] = (char)rnd64();
        else {
            char c = (char)(rnd(2) ? rnd64() : 32 + rnd(95));
            buf_splice(b, at, 0, &c, 1);
        }
    }
}

/*============================================================================*/
/* Corpus */

static const char *only_dir;

static int visit(const char *path, const struct stat *sb, int flag, struct FTW *ftw)
{
    (void)ftw;
    size_t n = strlen(path);
    if (flag != FTW_F || n < 8 || strcmp(path + n - 8, ".syntran") != 0) return 0;
    if (only_dir && !strstr(path, only_dir)) return 0;
    if (sb->st_size == 0 || sb->st_size > MAXFILE) return 0;
    corpus = realloc(corpus, (ncorpus + 1) * sizeof *corpus);
    if (!corpus) die("out of memory");
    seed_t *s = &corpus[ncorpus++];
    memset(s, 0, sizeof *s);
    s->path = strdup(path);
    read_file(path, &s->src);
    return 0;
}

/*============================================================================*/
/* Fuzzing workers */

typedef struct { uint64_t runs, ok, diag, crashes, unique; } stats_t;

static void save_crash(const char *outdir, const char *sig, const buf_t *b, const char *seed,
    const char *output, stats_t *st)
{
    char path[PATH_MAX];
    snprintf(path, sizeof path, "%s/crashes/%s.syntran", outdir, sig);
    if (write_file(path, b->p, b->n, 1) < 0) return;    /* signature already seen */
    st->unique++;

    char info[MAXCAP + 512];
    int k = snprintf(info, sizeof info, "signature: %s\nseed: %s\n\n%s", sig, seed, output);
    snprintf(path, sizeof path, "%s/crashes/%s.txt", outdir, sig);
    write_file(path, info, (size_t)k, 0);
    fprintf(stderr, "NEW crash %s (from %s)\n", sig, seed);
}

static void worker(int id, uint64_t seed, const char *bin, const char *outdir, long iters,
    double deadline, int timeout_s, stats_t *st)
{
    char workdir[PATH_MAX], file[PATH_MAX], sig[SIGLEN];
    char *output = malloc(MAXCAP + 1);
    snprintf(workdir, sizeof workdir, "%s/work/%d", outdir, id);
    mkdir_p(workdir);
    snprintf(file, sizeof file, "%s/case.syntran", workdir);
    rs = (seed + (uint64_t)id) * 0x9E3779B97F4A7C15ULL | 1;
    for (int i = 0; i < 10; i++) rnd64();

    buf_t b = {0};
    for (long it = 0; (iters <= 0 || it < iters) && (deadline <= 0 || now() < deadline); it++) {
        seed_t *s = &corpus[rnd(ncorpus)];
        buf_set(&b, s->src.p, s->src.n);
        size_t nmut = 1 + rnd(4);
        for (size_t m = 0; m < nmut; m++) mutate_once(&b);

        write_file(file, b.p, b.n, 0);
        int r = run_case(bin, workdir, timeout_s, sig, sizeof sig, output);
        st->runs++;
        if (r == RES_OK) st->ok++;
        else if (r == RES_DIAG) st->diag++;
        else {
            st->crashes++;
            save_crash(outdir, sig, &b, s->path, output, st);
        }
    }
    free(output);
    free(b.p);
}

/*============================================================================*/
/* Test-case reduction */

static char red_bin[PATH_MAX], red_dir[PATH_MAX], red_sig0[SIGLEN];
static char *red_out;

static int still_crashes(const buf_t *b)
{
    char path[PATH_MAX], sig[SIGLEN];
    snprintf(path, sizeof path, "%s/case.syntran", red_dir);
    write_file(path, b->p, b->n, 0);
    int r = run_case(red_bin, red_dir, 5, sig, sizeof sig, red_out);
    return r == RES_CRASH && strcmp(sig, red_sig0) == 0;
}

/* Greedy delta debugging: try deleting chunks of units, from large to small */
static int reduce_pass(buf_t *b, int by_token)
{
    int progress = 0;
    size_t nunits;
    size_t *us = malloc((MAXTOK + 2) * sizeof *us), *ue = malloc((MAXTOK + 2) * sizeof *ue);

    for (int rounds = 0; rounds < 1000; rounds++) {
        nunits = 0;
        if (by_token) {
            size_t nt = lex(b->p, b->n, toks, MAXTOK);
            for (size_t i = 0; i < nt; i++) {
                if (toks[i].cls == T_WS) continue;
                us[nunits] = toks[i].s;
                ue[nunits++] = toks[i].s + toks[i].n;
            }
        } else {
            size_t a = 0;
            while (a < b->n) {
                size_t s, e;
                line_bounds(b, a, &s, &e);
                us[nunits] = s;
                ue[nunits++] = e;
                a = e;
            }
        }
        int changed = 0;
        for (size_t chunk = nunits / 2 > 0 ? nunits / 2 : 1; ; chunk /= 2) {
            for (size_t i = 0; i < nunits; ) {
                size_t j = i + chunk < nunits ? i + chunk : nunits;
                buf_t c = {0};
                buf_set(&c, b->p, b->n);
                buf_splice(&c, us[i], ue[j - 1] - us[i], "", 0);
                if (c.n < b->n && still_crashes(&c)) {
                    buf_set(b, c.p, c.n);
                    free(c.p);
                    changed = progress = 1;
                    break;      /* the offsets are stale, so recompute them */
                }
                free(c.p);
                i = j;
            }
            if (changed || chunk == 1) break;
        }
        if (!changed) break;
    }
    free(us);
    free(ue);
    return progress;
}

static int reduce(const char *bin, const char *outdir, const char *file)
{
    buf_t b = {0};
    if (read_file(file, &b) < 0) die("cannot read the file to reduce");
    snprintf(red_bin, sizeof red_bin, "%s", bin);
    snprintf(red_dir, sizeof red_dir, "%s/work/reduce", outdir);
    mkdir_p(red_dir);
    red_out = malloc(MAXCAP + 1);

    char path[PATH_MAX];
    snprintf(path, sizeof path, "%s/case.syntran", red_dir);
    write_file(path, b.p, b.n, 0);
    int r = run_case(bin, red_dir, 5, red_sig0, sizeof red_sig0, red_out);
    if (r != RES_CRASH) {
        fprintf(stderr, "fuzz: %s does not crash\n", file);
        return 1;
    }
    fprintf(stderr, "reducing %s, signature %s, %zu bytes\n", file, red_sig0, b.n);

    for (int round = 0; round < 10; round++) {
        int p = reduce_pass(&b, 0);
        p |= reduce_pass(&b, 1);
        fprintf(stderr, "  round %d: %zu bytes\n", round + 1, b.n);
        if (!p) break;
    }
    snprintf(path, sizeof path, "%s.min", file);
    write_file(path, b.p, b.n, 0);
    fprintf(stderr, "wrote %s (%zu bytes)\n", path, b.n);
    fwrite(b.p, 1, b.n, stdout);
    return 0;
}

/*============================================================================*/

static void usage(void)
{
    fputs("usage: fuzz [options] [seed-dir ...]\n"
        "  --bin <path>       syntran binary          (build/fuzz/bin/syntran)\n"
        "  --out <dir>        output directory        (build/fuzz)\n"
        "  --time <s>         stop after s seconds\n"
        "  --iters <n>        stop after n runs in total (default 1000, if no --time)\n"
        "  --jobs <n>         parallel workers        (4)\n"
        "  --seed <n>         RNG seed                (the clock)\n"
        "  --timeout <s>      per-run timeout         (5)\n"
        "  --only-dir <str>   only seed files whose path contains str\n"
        "  --reduce <file>    shrink a crasher to <file>.min and exit\n"
        "seed dirs default to src/tests/test-src samples src/tests/long\n", stderr);
    exit(2);
}

int main(int argc, char **argv)
{
    const char *bin = "build/fuzz/bin/syntran", *outdir = "build/fuzz", *red = NULL;
    const char *dirs[64] = {0};
    int ndirs = 0, jobs = 4, timeout_s = 5;
    long iters = 0, tlimit = 0;
    uint64_t seed = (uint64_t)time(NULL);

    for (int i = 1; i < argc; i++) {
        const char *a = argv[i];
        int more = i + 1 < argc;
        if (!strcmp(a, "--bin") && more) bin = argv[++i];
        else if (!strcmp(a, "--out") && more) outdir = argv[++i];
        else if (!strcmp(a, "--time") && more) tlimit = atol(argv[++i]);
        else if (!strcmp(a, "--iters") && more) iters = atol(argv[++i]);
        else if (!strcmp(a, "--jobs") && more) jobs = atoi(argv[++i]);
        else if (!strcmp(a, "--seed") && more) seed = strtoull(argv[++i], NULL, 10);
        else if (!strcmp(a, "--timeout") && more) timeout_s = atoi(argv[++i]);
        else if (!strcmp(a, "--only-dir") && more) only_dir = argv[++i];
        else if (!strcmp(a, "--reduce") && more) red = argv[++i];
        else if (a[0] != '-' && ndirs < 63) dirs[ndirs++] = a;
        else usage();
    }
    if (jobs < 1) jobs = 1;
    if (!tlimit && !iters) iters = 1000;
    if (!ndirs) {
        dirs[ndirs++] = "src/tests/test-src";
        dirs[ndirs++] = "samples";
        dirs[ndirs++] = "src/tests/long";
    }

    /* The children chdir(), so the binary and output paths have to be absolute */
    char binabs[PATH_MAX], outabs[PATH_MAX];
    mkdir_p(outdir);
    if (!realpath(bin, binabs)) {
        fprintf(stderr, "fuzz: cannot find the syntran binary `%s`\n", bin);
        return 1;
    }
    if (!realpath(outdir, outabs)) die("cannot resolve the output directory");

    if (red) return reduce(binabs, outabs, red);

    for (int i = 0; i < ndirs; i++) nftw(dirs[i], visit, 16, FTW_PHYS);
    if (!ncorpus) die("no seed files found; run from the repo root");

    char path[PATH_MAX];
    snprintf(path, sizeof path, "%s/crashes", outabs);
    mkdir_p(path);

    fprintf(stderr, "fuzzing %s: %zu seeds, %d jobs, seed %llu\n", binabs, ncorpus, jobs,
        (unsigned long long)seed);

    stats_t *st = mmap(NULL, (size_t)jobs * sizeof *st, PROT_READ | PROT_WRITE,
        MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (st == MAP_FAILED) die("mmap() failed");
    memset(st, 0, (size_t)jobs * sizeof *st);

    double t0 = now();
    double deadline = tlimit ? t0 + (double)tlimit : 0;
    pid_t *pids = calloc((size_t)jobs, sizeof *pids);
    for (int i = 0; i < jobs; i++) {
        long share = iters ? iters / jobs + (i < iters % jobs) : 0;
        if (iters && !share) continue;
        pids[i] = fork();
        if (pids[i] < 0) die("fork() failed");
        if (pids[i] == 0) {
            worker(i, seed, binabs, outabs, share, deadline, timeout_s, &st[i]);
            _exit(0);
        }
    }

    double last = t0;
    for (;;) {
        int alive = 0;
        for (int i = 0; i < jobs; i++) {
            if (!pids[i]) continue;
            if (waitpid(pids[i], NULL, WNOHANG) == pids[i]) pids[i] = 0;
            else alive++;
        }
        if (!alive) break;
        if (now() - last >= 10) {
            stats_t s = {0};
            for (int i = 0; i < jobs; i++) {
                s.runs += st[i].runs;
                s.unique += st[i].unique;
            }
            fprintf(stderr, "  %6.0fs: %llu runs, %llu unique crashes\n", now() - t0,
                (unsigned long long)s.runs, (unsigned long long)s.unique);
            last = now();
        }
        usleep(200000);
    }

    stats_t s = {0};
    for (int i = 0; i < jobs; i++) {
        s.runs += st[i].runs;
        s.ok += st[i].ok;
        s.diag += st[i].diag;
        s.crashes += st[i].crashes;
        s.unique += st[i].unique;
    }
    printf("runs %llu: ok %llu, diagnostic %llu, crash %llu (%llu new unique)\n",
        (unsigned long long)s.runs, (unsigned long long)s.ok, (unsigned long long)s.diag,
        (unsigned long long)s.crashes, (unsigned long long)s.unique);
    if (s.unique) printf("reproducers are in %s/crashes\n", outabs);
    return s.unique ? 1 : 0;
}
