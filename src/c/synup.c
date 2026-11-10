/* synup.c
 *
 * The parts of `synup`, the toolchain manager, that Fortran can't do portably: the platform of
 * this build, the path of the running exe, SHA-256, and a few file system calls.  The rest is
 * in src/synup.f90.
 *
 * The platform is picked by the C preprocessor, since gfortran defines no _WIN32 etc.  Release
 * asset names are part of a lasting contract with older synups, so don't rename them.
 *
 * Every fn returns 0 for success, unless it says otherwise.
 *
 * Use spaces, not tabs, and keep lines under 100 chars.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#else
#include <limits.h>
#include <unistd.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
#endif

static int copy_out(char *buf, int n, const char *s)
{
    size_t len = strlen(s);
    if (n <= 0 || len >= (size_t)n) return -1;
    memcpy(buf, s, len + 1);
    return 0;
}

/* The release asset for this platform, or -1 if there's no prebuilt binary */
int synup_asset(char *buf, int n)
{
#if defined(__linux__) && (defined(__x86_64__) || defined(__amd64__))
    return copy_out(buf, n, "syntran-linux.zip");
#elif defined(__APPLE__) && defined(__aarch64__)
    return copy_out(buf, n, "syntran-macos.zip");
#elif defined(_WIN32) && (defined(__x86_64__) || defined(_M_X64))
    return copy_out(buf, n, "syntran-windows.zip");
#else
    (void)buf; (void)n;
    return -1;
#endif
}

/* The path of the running exe, with symlinks resolved on POSIX */
int synup_self_exe(char *buf, int n)
{
#if defined(_WIN32)
    DWORD r = GetModuleFileNameA(NULL, buf, (DWORD)n);
    return (r == 0 || r >= (DWORD)n) ? -1 : 0;
#elif defined(__APPLE__)
    char raw[PATH_MAX], real[PATH_MAX];
    uint32_t sz = (uint32_t)sizeof raw;
    if (_NSGetExecutablePath(raw, &sz) != 0) return -1;
    if (realpath(raw, real) == NULL) return -1;
    return copy_out(buf, n, real);
#else
    char raw[PATH_MAX];
    ssize_t r = readlink("/proc/self/exe", raw, sizeof raw - 1);
    if (r <= 0) return -1;
    raw[r] = '\0';
    return copy_out(buf, n, raw);
#endif
}

/* 0 if the path doesn't exist, 1 for a file, 2 for a dir */
int synup_exists(const char *path)
{
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode) ? 2 : 1;
}

/* Succeeds if the dir already exists */
int synup_mkdir(const char *path)
{
    if (synup_exists(path) == 2) return 0;
#ifdef _WIN32
    return _mkdir(path);
#else
    return mkdir(path, 0755);
#endif
}

/* Replaces `to` if it exists, atomically on POSIX.  C rename() fails on Windows if `to` exists */
int synup_rename(const char *from, const char *to)
{
#ifdef _WIN32
    return MoveFileExA(from, to, MOVEFILE_REPLACE_EXISTING) ? 0 : -1;
#else
    return rename(from, to);
#endif
}

int synup_remove(const char *path)
{
    return remove(path);
}

/* Windows needs privileges for symlinks, so callers copy there instead */
int synup_symlink(const char *target, const char *link)
{
#ifdef _WIN32
    (void)target; (void)link;
    return -1;
#else
    return symlink(target, link);
#endif
}

int synup_chmod_x(const char *path)
{
#ifdef _WIN32
    (void)path;
    return 0;
#else
    return chmod(path, 0755);
#endif
}

/*==============================================================================
 * SHA-256, FIPS 180-4
 */

static const uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))

typedef struct {
    uint32_t h[8];
    uint8_t block[64];
    uint64_t total;  /* bytes hashed */
    size_t fill;     /* bytes in block */
} sha256_t;

static void sha256_init(sha256_t *s)
{
    static const uint32_t h0[8] = {
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    };
    memcpy(s->h, h0, sizeof h0);
    s->total = 0;
    s->fill = 0;
}

static void sha256_compress(sha256_t *s, const uint8_t *p)
{
    uint32_t w[64], a, b, c, d, e, f, g, h, t1, t2;
    int i;

    for (i = 0; i < 16; i++) {
        w[i] = ((uint32_t)p[4*i] << 24) | ((uint32_t)p[4*i + 1] << 16) |
               ((uint32_t)p[4*i + 2] << 8) | (uint32_t)p[4*i + 3];
    }
    for (i = 16; i < 64; i++) {
        uint32_t s0 = ROR(w[i-15], 7) ^ ROR(w[i-15], 18) ^ (w[i-15] >> 3);
        uint32_t s1 = ROR(w[i-2], 17) ^ ROR(w[i-2], 19) ^ (w[i-2] >> 10);
        w[i] = w[i-16] + s0 + w[i-7] + s1;
    }

    a = s->h[0]; b = s->h[1]; c = s->h[2]; d = s->h[3];
    e = s->h[4]; f = s->h[5]; g = s->h[6]; h = s->h[7];

    for (i = 0; i < 64; i++) {
        t1 = h + (ROR(e, 6) ^ ROR(e, 11) ^ ROR(e, 25)) + ((e & f) ^ (~e & g)) + K[i] + w[i];
        t2 = (ROR(a, 2) ^ ROR(a, 13) ^ ROR(a, 22)) + ((a & b) ^ (a & c) ^ (b & c));
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    s->h[0] += a; s->h[1] += b; s->h[2] += c; s->h[3] += d;
    s->h[4] += e; s->h[5] += f; s->h[6] += g; s->h[7] += h;
}

static void sha256_update(sha256_t *s, const uint8_t *p, size_t n)
{
    s->total += n;
    while (n > 0) {
        size_t take = 64 - s->fill;
        if (take > n) take = n;
        memcpy(s->block + s->fill, p, take);
        s->fill += take;
        p += take;
        n -= take;
        if (s->fill == 64) {
            sha256_compress(s, s->block);
            s->fill = 0;
        }
    }
}

static void sha256_final(sha256_t *s, char *hex)
{
    static const char digits[] = "0123456789abcdef";
    uint64_t bits = s->total * 8;
    uint8_t pad[72];
    size_t padlen;
    int i;

    /* A 1 bit, zeros up to 56 mod 64, then the 64-bit big-endian length */
    padlen = (s->fill < 56) ? 56 - s->fill : 120 - s->fill;
    memset(pad, 0, sizeof pad);
    pad[0] = 0x80;
    for (i = 0; i < 8; i++) pad[padlen + i] = (uint8_t)(bits >> (56 - 8*i));
    sha256_update(s, pad, padlen + 8);

    for (i = 0; i < 8; i++) {
        int j;
        for (j = 0; j < 4; j++) {
            uint8_t byte = (uint8_t)(s->h[i] >> (24 - 8*j));
            hex[8*i + 2*j]     = digits[byte >> 4];
            hex[8*i + 2*j + 1] = digits[byte & 15];
        }
    }
    hex[64] = '\0';
}

/* Lowercase hex digest of a file into hex[65].  Nonzero if the file can't be read */
int synup_sha256_file(const char *path, char *hex)
{
    sha256_t s;
    uint8_t buf[65536];
    size_t n;
    FILE *f = fopen(path, "rb");

    if (f == NULL) return -1;
    sha256_init(&s);
    while ((n = fread(buf, 1, sizeof buf, f)) > 0) sha256_update(&s, buf, n);
    if (ferror(f)) {
        fclose(f);
        return -1;
    }
    fclose(f);
    sha256_final(&s, hex);
    return 0;
}
