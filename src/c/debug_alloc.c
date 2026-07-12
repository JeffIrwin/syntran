#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <windows.h>

/* Minimal guard-byte heap checker.  Wraps malloc/free/realloc/calloc via
 * the linker (-Wl,--wrap=malloc etc) to detect heap-buffer overruns at
 * free()/realloc() time instead of much later inside the allocator's own
 * internals.  Also records the allocation call site.
 *
 * Only intercepts calls made from the executable's own compiled code;
 * separately-linked DLLs (e.g. libgfortran-5.dll) have their own imports of
 * malloc/free and are NOT affected by --wrap, so pointers can cross that
 * boundary (allocated by a DLL's own unwrapped malloc, freed by code that
 * resolves to our wrapped free, or vice versa).  A magic canary at a fixed
 * offset distinguishes blocks we actually own from foreign ones, so foreign
 * pointers are passed straight to the real allocator instead of being
 * misinterpreted as having our header. */

#define GUARD_SIZE 8
#define MAGIC_BYTE 0xA5
#define OWNER_MAGIC 0x53594e5452414e21ULL /* "SYNTRAN!" */

void *__real_malloc(size_t size);
void __real_free(void *ptr);
void *__real_realloc(void *ptr, size_t size);
void *__real_calloc(size_t nmemb, size_t size);

typedef struct {
	uint64_t owner_magic;
	size_t size;
	void *alloc_caller;
	unsigned char guard_pre[GUARD_SIZE];
} header_t;

static void fill_guard(unsigned char *g) {
	for (int i = 0; i < GUARD_SIZE; i++) g[i] = MAGIC_BYTE;
}

static int check_guard(const unsigned char *g, const char *which, void *user_ptr, void *alloc_caller, size_t size) {
	int bad = 0;
	for (int i = 0; i < GUARD_SIZE; i++) {
		if (g[i] != MAGIC_BYTE) { bad = 1; break; }
	}
	if (!bad) return 0;

	{
		HMODULE mod_base = GetModuleHandle(NULL);
		fprintf(stderr,
			"\n*** GUARD CORRUPTION DETECTED: %s guard of alloc %p ***\n"
			"*** module base: %p ; alloc caller RVA: 0x%llx ***\n"
			"*** this alloc (size=%zu) was allocated by caller at %p ***\n"
			"*** guard hex: ",
			which, user_ptr, (void *)mod_base,
			(unsigned long long)((unsigned char *)alloc_caller - (unsigned char *)mod_base),
			size, alloc_caller);
	}
	for (int i = 0; i < GUARD_SIZE; i++) fprintf(stderr, "%02x ", g[i]);
	fprintf(stderr, "\n*** guard as text: \"");
	for (int i = 0; i < GUARD_SIZE; i++) {
		unsigned char c = g[i];
		fputc((c >= 32 && c < 127) ? c : '.', stderr);
	}
	fprintf(stderr, "\" ***\n");
	fflush(stderr);
	return 1;
}

static void *wrap_alloc_common(size_t size, void *caller) {
	if (size == 0) size = 1;
	size_t total = sizeof(header_t) + size + GUARD_SIZE;
	unsigned char *base = (unsigned char *)__real_malloc(total);
	if (!base) return NULL;

	header_t *hdr = (header_t *)base;
	hdr->owner_magic = OWNER_MAGIC;
	hdr->size = size;
	hdr->alloc_caller = caller;
	fill_guard(hdr->guard_pre);

	unsigned char *user_ptr = base + sizeof(header_t);
	unsigned char *post_guard = user_ptr + size;
	fill_guard(post_guard);

	return user_ptr;
}

void *__wrap_malloc(size_t size) {
	return wrap_alloc_common(size, __builtin_return_address(0));
}

void *__wrap_calloc(size_t nmemb, size_t size) {
	size_t total_size = nmemb * size;
	void *p = wrap_alloc_common(total_size, __builtin_return_address(0));
	if (p) memset(p, 0, total_size);
	return p;
}

/* Is this a pointer we actually allocated?  Only trust the header bytes if
 * the owner magic is present -- foreign pointers (from a DLL's own
 * unwrapped allocator) don't have it, and reading arbitrary "header" bytes
 * for those would be undefined behavior in its own right. */
static int is_ours(void *user_ptr) {
	if (!user_ptr) return 0;
	header_t *hdr = (header_t *)((unsigned char *)user_ptr - sizeof(header_t));
	/* Best-effort: this read itself assumes the header region is mapped,
	 * which holds for any real heap pointer (there's always some prior
	 * allocator metadata immediately before a live block). */
	return hdr->owner_magic == OWNER_MAGIC;
}

void __wrap_free(void *ptr) {
	if (!ptr) return;
	if (!is_ours(ptr)) { __real_free(ptr); return; }

	unsigned char *user_ptr = (unsigned char *)ptr;
	header_t *hdr = (header_t *)(user_ptr - sizeof(header_t));
	unsigned char *post_guard = user_ptr + hdr->size;

	int pre_bad = check_guard(hdr->guard_pre, "PRE (underflow before alloc)", ptr, hdr->alloc_caller, hdr->size);
	int post_bad = check_guard(post_guard, "POST (overflow after alloc)", ptr, hdr->alloc_caller, hdr->size);

	if (pre_bad || post_bad) {
		HMODULE mod_base = GetModuleHandle(NULL);
		void *free_caller = __builtin_return_address(0);
		fprintf(stderr, "*** freeing caller: %p (RVA 0x%llx); aborting now for a clean stack trace ***\n",
			free_caller,
			(unsigned long long)((unsigned char *)free_caller - (unsigned char *)mod_base));
		fflush(stderr);
		abort();
	}

	__real_free((void *)hdr);
}

void *__wrap_realloc(void *ptr, size_t size) {
	if (!ptr) return wrap_alloc_common(size, __builtin_return_address(0));
	if (size == 0) { __wrap_free(ptr); return NULL; }
	if (!is_ours(ptr)) return __real_realloc(ptr, size);

	unsigned char *user_ptr = (unsigned char *)ptr;
	header_t *hdr = (header_t *)(user_ptr - sizeof(header_t));
	unsigned char *post_guard = user_ptr + hdr->size;

	int pre_bad = check_guard(hdr->guard_pre, "PRE (underflow before realloc)", ptr, hdr->alloc_caller, hdr->size);
	int post_bad = check_guard(post_guard, "POST (overflow after realloc, before realloc call)", ptr, hdr->alloc_caller, hdr->size);
	if (pre_bad || post_bad) {
		fprintf(stderr, "*** realloc caller: %p; aborting now for a clean stack trace ***\n",
			__builtin_return_address(0));
		fflush(stderr);
		abort();
	}

	size_t old_size = hdr->size;
	void *new_user_ptr = wrap_alloc_common(size, __builtin_return_address(0));
	if (!new_user_ptr) return NULL;

	size_t copy_size = old_size < size ? old_size : size;
	memcpy(new_user_ptr, ptr, copy_size);

	__real_free((void *)hdr);
	return new_user_ptr;
}
