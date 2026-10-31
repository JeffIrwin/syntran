/*
 * llvm_shim.c -- the one part of the native backend (src/native.F90) that is
 * easier in C than in Fortran: initializing LLVM for the machine that syntran
 * runs on.
 *
 * LLVM-C has `LLVMInitializeNativeTarget()`, but it's a static inline function
 * in a header, so there is nothing to link to.  What it does is call the
 * initializers of one architecture, which only exist in LLVM if that target
 * was built.  They are declared here instead of including the headers, so
 * that no include path is needed.
 *
 * This is only built if SYNTRAN_LLVM is defined.  Without it, the file is
 * empty, and nothing refers to its symbol.
 */

#ifdef SYNTRAN_LLVM

#if defined(__aarch64__) || defined(__arm64__)

void LLVMInitializeAArch64TargetInfo(void);
void LLVMInitializeAArch64Target(void);
void LLVMInitializeAArch64TargetMC(void);
void LLVMInitializeAArch64AsmPrinter(void);

void sy_llvm_init_native(void)
{
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmPrinter();
}

#elif defined(__x86_64__) || defined(_M_X64)

void LLVMInitializeX86TargetInfo(void);
void LLVMInitializeX86Target(void);
void LLVMInitializeX86TargetMC(void);
void LLVMInitializeX86AsmPrinter(void);

void sy_llvm_init_native(void)
{
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
}

#else
#error "src/c/llvm_shim.c: add the LLVM initializers of this architecture"
#endif

/*============================================================================*/

/* Run a module in memory, with LLVM's ORC JIT, for the test suite.  Compiling and
 * linking an executable for each of thousands of tests is too slow, so a test
 * compiles its program here and calls its main() in the process of the tests.
 *
 * Like above, what is needed of LLVM-C is declared here, so that no include path is
 * needed.  These are the same types and fns as in llvm-c/Orc.h and LLJIT.h, which
 * are stable.  A LLVMContextRef that the emitter made is handed over to the JIT
 * along with its module */

#include <dlfcn.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct LLVMOpaqueError *LLVMErrorRef;
typedef struct LLVMOrcOpaqueLLJIT *LLVMOrcLLJITRef;
typedef struct LLVMOrcOpaqueJITDylib *LLVMOrcJITDylibRef;
typedef struct LLVMOrcOpaqueThreadSafeContext *LLVMOrcThreadSafeContextRef;
typedef struct LLVMOrcOpaqueThreadSafeModule *LLVMOrcThreadSafeModuleRef;
typedef struct LLVMOrcOpaqueDefinitionGenerator *LLVMOrcDefinitionGeneratorRef;
typedef struct LLVMOrcOpaqueMaterializationUnit *LLVMOrcMaterializationUnitRef;
typedef struct LLVMOrcOpaqueSymbolStringPoolEntry *LLVMOrcSymbolStringPoolEntryRef;

typedef struct {
    uint8_t GenericFlags;
    uint8_t TargetFlags;
} LLVMJITSymbolFlags;

typedef struct {
    uint64_t Address;
    LLVMJITSymbolFlags Flags;
} LLVMJITEvaluatedSymbol;

typedef struct {
    LLVMOrcSymbolStringPoolEntryRef Name;
    LLVMJITEvaluatedSymbol Sym;
} LLVMOrcCSymbolMapPair;

LLVMErrorRef LLVMOrcCreateLLJIT(LLVMOrcLLJITRef *Result, void *Builder);
LLVMErrorRef LLVMOrcDisposeLLJIT(LLVMOrcLLJITRef J);
LLVMOrcJITDylibRef LLVMOrcLLJITGetMainJITDylib(LLVMOrcLLJITRef J);
char LLVMOrcLLJITGetGlobalPrefix(LLVMOrcLLJITRef J);
LLVMOrcSymbolStringPoolEntryRef LLVMOrcLLJITMangleAndIntern(LLVMOrcLLJITRef J,
    const char *UnmangledName);
LLVMOrcMaterializationUnitRef LLVMOrcAbsoluteSymbols(LLVMOrcCSymbolMapPair *Syms,
    size_t NumPairs);
LLVMErrorRef LLVMOrcJITDylibDefine(LLVMOrcJITDylibRef JD, LLVMOrcMaterializationUnitRef MU);
LLVMErrorRef LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(
    LLVMOrcDefinitionGeneratorRef *Result, char GlobalPrefx, void *Filter, void *FilterCtx);
void LLVMOrcJITDylibAddGenerator(LLVMOrcJITDylibRef JD, LLVMOrcDefinitionGeneratorRef DG);
LLVMOrcThreadSafeContextRef LLVMOrcCreateNewThreadSafeContext(void);
void LLVMOrcDisposeThreadSafeContext(LLVMOrcThreadSafeContextRef TSCtx);
LLVMOrcThreadSafeModuleRef LLVMOrcCreateNewThreadSafeModule(void *M,
    LLVMOrcThreadSafeContextRef TSCtx);
void LLVMOrcDisposeThreadSafeModule(LLVMOrcThreadSafeModuleRef TSM);
LLVMErrorRef LLVMOrcLLJITAddLLVMIRModule(LLVMOrcLLJITRef J, LLVMOrcJITDylibRef JD,
    LLVMOrcThreadSafeModuleRef TSM);
LLVMErrorRef LLVMOrcLLJITLookup(LLVMOrcLLJITRef J, uint64_t *Result, const char *Name);
void *LLVMWriteBitcodeToMemoryBuffer(void *M);
int LLVMParseBitcodeInContext2(void *ContextRef, void *MemBuf, void **OutModule);
void LLVMDisposeMemoryBuffer(void *MemBuf);
void LLVMDisposeModule(void *M);
void LLVMContextDispose(void *C);
char *LLVMGetErrorMessage(LLVMErrorRef Err);
void LLVMDisposeErrorMessage(char *ErrMsg);

/* In the C runtime, which is also compiled into syntran when it is built with LLVM */
int sy_jit_call(int (*fn)(int, char **));

/* The symbols of the C runtime, which the program refers to by name */
#define SY_SYM(n) extern char n;
#include "../rt/syntran_syms.inc"
#undef SY_SYM

#define SY_SYM(n) {#n, (void *)&n},
static const struct {
    const char *name;
    void *addr;
} sy_syms[] = {
#include "../rt/syntran_syms.inc"
};
#undef SY_SYM

static char sy_jit_err[1024];

static int sy_jit_fail(LLVMErrorRef e, const char *what)
{
    char *m = LLVMGetErrorMessage(e);

    snprintf(sy_jit_err, sizeof(sy_jit_err), "%s: %s", what, m);
    LLVMDisposeErrorMessage(m);
    return -1;
}

/* Hand a module to the JIT, as a thread safe module.  Which fns there are for it depends
 * on the version of LLVM, so they are looked up by name rather than linked.  A newer one
 * (21 or so) can wrap the context that the module is already in.  An older one can only
 * make a context of its own and ask what it is, so the module is moved into it through
 * bitcode.  Either way, this takes ownership of the module and the context */
static LLVMOrcThreadSafeModuleRef sy_jit_wrap(void *module, void *ctx)
{
    LLVMOrcThreadSafeContextRef (*from_ctx)(void *);
    void *(*get_ctx)(LLVMOrcThreadSafeContextRef);
    LLVMOrcThreadSafeContextRef tsc;
    LLVMOrcThreadSafeModuleRef tsm;
    void *ctx2, *buf, *module2 = NULL;

    *(void **)&from_ctx = dlsym(RTLD_DEFAULT, "LLVMOrcCreateNewThreadSafeContextFromLLVMContext");
    if (from_ctx != NULL) {
        /* Disposing of this reference after the module is handed over keeps the
         * context alive until the JIT is done */
        tsc = from_ctx(ctx);
        tsm = LLVMOrcCreateNewThreadSafeModule(module, tsc);
        LLVMOrcDisposeThreadSafeContext(tsc);
        return tsm;
    }

    *(void **)&get_ctx = dlsym(RTLD_DEFAULT, "LLVMOrcThreadSafeContextGetContext");
    if (get_ctx == NULL) {
        snprintf(sy_jit_err, sizeof(sy_jit_err),
            "this LLVM has no way to run a module in its own context");
        return NULL;
    }
    tsc = LLVMOrcCreateNewThreadSafeContext();
    ctx2 = get_ctx(tsc);
    buf = LLVMWriteBitcodeToMemoryBuffer(module);
    /* The buffer is taken by the parser */
    if (LLVMParseBitcodeInContext2(ctx2, buf, &module2) != 0) {
        snprintf(sy_jit_err, sizeof(sy_jit_err), "cannot move the module to the JIT context");
        LLVMDisposeMemoryBuffer(buf);
        LLVMOrcDisposeThreadSafeContext(tsc);
        return NULL;
    }
    LLVMDisposeMemoryBuffer(buf);
    LLVMDisposeModule(module);
    LLVMContextDispose(ctx);
    tsm = LLVMOrcCreateNewThreadSafeModule(module2, tsc);
    LLVMOrcDisposeThreadSafeContext(tsc);
    return tsm;
}

/* Compile `module` and run its main().  This takes ownership of the module, and of
 * the context that it is in.  The result is what sy_jit_call() says, or -1 if LLVM
 * couldn't compile it, in which case sy_llvm_jit_error() says why */
int sy_llvm_jit_run(void *module, void *ctx)
{
    LLVMOrcLLJITRef jit = NULL;
    LLVMOrcJITDylibRef jd;
    LLVMOrcDefinitionGeneratorRef gen = NULL;
    LLVMOrcThreadSafeModuleRef tsm;
    LLVMErrorRef e;
    LLVMOrcCSymbolMapPair *pairs;
    size_t i, n = sizeof(sy_syms) / sizeof(sy_syms[0]);
    uint64_t addr = 0;
    int status;

    sy_jit_err[0] = '\0';

    tsm = sy_jit_wrap(module, ctx);
    if (tsm == NULL) return -1;

    e = LLVMOrcCreateLLJIT(&jit, NULL);
    if (e != NULL) {
        LLVMOrcDisposeThreadSafeModule(tsm);
        return sy_jit_fail(e, "cannot start the JIT");
    }
    jd = LLVMOrcLLJITGetMainJITDylib(jit);

    /* libc and libm, from the process.  The runtime itself next, whose symbols the
     * executable of syntran doesn't export */
    e = LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(&gen,
        LLVMOrcLLJITGetGlobalPrefix(jit), NULL, NULL);
    if (e != NULL) {
        LLVMOrcDisposeThreadSafeModule(tsm);
        LLVMOrcDisposeLLJIT(jit);
        return sy_jit_fail(e, "cannot find the symbols of the process");
    }
    LLVMOrcJITDylibAddGenerator(jd, gen);

    pairs = (LLVMOrcCSymbolMapPair *)malloc(n * sizeof(*pairs));
    for (i = 0; i < n; i++) {
        pairs[i].Name = LLVMOrcLLJITMangleAndIntern(jit, sy_syms[i].name);
        pairs[i].Sym.Address = (uint64_t)(uintptr_t)sy_syms[i].addr;
        /* Exported (1) | Callable (4).  A variable isn't callable, but it doesn't matter */
        pairs[i].Sym.Flags.GenericFlags = 1 | 4;
        pairs[i].Sym.Flags.TargetFlags = 0;
    }
    e = LLVMOrcJITDylibDefine(jd, LLVMOrcAbsoluteSymbols(pairs, n));
    free(pairs);
    if (e != NULL) {
        LLVMOrcDisposeThreadSafeModule(tsm);
        LLVMOrcDisposeLLJIT(jit);
        return sy_jit_fail(e, "cannot define the runtime");
    }

    e = LLVMOrcLLJITAddLLVMIRModule(jit, jd, tsm);
    if (e != NULL) {
        /* tsm belongs to the JIT even if this fails */
        LLVMOrcDisposeLLJIT(jit);
        return sy_jit_fail(e, "cannot add the module");
    }

    e = LLVMOrcLLJITLookup(jit, &addr, "main");
    if (e != NULL) {
        LLVMOrcDisposeLLJIT(jit);
        return sy_jit_fail(e, "cannot compile the module");
    }

    status = sy_jit_call((int (*)(int, char **))(uintptr_t)addr);
    e = LLVMOrcDisposeLLJIT(jit);
    if (e != NULL) LLVMDisposeErrorMessage(LLVMGetErrorMessage(e));
    return status;
}

const char *sy_llvm_jit_error(void)
{
    return sy_jit_err;
}

#endif /* SYNTRAN_LLVM */
