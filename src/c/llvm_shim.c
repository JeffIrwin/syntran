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

#endif /* SYNTRAN_LLVM */
