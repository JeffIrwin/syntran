
!===============================================================================

module syntran__llvm_c_m

	! Interfaces to the part of the LLVM-C API that the native backend (see
	! native.F90) uses.  This is only declarations: nothing here needs LLVM to
	! be linked unless it's called, and native.F90 only calls these if syntran
	! is built with SYNTRAN_LLVM.
	!
	! Every LLVM handle (context, module, type, value, basic block, builder,
	! target machine, ...) is an opaque pointer, i.e. a c_ptr.  Names are C
	! strings, so pass `"name"//c_null_char`.  The names of the constants below
	! are those of the C enums.  They are what the headers number them as, and
	! have been the same since LLVM 17

	use, intrinsic :: iso_c_binding

	implicit none

	! LLVMIntPredicate
	integer(c_int), parameter :: &
		LLVM_INT_EQ  = 32, LLVM_INT_NE  = 33, &
		LLVM_INT_UGT = 34, LLVM_INT_UGE = 35, LLVM_INT_ULT = 36, LLVM_INT_ULE = 37, &
		LLVM_INT_SGT = 38, LLVM_INT_SGE = 39, LLVM_INT_SLT = 40, LLVM_INT_SLE = 41

	! LLVMRealPredicate.  The ordered ones are false for a NaN, and UNE is true
	integer(c_int), parameter :: &
		LLVM_REAL_OEQ = 1, LLVM_REAL_OGT = 2, LLVM_REAL_OGE = 3, LLVM_REAL_OLT = 4, &
		LLVM_REAL_OLE = 5, LLVM_REAL_ONE = 6, LLVM_REAL_UNE = 14

	! LLVMLinkage
	integer(c_int), parameter :: LLVM_EXTERNAL_LINKAGE = 0, LLVM_INTERNAL_LINKAGE = 8, &
		LLVM_PRIVATE_LINKAGE = 9

	! LLVMVerifierFailureAction
	integer(c_int), parameter :: LLVM_RETURN_STATUS_ACTION = 2

	! LLVMCodeGenOptLevel, LLVMRelocMode, LLVMCodeModel, LLVMCodeGenFileType
	integer(c_int), parameter :: LLVM_CODEGEN_DEFAULT = 2, LLVM_CODEGEN_AGGRESSIVE = 3, &
		LLVM_RELOC_DEFAULT = 0, LLVM_RELOC_PIC = 2, LLVM_CODE_MODEL_DEFAULT = 0, &
		LLVM_ASSEMBLY_FILE = 0, LLVM_OBJECT_FILE = 1

	interface

		!**** Context, module, target

		function LLVMContextCreate() bind(c, name = "LLVMContextCreate")
			import :: c_ptr
			type(c_ptr) :: LLVMContextCreate
		end function LLVMContextCreate

		subroutine LLVMContextDispose(ctx) bind(c, name = "LLVMContextDispose")
			import :: c_ptr
			type(c_ptr), value :: ctx
		end subroutine LLVMContextDispose

		function LLVMModuleCreateWithNameInContext(nm, ctx) &
				bind(c, name = "LLVMModuleCreateWithNameInContext")
			import :: c_ptr, c_char
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMModuleCreateWithNameInContext
		end function LLVMModuleCreateWithNameInContext

		subroutine LLVMDisposeModule(m) bind(c, name = "LLVMDisposeModule")
			import :: c_ptr
			type(c_ptr), value :: m
		end subroutine LLVMDisposeModule

		subroutine LLVMSetTarget(m, triple) bind(c, name = "LLVMSetTarget")
			import :: c_ptr, c_char
			type(c_ptr), value :: m
			character(kind = c_char), intent(in) :: triple(*)
		end subroutine LLVMSetTarget

		subroutine LLVMSetModuleDataLayout(m, dl) bind(c, name = "LLVMSetModuleDataLayout")
			import :: c_ptr
			type(c_ptr), value :: m, dl
		end subroutine LLVMSetModuleDataLayout

		function LLVMVerifyModule(m, action, msg) bind(c, name = "LLVMVerifyModule")
			import :: c_ptr, c_int
			type(c_ptr), value :: m
			integer(c_int), value :: action
			type(c_ptr), intent(out) :: msg
			integer(c_int) :: LLVMVerifyModule
		end function LLVMVerifyModule

		function LLVMPrintModuleToFile(m, filename, msg) bind(c, name = "LLVMPrintModuleToFile")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: m
			character(kind = c_char), intent(in) :: filename(*)
			type(c_ptr), intent(out) :: msg
			integer(c_int) :: LLVMPrintModuleToFile
		end function LLVMPrintModuleToFile

		subroutine LLVMDisposeMessage(msg) bind(c, name = "LLVMDisposeMessage")
			import :: c_ptr
			type(c_ptr), value :: msg
		end subroutine LLVMDisposeMessage

		!**** Types

		function LLVMInt1TypeInContext(ctx) bind(c, name = "LLVMInt1TypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMInt1TypeInContext
		end function LLVMInt1TypeInContext

		function LLVMInt8TypeInContext(ctx) bind(c, name = "LLVMInt8TypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMInt8TypeInContext
		end function LLVMInt8TypeInContext

		function LLVMInt32TypeInContext(ctx) bind(c, name = "LLVMInt32TypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMInt32TypeInContext
		end function LLVMInt32TypeInContext

		function LLVMInt64TypeInContext(ctx) bind(c, name = "LLVMInt64TypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMInt64TypeInContext
		end function LLVMInt64TypeInContext

		function LLVMFloatTypeInContext(ctx) bind(c, name = "LLVMFloatTypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMFloatTypeInContext
		end function LLVMFloatTypeInContext

		function LLVMDoubleTypeInContext(ctx) bind(c, name = "LLVMDoubleTypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMDoubleTypeInContext
		end function LLVMDoubleTypeInContext

		function LLVMVoidTypeInContext(ctx) bind(c, name = "LLVMVoidTypeInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMVoidTypeInContext
		end function LLVMVoidTypeInContext

		function LLVMPointerTypeInContext(ctx, addrspace) bind(c, name = "LLVMPointerTypeInContext")
			import :: c_ptr, c_int
			type(c_ptr), value :: ctx
			integer(c_int), value :: addrspace
			type(c_ptr) :: LLVMPointerTypeInContext
		end function LLVMPointerTypeInContext

		function LLVMArrayType2(elem, n) bind(c, name = "LLVMArrayType2")
			import :: c_ptr, c_int64_t
			type(c_ptr), value :: elem
			integer(c_int64_t), value :: n
			type(c_ptr) :: LLVMArrayType2
		end function LLVMArrayType2

		function LLVMStructTypeInContext(ctx, elems, n, packed) &
				bind(c, name = "LLVMStructTypeInContext")
			import :: c_ptr, c_int
			type(c_ptr), value :: ctx
			type(c_ptr), intent(in) :: elems(*)
			integer(c_int), value :: n, packed
			type(c_ptr) :: LLVMStructTypeInContext
		end function LLVMStructTypeInContext

		function LLVMFunctionType(ret, params, n, vararg) bind(c, name = "LLVMFunctionType")
			import :: c_ptr, c_int
			type(c_ptr), value :: ret
			type(c_ptr), intent(in) :: params(*)
			integer(c_int), value :: n, vararg
			type(c_ptr) :: LLVMFunctionType
		end function LLVMFunctionType

		function LLVMTypeOf(v) bind(c, name = "LLVMTypeOf")
			import :: c_ptr
			type(c_ptr), value :: v
			type(c_ptr) :: LLVMTypeOf
		end function LLVMTypeOf

		!**** Functions, globals, constants

		function LLVMAddFunction(m, nm, ty) bind(c, name = "LLVMAddFunction")
			import :: c_ptr, c_char
			type(c_ptr), value :: m, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMAddFunction
		end function LLVMAddFunction

		function LLVMGetNamedFunction(m, nm) bind(c, name = "LLVMGetNamedFunction")
			import :: c_ptr, c_char
			type(c_ptr), value :: m
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMGetNamedFunction
		end function LLVMGetNamedFunction

		function LLVMGetParam(f, i) bind(c, name = "LLVMGetParam")
			import :: c_ptr, c_int
			type(c_ptr), value :: f
			integer(c_int), value :: i
			type(c_ptr) :: LLVMGetParam
		end function LLVMGetParam

		function LLVMAddGlobal(m, ty, nm) bind(c, name = "LLVMAddGlobal")
			import :: c_ptr, c_char
			type(c_ptr), value :: m, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMAddGlobal
		end function LLVMAddGlobal

		subroutine LLVMSetInitializer(g, v) bind(c, name = "LLVMSetInitializer")
			import :: c_ptr
			type(c_ptr), value :: g, v
		end subroutine LLVMSetInitializer

		subroutine LLVMSetGlobalConstant(g, flag) bind(c, name = "LLVMSetGlobalConstant")
			import :: c_ptr, c_int
			type(c_ptr), value :: g
			integer(c_int), value :: flag
		end subroutine LLVMSetGlobalConstant

		subroutine LLVMSetLinkage(g, linkage) bind(c, name = "LLVMSetLinkage")
			import :: c_ptr, c_int
			type(c_ptr), value :: g
			integer(c_int), value :: linkage
		end subroutine LLVMSetLinkage

		function LLVMConstInt(ty, n, signext) bind(c, name = "LLVMConstInt")
			import :: c_ptr, c_int, c_int64_t
			type(c_ptr), value :: ty
			integer(c_int64_t), value :: n
			integer(c_int), value :: signext
			type(c_ptr) :: LLVMConstInt
		end function LLVMConstInt

		function LLVMConstReal(ty, x) bind(c, name = "LLVMConstReal")
			import :: c_ptr, c_double
			type(c_ptr), value :: ty
			real(c_double), value :: x
			type(c_ptr) :: LLVMConstReal
		end function LLVMConstReal

		function LLVMConstNull(ty) bind(c, name = "LLVMConstNull")
			import :: c_ptr
			type(c_ptr), value :: ty
			type(c_ptr) :: LLVMConstNull
		end function LLVMConstNull

		! Not the *2 version with a size_t length, which is only in LLVM 19 and later
		function LLVMConstStringInContext(ctx, str, n, dont_null_terminate) &
				bind(c, name = "LLVMConstStringInContext")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: ctx
			character(kind = c_char), intent(in) :: str(*)
			integer(c_int), value :: n
			integer(c_int), value :: dont_null_terminate
			type(c_ptr) :: LLVMConstStringInContext
		end function LLVMConstStringInContext

		function LLVMConstArray(elem, vals, n) bind(c, name = "LLVMConstArray")
			import :: c_ptr, c_int
			type(c_ptr), value :: elem
			type(c_ptr), intent(in) :: vals(*)
			integer(c_int), value :: n
			type(c_ptr) :: LLVMConstArray
		end function LLVMConstArray

		function LLVMConstStructInContext(ctx, vals, n, packed) &
				bind(c, name = "LLVMConstStructInContext")
			import :: c_ptr, c_int
			type(c_ptr), value :: ctx
			type(c_ptr), intent(in) :: vals(*)
			integer(c_int), value :: n, packed
			type(c_ptr) :: LLVMConstStructInContext
		end function LLVMConstStructInContext

		!**** Basic blocks and the builder

		function LLVMCreateBuilderInContext(ctx) bind(c, name = "LLVMCreateBuilderInContext")
			import :: c_ptr
			type(c_ptr), value :: ctx
			type(c_ptr) :: LLVMCreateBuilderInContext
		end function LLVMCreateBuilderInContext

		subroutine LLVMDisposeBuilder(b) bind(c, name = "LLVMDisposeBuilder")
			import :: c_ptr
			type(c_ptr), value :: b
		end subroutine LLVMDisposeBuilder

		function LLVMAppendBasicBlockInContext(ctx, f, nm) &
				bind(c, name = "LLVMAppendBasicBlockInContext")
			import :: c_ptr, c_char
			type(c_ptr), value :: ctx, f
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMAppendBasicBlockInContext
		end function LLVMAppendBasicBlockInContext

		subroutine LLVMPositionBuilderAtEnd(b, bb) bind(c, name = "LLVMPositionBuilderAtEnd")
			import :: c_ptr
			type(c_ptr), value :: b, bb
		end subroutine LLVMPositionBuilderAtEnd

		function LLVMGetInsertBlock(b) bind(c, name = "LLVMGetInsertBlock")
			import :: c_ptr
			type(c_ptr), value :: b
			type(c_ptr) :: LLVMGetInsertBlock
		end function LLVMGetInsertBlock

		function LLVMGetBasicBlockTerminator(bb) bind(c, name = "LLVMGetBasicBlockTerminator")
			import :: c_ptr
			type(c_ptr), value :: bb
			type(c_ptr) :: LLVMGetBasicBlockTerminator
		end function LLVMGetBasicBlockTerminator

		!**** Instructions

		function LLVMBuildRet(b, v) bind(c, name = "LLVMBuildRet")
			import :: c_ptr
			type(c_ptr), value :: b, v
			type(c_ptr) :: LLVMBuildRet
		end function LLVMBuildRet

		function LLVMBuildRetVoid(b) bind(c, name = "LLVMBuildRetVoid")
			import :: c_ptr
			type(c_ptr), value :: b
			type(c_ptr) :: LLVMBuildRetVoid
		end function LLVMBuildRetVoid

		function LLVMBuildBr(b, dest) bind(c, name = "LLVMBuildBr")
			import :: c_ptr
			type(c_ptr), value :: b, dest
			type(c_ptr) :: LLVMBuildBr
		end function LLVMBuildBr

		function LLVMBuildCondBr(b, cond, t, f) bind(c, name = "LLVMBuildCondBr")
			import :: c_ptr
			type(c_ptr), value :: b, cond, t, f
			type(c_ptr) :: LLVMBuildCondBr
		end function LLVMBuildCondBr

		function LLVMBuildUnreachable(b) bind(c, name = "LLVMBuildUnreachable")
			import :: c_ptr
			type(c_ptr), value :: b
			type(c_ptr) :: LLVMBuildUnreachable
		end function LLVMBuildUnreachable

		function LLVMBuildICmp(b, pred, l, r, nm) bind(c, name = "LLVMBuildICmp")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: b, l, r
			integer(c_int), value :: pred
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildICmp
		end function LLVMBuildICmp

		function LLVMBuildFCmp(b, pred, l, r, nm) bind(c, name = "LLVMBuildFCmp")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: b, l, r
			integer(c_int), value :: pred
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFCmp
		end function LLVMBuildFCmp

		function LLVMBuildSelect(b, cond, t, f, nm) bind(c, name = "LLVMBuildSelect")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, cond, t, f
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSelect
		end function LLVMBuildSelect

		function LLVMBuildAlloca(b, ty, nm) bind(c, name = "LLVMBuildAlloca")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildAlloca
		end function LLVMBuildAlloca

		function LLVMBuildLoad2(b, ty, p, nm) bind(c, name = "LLVMBuildLoad2")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, ty, p
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildLoad2
		end function LLVMBuildLoad2

		function LLVMBuildStore(b, v, p) bind(c, name = "LLVMBuildStore")
			import :: c_ptr
			type(c_ptr), value :: b, v, p
			type(c_ptr) :: LLVMBuildStore
		end function LLVMBuildStore

		function LLVMBuildGEP2(b, ty, p, idx, n, nm) bind(c, name = "LLVMBuildGEP2")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: b, ty, p
			type(c_ptr), intent(in) :: idx(*)
			integer(c_int), value :: n
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildGEP2
		end function LLVMBuildGEP2

		function LLVMBuildStructGEP2(b, ty, p, idx, nm) bind(c, name = "LLVMBuildStructGEP2")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: b, ty, p
			integer(c_int), value :: idx
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildStructGEP2
		end function LLVMBuildStructGEP2

		function LLVMBuildCall2(b, fnty, fn, args, n, nm) bind(c, name = "LLVMBuildCall2")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: b, fnty, fn
			type(c_ptr), intent(in) :: args(*)
			integer(c_int), value :: n
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildCall2
		end function LLVMBuildCall2

		function LLVMBuildPhi(b, ty, nm) bind(c, name = "LLVMBuildPhi")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildPhi
		end function LLVMBuildPhi

		subroutine LLVMAddIncoming(phi, vals, blocks, n) bind(c, name = "LLVMAddIncoming")
			import :: c_ptr, c_int
			type(c_ptr), value :: phi
			type(c_ptr), intent(in) :: vals(*), blocks(*)
			integer(c_int), value :: n
		end subroutine LLVMAddIncoming

		function LLVMBuildAdd(b, l, r, nm) bind(c, name = "LLVMBuildAdd")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildAdd
		end function LLVMBuildAdd

		function LLVMBuildSub(b, l, r, nm) bind(c, name = "LLVMBuildSub")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSub
		end function LLVMBuildSub

		function LLVMBuildMul(b, l, r, nm) bind(c, name = "LLVMBuildMul")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildMul
		end function LLVMBuildMul

		function LLVMBuildSDiv(b, l, r, nm) bind(c, name = "LLVMBuildSDiv")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSDiv
		end function LLVMBuildSDiv

		function LLVMBuildSRem(b, l, r, nm) bind(c, name = "LLVMBuildSRem")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSRem
		end function LLVMBuildSRem

		function LLVMBuildFAdd(b, l, r, nm) bind(c, name = "LLVMBuildFAdd")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFAdd
		end function LLVMBuildFAdd

		function LLVMBuildFSub(b, l, r, nm) bind(c, name = "LLVMBuildFSub")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFSub
		end function LLVMBuildFSub

		function LLVMBuildFMul(b, l, r, nm) bind(c, name = "LLVMBuildFMul")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFMul
		end function LLVMBuildFMul

		function LLVMBuildFDiv(b, l, r, nm) bind(c, name = "LLVMBuildFDiv")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFDiv
		end function LLVMBuildFDiv

		function LLVMBuildFRem(b, l, r, nm) bind(c, name = "LLVMBuildFRem")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFRem
		end function LLVMBuildFRem

		function LLVMBuildAnd(b, l, r, nm) bind(c, name = "LLVMBuildAnd")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildAnd
		end function LLVMBuildAnd

		function LLVMBuildOr(b, l, r, nm) bind(c, name = "LLVMBuildOr")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildOr
		end function LLVMBuildOr

		function LLVMBuildXor(b, l, r, nm) bind(c, name = "LLVMBuildXor")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildXor
		end function LLVMBuildXor

		function LLVMBuildShl(b, l, r, nm) bind(c, name = "LLVMBuildShl")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildShl
		end function LLVMBuildShl

		function LLVMBuildAShr(b, l, r, nm) bind(c, name = "LLVMBuildAShr")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildAShr
		end function LLVMBuildAShr

		function LLVMBuildLShr(b, l, r, nm) bind(c, name = "LLVMBuildLShr")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, l, r
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildLShr
		end function LLVMBuildLShr

		function LLVMBuildNeg(b, v, nm) bind(c, name = "LLVMBuildNeg")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildNeg
		end function LLVMBuildNeg

		function LLVMBuildFNeg(b, v, nm) bind(c, name = "LLVMBuildFNeg")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFNeg
		end function LLVMBuildFNeg

		function LLVMBuildNot(b, v, nm) bind(c, name = "LLVMBuildNot")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildNot
		end function LLVMBuildNot

		function LLVMBuildSExt(b, v, ty, nm) bind(c, name = "LLVMBuildSExt")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSExt
		end function LLVMBuildSExt

		function LLVMBuildZExt(b, v, ty, nm) bind(c, name = "LLVMBuildZExt")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildZExt
		end function LLVMBuildZExt

		function LLVMBuildTrunc(b, v, ty, nm) bind(c, name = "LLVMBuildTrunc")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildTrunc
		end function LLVMBuildTrunc

		function LLVMBuildSIToFP(b, v, ty, nm) bind(c, name = "LLVMBuildSIToFP")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildSIToFP
		end function LLVMBuildSIToFP

		function LLVMBuildFPToSI(b, v, ty, nm) bind(c, name = "LLVMBuildFPToSI")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFPToSI
		end function LLVMBuildFPToSI

		function LLVMBuildFPExt(b, v, ty, nm) bind(c, name = "LLVMBuildFPExt")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFPExt
		end function LLVMBuildFPExt

		function LLVMBuildFPTrunc(b, v, ty, nm) bind(c, name = "LLVMBuildFPTrunc")
			import :: c_ptr, c_char
			type(c_ptr), value :: b, v, ty
			character(kind = c_char), intent(in) :: nm(*)
			type(c_ptr) :: LLVMBuildFPTrunc
		end function LLVMBuildFPTrunc

		!**** Target machine, optimization, and output

		! In src/c/llvm_shim.c, because LLVM has a different initializer for each
		! architecture.  Only linked if SYNTRAN_LLVM
		subroutine sy_llvm_init_native() bind(c, name = "sy_llvm_init_native")
		end subroutine sy_llvm_init_native

		function LLVMGetDefaultTargetTriple() bind(c, name = "LLVMGetDefaultTargetTriple")
			import :: c_ptr
			type(c_ptr) :: LLVMGetDefaultTargetTriple
		end function LLVMGetDefaultTargetTriple

		function LLVMGetHostCPUName() bind(c, name = "LLVMGetHostCPUName")
			import :: c_ptr
			type(c_ptr) :: LLVMGetHostCPUName
		end function LLVMGetHostCPUName

		function LLVMGetHostCPUFeatures() bind(c, name = "LLVMGetHostCPUFeatures")
			import :: c_ptr
			type(c_ptr) :: LLVMGetHostCPUFeatures
		end function LLVMGetHostCPUFeatures

		function LLVMGetTargetFromTriple(triple, target, msg) &
				bind(c, name = "LLVMGetTargetFromTriple")
			import :: c_ptr, c_char, c_int
			character(kind = c_char), intent(in) :: triple(*)
			type(c_ptr), intent(out) :: target, msg
			integer(c_int) :: LLVMGetTargetFromTriple
		end function LLVMGetTargetFromTriple

		function LLVMCreateTargetMachine(target, triple, cpu, features, level, reloc, model) &
				bind(c, name = "LLVMCreateTargetMachine")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: target
			character(kind = c_char), intent(in) :: triple(*), cpu(*), features(*)
			integer(c_int), value :: level, reloc, model
			type(c_ptr) :: LLVMCreateTargetMachine
		end function LLVMCreateTargetMachine

		subroutine LLVMDisposeTargetMachine(tm) bind(c, name = "LLVMDisposeTargetMachine")
			import :: c_ptr
			type(c_ptr), value :: tm
		end subroutine LLVMDisposeTargetMachine

		function LLVMCreateTargetDataLayout(tm) bind(c, name = "LLVMCreateTargetDataLayout")
			import :: c_ptr
			type(c_ptr), value :: tm
			type(c_ptr) :: LLVMCreateTargetDataLayout
		end function LLVMCreateTargetDataLayout

		subroutine LLVMDisposeTargetData(dl) bind(c, name = "LLVMDisposeTargetData")
			import :: c_ptr
			type(c_ptr), value :: dl
		end subroutine LLVMDisposeTargetData

		function LLVMTargetMachineEmitToFile(tm, m, filename, kind, msg) &
				bind(c, name = "LLVMTargetMachineEmitToFile")
			import :: c_ptr, c_char, c_int
			type(c_ptr), value :: tm, m
			character(kind = c_char), intent(in) :: filename(*)
			integer(c_int), value :: kind
			type(c_ptr), intent(out) :: msg
			integer(c_int) :: LLVMTargetMachineEmitToFile
		end function LLVMTargetMachineEmitToFile

		function LLVMCreatePassBuilderOptions() bind(c, name = "LLVMCreatePassBuilderOptions")
			import :: c_ptr
			type(c_ptr) :: LLVMCreatePassBuilderOptions
		end function LLVMCreatePassBuilderOptions

		subroutine LLVMDisposePassBuilderOptions(opts) &
				bind(c, name = "LLVMDisposePassBuilderOptions")
			import :: c_ptr
			type(c_ptr), value :: opts
		end subroutine LLVMDisposePassBuilderOptions

		! Returns an LLVMErrorRef, which is null on success
		function LLVMRunPasses(m, passes, tm, opts) bind(c, name = "LLVMRunPasses")
			import :: c_ptr, c_char
			type(c_ptr), value :: m, tm, opts
			character(kind = c_char), intent(in) :: passes(*)
			type(c_ptr) :: LLVMRunPasses
		end function LLVMRunPasses

		function LLVMGetErrorMessage(err) bind(c, name = "LLVMGetErrorMessage")
			import :: c_ptr
			type(c_ptr), value :: err
			type(c_ptr) :: LLVMGetErrorMessage
		end function LLVMGetErrorMessage

		function c_strlen(s) bind(c, name = "strlen")
			import :: c_ptr, c_size_t
			type(c_ptr), value :: s
			integer(c_size_t) :: c_strlen
		end function c_strlen

	end interface

contains

!===============================================================================

function c_to_f_string(p) result(s)

	! Copy a C string, e.g. an LLVM message, to a Fortran one.  A null pointer is
	! an empty string

	type(c_ptr), intent(in) :: p
	character(len = :), allocatable :: s

	!********

	character(kind = c_char), pointer :: chars(:)

	integer :: i, n

	if (.not. c_associated(p)) then
		s = ''
		return
	end if

	n = int(c_strlen(p))
	call c_f_pointer(p, chars, [n])

	allocate(character(len = n) :: s)
	do i = 1, n
		s(i:i) = chars(i)
	end do

end function c_to_f_string

!===============================================================================

end module syntran__llvm_c_m

!===============================================================================

