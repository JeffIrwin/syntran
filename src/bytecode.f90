
!===============================================================================

module syntran__bytecode_m

	use syntran__types_m

	implicit none

	!********

	! Opcode enum.  Uses a separate numbering space from the token/type/node-kind
	! enum in consts.f90 (which tops out at 121) to prevent accidental aliasing.

	integer, parameter :: &
		OP_LOAD_CONST       = 1002, &	! push consts(a) onto the operand stack (deep copy)
		OP_LOAD_GLOBAL      = 1003, &	! push state%vars%vals(a) (deep copy)
		OP_LOAD_LOCAL       = 1004, &	! push state%locs%vals(a) (deep copy)
		OP_STORE_GLOBAL     = 1005, &	! copy TOS into state%vars%vals(a), keep TOS
		OP_STORE_LOCAL      = 1006, &	! copy TOS into state%locs%vals(a), keep TOS
		OP_BINOP            = 1007, &	! pop right+left, compute with op-token a, push result
		OP_UNOP             = 1008, &	! pop operand, compute with op-token a, push result
		OP_POP              = 1009, &	! discard TOS
		OP_JUMP             = 1010, &	! unconditional jump: ip = a
		OP_JUMP_IF_FALSE    = 1011, &	! pop bool TOS; if false: ip = a, else continue
		OP_CALL             = 1012, &	! call user fn: a=fn_id, b=node_pool_idx (fn_call node)
		OP_RET              = 1013, &	! return from fn: TOS is return value
		OP_LOAD_REF_GLOBAL  = 1014, &	! move state%vars%vals(a) to stack (by-ref arg, pass 2)
		OP_LOAD_REF_LOCAL   = 1015, &	! move state%locs%vals(a) to stack (by-ref arg, pass 2)
		OP_INDEX            = 1016, &	! scalar subscript read: a=node_idx (name_expr w/ scalar subs)
		OP_SLICE            = 1017, &	! non-scalar subscript/slice read: a=node_idx
		OP_STORE_IDX        = 1018, &	! scalar subscript write: a=node_idx, b=op_kind; TOS=RHS
		OP_MAKE_STRUCT      = 1019, &	! M5: struct instance: a=node_idx (for struct_name+nmembers), members on stack
		OP_LOAD_MEMBER      = 1020, &	! M5: dot_expr read: a=node_idx; uses get_val
		OP_STORE_MEMBER     = 1021, &	! M5: dot member write: a=node_idx, b=op_kind; TOS=RHS; uses get_val+set_val
		OP_LOAD_MEMBER_TOS  = 1139, &	! M5: dot_expr read when root is a fn return: root on TOS, a=member-wrapper node_idx
		OP_CALL_INTR        = 1022, &	! M6: intrinsic call; native: a=intr_id b=argc (args on stack);
		                            	!   readln/close: a=intr_id b=1 c=(id_index*2+is_loc) for slot writeback
		OP_FOR_SETUP        = 1023, &	! M8: for-loop setup: a=node_idx; pushes iter onto for_iter stack
		OP_FOR_NEXT         = 1024, &	! M8: for-loop advance: a=L_pop; exhausted->jump (no pop), else write loop var
		OP_FOR_POP          = 1025, &	! M8: for-loop exit: pop for_iter stack (emitted at all loop exits)
		OP_NEW_ARRAY        = 1026, &	! M8: array construction: a=node_idx; calls eval_array_expr, pushes result
		OP_STORE_SLICE      = 1027, &	! M8: slice/complex LHS assign: a=node_idx; calls eval_assignment_expr
		OP_HALT             = 1028  	! M8: top-level return: exit VM loop, TOS is result

	!**** Typed scalar opcodes (Stage 2) -------------------------------------------
	! Each typed opcode operates in-place on TOS / TOS-1, avoiding value_move and
	! get_binary_op_kind overhead.  Only same-type scalar combinations are covered;
	! mixed types and array/string ops fall back to the generic OP_BINOP/UNOP.
	!
	! Arithmetic (arith): left=TOS-1, right=TOS; result written to TOS-1, len_--.
	! Same result type as both operands.
	integer, parameter :: &
		OP_ADD_I32 = 1029, OP_ADD_I64 = 1030, OP_ADD_F32 = 1031, OP_ADD_F64 = 1032, &
		OP_SUB_I32 = 1033, OP_SUB_I64 = 1034, OP_SUB_F32 = 1035, OP_SUB_F64 = 1036, &
		OP_MUL_I32 = 1037, OP_MUL_I64 = 1038, OP_MUL_F32 = 1039, OP_MUL_F64 = 1040, &
		OP_DIV_I32 = 1041, OP_DIV_I64 = 1042, OP_DIV_F32 = 1043, OP_DIV_F64 = 1044, &
		OP_MOD_I32 = 1045, OP_MOD_I64 = 1046, OP_MOD_F32 = 1047, OP_MOD_F64 = 1048

	! Comparisons: result type is bool_type; updates TOS-1%type = bool_type.
	integer, parameter :: &
		OP_LT_I32  = 1049, OP_LT_I64  = 1050, OP_LT_F32  = 1051, OP_LT_F64  = 1052, &
		OP_LE_I32  = 1053, OP_LE_I64  = 1054, OP_LE_F32  = 1055, OP_LE_F64  = 1056, &
		OP_GT_I32  = 1057, OP_GT_I64  = 1058, OP_GT_F32  = 1059, OP_GT_F64  = 1060, &
		OP_GE_I32  = 1061, OP_GE_I64  = 1062, OP_GE_F32  = 1063, OP_GE_F64  = 1064, &
		OP_EQ_I32  = 1065, OP_EQ_I64  = 1066, OP_EQ_F32  = 1067, OP_EQ_F64  = 1068, &
		OP_EQ_BOOL = 1069, &
		OP_NE_I32  = 1070, OP_NE_I64  = 1071, OP_NE_F32  = 1072, OP_NE_F64  = 1073, &
		OP_NE_BOOL = 1074

	! Bool binary: left=TOS-1, right=TOS; result written to TOS-1 (bool), len_--.
	integer, parameter :: &
		OP_AND_BOOL = 1075, &
		OP_OR_BOOL  = 1076

	! Unary: operand=TOS; result written to TOS in-place (no pop).
	integer, parameter :: &
		OP_NEG_I32  = 1077, OP_NEG_I64  = 1078, OP_NEG_F32 = 1079, OP_NEG_F64 = 1080, &
		OP_NOT_BOOL = 1081, &
		OP_BNOT_I32 = 1082, OP_BNOT_I64 = 1083

	! Typed scalar loads: push a scalar onto the stack without value_copy overhead.
	!   LOAD_CONST_BOOL:  a = 0 (false) or 1 (true)
	!   LOAD_CONST_I32:   a = i32 value directly (no const pool)
	!   LOAD_CONST_I64:   c = i64 value directly (no const pool)
	!   LOAD_CONST_F32:   a = transfer(f32, 0) — bit pattern as integer
	!   LOAD_CONST_F64:   c = transfer(f64, 0_8) — bit pattern as integer(8)
	!   LOAD_LOCAL/GLOBAL_*: a = slot index (same as generic LOAD_LOCAL/GLOBAL)
	integer, parameter :: &
		OP_LOAD_CONST_BOOL  = 1084, &
		OP_LOAD_CONST_I32   = 1085, OP_LOAD_CONST_I64  = 1086, &
		OP_LOAD_CONST_F32   = 1087, OP_LOAD_CONST_F64  = 1088, &
		OP_LOAD_LOCAL_BOOL  = 1089, &
		OP_LOAD_LOCAL_I32   = 1090, OP_LOAD_LOCAL_I64  = 1091, &
		OP_LOAD_LOCAL_F32   = 1092, OP_LOAD_LOCAL_F64  = 1093, &
		OP_LOAD_GLOBAL_BOOL = 1094, &
		OP_LOAD_GLOBAL_I32  = 1095, OP_LOAD_GLOBAL_I64 = 1096, &
		OP_LOAD_GLOBAL_F32  = 1097, OP_LOAD_GLOBAL_F64 = 1098

	! Typed scalar stores: write TOS into variable slot without assign_ overhead.
	! Keep TOS (consistent with generic OP_STORE_LOCAL/GLOBAL).
	! Sets both %type and %sca%*.  Safe because the compiler only emits these when
	! the static RHS type matches the variable type.
	integer, parameter :: &
		OP_STORE_LOCAL_BOOL  = 1099, &
		OP_STORE_LOCAL_I32   = 1100, OP_STORE_LOCAL_I64  = 1101, &
		OP_STORE_LOCAL_F32   = 1102, OP_STORE_LOCAL_F64  = 1103, &
		OP_STORE_GLOBAL_BOOL = 1104, &
		OP_STORE_GLOBAL_I32  = 1105, OP_STORE_GLOBAL_I64 = 1106, &
		OP_STORE_GLOBAL_F32  = 1107, OP_STORE_GLOBAL_F64 = 1108

	!**** Typed scalar opcodes (Stage 3) -------------------------------------------

	! Power: same as arithmetic opcodes — left=TOS-1, right=TOS; result=TOS-1, len_--.
	! Semantics match math_bin_pow.f90 same-type scalar cases.
	integer, parameter :: &
		OP_POW_I32 = 1109, OP_POW_I64 = 1110, &
		OP_POW_F32 = 1111, OP_POW_F64 = 1112

	! Mixed i32/i64 scalar arithmetic: result type is i64.
	! Naming: OP_<OP>_<LTYPE>_<RTYPE>.  TOS-1 type updated to i64_type.
	integer, parameter :: &
		OP_ADD_I32_I64 = 1113, OP_ADD_I64_I32 = 1114, &
		OP_SUB_I32_I64 = 1115, OP_SUB_I64_I32 = 1116, &
		OP_MUL_I32_I64 = 1117, OP_MUL_I64_I32 = 1118, &
		OP_DIV_I32_I64 = 1119, OP_DIV_I64_I32 = 1120, &
		OP_MOD_I32_I64 = 1121, OP_MOD_I64_I32 = 1122

	! Mixed i32/i64 scalar comparisons: result type is bool.
	! TOS-1 type updated to bool_type, len_--.
	integer, parameter :: &
		OP_LT_I32_I64  = 1123, OP_LT_I64_I32  = 1124, &
		OP_LE_I32_I64  = 1125, OP_LE_I64_I32  = 1126, &
		OP_GT_I32_I64  = 1127, OP_GT_I64_I32  = 1128, &
		OP_GE_I32_I64  = 1129, OP_GE_I64_I32  = 1130, &
		OP_EQ_I32_I64  = 1131, OP_EQ_I64_I32  = 1132, &
		OP_NE_I32_I64  = 1133, OP_NE_I64_I32  = 1134

	! Same-type array binop: a = op_kind (token), b = element type.
	! Dispatches to do_array_binop_typed for i32/i64/f32/f64 arrays.
	! Arithmetic: result type is array of same elem type.
	! Comparisons: result type is bool array.
	integer, parameter :: &
		OP_ARR_BINOP = 1135

	!**** M9: native scalar array indexing ----------------------------------------
	!
	! OP_INDEX_NAT: fast array element read without syntax_eval per subscript.
	!   Compiler emits one typed compile_node per subscript (pushing each index
	!   onto the operand stack), then emits this opcode.
	!   Instruction fields:
	!     a = id_index (slot index of the array variable)
	!     b = nsub     (number of subscripts = array rank for this access)
	!     c = is_local (0 = global, 1 = local)
	!   Stack before:  [sub_1][sub_2]...[sub_nsub]
	!   Stack after:   [element_value]
	!   Only emitted when the result type is a numeric/bool scalar (guard in
	!   index_native_ok).  String, struct, and slice accesses fall back to
	!   OP_INDEX / OP_SLICE.
	!
	! OP_STORE_IDX_NAT: fast array element write (plain '=' only).
	!   Compiler emits one compile_node per subscript then the RHS.
	!   Instruction fields:
	!     a = id_index
	!     b = nsub
	!     c = is_local (0 = global, 1 = local)
	!   Stack before:  [sub_1]...[sub_nsub][rhs]
	!   Stack after:   [rhs]   (leaves the stored value on top, matching OP_STORE_IDX)
	!   Only emitted when rhs is a numeric/bool scalar and op is '='
	!   (guard in store_idx_native_ok).  String/struct writes and compound ops
	!   (a[i]+=x) fall back to OP_STORE_IDX.
	!
	! OP_COMPOUND_IDX_NAT: compound array element op (a[i] += rhs, etc.) without
	!   subscript_eval.  Like OP_STORE_IDX_NAT but also reads current element,
	!   applies op, and writes back.
	!   Instruction fields:
	!     a = id_index
	!     b = nsub
	!     c = op_kind * 2 + is_local  (op_kind = compound assignment token)
	!   Stack before:  [sub_1]...[sub_nsub][rhs]
	!   Stack after:   [rhs]
	!
	! OP_STR_INDEX_NAT: scalar string single-character read without subscript_eval.
	!   Compiler pushes the subscript expression, then emits this opcode.
	!   Instruction fields:
	!     a = id_index  (string variable slot)
	!     c = is_local  (0 = global, 1 = local)
	!   Stack before:  [subscript]
	!   Stack after:   [1-char string]
	!
	! OP_STR_SLICE_NAT: scalar string substring (bound_array range) without
	!   eval_name_expr.  Compiler pushes lbound then ubound expressions.
	!   Instruction fields:
	!     a = id_index  (string variable slot)
	!     c = is_local  (0 = global, 1 = local)
	!   Stack before:  [lbound][ubound]
	!   Stack after:   [substring]
	integer, parameter :: &
		OP_INDEX_NAT        = 1136, &
		OP_STORE_IDX_NAT    = 1137, &
		OP_SUBSCRIPT_TOS    = 1138, &	! pop TOS (fn return val), apply subscripts from nodes(a), push result
		OP_COMPOUND_IDX_NAT = 1140, &
		OP_STR_INDEX_NAT    = 1141, &
		OP_STR_SLICE_NAT    = 1142

	!**** M6: intrinsic function ids (match order in eval_fn_call_intr / declare_intr_fns)

	! Math
	integer, parameter :: &
		INTR_EXP_F32      =  1, INTR_EXP_F64      =  2, &
		INTR_EXP_F32_ARR  =  3, INTR_EXP_F64_ARR  =  4, &
		INTR_LOG_F32      =  5, INTR_LOG_F64      =  6, &
		INTR_LOG_F32_ARR  =  7, INTR_LOG_F64_ARR  =  8, &
		INTR_LOG10_F32    =  9, INTR_LOG10_F64    = 10, &
		INTR_LOG10_F32_ARR= 11, INTR_LOG10_F64_ARR= 12, &
		INTR_LOG2_F32     = 13, INTR_LOG2_F64     = 14, &
		INTR_LOG2_F32_ARR = 15, INTR_LOG2_F64_ARR = 16, &
		INTR_SQRT_F32     = 17, INTR_SQRT_F64     = 18, &
		INTR_SQRT_F32_ARR = 19, INTR_SQRT_F64_ARR = 20, &
		INTR_ABS_F32      = 21, INTR_ABS_F64      = 22, &
		INTR_ABS_F32_ARR  = 23, INTR_ABS_F64_ARR  = 24, &
		INTR_ABS_I32      = 25, INTR_ABS_I64      = 26, &
		INTR_ABS_I32_ARR  = 27, INTR_ABS_I64_ARR  = 28

	! Trig
	integer, parameter :: &
		INTR_COS_F32      = 29, INTR_COS_F64      = 30, &
		INTR_COS_F32_ARR  = 31, INTR_COS_F64_ARR  = 32, &
		INTR_SIN_F32      = 33, INTR_SIN_F64      = 34, &
		INTR_SIN_F32_ARR  = 35, INTR_SIN_F64_ARR  = 36, &
		INTR_TAN_F32      = 37, INTR_TAN_F64      = 38, &
		INTR_TAN_F32_ARR  = 39, INTR_TAN_F64_ARR  = 40, &
		INTR_COSD_F32     = 41, INTR_COSD_F64     = 42, &
		INTR_COSD_F32_ARR = 43, INTR_COSD_F64_ARR = 44, &
		INTR_SIND_F32     = 45, INTR_SIND_F64     = 46, &
		INTR_SIND_F32_ARR = 47, INTR_SIND_F64_ARR = 48, &
		INTR_TAND_F32     = 49, INTR_TAND_F64     = 50, &
		INTR_TAND_F32_ARR = 51, INTR_TAND_F64_ARR = 52, &
		INTR_ACOS_F32     = 53, INTR_ACOS_F64     = 54, &
		INTR_ACOS_F32_ARR = 55, INTR_ACOS_F64_ARR = 56, &
		INTR_ASIN_F32     = 57, INTR_ASIN_F64     = 58, &
		INTR_ASIN_F32_ARR = 59, INTR_ASIN_F64_ARR = 60, &
		INTR_ATAN_F32     = 61, INTR_ATAN_F64     = 62, &
		INTR_ATAN_F32_ARR = 63, INTR_ATAN_F64_ARR = 64, &
		INTR_ACOSD_F32    = 65, INTR_ACOSD_F64    = 66, &
		INTR_ACOSD_F32_ARR= 67, INTR_ACOSD_F64_ARR= 68, &
		INTR_ASIND_F32    = 69, INTR_ASIND_F64    = 70, &
		INTR_ASIND_F32_ARR= 71, INTR_ASIND_F64_ARR= 72, &
		INTR_ATAND_F32    = 73, INTR_ATAND_F64    = 74, &
		INTR_ATAND_F32_ARR= 75, INTR_ATAND_F64_ARR= 76

	! Minmax
	integer, parameter :: &
		INTR_MIN_I32      = 77, INTR_MIN_I64      = 78, &
		INTR_MIN_F32      = 79, INTR_MIN_F64      = 80, &
		INTR_MAX_I32      = 81, INTR_MAX_I64      = 82, &
		INTR_MAX_F32      = 83, INTR_MAX_F64      = 84

	! String / conversion
	integer, parameter :: &
		INTR_PRINTLN      = 85, INTR_STR          = 86, &
		INTR_LEN          = 87, INTR_REPEAT       = 88, &
		INTR_PARSE_I32    = 89, INTR_PARSE_I64    = 90, &
		INTR_PARSE_F32    = 91, INTR_PARSE_F64    = 92, &
		INTR_CHAR         = 93, &
		INTR_I32_SCA      = 94, INTR_I32_ARR      = 95, &
		INTR_I64_SCA      = 96, INTR_I64_ARR      = 97

	! I/O
	integer, parameter :: &
		INTR_OPEN         = 98, &
		INTR_READLN       = 99,  &  ! CALL_INTR_NODE fallback: needs writeback
		INTR_WRITELN      = 100, &
		INTR_EOF          = 101, &
		INTR_CLOSE        = 102     ! CALL_INTR_NODE fallback: needs writeback

	! Misc + reduction
	integer, parameter :: &
		INTR_EXIT         = 103, &
		INTR_SIZE         = 104, INTR_COUNT        = 105, &
		INTR_MINVAL_I32   = 106, INTR_MINVAL_I64   = 107, &
		INTR_MINVAL_F32   = 108, INTR_MINVAL_F64   = 109, &
		INTR_MAXVAL_I32   = 110, INTR_MAXVAL_I64   = 111, &
		INTR_MAXVAL_F32   = 112, INTR_MAXVAL_F64   = 113, &
		INTR_SUM_I32      = 114, INTR_SUM_I64      = 115, &
		INTR_SUM_F32      = 116, INTR_SUM_F64      = 117, &
		INTR_PRODUCT_I32  = 118, INTR_PRODUCT_I64  = 119, &
		INTR_PRODUCT_F32  = 120, INTR_PRODUCT_F64  = 121, &
		INTR_NORM2_F32    = 122, INTR_NORM2_F64    = 123, &
		INTR_DOT_F32      = 124, INTR_DOT_F64      = 125, &
		INTR_DOT_I32      = 126, INTR_DOT_I64      = 127, &
		INTR_ALL          = 128, INTR_ANY          = 129, &
		INTR_ARGS         = 130, &
		INTR_RESHAPE      = 131, &
		INTR_TRANSPOSE    = 132, &
		INTR_SHAPE        = 133

	!********

	! A single bytecode instruction.  Kept as a plain POD record (no allocatable
	! members) so that arrays of instr_t can be grown with plain array assignment
	! or move_alloc without triggering the deep-copy machinery.
	!
	! Fields:
	!   op  - opcode
	!   a,b - integer operands (slot index, const index, jump target, etc.)
	!   c   - wide integer operand (e.g. element count)

	type instr_t
		integer :: op
		integer :: a = 0, b = 0
		integer(kind = 8) :: c = 0
	end type instr_t

	!********

	! A compiled program.  All function bodies and the top-level code are
	! concatenated into a single flat code(:) array; per-function entry offsets
	! are stored in fn_entry(:).
	!
	! The constant pool consts(:) holds literal values referenced by LOAD_CONST.
	!
	! The node pool nodes(:) holds AST subtrees referenced by opcodes that still
	! delegate partial evaluation to the AST layer: OP_INDEX, OP_SLICE,
	! OP_STORE_IDX, OP_MAKE_STRUCT, OP_LOAD_MEMBER, OP_STORE_MEMBER,
	! OP_FOR_SETUP, OP_NEW_ARRAY, OP_STORE_SLICE, and OP_CALL.

	type program_t

		type(instr_t), allocatable :: code(:)
		integer :: len_ = 0, cap = 0

		type(value_t), allocatable :: consts(:)
		integer :: nconsts = 0

		! AST node pool for opcodes that delegate to the AST layer
		type(syntax_node_t), allocatable :: nodes(:)
		integer :: nnodes = 0

		integer, allocatable :: fn_entry(:)
		integer, allocatable :: fn_num_locs(:)
		integer :: entry_main = 1

	end type program_t

	!********

	! Per-compilation state threaded through the recursive compiler.
	! Using a derived type (rather than module variables) makes the compiler
	! reentrant: each compile_tree() call constructs its own compiler_state_t
	! and passes it explicitly through compile_node/compile_module_fns.

	type compiler_state_t

		! --- loop break/continue backpatching ---
		!
		! loop_depth is incremented when entering a natively-compiled while/for
		! loop and decremented on exit.  continue_target(d) is the instruction
		! index to jump to for a `continue` inside loop depth d.  break fixups
		! inside loops are collected and backpatched after the loop body.
		!
		! All arrays are growable (no hard limit) for parity with the AST walker.
		! INIT_LOOP_DEPTH / INIT_BREAK_FIXUPS are starting capacities only;
		! arrays double on demand.

		integer :: loop_depth = 0
		integer, allocatable :: continue_target(:)

		integer, allocatable :: break_fixup_ips(:)
		integer, allocatable :: break_fixup_depths(:)
		integer :: nbreak_fixups = 0

		! --- Block-level break context ---
		!
		! In Syntran, `break` can exit a plain block statement (not just loops).
		! When loop_depth == 0 and this is the outermost natively-compiled block,
		! `break` records a fixup that gets patched to the block's end.

		logical :: in_block_break_ctx = .false.
		integer, allocatable :: block_break_ips(:)
		integer :: nblock_break_fixups = 0

		! Set to true while compiling a user function body; used to distinguish
		! a function-level return_statement (emit OP_RET) from a top-level one
		! (emit OP_HALT).
		logical :: in_fn_body = .false.

	end type compiler_state_t

!===============================================================================

contains

!===============================================================================

function new_compiler_state() result(cs)

	! Construct a freshly-initialised compiler_state_t for one compile_tree()
	! call.  All growable arrays are allocated at their initial capacities.

	type(compiler_state_t) :: cs

	!*******

	integer, parameter :: INIT_LOOP_DEPTH   = 64
	integer, parameter :: INIT_BREAK_FIXUPS = 256

	cs%loop_depth          = 0
	cs%nbreak_fixups       = 0
	cs%in_block_break_ctx  = .false.
	cs%nblock_break_fixups = 0
	cs%in_fn_body          = .false.

	allocate(cs%continue_target(INIT_LOOP_DEPTH))
	allocate(cs%break_fixup_ips(INIT_BREAK_FIXUPS))
	allocate(cs%break_fixup_depths(INIT_BREAK_FIXUPS))
	allocate(cs%block_break_ips(INIT_BREAK_FIXUPS))

end function new_compiler_state

!===============================================================================

function add_const(prog, val) result(idx)

	! Add a value to the constant pool and return its 1-based index.
	! Grows the pool using a single copy into tmp + move_alloc (one deep-copy
	! pass instead of two).  move_alloc is safe here: it transfers the outer
	! array descriptor; the elements' allocatable members stay in place.

	type(program_t), intent(inout) :: prog
	type(value_t), intent(in) :: val
	integer :: idx

	!*******

	type(value_t), allocatable :: tmp(:)
	integer :: i, new_cap

	integer, parameter :: INIT_CONST_CAP = 16

	prog%nconsts = prog%nconsts + 1
	idx = prog%nconsts

	if (.not. allocated(prog%consts)) then
		allocate(prog%consts(INIT_CONST_CAP))

	else if (prog%nconsts > size(prog%consts)) then
		new_cap = 2 * prog%nconsts
		allocate(tmp(new_cap))
		do i = 1, prog%nconsts - 1
			tmp(i) = prog%consts(i)
		end do
		call move_alloc(tmp, prog%consts)
	end if

	prog%consts(idx) = val

end function add_const

!===============================================================================

function new_program() result(prog)

	type(program_t) :: prog

	!*******

	integer, parameter :: INIT_CAP = 16

	prog%cap     = INIT_CAP
	prog%len_    = 0
	prog%nconsts = 0
	prog%nnodes  = 0
	prog%entry_main = 1

	allocate(prog%code(INIT_CAP))

end function new_program

!===============================================================================

subroutine emit(prog, op, a, b, c)

	! Append one instruction to prog%code, growing the array if needed.
	! instr_t has no allocatable members so a simple array-section copy + move_alloc
	! is safe.

	type(program_t), intent(inout) :: prog
	integer, intent(in) :: op
	integer, intent(in), optional :: a, b
	integer(kind = 8), intent(in), optional :: c

	!*******

	type(instr_t), allocatable :: tmp(:)

	prog%len_ = prog%len_ + 1

	if (prog%len_ > prog%cap) then
		prog%cap = 2 * prog%len_
		allocate(tmp(prog%cap))
		tmp(1 : prog%len_ - 1) = prog%code(1 : prog%len_ - 1)
		call move_alloc(tmp, prog%code)
	end if

	prog%code(prog%len_)%op = op
	prog%code(prog%len_)%a  = 0
	prog%code(prog%len_)%b  = 0
	prog%code(prog%len_)%c  = 0_8
	if (present(a)) prog%code(prog%len_)%a = a
	if (present(b)) prog%code(prog%len_)%b = b
	if (present(c)) prog%code(prog%len_)%c = c

end subroutine emit

!===============================================================================

function add_node(prog, node) result(idx)

	! Store an AST node in the program's node pool and return its index.
	!
	! Grows the pool using a single copy into tmp + move_alloc (one deep-copy
	! pass instead of two).  move_alloc is safe here: it transfers the outer
	! array descriptor; the elements' allocatable members stay in place.

	type(program_t), intent(inout) :: prog
	type(syntax_node_t), intent(in) :: node
	integer :: idx

	!*******

	type(syntax_node_t), allocatable :: tmp(:)
	integer :: i, new_cap

	integer, parameter :: INIT_NODE_CAP = 8

	prog%nnodes = prog%nnodes + 1
	idx = prog%nnodes

	if (.not. allocated(prog%nodes)) then
		allocate(prog%nodes(INIT_NODE_CAP))

	else if (prog%nnodes > size(prog%nodes)) then
		new_cap = 2 * prog%nnodes
		allocate(tmp(new_cap))
		do i = 1, prog%nnodes - 1
			tmp(i) = prog%nodes(i)
		end do
		call move_alloc(tmp, prog%nodes)
	end if

	prog%nodes(idx) = node

end function add_node

!===============================================================================

subroutine patch_jump(prog, ip, tgt)

	! Backpatch the jump target (field `a`) of a previously emitted
	! OP_JUMP or OP_JUMP_IF_FALSE instruction at position ip.

	type(program_t), intent(inout) :: prog
	integer, intent(in) :: ip, tgt

	prog%code(ip)%a = tgt

end subroutine patch_jump

!===============================================================================

pure integer function intr_id_from_name(name) result(id)

	! Map a mangled intrinsic function name to its INTR_* enum id.
	! Returns 0 for unknown names (should not happen after successful parsing).

	character(len = *), intent(in) :: name

	select case (name)
	! Math
	case ("0exp_f32");       id = INTR_EXP_F32
	case ("0exp_f64");       id = INTR_EXP_F64
	case ("0exp_f32_arr");   id = INTR_EXP_F32_ARR
	case ("0exp_f64_arr");   id = INTR_EXP_F64_ARR
	case ("0log_f32");       id = INTR_LOG_F32
	case ("0log_f64");       id = INTR_LOG_F64
	case ("0log_f32_arr");   id = INTR_LOG_F32_ARR
	case ("0log_f64_arr");   id = INTR_LOG_F64_ARR
	case ("0log10_f32");     id = INTR_LOG10_F32
	case ("0log10_f64");     id = INTR_LOG10_F64
	case ("0log10_f32_arr"); id = INTR_LOG10_F32_ARR
	case ("0log10_f64_arr"); id = INTR_LOG10_F64_ARR
	case ("0log2_f32");      id = INTR_LOG2_F32
	case ("0log2_f64");      id = INTR_LOG2_F64
	case ("0log2_f32_arr");  id = INTR_LOG2_F32_ARR
	case ("0log2_f64_arr");  id = INTR_LOG2_F64_ARR
	case ("0sqrt_f32");      id = INTR_SQRT_F32
	case ("0sqrt_f64");      id = INTR_SQRT_F64
	case ("0sqrt_f32_arr");  id = INTR_SQRT_F32_ARR
	case ("0sqrt_f64_arr");  id = INTR_SQRT_F64_ARR
	case ("0abs_f32");       id = INTR_ABS_F32
	case ("0abs_f64");       id = INTR_ABS_F64
	case ("0abs_f32_arr");   id = INTR_ABS_F32_ARR
	case ("0abs_f64_arr");   id = INTR_ABS_F64_ARR
	case ("0abs_i32");       id = INTR_ABS_I32
	case ("0abs_i64");       id = INTR_ABS_I64
	case ("0abs_i32_arr");   id = INTR_ABS_I32_ARR
	case ("0abs_i64_arr");   id = INTR_ABS_I64_ARR
	! Trig
	case ("0cos_f32");       id = INTR_COS_F32
	case ("0cos_f64");       id = INTR_COS_F64
	case ("0cos_f32_arr");   id = INTR_COS_F32_ARR
	case ("0cos_f64_arr");   id = INTR_COS_F64_ARR
	case ("0sin_f32");       id = INTR_SIN_F32
	case ("0sin_f64");       id = INTR_SIN_F64
	case ("0sin_f32_arr");   id = INTR_SIN_F32_ARR
	case ("0sin_f64_arr");   id = INTR_SIN_F64_ARR
	case ("0tan_f32");       id = INTR_TAN_F32
	case ("0tan_f64");       id = INTR_TAN_F64
	case ("0tan_f32_arr");   id = INTR_TAN_F32_ARR
	case ("0tan_f64_arr");   id = INTR_TAN_F64_ARR
	case ("0cosd_f32");      id = INTR_COSD_F32
	case ("0cosd_f64");      id = INTR_COSD_F64
	case ("0cosd_f32_arr");  id = INTR_COSD_F32_ARR
	case ("0cosd_f64_arr");  id = INTR_COSD_F64_ARR
	case ("0sind_f32");      id = INTR_SIND_F32
	case ("0sind_f64");      id = INTR_SIND_F64
	case ("0sind_f32_arr");  id = INTR_SIND_F32_ARR
	case ("0sind_f64_arr");  id = INTR_SIND_F64_ARR
	case ("0tand_f32");      id = INTR_TAND_F32
	case ("0tand_f64");      id = INTR_TAND_F64
	case ("0tand_f32_arr");  id = INTR_TAND_F32_ARR
	case ("0tand_f64_arr");  id = INTR_TAND_F64_ARR
	case ("0acos_f32");      id = INTR_ACOS_F32
	case ("0acos_f64");      id = INTR_ACOS_F64
	case ("0acos_f32_arr");  id = INTR_ACOS_F32_ARR
	case ("0acos_f64_arr");  id = INTR_ACOS_F64_ARR
	case ("0asin_f32");      id = INTR_ASIN_F32
	case ("0asin_f64");      id = INTR_ASIN_F64
	case ("0asin_f32_arr");  id = INTR_ASIN_F32_ARR
	case ("0asin_f64_arr");  id = INTR_ASIN_F64_ARR
	case ("0atan_f32");      id = INTR_ATAN_F32
	case ("0atan_f64");      id = INTR_ATAN_F64
	case ("0atan_f32_arr");  id = INTR_ATAN_F32_ARR
	case ("0atan_f64_arr");  id = INTR_ATAN_F64_ARR
	case ("0acosd_f32");     id = INTR_ACOSD_F32
	case ("0acosd_f64");     id = INTR_ACOSD_F64
	case ("0acosd_f32_arr"); id = INTR_ACOSD_F32_ARR
	case ("0acosd_f64_arr"); id = INTR_ACOSD_F64_ARR
	case ("0asind_f32");     id = INTR_ASIND_F32
	case ("0asind_f64");     id = INTR_ASIND_F64
	case ("0asind_f32_arr"); id = INTR_ASIND_F32_ARR
	case ("0asind_f64_arr"); id = INTR_ASIND_F64_ARR
	case ("0atand_f32");     id = INTR_ATAND_F32
	case ("0atand_f64");     id = INTR_ATAND_F64
	case ("0atand_f32_arr"); id = INTR_ATAND_F32_ARR
	case ("0atand_f64_arr"); id = INTR_ATAND_F64_ARR
	! Minmax
	case ("0min_i32");       id = INTR_MIN_I32
	case ("0min_i64");       id = INTR_MIN_I64
	case ("0min_f32");       id = INTR_MIN_F32
	case ("0min_f64");       id = INTR_MIN_F64
	case ("0max_i32");       id = INTR_MAX_I32
	case ("0max_i64");       id = INTR_MAX_I64
	case ("0max_f32");       id = INTR_MAX_F32
	case ("0max_f64");       id = INTR_MAX_F64
	! String / conversion
	case ("println");        id = INTR_PRINTLN
	case ("str");            id = INTR_STR
	case ("len");            id = INTR_LEN
	case ("repeat");         id = INTR_REPEAT
	case ("parse_i32");      id = INTR_PARSE_I32
	case ("parse_i64");      id = INTR_PARSE_I64
	case ("parse_f32");      id = INTR_PARSE_F32
	case ("parse_f64");      id = INTR_PARSE_F64
	case ("char");           id = INTR_CHAR
	case ("0i32_sca");       id = INTR_I32_SCA
	case ("0i32_arr");       id = INTR_I32_ARR
	case ("0i64_sca");       id = INTR_I64_SCA
	case ("0i64_arr");       id = INTR_I64_ARR
	! I/O
	case ("open");           id = INTR_OPEN
	case ("readln");         id = INTR_READLN
	case ("writeln");        id = INTR_WRITELN
	case ("eof");            id = INTR_EOF
	case ("close");          id = INTR_CLOSE
	! Misc + reduction
	case ("exit");           id = INTR_EXIT
	case ("size");           id = INTR_SIZE
	case ("count");          id = INTR_COUNT
	case ("0minval_i32");    id = INTR_MINVAL_I32
	case ("0minval_i64");    id = INTR_MINVAL_I64
	case ("0minval_f32");    id = INTR_MINVAL_F32
	case ("0minval_f64");    id = INTR_MINVAL_F64
	case ("0maxval_i32");    id = INTR_MAXVAL_I32
	case ("0maxval_i64");    id = INTR_MAXVAL_I64
	case ("0maxval_f32");    id = INTR_MAXVAL_F32
	case ("0maxval_f64");    id = INTR_MAXVAL_F64
	case ("0sum_i32");       id = INTR_SUM_I32
	case ("0sum_i64");       id = INTR_SUM_I64
	case ("0sum_f32");       id = INTR_SUM_F32
	case ("0sum_f64");       id = INTR_SUM_F64
	case ("0product_i32");   id = INTR_PRODUCT_I32
	case ("0product_i64");   id = INTR_PRODUCT_I64
	case ("0product_f32");   id = INTR_PRODUCT_F32
	case ("0product_f64");   id = INTR_PRODUCT_F64
	case ("0norm2_f32");     id = INTR_NORM2_F32
	case ("0norm2_f64");     id = INTR_NORM2_F64
	case ("0dot_f32");       id = INTR_DOT_F32
	case ("0dot_f64");       id = INTR_DOT_F64
	case ("0dot_i32");       id = INTR_DOT_I32
	case ("0dot_i64");       id = INTR_DOT_I64
	case ("all");            id = INTR_ALL
	case ("any");            id = INTR_ANY
	case ("args");           id = INTR_ARGS
	case ("reshape");        id = INTR_RESHAPE
	case ("transpose");      id = INTR_TRANSPOSE
	case ("shape");          id = INTR_SHAPE
	case default;            id = 0
	end select

end function intr_id_from_name

!===============================================================================

pure integer function binop_typed_opcode(op_kind, ltype, rtype) result(op)

	! Return the typed scalar opcode for a binary operation on two same-type
	! scalar operands, or 0 if not specializable (mixed types, arrays, etc.).

	integer, intent(in) :: op_kind, ltype, rtype

	op = 0

	! --- Fast path: same-type scalars ---
	if (ltype == rtype) then
		select case (op_kind)
		case (plus_token)
			select case (ltype)
			case (i32_type); op = OP_ADD_I32
			case (i64_type); op = OP_ADD_I64
			case (f32_type); op = OP_ADD_F32
			case (f64_type); op = OP_ADD_F64
			end select
		case (minus_token)
			select case (ltype)
			case (i32_type); op = OP_SUB_I32
			case (i64_type); op = OP_SUB_I64
			case (f32_type); op = OP_SUB_F32
			case (f64_type); op = OP_SUB_F64
			end select
		case (star_token)
			select case (ltype)
			case (i32_type); op = OP_MUL_I32
			case (i64_type); op = OP_MUL_I64
			case (f32_type); op = OP_MUL_F32
			case (f64_type); op = OP_MUL_F64
			end select
		case (slash_token)
			select case (ltype)
			case (i32_type); op = OP_DIV_I32
			case (i64_type); op = OP_DIV_I64
			case (f32_type); op = OP_DIV_F32
			case (f64_type); op = OP_DIV_F64
			end select
		case (percent_token)
			select case (ltype)
			case (i32_type); op = OP_MOD_I32
			case (i64_type); op = OP_MOD_I64
			case (f32_type); op = OP_MOD_F32
			case (f64_type); op = OP_MOD_F64
			end select
		case (sstar_token)
			select case (ltype)
			case (i32_type); op = OP_POW_I32
			case (i64_type); op = OP_POW_I64
			case (f32_type); op = OP_POW_F32
			case (f64_type); op = OP_POW_F64
			end select
		case (less_token)
			select case (ltype)
			case (i32_type); op = OP_LT_I32
			case (i64_type); op = OP_LT_I64
			case (f32_type); op = OP_LT_F32
			case (f64_type); op = OP_LT_F64
			end select
		case (less_equals_token)
			select case (ltype)
			case (i32_type); op = OP_LE_I32
			case (i64_type); op = OP_LE_I64
			case (f32_type); op = OP_LE_F32
			case (f64_type); op = OP_LE_F64
			end select
		case (greater_token)
			select case (ltype)
			case (i32_type); op = OP_GT_I32
			case (i64_type); op = OP_GT_I64
			case (f32_type); op = OP_GT_F32
			case (f64_type); op = OP_GT_F64
			end select
		case (greater_equals_token)
			select case (ltype)
			case (i32_type); op = OP_GE_I32
			case (i64_type); op = OP_GE_I64
			case (f32_type); op = OP_GE_F32
			case (f64_type); op = OP_GE_F64
			end select
		case (eequals_token)
			select case (ltype)
			case (i32_type); op = OP_EQ_I32
			case (i64_type); op = OP_EQ_I64
			case (f32_type); op = OP_EQ_F32
			case (f64_type); op = OP_EQ_F64
			case (bool_type); op = OP_EQ_BOOL
			end select
		case (bang_equals_token)
			select case (ltype)
			case (i32_type); op = OP_NE_I32
			case (i64_type); op = OP_NE_I64
			case (f32_type); op = OP_NE_F32
			case (f64_type); op = OP_NE_F64
			case (bool_type); op = OP_NE_BOOL
			end select
		case (and_keyword)
			if (ltype == bool_type) op = OP_AND_BOOL
		case (or_keyword)
			if (ltype == bool_type) op = OP_OR_BOOL
		end select
		return
	end if

	! --- Mixed i32/i64: result is i64 (arithmetic) or bool (comparisons) ---
	if (.not. ((ltype == i32_type .and. rtype == i64_type) .or. &
	           (ltype == i64_type .and. rtype == i32_type))) return

	select case (op_kind)
	case (plus_token)
		if (ltype == i32_type) then; op = OP_ADD_I32_I64
		else;                         op = OP_ADD_I64_I32; end if
	case (minus_token)
		if (ltype == i32_type) then; op = OP_SUB_I32_I64
		else;                         op = OP_SUB_I64_I32; end if
	case (star_token)
		if (ltype == i32_type) then; op = OP_MUL_I32_I64
		else;                         op = OP_MUL_I64_I32; end if
	case (slash_token)
		if (ltype == i32_type) then; op = OP_DIV_I32_I64
		else;                         op = OP_DIV_I64_I32; end if
	case (percent_token)
		if (ltype == i32_type) then; op = OP_MOD_I32_I64
		else;                         op = OP_MOD_I64_I32; end if
	case (less_token)
		if (ltype == i32_type) then; op = OP_LT_I32_I64
		else;                         op = OP_LT_I64_I32; end if
	case (less_equals_token)
		if (ltype == i32_type) then; op = OP_LE_I32_I64
		else;                         op = OP_LE_I64_I32; end if
	case (greater_token)
		if (ltype == i32_type) then; op = OP_GT_I32_I64
		else;                         op = OP_GT_I64_I32; end if
	case (greater_equals_token)
		if (ltype == i32_type) then; op = OP_GE_I32_I64
		else;                         op = OP_GE_I64_I32; end if
	case (eequals_token)
		if (ltype == i32_type) then; op = OP_EQ_I32_I64
		else;                         op = OP_EQ_I64_I32; end if
	case (bang_equals_token)
		if (ltype == i32_type) then; op = OP_NE_I32_I64
		else;                         op = OP_NE_I64_I32; end if
	end select

end function binop_typed_opcode

!===============================================================================

pure integer function unop_typed_opcode(op_kind, type_) result(op)

	! Return the typed scalar opcode for a unary operation, or 0 if not
	! specializable.

	integer, intent(in) :: op_kind, type_

	op = 0
	select case (op_kind)
	case (minus_token)
		select case (type_)
		case (i32_type); op = OP_NEG_I32
		case (i64_type); op = OP_NEG_I64
		case (f32_type); op = OP_NEG_F32
		case (f64_type); op = OP_NEG_F64
		end select
	case (not_keyword)
		if (type_ == bool_type) op = OP_NOT_BOOL
	case (bang_token)
		select case (type_)
		case (i32_type); op = OP_BNOT_I32
		case (i64_type); op = OP_BNOT_I64
		end select
	end select

end function unop_typed_opcode

!===============================================================================

pure integer function arr_binop_typed_opcode(op_kind, elem_type) result(op)

	! Return OP_ARR_BINOP when op_kind is a supported binary operation on
	! same-type numeric arrays (i32/i64/f32/f64), or 0 to fall back to OP_BINOP.
	! The caller is responsible for checking that both operands are array_type
	! and that their element types are equal.

	integer, intent(in) :: op_kind, elem_type

	op = 0

	! Only numeric element types
	select case (elem_type)
	case (i32_type, i64_type, f32_type, f64_type); continue
	case default; return
	end select

	! Only the operators handled by do_array_binop_typed in the VM
	select case (op_kind)
	case (plus_token, minus_token, star_token, slash_token, percent_token, &
	      less_token, less_equals_token, greater_token, greater_equals_token, &
	      eequals_token, bang_equals_token)
		op = OP_ARR_BINOP
	end select

end function arr_binop_typed_opcode

!===============================================================================

pure logical function index_native_ok(node) result(ok)

	! Return .true. when a name_expr node with all-scalar subscripts can be
	! lowered to OP_INDEX_NAT (fast inline element read) instead of OP_INDEX.
	! Requires:
	!   - subscripts are allocated and all scalar_sub
	!   - result element type is a numeric/bool scalar (not str, struct, array)

	type(syntax_node_t), intent(in) :: node

	ok = .false.
	if (.not. allocated(node%lsubscripts)) return
	if (.not. all(node%lsubscripts%sub_kind == scalar_sub)) return

	select case (node%val%type)
	case (bool_type, i32_type, i64_type, f32_type, f64_type)
		ok = .true.
	end select

end function index_native_ok

!===============================================================================

pure logical function store_idx_native_ok(node) result(ok)

	! Return .true. when an assignment_expr node with all-scalar subscripts and
	! a plain '=' op can be lowered to OP_STORE_IDX_NAT (fast inline element
	! write) instead of OP_STORE_IDX.
	! Requires:
	!   - subscripts are allocated and all scalar_sub   (caller guarantees this)
	!   - op is plain '=' (not +=, -=, etc.)
	!   - RHS element type is a numeric/bool scalar

	type(syntax_node_t), intent(in) :: node

	ok = .false.
	if (.not. allocated(node%lsubscripts)) return
	if (.not. all(node%lsubscripts%sub_kind == scalar_sub)) return
	if (node%op%kind /= equals_token) return
	if (.not. allocated(node%right)) return

	select case (node%right%val%type)
	case (bool_type, i32_type, i64_type, f32_type, f64_type)
		ok = .true.
	end select

end function store_idx_native_ok

!===============================================================================

pure logical function compound_idx_native_ok(node) result(ok)

	! Return .true. when an assignment_expr node with a compound op (+=, -=, etc.)
	! and all-scalar subscripts can be lowered to OP_COMPOUND_IDX_NAT.
	! Requires the same conditions as store_idx_native_ok except op must be
	! a supported arithmetic compound assignment (not plain '=').

	type(syntax_node_t), intent(in) :: node

	ok = .false.
	if (.not. allocated(node%lsubscripts)) return
	if (.not. all(node%lsubscripts%sub_kind == scalar_sub)) return
	if (node%op%kind == equals_token) return
	if (compound_to_arith_token(node%op%kind) == 0) return
	if (.not. allocated(node%right)) return

	select case (node%right%val%type)
	case (bool_type, i32_type, i64_type, f32_type, f64_type)
		ok = .true.
	end select

end function compound_idx_native_ok

!===============================================================================

pure logical function str_index_native_ok(node) result(ok)

	! Return .true. when a name_expr node is a single-subscript scalar read from
	! a scalar string variable, lowerable to OP_STR_INDEX_NAT.

	type(syntax_node_t), intent(in) :: node

	ok = .false.
	if (.not. allocated(node%lsubscripts)) return
	if (size(node%lsubscripts) /= 1) return
	if (node%lsubscripts(1)%sub_kind /= scalar_sub) return
	if (node%val%type /= str_type) return
	! Exclude str[] array element reads — for those, val%array stays allocated
	! (with rank 0) after parse_subscripts changes val%type to str_type.
	if (allocated(node%val%array)) return

end function str_index_native_ok

!===============================================================================

pure logical function str_slice_native_ok(node) result(ok)

	! Return .true. when a name_expr node is a bound_array (unit-step) range read
	! from a scalar string variable, lowerable to OP_STR_SLICE_NAT.

	type(syntax_node_t), intent(in) :: node

	ok = .false.
	if (.not. allocated(node%lsubscripts)) return
	if (size(node%lsubscripts) /= 1) return
	if (node%lsubscripts(1)%sub_kind /= range_sub) return
	if (node%val%type /= str_type) return

end function str_slice_native_ok

!===============================================================================

pure integer function typed_load_op(type_, is_const, is_local) result(op)

	! Return the typed scalar load opcode, or 0 for non-scalar types.

	integer, intent(in) :: type_
	logical, intent(in) :: is_const, is_local

	op = 0
	select case (type_)
	case (bool_type)
		if (is_const) then; op = OP_LOAD_CONST_BOOL
		else if (is_local) then; op = OP_LOAD_LOCAL_BOOL
		else; op = OP_LOAD_GLOBAL_BOOL; end if
	case (i32_type)
		if (is_const) then; op = OP_LOAD_CONST_I32
		else if (is_local) then; op = OP_LOAD_LOCAL_I32
		else; op = OP_LOAD_GLOBAL_I32; end if
	case (i64_type)
		if (is_const) then; op = OP_LOAD_CONST_I64
		else if (is_local) then; op = OP_LOAD_LOCAL_I64
		else; op = OP_LOAD_GLOBAL_I64; end if
	case (f32_type)
		if (is_const) then; op = OP_LOAD_CONST_F32
		else if (is_local) then; op = OP_LOAD_LOCAL_F32
		else; op = OP_LOAD_GLOBAL_F32; end if
	case (f64_type)
		if (is_const) then; op = OP_LOAD_CONST_F64
		else if (is_local) then; op = OP_LOAD_LOCAL_F64
		else; op = OP_LOAD_GLOBAL_F64; end if
	end select

end function typed_load_op

!===============================================================================

pure integer function typed_store_op(type_, is_local) result(op)

	! Return the typed scalar store opcode, or 0 for non-scalar types.

	integer, intent(in) :: type_
	logical, intent(in) :: is_local

	op = 0
	select case (type_)
	case (bool_type)
		if (is_local) then; op = OP_STORE_LOCAL_BOOL
		else; op = OP_STORE_GLOBAL_BOOL; end if
	case (i32_type)
		if (is_local) then; op = OP_STORE_LOCAL_I32
		else; op = OP_STORE_GLOBAL_I32; end if
	case (i64_type)
		if (is_local) then; op = OP_STORE_LOCAL_I64
		else; op = OP_STORE_GLOBAL_I64; end if
	case (f32_type)
		if (is_local) then; op = OP_STORE_LOCAL_F32
		else; op = OP_STORE_GLOBAL_F32; end if
	case (f64_type)
		if (is_local) then; op = OP_STORE_LOCAL_F64
		else; op = OP_STORE_GLOBAL_F64; end if
	end select

end function typed_store_op

!===============================================================================

pure integer function compound_to_arith_token(op_kind) result(tok)

	! Map a compound-assignment operator token to its arithmetic counterpart,
	! or 0 if the compound op is not a standard arithmetic op.
	! Used to select the typed binop opcode for native compound scalar assignment.

	integer, intent(in) :: op_kind

	select case (op_kind)
	case (plus_equals_token);    tok = plus_token
	case (minus_equals_token);   tok = minus_token
	case (star_equals_token);    tok = star_token
	case (slash_equals_token);   tok = slash_token
	case (percent_equals_token); tok = percent_token
	case (sstar_equals_token);   tok = sstar_token
	case default;                tok = 0
	end select

end function compound_to_arith_token

!===============================================================================

end module syntran__bytecode_m

!===============================================================================
