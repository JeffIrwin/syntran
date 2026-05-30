
!===============================================================================

module syntran__bytecode_m

	use syntran__types_m

	implicit none

	!********

	! Opcode enum.  Uses a separate numbering space from the token/type/node-kind
	! enum in consts.f90 (which tops out at 121) to prevent accidental aliasing.

	integer, parameter :: &
		OP_EVAL_NODE        = 1001, &	! debug fallback: run one AST node through syntax_eval (no compiler emit)
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
		OP_CALL_INTR        = 1022, &	! M6: intrinsic call; native: a=intr_id b=argc (args on stack);
		                            	!   readln/close: a=intr_id b=1 c=(id_index*2+is_loc) for slot writeback
		OP_FOR_SETUP        = 1023, &	! M8: for-loop setup: a=node_idx; pushes iter onto for_iter stack
		OP_FOR_NEXT         = 1024, &	! M8: for-loop advance: a=L_pop; exhausted->jump (no pop), else write loop var
		OP_FOR_POP          = 1025, &	! M8: for-loop exit: pop for_iter stack (emitted at all loop exits)
		OP_NEW_ARRAY        = 1026, &	! M8: array construction: a=node_idx; calls eval_array_expr, pushes result
		OP_STORE_SLICE      = 1027, &	! M8: slice/complex LHS assign: a=node_idx; calls eval_assignment_expr
		OP_HALT             = 1028  	! M8: top-level return: exit VM loop, TOS is result

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
		INTR_ARGS         = 130

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
	! The node pool nodes(:) holds AST subtrees referenced by OP_EVAL_NODE.
	! This is only used for the M0 fallback and will be removed once all node
	! kinds have been lowered to real opcodes.

	type program_t

		type(instr_t), allocatable :: code(:)
		integer :: len_ = 0, cap = 0

		type(value_t), allocatable :: consts(:)
		integer :: nconsts = 0

		! AST node pool for the OP_EVAL_NODE fallback
		type(syntax_node_t), allocatable :: nodes(:)
		integer :: nnodes = 0

		integer, allocatable :: fn_entry(:)
		integer, allocatable :: fn_num_locs(:)
		integer :: entry_main = 1

	end type program_t

!===============================================================================

contains

!===============================================================================

function add_const(prog, val) result(idx)

	! Add a value to the constant pool and return its 1-based index.
	! Uses the copy-to-tmp-and-back growth pattern required for types with
	! allocatable members (same rationale as push_value in value.f90).

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
		deallocate(prog%consts)
		allocate(prog%consts(new_cap))
		do i = 1, prog%nconsts - 1
			prog%consts(i) = tmp(i)
		end do
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
	! Growth uses the copy-to-tmp-and-back pattern required for types with
	! allocatable members (same rationale as push_value in value.f90).

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
		deallocate(prog%nodes)
		allocate(prog%nodes(new_cap))
		do i = 1, prog%nnodes - 1
			prog%nodes(i) = tmp(i)
		end do
	end if

	prog%nodes(idx) = node

end function add_node

!===============================================================================

subroutine patch_jump(prog, ip, target)

	! Backpatch the jump target (field `a`) of a previously emitted
	! OP_JUMP or OP_JUMP_IF_FALSE instruction at position ip.

	type(program_t), intent(inout) :: prog
	integer, intent(in) :: ip, target

	prog%code(ip)%a = target

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
	case default;            id = 0
	end select

end function intr_id_from_name

!===============================================================================

end module syntran__bytecode_m

!===============================================================================
