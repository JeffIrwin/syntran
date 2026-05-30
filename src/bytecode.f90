
!===============================================================================

module syntran__bytecode_m

	use syntran__types_m

	implicit none

	!********

	! Opcode enum.  Uses a separate numbering space from the token/type/node-kind
	! enum in consts.f90 (which tops out at 121) to prevent accidental aliasing.

	integer, parameter :: &
		OP_EVAL_NODE   = 1001, &	! fallback: run one AST node through syntax_eval
		OP_LOAD_CONST  = 1002, &	! push consts(a) onto the operand stack (deep copy)
		OP_LOAD_GLOBAL = 1003, &	! push state%vars%vals(a) (deep copy)
		OP_LOAD_LOCAL  = 1004, &	! push state%locs%vals(a) (deep copy)
		OP_STORE_GLOBAL= 1005, &	! copy TOS into state%vars%vals(a), keep TOS
		OP_STORE_LOCAL = 1006, &	! copy TOS into state%locs%vals(a), keep TOS
		OP_BINOP       = 1007, &	! pop right+left, compute with op-token a, push result
		OP_UNOP        = 1008, &	! pop operand, compute with op-token a, push result
		OP_POP         = 1009, &	! discard TOS
		OP_JUMP          = 1010, &	! unconditional jump: ip = a
		OP_JUMP_IF_FALSE = 1011  	! pop bool TOS; if false: ip = a, else continue

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

end module syntran__bytecode_m

!===============================================================================
