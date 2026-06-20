
!===============================================================================

module syntran__errors_m

	use syntran__utils_m

	implicit none

	character(len = :), allocatable :: err_prefix, err_int_prefix, err_rt_prefix, warn_prefix

	! Unique error/warning codes, in the style of rust's `error[E0631]`.
	! E = compile-time (parse/semantic), R = runtime, I = internal
	! ("should never happen"), W = warning.  Codes are permanent once
	! released: never renumber or reuse a retired code, just stop
	! assigning it.  get_all_error_codes() below must list every one of
	! these for the error-code unit test (uniqueness + format).
	character(len = *), parameter :: &
		EC_BAD_I32 = "E1", &
		EC_BAD_I64 = "E2", &
		EC_BAD_HEX32 = "E3", &
		EC_BAD_HEX64 = "E4", &
		EC_BAD_OCT32 = "E5", &
		EC_BAD_OCT64 = "E6", &
		EC_BAD_BIN32 = "E7", &
		EC_BAD_BIN64 = "E8", &
		EC_BAD_EXPR = "E9", &
		EC_UNTERMINATED_STR = "E10", &
		EC_UNTERMINATED_RAW_STR = "E11", &
		EC_ARRAY_STRUCT_SLICE = "E12", &
		EC_STRUCT_ARRAY_SLICE = "E13", &
		EC_NON_INT_SUBSCRIPT = "E14", &
		EC_BAD_F32 = "E15", &
		EC_BAD_F64 = "E16", &
		EC_BAD_TYPE = "E17", &
		EC_BAD_TYPE_SUFFIX = "E18", &
		EC_UNEXPECTED_CHAR = "E19", &
		EC_UNEXPECTED_TOKEN = "E20", &
		EC_VOID_ASSIGN = "E21", &
		EC_REDECLARE_VAR = "E22", &
		EC_REDECLARE_MEM = "E23", &
		EC_REDECLARE_FN = "E24", &
		EC_REDECLARE_INTR_FN = "E25", &
		EC_REDECLARE_STRUCT = "E26", &
		EC_REDECLARE_PRIMITIVE = "E27", &
		EC_UNDECLARE_VAR = "E28", &
		EC_UNDECLARE_FN = "E29", &
		EC_STD_ONLY_FN = "E30", &
		EC_NO_RETURN = "E31", &
		EC_MISSING_RETURN = "E32", &
		EC_BAD_ARG_COUNT = "E33", &
		EC_TOO_FEW_ARGS = "E34", &
		EC_TOO_MANY_ARGS = "E35", &
		EC_BAD_SUB_COUNT = "E36", &
		EC_BAD_SUB_RANK = "E37", &
		EC_EMPTY_STEP = "E38", &
		EC_SCALAR_SUBSCRIPT = "E39", &
		EC_BAD_CAT_RANK = "E40", &
		EC_BAD_RET_TYPE = "E41", &
		EC_BAD_ARG_TYPE = "E42", &
		EC_BAD_ARG_VAL = "E43", &
		EC_BAD_ARG_REF = "E44", &
		EC_NON_NAME_REF = "E45", &
		EC_SUB_REF = "E46", &
		EC_BAD_ARG_RANK = "E47", &  ! retired: constructor removed, never reuse
		EC_BINARY_TYPES = "E48", &
		EC_BINARY_RANKS = "E49", &
		EC_UNARY_TYPES = "E50", &
		EC_NON_ARRAY_LOOP = "E51", &
		EC_NON_BOOL_CONDITION = "E52", &
		EC_NON_FLOAT_LEN_RANGE = "E53", &
		EC_NON_INT_LEN = "E54", &
		EC_BOUND_TYPE_MISMATCH = "E55", &
		EC_NON_NUM_RANGE = "E56", &
		EC_NON_SCA_VAL = "E57", &
		EC_NON_INT_RANGE = "E58", &
		EC_HET_ARRAY = "E59", &
		EC_UNSET_MEMBER = "E60", &
		EC_RESET_MEMBER = "E61", &
		EC_NON_STRUCT_DOT = "E62", &
		EC_BAD_MEMBER_NAME = "E63", &
		EC_BAD_MEMBER_NAME_SHORT = "E64", &
		EC_BAD_MEMBER_TYPE = "E65", &
		EC_INC_404 = "E66", &
		EC_INC_READ = "E67", &
		EC_MOD_404 = "E68", &
		EC_MOD_READ = "E69", &
		EC_CIRCULAR_IMPORT = "E70", &
		EC_DUPLICATE_IMPORT = "E71", &
		EC_MOD_HYPHEN = "E72", &
		EC_MOD_KEYWORD = "E73", &
		EC_MOD_RESERVED_STD = "E74", &
		EC_MOD_SPACE = "E75", &
		EC_ALIAS_KEYWORD = "E76", &
		EC_ALIAS_RESERVED_STD = "E77", &
		EC_ALIAS_HYPHEN = "E78", &
		EC_ALIAS_SPACE = "E79", &
		EC_ALIAS_WITH_DOUBLECOLON = "E80", &
		EC_404 = "E81", &
		IC_EVAL_UNARY_TYPE = "I1", &
		IC_EVAL_BINARY_TYPES = "I2", &
		IC_EVAL_LEN_ARRAY = "I3", &
		IC_EVAL_UNARY_OP = "I4", &
		IC_EVAL_NODE = "I5", &
		IC_EVAL_BINARY_OP = "I6", &
		IC_UNIT_STEP_TYPE = "I7", &
		IC_FOR_STEP_ZERO = "I8", &
		IC_FOR_STEP_ZERO_F = "I9", &
		IC_STEP_ARRAY_TYPE = "I10", &
		IC_BOUND_LEN_TYPE = "I11", &
		IC_FOR_ARRAY_KIND = "I12", &
		IC_STR_CHAR_SUBSCRIPT = "I13", &
		IC_ARRAY_STEP_ZERO = "I14", &
		IC_ARRAY_STEP_ZERO_F = "I15", &
		IC_UNEXPECTED_ARRAY_KIND = "I16", &
		IC_ALLOC_ARRAY_TYPE = "I17", &
		IC_ARRAY_TYPE_NOT_IMPL = "I18", &
		IC_UNEXPECTED_ASSIGN_OP = "I19", &
		IC_SUBSCRIPT_STEP_ZERO = "I20", &
		IC_BAD_ARRAY_SUBSCRIPT_TYPE = "I21", &
		IC_EVAL_SUBSCRIPT_KIND = "I22", &
		IC_BAD_ARRAY_VAL_TYPE = "I23", &
		IC_UNKNOWN_NAME_EXPR_TYPE = "I24", &
		IC_BAD_TYPE_EXPECT_ARRAY = "I25", &
		IC_UNEXPECTED_USER_FN = "I26", &
		IC_FN_END_REACHED = "I27", &
		IC_UNEXPECTED_INTR_FN = "I28", &
		IC_PUSH_ARRAY_TYPE = "I29", &
		IC_TRIM_ARRAY_TYPE = "I30", &
		IC_CONVERT_F32 = "I31", &
		IC_CONVERT_F64 = "I32", &
		IC_CONVERT_I32 = "I33", &
		IC_CONVERT_I32_ARR = "I34", &
		IC_CONVERT_I64 = "I35", &
		IC_CONVERT_I64_ARR = "I36", &
		IC_SCOPE_STACK_EMPTY = "I37", &
		IC_UNREACHABLE_STRUCT_LOOKUP = "I38", &
		RC_MATMUL_DIM = "R1", &
		RC_PARSE_I32 = "R2", &
		RC_PARSE_I64 = "R3", &
		RC_PARSE_F32 = "R4", &
		RC_PARSE_F64 = "R5", &
		RC_BAD_FILE_MODE = "R6", &
		RC_FILE_RW_MODE = "R7", &
		RC_OPEN_FILE = "R8", &
		RC_READLN_NOT_OPEN = "R9", &
		RC_READLN_NOT_READ_MODE = "R10", &
		RC_READLN_FAIL = "R11", &
		RC_WRITELN_NOT_OPEN = "R12", &
		RC_WRITELN_NOT_WRITE_MODE = "R13", &
		RC_EOF_NOT_OPEN = "R14", &
		RC_EOF_NOT_READ_MODE = "R15", &
		RC_CLOSE_NOT_OPEN = "R16", &
		RC_SIZE_RANK_MISMATCH = "R17", &
		RC_TRANSPOSE_RANK = "R18", &
		RC_RESHAPE_MISMATCH = "R19", &
		RC_BAD_SUBSCRIPT_KIND = "R20", &
		RC_ARRAY_SIZE_MISMATCH = "R21", &
		RC_STRUCT_ARRAY_SLICE = "R22", &
		RC_FOR_STEP_ZERO = "R23", &
		RC_FOR_STEP_ZERO_F = "R24", &
		WC_MISSING_RETURN = "W1"

	! A text span indicates which characters to underline in a faulty line of
	! code
	type text_span_t
		integer :: start, length
	end type text_span_t

	! Span is different for each error.  Other things, like the src_file name,
	! text, and lines, stay the same (at least per parser invocation for now).
	! Those constants are in text_context_t, which is constructed within
	! new_parser() and new_lexer()
	type text_context_t

		! Text is the full text of the source code with filename src_file.  The
		! array lines(:) contains the character indices of the start of each
		! line
		character(len = :), allocatable :: text, src_file
		integer, allocatable :: lines(:)

	end type text_context_t

	!********

	! With include files, the context needs to be a vector with one element per
	! file
	type text_context_vector_t
		type(text_context_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_context
	end type text_context_vector_t

!===============================================================================

contains

!===============================================================================

function err_pre(code) result(pre)
	! Coded prefix for a compile-time error, e.g. "Error[E42]: "
	character(len = *), intent(in) :: code
	character(len = :), allocatable :: pre
	pre = fg_bold_bright_red//'Error['//code//']'//fg_bold//': '
end function err_pre

!===============================================================================

function err_rt_pre(code) result(pre)
	! Coded prefix for a runtime error, e.g. "Runtime error[R3]: "
	character(len = *), intent(in) :: code
	character(len = :), allocatable :: pre
	pre = fg_bold_bright_red//'Runtime error['//code//']'//fg_bold//': '
end function err_rt_pre

!===============================================================================

function err_int_pre(code) result(pre)
	! Coded prefix for an internal "should never happen" error, e.g.
	! "Internal syntran error[I7]: "
	character(len = *), intent(in) :: code
	character(len = :), allocatable :: pre
	pre = fg_bold_bright_red//'Internal syntran error['//code//']'//fg_bold//': '
end function err_int_pre

!===============================================================================

function warn_pre(code) result(pre)
	! Coded prefix for a warning, e.g. "Warning[W1]: "
	character(len = *), intent(in) :: code
	character(len = :), allocatable :: pre
	pre = fg_bold_yellow//'Warning['//code//']'//fg_bold//': '
end function warn_pre

!===============================================================================

function err_int(code, msg) result(err)
	! Generic internal-error constructor for inline call sites (eval_*, vm_*,
	! value.f90, etc.) that don't have source context/span to underline.  Each
	! call site supplies its own unique code and message text
	character(len = *), intent(in) :: code, msg
	character(len = :), allocatable :: err
	err = err_int_pre(code)//msg//color_reset
end function err_int

!===============================================================================

function err_rt(code, msg) result(err)
	! Generic runtime-error constructor for inline call sites.  Each call site
	! supplies its own unique code and message text
	character(len = *), intent(in) :: code, msg
	character(len = :), allocatable :: err
	err = err_rt_pre(code)//msg//color_reset
end function err_rt

!===============================================================================

function get_all_error_codes() result(codes)
	! Every error/warning code in the registry.  Used by the error-code unit
	! test to check uniqueness and format.  Keep this in sync with the
	! `parameter` declarations above
	type(string_vector_t) :: codes
	codes = new_string_vector()
	call codes%push(EC_BAD_I32)
	call codes%push(EC_BAD_I64)
	call codes%push(EC_BAD_HEX32)
	call codes%push(EC_BAD_HEX64)
	call codes%push(EC_BAD_OCT32)
	call codes%push(EC_BAD_OCT64)
	call codes%push(EC_BAD_BIN32)
	call codes%push(EC_BAD_BIN64)
	call codes%push(EC_BAD_EXPR)
	call codes%push(EC_UNTERMINATED_STR)
	call codes%push(EC_UNTERMINATED_RAW_STR)
	call codes%push(EC_ARRAY_STRUCT_SLICE)
	call codes%push(EC_STRUCT_ARRAY_SLICE)
	call codes%push(EC_NON_INT_SUBSCRIPT)
	call codes%push(EC_BAD_F32)
	call codes%push(EC_BAD_F64)
	call codes%push(EC_BAD_TYPE)
	call codes%push(EC_BAD_TYPE_SUFFIX)
	call codes%push(EC_UNEXPECTED_CHAR)
	call codes%push(EC_UNEXPECTED_TOKEN)
	call codes%push(EC_VOID_ASSIGN)
	call codes%push(EC_REDECLARE_VAR)
	call codes%push(EC_REDECLARE_MEM)
	call codes%push(EC_REDECLARE_FN)
	call codes%push(EC_REDECLARE_INTR_FN)
	call codes%push(EC_REDECLARE_STRUCT)
	call codes%push(EC_REDECLARE_PRIMITIVE)
	call codes%push(EC_UNDECLARE_VAR)
	call codes%push(EC_UNDECLARE_FN)
	call codes%push(EC_STD_ONLY_FN)
	call codes%push(EC_NO_RETURN)
	call codes%push(EC_MISSING_RETURN)
	call codes%push(EC_BAD_ARG_COUNT)
	call codes%push(EC_TOO_FEW_ARGS)
	call codes%push(EC_TOO_MANY_ARGS)
	call codes%push(EC_BAD_SUB_COUNT)
	call codes%push(EC_BAD_SUB_RANK)
	call codes%push(EC_EMPTY_STEP)
	call codes%push(EC_SCALAR_SUBSCRIPT)
	call codes%push(EC_BAD_CAT_RANK)
	call codes%push(EC_BAD_RET_TYPE)
	call codes%push(EC_BAD_ARG_TYPE)
	call codes%push(EC_BAD_ARG_VAL)
	call codes%push(EC_BAD_ARG_REF)
	call codes%push(EC_NON_NAME_REF)
	call codes%push(EC_SUB_REF)
	call codes%push(EC_BAD_ARG_RANK)
	call codes%push(EC_BINARY_TYPES)
	call codes%push(EC_BINARY_RANKS)
	call codes%push(EC_UNARY_TYPES)
	call codes%push(EC_NON_ARRAY_LOOP)
	call codes%push(EC_NON_BOOL_CONDITION)
	call codes%push(EC_NON_FLOAT_LEN_RANGE)
	call codes%push(EC_NON_INT_LEN)
	call codes%push(EC_BOUND_TYPE_MISMATCH)
	call codes%push(EC_NON_NUM_RANGE)
	call codes%push(EC_NON_SCA_VAL)
	call codes%push(EC_NON_INT_RANGE)
	call codes%push(EC_HET_ARRAY)
	call codes%push(EC_UNSET_MEMBER)
	call codes%push(EC_RESET_MEMBER)
	call codes%push(EC_NON_STRUCT_DOT)
	call codes%push(EC_BAD_MEMBER_NAME)
	call codes%push(EC_BAD_MEMBER_NAME_SHORT)
	call codes%push(EC_BAD_MEMBER_TYPE)
	call codes%push(EC_INC_404)
	call codes%push(EC_INC_READ)
	call codes%push(EC_MOD_404)
	call codes%push(EC_MOD_READ)
	call codes%push(EC_CIRCULAR_IMPORT)
	call codes%push(EC_DUPLICATE_IMPORT)
	call codes%push(EC_MOD_HYPHEN)
	call codes%push(EC_MOD_KEYWORD)
	call codes%push(EC_MOD_RESERVED_STD)
	call codes%push(EC_MOD_SPACE)
	call codes%push(EC_ALIAS_KEYWORD)
	call codes%push(EC_ALIAS_RESERVED_STD)
	call codes%push(EC_ALIAS_HYPHEN)
	call codes%push(EC_ALIAS_SPACE)
	call codes%push(EC_ALIAS_WITH_DOUBLECOLON)
	call codes%push(EC_404)
	call codes%push(IC_EVAL_UNARY_TYPE)
	call codes%push(IC_EVAL_BINARY_TYPES)
	call codes%push(IC_EVAL_LEN_ARRAY)
	call codes%push(IC_EVAL_UNARY_OP)
	call codes%push(IC_EVAL_NODE)
	call codes%push(IC_EVAL_BINARY_OP)
	call codes%push(IC_UNIT_STEP_TYPE)
	call codes%push(IC_FOR_STEP_ZERO)
	call codes%push(IC_FOR_STEP_ZERO_F)
	call codes%push(IC_STEP_ARRAY_TYPE)
	call codes%push(IC_BOUND_LEN_TYPE)
	call codes%push(IC_FOR_ARRAY_KIND)
	call codes%push(IC_STR_CHAR_SUBSCRIPT)
	call codes%push(IC_ARRAY_STEP_ZERO)
	call codes%push(IC_ARRAY_STEP_ZERO_F)
	call codes%push(IC_UNEXPECTED_ARRAY_KIND)
	call codes%push(IC_ALLOC_ARRAY_TYPE)
	call codes%push(IC_ARRAY_TYPE_NOT_IMPL)
	call codes%push(IC_UNEXPECTED_ASSIGN_OP)
	call codes%push(IC_SUBSCRIPT_STEP_ZERO)
	call codes%push(IC_BAD_ARRAY_SUBSCRIPT_TYPE)
	call codes%push(IC_EVAL_SUBSCRIPT_KIND)
	call codes%push(IC_BAD_ARRAY_VAL_TYPE)
	call codes%push(IC_UNKNOWN_NAME_EXPR_TYPE)
	call codes%push(IC_BAD_TYPE_EXPECT_ARRAY)
	call codes%push(IC_UNEXPECTED_USER_FN)
	call codes%push(IC_FN_END_REACHED)
	call codes%push(IC_UNEXPECTED_INTR_FN)
	call codes%push(IC_PUSH_ARRAY_TYPE)
	call codes%push(IC_TRIM_ARRAY_TYPE)
	call codes%push(IC_CONVERT_F32)
	call codes%push(IC_CONVERT_F64)
	call codes%push(IC_CONVERT_I32)
	call codes%push(IC_CONVERT_I32_ARR)
	call codes%push(IC_CONVERT_I64)
	call codes%push(IC_CONVERT_I64_ARR)
	call codes%push(IC_SCOPE_STACK_EMPTY)
	call codes%push(IC_UNREACHABLE_STRUCT_LOOKUP)
	call codes%push(RC_MATMUL_DIM)
	call codes%push(RC_PARSE_I32)
	call codes%push(RC_PARSE_I64)
	call codes%push(RC_PARSE_F32)
	call codes%push(RC_PARSE_F64)
	call codes%push(RC_BAD_FILE_MODE)
	call codes%push(RC_FILE_RW_MODE)
	call codes%push(RC_OPEN_FILE)
	call codes%push(RC_READLN_NOT_OPEN)
	call codes%push(RC_READLN_NOT_READ_MODE)
	call codes%push(RC_READLN_FAIL)
	call codes%push(RC_WRITELN_NOT_OPEN)
	call codes%push(RC_WRITELN_NOT_WRITE_MODE)
	call codes%push(RC_EOF_NOT_OPEN)
	call codes%push(RC_EOF_NOT_READ_MODE)
	call codes%push(RC_CLOSE_NOT_OPEN)
	call codes%push(RC_SIZE_RANK_MISMATCH)
	call codes%push(RC_TRANSPOSE_RANK)
	call codes%push(RC_RESHAPE_MISMATCH)
	call codes%push(RC_BAD_SUBSCRIPT_KIND)
	call codes%push(RC_ARRAY_SIZE_MISMATCH)
	call codes%push(RC_STRUCT_ARRAY_SLICE)
	call codes%push(RC_FOR_STEP_ZERO)
	call codes%push(RC_FOR_STEP_ZERO_F)
	call codes%push(WC_MISSING_RETURN)
end function get_all_error_codes

!===============================================================================

function err_bad_i32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_I32)//'bad i32 integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end function err_bad_i32

!===============================================================================

function err_bad_i64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_I64)//'bad i64 integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end function err_bad_i64

!===============================================================================

function err_bad_hex32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_HEX32)//'bad hexadecimal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end function err_bad_hex32

!===============================================================================

function err_bad_hex64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_HEX64)//'bad hexadecimal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end function err_bad_hex64

!===============================================================================

function err_bad_oct32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_OCT32)//'bad octal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end function err_bad_oct32

!===============================================================================

function err_bad_oct64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_OCT64)//'bad octal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end function err_bad_oct64

!===============================================================================

function err_bad_bin32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_BIN32)//'bad binary integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end function err_bad_bin32

!===============================================================================

function err_bad_bin64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_BIN64)//'bad binary integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end function err_bad_bin64

!===============================================================================

function err_bad_expr(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_BAD_EXPR)//'bad expression.  ' &
		//'Expression statements are only allowed in the REPL.  ' &
		//'Use `println()` to log its value' &
		//underline(context, span) &
		//' bad expression statement'//color_reset

end function err_bad_expr

!===============================================================================

function err_unterminated_str(context, span, str_) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: str_
	err = err_pre(EC_UNTERMINATED_STR)//'unterminated str literal `'//str_ &
		//'`' &
		//underline(context, span) &
		//' unterminated str'//color_reset

end function err_unterminated_str

!===============================================================================

function err_unterminated_raw_str(context, span, str_) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: str_
	err = err_pre(EC_UNTERMINATED_RAW_STR)//'unterminated raw str literal `'//str_ &
		//'`' &
		//underline(context, span) &
		//' unterminated raw str'//color_reset

end function err_unterminated_raw_str

!===============================================================================

function err_array_struct_slice(context, span, array) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: array

	err = err_pre(EC_ARRAY_STRUCT_SLICE) &
		//'slices are not implemented for arrays of structs, on array `' &
		//array//'`' &
		//underline(context, span) &
		//" slice subscript not implemented"//color_reset

end function err_array_struct_slice

!===============================================================================

function err_struct_array_slice(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_STRUCT_ARRAY_SLICE)//'slices are not implemented for structs of arrays.  ' &
		//'Only scalar subscripts can be used here' &
		//underline(context, span) &
		//' slice subscript not implemented'//color_reset

end function err_struct_array_slice

!===============================================================================

function err_non_int_subscript(context, span, subscript) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: subscript
	err = err_pre(EC_NON_INT_SUBSCRIPT)//'array subscript `'//subscript &
		//'` is not an integer' &
		//underline(context, span) &
		//' non-integer subscript'//color_reset

end function err_non_int_subscript

!===============================================================================

function err_bad_f32(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_F32)//'bad f32 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end function err_bad_f32

!===============================================================================

function err_bad_f64(context, span, num) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: num
	err = err_pre(EC_BAD_F64)//'bad f64 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end function err_bad_f64

!===============================================================================

function err_bad_type(context, span, type) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: type
	err = err_pre(EC_BAD_TYPE)//'bad type annotation `'//type//'`' &
		//underline(context, span) &
		//' bad type'//color_reset

end function err_bad_type

!===============================================================================

function err_bad_type_suffix(context, span, type, literal_kind) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: type, literal_kind
	err = err_pre(EC_BAD_TYPE_SUFFIX)//'bad literal type suffix `'//type//'` after ' &
		//literal_kind//' literal' &
		//underline(context, span) &
		//' bad type suffix'//color_reset

end function err_bad_type_suffix

!===============================================================================

function err_unexpected_char(context, span, c) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: c
	err = err_pre(EC_UNEXPECTED_CHAR) &
		//"unexpected character `"//c//"`"//underline(context, span) &
		//" unexpected character"//color_reset

end function err_unexpected_char

!===============================================================================

function err_unexpected_token(context, span, got, kind, expect) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: got, kind ,expect
	err = err_pre(EC_UNEXPECTED_TOKEN) &
		//'unexpected token `'//got//'` of kind `'//kind &
		//'`, expected `'//expect//'`'//underline(context, span) &
		//" unexpected token"//color_reset

end function err_unexpected_token

!===============================================================================

function err_void_assign(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_pre(EC_VOID_ASSIGN) &
		//'variable `'//var//'` cannot be initialized to void type' &
		//underline(context, span)//" void RHS type"//color_reset

end function err_void_assign

!===============================================================================

function err_redeclare_var(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_pre(EC_REDECLARE_VAR) &
		//'variable `'//var//'` has already been declared in this scope' &
		//underline(context, span)//" variable already declared"//color_reset

end function err_redeclare_var

!===============================================================================

function err_redeclare_mem(context, span, var) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	err = err_pre(EC_REDECLARE_MEM) &
		//'member `'//var//'` has already been declared in this struct' &
		//underline(context, span)//" member already declared"//color_reset

end function err_redeclare_mem

!===============================================================================

function err_redeclare_fn(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_pre(EC_REDECLARE_FN) &
		//'function `'//fn//'` has already been declared' &
		//underline(context, span)//" function already declared"//color_reset

end function err_redeclare_fn

function err_redeclare_intr_fn(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_pre(EC_REDECLARE_INTR_FN) &
		//'function `'//fn//'` is already a built-in function' &
		//underline(context, span)//" function already exists"//color_reset

end function err_redeclare_intr_fn

!===============================================================================

function err_redeclare_struct(context, span, struct) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: struct
	err = err_pre(EC_REDECLARE_STRUCT) &
		//'struct `'//struct//'` has already been declared' &
		//underline(context, span)//" struct already declared"//color_reset

end function err_redeclare_struct

!===============================================================================

function err_redeclare_primitive(context, span, struct) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: struct
	err = err_pre(EC_REDECLARE_PRIMITIVE) &
		//'struct name `'//struct//'` is reserved for a primitive type' &
		//underline(context, span)//" cannot redeclare primitives"//color_reset

end function err_redeclare_primitive

!===============================================================================

function err_undeclare_var(context, span, var, suggest) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: var
	character(len = *), intent(in), optional :: suggest

	err = err_pre(EC_UNDECLARE_VAR) &
		//'variable `'//var//'` has not been declared in this scope' &
		//underline(context, span)//" variable undeclared"//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end function err_undeclare_var

!===============================================================================

function err_undeclare_fn(context, span, fn, suggest) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	character(len = *), intent(in), optional :: suggest

	err = err_pre(EC_UNDECLARE_FN) &
		//'function `'//fn//'` has not been defined' &
		//underline(context, span)//" undefined function"//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end function err_undeclare_fn

!===============================================================================

function err_std_only_fn(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_pre(EC_STD_ONLY_FN) &
		//'function `'//fn//'` must be called with std:: prefix (use `std::'//fn//'()`)' &
		//underline(context, span)//" requires std:: prefix"//color_reset

end function err_std_only_fn

!===============================================================================

function err_no_return(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_pre(EC_NO_RETURN) &
		//'function `'//fn//'` does not have any return statements' &
		//underline(context, span)//" function without returns"//color_reset

end function err_no_return

!===============================================================================

function err_missing_return(context, span, fn) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn
	err = err_pre(EC_MISSING_RETURN) &
		//'not all code paths in function `'//fn//'` return' &
		//underline(context, span)//" function may not return on all paths" &
		//color_reset

end function err_missing_return

!===============================================================================

function warn_missing_return(context, span, fn) result(warn)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: warn

	character(len = *), intent(in) :: fn
	warn = warn_pre(WC_MISSING_RETURN) &
		//'not all code paths in function `'//fn//'` return' &
		//underline(context, span)//" function may not return on all paths" &
		//color_reset

end function warn_missing_return

!===============================================================================

function err_bad_arg_count(context, span, fn, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, argument_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: fn

	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_BAD_ARG_COUNT) &
		//'function `'//fn//'` requires '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong argument count"//color_reset

end function err_bad_arg_count

!===============================================================================

function err_too_few_args(context, span, fn, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, argument_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: fn

	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_TOO_FEW_ARGS) &
		//'variadic function `'//fn//'` requires at least '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" not enough arguments"//color_reset

end function err_too_few_args

!===============================================================================

function err_too_many_args(context, span, fn, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, argument_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: fn

	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_TOO_MANY_ARGS) &
		//'variadic function `'//fn//'` requires at most '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" too many arguments"//color_reset

end function err_too_many_args

!===============================================================================

function err_bad_sub_count(context, span, array, expect, actual) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err, subscript_s
	integer, intent(in):: expect, actual

	character(len = *), intent(in) :: array

	if (expect == 1) then
		subscript_s = 'subscript'
	else
		subscript_s = 'subscripts'
	end if

	err = err_pre(EC_BAD_SUB_COUNT) &
		//'array `'//array//'` requires '//str(expect) &
		//' '//subscript_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong subscript count"//color_reset

end function err_bad_sub_count

!===============================================================================

function err_bad_sub_rank(context, span, rank_) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	integer, intent(in) :: rank_

	err = err_pre(EC_BAD_SUB_RANK) &
		//"subscript index array of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector subscript"//color_reset

end function err_bad_sub_rank

!===============================================================================

function err_empty_step(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_EMPTY_STEP) &
		//'slice step cannot be omitted between two colons' &
		//underline(context, span)//" write the step explicitly"//color_reset

end function err_empty_step

!===============================================================================

function err_scalar_subscript(context, span, scalar) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: scalar

	err = err_pre(EC_SCALAR_SUBSCRIPT) &
		//'scalar `'//scalar//'` cannot have subscripts' &
		//underline(context, span)//" unexpected subscripts"//color_reset

end function err_scalar_subscript

!===============================================================================

function err_bad_cat_rank(context, span, rank_) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	integer, intent(in) :: rank_

	err = err_pre(EC_BAD_CAT_RANK) &
		//"concatenated array of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector concatenation"//color_reset

end function err_bad_cat_rank

!===============================================================================

function err_bad_ret_type(context, span, fn, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: fn, expect, actual

	err = err_pre(EC_BAD_RET_TYPE) &
		//'function `'//fn &
		//'` requires return value of '//expect//' but returns a value of ' &
		//actual &
		//underline(context, span)//" wrong return type"//color_reset

end function err_bad_ret_type

!===============================================================================

function err_bad_arg_type(context, span, fn, iarg, param, expect, actual) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param, expect, actual

	err = err_pre(EC_BAD_ARG_TYPE) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires type `'//expect//'` but was given `' &
		//actual//'`' &
		//underline(context, span)//" wrong argument type"//color_reset

end function err_bad_arg_type

!===============================================================================

function err_bad_arg_val(context, span, fn, iarg, param) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param

	err = err_pre(EC_BAD_ARG_VAL) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a `&` reference but was given a value argument' &
		//underline(context, span)//" missing `&` ref"//color_reset

end function err_bad_arg_val

!===============================================================================

function err_bad_arg_ref(context, span, fn, iarg, param) &
		result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	integer, intent(in):: iarg

	character(len = *), intent(in) :: fn, param

	err = err_pre(EC_BAD_ARG_REF) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a value but was given a `&` reference argument' &
		//underline(context, span)//" bad `&` ref"//color_reset

end function err_bad_arg_ref

!===============================================================================

function err_non_name_ref(context, span) result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_NON_NAME_REF) &
		//'`&` reference to unexpected expression kind.  references can only ' &
		//'be made to variable name expressions' &
		//underline(context, span)//" non-name `&` ref"//color_reset

end function err_non_name_ref

!===============================================================================

function err_sub_ref(context, span) result(err)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_SUB_REF) &
		//'`&` reference to unexpected subscripted expression.  references can only ' &
		//'be made to name expressions without subscripts' &
		//underline(context, span)//" subscripted `&` ref"//color_reset

end function err_sub_ref

!===============================================================================

! EC_BAD_ARG_RANK (E47) is formally retired: its constructor was deleted
! because it was never wired up to any call site, but the code itself stays
! registered in get_all_error_codes() forever and must never be reused (see
! the permanence policy at the top of this file and in doc/errors.md)

!===============================================================================

function err_binary_types(context, span, op, left, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, left, right

	!print *, 'starting err_binary_types'

	err = err_pre(EC_BINARY_TYPES) &
		//'binary operator `'//op//'` is not defined for types ' &
		//left//' and '//right//underline(context, span) &
		//" wrong types for this binary operator"//color_reset

end function err_binary_types

!===============================================================================

function err_binary_ranks(context, span, op, left, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op
	integer, intent(in) :: left, right

	!print *, 'starting err_binary_ranks'

	err = err_pre(EC_BINARY_RANKS) &
		//'rank mismatch for binary operator `'//op//'` with ranks ' &
		//str(left)//' and '//str(right)//underline(context, span) &
		//" array rank mismatch"//color_reset

end function err_binary_ranks

!===============================================================================

function err_unary_types(context, span, op, right) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: op, right
	err = err_pre(EC_UNARY_TYPES) &
		//'unary operator `'//op//'` is not defined for type ' &
		//right//underline(context, span) &
		//" wrong type for this unary operator"//color_reset

end function err_unary_types

!===============================================================================

function err_non_array_loop(context, span, range_) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range_
	err = err_pre(EC_NON_ARRAY_LOOP) &
		//'range `'//trimw(range_)//'` of for loop is not an array' &
		//underline(context, span) &
		//" non-array range"//color_reset

end function err_non_array_loop

!===============================================================================

function err_non_bool_condition(context, span, condition, statement) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: condition, statement
	err = err_pre(EC_NON_BOOL_CONDITION) &
		//'condition `'//trimw(condition)//'` of '//statement//' is not bool' &
		//underline(context, span) &
		//" non-bool condition"//color_reset

end function err_non_bool_condition

!===============================================================================

function err_non_float_len_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range
	err = err_pre(EC_NON_FLOAT_LEN_RANGE) &
		//'bound `'//range//'` of length-based array range is not a float' &
		//underline(context, span) &
		//" non-float bound"//color_reset

end function err_non_float_len_range

!===============================================================================

function err_non_int_len(context, span, len) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: len
	err = err_pre(EC_NON_INT_LEN) &
		//'length `'//len//'` of array is not an integer' &
		//underline(context, span) &
		//" non-int length"//color_reset

end function err_non_int_len

!===============================================================================

function err_bound_type_mismatch(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	err = err_pre(EC_BOUND_TYPE_MISMATCH) &
		//'types of lower and upper range bounds do not match' &
		//underline(context, span) &
		//" mismatched types"//color_reset

end function err_bound_type_mismatch

!===============================================================================

function err_non_num_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range

	! This language will be technically wrong if I add a complex number type
	! (complex numbers are numbers, but un-ordered and hence unsuitable for
	! ranges)
	err = err_pre(EC_NON_NUM_RANGE) &
		//'bound `'//range//'` of array range is not a numeric type' &
		//underline(context, span) &
		//" non-numeric range"//color_reset

end function err_non_num_range

!===============================================================================

function err_non_sca_val(context, span, val) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: val

	err = err_pre(EC_NON_SCA_VAL) &
		//'value `'//val//'` of uniform array is not a scalar' &
		//underline(context, span) &
		//" non-scalar array value"//color_reset

end function err_non_sca_val

!===============================================================================

function err_non_int_range(context, span, range) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: range
	err = err_pre(EC_NON_INT_RANGE) &
		//'bound `'//range//'` of array range is not an i32 integer' &
		//underline(context, span) &
		//" non-i32 range"//color_reset

end function err_non_int_range

!===============================================================================

function err_het_array(context, span, elem) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: elem
	err = err_pre(EC_HET_ARRAY) &
		//'array is heterogeneous.  Element `'//elem  &
		//"` does not match the first element's type" &
		//underline(context, span) &
		//" heterogeneous array element"//color_reset

end function err_het_array

!===============================================================================

function err_unset_member(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_pre(EC_UNSET_MEMBER) &
		//'not all members in struct `'//struct_name//'` are initialized.  ' &
		//'Member `'//mem_name//'` is uninitialized' &
		//underline(context, span) &
		//" uninitialized member(s)"//color_reset

end function err_unset_member

!===============================================================================

function err_reset_member(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_pre(EC_RESET_MEMBER) &
		//'member `'//mem_name//'` is already initialized in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" duplicate member"//color_reset

end function err_reset_member

!===============================================================================

function err_non_struct_dot(context, span, ident) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: ident
	err = err_pre(EC_NON_STRUCT_DOT) &
		//'dot member access cannot be performed on non-struct variable `' &
		//ident//'`' &
		//underline(context, span) &
		//" dot on a non-struct"//color_reset

end function err_non_struct_dot

!===============================================================================

function err_bad_member_name(context, span, mem_name, struct_var_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	! This msg yells about both the variable name `struct_var_name` and its
	! "class" `struct_name`.  Its useful for dot expressions `var.mem`

	character(len = *), intent(in) :: mem_name, struct_var_name, struct_name
	err = err_pre(EC_BAD_MEMBER_NAME) &
		//'member `'//mem_name//'` does not exist in struct `'//struct_var_name//'`' &
		//' of type `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

end function err_bad_member_name

!===============================================================================

function err_bad_member_name_short(context, span, mem_name, struct_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	! This msg yells only about the "class" `struct_name`.  Its useful for
	! struct instantiations as in `return Class{mem = val};` where there may not
	! be a variable identifier like in the longer fn above

	character(len = *), intent(in) :: mem_name, struct_name
	err = err_pre(EC_BAD_MEMBER_NAME_SHORT) &
		//'member `'//mem_name//'` does not exist in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

end function err_bad_member_name_short

!===============================================================================

function err_bad_member_type(context, span, mem_name, struct_name, act_type, exp_type) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: mem_name, struct_name, exp_type, act_type
	err = err_pre(EC_BAD_MEMBER_TYPE) &
		//'member `'//mem_name//'` in struct `'//struct_name//'` has the wrong type.  ' &
		//'Member requires type `'//exp_type//'` but was given `'//act_type//'`' &
		//underline(context, span) &
		//" bad member type"//color_reset

end function err_bad_member_type

!===============================================================================

function err_inc_404(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_pre(EC_INC_404) &
		//'#include file `'//filename//'` not found' &
		//underline(context, span) &
		//" file not found"//color_reset

end function err_inc_404

!===============================================================================

function err_inc_read(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_pre(EC_INC_READ) &
		//'#include file `'//filename//'` cannot be read' &
		//underline(context, span) &
		//" cannot read file"//color_reset

end function err_inc_read

!===============================================================================

function err_mod_404(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_pre(EC_MOD_404) &
		//'module file `'//filename//'` not found' &
		//underline(context, span) &
		//" file not found"//color_reset

end function err_mod_404

!===============================================================================

function err_mod_read(context, span, filename) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err

	character(len = *), intent(in) :: filename
	err = err_pre(EC_MOD_READ) &
		//'module file `'//filename//'` cannot be read' &
		//underline(context, span) &
		//" cannot read file"//color_reset

end function err_mod_read

!===============================================================================

function err_circular_import(context, span, module_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = *), intent(in) :: module_name
	character(len = :), allocatable :: err
	err = err_pre(EC_CIRCULAR_IMPORT) &
		//'circular module dependency on `'//module_name//'`' &
		//underline(context, span) &
		//" circular import"//color_reset
end function err_circular_import

!===============================================================================

function err_duplicate_import(context, span, module_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = *), intent(in) :: module_name
	character(len = :), allocatable :: err
	err = err_pre(EC_DUPLICATE_IMPORT) &
		//'duplicate import of module `'//module_name//'`' &
		//underline(context, span) &
		//" duplicate import"//color_reset
end function err_duplicate_import

!===============================================================================

function err_mod_hyphen(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_MOD_HYPHEN) &
		//'hyphens are not allowed in module names, use underscores instead' &
		//underline(context, span) &
		//" bad module name"//color_reset
end function err_mod_hyphen

!===============================================================================

function err_mod_keyword(context, span, keyword) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = *), intent(in) :: keyword
	character(len = :), allocatable :: err
	err = err_pre(EC_MOD_KEYWORD) &
		//'module name `'//keyword//'` is a reserved keyword' &
		//underline(context, span) &
		//" keyword as module name"//color_reset
end function err_mod_keyword

!===============================================================================

function err_mod_reserved_std(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_MOD_RESERVED_STD) &
		//'module name `std` is reserved for the standard library' &
		//underline(context, span) &
		//" reserved name"//color_reset
end function err_mod_reserved_std

!===============================================================================

function err_mod_space(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_MOD_SPACE) &
		//'spaces are not allowed in module names' &
		//underline(context, span) &
		//" bad module name"//color_reset
end function err_mod_space

!===============================================================================

function err_alias_keyword(context, span, alias_name) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = *), intent(in) :: alias_name
	character(len = :), allocatable :: err
	err = err_pre(EC_ALIAS_KEYWORD) &
		//'keyword `'//alias_name//'` cannot be used as module alias' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end function err_alias_keyword

!===============================================================================

function err_alias_reserved_std(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_ALIAS_RESERVED_STD) &
		//'`std` is reserved and cannot be used as module alias' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end function err_alias_reserved_std

!===============================================================================

function err_alias_hyphen(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_ALIAS_HYPHEN) &
		//'hyphens are not allowed in module aliases' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end function err_alias_hyphen

!===============================================================================

function err_alias_space(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_ALIAS_SPACE) &
		//'spaces are not allowed in module aliases' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end function err_alias_space

!===============================================================================

function err_alias_with_doublecolon(context, span) result(err)
	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: err
	err = err_pre(EC_ALIAS_WITH_DOUBLECOLON) &
		//'cannot combine alias with `::` syntax' &
		//underline(context, span) &
		//" use `use module as alias;` without `::`"//color_reset
end function err_alias_with_doublecolon

!===============================================================================

function err_404(filename) result(err)
	character(len = *), intent(in) :: filename
	character(len = :), allocatable :: err
	err = err_pre(EC_404)//'file `'//filename//'` not found'//color_reset
end function err_404

!===============================================================================

function err_eval_unary_type(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_UNARY_TYPE) &
		//'unary operator `'//op//'` cannot be evaluated for operand type ' &
		//color_reset

end function err_eval_unary_type

!===============================================================================

function err_eval_binary_types(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_BINARY_TYPES) &
		//'binary operator `'//op//'` cannot be evaluated for operand types ' &
		//color_reset

end function err_eval_binary_types

!===============================================================================

function err_matmul_dim(lsize, rsize) result(err)
	integer(kind = 8), intent(in) :: lsize, rsize
	character(len = :), allocatable :: err

	err = err_rt_pre(RC_MATMUL_DIM) &
		//'matmul `@` dimension mismatch: inner dimensions ' &
		//str(int(lsize))//' and ' &
		//str(int(rsize))//' do not agree'//color_reset

end function err_matmul_dim

!===============================================================================

function err_eval_len_array(type_name) result(err)
	character(len = *), intent(in) :: type_name
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_LEN_ARRAY) &
		//'len array cannot be evaluated for type `' &
		//type_name//'`'//color_reset

end function err_eval_len_array

!===============================================================================

function err_eval_unary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_UNARY_OP) &
		//'unexpected unary operator `'//op//'`'//color_reset

end function err_eval_unary_op

!===============================================================================

function err_eval_node(node_kind) result(err)
	character(len = *), intent(in) :: node_kind
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_NODE) &
		//'unexpected node `'//node_kind//'`'//color_reset

end function err_eval_node

!===============================================================================

function err_eval_binary_op(op) result(err)
	character(len = *), intent(in) :: op
	character(len = :), allocatable :: err

	err = err_int_pre(IC_EVAL_BINARY_OP) &
		//'unexpected binary operator `'//op//'`'//color_reset

end function err_eval_binary_op

!===============================================================================

function new_span(start, length) result(span)
	! Maybe this should take end pos instead of length?  Seems like I end up
	! back-calculating len from pos most of the time

	integer, intent(in) :: start, length

	type(text_span_t) :: span

	span%start    = start
	span%length   = max(length, 1)

end function new_span

!===============================================================================

function new_context(text, src_file, lines) result(context)

	character(len = *), intent(in) :: text, src_file
	integer, intent(in) :: lines(:)
	type(text_context_t) :: context

	context%text = text
	context%src_file = src_file

	! gfortran warns if i just let this auto-allocate in the next line?
	allocate(context%lines( 1: size(lines) ))  

	context%lines = lines

end function new_context

!===============================================================================

function new_context_vector() result(vector)

	type(text_context_vector_t) :: vector

	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end function new_context_vector

!===============================================================================

subroutine push_context(vector, val)

	class(text_context_vector_t) :: vector
	type(text_context_t) :: val

	!********

	type(text_context_t), allocatable :: tmp(:)

	integer :: tmp_cap

	vector%len_ = vector%len_ + 1

	if (vector%len_ > vector%cap) then
		!print *, 'growing vector'

		tmp_cap = 2 * vector%len_
		allocate(tmp( tmp_cap ))
		tmp(1: vector%cap) = vector%v

		call move_alloc(tmp, vector%v)
		vector%cap = tmp_cap

	end if

	vector%v( vector%len_ ) = val

end subroutine push_context

!===============================================================================

function underline(context, span)

	type(text_context_t) :: context
	type(text_span_t), intent(in) :: span
	character(len = :), allocatable :: underline

	character(len = :), allocatable :: str_i, spaces, fg1, rst, col, text
	integer :: i1(1), i, j, str_i_len, start, last, length

	! Get line number.  Ideally use a binary search, but it's so nice to do
	! something with a Fortran intrinsic for a change
	i1 = maxloc(context%lines, context%lines <= span%start)
	i = min(size(context%lines)-1, max(1, i1(1)))

	str_i = str(i)
	str_i_len = len(str_i)

	!print *, 'line # = ', i

	! Pad spaces the same length as the line number string
	spaces = repeat(' ', str_i_len + 2)

	! First and last character indices of line
	start = context%lines(i)
	last  = context%lines(i+1) - 1

	! Trim whitespace from end of line.  Make sure interpreter looks ok with
	! errors at final character of line.  TODO: use is_whitespace()
	do while (context%text(last:last) == line_feed .or. &
	          context%text(last:last) == carriage_return)
		last = last - 1
	end do

	! Without substitution, this doesn't work if the line is indented with tabs,
	! because it depends on how wide the console displays a tab!
	text = tabs2spaces(context%text(start: last))

	! Length of error token(s) within the line
	length = span%length

	! Same idea as 'last' adjustment: trim whitespace from error tokens.  It
	! would make more sense to clamp length directly instead of clamping
	! j :shrug:
	j = max(min(len(context%text), span%start + length - 1), 1)
	!print *, 'char = "', context%text(j:j), '"'
	do while (is_whitespace(context%text(j:j)))
		length = length - 1
		j = min(len(context%text), span%start + length - 1)
	end do
	length = max(length, 1)

	! Here's an example of a rust error message, from which I'm stealing UX:
	!
	! """
	!
	!    Compiling skillet v0.4.0 (C:\git\skillet)
	! error[E0433]: failed to resolve: use of undeclared crate or module `st`
	!  --> src\main.rs:6:5
	!   |
	! 6 | use st::path::PathBuf;
	!   |     ^^ use of undeclared crate or module `st`
	!   |
	! help: there is a crate or module with a similar name
	!   |
	! 6 | use std::path::PathBuf;
	!   |     ~~~
	!
	! """

	col = str(span%start - context%lines(i) + 1)

	fg1 = fg_bright_cyan
	!fg1 = fg_bright_blue

	rst = color_reset

	underline = line_feed//fg1//spaces(2:)//"--> "//rst//context%src_file &
		//":"//str_i//":"//col//line_feed &
		//fg1//     spaces//"| "//line_feed &
		//fg1//" "//str_i//" | "//rst//text//line_feed &
		//fg1//     spaces//"| " &
		//repeat(' ', max(span%start - context%lines(i), 0)) &
		//fg_bright_red//repeat('^', length)

end function underline

!===============================================================================

subroutine internal_error()

	! The goal is for this to be unreachable

	write(*,*) fg_bold_bright_red//'Fatal error'//color_reset
	call exit(exit_failure)

end subroutine internal_error

!===============================================================================

end module syntran__errors_m

!===============================================================================

