
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
		EC_IMMUTABLE_VAR = "E82", &
		EC_CONST_ASSIGN = "E83", &
		EC_MUTABLE_METHOD_ON_TEMP = "E84", &
		EC_MEMBER_METHOD_CLASH = "E85", &
		EC_MODULE_RETURN = "E86", &
		EC_FN_PTR_UNSUPPORTED = "E87", &
		EC_NOT_CALLABLE = "E88", &
		EC_FN_PTR_ARRAY = "E89", &
		EC_FN_PTR_STRUCT_MEMBER = "E90", &
		EC_VOID_ARG = "E91", &
		EC_REDECLARE_ENUM = "E92", &
		EC_REDECLARE_VARIANT = "E93", &
		EC_UNKNOWN_VARIANT = "E94", &
		EC_DUPLICATE_ENUM_VALUE = "E95", &
		EC_ENUM_CAST_RANGE = "E96", &
		EC_ENUM_INDEX = "E97", &
		EC_VAR_TYPE_CLASH = "E98", &
		EC_ENUM_NAME_VALUE = "E99", &
		EC_BAD_FILE_MEMBER = "E100", &
		EC_READONLY_FILE_MEMBER = "E101", &
		EC_EXPL_ARRAY_SIZE = "E102", &
		EC_NON_INT_SIZE = "E103", &
		EC_FLOAT_INT_SUFFIX = "E104", &
		EC_REF_TYPE = "E105", &
		IC_EVAL_UNARY_TYPE = "I1", &
		IC_EVAL_BINARY_TYPES = "I2", &
		IC_EVAL_LEN_ARRAY = "I3", &
		IC_EVAL_UNARY_OP = "I4", &
		IC_EVAL_NODE = "I5", &            ! retired: only emission site was the deleted AST-walker dispatcher (eval.f90), never reuse
		IC_EVAL_BINARY_OP = "I6", &
		IC_UNIT_STEP_TYPE = "I7", &
		IC_FOR_STEP_ZERO = "I8", &       ! retired: replaced by RC_FOR_STEP_ZERO, never reuse
		IC_FOR_STEP_ZERO_F = "I9", &     ! retired: replaced by RC_FOR_STEP_ZERO_F, never reuse
		IC_STEP_ARRAY_TYPE = "I10", &
		IC_BOUND_LEN_TYPE = "I11", &
		IC_FOR_ARRAY_KIND = "I12", &
		IC_STR_CHAR_SUBSCRIPT = "I13", &
		IC_ARRAY_STEP_ZERO = "I14", &    ! retired: replaced by RC_ARRAY_STEP_ZERO, never reuse
		IC_ARRAY_STEP_ZERO_F = "I15", &  ! retired: replaced by RC_ARRAY_STEP_ZERO_F, never reuse
		IC_UNEXPECTED_ARRAY_KIND = "I16", &
		IC_ALLOC_ARRAY_TYPE = "I17", &
		IC_ARRAY_TYPE_NOT_IMPL = "I18", &
		IC_UNEXPECTED_ASSIGN_OP = "I19", &
		IC_SUBSCRIPT_STEP_ZERO = "I20", & ! retired: replaced by RC_SUBSCRIPT_STEP_ZERO, never reuse
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
		IC_CONVERT_F32_ARR = "I39", &
		IC_CONVERT_F64_ARR = "I40", &
		IC_TRANSPOSE_ARRAY_TYPE = "I41", &
		IC_FILE_MEMBER = "I42", &
		IC_MISSING_RECV_SLOTS = "I43", &
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
		RC_ARRAY_STEP_ZERO = "R25", &
		RC_ARRAY_STEP_ZERO_F = "R26", &
		RC_SUBSCRIPT_STEP_ZERO = "R27", &
		RC_CLOSE_STANDARD = "R28", &
		RC_GETENV_UNSET = "R29", &
		RC_WRITELN_FAIL = "R30", &
		RC_CLOSE_FAIL   = "R31", &
		RC_ENUM_CAST_RANGE = "R32", &
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


	! Bodies live in errors_impl.f90 so that editing them does not
	! invalidate this module's .mod and rebuild the tree -- only the
	! .smod changes, which fpm's dependents don't recompile on.
	interface

		module function err_pre(code) result(pre)
			character(len = *), intent(in) :: code
			character(len = :), allocatable :: pre
		end function err_pre

		module function err_rt_pre(code) result(pre)
			character(len = *), intent(in) :: code
			character(len = :), allocatable :: pre
		end function err_rt_pre

		module function err_int_pre(code) result(pre)
			character(len = *), intent(in) :: code
			character(len = :), allocatable :: pre
		end function err_int_pre

		module function warn_pre(code) result(pre)
			character(len = *), intent(in) :: code
			character(len = :), allocatable :: pre
		end function warn_pre

		module function err_int(code, msg) result(err)
			character(len = *), intent(in) :: code, msg
			character(len = :), allocatable :: err
		end function err_int

		module function err_rt(code, msg) result(err)
			character(len = *), intent(in) :: code, msg
			character(len = :), allocatable :: err
		end function err_rt

		module function get_all_error_codes() result(codes)
			type(string_vector_t) :: codes
		end function get_all_error_codes

		module function err_bad_i32(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_i32

		module function err_bad_i64(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_i64

		module function err_bad_hex32(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_hex32

		module function err_bad_hex64(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_hex64

		module function err_bad_oct32(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_oct32

		module function err_bad_oct64(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_oct64

		module function err_bad_bin32(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_bin32

		module function err_bad_bin64(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_bin64

		module function err_bad_expr(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_bad_expr

		module function err_unterminated_str(context, span, str_) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: str_
		end function err_unterminated_str

		module function err_unterminated_raw_str(context, span, str_) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: str_
		end function err_unterminated_raw_str

		module function err_array_struct_slice(context, span, array) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: array
		end function err_array_struct_slice

		module function err_struct_array_slice(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_struct_array_slice

		module function err_non_int_subscript(context, span, subscript) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: subscript
		end function err_non_int_subscript

		module function err_bad_f32(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_f32

		module function err_bad_f64(context, span, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: num
		end function err_bad_f64

		module function err_bad_type(context, span, type, suggest) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: type
			character(len = *), intent(in), optional :: suggest
		end function err_bad_type

		module function err_bad_type_suffix(context, span, type, literal_kind, allowed) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: type, literal_kind
			character(len = *), intent(in), optional :: allowed
		end function err_bad_type_suffix

		module function err_float_int_suffix(context, span, type, num) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: type, num
		end function err_float_int_suffix

		module function err_ref_type(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_ref_type

		module function err_unexpected_char(context, span, c) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: c
		end function err_unexpected_char

		module function err_unexpected_token(context, span, got, kind, expect) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: got, kind ,expect
		end function err_unexpected_token

		module function err_void_assign(context, span, var) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
		end function err_void_assign

		module function err_redeclare_var(context, span, var) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
		end function err_redeclare_var

		module function err_immutable_var(context, span, var) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
		end function err_immutable_var

		module function err_const_assign(context, span, var) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
		end function err_const_assign

		module function err_mutable_method_on_temp(context, span, method) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: method
		end function err_mutable_method_on_temp

		module function err_member_method_clash(context, span, name, struct_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: name, struct_name
		end function err_member_method_clash

		module function err_redeclare_mem(context, span, var) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
		end function err_redeclare_mem

		module function err_redeclare_fn(context, span, fn) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
		end function err_redeclare_fn

		module function err_redeclare_intr_fn(context, span, fn) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
		end function err_redeclare_intr_fn

		module function err_redeclare_struct(context, span, struct) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: struct
		end function err_redeclare_struct

		module function err_redeclare_enum(context, span, enum) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: enum
		end function err_redeclare_enum

		module function err_redeclare_variant(context, span, variant) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: variant
		end function err_redeclare_variant

		module function err_duplicate_enum_value(context, span, variant, other, value) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: variant, other
			integer, intent(in) :: value
		end function err_duplicate_enum_value

		module function err_unknown_variant(context, span, variant, enum, suggest) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: variant, enum
			character(len = *), intent(in), optional :: suggest
		end function err_unknown_variant

		module function err_enum_cast_range(context, span, enum, value) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: enum
			integer, intent(in) :: value
		end function err_enum_cast_range

		module function err_enum_index(context, span, enum) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: enum
		end function err_enum_index

		module function err_enum_name_value(context, span, enum) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: enum
		end function err_enum_name_value

		module function err_redeclare_primitive(context, span, struct) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: struct
		end function err_redeclare_primitive

		module function err_var_type_clash(context, span, var, type_kind) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var, type_kind
		end function err_var_type_clash

		module function err_undeclare_var(context, span, var, suggest) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var
			character(len = *), intent(in), optional :: suggest
		end function err_undeclare_var

		module function err_undeclare_fn(context, span, fn, suggest, module_prefix) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
			character(len = *), intent(in), optional :: suggest, module_prefix
		end function err_undeclare_fn

		module function err_std_only_fn(context, span, fn) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
		end function err_std_only_fn

		module function err_no_return(context, span, fn) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
		end function err_no_return

		module function err_module_return(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_module_return

		module function err_fn_ptr_unsupported(context, span, fn, reason) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn, reason
		end function err_fn_ptr_unsupported

		module function err_not_callable(context, span, var, type) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: var, type
		end function err_not_callable

		module function err_fn_ptr_array(context, span, elem) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: elem
		end function err_fn_ptr_array

		module function err_fn_ptr_struct_member(context, span, mem_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name
		end function err_fn_ptr_struct_member

		module function err_missing_return(context, span, fn) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn
		end function err_missing_return

		module function warn_missing_return(context, span, fn) result(warn)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: warn
			character(len = *), intent(in) :: fn
		end function warn_missing_return

		module function err_bad_arg_count(context, span, fn, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: expect, actual
			character(len = *), intent(in) :: fn
		end function err_bad_arg_count

		module function err_too_few_args(context, span, fn, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: expect, actual
			character(len = *), intent(in) :: fn
		end function err_too_few_args

		module function err_too_many_args(context, span, fn, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: expect, actual
			character(len = *), intent(in) :: fn
		end function err_too_many_args

		module function err_bad_sub_count(context, span, array, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: expect, actual
			character(len = *), intent(in) :: array
		end function err_bad_sub_count

		module function err_bad_sub_rank(context, span, rank_) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in) :: rank_
		end function err_bad_sub_rank

		module function err_empty_step(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_empty_step

		module function err_scalar_subscript(context, span, scalar) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: scalar
		end function err_scalar_subscript

		module function err_bad_cat_rank(context, span, rank_, arr) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in) :: rank_
			character(len = *), intent(in) :: arr
		end function err_bad_cat_rank

		module function err_bad_ret_type(context, span, fn, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: fn, expect, actual
		end function err_bad_ret_type

		module function err_bad_arg_type(context, span, fn, iarg, param, expect, actual) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: iarg
			character(len = *), intent(in) :: fn, param, expect, actual
		end function err_bad_arg_type

		module function err_void_arg(context, span, fn, iarg, param) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: iarg
			character(len = *), intent(in) :: fn, param
		end function err_void_arg

		module function err_bad_arg_val(context, span, fn, iarg, param) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: iarg
			character(len = *), intent(in) :: fn, param
		end function err_bad_arg_val

		module function err_bad_arg_ref(context, span, fn, iarg, param) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in):: iarg
			character(len = *), intent(in) :: fn, param
		end function err_bad_arg_ref

		module function err_non_name_ref(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_non_name_ref

		module function err_sub_ref(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_sub_ref

		module function err_binary_types(context, span, op, left, right) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: op, left, right
		end function err_binary_types

		module function err_binary_ranks(context, span, op, left, right) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: op
			integer, intent(in) :: left, right
		end function err_binary_ranks

		module function err_unary_types(context, span, op, right) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: op, right
		end function err_unary_types

		module function err_non_array_loop(context, span, range_) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: range_
		end function err_non_array_loop

		module function err_non_bool_condition(context, span, condition, statement) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: condition, statement
		end function err_non_bool_condition

		module function err_non_float_len_range(context, span, range) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: range
		end function err_non_float_len_range

		module function err_non_int_len(context, span, len) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: len
		end function err_non_int_len

		module function err_non_int_size(context, span, size) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: size
		end function err_non_int_size

		module function err_bound_type_mismatch(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_bound_type_mismatch

		module function err_non_num_range(context, span, range) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: range
		end function err_non_num_range

		module function err_non_sca_val(context, span, val, descriptor) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: val, descriptor
		end function err_non_sca_val

		module function err_non_int_range(context, span, range) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: range
		end function err_non_int_range

		module function err_het_array(context, span, elem) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: elem
		end function err_het_array

		module function err_unset_member(context, span, mem_name, struct_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, struct_name
		end function err_unset_member

		module function err_reset_member(context, span, mem_name, struct_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, struct_name
		end function err_reset_member

		module function err_non_struct_dot(context, span, ident) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: ident
		end function err_non_struct_dot

		module function err_bad_member_name(context, span, mem_name, struct_var_name, struct_name, suggest) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, struct_var_name, struct_name
			character(len = *), intent(in), optional :: suggest
		end function err_bad_member_name

		module function err_bad_member_name_short(context, span, mem_name, struct_name, suggest) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, struct_name
			character(len = *), intent(in), optional :: suggest
		end function err_bad_member_name_short

		module function err_bad_file_member(context, span, mem_name, file_var_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, file_var_name
		end function err_bad_file_member

		module function err_readonly_file_member(context, span, mem_name, file_var_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, file_var_name
		end function err_readonly_file_member

		module function err_expl_array_size(context, span, nelems, dims, total) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			integer, intent(in) :: nelems
			character(len = *), intent(in) :: dims
			integer(kind = 8), intent(in) :: total
		end function err_expl_array_size

		module function err_rt_expl_array_size(nelems, sizes) result(err)
			integer, intent(in) :: nelems
			integer(kind = 8), intent(in) :: sizes(:)
			character(len = :), allocatable :: err
		end function err_rt_expl_array_size

		module function err_bad_member_type(context, span, mem_name, struct_name, act_type, exp_type) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: mem_name, struct_name, exp_type, act_type
		end function err_bad_member_type

		module function err_inc_404(context, span, filename) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: filename
		end function err_inc_404

		module function err_inc_read(context, span, filename) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: filename
		end function err_inc_read

		module function err_mod_404(context, span, filename) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: filename
		end function err_mod_404

		module function err_mod_read(context, span, filename) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
			character(len = *), intent(in) :: filename
		end function err_mod_read

		module function err_circular_import(context, span, module_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = *), intent(in) :: module_name
			character(len = :), allocatable :: err
		end function err_circular_import

		module function err_duplicate_import(context, span, module_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = *), intent(in) :: module_name
			character(len = :), allocatable :: err
		end function err_duplicate_import

		module function err_mod_hyphen(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_mod_hyphen

		module function err_mod_keyword(context, span, keyword) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = *), intent(in) :: keyword
			character(len = :), allocatable :: err
		end function err_mod_keyword

		module function err_mod_reserved_std(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_mod_reserved_std

		module function err_mod_space(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_mod_space

		module function err_alias_keyword(context, span, alias_name) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = *), intent(in) :: alias_name
			character(len = :), allocatable :: err
		end function err_alias_keyword

		module function err_alias_reserved_std(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_alias_reserved_std

		module function err_alias_hyphen(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_alias_hyphen

		module function err_alias_space(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_alias_space

		module function err_alias_with_doublecolon(context, span) result(err)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: err
		end function err_alias_with_doublecolon

		module function err_404(filename) result(err)
			character(len = *), intent(in) :: filename
			character(len = :), allocatable :: err
		end function err_404

		module function err_eval_unary_type(op) result(err)
			character(len = *), intent(in) :: op
			character(len = :), allocatable :: err
		end function err_eval_unary_type

		module function err_eval_binary_types(op) result(err)
			character(len = *), intent(in) :: op
			character(len = :), allocatable :: err
		end function err_eval_binary_types

		module function err_matmul_dim(lsize, rsize) result(err)
			integer(kind = 8), intent(in) :: lsize, rsize
			character(len = :), allocatable :: err
		end function err_matmul_dim

		module function err_eval_len_array(type_name) result(err)
			character(len = *), intent(in) :: type_name
			character(len = :), allocatable :: err
		end function err_eval_len_array

		module function err_eval_unary_op(op) result(err)
			character(len = *), intent(in) :: op
			character(len = :), allocatable :: err
		end function err_eval_unary_op

		module function err_eval_binary_op(op) result(err)
			character(len = *), intent(in) :: op
			character(len = :), allocatable :: err
		end function err_eval_binary_op

		module function new_span(start, length) result(span)
			integer, intent(in) :: start, length
			type(text_span_t) :: span
		end function new_span

		module function new_context(text, src_file, lines) result(context)
			character(len = *), intent(in) :: text, src_file
			integer, intent(in) :: lines(:)
			type(text_context_t) :: context
		end function new_context

		module function new_context_vector() result(vector)
			type(text_context_vector_t) :: vector
		end function new_context_vector

		module subroutine push_context(vector, val)
			class(text_context_vector_t) :: vector
			type(text_context_t) :: val
		end subroutine push_context

		module function underline(context, span)
			type(text_context_t) :: context
			type(text_span_t), intent(in) :: span
			character(len = :), allocatable :: underline
		end function underline

		module subroutine internal_error()
		end subroutine internal_error

	end interface

end module syntran__errors_m

!===============================================================================

