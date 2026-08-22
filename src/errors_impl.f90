
!===============================================================================

submodule (syntran__errors_m) syntran__errors_impl_m

	implicit none

contains


!===============================================================================

module procedure err_pre
	! Coded prefix for a compile-time error, e.g. "Error[E42]: "
	pre = fg_bold_bright_red//'Error['//code//']'//fg_bold//': '
end procedure err_pre


!===============================================================================

module procedure err_rt_pre
	! Coded prefix for a runtime error, e.g. "Runtime error[R3]: "
	pre = fg_bold_bright_red//'Runtime error['//code//']'//fg_bold//': '
end procedure err_rt_pre


!===============================================================================

module procedure err_int_pre
	! Coded prefix for an internal "should never happen" error, e.g.
	! "Internal syntran error[I7]: "
	pre = fg_bold_bright_red//'Internal syntran error['//code//']'//fg_bold//': '
end procedure err_int_pre


!===============================================================================

module procedure warn_pre
	! Coded prefix for a warning, e.g. "Warning[W1]: "
	pre = fg_bold_yellow//'Warning['//code//']'//fg_bold//': '
end procedure warn_pre


!===============================================================================

module procedure err_int
	! Generic internal-error constructor for inline call sites (eval_*, vm_*,
	! value.f90, etc.) that don't have source context/span to underline.  Each
	! call site supplies its own unique code and message text
	err = err_int_pre(code)//msg//color_reset
end procedure err_int


!===============================================================================

module procedure err_rt
	! Generic runtime-error constructor for inline call sites.  Each call site
	! supplies its own unique code and message text
	err = err_rt_pre(code)//msg//color_reset
end procedure err_rt


!===============================================================================

module procedure get_all_error_codes
	! Every error/warning code in the registry.  Used by the error-code unit
	! test to check uniqueness and format.  Keep this in sync with the
	! `parameter` declarations above
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
	call codes%push(EC_IMMUTABLE_VAR)
	call codes%push(EC_CONST_ASSIGN)
	call codes%push(EC_MUTABLE_METHOD_ON_TEMP)
	call codes%push(EC_MEMBER_METHOD_CLASH)
	call codes%push(EC_MODULE_RETURN)
	call codes%push(EC_FN_PTR_UNSUPPORTED)
	call codes%push(EC_NOT_CALLABLE)
	call codes%push(EC_FN_PTR_ARRAY)
	call codes%push(EC_FN_PTR_STRUCT_MEMBER)
	call codes%push(EC_VOID_ARG)
	call codes%push(EC_REDECLARE_ENUM)
	call codes%push(EC_REDECLARE_VARIANT)
	call codes%push(EC_UNKNOWN_VARIANT)
	call codes%push(EC_DUPLICATE_ENUM_VALUE)
	call codes%push(EC_ENUM_CAST_RANGE)
	call codes%push(EC_ENUM_INDEX)
	call codes%push(EC_VAR_TYPE_CLASH)
	call codes%push(EC_ENUM_NAME_VALUE)
	call codes%push(EC_BAD_FILE_MEMBER)
	call codes%push(EC_READONLY_FILE_MEMBER)
	call codes%push(EC_EXPL_ARRAY_SIZE)
	call codes%push(EC_NON_INT_SIZE)
	call codes%push(EC_FLOAT_INT_SUFFIX)
	call codes%push(EC_REF_TYPE)
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
	call codes%push(IC_CONVERT_F32_ARR)
	call codes%push(IC_CONVERT_F64_ARR)
	call codes%push(IC_TRANSPOSE_ARRAY_TYPE)
	call codes%push(IC_FILE_MEMBER)
	call codes%push(IC_MISSING_RECV_SLOTS)
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
	call codes%push(RC_ARRAY_STEP_ZERO)
	call codes%push(RC_ARRAY_STEP_ZERO_F)
	call codes%push(RC_SUBSCRIPT_STEP_ZERO)
	call codes%push(RC_CLOSE_STANDARD)
	call codes%push(RC_WRITELN_FAIL)
	call codes%push(RC_CLOSE_FAIL)
	call codes%push(RC_ENUM_CAST_RANGE)
	call codes%push(RC_SUBSCRIPT_OOB)
	call codes%push(WC_MISSING_RETURN)
end procedure get_all_error_codes


!===============================================================================

module procedure err_bad_i32

	err = err_pre(EC_BAD_I32)//'bad i32 integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end procedure err_bad_i32


!===============================================================================

module procedure err_bad_i64

	err = err_pre(EC_BAD_I64)//'bad i64 integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad integer'//color_reset

end procedure err_bad_i64


!===============================================================================

module procedure err_bad_hex32

	err = err_pre(EC_BAD_HEX32)//'bad hexadecimal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end procedure err_bad_hex32


!===============================================================================

module procedure err_bad_hex64

	err = err_pre(EC_BAD_HEX64)//'bad hexadecimal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad hex integer'//color_reset

end procedure err_bad_hex64


!===============================================================================

module procedure err_bad_oct32

	err = err_pre(EC_BAD_OCT32)//'bad octal integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end procedure err_bad_oct32


!===============================================================================

module procedure err_bad_oct64

	err = err_pre(EC_BAD_OCT64)//'bad octal integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad octal integer'//color_reset

end procedure err_bad_oct64


!===============================================================================

module procedure err_bad_bin32

	err = err_pre(EC_BAD_BIN32)//'bad binary integer `'//num &
		//'` does not fit in 32 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end procedure err_bad_bin32


!===============================================================================

module procedure err_bad_bin64

	err = err_pre(EC_BAD_BIN64)//'bad binary integer `'//num &
		//'` does not fit in 64 bits' &
		//underline(context, span) &
		//' bad binary integer'//color_reset

end procedure err_bad_bin64


!===============================================================================

module procedure err_bad_expr

	err = err_pre(EC_BAD_EXPR)//'bad expression.  ' &
		//'Expression statements are only allowed in the REPL.  ' &
		//'Use `println()` to log its value' &
		//underline(context, span) &
		//' bad expression statement'//color_reset

end procedure err_bad_expr


!===============================================================================

module procedure err_unterminated_str

	err = err_pre(EC_UNTERMINATED_STR)//'unterminated str literal `'//str_ &
		//'`' &
		//underline(context, span) &
		//' unterminated str'//color_reset

end procedure err_unterminated_str


!===============================================================================

module procedure err_unterminated_raw_str

	err = err_pre(EC_UNTERMINATED_RAW_STR)//'unterminated raw str literal `'//str_ &
		//'`' &
		//underline(context, span) &
		//' unterminated raw str'//color_reset

end procedure err_unterminated_raw_str


!===============================================================================

module procedure err_array_struct_slice


	err = err_pre(EC_ARRAY_STRUCT_SLICE) &
		//'slices are not implemented for arrays of structs, on array `' &
		//array//'`' &
		//underline(context, span) &
		//" slice subscript not implemented"//color_reset

end procedure err_array_struct_slice


!===============================================================================

module procedure err_struct_array_slice

	err = err_pre(EC_STRUCT_ARRAY_SLICE)//'slices are not implemented for structs of arrays.  ' &
		//'Only scalar subscripts can be used here' &
		//underline(context, span) &
		//' slice subscript not implemented'//color_reset

end procedure err_struct_array_slice


!===============================================================================

module procedure err_non_int_subscript

	err = err_pre(EC_NON_INT_SUBSCRIPT)//'array subscript `'//subscript &
		//'` is not an integer' &
		//underline(context, span) &
		//' non-integer subscript'//color_reset

end procedure err_non_int_subscript


!===============================================================================

module procedure err_bad_f32

	err = err_pre(EC_BAD_F32)//'bad f32 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end procedure err_bad_f32


!===============================================================================

module procedure err_bad_f64

	err = err_pre(EC_BAD_F64)//'bad f64 number `'//num//'`' &
		//underline(context, span) &
		//' bad real number'//color_reset

end procedure err_bad_f64


!===============================================================================

module procedure err_bad_type


	err = err_pre(EC_BAD_TYPE)//'bad type annotation `'//type//'`' &
		//underline(context, span) &
		//' bad type'//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end procedure err_bad_type


!===============================================================================

module procedure err_bad_type_suffix


	err = err_pre(EC_BAD_TYPE_SUFFIX)//'bad literal type suffix `'//type//'` after ' &
		//literal_kind//' literal' &
		//underline(context, span) &
		//' bad type suffix'//color_reset

	if (present(allowed)) then
		err = err//line_feed &
			//fg_bright_green//"help"//color_reset &
			//": valid suffixes after a "//literal_kind//" literal are " &
			//allowed
	end if

end procedure err_bad_type_suffix


!===============================================================================

module procedure err_float_int_suffix

	err = err_pre(EC_FLOAT_INT_SUFFIX)//'integer type suffix `'//type &
		//'` on float literal `'//num//'`' &
		//underline(context, span) &
		//' float literal with int suffix'//color_reset

end procedure err_float_int_suffix


!===============================================================================

module procedure err_ref_type

	err = err_pre(EC_REF_TYPE)//'`&` reference not allowed in this type annotation' &
		//underline(context, span) &
		//' reference type'//color_reset &
		//line_feed &
		//fg_bright_green//"help"//color_reset &
		//": references are only allowed on fn parameters"

end procedure err_ref_type


!===============================================================================

module procedure err_unexpected_char

	err = err_pre(EC_UNEXPECTED_CHAR) &
		//"unexpected character `"//c//"`"//underline(context, span) &
		//" unexpected character"//color_reset

end procedure err_unexpected_char


!===============================================================================

module procedure err_unexpected_token

	err = err_pre(EC_UNEXPECTED_TOKEN) &
		//'unexpected token `'//got//'` of kind `'//kind &
		//'`, expected `'//expect//'`'//underline(context, span) &
		//" unexpected token"//color_reset

end procedure err_unexpected_token


!===============================================================================

module procedure err_void_assign

	err = err_pre(EC_VOID_ASSIGN) &
		//'variable `'//var//'` cannot be initialized to void type' &
		//underline(context, span)//" void RHS type"//color_reset

end procedure err_void_assign


!===============================================================================

module procedure err_redeclare_var

	err = err_pre(EC_REDECLARE_VAR) &
		//'variable `'//var//'` has already been declared in this scope' &
		//underline(context, span)//" variable already declared"//color_reset

end procedure err_redeclare_var


!===============================================================================

module procedure err_immutable_var

	err = err_pre(EC_IMMUTABLE_VAR) &
		//'`'//var//'` is constant' &
		//underline(context, span)//" cannot assign to std:: constant"//color_reset

end procedure err_immutable_var


!===============================================================================

module procedure err_const_assign

	err = err_pre(EC_CONST_ASSIGN) &
		//'`'//var//'` is declared const' &
		//underline(context, span)//" cannot assign to const variable"//color_reset

end procedure err_const_assign


!===============================================================================

module procedure err_mutable_method_on_temp

	err = err_pre(EC_MUTABLE_METHOD_ON_TEMP) &
		//'cannot call mutable method `'//method//'` on a temporary value' &
		//underline(context, span)//" add `const` to the method declaration"//color_reset

end procedure err_mutable_method_on_temp


!===============================================================================

module procedure err_member_method_clash

	err = err_pre(EC_MEMBER_METHOD_CLASH) &
		//'method `'//name//'` conflicts with a member of the same name in struct `' &
		//struct_name//'`' &
		//underline(context, span)//" name already used by a member"//color_reset

end procedure err_member_method_clash


!===============================================================================

module procedure err_redeclare_mem

	err = err_pre(EC_REDECLARE_MEM) &
		//'member `'//var//'` has already been declared in this struct' &
		//underline(context, span)//" member already declared"//color_reset

end procedure err_redeclare_mem


!===============================================================================

module procedure err_redeclare_fn

	err = err_pre(EC_REDECLARE_FN) &
		//'function `'//fn//'` has already been declared' &
		//underline(context, span)//" function already declared"//color_reset

end procedure err_redeclare_fn


module procedure err_redeclare_intr_fn

	err = err_pre(EC_REDECLARE_INTR_FN) &
		//'function `'//fn//'` is already a built-in function' &
		//underline(context, span)//" function already exists"//color_reset

end procedure err_redeclare_intr_fn


!===============================================================================

module procedure err_redeclare_struct

	err = err_pre(EC_REDECLARE_STRUCT) &
		//'struct `'//struct//'` has already been declared' &
		//underline(context, span)//" struct already declared"//color_reset

end procedure err_redeclare_struct


!===============================================================================

module procedure err_redeclare_enum

	err = err_pre(EC_REDECLARE_ENUM) &
		//'enum `'//enum//'` has already been declared' &
		//underline(context, span)//" enum already declared"//color_reset

end procedure err_redeclare_enum


!===============================================================================

module procedure err_redeclare_variant

	err = err_pre(EC_REDECLARE_VARIANT) &
		//'variant `'//variant//'` has already been declared in this enum' &
		//underline(context, span)//" variant already declared"//color_reset

end procedure err_redeclare_variant


!===============================================================================

module procedure err_duplicate_enum_value


	err = err_pre(EC_DUPLICATE_ENUM_VALUE) &
		//'variant `'//variant//'` reuses value '//str(value) &
		//', already assigned to `'//other//'`' &
		//underline(context, span)//" duplicate enum value" &
		//color_reset &
		//line_feed &
		//fg_bright_green//"help"//color_reset &
		//": only explicitly-valued variants may share a value; " &
		//"assign `"//variant//"` an explicit value to alias `"//other//"`"

end procedure err_duplicate_enum_value


!===============================================================================

module procedure err_unknown_variant


	err = err_pre(EC_UNKNOWN_VARIANT) &
		//'variant `'//variant//'` does not exist in enum `'//enum//'`' &
		//underline(context, span) &
		//" unknown variant"//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end procedure err_unknown_variant


!===============================================================================

module procedure err_enum_cast_range


	err = err_pre(EC_ENUM_CAST_RANGE) &
		//'no variant with value '//str(value)//' in enum `'//enum//'`' &
		//underline(context, span) &
		//" out-of-range enum cast"//color_reset

end procedure err_enum_cast_range


!===============================================================================

module procedure err_enum_index


	err = err_pre(EC_ENUM_INDEX) &
		//'cannot index enum type `'//enum//'`' &
		//underline(context, span) &
		//" enum type is not subscriptable"//color_reset &
		//line_feed &
		//fg_bright_green//"help"//color_reset &
		//": use `"//enum//"(ordinal)` to cast an integer ordinal to a variant"

end procedure err_enum_index


!===============================================================================

module procedure err_enum_name_value


	err = err_pre(EC_ENUM_NAME_VALUE) &
		//'enum type `'//enum//'` cannot be used as a value' &
		//underline(context, span) &
		//" enum type is not a value"//color_reset &
		//line_feed &
		//fg_bright_green//"help"//color_reset &
		//": a bare enum name is only valid as a `for` iterable or an " &
		//"argument to size()/str()/println()"

end procedure err_enum_name_value


!===============================================================================

module procedure err_redeclare_primitive

	err = err_pre(EC_REDECLARE_PRIMITIVE) &
		//'struct name `'//struct//'` is reserved for a primitive type' &
		//underline(context, span)//" cannot redeclare primitives"//color_reset

end procedure err_redeclare_primitive


!===============================================================================

module procedure err_var_type_clash

	character(len = :), allocatable :: article

	article = "a"
	if (type_kind == "enum") article = "an"

	err = err_pre(EC_VAR_TYPE_CLASH) &
		//'variable `'//var//'` conflicts with the '//type_kind &
		//' of the same name' &
		//underline(context, span)//" name already used by "//article &
		//" "//type_kind &
		//color_reset

end procedure err_var_type_clash


!===============================================================================

module procedure err_undeclare_var


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

end procedure err_undeclare_var


!===============================================================================

module procedure err_undeclare_fn


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

	if (present(module_prefix)) then
		err = err//line_feed &
			//fg_bright_green//"help"//color_reset &
			//": did you `use` module `"//fg_bright_green//module_prefix//color_reset//"`?"
	end if

end procedure err_undeclare_fn


!===============================================================================

module procedure err_std_only_fn

	err = err_pre(EC_STD_ONLY_FN) &
		//'function `'//fn//'` must be called with std:: prefix (use `std::'//fn//'()`)' &
		//underline(context, span)//" requires std:: prefix"//color_reset

end procedure err_std_only_fn


!===============================================================================

module procedure err_no_return

	err = err_pre(EC_NO_RETURN) &
		//'function `'//fn//'` does not have any return statements' &
		//underline(context, span)//" function without returns"//color_reset

end procedure err_no_return


module procedure err_module_return

	err = err_pre(EC_MODULE_RETURN) &
		//'`return` is not allowed at the top level of an imported module' &
		//underline(context, span)//" move it into a function"//color_reset

end procedure err_module_return


!===============================================================================

module procedure err_fn_ptr_unsupported

	err = err_pre(EC_FN_PTR_UNSUPPORTED) &
		//'cannot take a function pointer to `'//fn//'`: '//reason &
		//underline(context, span)//" not fn-pointer-able"//color_reset

end procedure err_fn_ptr_unsupported


!===============================================================================

module procedure err_not_callable

	err = err_pre(EC_NOT_CALLABLE) &
		//'variable `'//var//'` of type `'//type//'` is not callable' &
		//underline(context, span)//" not a fn pointer"//color_reset

end procedure err_not_callable


!===============================================================================

module procedure err_fn_ptr_array

	err = err_pre(EC_FN_PTR_ARRAY) &
		//'array element `'//elem//'` is a fn pointer.  ' &
		//'Arrays of fn pointers are not supported' &
		//underline(context, span)//" fn pointer in array literal"//color_reset

end procedure err_fn_ptr_array


!===============================================================================

module procedure err_fn_ptr_struct_member

	err = err_pre(EC_FN_PTR_STRUCT_MEMBER) &
		//'struct member `'//mem_name//'` is a fn pointer.  ' &
		//'Fn pointers cannot be struct members' &
		//underline(context, span)//" fn pointer in struct member"//color_reset

end procedure err_fn_ptr_struct_member


!===============================================================================

module procedure err_missing_return

	err = err_pre(EC_MISSING_RETURN) &
		//'not all code paths in function `'//fn//'` return' &
		//underline(context, span)//" function may not return on all paths" &
		//color_reset

end procedure err_missing_return


!===============================================================================

module procedure warn_missing_return

	warn = warn_pre(WC_MISSING_RETURN) &
		//'not all code paths in function `'//fn//'` return' &
		//underline(context, span)//" function may not return on all paths" &
		//color_reset

end procedure warn_missing_return


!===============================================================================

module procedure err_bad_arg_count
	character(len = :), allocatable :: argument_s


	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_BAD_ARG_COUNT) &
		//'function `'//fn//'` requires '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong argument count"//color_reset

end procedure err_bad_arg_count


!===============================================================================

module procedure err_too_few_args
	character(len = :), allocatable :: argument_s


	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_TOO_FEW_ARGS) &
		//'variadic function `'//fn//'` requires at least '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" not enough arguments"//color_reset

end procedure err_too_few_args


!===============================================================================

module procedure err_too_many_args
	character(len = :), allocatable :: argument_s


	if (expect == 1) then
		argument_s = 'argument'
	else
		argument_s = 'arguments'
	end if

	err = err_pre(EC_TOO_MANY_ARGS) &
		//'variadic function `'//fn//'` requires at most '//str(expect) &
		//' '//argument_s//' but was given '//str(actual) &
		//underline(context, span)//" too many arguments"//color_reset

end procedure err_too_many_args


!===============================================================================

module procedure err_bad_sub_count
	character(len = :), allocatable :: subscript_s


	if (expect == 1) then
		subscript_s = 'subscript'
	else
		subscript_s = 'subscripts'
	end if

	err = err_pre(EC_BAD_SUB_COUNT) &
		//'array `'//array//'` requires '//str(expect) &
		//' '//subscript_s//' but was given '//str(actual) &
		//underline(context, span)//" wrong subscript count"//color_reset

end procedure err_bad_sub_count


!===============================================================================

module procedure err_bad_sub_rank



	err = err_pre(EC_BAD_SUB_RANK) &
		//"subscript index array of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector subscript"//color_reset

end procedure err_bad_sub_rank


!===============================================================================

module procedure err_empty_step

	err = err_pre(EC_EMPTY_STEP) &
		//'slice step cannot be omitted between two colons' &
		//underline(context, span)//" write the step explicitly"//color_reset

end procedure err_empty_step


!===============================================================================

module procedure err_scalar_subscript


	err = err_pre(EC_SCALAR_SUBSCRIPT) &
		//'scalar `'//scalar//'` cannot have subscripts' &
		//underline(context, span)//" unexpected subscripts"//color_reset

end procedure err_scalar_subscript


!===============================================================================

module procedure err_bad_cat_rank




	err = err_pre(EC_BAD_CAT_RANK) &
		//"concatenated array `"//arr//"` of rank-"//str(rank_)//" is not rank-1" &
		//underline(context, span)//" non-vector concatenation"//color_reset

end procedure err_bad_cat_rank


!===============================================================================

module procedure err_bad_ret_type



	err = err_pre(EC_BAD_RET_TYPE) &
		//'function `'//fn &
		//'` requires return value of '//expect//' but returns a value of ' &
		//actual &
		//underline(context, span)//" wrong return type"//color_reset

end procedure err_bad_ret_type


!===============================================================================

module procedure err_bad_arg_type



	err = err_pre(EC_BAD_ARG_TYPE) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires type `'//expect//'` but was given `' &
		//actual//'`' &
		//underline(context, span)//" wrong argument type"//color_reset

end procedure err_bad_arg_type


!===============================================================================

module procedure err_void_arg



	err = err_pre(EC_VOID_ARG) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` was given a void (no return value) argument' &
		//underline(context, span)//" void argument"//color_reset

end procedure err_void_arg


!===============================================================================

module procedure err_bad_arg_val



	err = err_pre(EC_BAD_ARG_VAL) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a `&` reference but was given a value argument' &
		//underline(context, span)//" missing `&` ref"//color_reset

end procedure err_bad_arg_val


!===============================================================================

module procedure err_bad_arg_ref



	err = err_pre(EC_BAD_ARG_REF) &
		//'function `'//fn//'` parameter '//str(iarg)//' `'//param &
		//'` requires a value but was given a `&` reference argument' &
		//underline(context, span)//" bad `&` ref"//color_reset

end procedure err_bad_arg_ref


!===============================================================================

module procedure err_non_name_ref


	err = err_pre(EC_NON_NAME_REF) &
		//'`&` reference to unexpected expression kind.  references can only ' &
		//'be made to variable name expressions' &
		//underline(context, span)//" non-name `&` ref"//color_reset

end procedure err_non_name_ref


!===============================================================================

module procedure err_sub_ref


	err = err_pre(EC_SUB_REF) &
		//'`&` reference to unexpected subscripted expression.  references can only ' &
		//'be made to name expressions without subscripts' &
		//underline(context, span)//" subscripted `&` ref"//color_reset

end procedure err_sub_ref


!===============================================================================

! EC_BAD_ARG_RANK (E47) is formally retired: its constructor was deleted
! because it was never wired up to any call site, but the code itself stays
! registered in get_all_error_codes() forever and must never be reused (see
! the permanence policy at the top of this file and in doc/errors.md)

! IC_FOR_STEP_ZERO/_F (I8/I9), IC_ARRAY_STEP_ZERO/_F (I14/I15), and
! IC_SUBSCRIPT_STEP_ZERO (I20) are likewise formally retired.  They were
! reachable from ordinary syntran code (a runtime-valued step of 0), so their
! err_int()/internal_error() call sites were replaced with rt_throw()/err_rt()
! using new runtime codes RC_FOR_STEP_ZERO/_F, RC_ARRAY_STEP_ZERO/_F, and
! RC_SUBSCRIPT_STEP_ZERO instead.  The five IC_* codes stay registered forever
! per the permanence policy and must never be reused

!===============================================================================

module procedure err_binary_types


	!print *, 'starting err_binary_types'

	err = err_pre(EC_BINARY_TYPES) &
		//'binary operator `'//op//'` is not defined for types ' &
		//left//' and '//right//underline(context, span) &
		//" wrong types for this binary operator"//color_reset

end procedure err_binary_types


!===============================================================================

module procedure err_binary_ranks


	!print *, 'starting err_binary_ranks'

	err = err_pre(EC_BINARY_RANKS) &
		//'rank mismatch for binary operator `'//op//'` with ranks ' &
		//str(left)//' and '//str(right)//underline(context, span) &
		//" array rank mismatch"//color_reset

end procedure err_binary_ranks


!===============================================================================

module procedure err_unary_types

	err = err_pre(EC_UNARY_TYPES) &
		//'unary operator `'//op//'` is not defined for type ' &
		//right//underline(context, span) &
		//" wrong type for this unary operator"//color_reset

end procedure err_unary_types


!===============================================================================

module procedure err_non_array_loop

	err = err_pre(EC_NON_ARRAY_LOOP) &
		//'range `'//trimw(range_)//'` of for loop is not an array' &
		//underline(context, span) &
		//" non-array range"//color_reset

end procedure err_non_array_loop


!===============================================================================

module procedure err_non_bool_condition

	err = err_pre(EC_NON_BOOL_CONDITION) &
		//'condition `'//trimw(condition)//'` of '//statement//' is not bool' &
		//underline(context, span) &
		//" non-bool condition"//color_reset

end procedure err_non_bool_condition


!===============================================================================

module procedure err_non_float_len_range

	err = err_pre(EC_NON_FLOAT_LEN_RANGE) &
		//'bound `'//range//'` of length-based array range is not a float' &
		//underline(context, span) &
		//" non-float bound"//color_reset

end procedure err_non_float_len_range


!===============================================================================

module procedure err_non_int_len

	err = err_pre(EC_NON_INT_LEN) &
		//'length `'//len//'` of array is not an integer' &
		//underline(context, span) &
		//" non-int length"//color_reset

end procedure err_non_int_len


!===============================================================================

module procedure err_non_int_size

	err = err_pre(EC_NON_INT_SIZE) &
		//'size `'//size//'` of array dimension is not an integer' &
		//underline(context, span) &
		//" non-int size"//color_reset

end procedure err_non_int_size


!===============================================================================

module procedure err_bound_type_mismatch

	err = err_pre(EC_BOUND_TYPE_MISMATCH) &
		//'types of array range bounds do not match' &
		//underline(context, span) &
		//" mismatched types"//color_reset

end procedure err_bound_type_mismatch


!===============================================================================

module procedure err_non_num_range


	! This language will be technically wrong if I add a complex number type
	! (complex numbers are numbers, but un-ordered and hence unsuitable for
	! ranges)
	err = err_pre(EC_NON_NUM_RANGE) &
		//'bound `'//range//'` of array range is not a numeric type' &
		//underline(context, span) &
		//" non-numeric range"//color_reset

end procedure err_non_num_range


!===============================================================================

module procedure err_non_sca_val


	err = err_pre(EC_NON_SCA_VAL) &
		//'value `'//val//'` of '//descriptor//' array is not a scalar' &
		//underline(context, span) &
		//" non-scalar array value"//color_reset

end procedure err_non_sca_val


!===============================================================================

module procedure err_non_int_range

	err = err_pre(EC_NON_INT_RANGE) &
		//'bound `'//range//'` of array range is not an integer' &
		//underline(context, span) &
		//" non-integer range"//color_reset

end procedure err_non_int_range


!===============================================================================

module procedure err_het_array

	err = err_pre(EC_HET_ARRAY) &
		//'array is heterogeneous.  Element `'//elem  &
		//"` does not match the first element's type" &
		//underline(context, span) &
		//" heterogeneous array element"//color_reset

end procedure err_het_array


!===============================================================================

module procedure err_unset_member

	err = err_pre(EC_UNSET_MEMBER) &
		//'not all members in struct `'//struct_name//'` are initialized.  ' &
		//'Member `'//mem_name//'` is uninitialized' &
		//underline(context, span) &
		//" uninitialized member(s)"//color_reset

end procedure err_unset_member


!===============================================================================

module procedure err_reset_member

	err = err_pre(EC_RESET_MEMBER) &
		//'member `'//mem_name//'` is already initialized in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" duplicate member"//color_reset

end procedure err_reset_member


!===============================================================================

module procedure err_non_struct_dot

	err = err_pre(EC_NON_STRUCT_DOT) &
		//'dot member access cannot be performed on non-struct variable `' &
		//ident//'`' &
		//underline(context, span) &
		//" dot on a non-struct"//color_reset

end procedure err_non_struct_dot


!===============================================================================

module procedure err_bad_member_name

	! This msg yells about both the variable name `struct_var_name` and its
	! "class" `struct_name`.  Its useful for dot expressions `var.mem`


	err = err_pre(EC_BAD_MEMBER_NAME) &
		//'member `'//mem_name//'` does not exist in struct `'//struct_var_name//'`' &
		//' of type `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end procedure err_bad_member_name


!===============================================================================

module procedure err_bad_member_name_short

	! This msg yells only about the "class" `struct_name`.  Its useful for
	! struct instantiations as in `return Class{mem = val};` where there may not
	! be a variable identifier like in the longer fn above


	err = err_pre(EC_BAD_MEMBER_NAME_SHORT) &
		//'member `'//mem_name//'` does not exist in struct `'//struct_name//'`' &
		//underline(context, span) &
		//" bad member name"//color_reset

	if (present(suggest)) then
		if (len(suggest) > 0) then
			err = err//line_feed &
				//fg_bright_green//"help"//color_reset &
				//": did you mean `" &
				//fg_bright_green//suggest//color_reset//"`?"
		end if
	end if

end procedure err_bad_member_name_short


!===============================================================================

module procedure err_bad_file_member

	err = err_pre(EC_BAD_FILE_MEMBER) &
		//'member `'//mem_name//'` does not exist on file handle `'//file_var_name//'`' &
		//underline(context, span) &
		//" bad file member"//color_reset &
		//line_feed &
		//fg_bright_green//"help"//color_reset &
		//": file handles have members `is_open`, `eof`, and `name`"

end procedure err_bad_file_member


!===============================================================================

module procedure err_readonly_file_member

	err = err_pre(EC_READONLY_FILE_MEMBER) &
		//'file handle member `'//file_var_name//'.'//mem_name//'` is read-only' &
		//underline(context, span) &
		//" cannot assign to a file member"//color_reset

end procedure err_readonly_file_member


!===============================================================================

module procedure err_expl_array_size
	! A rank-2+ array literal `[e0, e1, ... ; d0, d1, ...]` whose size list is
	! all literal constants doesn't have to wait for runtime (R21) to catch a
	! mismatched element count -- it's caught here at parse time instead


	err = err_pre(EC_EXPL_ARRAY_SIZE) &
		//'explicit array has '//str(nelems)//' elements but declared size is ' &
		//dims//' = '//str(total) &
		//underline(context, span)//" element count does not match size"//color_reset

end procedure err_expl_array_size


!===============================================================================

module procedure err_rt_expl_array_size
	! Runtime (R21) counterpart to err_expl_array_size()'s parse-time E102.
	! Shared by eval_array_expr() and the bytecode VM's OP_FOR_SETUP
	! (size_array case) so their messages can't drift apart

	integer :: i
	integer(kind = 8) :: total
	character(len = :), allocatable :: dims

	dims  = ''
	total = 1
	do i = 1, size(sizes)
		total = total * sizes(i)
		if (i > 1) dims = dims//' x '
		dims = dims//str(sizes(i))
	end do

	err = err_rt(RC_ARRAY_SIZE_MISMATCH, &
		"explicit array has "//str(nelems)// &
		" elements but declared size is "//dims//" = "//str(total))

end procedure err_rt_expl_array_size


!===============================================================================

module procedure err_rt_subscript_oob
	! idim/rank are both 1-based; rank == 1 omits the "of N" clause since it's
	! redundant for the common scalar/rank-1 case
	if (rank == 1) then
		err = err_rt(RC_SUBSCRIPT_OOB, &
			"subscript "//str(sub)//" is out of bounds for size "//str(sz))
	else
		err = err_rt(RC_SUBSCRIPT_OOB, &
			"subscript "//str(sub)//" is out of bounds for dimension "// &
			str(idim)//" of "//str(rank)//" (size "//str(sz)//")")
	end if
end procedure err_rt_subscript_oob


!===============================================================================

module procedure err_rt_str_index_oob
	err = err_rt(RC_SUBSCRIPT_OOB, &
		"string index "//str(sub)//" is out of bounds for length "//str(len_))
end procedure err_rt_str_index_oob


!===============================================================================

module procedure err_bad_member_type

	err = err_pre(EC_BAD_MEMBER_TYPE) &
		//'member `'//mem_name//'` in struct `'//struct_name//'` has the wrong type.  ' &
		//'Member requires type `'//exp_type//'` but was given `'//act_type//'`' &
		//underline(context, span) &
		//" bad member type"//color_reset

end procedure err_bad_member_type


!===============================================================================

module procedure err_inc_404

	err = err_pre(EC_INC_404) &
		//'#include file `'//filename//'` not found' &
		//underline(context, span) &
		//" file not found"//color_reset

end procedure err_inc_404


!===============================================================================

module procedure err_inc_read

	err = err_pre(EC_INC_READ) &
		//'#include file `'//filename//'` cannot be read' &
		//underline(context, span) &
		//" cannot read file"//color_reset

end procedure err_inc_read


!===============================================================================

module procedure err_mod_404

	err = err_pre(EC_MOD_404) &
		//'module file `'//filename//'` not found' &
		//underline(context, span) &
		//" file not found"//color_reset

end procedure err_mod_404


!===============================================================================

module procedure err_mod_read

	err = err_pre(EC_MOD_READ) &
		//'module file `'//filename//'` cannot be read' &
		//underline(context, span) &
		//" cannot read file"//color_reset

end procedure err_mod_read


!===============================================================================

module procedure err_circular_import
	err = err_pre(EC_CIRCULAR_IMPORT) &
		//'circular `use` dependency on `'//module_name//'`' &
		//underline(context, span) &
		//" circular `use`"//color_reset
end procedure err_circular_import


!===============================================================================

module procedure err_duplicate_import
	err = err_pre(EC_DUPLICATE_IMPORT) &
		//'duplicate `use` of module `'//module_name//'`' &
		//underline(context, span) &
		//" duplicate `use`"//color_reset
end procedure err_duplicate_import


!===============================================================================

module procedure err_mod_hyphen
	err = err_pre(EC_MOD_HYPHEN) &
		//'hyphens are not allowed in module names, use underscores instead' &
		//underline(context, span) &
		//" bad module name"//color_reset
end procedure err_mod_hyphen


!===============================================================================

module procedure err_mod_keyword
	err = err_pre(EC_MOD_KEYWORD) &
		//'module name `'//keyword//'` is a reserved keyword' &
		//underline(context, span) &
		//" keyword as module name"//color_reset
end procedure err_mod_keyword


!===============================================================================

module procedure err_mod_reserved_std
	err = err_pre(EC_MOD_RESERVED_STD) &
		//'module name `std` is reserved for the standard library' &
		//underline(context, span) &
		//" reserved name"//color_reset
end procedure err_mod_reserved_std


!===============================================================================

module procedure err_mod_space
	err = err_pre(EC_MOD_SPACE) &
		//'spaces are not allowed in module names' &
		//underline(context, span) &
		//" bad module name"//color_reset
end procedure err_mod_space


!===============================================================================

module procedure err_alias_keyword
	err = err_pre(EC_ALIAS_KEYWORD) &
		//'keyword `'//alias_name//'` cannot be used as module alias' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end procedure err_alias_keyword


!===============================================================================

module procedure err_alias_reserved_std
	err = err_pre(EC_ALIAS_RESERVED_STD) &
		//'`std` is reserved and cannot be used as module alias' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end procedure err_alias_reserved_std


!===============================================================================

module procedure err_alias_hyphen
	err = err_pre(EC_ALIAS_HYPHEN) &
		//'hyphens are not allowed in module aliases' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end procedure err_alias_hyphen


!===============================================================================

module procedure err_alias_space
	err = err_pre(EC_ALIAS_SPACE) &
		//'spaces are not allowed in module aliases' &
		//underline(context, span) &
		//" bad alias name"//color_reset
end procedure err_alias_space


!===============================================================================

module procedure err_alias_with_doublecolon
	err = err_pre(EC_ALIAS_WITH_DOUBLECOLON) &
		//'cannot combine alias with `::` syntax' &
		//underline(context, span) &
		//" use `use module as alias;` without `::`"//color_reset
end procedure err_alias_with_doublecolon


!===============================================================================

module procedure err_404
	err = err_pre(EC_404)//'file `'//filename//'` not found'//color_reset
end procedure err_404


!===============================================================================

module procedure err_eval_unary_type

	err = err_int_pre(IC_EVAL_UNARY_TYPE) &
		//'unary operator `'//op//'` cannot be evaluated for operand type ' &
		//color_reset

end procedure err_eval_unary_type


!===============================================================================

module procedure err_eval_binary_types

	err = err_int_pre(IC_EVAL_BINARY_TYPES) &
		//'binary operator `'//op//'` cannot be evaluated for operand types ' &
		//color_reset

end procedure err_eval_binary_types


!===============================================================================

module procedure err_matmul_dim

	err = err_rt_pre(RC_MATMUL_DIM) &
		//'matmul `@` dimension mismatch: inner dimensions ' &
		//str(int(lsize))//' and ' &
		//str(int(rsize))//' do not agree'//color_reset

end procedure err_matmul_dim


!===============================================================================

module procedure err_eval_len_array

	err = err_int_pre(IC_EVAL_LEN_ARRAY) &
		//'len array cannot be evaluated for type `' &
		//type_name//'`'//color_reset

end procedure err_eval_len_array


!===============================================================================

module procedure err_eval_unary_op

	err = err_int_pre(IC_EVAL_UNARY_OP) &
		//'unexpected unary operator `'//op//'`'//color_reset

end procedure err_eval_unary_op


!===============================================================================

module procedure err_eval_binary_op

	err = err_int_pre(IC_EVAL_BINARY_OP) &
		//'unexpected binary operator `'//op//'`'//color_reset

end procedure err_eval_binary_op


!===============================================================================

module procedure new_span
	! Maybe this should take end pos instead of length?  Seems like I end up
	! back-calculating len from pos most of the time



	span%start    = start
	span%length   = max(length, 1)

end procedure new_span


!===============================================================================

module procedure new_context


	context%text = text
	context%src_file = src_file

	! gfortran warns if i just let this auto-allocate in the next line?
	allocate(context%lines( 1: size(lines) ))  

	context%lines = lines

end procedure new_context


!===============================================================================

module procedure new_context_vector


	vector%len_ = 0
	vector%cap = 2  ! I think a small default makes sense here

	allocate(vector%v( vector%cap ))

end procedure new_context_vector


!===============================================================================

module procedure push_context


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

end procedure push_context


!===============================================================================

module procedure underline


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

end procedure underline


!===============================================================================

module procedure internal_error

	! The goal is for this to be unreachable

	write(*,*) fg_bold_bright_red//'Fatal error'//color_reset
	call exit(exit_failure)

end procedure internal_error


!===============================================================================

end submodule syntran__errors_impl_m

!===============================================================================
