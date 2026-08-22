
!===============================================================================

submodule (syntran__consts_m) syntran__consts_impl_m

	implicit none

contains

!===============================================================================

module procedure kind_token

	character(len = *), parameter :: tokens(*) = [ &
			"End of file          ", & !   1
			"[0-9]                ", & !   2
			"[\s]                 ", & !   3
			"-                    ", & !   4
			"+                    ", & !   5
			"Bad token            ", & !   6
			"/                    ", & !   7
			"*                    ", & !   8
			"Binary expression    ", & !   9
			"i32 expression       ", & !  10
			")                    ", & !  11
			"(                    ", & !  12
			"Unary expression     ", & !  13
			"Identifier           ", & !  14
			"false                ", & !  15
			"true                 ", & !  16
			"Literal expression   ", & !  17
			"bool expression      ", & !  18
			"not                  ", & !  19
			"or                   ", & !  20
			"and                  ", & !  21
			"==                   ", & !  22
			"!=                   ", & !  23
			"Assignment expression", & !  24
			"=                    ", & !  25
			"Name expression      ", & !  26
			"let                  ", & !  27
			"**                   ", & !  28
			"}                    ", & !  29
			"{                    ", & !  30
			"Expression statement ", & !  31
			"Block statement      ", & !  32
			";                    ", & !  33
			"if                   ", & !  34
			"else                 ", & !  35
			"for                  ", & !  36
			"in                   ", & !  37
			"while                ", & !  38
			"if statement         ", & !  39
			"]                    ", & !  40
			"[                    ", & !  41
			"for                  ", & !  42
			":                    ", & !  43
			"while                ", & !  44
			"let expression       ", & !  45
			"<                    ", & !  46
			"<=                   ", & !  47
			">                    ", & !  48
			">=                   ", & !  49
			"[0-9.+-e]f           ", & !  50
			"f32 expression       ", & !  51
			"Uniform array        ", & !  52
			"Explicit array       ", & !  53
			"Array expression     ", & !  54
			"Array type           ", & !  55
			",                    ", & !  56
			"Unknown type         ", & !  57
			"fn call expression   ", & !  58
			"Translation unit     ", & !  59
			"fn declaration       ", & !  60
			"fn keyword           ", & !  61
			"void type            ", & !  62
			"any type             ", & !  63
			"str type             ", & !  64
			"str token            ", & !  65
			"%                    ", & !  66
			"+=                   ", & !  67
			"-=                   ", & !  68
			"*=                   ", & !  69
			"scalar subript       ", & !  70
			"range subript        ", & !  71
			"step subcript        ", & !  72
			"/=                   ", & !  73
			"file type            ", & !  74
			"i64 type             ", & !  75
			"i64 token            ", & !  76
			"**= token            ", & !  77
			"%=                   ", & !  78
			"#                    ", & !  79
			"include              ", & !  80
			"all_sub              ", & !  81
			"bool_array_type      ", & !  82
			"i32_array_type       ", & !  83
			"i64_array_type       ", & !  84
			"f32_array_type       ", & !  85
			"str_array_type       ", & !  86
			"step_array           ", & !  87
			"len_array            ", & !  88
			"bound_array          ", & !  89
			"size_array           ", & !  90
			"return               ", & !  91
			"return statement     ", & !  92
			".                    ", & !  93
			"struct               ", & !  94
			"struct declaration   ", & !  95
			"struct instance expr ", & !  96
			"struct type          ", & !  97
			"dot expression       ", & !  98
			"[0-9.+-e]            ", & !  99
			"f64 expression       ", & ! 100
			"f64_array_type       ", & ! 101
			"fn call intr expr    ", & ! 102
			"break                ", & ! 103
			"break statement      ", & ! 104
			"continue             ", & ! 105
			"continue statement   ", & ! 106
			"<<                   ", & ! 107
			">>                   ", & ! 108
			"&                    ", & ! 109
			"|                    ", & ! 110
			"^                    ", & ! 111
			"!                    ", & ! 112
			"<<=                  ", & ! 113
			">>=                  ", & ! 114
			"&=                   ", & ! 115
			"^=                   ", & ! 116
			"|=                   ", & ! 117
			"arr_sub              ", & ! 118
			"::                   ", & ! 119
			"use                  ", & ! 120
			"use statement        ", & ! 121
			"@                    ", & ! 122
			"const                ", & ! 123
			"method call expr     ", & ! 124
			"fn type              ", & ! 125
			"fn ref expr          ", & ! 126
			"fn call ptr expr     ", & ! 127
			"unknown              "  & ! inf
		]

	if (.not. (1 <= kind .and. kind <= size(tokens))) then
		kind_token = "unknown"
		return
	end if

	kind_token = trim(tokens(kind))

end procedure kind_token

!===============================================================================

module procedure kind_name

	character(len = *), parameter :: names(*) = [ &
			"eof_token            ", & !   1
			"i32_token            ", & !   2
			"whitespace_token     ", & !   3
			"minus_token          ", & !   4
			"plus_token           ", & !   5
			"bad_token            ", & !   6
			"slash_token          ", & !   7
			"star_token           ", & !   8
			"binary_expr          ", & !   9
			"i32_type             ", & !  10
			"rparen_token         ", & !  11
			"lparen_token         ", & !  12
			"unary_expr           ", & !  13
			"identifier_token     ", & !  14
			"false_keyword        ", & !  15
			"true_keyword         ", & !  16
			"literal_expr         ", & !  17
			"bool_type            ", & !  18
			"not_keyword          ", & !  19
			"or_keyword           ", & !  20
			"and_keyword          ", & !  21
			"eequals_token        ", & !  22
			"bang_equals_token    ", & !  23
			"assignment_expr      ", & !  24
			"equals_token         ", & !  25
			"name_expr            ", & !  26
			"let_keyword          ", & !  27
			"sstar_token          ", & !  28
			"rbrace_token         ", & !  29
			"lbrace_token         ", & !  30
			"expr_statement       ", & !  31
			"block_statement      ", & !  32
			"semicolon_token      ", & !  33
			"if_keyword           ", & !  34
			"else_keyword         ", & !  35
			"for_keyword          ", & !  36
			"in_keyword           ", & !  37
			"while_keyword        ", & !  38
			"if_statement         ", & !  39
			"rbracket_token       ", & !  40
			"lbracket_token       ", & !  41
			"for_statement        ", & !  42
			"colon_token          ", & !  43
			"while_statement      ", & !  44
			"let_expr             ", & !  45
			"less_token           ", & !  46
			"less_equals_token    ", & !  47
			"greater_token        ", & !  48
			"greater_equals_token ", & !  49
			"f32_token            ", & !  50
			"f32_type             ", & !  51
			"unif_array           ", & !  52
			"expl_array           ", & !  53
			"array_expr           ", & !  54
			"array_type           ", & !  55
			"comma_token          ", & !  56
			"unknown_type         ", & !  57
			"fn_call_expr         ", & !  58
			"translation_unit     ", & !  59
			"fn_declaration       ", & !  60
			"fn_keyword           ", & !  61
			"void_type            ", & !  62
			"any_type             ", & !  63
			"str_type             ", & !  64
			"str_token            ", & !  65
			"percent_token        ", & !  66
			"plus_equals_token    ", & !  67
			"minus_equals_token   ", & !  68
			"star_equals_token    ", & !  69
			"scalar_sub           ", & !  70
			"range_sub            ", & !  71
			"step_sub             ", & !  72
			"slash_equals_token   ", & !  73
			"file_type            ", & !  74
			"i64_type             ", & !  75
			"i64_token            ", & !  76
			"sstar_equals_token   ", & !  77
			"percent_equals_token ", & !  78
			"hash_token           ", & !  79
			"include_keyword      ", & !  80
			"all_sub              ", & !  81
			"bool_array_type      ", & !  82
			"i32_array_type       ", & !  83
			"i64_array_type       ", & !  84
			"f32_array_type       ", & !  85
			"str_array_type       ", & !  86
			"step_array           ", & !  87
			"len_array            ", & !  88
			"bound_array          ", & !  89
			"size_array           ", & !  90
			"return_keyword       ", & !  91
			"return_statement     ", & !  92
			"dot_token            ", & !  93
			"struct_keyword       ", & !  94
			"struct_declaration   ", & !  95
			"struct_instance_expr ", & !  96
			"struct_type          ", & !  97
			"dot_expr             ", & !  98
			"f64_token            ", & !  99
			"f64_type             ", & ! 100
			"f64_array_type       ", & ! 101
			"fn_call_intr_expr    ", & ! 102
			"break_keyword        ", & ! 103
			"break_statement      ", & ! 104
			"continue keyword     ", & ! 105
			"continue statement   ", & ! 106
			"lless_token          ", & ! 107
			"ggreater_token       ", & ! 108
			"amp_token            ", & ! 109
			"pipe_token           ", & ! 110
			"caret_token          ", & ! 111
			"bang_token           ", & ! 112
			"lless_equals_token   ", & ! 113
			"ggreater_equals_token", & ! 114
			"amp_equals_token     ", & ! 115
			"caret_equals_token   ", & ! 116
			"pipe_equals_token    ", & ! 117
			"arr_sub              ", & ! 118
			"double_colon_token   ", & ! 119
			"use_keyword          ", & ! 120
			"use_statement        ", & ! 121
			"matmul_token         ", & ! 122
			"const_keyword        ", & ! 123
			"method_call_expr     ", & ! 124
			"fn_type              ", & ! 125
			"fn_ref_expr          ", & ! 126
			"fn_call_ptr_expr     ", & ! 127
			"enum_keyword         ", & ! 128
			"enum_declaration     ", & ! 129
			"enum_type            ", & ! 130
			"enum_access_expr     ", & ! 131
			"enum_cast_expr       ", & ! 132
			"comment_token        ", & ! 133
			"unknown              "  & ! inf (trailing comma hack)
		]
			! FIXME: update kind_tokens array too

	if (.not. (1 <= kind .and. kind <= size(names))) then
		kind_name = "unknown"
		return
	end if

	kind_name = trim(names(kind))

end procedure kind_name

!===============================================================================

end submodule syntran__consts_impl_m

!===============================================================================

