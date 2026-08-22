
!===============================================================================

module syntran__consts_m

	! Debug logging verbosity (0 == silent)
	integer, parameter :: debug = 0

	integer :: maxerr  ! TODO: move this (not default) into a settings struct that gets passed around
	integer, parameter :: maxerr_def = 4

	logical :: permissive_return = .false.

	! Initial capacity of scope dict pointer arrays.  They are dynamic arrays of
	! pointers, so they take little memory to begin with, and then grow
	! dynamically.  Hence, I have no idea what the default should be and it
	! shouldn't really matter
	integer, parameter :: SCOPE_CAP_INIT = 8

	! Max array rank handled by the native OP_SLICE_NAT/OP_STORE_SLICE_NAT
	! bytecode handlers' fixed-size local buffers.  Higher-rank slices fall
	! back to OP_SLICE/OP_STORE_SLICE (runtime_array.f90), which use allocatable
	! subscript arrays and have no rank limit.
	integer, parameter :: MAX_NAT_SLICE_RANK = 4

	! Max array rank handled by the native OP_UNIF_ARRAY_NAT bytecode handler's
	! fixed-size dims_ buffer.  Higher-rank uniform arrays fall back to
	! OP_NEW_ARRAY (eval_array_expr), which has no rank limit.
	integer, parameter :: MAX_NAT_UNIF_RANK = 8

	! Must be larger than largest token enum below.  TODO: add an init check for
	! this
	integer, parameter :: magic = 256

	! Token and syntax node kinds enum.  Is there a better way to do this that
	! allows re-ordering enums?  Currently it would break kind_name()
	integer, parameter ::          &
			comment_token         = 133, &
			enum_cast_expr        = 132, &
			enum_access_expr      = 131, &
			enum_type             = 130, &
			enum_declaration      = 129, &
			enum_keyword          = 128, &
			fn_call_ptr_expr      = 127, &
			fn_ref_expr           = 126, &
			fn_type               = 125, &
			method_call_expr      = 124, &
			const_keyword         = 123, &
			matmul_token          = 122, &
			use_statement         = 121, &
			use_keyword           = 120, &
			double_colon_token    = 119, &
			arr_sub               = 118, &
			pipe_equals_token     = 117, &
			caret_equals_token    = 116, &
			amp_equals_token      = 115, &
			ggreater_equals_token = 114, &
			lless_equals_token    = 113, &
			bang_token            = 112, &
			caret_token           = 111, &
			pipe_token            = 110, &
			amp_token             = 109, &
			ggreater_token        = 108, &
			lless_token           = 107, &
			continue_statement    = 106, &
			continue_keyword      = 105, &
			break_statement       = 104, &
			break_keyword         = 103, &
			fn_call_intr_expr     = 102, &
			f64_array_type        = 101, &
			f64_type              = 100, &
			f64_token             =  99, &
			dot_expr              =  98, &
			struct_type           =  97, &
			struct_instance_expr  =  96, &
			struct_declaration    =  95, &
			struct_keyword        =  94, &
			dot_token             =  93, &
			return_statement      =  92, &
			return_keyword        =  91, &
			size_array            =  90, &
			bound_array           =  89, &
			len_array             =  88, &
			step_array            =  87, &
			str_array_type        =  86, &
			f32_array_type        =  85, &
			i64_array_type        =  84, &
			i32_array_type        =  83, &
			bool_array_type       =  82, &  ! only used so get_binary_op_kind can return a single int
			all_sub               =  81, &
			include_keyword       =  80, &
			hash_token            =  79, &
			percent_equals_token  =  78, &
			sstar_equals_token    =  77, &
			i64_token             =  76, &
			i64_type              =  75, &
			file_type             =  74, &
			slash_equals_token    =  73, &
			step_sub              =  72, &
			range_sub             =  71, &
			scalar_sub            =  70, &
			star_equals_token     =  69, &
			minus_equals_token    =  68, &
			plus_equals_token     =  67, &
			percent_token         =  66, &
			str_token             =  65, &
			str_type              =  64, &
			any_type              =  63, &
			void_type             =  62, &
			fn_keyword            =  61, &
			fn_declaration        =  60, &
			translation_unit      =  59, &
			fn_call_expr          =  58, &
			unknown_type          =  57, &
			comma_token           =  56, &
			array_type            =  55, &
			array_expr            =  54, &
			expl_array            =  53, &
			unif_array            =  52, &
			f32_type              =  51, &
			f32_token             =  50, &
			greater_equals_token  =  49, &
			greater_token         =  48, &
			less_equals_token     =  47, &
			less_token            =  46, &
			let_expr              =  45, &
			while_statement       =  44, &
			colon_token           =  43, &
			for_statement         =  42, &
			lbracket_token        =  41, &
			rbracket_token        =  40, &
			if_statement          =  39, &
			while_keyword         =  38, &
			in_keyword            =  37, &
			for_keyword           =  36, &
			else_keyword          =  35, &
			if_keyword            =  34, &
			semicolon_token       =  33, &
			block_statement       =  32, &
			expr_statement        =  31, &
			lbrace_token          =  30, &
			rbrace_token          =  29, &
			sstar_token           =  28, &
			let_keyword           =  27, &
			name_expr             =  26, &
			equals_token          =  25, & ! '='
			assignment_expr       =  24, &
			bang_equals_token     =  23, &
			eequals_token         =  22, & ! '=='
			and_keyword           =  21, &
			or_keyword            =  20, &
			not_keyword           =  19, &
			bool_type             =  18, &
			literal_expr          =  17, &
			true_keyword          =  16, &
			false_keyword         =  15, &
			identifier_token      =  14, &
			unary_expr            =  13, &
			lparen_token          =  12, &
			rparen_token          =  11, &
			i32_type              =  10, &
			binary_expr           =   9, &
			star_token            =   8, &
			slash_token           =   7, &
			bad_token             =   6, &
			plus_token            =   5, &
			minus_token           =   4, &
			whitespace_token      =   3, &
			i32_token             =   2, &
			eof_token             =   1

	! Read-only member indices for dot access on a file handle (c.f.
	! parse_dot() and get_val()).  These occupy the same node%member%id_index
	! slot that a struct field index would, but are an independent local
	! namespace, not part of the token/syntax-node-kind enum above
	integer, parameter :: &
		FILE_MEM_IS_OPEN = 1, &
		FILE_MEM_EOF     = 2, &
		FILE_MEM_NAME    = 3

!===============================================================================

	! Bodies live in consts_impl.f90 so that editing the big token tables below
	! does not invalidate this module's .mod and rebuild the whole tree -- only
	! the .smod changes, which fpm's dependents don't recompile on.
	interface

		module function kind_token(kind)
			integer, intent(in) :: kind
			character(len = :), allocatable :: kind_token
		end function kind_token

		module function kind_name(kind)
			integer, intent(in) :: kind
			character(len = :), allocatable :: kind_name
		end function kind_name

	end interface

!===============================================================================

end module syntran__consts_m

!===============================================================================

