
!===============================================================================

submodule (syntran__parse_m) syntran__parse_struct

	! Parsing of struct and enum declarations, struct instances, and methods.
	! Split out of parse_fn.f90, which keeps fn declarations and calls

	use syntran__intr_fns_m

	implicit none

!===============================================================================

contains

!===============================================================================

function at_method_start(parser) result(at_start)

	! True when the parser is sitting at the start of a struct method decl:
	! `fn name(...)`, `const fn name(...)`, or (E106) a method missing its
	! `fn` keyword -- `name(...)` / `const name(...)`.  Not type-bound (same
	! rationale as looks_like_fn_decl() in parse_misc.f90): a private
	! implementation detail used only from parse_struct_declaration() below.
	!
	! Unlike looks_like_fn_decl(), no token-stream scan is needed here: inside
	! a struct body a member is always `name: type`, so `name(` can only be a
	! method

	class(parser_t) :: parser

	logical :: at_start

	!********

	integer :: offset

	offset = 0
	if (parser%current_kind() == const_keyword) offset = 1

	at_start = parser%peek_kind(offset) == fn_keyword .or. &
		(parser%peek_kind(offset)     == identifier_token .and. &
		 parser%peek_kind(offset + 1) == lparen_token)

end function at_method_start

!===============================================================================

module subroutine parse_struct_declaration(parser, decl)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl

	!********

	character(len = :), allocatable :: type_text

	integer :: itype, i, io, pos0

	logical :: overwrite

	type(struct_t) :: struct

	type(syntax_node_t) :: method_decl

	type(syntax_node_vector_t) :: method_decls

	logical :: is_const_meth, no_fn_kw

	integer :: j

	type(syntax_token_t) :: identifier, comma, lbrace, rbrace, dummy, &
		colon, name, struct_kw

	type(text_span_t) :: span

	type( string_vector_t) :: names
	type(integer_vector_t) :: pos_mems

	type(value_t) :: member  ! local type meta-data
	type(value_t) :: type
	type(value_vector_t) :: types

	call parser%match(struct_keyword, struct_kw)

	call parser%match(identifier_token, identifier)
	!print *, "parsing struct ", identifier%text

	itype = lookup_type(identifier%text, parser%structs, parser%enums)
	!print *, "itype = ", itype, kind_name(itype)
	if (itype /= unknown_type .and. itype /= struct_type) then
		! Redeclared structs are caught below
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_primitive( &
			parser%context(), &
			span, &
			identifier%text))
	end if
	call parser%check_var_clash(identifier%text, identifier%pos, "struct")

	call parser%match(lbrace_token, lbrace)

	! Structs use this syntax:
	!
	!     // declaration
	!     struct Time
	!     {
	!     	hh: i32,
	!     	mm: i32,
	!     	ss: f32,
	!     }
	!
	!     // instance
	!     let t1 = Time{hh = 9, mm = 20, ss = 0.030,};
	!     t1.hh = 10;
	!
	! A struct declaration is a lot like a fn declaration.  Instead of a list of
	! fn parameters, we have a list of struct members.  Unlike a fn declaration,
	! there is no "body" for a struct, only members.

	! Parse member names and types.  Save in temp vectors initially
	names  = new_string_vector()
	types = new_value_vector()

	! For diagnostic text spans
	pos_mems = new_integer_vector()

	i = 0
	do while ( &
			parser%current_kind() /= rbrace_token .and. &
			parser%current_kind() /= eof_token .and. &
			.not. at_method_start(parser))
		i = i + 1

		pos0 = parser%current_pos()

		call parser%match(identifier_token, name)
		call pos_mems%push( name%pos )
		call parser%match(colon_token, colon)

		call parser%parse_type(type_text, type)
		!print *, "type = ", type_text

		! Fn-pointer-typed struct members: value_copy()/value_destroy()
		! already handle fn_type's nested fn_params(:)/fn_ret correctly
		! (c.f. fn_copy() in types_copy.f90, which fixed the identical
		! element-wise-copy requirement for fn param types), and
		! var_dict_destroy() (types_copy.f90) now explicitly tears down
		! each slot's %val via value_destroy() before the struct
		! member-dict's overwrite path (2nd parser pass redeclares every
		! struct) can reach it -- so nothing here needs to special-case
		! fn_type any more. This used to be rejected as E90 (fn-ptr-struct-
		! member, now retired) because of a musl/gfortran segfault in that
		! exact deep-copy/destroy path; c.f. errors.f90's retirement note.

		call types%push(type)
		call names%push( name%text )

		if (parser%current_kind() /= rbrace_token .and. &
		    .not. at_method_start(parser)) then
			! Delimiting commas are required; trailing comma is optional
			call parser%match(comma_token, comma)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) call parser%next(dummy)

	end do

	! Sentinel for duplicate-member span: end of the last field (or start of method/rbrace)
	call pos_mems%push(parser%current_pos())

	! Now that we have the number of members, save them

	struct%num_vars = 0
	allocate(struct%member_names%v( names%len_ ))
	allocate(struct%vars%dicts(1))

	! Canonical alias-independent identity, set once at declaration time.  This
	! is invariant to the module alias/import path used to reach the struct, so
	! it lets type_match() recognize the same struct imported two different ways
	! as the same type (c.f. struct_cookie in types_ops.f90)
	struct%cookie = parser%contexts%v(parser%current_unit())%src_file &
		// "::" // identifier%text

	do i = 1, names%len_
		!print *, "name = ", names%v(i)%s

		struct%member_names%v(i)%s = names%v(i)%s

		! Copy a value_t object to store the type
		member = types%v(i)

		! Declare the member
		!parser%num_vars = parser%num_vars + 1
		struct%num_vars = struct%num_vars + 1
		!print *, "struct%num_vars = ", struct%num_vars

		!! Save parameters by id_index
		!decl%params(i) = parser%num_vars

		! Each struct has its own dict of members.  Create one and insert the
		! member name into that dict instead of the (global) vars dict here.
		! Might not need a new type, could probably just re-use the `vars_t`
		! type, just like `parser%vars`.  Just add one inside of the `struct_t`
		! type.

		call struct%vars%insert(names%v(i)%s, member, &
			struct%num_vars, io, overwrite = .false.)
		!print *, 'io = ', io
		if (io /= exit_success) then
			span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
			call parser%diagnostics%push(err_redeclare_mem( &
				parser%context(), &
				span, &
				names%v(i)%s))
		end if

		if (allocated(member%array)) deallocate(member%array)
	end do

	! Register member names under this struct's cookie, so value_to_str()
	! can print them with `name = value` labels.  The returned slot index is
	! stashed on struct%reg_idx and copied onto every value_t built from
	! this struct, so printing indexes the registry directly instead of
	! hashing struct_cookie on every call (c.f. struct_reg_set() in value.f90)
	struct%reg_idx = struct_reg_set(struct%cookie, struct%member_names%v( 1: names%len_ ))

	! Parse method declarations (fn / const fn inside the struct body)
	method_decls = new_syntax_node_vector()

	do while ( &
			parser%current_kind() /= rbrace_token .and. &
			parser%current_kind() /= eof_token)

		pos0 = parser%current_pos()

		is_const_meth = (parser%current_kind() == const_keyword)
		if (is_const_meth) call parser%next(dummy)   ! consume 'const'

		! E106: a method missing its `fn` keyword.  at_method_start() (used by
		! the member loop above) already confirmed `identifier (` is
		! unambiguously a method start in this position, not a member
		no_fn_kw = parser%current_kind() == identifier_token .and. &
			parser%peek_kind(1) == lparen_token
		if (no_fn_kw) then
			span = new_span(parser%current_pos(), len(parser%current_text()))
			call parser%diagnostics%push(err_missing_fn_kw( &
				parser%context(), span, parser%current_text()))
		end if

		call parser%parse_method_declaration(method_decl, struct, is_const_meth, &
			identifier%text, no_fn_kw)

		! Save method decl node for the bytecode compiler pre-pass
		call method_decls%push(method_decl)

		! Break infinite loop
		if (parser%current_pos() == pos0) call parser%next(dummy)

	end do

	! Store method nodes in decl%members so the bytecode compiler can compile them
	if (method_decls%len_ > 0) then
		allocate(decl%members(method_decls%len_))
		do j = 1, method_decls%len_
			decl%members(j) = method_decls%v(j)
		end do
	end if

	call parser%match(rbrace_token, rbrace)

	! Insert struct into parser dict

	parser%num_structs = parser%num_structs + 1
	decl%id_index  = parser%num_structs

	overwrite = .false.
	if (parser%ipass > 0) overwrite = .true.

	!print *, "inserting identifier ", identifier%text, " into parser structs"
	call parser%structs%insert( &
		identifier%text, struct, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%struct_names%push(identifier%text)
	!print *, "io = ", io
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		! The closing `}` is matched, so the current token can be from a
		! `#include` right after.  c.f. parse_use_statement()
		call parser%diagnostics%push(err_redeclare_struct( &
			parser%contexts%v(identifier%unit_), &
			span, &
			identifier%text))
	end if

	!call ternary_tree_final(struct%vars%dicts(1)%root)

	decl%kind = struct_declaration

	!print *, "done parsing struct"

end subroutine parse_struct_declaration

!===============================================================================

module subroutine parse_enum_declaration(parser, decl)

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl

	!********

	integer :: itype, i, j, k, io, pos0
	integer :: next_value, this_value, this_explicit, sgn

	logical :: overwrite, found_alias

	type(enum_t) :: enum

	type(syntax_token_t) :: identifier, comma, lbrace, rbrace, dummy, &
		equals, name, enum_kw, intlit, alias

	type(text_span_t) :: span

	type(string_vector_t) :: names
	type(integer_vector_t) :: values, pos_mems, explicits

	! Enums use this syntax:
	!
	!     // declaration
	!     enum Card
	!     {
	!     	Two,        // 0
	!     	Three,      // 1
	!     	Jack = 10,  // 10
	!     	Queen,      // 11
	!     	King = Jack,// 10 (alias)
	!     }
	!
	!     // access
	!     let c = Card.Jack;
	!
	! Variants are compile-time constants -- unlike struct members there is
	! no type annotation, just an optional `= <intlit>` or `= <prior variant
	! name>` to pin the backing value.  A name reference must refer to a
	! variant already declared above it (like C's enumerator constants) and
	! makes this variant an intentional alias.  Subsequent variants continue
	! the auto-increment from there

	call parser%match(enum_keyword, enum_kw)

	call parser%match(identifier_token, identifier)

	itype = lookup_type(identifier%text, parser%structs, parser%enums)
	if (itype /= unknown_type .and. itype /= enum_type) then
		! Redeclared enums are caught below
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_primitive( &
			parser%context(), &
			span, &
			identifier%text))
	end if
	call parser%check_var_clash(identifier%text, identifier%pos, "enum")

	call parser%match(lbrace_token, lbrace)

	names  = new_string_vector()
	values = new_integer_vector()
	pos_mems = new_integer_vector()
	explicits = new_integer_vector()

	next_value = 0
	do while ( &
			parser%current_kind() /= rbrace_token .and. &
			parser%current_kind() /= eof_token)

		pos0 = parser%current_pos()

		call parser%match(identifier_token, name)
		call pos_mems%push( name%pos )

		this_value = next_value
		this_explicit = 0
		if (parser%current_kind() == equals_token) then
			call parser%next(equals)

			if (parser%current_kind() == identifier_token) then
				! Named alias, e.g. `King = Jack`.  Only variants already
				! declared above this one are in scope, exactly like C
				! enumerator constants -- forward references are an error
				call parser%match(identifier_token, alias)

				found_alias = .false.
				do k = 1, names%len_
					if (names%v(k)%s == alias%text) then
						this_value = values%v(k)
						found_alias = .true.
						exit
					end if
				end do

				if (.not. found_alias) then
					span = new_span(alias%pos, len(alias%text))
					call parser%diagnostics%push(err_unknown_variant( &
						parser%context(), &
						span, &
						alias%text, &
						identifier%text))
				end if

				! Whether resolved or not, this is an intentional alias --
				! not an accidental auto-increment collision -- so it's
				! exempt from the duplicate-value check below
				this_explicit = 2
			else
				! Optional leading sign: `-1` (and `+1`) lex as a separate
				! unary minus/plus token before the i32 literal -- enum
				! values are plain int literals, not full expressions, so
				! only a single leading sign is handled here
				sgn = 1
				if (parser%current_kind() == minus_token) then
					call parser%next(dummy)
					sgn = -1
				else if (parser%current_kind() == plus_token) then
					call parser%next(dummy)
				end if
				call parser%match(i32_token, intlit)
				this_value = sgn * intlit%val%sca%i32
				this_explicit = 1
			end if
		end if

		call values%push(this_value)
		call names%push( name%text )
		call explicits%push(this_explicit)
		next_value = this_value + 1

		if (parser%current_kind() /= rbrace_token) then
			! Delimiting commas are required; trailing comma is optional
			call parser%match(comma_token, comma)
		end if

		! Break infinite loop
		if (parser%current_pos() == pos0) call parser%next(dummy)

	end do

	! Sentinel for duplicate-variant span: end of the last variant (or rbrace)
	call pos_mems%push(parser%current_pos())

	enum%num_vars = 0
	allocate(enum%variant_names%v( names%len_ ))
	allocate(enum%variant_values ( names%len_ ))

	! Canonical alias-independent identity, set once at declaration time,
	! mirroring struct%cookie above
	enum%cookie = parser%contexts%v(parser%current_unit())%src_file &
		// "::" // identifier%text

	do i = 1, names%len_

		enum%variant_names%v(i)%s = names%v(i)%s
		enum%variant_values(i)    = values%v(i)
		enum%num_vars = enum%num_vars + 1

		do j = 1, i - 1
			if (enum%variant_names%v(j)%s == names%v(i)%s) then
				span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
				call parser%diagnostics%push(err_redeclare_variant( &
					parser%context(), &
					span, &
					names%v(i)%s))
				exit
			end if
		end do

		! Duplicate values are only allowed when the collision is
		! intentional: either a named alias (`King = Jack`, explicits == 2)
		! on either side, or both variants pinned explicitly to the same
		! int literal (e.g. `King = 10` next to `Jack = 10`).  Any
		! collision that involves an auto-incremented value is always
		! accidental, since auto-increment has no way to express aliasing
		! intent -- so it's a hard error
		do j = 1, i - 1
			if (values%v(j) == values%v(i) .and. &
					names%v(j)%s /= names%v(i)%s .and. &
					explicits%v(i) /= 2 .and. explicits%v(j) /= 2 .and. &
					.not. (explicits%v(i) == 1 .and. explicits%v(j) == 1)) then
				span = new_span(pos_mems%v(i), pos_mems%v(i+1) - pos_mems%v(i))
				call parser%diagnostics%push(err_duplicate_enum_value( &
					parser%context(), &
					span, &
					names%v(i)%s, &
					names%v(j)%s, &
					values%v(i)))
				exit
			end if
		end do

	end do

	call parser%match(rbrace_token, rbrace)

	! Insert enum into parser dict

	parser%num_enums = parser%num_enums + 1
	decl%id_index  = parser%num_enums

	overwrite = .false.
	if (parser%ipass > 0) overwrite = .true.

	call parser%enums%insert( &
		identifier%text, enum, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%enum_names%push(identifier%text)
	if (io /= 0) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_redeclare_enum( &
			parser%contexts%v(identifier%unit_), &
			span, &
			identifier%text))
	end if

	decl%kind = enum_declaration

end subroutine parse_enum_declaration

!===============================================================================

module subroutine parse_enum_access(parser, expr, enum_name)

	! Parse `EnumName.Variant`, called from parse_primary_expr() when the
	! current identifier is a registered enum name.  Bakes a fully-resolved
	! enum value_t into expr%val at parse time -- enum values are
	! compile-time constants, so there is no runtime lookup
	!
	! When `enum_name` is present, it is the already-parsed, alias-qualified
	! lookup name (e.g. "mod::Suit") from parse_qualified_expr(), and only the
	! trailing `.Variant` remains to be consumed here -- mirrors
	! parse_struct_instance()'s optional `struct_name` argument

	class(parser_t), target :: parser
	type(syntax_node_t), intent(out) :: expr
	character(len = *), intent(in), optional :: enum_name

	!********

	integer :: enum_id, i, variant_id

	character(len = :), allocatable :: enum_name_text

	type(enum_t), pointer :: enum

	type(syntax_token_t) :: enum_tok, dot, variant_tok

	type(text_span_t) :: span

	if (present(enum_name)) then
		enum_name_text = enum_name
	else
		call parser%match(identifier_token, enum_tok)
		enum_name_text = enum_tok%text
	end if

	call parser%match(dot_token, dot)
	call parser%match(identifier_token, variant_tok)

	enum_id = parser%enums%find(enum_name_text)
	enum => parser%enums%get(enum_id)

	variant_id = 0
	do i = 1, enum%num_vars
		if (enum%variant_names%v(i)%s == variant_tok%text) then
			variant_id = i
			exit
		end if
	end do

	expr%kind = enum_access_expr
	expr%identifier = variant_tok

	if (variant_id == 0) then
		span = new_span(variant_tok%pos, len(variant_tok%text))
		call parser%diagnostics%push(err_unknown_variant( &
			parser%context(), span, variant_tok%text, enum_name_text, &
			enum_closest_variant(enum, variant_tok%text)))

		! Error recovery: sanitize to unknown_type so this doesn't cascade
		! into a fn-ptr-struct-member-style crash downstream
		expr%val%type = unknown_type
		return
	end if

	call bake_enum_variant(enum, variant_id, enum_name_text, expr%val)

end subroutine parse_enum_access

!===============================================================================

module subroutine parse_enum_cast(parser, expr, enum_name)

	! Parse `EnumName(ordinal)`, the reverse cast from an integer ordinal back
	! to an enum variant, e.g. `Suit(2) -> Suit.Clubs`.  Called from
	! parse_primary_expr() when the current identifier is a registered enum
	! name followed by `(`
	!
	! When `enum_name` is present, it is the already-parsed, alias-qualified
	! lookup name (e.g. "mod::Suit") from parse_qualified_expr(), and only the
	! trailing `(ordinal)` remains to be consumed here -- mirrors
	! parse_enum_access()'s optional `enum_name` argument
	!
	! Bakes every one of the enum's variants into expr%val%struct(:) at parse
	! time -- the same representation used for enum-array elements (c.f.
	! parse_array_expr) -- so the ordinal->variant lookup at eval time needs
	! no runtime enum registry; it just scans this baked list.  When the
	! argument is a constant int literal, the ordinal is checked against the
	! baked variants right here at parse time (E96); otherwise the check is
	! deferred to runtime (R32)

	class(parser_t), target :: parser
	type(syntax_node_t), intent(out) :: expr
	character(len = *), intent(in), optional :: enum_name

	!********

	integer :: enum_id, i
	integer :: span_beg, span_end

	character(len = :), allocatable :: enum_name_text

	type(enum_t), pointer :: enum

	type(syntax_node_t) :: arg

	type(syntax_token_t) :: enum_tok, lparen, rparen

	type(text_span_t) :: span

	logical :: found

	if (present(enum_name)) then
		enum_name_text = enum_name
	else
		call parser%match(identifier_token, enum_tok)
		enum_name_text = enum_tok%text
	end if

	call parser%match(lparen_token, lparen)

	span_beg = parser%peek_pos(0)
	call parser%parse_expr(expr=arg)
	span_end = parser%peek_pos(0) - 1

	call parser%match(rparen_token, rparen)

	enum_id = parser%enums%find(enum_name_text)
	enum => parser%enums%get(enum_id)

	if (.not. is_int_type(arg%val%type)) then
		span = new_span(span_beg, span_end - span_beg + 1)
		call parser%diagnostics%push(err_bad_arg_type( &
			parser%context(), span, enum_name_text, 1, "ordinal", "i32", &
			type_name(arg%val)))

	else if (arg%kind == literal_expr .and. arg%val%type == i32_type) then
		! Parse-time check: a constant i32 literal argument can be validated
		! right now instead of waiting for a runtime error
		found = .false.
		do i = 1, enum%num_vars
			if (enum%variant_values(i) == arg%val%sca%i32) then
				found = .true.
				exit
			end if
		end do
		if (.not. found) then
			span = new_span(span_beg, span_end - span_beg + 1)
			call parser%diagnostics%push(err_enum_cast_range( &
				parser%context(), span, enum_name_text, arg%val%sca%i32))
		end if
	end if

	expr%kind = enum_cast_expr
	if (.not. present(enum_name)) expr%identifier = enum_tok
	call syntax_node_move(arg, expr%right)

	! Bake every variant into expr%val%struct(:), mirroring how an
	! enum-array literal tags its elements (c.f. parse_array_expr)
	expr%val%type = enum_type
	expr%val%enum_name = enum_name_text
	expr%val%enum_cookie = enum%cookie
	allocate(expr%val%struct( enum%num_vars ))
	do i = 1, enum%num_vars
		call bake_enum_variant(enum, i, enum_name_text, expr%val%struct(i))
	end do

end subroutine parse_enum_cast

!===============================================================================

module subroutine parse_enum_name_expr(parser, expr, enum_name)

	! Parse a bare enum type name, e.g. `Suit`, used as an expression rather
	! than in `Suit.Variant` or `Suit(ordinal)` position.  Called from
	! parse_primary_expr() when the current identifier is a registered enum
	! name and not shadowed by a live variable of the same name
	!
	! Following Python's model, a bare enum name is an array of all its
	! variants in declaration order (aliases included).  This is synthesized
	! here as an ordinary explicit array literal (expl_array), so it rides
	! the existing array machinery -- native OP_NEW_ARRAY when the elements
	! qualify, eval_array_expr's slot-consuming fallback otherwise -- and
	! iterating it with
	! `for` takes the same expl_array path already exercised by a literal
	! enum array (c.f. parse_array_expr).  No new node kind, no runtime enum
	! registry
	!
	! When `enum_name` is present, it is the already-parsed, alias-qualified
	! lookup name (e.g. "mod::Suit") from parse_qualified_expr(), and the
	! bare name has already been fully consumed -- mirrors
	! parse_enum_access()'s and parse_enum_cast()'s optional `enum_name` arg

	class(parser_t), target :: parser
	type(syntax_node_t), intent(out) :: expr
	character(len = *), intent(in), optional :: enum_name

	!********

	integer :: enum_id, i

	character(len = :), allocatable :: enum_name_text

	type(enum_t), pointer :: enum

	type(syntax_token_t) :: enum_tok

	if (present(enum_name)) then
		enum_name_text = enum_name
	else
		call parser%match(identifier_token, enum_tok)
		enum_name_text = enum_tok%text
	end if

	enum_id = parser%enums%find(enum_name_text)
	enum => parser%enums%get(enum_id)

	expr%kind             = array_expr
	if (.not. present(enum_name)) expr%identifier = enum_tok
	expr%is_enum_name     = .true.
	expr%val%type         = array_type
	expr%val%enum_name    = enum_name_text
	expr%val%enum_cookie  = enum%cookie

	allocate(expr%val%array)
	expr%val%array%type = enum_type
	expr%val%array%kind = expl_array
	expr%val%array%rank = 1
	expr%val%array%len_ = enum%num_vars

	allocate(expr%elems( enum%num_vars ))
	do i = 1, enum%num_vars
		expr%elems(i)%kind = enum_access_expr
		call bake_enum_variant(enum, i, enum_name_text, expr%elems(i)%val)
	end do

end subroutine parse_enum_name_expr

!===============================================================================

subroutine bake_enum_variant(enum, i, enum_name_text, val)

	! Bake variant i of `enum` into `val` as a fully-resolved enum value_t --
	! enum values are compile-time constants, so there is no runtime lookup.
	! Shared by parse_enum_access(), parse_enum_cast(), and
	! parse_enum_name_expr()

	type(enum_t), intent(in) :: enum
	integer, intent(in) :: i
	character(len = *), intent(in) :: enum_name_text
	type(value_t), intent(out) :: val

	val%type = enum_type
	val%sca%i32 = enum%variant_values(i)
	val%enum_name = enum_name_text
	val%enum_variant = enum%variant_names%v(i)%s
	val%enum_cookie = enum%cookie

end subroutine bake_enum_variant

!===============================================================================

function enum_closest_variant(enum, key) result(closest)

	! Return the closest variant name in `enum` to `key`, or "" when none is
	! close enough.  Mirrors structs_t%closest()/enums_t%closest(), but scans
	! an enum_t's variant_names instead of a whole hash table of enum_t's

	type(enum_t), intent(in) :: enum
	character(len = *), intent(in) :: key
	character(len = :), allocatable :: closest

	!********

	integer :: i, min_dist, dist, threshold
	character(len = :), allocatable :: key_low, target_low

	closest  = ""
	min_dist = huge(min_dist)
	target_low = to_lower(key)

	do i = 1, enum%num_vars
		key_low = to_lower(enum%variant_names%v(i)%s)
		if (key_low == target_low) cycle

		dist = levenshtein(target_low, key_low)
		if (dist < min_dist) then
			min_dist = dist
			closest  = enum%variant_names%v(i)%s
		end if
	end do

	threshold = max(2, len(target_low) / 3)
	if (min_dist > threshold) closest = ""

end function enum_closest_variant

!===============================================================================

module subroutine parse_method_declaration(parser, decl, struct, is_const, struct_name, no_fn_kw)

	! Parse a method declared inside a struct body.
	! The implicit first parameter "0self" is the struct passed by reference.
	! Field names in the method body are transformed to dot_expr on "0self"
	! by parse_name_expr and parse_expr_statement (via parser%in_method context).

	class(parser_t) :: parser
	type(syntax_node_t), intent(out) :: decl
	type(struct_t), intent(in) :: struct
	logical, intent(in) :: is_const
	character(len = *), intent(in) :: struct_name
	logical, intent(in), optional :: no_fn_kw

	!********

	character(len = :), allocatable :: type_text, mangled_name

	integer :: i, io, pos0, rank, fn_beg, fn_name_end, mem_id

	! See the identical fn_src_id note in parse_fn_declaration() above
	integer :: fn_src_id

	logical :: overwrite, const_param, in_fn_body0, no_fn_kw0

	type(fn_t) :: fn

	type(string_vector_t) :: names
	type(integer_vector_t) :: pos_args
	type(logical_vector_t) :: is_ref, is_const_ref

	type(syntax_node_t) :: body
	type(syntax_token_t) :: fn_kw, identifier, lparen, rparen, colon, &
		name, comma, dummy, amp

	type(text_span_t) :: span

	type(value_t) :: type, self_val, mem_val
	type(value_vector_t) :: types

	no_fn_kw0 = .false.
	if (present(no_fn_kw)) no_fn_kw0 = no_fn_kw

	call parser%vars%push_scope()
	call parser%locs%push_scope()
	parser%is_loc = .true.
	parser%num_locs = 0

	parser%returned = .false.
	fn_beg = parser%peek_pos(0)
	fn_src_id = parser%contexts%v(parser%current_unit())%src_id
	if (.not. no_fn_kw0) then
		call parser%match(fn_keyword, fn_kw)
	end if
	! When no_fn_kw0, the caller (the method loop in parse_struct_declaration()
	! above) has already pushed E106 for the missing `fn`, so just carry on
	! parsing from the name

	call parser%match(identifier_token, identifier)
	fn_name_end = parser%peek_pos(0) - 1
	fn%is_intr = .false.
	fn%is_method = .true.
	fn%is_const_method = is_const

	! A method may not share a name with a member of the same struct -- that
	! would make `s.foo` ambiguous between a field read and a method call
	call struct%vars%search(identifier%text, mem_id, io, mem_val)
	if (io == exit_success) then
		span = new_span(identifier%pos, len(identifier%text))
		call parser%diagnostics%push(err_member_method_clash( &
			parser%context(), span, identifier%text, struct_name))
	end if

	! Insert implicit "0self" as first local (the struct receiver)
	self_val%type = struct_type
	self_val%struct_name = struct_name
	if (allocated(struct%cookie)) self_val%struct_cookie = struct%cookie
	self_val%struct_reg_idx = struct%reg_idx
	parser%num_locs = parser%num_locs + 1
	const_param = is_const
	call parser%locs%insert("0self", self_val, parser%num_locs, io, &
		is_const = const_param)
	parser%self_loc_id = parser%num_locs

	! Set method parsing context so field names resolve to dot_expr("0self", field)
	! (and bare calls resolve to sibling self-method calls, c.f. parse_fn_call)
	parser%in_method = .true.
	parser%in_const_method = is_const
	parser%method_struct = struct
	parser%method_struct_name = struct_name

	if (parser%current_kind() /= lparen_token) then
		parser%in_method = .false.
		parser%in_const_method = .false.
		call parser%vars%pop_scope()
		call parser%locs%pop_scope()
		parser%is_loc = .false.
		return
	end if

	call parser%match(lparen_token, lparen)

	names        = new_string_vector()
	pos_args     = new_integer_vector()
	is_ref       = new_logical_vector()
	is_const_ref = new_logical_vector()
	types        = new_value_vector()

	i = 0
	do while ( &
			parser%current_kind() /= rparen_token .and. &
			parser%current_kind() /= eof_token)
		i = i + 1

		pos0 = parser%current_pos()
		call pos_args%push(pos0)

		call parser%match(identifier_token, name)
		call parser%check_type_clash(name%text, name%pos)
		call parser%match(colon_token, colon)

		if (parser%current_kind() == amp_token) then
			call parser%match(amp_token, amp)
			call is_ref%push(.true.)
			if (parser%current_kind() == const_keyword) then
				call parser%next(dummy)
				call is_const_ref%push(.true.)
			else
				call is_const_ref%push(.false.)
			end if
		else
			call is_ref%push(.false.)
			call is_const_ref%push(.false.)
		end if

		call parser%parse_type(type_text, type)

		call names%push( name%text )
		call types%push(type)

		if (parser%current_kind() /= rparen_token) then
			call parser%match(comma_token, comma)
		end if

		if (parser%current_pos() == pos0) call parser%next(dummy)

	end do
	call pos_args%push(parser%current_pos() + 1)

	call parser%match(rparen_token, rparen)

	! params(1) = self slot, params(2..) = explicit params
	! is_ref(1) = true (self is always by-ref), is_const_ref(1) = is_const
	allocate(fn%params          ( names%len_ ))
	allocate(fn%param_names%v   ( names%len_ ))
	allocate(decl%params        ( 1 + names%len_ ))
	allocate(decl%is_ref        ( 1 + names%len_ ))
	allocate(decl%is_const_ref  ( 1 + names%len_ ))

	! Self is slot 1
	decl%params(1)       = parser%self_loc_id
	decl%is_ref(1)       = .true.
	decl%is_const_ref(1) = is_const

	do i = 1, names%len_
		fn%param_names%v(i)%s = names%v(i)%s
		fn%params(i) = types%v(i)

		parser%num_locs = parser%num_locs + 1
		decl%params(1 + i)       = parser%num_locs
		decl%is_ref(1 + i)       = is_ref%v(i)
		decl%is_const_ref(1 + i) = is_const_ref%v(i)

		const_param = is_const_ref%v(i)
		call parser%locs%insert(fn%param_names%v(i)%s, fn%params(i), parser%num_locs, &
			is_const = const_param)
	end do

	fn%type%type = void_type
	rank = 0
	if (parser%current_kind() == colon_token) then
		call parser%match(colon_token, colon)
		call parser%parse_type(type_text, type)
		fn%type = type
	end if

	parser%fn_name = identifier%text
	parser%fn_type = fn%type

	in_fn_body0 = parser%in_fn_body
	parser%in_fn_body = .true.
	call parser%parse_statement(body)
	parser%in_fn_body = in_fn_body0

	if (.not. parser%returned .and. fn%type%type /= void_type) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		call parser%diagnostics%push( &
			err_no_return(parser%context(), &
			span, identifier%text))
	else if (parser%returned .and. .not. all_paths_return(body)) then
		span = new_span(fn_beg, fn_name_end - fn_beg + 1)
		if (permissive_return) then
			if (parser%ipass /= 0) write(error_unit, '(a)') warn_missing_return(parser%context(), &
				span, identifier%text)
		else
			call parser%diagnostics%push( &
				err_missing_return(parser%context(), &
				span, identifier%text))
		end if
	end if

	parser%fn_type%type = any_type

	! Clear method context before registering fn
	parser%in_method = .false.
	parser%in_const_method = .false.

	! Register method in global fns with mangled name "0StructName::method_name"
	parser%num_fns = parser%num_fns + 1
	decl%id_index  = parser%num_fns

	decl%kind       = fn_declaration
	decl%identifier = identifier
	decl%num_locs   = parser%num_locs
	decl%src_id     = fn_src_id
	decl%src_pos    = fn_beg
	call syntax_node_move(body, decl%body)

	call parser%vars%pop_scope()
	call parser%locs%pop_scope()
	parser%is_loc = .false.

	allocate(fn%node)
	fn%node = decl

	overwrite = .true.
	if (parser%ipass == 0) overwrite = .false.

	mangled_name = "0" // struct_name // "::" // identifier%text
	io = 0
	call parser%fns%insert(mangled_name, fn, decl%id_index, io, overwrite = overwrite)
	if (parser%ipass == 0) call parser%fn_names%push(mangled_name)

end subroutine parse_method_declaration

!===============================================================================

recursive module subroutine parse_struct_instance(parser, inst, struct_name)

	! A struct instantiator initializes all the members of an instance of a
	! struct

	class(parser_t), target :: parser

	type(syntax_node_t), intent(out) :: inst
	character(len = *), intent(in), optional :: struct_name

	!********

	character(len = :), allocatable :: unset_name, exp_type, act_type, lookup_name

	integer :: io, pos0, pos1, struct_id, member_id, id1(1)

	logical :: is_ok
	logical, allocatable :: member_set(:)

	type(struct_t), pointer :: struct

	type(syntax_node_t) :: mem

	type(syntax_token_t) :: identifier, name, equals, comma, lbrace, rbrace, dummy

	type(text_span_t) :: span

	type(value_t) :: member

	!print *, "starting parse_struct_instance()"

	if (present(struct_name)) then
		! Called from parse_qualified_expr with pre-parsed qualified name
		lookup_name = struct_name
	else
		! Original path: parse identifier from current position
		call parser%match(identifier_token, identifier)
		lookup_name = identifier%text
	end if

	!print *, "parsing struct instance of lookup_name = ", lookup_name

	struct_id = parser%structs%find(lookup_name)
	if (struct_id == 0) then
		! Both callers of parse_struct_instance() (parse_expr.f90 and
		! parse_qualified_expr() in this file) already gate the call on
		! parser%structs%exists(lookup_name), so this should be unreachable.
		! Guard anyway -- struct_id == 0 would otherwise be passed straight
		! into get(), which indexes table(0) (out of bounds) -- matching the
		! same defensive check in parse_dot() (parse_expr.f90)
		write(*,*) err_int(IC_UNREACHABLE_STRUCT_LOOKUP, "unreachable struct lookup failure")
		call internal_error()
	end if
	struct => parser%structs%get(struct_id)

	call parser%match(lbrace_token, lbrace)

	inst%kind = struct_instance_expr
	!inst%identifier = identifier

	! Save everything in the inst syntax node
	inst%val%type = struct_type
	allocate(inst%val%struct( struct%num_vars ))
	allocate(inst%members   ( struct%num_vars ))

	member_set = spread(.false., 1, struct%num_vars)

	inst%struct_name = lookup_name
	inst%val%struct_name = lookup_name
	inst%val%struct_cookie = struct%cookie
	inst%val%struct_reg_idx = struct%reg_idx

	!print *, "struct name = ", inst%struct_name

	do while ( &
		parser%current_kind() /= rbrace_token .and. &
		parser%current_kind() /= eof_token)

		pos0 = parser%pos

		! Members must be named (`x = expr`).  Positional init was considered
		! and rejected: it silently breaks when a struct's member order
		! changes

		call parser%match(identifier_token, name)
		call parser%match(equals_token, equals)
		pos1   = parser%current_pos()
		call parser%parse_expr(expr=mem)
		call parser%check_enum_name_value(mem)

		!print *, "name%text = ", name%text

		!print *, "allocated = ", allocated(struct%vars%dicts(1)%root)
		!print *, "char root = ", struct%vars%dicts(1)%root%split_char
		!print *, "char mid  = ", struct%vars%dicts(1)%root%mid%split_char

		call struct%vars%search(name%text, member_id, io, member)
		!print *, "member io = ", io
		!print *, "member id = ", member_id
		is_ok = io == 0
		if (.not. is_ok) then
			span = new_span(name%pos, len(name%text))
			call parser%diagnostics%push(err_bad_member_name_short( &
				parser%context(), &
				span, &
				name%text, &
				lookup_name, &
				struct%vars%closest(name%text)))
			!return
		end if

		!print *, "member type = ", kind_name(member%type)
		!print *, "mem    type = ", kind_name(mem%val%type)
		if (is_ok) then
		if (types_match(member, mem%val) /= TYPE_MATCH) then

			exp_type = type_name(member)
			act_type = type_name(mem%val)

			!span = new_span(name%pos, parser%current_pos() - name%pos + 1)      ! `mem = expr`
			span = new_span(pos1, parser%current_pos() - pos1) ! just `expr`
			call parser%diagnostics%push(err_bad_member_type( &
				parser%context(), &
				span, &
				name%text, &
				lookup_name, &
				act_type, &
				exp_type))

		end if
		end if
		!print *, "mem type = ", kind_name(mem%val%type)

		! member_id may be out-of-bounds.  Probably want to parse the rest of
		! the tokens in this loop but not try any indexing by member_id if not
		! ok

		if (is_ok) then

			if (member_set(member_id)) then
				span = new_span(name%pos, len(name%text))
				call parser%diagnostics%push(err_reset_member( &
					parser%context(), &
					span, &
					name%text, &
					lookup_name))
			end if

			! Members can be instantiated out of order.  Insert by id, not loop iterator
			inst%val%struct( member_id ) = mem%val
			inst%members( member_id ) = mem
			member_set(member_id) = .true.

		end if

		if (parser%current_kind() /= rbrace_token) then
			call parser%match(comma_token, comma)
		end if

		! break infinite loop
		if (parser%pos == pos0) call parser%next(dummy)

	end do

	call parser%match(rbrace_token, rbrace)

	! Use a boolean array to check if all members are set.  You could have the
	! correct number but with duplicates and other members missing
	!print *, "member_set = ", member_set
	if (.not. all(member_set)) then

		! There could be more than 1 unset member but we only log diag for the
		! 1st one
		id1 = findlocl1(member_set, .false.)
		unset_name = struct%member_names%v(id1(1))%s
		!print *, "id1 = ", id1
		!print *, "name = ", unset_name

		if (present(struct_name)) then
			! Use lbrace position for qualified struct instances
			span = new_span(lbrace%pos, len(lookup_name))
		else
			span = new_span(identifier%pos, len(identifier%text))
		end if
		call parser%diagnostics%push(err_unset_member( &
			parser%context(), &
			span, &
			unset_name, &
			lookup_name))
	end if

	!print *, "ending parse_struct_instance()"

end subroutine parse_struct_instance

!===============================================================================

end submodule syntran__parse_struct

!===============================================================================
