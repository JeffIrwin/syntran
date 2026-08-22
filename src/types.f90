
!===============================================================================

module syntran__types_m

	use syntran__value_m

	implicit none

	integer, parameter :: &
		TYPE_ARRAY_ENUM_MISMATCH = 6, &
		TYPE_ARRAY_STRUCT_MISMATCH = 5, &
		TYPE_RANK_MISMATCH = 4, &
		TYPE_ARRAY_MISMATCH = 3, &
		TYPE_STRUCT_MISMATCH = 2, &
		TYPE_MISMATCH = 1, &
		TYPE_MATCH = 0

	!********

	type fn_t
		! Function signature: input and output types

		! Return type.  "A type is a value!"
		type(value_t) :: type

		! Polymorphic intrinsic fns (e.g. i32 min(1, 2) vs f32 min(1.0, 2.0),
		! but not bool min(true, false)) don't need a types(:,:) matrix here:
		! resolve_overload() (intr_fns.f90) rewrites the call's identifier to
		! a type-specific mangled name (e.g. "min" -> "0min_i32"/"0min_f64")
		! from the first arg's type at parse time, before this fn_t's single
		! `type`/`params` are ever consulted

		! Arguments/parameters.  Technically, "arguments" in most languages are
		! what Fortran calls "actual arguments" and "parameters" are Fortran
		! "dummy arguments"
		type(value_t), allocatable :: params(:)
		type(string_vector_t) :: param_names

		! Min number of variadic params.  Default < 0 means fn is not variadic
		integer :: variadic_min = -1, variadic_max = -1, variadic_type
		character(len = :), allocatable :: variadic_name

		! Reference to the function definition, i.e. the syntax node containing
		! the function parameters and body
		type(syntax_node_t), allocatable :: node

		logical :: is_intr = .true.

		! M6: integer dispatch id for intrinsic fns (0 = unassigned / user fn)
		integer :: intr_id = 0

		logical :: is_method = .false.
		logical :: is_const_method = .false.

		contains
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => fn_copy
			generic, public :: assignment(=) => copy
#endif

	end type fn_t

	!********

	type fn_entry_t
		! One slot of the fns_t hash table.  An unallocated `key` marks an
		! empty (never-used) slot

		character(len = :), allocatable :: key
		type(fn_t), allocatable :: val
		integer :: id_index = 0

	end type fn_entry_t

	!********

	type fns_t

		! Open-addressing hash table (FNV-1a + linear probing, like
		! structs_t in this same module) mapping fn name -> fn_t.  Entries
		! carry the fn_t payload directly instead of indirecting through a
		! parallel array, so there is only one table

		type(fn_entry_t), allocatable :: table(:)
		integer :: capacity = 0, count = 0
		real :: load_factor_threshold = 0.75

		! Flat array of fns from all scopes, used for efficient interpreted
		! evaluation
		type(fn_t), allocatable :: fns(:)
		integer :: num_intr_fns

		! Unlike vars_t/structs_t/enums_t, fns_t has no scope level: fn
		! declarations are only ever dispatched at translation-unit level
		! (see parse_misc.f90's `case (fn_keyword)`), and parse_fn.f90
		! explicitly resets `is_loc` after a fn body to keep it that way, so
		! there is exactly one (global) fn namespace to scope
		contains
			procedure :: &
				insert    => fn_insert, &
				find      => fn_find, &
				get       => fn_get, &
				id_at     => fn_id_at, &
				closest   => fn_closest, &
				grow_flat => fns_grow_flat, &
				rollback  => fns_rollback

	end type fns_t

	!********

	type syntax_token_t

		integer :: kind
		type(value_t) :: val
		integer :: pos
		!integer :: unit_ = 0  ! translation unit (src file) index for error diagnostic context
		integer :: unit_   ! translation unit (src file) index for error diagnostic context
		character(len = :), allocatable :: text

		contains
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => syntax_token_copy
			generic, public :: assignment(=) => copy
#endif

	end type syntax_token_t

	!********

	type syntax_node_t

		! NOTE: when adding/removing a member here, four parallel component
		! lists in types_copy.f90 must be kept in sync by hand: syntax_node_copy,
		! syntax_node_destroy, syntax_node_move, and syntax_node_move_into.
		! Skipping one silently corrupts assignment, leaks memory, or drops
		! data on a move.  utils/check-node-sync.sh enforces this in CI

		integer :: kind = 0

		! This structure could be more efficient.  For example, unary
		! expressions don't use left, name expressions don't use left or right,
		! binary expressions don't use an identifier, etc.  On the other hand,
		! they're all allocatable, so they shouldn't take up much space if not
		! allocated

		type(syntax_node_t), allocatable :: left, right, members(:), &
			condition, if_clause, else_clause, body, array, member

		! Array expression syntax nodes.  Trailing underscore on lbound_,
		! ubound_, size_ (like len_ below) to avoid conflicts w/ Fortran
		! intrinsics of the same name
		type(syntax_node_t), allocatable :: lbound_, step, ubound_, len_, &
			elems(:), rank

		type(syntax_node_t), allocatable :: lsubscripts(:), size_(:), args(:), &
			usubscripts(:), ssubscripts(:)

		! Either scalar_sub, range_sub (unit step [0:2]), all_sub ([:]), or
		! step_sub ([0: 2: 8])
		integer :: sub_kind

		! Flags for omitted lower/upper bounds in range/step subscripts, e.g.
		! vec[3:] (usub_omit), vec[:5] (lsub_omit), vec[:-1:] (both with step)
		logical :: lsub_omit = .false., usub_omit = .false.

		type(syntax_token_t) :: op, identifier

		integer :: id_index = 0, num_locs
		logical :: is_loc = .false.

		! Set only on the array_expr synthesized by parse_enum_name_expr() for
		! a bare enum type name (e.g. `Suit`).  Structurally identical to a
		! literal enum array (both carry val%enum_name), so this flag is the
		! only way to tell "the special form" apart from "a real array value"
		! at the sites that ban binding/passing a bare enum name as a value
		! (see check_enum_name_value())
		logical :: is_enum_name = .false.

		! When a dot_expr's root was a fn_call_expr or method_call_expr (e.g.
		! `fn().field`), root_kind stores the original kind so evaluators can
		! re-evaluate the root as a function call and then apply the member chain.
		integer :: root_kind = 0

		integer, allocatable :: params(:)
		logical, allocatable :: is_ref(:)       ! is param passed by reference?
		logical, allocatable :: is_const_ref(:) ! is ref param declared &const?

		type(value_t) :: val

		character(len = :), allocatable :: struct_name

		! Module prefix for qualified names (e.g., "std" in "std::println")
		character(len = :), allocatable :: module_prefix

		type(string_vector_t) :: diagnostics

		! Only used to handle comment/whitespace lines for now
		logical :: is_empty = .false.

		! Is the parser expecting more input, e.g. from a continued line in the
		! interactive interpreter to match a semicolon, brace, etc.?
		logical :: expecting = .false., first_expecting = .false.
		character(len = :), allocatable :: first_expected

		contains

			procedure :: to_str => syntax_node_str, log_diagnostics

			! For gfortran, use a hand-written copy constructor.  For ifx, use
			! the intrinsic copy constructor.  If you try anything else, both
			! compilers will crash :(
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => syntax_node_copy
			generic, public :: assignment(=) => copy
#endif

	end type syntax_node_t

	!********

	! Dependencies between types could make this module difficult to split into
	! separate files.  I think I like the monolithic design anyway

	type var_entry_t
		! One slot of a var_dict_t hash table.  An unallocated `key` marks an
		! empty (never-used) slot

		character(len = :), allocatable :: key
		type(value_t), allocatable :: val
		integer :: id_index = 0
		logical :: is_const = .false.

	end type var_entry_t

	!********

	type var_dict_t
		! This is the variable dictionary of a single scope.  Open-addressing
		! hash table (FNV-1a + linear probing, like structs_t/fns_t)

		type(var_entry_t), allocatable :: table(:)
		integer :: capacity = 0, count = 0
		real :: load_factor_threshold = 0.75
	end type var_dict_t

	!********

	type vars_t

		! A list of variable dictionaries for each scope level used during
		! parsing
		type(var_dict_t), allocatable :: dicts(:)
		integer :: scope_cap

		! Flat array of variables from all scopes, used for efficient
		! interpreted evaluation
		type(value_t), allocatable :: vals(:)

		! This is the scope level.  Each nested block statement that is entered
		! pushes 1 to scope.  Popping out of a block decrements the scope.
		! Each scope level has its own variable dict in dicts(:)
		integer :: scope = 1

		contains
			procedure :: &
				insert    => var_insert, &
				search    => var_search, &
				is_const  => var_is_const, &
				closest   => var_closest, &
				push_scope, pop_scope

			! This is required unfortunately
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => vars_copy
			generic, public :: assignment(=) => copy
#endif

	end type vars_t

	!********

	type struct_t
		! User-defined structure, aka derived type

		type(string_vector_t) :: member_names

		type(vars_t) :: vars  ! can't compile w/o allocatable if vars_t is defined below
		!type(vars_t), allocatable :: vars
		integer :: num_vars = 0

		! Canonical, alias-independent identity: "<defining src file>::<local
		! struct name>", set once at declaration time. c.f. value_t%struct_cookie
		character(len = :), allocatable :: cookie

		! struct_reg_set()'s returned registry slot for this struct's member
		! names, stashed here so every instantiation site can copy it onto
		! value_t%struct_reg_idx without a second registry lookup
		integer :: reg_idx = 0

		contains
			! This is also required unfortunately
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => struct_copy
			generic, public :: assignment(=) => copy
#endif

	end type struct_t

	!********

	type struct_entry_t
		! One slot of the structs_t hash table.  An unallocated `key` marks an
		! empty (never-used) slot

		character(len = :), allocatable :: key
		type(struct_t), allocatable :: val
		integer :: id_index = 0

	end type struct_entry_t

	!********

	type structs_t

		! Open-addressing hash table (FNV-1a + linear probing, like
		! map_i32_t in utils.f90) mapping struct name -> struct_t.  Unlike
		! map_i32_t, entries carry the struct_t payload directly instead of
		! indirecting through a parallel array, so there is only one table

		type(struct_entry_t), allocatable :: table(:)
		integer :: capacity = 0, count = 0
		real :: load_factor_threshold = 0.75

		! Number of structs ever declared (== the highest id_index in the
		! table).  Persisted across REPL lines in state_t (c.f. fns_t%fns,
		! whose size serves the same purpose for fns) and used by
		! syntax_parse() (core.f90) as the "declared before this REPL line"
		! threshold for rollback()
		integer :: num_structs = 0

		contains
			procedure :: &
				insert   => struct_insert, &
				find     => struct_find, &
				get      => struct_get, &
				id_at    => struct_id_at, &
				exists   => struct_exists, &
				closest  => struct_closest, &
				rollback => structs_rollback

	end type structs_t

	!********

	type enum_t
		! User-defined enumeration: a named integer constant ("variant") per
		! entry.  Variants are compile-time constants, so unlike struct_t
		! there is no vars_t of member types -- just parallel arrays of
		! variant names and their backing i32 values

		type(string_vector_t) :: variant_names
		integer, allocatable :: variant_values(:)  ! parallel to variant_names
		integer :: num_vars = 0

		! Canonical, alias-independent identity: "<defining src file>::<local
		! enum name>", set once at declaration time. c.f. value_t%enum_cookie
		character(len = :), allocatable :: cookie

		contains
			! This is also required unfortunately
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => enum_copy
			generic, public :: assignment(=) => copy
#endif

	end type enum_t

	!********

	type enum_entry_t
		! One slot of the enums_t hash table.  An unallocated `key` marks an
		! empty (never-used) slot

		character(len = :), allocatable :: key
		type(enum_t), allocatable :: val
		integer :: id_index = 0

	end type enum_entry_t

	!********

	type enums_t

		! Open-addressing hash table (FNV-1a + linear probing), mirroring
		! structs_t, mapping enum name -> enum_t

		type(enum_entry_t), allocatable :: table(:)
		integer :: capacity = 0, count = 0
		real :: load_factor_threshold = 0.75

		! Number of enums ever declared (== the highest id_index in the
		! table).  Mirrors structs_t%num_structs
		integer :: num_enums = 0

		contains
			procedure :: &
				insert   => enum_insert, &
				find     => enum_find, &
				get      => enum_get, &
				id_at    => enum_id_at, &
				exists   => enum_exists, &
				closest  => enum_closest, &
				rollback => enums_rollback

	end type enums_t

	!********

	type syntax_token_vector_t
		type(syntax_token_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push => push_token
	end type syntax_token_vector_t

	!********

	type syntax_node_vector_t
		type(syntax_node_t), allocatable :: v(:)
		integer :: len_, cap
		contains
			procedure :: push      => push_node
			procedure :: push_move => push_node_move
#ifndef SYNTRAN_INTEL
			procedure, pass(dst) :: copy => syntax_node_vector_copy
			generic, public :: assignment(=) => copy
#endif
	end type syntax_node_vector_t

!===============================================================================

	! Interface declarations for submodule procedures

	interface

		!***************************************
		! types_copy.f90 procedures
		!***************************************

		recursive module subroutine syntax_token_copy(dst, src)
			class(syntax_token_t), intent(inout) :: dst
			class(syntax_token_t), intent(in)    :: src
		end subroutine syntax_token_copy

		recursive module subroutine vars_copy(dst, src)
			class(vars_t), intent(inout) :: dst
			class(vars_t), intent(in)    :: src
		end subroutine vars_copy

		module subroutine var_dict_copy(dst, src)
			type(var_dict_t), intent(inout) :: dst
			type(var_dict_t), intent(in)    :: src
		end subroutine var_dict_copy

		recursive module subroutine struct_copy(dst, src)
			class(struct_t), intent(inout) :: dst
			class(struct_t), intent(in)    :: src
		end subroutine struct_copy

		recursive module subroutine enum_copy(dst, src)
			class(enum_t), intent(inout) :: dst
			class(enum_t), intent(in)    :: src
		end subroutine enum_copy

		recursive module subroutine fn_copy(dst, src)
			class(fn_t), intent(inout) :: dst
			class(fn_t), intent(in)    :: src
		end subroutine fn_copy

		recursive module subroutine fn_move(src, dst)
			type(fn_t), intent(inout) :: src
			type(fn_t), intent(out)   :: dst
		end subroutine fn_move

		!***************************************
		! Explicit teardown ("destroy") family, mirroring the "copy" family
		! above.  Never rely on a bare deallocate() of these nested-
		! allocatable-value_t containers -- see value_array_destroy() and
		! value_destroy() (value.f90) for the rationale, which applies
		! equally here since var_dict_t/structs_t/enums_t/fns_t all store
		! value_t (or a type containing it) in their table slots
		!***************************************

		recursive module subroutine var_dict_destroy(dict)
			type(var_dict_t), intent(inout) :: dict
		end subroutine var_dict_destroy

		recursive module subroutine vars_destroy(vars)
			type(vars_t), intent(inout) :: vars
		end subroutine vars_destroy

		recursive module subroutine struct_destroy(struct)
			type(struct_t), intent(inout) :: struct
		end subroutine struct_destroy

		recursive module subroutine struct_table_destroy(table)
			type(struct_entry_t), intent(inout) :: table(:)
		end subroutine struct_table_destroy

		recursive module subroutine structs_destroy(dict)
			type(structs_t), intent(inout) :: dict
		end subroutine structs_destroy

		module subroutine enum_destroy(enum)
			type(enum_t), intent(inout) :: enum
		end subroutine enum_destroy

		module subroutine enum_table_destroy(table)
			type(enum_entry_t), intent(inout) :: table(:)
		end subroutine enum_table_destroy

		module subroutine enums_destroy(dict)
			type(enums_t), intent(inout) :: dict
		end subroutine enums_destroy

		recursive module subroutine syntax_node_destroy(node)
			type(syntax_node_t), intent(inout) :: node
		end subroutine syntax_node_destroy

		recursive module subroutine fn_destroy(fn)
			type(fn_t), intent(inout) :: fn
		end subroutine fn_destroy

		recursive module subroutine fns_destroy(dict)
			type(fns_t), intent(inout) :: dict
		end subroutine fns_destroy

		recursive module subroutine syntax_node_vector_copy(dst, src)
			class(syntax_node_vector_t), intent(inout) :: dst
			class(syntax_node_vector_t), intent(in)    :: src
		end subroutine syntax_node_vector_copy

		recursive module subroutine syntax_node_copy(dst, src)
			class(syntax_node_t), intent(inout) :: dst
			class(syntax_node_t), intent(in)    :: src
		end subroutine syntax_node_copy

		recursive module subroutine syntax_node_move(src, dst)
			type(syntax_node_t), intent(inout)            :: src
			type(syntax_node_t), allocatable, intent(out) :: dst
		end subroutine syntax_node_move

		recursive module subroutine syntax_node_move_into(src, dst)
			type(syntax_node_t), intent(inout) :: src, dst
		end subroutine syntax_node_move_into

		! ternary_tree_copy() was here.  It's no longer needed: var_dict_t is
		! now a flat hash table (var_entry_t table(:) in this module) instead
		! of a ternary tree, so intrinsic assignment suffices in vars_copy()

		recursive module subroutine syntax_nodes_copy(dst, src)
			type(syntax_node_t), allocatable, intent(inout) :: dst(:)
			type(syntax_node_t), intent(in)  :: src(:)
		end subroutine syntax_nodes_copy

		!***************************************
		! types_dict.f90 procedures
		!***************************************

		module function fn_find(dict, key) result(slot)
			! Returns the table slot for `key`, or 0 if not present.  The slot
			! is only valid until the next insert() (a resize rehashes the
			! table), so callers must read get()/id_at() before any
			! subsequent insert()
			class(fns_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			integer :: slot
		end function fn_find

		module function fn_get(dict, slot) result(val)
			class(fns_t), intent(in), target :: dict
			integer, intent(in) :: slot
			type(fn_t), pointer :: val
		end function fn_get

		module function fn_id_at(dict, slot) result(id_index)
			class(fns_t), intent(in) :: dict
			integer, intent(in) :: slot
			integer :: id_index
		end function fn_id_at

		module subroutine fn_insert(dict, key, val, id_index, iostat, overwrite)
			class(fns_t) :: dict
			character(len = *), intent(in) :: key
			type(fn_t), intent(in) :: val
			integer, intent(inout) :: id_index
			integer, intent(out), optional :: iostat
			logical, intent(in), optional :: overwrite
		end subroutine fn_insert

		module subroutine fns_grow_flat(dict, n)
			class(fns_t), intent(inout) :: dict
			integer, intent(in) :: n
		end subroutine fns_grow_flat

		module subroutine fns_rollback(dict, num_fns0)
			class(fns_t), intent(inout) :: dict
			integer, intent(in) :: num_fns0
		end subroutine fns_rollback

		module subroutine var_insert(dict, key, val, id_index, iostat, overwrite, is_const)
			class(vars_t) :: dict
			character(len = *), intent(in) :: key
			type(value_t), intent(in) :: val
			integer, intent(in) :: id_index
			integer, intent(out), optional :: iostat
			logical, intent(in), optional :: overwrite
			logical, intent(in), optional :: is_const
		end subroutine var_insert

		module subroutine var_search(dict, key, id_index, iostat, val, is_const)
			class(vars_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			integer, intent(out) :: id_index
			type(value_t), intent(out) :: val
			integer, intent(out), optional :: iostat
			logical, intent(out), optional :: is_const
		end subroutine var_search

		module function var_is_const(dict, key) result(is_const)
			class(vars_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			logical :: is_const
		end function var_is_const

		module function var_closest(dict, key) result(closest)
			class(vars_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			character(len = :), allocatable :: closest
		end function var_closest

		module function fn_closest(dict, key) result(closest)
			class(fns_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			character(len = :), allocatable :: closest
		end function fn_closest

		module subroutine push_scope(dict)
			class(vars_t) :: dict
		end subroutine push_scope

		module subroutine pop_scope(dict)
			class(vars_t) :: dict
		end subroutine pop_scope

		module subroutine push_token(vector, val)
			class(syntax_token_vector_t) :: vector
			type(syntax_token_t) :: val
		end subroutine push_token

		module subroutine push_node(vector, val)
			class(syntax_node_vector_t) :: vector
			type(syntax_node_t) :: val
		end subroutine push_node

		module subroutine push_node_move(vector, val)
			class(syntax_node_vector_t) :: vector
			type(syntax_node_t), intent(inout) :: val
		end subroutine push_node_move

		! ternary_search(), ternary_is_const(), ternary_closest(), and
		! ternary_insert() were here.  var_dict_t is now a flat hash table
		! (var_entry_t table(:)), so these tree walkers were replaced by
		! var_grow() (a submodule-local helper in types_dict.f90, not part of
		! the public API, mirroring struct_grow()/fn_grow()) plus the
		! var_insert/var_search/var_is_const/var_closest bodies below

		module subroutine struct_insert(dict, key, val, id_index, iostat, overwrite)
			class(structs_t) :: dict
			character(len = *), intent(in) :: key
			type(struct_t), intent(in) :: val
			integer, intent(inout) :: id_index
			integer, intent(out), optional :: iostat
			logical, intent(in), optional :: overwrite
		end subroutine struct_insert

		module function struct_find(dict, key) result(slot)
			! Returns the table slot for `key`, or 0 if not present.  The slot
			! is only valid until the next insert() (a resize rehashes the
			! table), so callers must read get()/id_at() before any
			! subsequent insert()
			class(structs_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			integer :: slot
		end function struct_find

		module function struct_get(dict, slot) result(val)
			class(structs_t), intent(in), target :: dict
			integer, intent(in) :: slot
			type(struct_t), pointer :: val
		end function struct_get

		module function struct_id_at(dict, slot) result(id_index)
			class(structs_t), intent(in) :: dict
			integer, intent(in) :: slot
			integer :: id_index
		end function struct_id_at

		module function struct_exists(dict, key) result(exists)
			class(structs_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			logical :: exists
		end function struct_exists

		module function struct_closest(dict, key) result(closest)
			class(structs_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			character(len = :), allocatable :: closest
		end function struct_closest

		module subroutine structs_rollback(dict, num_structs0)
			class(structs_t), intent(inout) :: dict
			integer, intent(in) :: num_structs0
		end subroutine structs_rollback

		module subroutine enum_insert(dict, key, val, id_index, iostat, overwrite)
			class(enums_t) :: dict
			character(len = *), intent(in) :: key
			type(enum_t), intent(in) :: val
			integer, intent(inout) :: id_index
			integer, intent(out), optional :: iostat
			logical, intent(in), optional :: overwrite
		end subroutine enum_insert

		module function enum_find(dict, key) result(slot)
			! Returns the table slot for `key`, or 0 if not present.  Mirrors
			! struct_find() -- see its comment for slot validity caveats
			class(enums_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			integer :: slot
		end function enum_find

		module function enum_get(dict, slot) result(val)
			class(enums_t), intent(in), target :: dict
			integer, intent(in) :: slot
			type(enum_t), pointer :: val
		end function enum_get

		module function enum_id_at(dict, slot) result(id_index)
			class(enums_t), intent(in) :: dict
			integer, intent(in) :: slot
			integer :: id_index
		end function enum_id_at

		module function enum_exists(dict, key) result(exists)
			class(enums_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			logical :: exists
		end function enum_exists

		module function enum_closest(dict, key) result(closest)
			class(enums_t), intent(in) :: dict
			character(len = *), intent(in) :: key
			character(len = :), allocatable :: closest
		end function enum_closest

		module subroutine enums_rollback(dict, num_enums0)
			class(enums_t), intent(inout) :: dict
			integer, intent(in) :: num_enums0
		end subroutine enums_rollback

		!***************************************
		! types_ops.f90 procedures
		!***************************************

		recursive module function syntax_node_str(node, indent) result(str_)
			class(syntax_node_t) :: node
			character(len = *), optional :: indent
			character(len = :), allocatable :: str_
		end function syntax_node_str

		module subroutine log_diagnostics(node, ou)
			class(syntax_node_t), intent(in) :: node
			integer, optional, intent(in) :: ou
		end subroutine log_diagnostics

		module integer function lookup_type(name, structs, enums, cookie) result(type)
			character(len = *), intent(in) :: name
			type(structs_t), intent(in), target :: structs
			type(enums_t), intent(in), target :: enums
			character(len = :), allocatable, intent(out), optional :: cookie
		end function lookup_type

		module integer function get_keyword_kind(text) result(kind)
			character(len = *), intent(in) :: text
		end function get_keyword_kind

		module logical function is_keyword(text)
			character(len = *), intent(in) :: text
		end function is_keyword

		module logical function is_identifier_or_keyword(kind)
			integer, intent(in) :: kind
		end function is_identifier_or_keyword

		module logical function is_assignment_op(op)
			integer, intent(in) :: op
		end function is_assignment_op

		module logical function is_binary_op_allowed(left, op, right, left_arr, right_arr) &
				result(allowed)
			integer, intent(in) :: left, op, right
			integer, intent(in), optional :: left_arr, right_arr
		end function is_binary_op_allowed

		module logical function is_unary_op_allowed(op, right, right_arr)
			integer, intent(in) :: op, right, right_arr
		end function is_unary_op_allowed

		module integer function get_unary_op_prec(kind) result(prec)
			integer, intent(in) :: kind
		end function get_unary_op_prec

		module integer function get_binary_op_prec(kind) result(prec)
			integer, intent(in) :: kind
		end function get_binary_op_prec

		elemental module logical function is_num_type(type)
			integer, intent(in) :: type
		end function is_num_type

		elemental module logical function is_int_type(type)
			integer, intent(in) :: type
		end function is_int_type

		elemental module logical function is_float_type(type)
			integer, intent(in) :: type
		end function is_float_type

		recursive module integer function get_binary_op_kind( &
				left, op, right, left_arr, right_arr) result(kind_)
			integer, intent(in) :: left, op, right
			integer, intent(in) :: left_arr, right_arr
		end function get_binary_op_kind

		module function scalar_to_array_type(scalar_type_) result(array_type_)
			integer, intent(in) :: scalar_type_
			integer :: array_type_
		end function scalar_to_array_type

		module function array_to_scalar_type(array_type_) result(scalar_type_)
			integer, intent(in) :: array_type_
			integer :: scalar_type_
		end function array_to_scalar_type

		recursive module function type_name(a) result(str_)
			type(value_t), intent(in) :: a
			character(len = :), allocatable :: str_
		end function type_name

		module function type_name_primitive(itype) result(str_)
			integer, intent(in) :: itype
			character(len = :), allocatable :: str_
		end function type_name_primitive

		recursive module integer function types_match(a, b) result(io)
			type(value_t), intent(in) :: a, b
		end function types_match

		module integer function matmul_out_rank(lrank, rrank) result(out_rank)
			integer, intent(in) :: lrank, rrank
		end function matmul_out_rank

		!***************************************
		! types_node.f90 procedures
		!***************************************

		module subroutine new_token(token, kind, pos, text, val)
			type(syntax_token_t), intent(out) :: token
			integer :: kind, pos
			character(len = *) :: text
			type(value_t), optional :: val
		end subroutine new_token

		module function new_syntax_node_vector() result(vector)
			type(syntax_node_vector_t) :: vector
		end function new_syntax_node_vector

		module function new_syntax_token_vector() result(vector)
			type(syntax_token_vector_t) :: vector
		end function new_syntax_token_vector

		module function new_literal_value(type, bool, i32, i64, f32, f64, str_) result(val)
			integer, intent(in) :: type
			integer(kind = 4), intent(in), optional :: i32
			integer(kind = 8), intent(in), optional :: i64
			real(kind = 4), intent(in), optional :: f32
			real(kind = 8), intent(in), optional :: f64
			logical, intent(in), optional :: bool
			character(len=*), intent(in), optional :: str_
			type(value_t) :: val
		end function new_literal_value

		module subroutine new_declaration_expr(identifier, op, right, expr)
			type(syntax_token_t), intent(in) :: identifier, op
			type(syntax_node_t) , intent(in) :: right
			type(syntax_node_t) , intent(out) :: expr
		end subroutine new_declaration_expr

		module subroutine new_name_expr(identifier, val, expr)
			type(syntax_token_t), intent(in) :: identifier
			type(value_t)                    :: val
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_name_expr

		module subroutine new_binary_expr(left, op, right, expr)
			type(syntax_node_t) , intent(inout) :: left, right
			type(syntax_token_t), intent(in)    :: op
			type(syntax_node_t) , intent(out)   :: expr
		end subroutine new_binary_expr

		module subroutine new_unary_expr(op, right, expr)
			type(syntax_node_t) , intent(inout) :: right
			type(syntax_token_t), intent(in)    :: op
			type(syntax_node_t) , intent(out)   :: expr
		end subroutine new_unary_expr

		module subroutine new_bool(bool, expr)
			logical            , intent(in)  :: bool
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_bool

		module subroutine new_f32(f32, expr)
			real(kind = 4)     , intent(in)  :: f32
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_f32

		module subroutine new_f64(f64, expr)
			real(kind = 8)     , intent(in)  :: f64
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_f64

		module subroutine new_i32(i32, expr)
			integer(kind = 4)  , intent(in)  :: i32
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_i32

		module subroutine new_i64(i64, expr)
			integer(kind = 8)  , intent(in)  :: i64
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_i64

		module subroutine new_str(str_, expr)
			character(len = *) , intent(in)  :: str_
			type(syntax_node_t), intent(out) :: expr
		end subroutine new_str

	end interface

!===============================================================================

end module syntran__types_m

!===============================================================================

