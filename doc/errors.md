# Syntran error code index

Every syntran diagnostic carries a unique, permanent code, rendered in brackets
after its severity label -- e.g. `Error[E42]: ...`, in the style of rustc's
`error[E0631]`.  Codes are grouped by category:

- **E#** -- parse-time errors (lexer/parser/semantic checks). The program
  failed before it ran.
- **R#** -- runtime errors. The program started running but hit a fault
  (bad file handle, dimension mismatch, etc.) and aborted.
- **I#** -- internal "should never happen" errors. If you see one of these,
  it's a syntran bug -- please report it.
- **W#** -- warnings. The program still runs, but something is likely wrong.

Codes are never renumbered or reused once released, even if the underlying
constructor is later removed -- this list only grows.  The registry lives in
`src/errors.f90` (`get_all_error_codes()`); a unit test
(`unit_test_error_codes` in `src/tests/test.f90`) enforces that every code is
unique and follows the `[ERIW][0-9]+` format.

## Parse-time errors

### E1 -- bad-i32

Integer literal with an explicit `'i32` suffix does not fit in 32 bits.

### E2 -- bad-i64

Integer literal with an explicit `'i64` suffix does not fit in 64 bits.

### E3 -- bad-hex32

Hexadecimal integer literal does not fit in 32 bits.

### E4 -- bad-hex64

Hexadecimal integer literal does not fit in 64 bits.

### E5 -- bad-oct32

Octal integer literal does not fit in 32 bits.

### E6 -- bad-oct64

Octal integer literal does not fit in 64 bits.

### E7 -- bad-bin32

Binary integer literal does not fit in 32 bits.

### E8 -- bad-bin64

Binary integer literal does not fit in 64 bits.

### E9 -- bad-expr

A bare expression statement was used outside the REPL.  Use `println()` to log its value.

### E10 -- unterminated-str

A string literal is missing its closing quote.

### E11 -- unterminated-raw-str

A raw string literal is missing its closing delimiter.

### E12 -- array-struct-slice

Slices are not implemented for arrays of structs.

### E13 -- struct-array-slice

Slices are not implemented for structs of arrays; only scalar subscripts are allowed there.

### E14 -- non-int-subscript

An array subscript expression is not an integer.

### E15 -- bad-f32

Real literal with an explicit `'f32` suffix could not be parsed.

### E16 -- bad-f64

Real literal with an explicit `'f64` suffix could not be parsed.

### E17 -- bad-type

An unrecognized type name was used in a type annotation.

### E18 -- bad-type-suffix

An unrecognized literal type suffix (e.g. `'i32`) was used after a number or string literal.

### E19 -- unexpected-char

The lexer encountered a character it doesn't recognize.

### E20 -- unexpected-token

The parser encountered a token it didn't expect at this position.

### E21 -- void-assign

A variable was initialized from an expression of void type.

### E22 -- redeclare-var

A variable was declared twice in the same scope.

### E23 -- redeclare-mem

A struct member was declared twice in the same struct.

### E24 -- redeclare-fn

A function was declared twice.

### E25 -- redeclare-intr-fn

A user function's name collides with a built-in (intrinsic) function.

### E26 -- redeclare-struct

A struct was declared twice.

### E27 -- redeclare-primitive

A struct was declared with a name reserved for a primitive type (e.g. `i32`).

### E28 -- undeclare-var

A variable was referenced before being declared in scope.  May include a "did you mean" suggestion.

### E29 -- undeclare-fn

A function was called before being defined.  May include a "did you mean" suggestion.

### E30 -- std-only-fn

A standard-library function was called without its required `std::` prefix.

### E31 -- no-return

A non-void function has no `return` statements at all.

### E32 -- missing-return

Not all code paths in a non-void function end in a `return` statement.

### E33 -- bad-arg-count

A function call was given the wrong number of arguments.

### E34 -- too-few-args

A variadic function call was given fewer than its minimum number of arguments.

### E35 -- too-many-args

A variadic function call was given more than its maximum number of arguments.

### E36 -- bad-sub-count

An array was subscripted with the wrong number of indices for its rank.

### E37 -- bad-sub-rank

An array used as a subscript index is not rank-1.

### E38 -- empty-step

A slice step was omitted between two colons (e.g. `a[1::4]`); write it explicitly.

### E39 -- scalar-subscript

A scalar variable was subscripted as if it were an array.

### E40 -- bad-cat-rank

A concatenated array operand is not rank-1.

### E41 -- bad-ret-type

A function's `return` value does not match its declared scalar return type.

### E42 -- bad-arg-type

A function argument's type does not match the corresponding parameter's declared type.

### E43 -- bad-arg-val

A `&` reference parameter was given a plain value argument instead of a reference.

### E44 -- bad-arg-ref

A by-value parameter was given a `&` reference argument instead of a value.

### E45 -- non-name-ref

A `&` reference was taken of an expression that isn't a plain variable name.

### E46 -- sub-ref

A `&` reference was taken of a subscripted expression, which isn't allowed.

### E47 -- bad-arg-rank

A function argument array's rank does not match the corresponding parameter's declared rank.

### E48 -- binary-types

A binary operator (e.g. `+`) is not defined for the given pair of operand types.

### E49 -- binary-ranks

A binary operator's array operands have mismatched ranks.

### E50 -- unary-types

A unary operator (e.g. unary `-`) is not defined for the given operand type.

### E51 -- non-array-loop

The range expression of a `for` loop is not an array.

### E52 -- non-bool-condition

The condition of an `if`/`while`/etc. statement is not a `bool`.

### E53 -- non-float-len-range

A length-based array range bound (`[a; n]`) is not a float.

### E54 -- non-int-len

An array length expression is not an integer.

### E55 -- bound-type-mismatch

The lower and upper bounds of an array range have mismatched types.

### E56 -- non-num-range

An array range bound is not a numeric type.

### E57 -- non-sca-val

A value used to fill a uniform array (`[v; n]`) is not a scalar.

### E58 -- non-int-range

An array range bound is not an `i32` integer.

### E59 -- het-array

An array literal is heterogeneous: an element's type doesn't match the first element's type.

### E60 -- unset-member

A struct instance was constructed without initializing all of its members.

### E61 -- reset-member

A struct instance initializer set the same member twice.

### E62 -- non-struct-dot

Dot member access (`.`) was used on a variable that isn't a struct.

### E63 -- bad-member-name

A dot expression referenced a member name that doesn't exist on the struct (variable and type both named in the message).

### E64 -- bad-member-name-short

A struct instantiation (`Type{...}`) referenced a member name that doesn't exist on the struct.

### E65 -- bad-member-type

A struct member was initialized with a value of the wrong type.

### E66 -- inc-404

An `#include` file could not be found.

### E67 -- inc-read

An `#include` file was found but could not be read.

### E68 -- mod-404

A `use`d module file could not be found.

### E69 -- mod-read

A `use`d module file was found but could not be read.

### E70 -- circular-import

Two or more modules import each other, forming a circular dependency.

### E71 -- duplicate-import

The same module was imported more than once.

### E72 -- mod-hyphen

A module name contains a hyphen; use underscores instead.

### E73 -- mod-keyword

A module name is a reserved language keyword.

### E74 -- mod-reserved-std

The module name `std` is reserved for the standard library.

### E75 -- mod-space

A module name contains a space.

### E76 -- alias-keyword

A `use ... as <alias>` alias is a reserved language keyword.

### E77 -- alias-reserved-std

The module alias `std` is reserved and cannot be reused.

### E78 -- alias-hyphen

A module alias contains a hyphen.

### E79 -- alias-space

A module alias contains a space.

### E80 -- alias-with-doublecolon

A `use module::*` glob import was combined with an alias, which isn't allowed.

### E81 -- 404

The top-level source file given to the interpreter could not be found.

## Internal errors

### I1 -- eval-unary-type

A unary operator could not be evaluated for its operand's runtime type (math/bool kernels).

### I2 -- eval-binary-types

A binary operator could not be evaluated for its operands' runtime types (math/bool kernels).

### I3 -- eval-len-array

`len` array evaluation hit a runtime type with no implementation.

### I4 -- eval-unary-op

An unexpected/unrecognized unary operator token reached the evaluator.

### I5 -- eval-node

An unexpected AST node kind reached the evaluator.

### I6 -- eval-binary-op

An unexpected/unrecognized binary operator token reached the evaluator.

### I7 -- unit-step-array-type

A `for` loop's unit-step (`a:b`) range has a runtime type other than `i32`/`i64` (AST and VM paths).

### I8 -- for-step-zero

A `for` loop's integer step (`a:step:b`) evaluated to 0 (AST and VM paths).

### I9 -- for-step-zero-f

A `for` loop's float step (`a:step:b`) evaluated to 0.0 (AST and VM paths).

### I10 -- step-array-type

A `for` loop's step-range has an unsupported runtime type (AST and VM paths).

### I11 -- bound-len-array-type

A `for` loop's length-based range has an unsupported runtime type (AST and VM paths).

### I12 -- for-array-kind

A `for` loop's range array has an unrecognized/unimplemented array kind (AST and VM paths).

### I13 -- str-char-subscript

An unexpected subscript kind was used while indexing the characters of a string.

### I14 -- array-step-0

An array range literal's (non-loop) integer step evaluated to 0.

### I15 -- array-step-0-f

An array range literal's (non-loop) float step evaluated to 0.0.

### I16 -- unexpected-array-kind

An array value has an unrecognized internal `array%kind`.

### I17 -- alloc-array-type

`allocate_array()` was asked to allocate a buffer for an unsupported element type.

### I18 -- array-type-not-impl

`new_array()` was asked to create a buffer for an unsupported element type.

### I19 -- unexpected-assign-op

A compound-assignment operator (e.g. `+=`) token was not recognized.

### I20 -- subscript-step-0

An array slice subscript's step (`a[::s]`) evaluated to 0.

### I21 -- bad-array-subscript-type

An array used as a subscript index has an unsupported element type.

### I22 -- eval-subscript-kind

An unrecognized subscript kind reached 1-D subscript evaluation.

### I23 -- bad-array-val-type

`get_array_val()` was asked to read an element of an unsupported array type.

### I24 -- unknown-name-expr-type

A name expression resolved to `unknown_type` at evaluation time.

### I25 -- bad-type-expect-array

A subscripted name expression's underlying value is not an array.

### I26 -- unexpected-user-fn

A user-defined function call node is missing its parameter list.

### I27 -- fn-end-reached

Execution fell off the end of a function body without hitting a `return` statement.

### I28 -- unexpected-intr-fn

An intrinsic function call did not match any known intrinsic name.

### I29 -- push-array-type

`push_array()` was asked to append to a buffer of an unsupported element type.

### I30 -- trim-array-type

`trim_array()` was asked to shrink a buffer of an unsupported element type.

### I31 -- convert-f32

A value could not be converted to `f32` (e.g. wrong source type; use `parse_f32()` for strings).

### I32 -- convert-f64

A value could not be converted to `f64` (e.g. wrong source type; use `parse_f64()` for strings).

### I33 -- convert-i32

A value could not be converted to `i32` (e.g. wrong source type; use `parse_i32()` for strings).

### I34 -- convert-i32-arr

An array value could not be converted to an `i32` array (unsupported source element type).

### I35 -- convert-i64

A value could not be converted to `i64` (e.g. wrong source type; use `parse_i64()` for strings).

### I36 -- convert-i64-arr

An array value could not be converted to an `i64` array (unsupported source element type).

### I37 -- scope-stack-empty

A scope was popped when the variable-dictionary scope stack was already empty (unbalanced `}`).

### I38 -- unreachable-struct-lookup

A struct lookup by name failed for a value already confirmed to be that struct type.

## Runtime errors

### R1 -- matmul-dim

The `@` matrix-multiplication operator's operands have incompatible inner dimensions.

### R2 -- parse-i32

`parse_i32()` could not parse its string argument as an `i32`.

### R3 -- parse-i64

`parse_i64()` could not parse its string argument as an `i64`.

### R4 -- parse-f32

`parse_f32()` could not parse its string argument as an `f32`.

### R5 -- parse-f64

`parse_f64()` could not parse its string argument as an `f64`.

### R6 -- bad-file-mode

`open()` was given a file-mode character other than `r` or `w`.

### R7 -- file-rw-mode

`open()` was given a mode string combining `r` and `w`, which isn't supported.

### R8 -- open-file

`open()` failed to open the requested file (see the accompanying `iostat`).

### R9 -- readln-not-open

`readln()` was called on a file handle that isn't open.

### R10 -- readln-not-read-mode

`readln()` was called on a file that wasn't opened in read (`"r"`) mode.

### R11 -- readln-fail

`readln()` failed reading from an open file (see the accompanying `iostat`).

### R12 -- writeln-not-open

`writeln()` was called on a file handle that isn't open.

### R13 -- writeln-not-write-mode

`writeln()` was called on a file that wasn't opened in write (`"w"`) mode.

### R14 -- eof-not-open

`eof()` was called on a file handle that isn't open.

### R15 -- eof-not-read-mode

`eof()` was called on a file that wasn't opened in read (`"r"`) mode.

### R16 -- close-not-open

`close()` was called on a file handle that isn't open.

### R17 -- size-rank-mismatch

`size(array, dim)` was given a `dim` outside the array's valid rank range.

### R18 -- transpose-rank

`std::transpose()` was given an array that isn't rank-2.

### R19 -- reshape-mismatch

`std::reshape()`'s requested shape doesn't have the same element count as the source array.

### R20 -- bad-subscript-kind

An unrecognized subscript kind was encountered while evaluating a name expression.

### R21 -- array-size-mismatch

An explicitly-shaped array literal's element count doesn't match its declared size.

### R22 -- struct-array-slice

A slice subscript was used on an array of structs at runtime, which isn't implemented.

### R23 -- for-step-zero

A bytecode-VM `for` loop's integer step evaluated to 0.

### R24 -- for-step-zero-f

A bytecode-VM `for` loop's float step evaluated to 0.0.

## Warnings

### W1 -- missing-return

Not all code paths in a function return (warned instead of erroring in some compilation passes).

