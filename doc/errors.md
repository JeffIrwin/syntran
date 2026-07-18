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

[Example](../src/tests/test-src/errors/E1-bad-i32.syntran)

### E2 -- bad-i64

Integer literal with an explicit `'i64` suffix does not fit in 64 bits.

[Example](../src/tests/test-src/errors/E2-bad-i64.syntran)

### E3 -- bad-hex32

Hexadecimal integer literal does not fit in 32 bits.

[Example](../src/tests/test-src/errors/E3-bad-hex32.syntran)

### E4 -- bad-hex64

Hexadecimal integer literal does not fit in 64 bits.

[Example](../src/tests/test-src/errors/E4-bad-hex64.syntran)

### E5 -- bad-oct32

Octal integer literal does not fit in 32 bits.

[Example](../src/tests/test-src/errors/E5-bad-oct32.syntran)

### E6 -- bad-oct64

Octal integer literal does not fit in 64 bits.

[Example](../src/tests/test-src/errors/E6-bad-oct64.syntran)

### E7 -- bad-bin32

Binary integer literal does not fit in 32 bits.

[Example](../src/tests/test-src/errors/E7-bad-bin32.syntran)

### E8 -- bad-bin64

Binary integer literal does not fit in 64 bits.

[Example](../src/tests/test-src/errors/E8-bad-bin64.syntran)

### E9 -- bad-expr

A bare expression statement was used outside the REPL.  Use `println()` to log its value.

[Example](../src/tests/test-src/errors/E9-bad-expr.syntran)

### E10 -- unterminated-str

A string literal is missing its closing quote.

[Example](../src/tests/test-src/errors/E10-unterminated-str.syntran)

### E11 -- unterminated-raw-str

A raw string literal is missing its closing delimiter.

[Example](../src/tests/test-src/errors/E11-unterminated-raw-str.syntran)

### E12 -- array-struct-slice

Slices are not implemented for arrays of structs.

[Example](../src/tests/test-src/errors/E12-array-struct-slice.syntran)

### E13 -- struct-array-slice

Slices are not implemented for structs of arrays; only scalar subscripts are allowed there.

[Example](../src/tests/test-src/errors/E13-struct-array-slice.syntran)

### E14 -- non-int-subscript

An array subscript expression is not an integer.

[Example](../src/tests/test-src/errors/E14-non-int-subscript.syntran)

### E15 -- bad-f32

Real literal with an explicit `'f32` suffix could not be parsed.

[Example](../src/tests/test-src/errors/E15-bad-f32.syntran)

### E16 -- bad-f64

Real literal with an explicit `'f64` suffix could not be parsed.

[Example](../src/tests/test-src/errors/E16-bad-f64.syntran)

### E17 -- bad-type

An unrecognized type name was used in a type annotation.

[Example](../src/tests/test-src/errors/E17-bad-type.syntran)

### E18 -- bad-type-suffix

An unrecognized literal type suffix (e.g. `'i32`) was used after a number or string literal.

[Example](../src/tests/test-src/errors/E18-bad-type-suffix.syntran)

### E19 -- unexpected-char

The lexer encountered a character it doesn't recognize.

[Example](../src/tests/test-src/errors/E19-unexpected-char.syntran)

### E20 -- unexpected-token

The parser encountered a token it didn't expect at this position.

[Example](../src/tests/test-src/errors/E20-unexpected-token.syntran)

### E21 -- void-assign

A variable was initialized from an expression of void type.

[Example](../src/tests/test-src/errors/E21-void-assign.syntran)

### E22 -- redeclare-var

A variable was declared twice in the same scope.

[Example](../src/tests/test-src/errors/E22-redeclare-var.syntran)

### E23 -- redeclare-mem

A struct member was declared twice in the same struct.

[Example](../src/tests/test-src/errors/E23-redeclare-mem.syntran)

### E24 -- redeclare-fn

A function was declared twice.

[Example](../src/tests/test-src/errors/E24-redeclare-fn.syntran)

### E25 -- redeclare-intr-fn

A user function's name collides with a built-in (intrinsic) function.

[Example](../src/tests/test-src/errors/E25-redeclare-intr-fn.syntran)

### E26 -- redeclare-struct

A struct was declared twice.

[Example](../src/tests/test-src/errors/E26-redeclare-struct.syntran)

### E27 -- redeclare-primitive

A struct was declared with a name reserved for a primitive type (e.g. `i32`).

[Example](../src/tests/test-src/errors/E27-redeclare-primitive.syntran)

### E28 -- undeclare-var

A variable was referenced before being declared in scope.  May include a "did you mean" suggestion.

[Example](../src/tests/test-src/errors/E28-undeclare-var.syntran)

### E29 -- undeclare-fn

A function was called before being defined.  May include a "did you mean" suggestion.

[Example](../src/tests/test-src/errors/E29-undeclare-fn.syntran)

### E30 -- std-only-fn

A standard-library function was called without its required `std::` prefix.

[Example](../src/tests/test-src/errors/E30-std-only-fn.syntran)

### E31 -- no-return

A non-void function has no `return` statements at all.

[Example](../src/tests/test-src/errors/E31-no-return.syntran)

### E32 -- missing-return

Not all code paths in a non-void function end in a `return` statement.

[Example](../src/tests/test-src/errors/E32-missing-return.syntran)

### E33 -- bad-arg-count

A function call was given the wrong number of arguments.

[Example](../src/tests/test-src/errors/E33-bad-arg-count.syntran)

### E34 -- too-few-args

A variadic function call was given fewer than its minimum number of arguments.

[Example](../src/tests/test-src/errors/E34-too-few-args.syntran)

### E35 -- too-many-args

A variadic function call was given more than its maximum number of arguments.

[Example](../src/tests/test-src/errors/E35-too-many-args.syntran)

### E36 -- bad-sub-count

An array was subscripted with the wrong number of indices for its rank.

[Example](../src/tests/test-src/errors/E36-bad-sub-count.syntran)

### E37 -- bad-sub-rank

An array used as a subscript index is not rank-1.

[Example](../src/tests/test-src/errors/E37-bad-sub-rank.syntran)

### E38 -- empty-step

A slice step was omitted between two colons (e.g. `a[1::4]`); write it explicitly.

[Example](../src/tests/test-src/errors/E38-empty-step.syntran)

### E39 -- scalar-subscript

A scalar variable was subscripted as if it were an array.

[Example](../src/tests/test-src/errors/E39-scalar-subscript.syntran)

### E40 -- bad-cat-rank

A concatenated array operand is not rank-1.

[Example](../src/tests/test-src/errors/E40-bad-cat-rank.syntran)

### E41 -- bad-ret-type

A function's `return` value does not match its declared scalar return type.

[Example](../src/tests/test-src/errors/E41-bad-ret-type.syntran)

### E42 -- bad-arg-type

A function argument's type does not match the corresponding parameter's declared type.

[Example](../src/tests/test-src/errors/E42-bad-arg-type.syntran)

### E43 -- bad-arg-val

A `&` reference parameter was given a plain value argument instead of a reference.

[Example](../src/tests/test-src/errors/E43-bad-arg-val.syntran)

### E44 -- bad-arg-ref

A by-value parameter was given a `&` reference argument instead of a value.

[Example](../src/tests/test-src/errors/E44-bad-arg-ref.syntran)

### E45 -- non-name-ref

A `&` reference was taken of an expression that isn't a plain variable name.

[Example](../src/tests/test-src/errors/E45-non-name-ref.syntran)

### E46 -- sub-ref

A `&` reference was taken of a subscripted expression, which isn't allowed.

[Example](../src/tests/test-src/errors/E46-sub-ref.syntran)

### E47 -- bad-arg-rank (retired)

Never shipped a working constructor and is no longer reachable.  The code is
kept reserved per the permanence policy above; do not reuse it.

### E48 -- binary-types

A binary operator (e.g. `+`) is not defined for the given pair of operand types.

[Example](../src/tests/test-src/errors/E48-binary-types.syntran)

### E49 -- binary-ranks

A binary operator's array operands have mismatched ranks.

[Example](../src/tests/test-src/errors/E49-binary-ranks.syntran)

### E50 -- unary-types

A unary operator (e.g. unary `-`) is not defined for the given operand type.

[Example](../src/tests/test-src/errors/E50-unary-types.syntran)

### E51 -- non-array-loop

The range expression of a `for` loop is not an array.

[Example](../src/tests/test-src/errors/E51-non-array-loop.syntran)

### E52 -- non-bool-condition

The condition of an `if`/`while`/etc. statement is not a `bool`.

[Example](../src/tests/test-src/errors/E52-non-bool-condition.syntran)

### E53 -- non-float-len-range

A length-based array range bound (`[a; n]`) is not a float.

[Example](../src/tests/test-src/errors/E53-non-float-len-range.syntran)

### E54 -- non-int-len

An array length expression is not an integer.

[Example](../src/tests/test-src/errors/E54-non-int-len.syntran)

### E55 -- bound-type-mismatch

The lower and upper bounds of an array range have mismatched types.

[Example](../src/tests/test-src/errors/E55-bound-type-mismatch.syntran)

### E56 -- non-num-range

An array range bound is not a numeric type.

[Example](../src/tests/test-src/errors/E56-non-num-range.syntran)

### E57 -- non-sca-val

A value used to fill a uniform array (`[v; n]`) is not a scalar.

[Example](../src/tests/test-src/errors/E57-non-sca-val.syntran)

### E58 -- non-int-range

An array range bound is not an `i32` integer.

[Example](../src/tests/test-src/errors/E58-non-int-range.syntran)

### E59 -- het-array

An array literal is heterogeneous: an element's type doesn't match the first element's type.

[Example](../src/tests/test-src/errors/E59-het-array.syntran)

### E60 -- unset-member

A struct instance was constructed without initializing all of its members.

[Example](../src/tests/test-src/errors/E60-unset-member.syntran)

### E61 -- reset-member

A struct instance initializer set the same member twice.

[Example](../src/tests/test-src/errors/E61-reset-member.syntran)

### E62 -- non-struct-dot

Dot member access (`.`) was used on a variable that isn't a struct.

[Example](../src/tests/test-src/errors/E62-non-struct-dot.syntran)

### E63 -- bad-member-name

A dot expression referenced a member name that doesn't exist on the struct (variable and type both named in the message).

[Example](../src/tests/test-src/errors/E63-bad-member-name.syntran)

### E64 -- bad-member-name-short

A struct instantiation (`Type{...}`) referenced a member name that doesn't exist on the struct.

[Example](../src/tests/test-src/errors/E64-bad-member-name-short.syntran)

### E65 -- bad-member-type

A struct member was initialized with a value of the wrong type.

[Example](../src/tests/test-src/errors/E65-bad-member-type.syntran)

### E66 -- inc-404

An `#include` file could not be found.

[Example](../src/tests/test-src/errors/E66-inc-404.syntran)

### E67 -- inc-read

An `#include` file was found but could not be read.

[Example](../src/tests/test-src/errors/E67-inc-read.syntran)

### E68 -- mod-404

A `use`d module file could not be found.

[Example](../src/tests/test-src/errors/E68-mod-404.syntran)

### E69 -- mod-read

A `use`d module file was found but could not be read.

[Example](../src/tests/test-src/errors/E69-mod-read.syntran)

### E70 -- circular-import

Two or more modules import each other, forming a circular dependency.

[Example](../src/tests/test-src/errors/E70-circular-import.syntran) (the cycle itself is in the fixture pair [circular_a.syntran](../src/tests/test-src/errors/circular_a.syntran) / [circular_b.syntran](../src/tests/test-src/errors/circular_b.syntran))

### E71 -- duplicate-import

The same module was imported more than once.

[Example](../src/tests/test-src/errors/E71-duplicate-import.syntran)

### E72 -- mod-hyphen

A module name contains a hyphen; use underscores instead.

[Example](../src/tests/test-src/errors/E72-mod-hyphen.syntran)

### E73 -- mod-keyword

A module name is a reserved language keyword.

[Example](../src/tests/test-src/errors/E73-mod-keyword.syntran)

### E74 -- mod-reserved-std

The module name `std` is reserved for the standard library.

[Example](../src/tests/test-src/errors/E74-mod-reserved-std.syntran)

### E75 -- mod-space

A module name contains a space.

[Example](../src/tests/test-src/errors/E75-mod-space.syntran)

### E76 -- alias-keyword

A `use ... as <alias>` alias is a reserved language keyword.

[Example](../src/tests/test-src/errors/E76-alias-keyword.syntran)

### E77 -- alias-reserved-std

The module alias `std` is reserved and cannot be reused.

[Example](../src/tests/test-src/errors/E77-alias-reserved-std.syntran)

### E78 -- alias-hyphen

A module alias contains a hyphen.

[Example](../src/tests/test-src/errors/E78-alias-hyphen.syntran)

### E79 -- alias-space

A module alias contains a space.

[Example](../src/tests/test-src/errors/E79-alias-space.syntran)

### E80 -- alias-with-doublecolon

A `use module::*` glob import was combined with an alias, which isn't allowed.

[Example](../src/tests/test-src/errors/E80-alias-with-doublecolon.syntran)

### E81 -- 404

The top-level source file given to the interpreter could not be found.

### E82 -- immutable-var

Assignment to a `std::` constant (e.g. `std::PI`), which is not allowed.

[Example](../src/tests/test-src/errors/E82-immutable-var.syntran)

### E83 -- const-assign

Assignment to a variable declared `const`.

[Example](../src/tests/test-src/errors/E83-const-assign.syntran)

### E84 -- mutable-method-on-temp

A mutable (non-`const`) method was called on a temporary struct value (e.g. a function's return value), so any mutation would be silently discarded.

[Example](../src/tests/test-src/errors/E84-mutable-method-on-temp.syntran)

### E85 -- member-method-clash

A struct method has the same name as one of the struct's members. Names must be unique across a struct's fields and methods so that `s.foo` is unambiguous.

[Example](../src/tests/test-src/errors/E85-member-method-clash.syntran)

### E86 -- module-return

`return` is not allowed at the top level of an imported module. A top-level `return` in a main-program script is still allowed and sets the program's result value.

[Example](../src/tests/test-src/errors/E86-module-return.syntran)

### E87 -- fn-ptr-unsupported

A function pointer (`fn(...)`-typed value) cannot be taken to an intrinsic function, a struct method, or a user-defined function with any `&`-reference parameter. A fn-pointer signature has no way to express reference-ness, so allowing this would silently drop reference semantics on an indirect call.

[Example](../src/tests/test-src/errors/E87-fn-ptr-unsupported.syntran)

### E88 -- not-callable

A variable that is not a fn-pointer value (`fn(...)` type) was called like a function, e.g. `x(1)` where `x` is an `i32`.

[Example](../src/tests/test-src/errors/E88-not-callable.syntran)

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

### I8 -- for-step-zero (retired)

Used to fire when a `for` loop's integer step (`a:step:b`) evaluated to 0 in
the AST walker. Reachable from ordinary syntran code (a runtime-valued step),
so it was replaced with runtime error R23 below. The code is kept reserved
per the permanence policy above; do not reuse it.

### I9 -- for-step-zero-f (retired)

Used to fire when a `for` loop's float step (`a:step:b`) evaluated to 0.0 in
the AST walker. Replaced with runtime error R24 below for the same reason as
I8; kept reserved, do not reuse.

### I10 -- step-array-type

A `for` loop's step-range has an unsupported runtime type (AST and VM paths).

### I11 -- bound-len-array-type

A `for` loop's length-based range has an unsupported runtime type (AST and VM paths).

### I12 -- for-array-kind

A `for` loop's range array has an unrecognized/unimplemented array kind (AST and VM paths).

### I13 -- str-char-subscript

An unexpected subscript kind was used while indexing the characters of a string.

### I14 -- array-step-0 (retired)

Used to fire when an array range literal's (non-loop) integer step evaluated
to 0. Reachable from ordinary syntran code, so it was replaced with runtime
error R25 below. Kept reserved per the permanence policy above; do not reuse.

### I15 -- array-step-0-f (retired)

Used to fire when an array range literal's (non-loop) float step evaluated to
0.0. Replaced with runtime error R26 below for the same reason as I14; kept
reserved, do not reuse.

### I16 -- unexpected-array-kind

An array value has an unrecognized internal `array%kind`.

### I17 -- alloc-array-type

`allocate_array()` was asked to allocate a buffer for an unsupported element type.

### I18 -- array-type-not-impl

`new_array()` was asked to create a buffer for an unsupported element type.

### I19 -- unexpected-assign-op

A compound-assignment operator (e.g. `+=`) token was not recognized.

### I20 -- subscript-step-0 (retired)

Used to fire when an array slice subscript's step (`a[::s]`) evaluated to 0.
Reachable from ordinary syntran code, so it was replaced with runtime error
R27 below. Kept reserved per the permanence policy above; do not reuse.

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

[Example](../src/tests/test-src/errors/R1-matmul-dim.syntran)

### R2 -- parse-i32

`parse_i32()` could not parse its string argument as an `i32`.

[Example](../src/tests/test-src/errors/R2-parse-i32.syntran)

### R3 -- parse-i64

`parse_i64()` could not parse its string argument as an `i64`.

[Example](../src/tests/test-src/errors/R3-parse-i64.syntran)

### R4 -- parse-f32

`parse_f32()` could not parse its string argument as an `f32`.

[Example](../src/tests/test-src/errors/R4-parse-f32.syntran)

### R5 -- parse-f64

`parse_f64()` could not parse its string argument as an `f64`.

[Example](../src/tests/test-src/errors/R5-parse-f64.syntran)

### R6 -- bad-file-mode

`open()` was given a file-mode character other than `r` or `w`.

[Example](../src/tests/test-src/errors/R6-bad-file-mode.syntran)

### R7 -- file-rw-mode

`open()` was given a mode string combining `r` and `w`, which isn't supported.

[Example](../src/tests/test-src/errors/R7-file-rw-mode.syntran)

### R8 -- open-file

`open()` failed to open the requested file (see the accompanying `iostat`).

[Example](../src/tests/test-src/errors/R8-open-file.syntran)

### R9 -- readln-not-open

`readln()` was called on a file handle that isn't open.

[Example](../src/tests/test-src/errors/R9-readln-not-open.syntran)

### R10 -- readln-not-read-mode

`readln()` was called on a file that wasn't opened in read (`"r"`) mode.

[Example](../src/tests/test-src/errors/R10-readln-not-read-mode.syntran)

### R11 -- readln-fail

`readln()` failed reading from an open file (see the accompanying `iostat`).

[Example](../src/tests/test-src/errors/R11-readln-fail.syntran)

### R12 -- writeln-not-open

`writeln()` was called on a file handle that isn't open.

[Example](../src/tests/test-src/errors/R12-writeln-not-open.syntran)

### R13 -- writeln-not-write-mode

`writeln()` was called on a file that wasn't opened in write (`"w"`) mode.

[Example](../src/tests/test-src/errors/R13-writeln-not-write-mode.syntran)

### R14 -- eof-not-open

`eof()` was called on a file handle that isn't open.

[Example](../src/tests/test-src/errors/R14-eof-not-open.syntran)

### R15 -- eof-not-read-mode

`eof()` was called on a file that wasn't opened in read (`"r"`) mode.

[Example](../src/tests/test-src/errors/R15-eof-not-read-mode.syntran)

### R16 -- close-not-open

`close()` was called on a file handle that isn't open.

[Example](../src/tests/test-src/errors/R16-close-not-open.syntran)

### R17 -- size-rank-mismatch

`size(array, dim)` was given a `dim` outside the array's valid rank range.

[Example](../src/tests/test-src/errors/R17-size-rank-mismatch.syntran)

### R18 -- transpose-rank

`std::transpose()` was given an array that isn't rank-2.

### R19 -- reshape-mismatch

`std::reshape()`'s requested shape doesn't have the same element count as the source array.

[Example](../src/tests/test-src/errors/R19-reshape-mismatch.syntran)

### R20 -- bad-subscript-kind

An unrecognized subscript kind was encountered while evaluating a name expression.

### R21 -- array-size-mismatch

An explicitly-shaped array literal's element count doesn't match its declared size.

### R22 -- struct-array-slice

A slice subscript was used on an array of structs at runtime, which isn't implemented.

### R23 -- for-step-zero

A `for` loop's integer step (`a:step:b`) evaluated to 0.

[Example](../src/tests/test-src/errors/R23-for-step-zero.syntran)

### R24 -- for-step-zero-f

A `for` loop's float step (`a:step:b`) evaluated to 0.0.

[Example](../src/tests/test-src/errors/R24-for-step-zero-f.syntran)

### R25 -- array-step-zero

An array range literal's (non-loop) integer step (`a:step:b`) evaluated to 0.

[Example](../src/tests/test-src/errors/R25-array-step-zero.syntran)

### R26 -- array-step-zero-f

An array range literal's (non-loop) float step (`a:step:b`) evaluated to 0.0.

[Example](../src/tests/test-src/errors/R26-array-step-zero-f.syntran)

### R27 -- subscript-step-zero

An array slice subscript's step (`a[::s]`) evaluated to 0.

[Example](../src/tests/test-src/errors/R27-subscript-step-zero.syntran)

### R28 -- close-standard

`close()` was called on a standard file handle (`std::IN`, `std::OUT`, or `std::ERR`), which isn't allowed.

[Example](../src/tests/test-src/errors/R28-close-standard.syntran)

### R29 -- getenv-unset

`std::getenv()` was given a name that isn't set in the environment.

[Example](../src/tests/test-src/errors/R29-getenv-unset.syntran)

### R30 -- writeln-fail

`writeln()` failed writing to an open file (see the accompanying `iostat`).

### R31 -- close-fail

`close()` failed to close an open file (see the accompanying `iostat`).

## Warnings

### W1 -- missing-return

Not all code paths in a function return (warned instead of erroring in some compilation passes).

