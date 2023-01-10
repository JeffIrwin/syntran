
![](https://github.com/JeffIrwin/syntran/workflows/CI/badge.svg)

# Syntran

[Syntax translator](https://www.practo.com/medicine-info/syntran-100-mg-capsule-18930)

An interpreter written in Fortran, I guess

> Jerry: Fungicide. I mean what could she have?
>
> Elaine: I don't know.
>
> Kramer: Fungus.

This is a sandbox for me to play in as I follow along with [Immo Landwerth's _building a compiler_ series](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)

## Build

    ./build.sh

A [Fortran compiler](https://fortran-lang.org/en/compilers/) and [CMake](https://cmake.org/download/) are required

## Run

Start the interpreter:

    ./build/syntran

Then enter arithmetic expressions like `1 + 2 * 3;` in the interpreter.  Semicolons are required at the end of statements!

<!-- cpp is the closest match I can find for markdown syntax highlighting -->

```cpp
    syntran$ 1 + 2 * 3;
    7
    syntran$ (1 + 2) * 3;
    9
```

Expressions are evaluated immediately and the result is printed to the console.  In the rest of this documentation, we will hide the `syntran$` prompt and show the result as a `// comment`, so you can copy and paste code blocks straight into the interpreter.

Use two asterisks for exponent powers, like Fortran and Scilab:

```cpp
    5 ** 2;
    // 25
```

There's no need to [import `math.h`](https://en.cppreference.com/w/c/numeric/math/pow) and call the `pow()` function!

### Variables, booleans, and type checking

Variable declarations use the [`let` keyword](https://doc.rust-lang.org/std/keyword.let.html) as in Rust.  This is also similar to JavaScript, except there is no `var` keyword.  Variables are mutable.

Integer and boolean types are supported.

```cpp
let foo = 1;
let bar = 2;
let baz = 4;

baz = 3;

foo + bar * baz;
// 7

let p = true;
let q = false;

p or q;
// true

not q and foo + bar == baz;
// true

foo and p;
//
// foo and p
//     ^^^
// Error: binary operator "and" is not defined for types num_expr and bool_expr
// 

```

Logical keywords `true`, `false`, `not`, `and`, and `or` are like Fortran's (e.g. `.true.`) but without the dots.  Note that they are lower case-sensitive, unlike Python (e.g. `True`).

### Comments

Only single-line comments are supported.  There are _no_ multi-line `/*comments*/`.

