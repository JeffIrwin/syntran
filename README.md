
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

## Run

Start the interpreter:

    ./build/syntran

Then enter arithmetic expressions like `1 + 2 * 3` in the interpreter:

    syntran$ 1 + 2 * 3
    7
    syntran$ (1 + 2) * 3
    9

### Variables, booleans, and type checking

Integer and boolean types are supported:

    syntran$ let foo = 1
    1
    syntran$ let bar = 2
    2
    syntran$ let baz = 3
    3
    syntran$ foo + bar * baz
    7
    syntran$ let p = true
    true
    syntran$ let q = false
    false
    syntran$ p or q
    true
    syntran$ not q and foo + bar == baz
    true
    syntran$ foo and p
    
    foo and p
    Error: binary operator "and" not defined for types num_expr and bool_expr
    
    syntran$

Logical keywords `true`, `false`, `not`, `and`, and `or` are like Fortran's (e.g. `.true.`) but without the dots.  Note that they are lower case-sensitive, unlike Python (e.g. `True`).

Variable declaration syntax uses the [`let` keyword](https://doc.rust-lang.org/std/keyword.let.html) as in Rust.  This is also somewhat similar to JavaScript, except there is no `var` keyword.

