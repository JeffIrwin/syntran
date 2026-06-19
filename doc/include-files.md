# Include files

> **Note:** Include files are still supported but have been superseded by the
> [module system](../README.md#modules) (`use` statements), which is the
> recommended way to share code across syntran files.

Like the C language, the contents of one source file can be included in a higher-level source using the `#include` preprocessing directive.

Let's say you have a file named `header.syntran` which defines a global variable and a string helper function:

```rust
// header.syntran

let my_global = 42;

fn my_scan(str_: str, set: str): i32
{
    // Return the first substring index i of `str_` which matches any character
    // from the string `set`.
    //
    // c.f. Fortran intrinsic scan()

    let found = false;
    let i = 0;
    while not found and i < len(str_)
    {
        let j = 0;
        while not found and j < len(set)
        {
            found = str_[i] == set[j];
            j += 1;
        }
        i += 1;
    }

    if (found)
        i -= 1;
    else
        i = -1;
    return i;
}
```

This can be included in a main program, assuming `main.syntran` and `header.syntran` are in the same folder:

```rust
// main.syntran

#include("header.syntran");

fn main()
{
    println(my_global);
    // 42

    println(my_scan("012345", "2"));
    // 2

    println(my_scan("012345", "3"));
    // 3

    return;
}

main();
```

As in C, the effect of `#include` is as if the contents of the header are pasted into the *including* file.  The difference is that any error messages will still reference the correct line number and filename where they occur.

If included files are in a separate folder, the included filename is a path relative to the parent including file.

Note that the syntax of the `#include` directive looks like a function call, i.e. it has parentheses around the filename argument and a semicolon at the end.  This gives syntran a consistent feel throughout its features, unlike C.  However, be careful noting that `#include` is *not* a real function!  It is a preprocessing directive, indicated by the hash `#`, so the full evaluation facilities of syntran are not available at the time that the directive is processed.

For example, it is *not* possible to concatenate strings together into the include filename:

```rust
#include("header." + "syntran");
// Error: #include file `"header."` not found
//   --> main.syntran:3:10
//    |
//  3 | #include("header." + "syntran");
//    |          ^^^^^^^^^ file not found
//
// Error: unexpected token `+` of kind `plus_token`, expected `rparen_token`
//   --> main.syntran:3:20
//    |
//  3 | #include("header." + "syntran");
//    |                    ^ unexpected token
// ...
```

The argument must be a single static lex-time constant string.
