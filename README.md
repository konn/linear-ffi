# linear-ffi - Pursuing Safer Memory Management in FFI with Linear Haskell

**NOTE**: This is still a work in progress.

## Motivation

Say you are writing a binding of some C API in Haskell.
The API is complex enough that it provides custom datatypes and/or structs.
In some cases, they must be freed on the caller side, but in some cases, they are freed on the API side.
Such an invariant easily gets tedious to keep track of, resulting in the SEGV due to a double-free or early free.
Always wrapping `Ptr`s into `ForeignPtr` with fixed finalisers is not enough - it can still error-prune to early free of memory still maintained at the API side!

In this repository, we will try to solve this problem with Linear Haskell.
Some fancy wrapper for converting between structs and records is also anticipated.

## References

Tweag's Blog Posts and Linear Types papers are really helpful and always fun to read.
The following are particularly related to our goals (and, indeed, seem doing what we want already in some regard):

- "[_Safe Sparkle: A Resource-Safe Interface with Linear Types_](https://www.tweag.io/blog/2021-11-17-sparkle-internship/)", by Noah Goodman.
- "[_Safe Memory Management in `inline-java` using Linear Types_](https://www.tweag.io/blog/2020-02-06-safe-inline-java/)", by Facundo Dominguez.
- "[_Making Two Garbage Collectors be Good Neighbours (Using Linear Types)_](https://www.tweag.io/blog/2017-11-29-linear-jvm/)", by Facundo Domínguez and Mathieu Boespflug.
