## shaped

[![Build Status](https://travis-ci.org/cchalmers/shaped.svg)](https://travis-ci.org/cchalmers/shaped)
[![Hackage](https://img.shields.io/hackage/v/shaped.svg?style=flat)](https://hackage.haskell.org/package/shaped)

High level multi-dimensional arrays library. Arrays are made by adding a
shape to a vector from the vector package using indices from the linear
package. Supports mutable a immutable arrays with a similar interface to
vector but with a strong lens flavour.

### Indices

`shaped` uses `linear`'s vectors (`V2`, `V3` etc.) for array indices.

```.haskell
> a ! V3 1 2 3
```

### Parallel arrays

Contains experimental support for parallel array computation. To map
over a vector in parallel you can use the `delayed` isomorphism:

```.haskell
> a & delayed . mapped +~ 100
```

or add two arrays in parallel by delaying and then manifesting:

```.haskell
> manifest $ delay a ^+^ 3 *^ delay b
```

### Mutable

A big benefit of this library over repa is native mutable support.
Supports basic mutable arrays operations in any `PrimMonad`.

### Performance

In some rudimentary benchmarks, simple parallel operations perform
similar to repa. Performance of higher level functions (like lenses onto
planes) has not yet been optimised/benchmarked (so is probably not very
fast yet).

