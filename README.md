## dense

[![Build Status](https://travis-ci.org/cchalmers/dense.svg)](https://travis-ci.org/cchalmers/shaped)
[![Haddock](https://rawgit.com/cchalmers/dense/gh-pages/haddock.svg)](https://cchalmers.github.io/shaped/)
[![Hackage](https://img.shields.io/hackage/v/dense.svg?style=flat)](https://hackage.haskell.org/package/shaped)

`dense` is a high level multidimensional arrays library with support for
mutable arrays, stencils and parallel computation.

### Array type

Arrays are just vectors (from
[`vector`](http://hackage.haskell.org/package/vector)) with a shape:


```.haskell
data Array v f a = Array !(f Int) !(v a)
```

where `Layout f = f Int` is the shape of the array, given by a  vector
from [`linear`](http://hackage.haskell.org/package/linear) (`V1`, `V2`,
`V3` or `V4`). These vectors are also used to indexing:

```.haskell
> a ! V3 1 2 3
```

### Delayed arrays

A delayed array, defined by

```.haskell
data Delayed f a = Delayed !(Layout f) (Int -> a)
```

can be constructing from a normal array via `delay`. It can be usefull
for mapping a function over an array and computing the result in
parallel via `manifest`:

```.haskell
> manifest . fmap (+100) . delay
```

or equivilently using the `delayed` isomorphism:

```.haskell
> delayed +~ 100
```

`Delayed` is an instance of many classes, including `Additive` from
[`linear`](http://hackage.haskell.org/package/linear):

```.haskell
> manifest $ delay a ^+^ 3 *^ delay b
```

### Mutable

`shaped` has similar mutable capabilities to `vector`, supporting
mutable operations over a `PrimMonad`.

### Stencils

`dense` has good stencil support, allowing constuction of 1D, 2D or 3D
stencils using template haskell.

### Performance

In some rudimentary benchmarks, simple parallel operations perform
similar to repa. Performance of higher level functions (like lenses onto
planes) has not yet been optimised/benchmarked (so is probably not very
fast yet).

### Package structure

Like `vector`, there is a `Data.Shaped.Generic` module for working over
any generic vector as well as `Data.Shaped.Unboxed` and
`Data.Shaped.Storable` modules. Unlike `vector`, boxed vectors are in
`Data.Shaped.Boxed`.

The `Data.Shaped` module includes a subset of `Data.Shaped.Generic` as
well as some extra reexports and is intended to be imported
*unqualified*.

