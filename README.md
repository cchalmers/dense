## dense

[![Build Status](https://travis-ci.org/cchalmers/dense.svg)](https://travis-ci.org/cchalmers/dense)
[![Haddock](https://rawgit.com/cchalmers/dense/gh-pages/haddock.svg)](https://cchalmers.github.io/dense/)
[![Hackage](https://img.shields.io/hackage/v/dense.svg?style=flat)](https://hackage.haskell.org/package/dense)

[`dense`]: http://hackage.haskell.org/package/dense
[`vector`]: http://hackage.haskell.org/package/vector
[`linear`]: http://hackage.haskell.org/package/linear
[`repa`]: http://hackage.haskell.org/package/repa
[`array`]: http://hackage.haskell.org/package/array
[`yarr`]: http://hackage.haskell.org/package/yarr

[`dense`] is a multidimensional array library build on top of the
[`vector`] package, using indices from the [`linear`] package. Native
support for mutable arrays, stencils and parallel computation.

### Array type

Arrays are just vectors (from [`vector`]) with a shape:


```.haskell
data Array v f a = Array !(f Layout) !(v a)
```

where `Layout f = f Int` is the shape of the array, given by a  vector
from [`linear`] (`V1`, `V2`, `V3` or `V4`). These vectors are also used
to indexing:

```.haskell
> a ! V3 1 2 3
```

### Delayed arrays

A delayed array, defined by

```.haskell
data Delayed f a = Delayed !(Layout f) (Int -> a)
```

can be constructing from a normal array via `delay`. It can be useful
for mapping a function over an array and computing the result in
parallel via `manifest`:

```.haskell
> manifest . fmap (+100) . delay
```

or equivalently using the `delayed` isomorphism:

```.haskell
> delayed +~ 100
```

`Delayed` is an instance of many classes, including `Additive` from
[`linear`](http://hackage.haskell.org/package/linear):

```.haskell
> manifest $ delay a ^+^ 3 *^ delay b
```

### Mutable

[`dense`] has similar mutable capabilities to [`vector`], supporting
mutable operations over a `PrimMonad` in `Data.Dense.Mutable`.

### Stencils

[`dense`] has good stencil support, allowing construction of 1D, 2D or 3D
stencils using template haskell and quasiquoters.

```.haskell
myStencil = [stencil|
  2/5 8/5 2/5
  8/5  2  8/5
  2/5 8/5 2/5
|]
```

Stencils made with template haskell are unrolled at compile time.

### Comparison to other array libraries

[`array`] supports multidimensional and mutable arrays but [`dense`]
provides many more high level functions as well as stencils and parallel
computation.

[`repa`] and [`yarr`]
[`dense`] has a lot of the same features as [`repa`] and [`yarr`]. 
Performance should be similar (more benchmarks needed) but [`dense`] also
has support for mutable arrays and multidimensional stencils.

### Package structure

Like [`vector`], there is a [`Data.Shaped.Generic`] module for working
over any generic vector as well as [`Data.Shaped.Unboxed`] and
[`Data.Shaped.Storable`] modules. Unlike [`vector`], boxed vectors are
in [`Data.Shaped.Boxed`].

The [`Data.Shaped`] module includes a subset of [`Data.Shaped.Generic`]
as well as some extra reexports and is intended to be imported
*unqualified*.


[`Data.Shaped`]: https://cchalmers.github.io/dense/Data-Shaped.html
[`Data.Shaped.Boxed`]: https://cchalmers.github.io/dense/Data-Shaped-Boxed.html
[`Data.Shaped.Generic`]: https://cchalmers.github.io/dense/Data-Shaped-Generic.html
[`Data.Shaped.Storable`]: https://cchalmers.github.io/dense/Data-Shaped-Storable.html
[`Data.Shaped.Unboxed`]: https://cchalmers.github.io/dense/Data-Shaped-Unboxed.html
