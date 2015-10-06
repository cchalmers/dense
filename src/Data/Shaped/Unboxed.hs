{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Unboxed
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Unboxed shaped vectors.
-----------------------------------------------------------------------------
module Data.Shaped.Unboxed
  (
    -- * Array types
    Array
  , Shape (..)

    -- ** Extracting size
  , extent
  , size

    -- ** Lenses
  , layout
  , vector

    -- ** Traversals
  , values
  , values'

  -- * Construction

  -- ** Flat arrays
  , flat
  , fromList

  -- ** Shaped from lists
  , fromListInto
  , fromListInto_

  -- ** Shaped from vectors
  , fromVectorInto
  , fromVectorInto_

  -- ** Initialisation
  , replicate
  , generate
  , linearGenerate

  -- ** Monadic initialisation
  , create
  , replicateM
  , generateM
  , linearGenerateM

  -- * Functions on arrays

  -- ** Empty arrays
  , empty
  , null

  -- ** Indexing

  , (!)
  , (!?)
  , unsafeIndex
  , linearIndex
  , unsafeLinearIndex

  -- *** Monadic indexing
  , indexM
  , unsafeIndexM
  , linearIndexM
  , unsafeLinearIndexM

  -- ** Modifying arrays
  , (//)

  -- * Zipping
  -- ** Tuples
  , zip
  , zip3

  -- ** Zip with function
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3

  -- ** Slices

  -- *** Matrix
  , ixRow
  , rows
  , ixColumn
  , columns

  -- *** 3D
  , ixPlane
  , planes
  , flattenPlane

  -- *** Ordinals
  , unsafeOrdinals

  -- * Mutable
  , M.UMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , G.Delayed

  -- ** Generating delayed

  , delayed
  , G.delay
  , G.manifest
  , manifestS
  , G.genDelayed
  , G.indexDelayed

  -- * Focused

  , G.Focused

  -- ** Generating focused

  , G.focusOn
  , G.unfocus
  , G.unfocused

  -- ** Focus location
  , G.locale
  , G.shiftFocus

  -- * Common layouts
  , V1 (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)
  , R1 (..)
  , R2 (..)
  , R3 (..)
  , R4 (..)

  -- ** Extra planes
  , _xz
  , _yz
  , _yx
  , _zy
  , _zx
  ) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Shaped.Generic as G
import qualified Data.Shaped.Mutable as M
import           Control.Monad.Primitive
import           Data.Shaped.Index
import           Data.Vector.Unboxed (Vector, Unbox)
import Control.Lens
import Linear
import Control.Monad.ST

import           Prelude                        hiding (null, replicate, zip, zip3,
                                                 zipWith, zipWith3)

type Array = G.UArray
type MArray = G.UMArray

-- Lenses --------------------------------------------------------------

-- | Same as 'values' but restrictive in the vector type.
values :: (Shape l, Unbox a, Unbox b)
       => IndexedTraversal (l Int) (Array l a) (Array l b) a b
values = G.values'
{-# INLINE values #-}

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape l, Unbox a, Unbox b)
       => IndexedTraversal (l Int) (Array l a) (Array l b) a b
values' = G.values'
{-# INLINE values' #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in 'Data.Vector.Generic' on them without
--   needing to convert.
flat :: (Unbox a, Unbox b) => Iso (Array V1 a) (Array V1 b) (Vector a) (Vector b)
flat = G.flat
{-# INLINE flat #-}

-- Constructing vectors ------------------------------------------------

-- | Contruct a flat array from a list. (This is just 'G.fromList' from
--   'Data.Vector.Generic'.)
fromList :: Unbox a => [a] -> Array V1 a
fromList = G.fromList
{-# INLINE fromList #-}

-- | O(n) Convert the first @n@ elements of a list to an Arrayith the
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto :: (Shape l, Unbox a) => Layout l-> [a] -> Maybe (Array l a)
fromListInto = G.fromListInto
{-# INLINE fromListInto #-}

-- | O(n) Convert the first @n@ elements of a list to an Arrayith the
--   given shape. Throw an error if the list is not long enough.
fromListInto_ :: (Shape l, Unbox a) => Layout l-> [a] -> Array l a
fromListInto_ = G.fromListInto_
{-# INLINE fromListInto_ #-}

-- | Create an array from a 'vector' and a 'layout'. Return 'Nothing' if
--   the vector is not the right shape.
fromVectorInto :: (Shape l, Unbox a) => Layout l-> Vector a -> Maybe (Array l a)
fromVectorInto = G.fromVectorInto
{-# INLINE fromVectorInto #-}

-- | Create an array from a 'vector' and a 'layout'. Throws an error if
--   the vector is not the right shape.
fromVectorInto_ :: (Shape l, Unbox a) => Layout l-> Vector a -> Array l a
fromVectorInto_ = G.fromVectorInto_
{-# INLINE fromVectorInto_ #-}

-- | The empty 'Array' with a 'zero' shape.
empty :: (Unbox a, Additive l) => Array l a
empty = G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: Foldable l => Array l a -> Bool
null = G.null
{-# INLINE null #-}

-- Indexing ------------------------------------------------------------

-- | Index an element of an array. Throws 'IndexOutOfBounds' if the
--   index is out of bounds.
(!) :: (Shape l, Unbox a) => Array l a -> l Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

-- | Safe index of an element.
(!?) :: (Shape l, Unbox a) => Array l a -> l Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

-- | Index an element of an array without bounds checking.
unsafeIndex :: (Shape l, Unbox a) => l Int -> Array l a -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

-- | Index an element of an array while ignoring its shape.
linearIndex :: Unbox a => Int -> Array l a -> a
linearIndex = G.linearIndex
{-# INLINE linearIndex #-}

-- | Index an element of an array while ignoring its shape, without
--   bounds checking.
unsafeLinearIndex :: Unbox a => Int -> Array l a -> a
unsafeLinearIndex = G.unsafeLinearIndex
{-# INLINE unsafeLinearIndex #-}

-- Monadic indexing ----------------------------------------------------

-- | /O(1)/ Indexing in a monad.
--
--   The monad allows operations to be strict in the vector when necessary.
--   Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
--   For lazy vectors, @v ! i@ would not be evaluated which means that
--   @mv@ would unnecessarily retain a reference to @v@ in each element
--   written.
--
--   With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >   x <- indexM v i
-- >   write mv i x
--
--   Here, no references to @v@ are retained because indexing (but /not/
--   the elements) is evaluated eagerly.
--
--   Throws an error if the index is out of range.
indexM :: (Shape l, Unbox a, Monad m) => Array l a -> l Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeIndexM :: (Shape l, Unbox a, Monad m) => Array l a -> l Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ Indexing in a monad. Throws an error if the index is out of
--   range.
linearIndexM :: (Shape l, Unbox a, Monad m) => Array l a -> Int -> m a
linearIndexM = G.linearIndexM
{-# INLINE linearIndexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeLinearIndexM :: (Unbox a, Monad m) => Array l a -> Int -> m a
unsafeLinearIndexM = G.unsafeLinearIndexM
{-# INLINE unsafeLinearIndexM #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: (Unbox a, Shape l)
       => (forall s. ST s (M.MArray UV.MVector l s a)) -> Array l a
create m = m `seq` runST (m >>= G.unsafeFreeze)
{-# INLINE create #-}

-- | O(n) Array of the given shape with the same value in each position.
replicate :: (Shape l, Unbox a) => l Int -> a -> Array l a
replicate = G.replicate
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
linearGenerate :: (Shape l, Unbox a) => Layout l-> (Int -> a) -> Array l a
linearGenerate = G.linearGenerate
{-# INLINE linearGenerate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: (Shape l, Unbox a) => Layout l-> (l Int -> a) -> Array l a
generate = G.generate
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape l, Unbox a) => Layout l -> m a -> m (Array l a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape l, Unbox a) => Layout l-> (l Int -> m a) -> m (Array l a)
generateM = G.generateM
{-# INLINE generateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
linearGenerateM :: (Monad m, Shape l, Unbox a) => Layout l-> (Int -> m a) -> m (Array l a)
linearGenerateM = G.linearGenerateM
{-# INLINE linearGenerateM #-}

-- | For each pair (i,a) from the list, replace the array element at
--   position i by a.
(//) :: (Unbox a, Shape l) => Array l a -> [(l Int, a)] -> Array l a
(//) = (G.//)

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- Tuple zip -----------------------------------------------------------

-- | Zip two arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip :: (Shape l, Unbox a, Unbox b)
    => Array l a
    -> Array l b
    -> Array l (a,b)
zip = G.zip

-- | Zip three arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip3 :: (Shape l, Unbox a, Unbox b, Unbox c)
     => Array l a
     -> Array l b
     -> Array l c
     -> Array l (a,b,c)
zip3 = G.zip3

-- Zip with function ---------------------------------------------------

-- | Zip two arrays using the given function. If the array's don't have
--   the same shape, the new array with be the intersection of the two
--   shapes.
zipWith :: (Shape l, Unbox a, Unbox b, Unbox c)
        => (a -> b -> c)
        -> Array l a
        -> Array l b
        -> Array l c
zipWith = G.zipWith
{-# INLINE zipWith #-}

-- | Zip three arrays using the given function. If the array's don't
--   have the same shape, the new array with be the intersection of the
--   two shapes.
zipWith3 :: (Shape l, Unbox a, Unbox b, Unbox c, Unbox d)
         => (a -> b -> c -> d)
         -> Array l a
         -> Array l b
         -> Array l c
         -> Array l d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

-- Indexed zipping -----------------------------------------------------

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith :: (Shape l, Unbox a, Unbox b, Unbox c)
         => (l Int -> a -> b -> c)
         -> Array l a
         -> Array l b
         -> Array l c
izipWith = G.izipWith
{-# INLINE izipWith #-}

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith3 :: (Shape l, Unbox a, Unbox b, Unbox c, Unbox d)
          => (l Int -> a -> b -> c -> d)
          -> Array l a
          -> Array l b
          -> Array l c
          -> Array l d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

------------------------------------------------------------------------
-- Slices
------------------------------------------------------------------------

-- $setup
-- >>> import Debug.SimpleReflect
-- >>> let m = fromListInto_ (V2 3 4) [a,b,c,d,e,f,g,h,i,j,k,l] :: BArray V2 Expr

-- | Indexed traversal over the rows of a matrix. Each row is an
--   efficient 'Data.Vector.Generic.slice' of the original vector.
--
-- >>> traverseOf_ rows print m
-- [a,b,c,d]
-- [e,f,g,h]
-- [i,j,k,l]
rows :: (Unbox a, Unbox b)
     => IndexedTraversal Int (Array V2 a) (Array V2 b) (Vector a) (Vector b)
rows = G.rows
{-# INLINE rows #-}

-- | Affine traversal over a single row in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixRow 1 . each *~ 2
-- [a,b,c,d]
-- [e * 2,f * 2,g * 2,h * 2]
-- [i,j,k,l]
--
--   The row vector should remain the same size to satisfy traversal
--   laws but give reasonable behaviour if the size differs:
--
-- >>> traverseOf_ rows print $ m & ixRow 1 .~ B.fromList [0,1]
-- [a,b,c,d]
-- [0,1,g,h]
-- [i,j,k,l]
--
-- >>> traverseOf_ rows print $ m & ixRow 1 .~ B.fromList [0..100]
-- [a,b,c,d]
-- [0,1,2,3]
-- [i,j,k,l]
ixRow :: Unbox a => Int -> IndexedTraversal' Int (Array V2 a) (Vector a)
ixRow = G.ixRow
{-# INLINE ixRow #-}

-- | Indexed traversal over the columns of a matrix. Unlike 'rows', each
--   column is a new separate vector.
--
-- >>> traverseOf_ columns print m
-- [a,e,i]
-- [b,f,j]
-- [c,g,k]
-- [d,h,l]
--
-- >>> traverseOf_ rows print $ m & columns . indices odd . each .~ 0
-- [a,0,c,0]
-- [e,0,g,0]
-- [i,0,k,0]
--
--   The vectors should be the same size to be a valid traversal. If the
--   vectors are different sizes, the number of rows in the new array
--   will be the length of the smallest vector.
columns :: (Unbox a, Unbox b)
        => IndexedTraversal Int (Array V2 a) (Array V2 b) (Vector a) (Vector b)
columns = G.columns
{-# INLINE columns #-}

-- | Affine traversal over a single column in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixColumn 2 . each +~ 1
-- [a,b,c + 1,d]
-- [e,f,g + 1,h]
-- [i,j,k + 1,l]
ixColumn :: Unbox a => Int -> IndexedTraversal' Int (Array V2 a) (Vector a)
ixColumn = G.ixColumn
{-# INLINE ixColumn #-}

-- | Traversal over a single plane of a 3D array given a lens onto that
--   plane (like '_xy', '_yz', '_zx').
ixPlane :: Unbox a
        => ALens' (V3 Int) (V2 Int)
        -> Int
        -> IndexedTraversal' Int (Array V3 a) (Array V2 a)
ixPlane = G.ixPlane
{-# INLINE ixPlane #-}

-- | Traversal over all planes of 3D array given a lens onto that plane
--   (like '_xy', '_yz', '_zx').
planes :: (Unbox a, Unbox b)
       => ALens' (V3 Int) (V2 Int)
       -> IndexedTraversal Int (Array V3 a) (Array V3 b) (Array V2 a) (Array V2 b)
planes = G.planes
{-# INLINE planes #-}

-- | Flatten a plane by reducing a vector in the third dimension to a
--   single value.
flattenPlane :: (Unbox a, Unbox b)
             => ALens' (V3 Int) (V2 Int)
             -> (Vector a -> b)
             -> Array V3 a
             -> Array V2 b
flattenPlane = G.flattenPlane
{-# INLINE flattenPlane #-}

-- Ordinals ------------------------------------------------------------

-- | This 'Traversal' should not have any duplicates in the list of
--   indices.
unsafeOrdinals :: (Unbox a, Shape l) => [l Int] -> IndexedTraversal' (l Int) (Array l a) a
unsafeOrdinals = G.unsafeOrdinals
{-# INLINE [0] unsafeOrdinals #-}

-- Mutable -------------------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape l, Unbox a)
       => MArray l (PrimState m) a -> m (Array l a)
freeze = G.freeze
{-# INLINE freeze #-}

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape l, Unbox a)
     => Array l a -> m (MArray l (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Shape l, Unbox a)
             => MArray l (PrimState m) a -> m (Array l a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Shape l, Unbox a)
           => Array l a -> m (MArray l (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}


------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | Isomorphism between an array and it's delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Unbox a, Unbox b, Shape l, Shape k)
        => Iso (Array l a) (Array k b) (G.Delayed l a) (G.Delayed k b)
delayed = G.delayed
{-# INLINE delayed #-}

-- | Sequential manifestation of a delayed array.
manifestS :: (Unbox a, Shape l) => G.Delayed l a -> Array l a
manifestS = G.manifestS
{-# INLINE manifestS #-}

