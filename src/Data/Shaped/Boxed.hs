{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Boxed
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Boxed shaped vectors.
-----------------------------------------------------------------------------
module Data.Shaped.Boxed
  (
    -- * BArray types
    BArray
  , Shape

    -- * Layout of an array
  , HasLayout (..)
  , Layout

    -- ** Extracting size
  , extent
  , size

    -- ** Folds over indexes
  , indexes
  , indexesFrom
  , indexesBetween

    -- * Underlying vector
  , vector

    -- ** Traversals
  , values
  , values'
  , valuesBetween

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

  -- ** Bulk updates
  , (//)

  -- ** Accumulations
  , accum

  -- ** Mapping
  , map
  , imap

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
  , BMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , G.Delayed

  -- ** Generating delayed

  , delayed
  , delay
  , manifest
  , seqManifest
  , G.genDelayed
  , G.indexDelayed
  , affirm
  , seqAffirm

  -- * Focused

  , G.Focused

  -- ** Generating focused

  , G.focusOn
  , G.unfocus
  , G.unfocused
  , G.extendFocus

  -- ** Focus location
  , G.locale
  , G.shiftFocus

  ) where

import           Control.Lens            hiding (imap)
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Foldable           as F
import           Data.Vector             (Vector)
import           Linear                  hiding (vector)

import           Prelude                 hiding (map, null, replicate, zip,
                                          zip3, zipWith, zipWith3)

import           Data.Shaped.Generic     (BArray)
import qualified Data.Shaped.Generic     as G
import           Data.Shaped.Index
import           Data.Shaped.Mutable     (BMArray)

-- Lenses --------------------------------------------------------------

-- | Same as 'values' but restrictive in the vector type.
values :: Shape l
       => IndexedTraversal (l Int) (BArray l a) (BArray l b) a b
values = G.values'
{-# INLINE values #-}

-- | Same as 'values' but restrictive in the vector type.
values' :: Shape l
       => IndexedTraversal (l Int) (BArray l a) (BArray l b) a b
values' = G.values'
{-# INLINE values' #-}

-- | Same as 'values' but restrictive in the vector type.
valuesBetween
  :: Shape l
  => l Int
  -> l Int
  -> IndexedTraversal' (l Int) (BArray l a) a
valuesBetween = G.valuesBetween
{-# INLINE valuesBetween #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in 'Data.Vector.Generic' on them without
--   needing to convert.
flat :: Iso (BArray V1 a) (BArray V1 b) (Vector a) (Vector b)
flat = G.flat
{-# INLINE flat #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must _not_ change the length of the
--   vector, otherwise an error will be thrown (even for 'V1' layouts,
--   use 'flat' for 'V1').
vector :: IndexedLens (Layout l) (BArray l a) (BArray l b) (Vector a) (Vector b)
vector = G.vector
{-# INLINE vector #-}

-- Constructing vectors ------------------------------------------------

-- | Contruct a flat array from a list. (This is just 'G.fromList' from
--   'Data.Vector.Generic'.)
fromList :: [a] -> BArray V1 a
fromList = G.fromList
{-# INLINE fromList #-}

-- | O(n) Convert the first @n@ elements of a list to an BArrayith the
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto :: Shape l => Layout l -> [a] -> Maybe (BArray l a)
fromListInto = G.fromListInto
{-# INLINE fromListInto #-}

-- | O(n) Convert the first @n@ elements of a list to an BArrayith the
--   given shape. Throw an error if the list is not long enough.
fromListInto_ :: Shape l => Layout l -> [a] -> BArray l a
fromListInto_ = G.fromListInto_
{-# INLINE fromListInto_ #-}

-- | Create an array from a 'vector' and a 'layout'. Return 'Nothing' if
--   the vector is not the right shape.
fromVectorInto :: Shape l => Layout l -> Vector a -> Maybe (BArray l a)
fromVectorInto = G.fromVectorInto
{-# INLINE fromVectorInto #-}

-- | Create an array from a 'vector' and a 'layout'. Throws an error if
--   the vector is not the right shape.
fromVectorInto_ :: Shape l => Layout l -> Vector a -> BArray l a
fromVectorInto_ = G.fromVectorInto_
{-# INLINE fromVectorInto_ #-}

-- | The empty 'BArray' with a 'zero' shape.
empty :: (Additive l) => BArray l a
empty = G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: F.Foldable l => BArray l a -> Bool
null = G.null
{-# INLINE null #-}

-- Indexing ------------------------------------------------------------

-- | Index an element of an array. Throws 'IndexOutOfBounds' if the
--   index is out of bounds.
(!) :: Shape l => BArray l a -> l Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

-- | Safe index of an element.
(!?) :: Shape l => BArray l a -> l Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

-- | Index an element of an array without bounds checking.
unsafeIndex :: Shape l => BArray l a -> l Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

-- | Index an element of an array while ignoring its shape.
linearIndex :: BArray l a -> Int -> a
linearIndex = G.linearIndex
{-# INLINE linearIndex #-}

-- | Index an element of an array while ignoring its shape, without
--   bounds checking.
unsafeLinearIndex :: BArray l a -> Int -> a
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
indexM :: (Shape l, Monad m) => BArray l a -> l Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeIndexM :: (Shape l, Monad m) => BArray l a -> l Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ Indexing in a monad. Throws an error if the index is out of
--   range.
linearIndexM :: (Shape l, Monad m) => BArray l a -> Int -> m a
linearIndexM = G.linearIndexM
{-# INLINE linearIndexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeLinearIndexM :: Monad m => BArray l a -> Int -> m a
unsafeLinearIndexM = G.unsafeLinearIndexM
{-# INLINE unsafeLinearIndexM #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: Shape l
       => (forall s. ST s (BMArray l s a)) -> BArray l a
create m = m `seq` runST (m >>= G.unsafeFreeze)
{-# INLINE create #-}

-- | O(n) BArray of the given shape with the same value in each position.
replicate :: Shape l => l Int -> a -> BArray l a
replicate = G.replicate
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
linearGenerate :: Shape l => Layout l -> (Int -> a) -> BArray l a
linearGenerate = G.linearGenerate
{-# INLINE linearGenerate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: Shape l => Layout l -> (l Int -> a) -> BArray l a
generate = G.generate
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape l) => Layout l -> m a -> m (BArray l a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape l) => Layout l -> (l Int -> m a) -> m (BArray l a)
generateM = G.generateM
{-# INLINE generateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
linearGenerateM :: (Monad m, Shape l) => Layout l -> (Int -> m a) -> m (BArray l a)
linearGenerateM = G.linearGenerateM
{-# INLINE linearGenerateM #-}

-- Modifying -----------------------------------------------------------

-- | /O(n)/ Map a function over an array
map :: (a -> b) -> BArray l a -> BArray l b
map = G.map
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: Shape l => (l Int -> a -> b) -> BArray l a -> BArray l b
imap = G.imap
{-# INLINE imap #-}

-- Bulk updates --------------------------------------------------------


-- | For each pair (i,a) from the list, replace the array element at
--   position i by a.
(//) :: Shape l => BArray l a -> [(l Int, a)] -> BArray l a
(//) = (G.//)
{-# INLINE (//) #-}

-- Accumilation --------------------------------------------------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the array element
--   @a@ at position @i@ by @f a b@.
--
accum :: Shape l
      => (a -> b -> a) -- ^ accumulating function @f@
      -> BArray l a     -- ^ initial array
      -> [(l Int, b)]  -- ^ list of index/value pairs (of length @n@)
      -> BArray l a
accum = G.accum
{-# INLINE accum #-}

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- Tuple zip -----------------------------------------------------------

-- | Zip two arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip :: Shape l
    => BArray l a
    -> BArray l b
    -> BArray l (a,b)
zip = G.zip

-- | Zip three arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip3 :: Shape l
     => BArray l a
     -> BArray l b
     -> BArray l c
     -> BArray l (a,b,c)
zip3 = G.zip3

-- Zip with function ---------------------------------------------------

-- | Zip two arrays using the given function. If the array's don't have
--   the same shape, the new array with be the intersection of the two
--   shapes.
zipWith :: Shape l
        => (a -> b -> c)
        -> BArray l a
        -> BArray l b
        -> BArray l c
zipWith = G.zipWith
{-# INLINE zipWith #-}

-- | Zip three arrays using the given function. If the array's don't
--   have the same shape, the new array with be the intersection of the
--   two shapes.
zipWith3 :: Shape l
         => (a -> b -> c -> d)
         -> BArray l a
         -> BArray l b
         -> BArray l c
         -> BArray l d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

-- Indexed zipping -----------------------------------------------------

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith :: Shape l
         => (l Int -> a -> b -> c)
         -> BArray l a
         -> BArray l b
         -> BArray l c
izipWith = G.izipWith
{-# INLINE izipWith #-}

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith3 :: Shape l
          => (l Int -> a -> b -> c -> d)
          -> BArray l a
          -> BArray l b
          -> BArray l c
          -> BArray l d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

------------------------------------------------------------------------
-- Slices
------------------------------------------------------------------------

-- $setup
-- >>> import Debug.SimpleReflect
-- >>> let m = fromListInto_ (V2 3 4) [a,b,c,d,e,f,g,h,i,j,k,l] :: BBArray V2 Expr

-- | Indexed traversal over the rows of a matrix. Each row is an
--   efficient 'Data.Vector.Generic.slice' of the original vector.
--
-- >>> traverseOf_ rows print m
-- [a,b,c,d]
-- [e,f,g,h]
-- [i,j,k,l]
rows :: IndexedTraversal Int (BArray V2 a) (BArray V2 b) (Vector a) (Vector b)
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
ixRow :: Int -> IndexedTraversal' Int (BArray V2 a) (Vector a)
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
columns :: IndexedTraversal Int (BArray V2 a) (BArray V2 b) (Vector a) (Vector b)
columns = G.columns
{-# INLINE columns #-}

-- | Affine traversal over a single column in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixColumn 2 . each +~ 1
-- [a,b,c + 1,d]
-- [e,f,g + 1,h]
-- [i,j,k + 1,l]
ixColumn :: Int -> IndexedTraversal' Int (BArray V2 a) (Vector a)
ixColumn = G.ixColumn
{-# INLINE ixColumn #-}

-- | Traversal over a single plane of a 3D array given a lens onto that
--   plane (like '_xy', '_yz', '_zx').
ixPlane :: ALens' (V3 Int) (V2 Int)
        -> Int
        -> IndexedTraversal' Int (BArray V3 a) (BArray V2 a)
ixPlane = G.ixPlane
{-# INLINE ixPlane #-}

-- | Traversal over all planes of 3D array given a lens onto that plane
--   (like '_xy', '_yz', '_zx').
planes :: ALens' (V3 Int) (V2 Int)
       -> IndexedTraversal Int (BArray V3 a) (BArray V3 b) (BArray V2 a) (BArray V2 b)
planes = G.planes
{-# INLINE planes #-}

-- | Flatten a plane by reducing a vector in the third dimension to a
--   single value.
flattenPlane :: ALens' (V3 Int) (V2 Int)
             -> (Vector a -> b)
             -> BArray V3 a
             -> BArray V2 b
flattenPlane = G.flattenPlane
{-# INLINE flattenPlane #-}

-- Ordinals ------------------------------------------------------------

-- | This 'Traversal' should not have any duplicates in the list of
--   indices.
unsafeOrdinals :: Shape l => [l Int] -> IndexedTraversal' (l Int) (BArray l a) a
unsafeOrdinals = G.unsafeOrdinals
{-# INLINE [0] unsafeOrdinals #-}

-- Mutable -------------------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape l)
       => BMArray l (PrimState m) a -> m (BArray l a)
freeze = G.freeze
{-# INLINE freeze #-}

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape l)
     => BArray l a -> m (BMArray l (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Shape l)
             => BMArray l (PrimState m) a -> m (BArray l a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Shape l)
           => BArray l a -> m (BMArray l (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | Isomorphism between an array and it's delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Shape l, Shape k)
        => Iso (BArray l a) (BArray k b) (G.Delayed l a) (G.Delayed k b)
delayed = G.delayed
{-# INLINE delayed #-}

-- | Turn a material array into a delayed one with the same shape.
delay :: Shape l => BArray l a -> G.Delayed l a
delay = G.delay
{-# INLINE delay #-}

-- | Parallel manifestation of a delayed array into a material one.
manifest :: Shape l => G.Delayed l a -> BArray l a
manifest = G.manifest
{-# INLINE manifest #-}

-- | Sequential manifestation of a delayed array.
seqManifest :: Shape l => G.Delayed l a -> BArray l a
seqManifest = G.seqManifest
{-# INLINE seqManifest #-}

-- | 'manifest' an array to a 'BArray' and delay again.
affirm :: Shape l => G.Delayed l a -> G.Delayed l a
affirm = delay . manifest
{-# INLINE affirm #-}

-- | 'seqManifest' an array to a 'BArray' and delay again.
seqAffirm :: Shape l => G.Delayed l a -> G.Delayed l a
seqAffirm = delay . seqManifest
{-# INLINE seqAffirm #-}

