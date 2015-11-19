{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Storable
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Storableed shaped vectors.
-----------------------------------------------------------------------------
module Data.Shaped.Storable
  (
    -- * SArray types
    SArray
  , Storable
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
  , SMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , G.Delayed

  -- ** Generating delayed

  , delayed
  , seqDelayed
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

  -- ** Pointers

  , unsafeWithPtr
  , unsafeToForeignPtr
  , unsafeFromForeignPtr
  ) where

import           Control.Lens            hiding (imap)
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Foldable           as F
import           Data.Vector.Storable    (Storable, Vector)
import qualified Data.Vector.Storable    as S
import           Linear                  hiding (vector)
import           Foreign (Ptr, ForeignPtr)

import           Prelude                 hiding (map, null, replicate, zip,
                                          zip3, zipWith, zipWith3)

import           Data.Shaped.Base        (Array (..))
import           Data.Shaped.Generic     (SArray)
import qualified Data.Shaped.Generic     as G
import           Data.Shaped.Index
import           Data.Shaped.Mutable     (SMArray)

-- Lenses --------------------------------------------------------------

-- | Same as 'values' but restrictive in the vector type.
values :: (Shape f, Storable a, Storable b)
       => IndexedTraversal (f Int) (SArray f a) (SArray f b) a b
values = G.values'
{-# INLINE values #-}

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape f, Storable a, Storable b)
       => IndexedTraversal (f Int) (SArray f a) (SArray f b) a b
values' = G.values'
{-# INLINE values' #-}

-- | Same as 'values' but restrictive in the vector type.
valuesBetween
  :: (Shape f, Storable a)
  => f Int
  -> f Int
  -> IndexedTraversal' (f Int) (SArray f a) a
valuesBetween = G.valuesBetween
{-# INLINE valuesBetween #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in 'Data.Vector.Generic' on them without
--   needing to convert.
flat :: (Storable a, Storable b) => Iso (SArray V1 a) (SArray V1 b) (Vector a) (Vector b)
flat = G.flat
{-# INLINE flat #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must _not_ change the length of the
--   vector, otherwise an error will be thrown (even for 'V1' layouts,
--   use 'flat' for 'V1').
vector :: (Storable a, Storable b) => IndexedLens (Layout f) (SArray f a) (SArray f b) (Vector a) (Vector b)
vector = G.vector
{-# INLINE vector #-}

-- Constructing vectors ------------------------------------------------

-- | Contruct a flat array from a list. (This is just 'G.fromList' from
--   'Data.Vector.Generic'.)
fromList :: Storable a => [a] -> SArray V1 a
fromList = G.fromList
{-# INLINE fromList #-}

-- | O(n) Convert the first @n@ elements of a list to an SArrayith the
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto :: (Shape f, Storable a) => Layout f -> [a] -> Maybe (SArray f a)
fromListInto = G.fromListInto
{-# INLINE fromListInto #-}

-- | O(n) Convert the first @n@ elements of a list to an SArrayith the
--   given shape. Throw an error if the list is not long enough.
fromListInto_ :: (Shape f, Storable a) => Layout f -> [a] -> SArray f a
fromListInto_ = G.fromListInto_
{-# INLINE fromListInto_ #-}

-- | Create an array from a 'vector' and a 'layout'. Return 'Nothing' if
--   the vector is not the right shape.
fromVectorInto :: (Shape f, Storable a) => Layout f -> Vector a -> Maybe (SArray f a)
fromVectorInto = G.fromVectorInto
{-# INLINE fromVectorInto #-}

-- | Create an array from a 'vector' and a 'layout'. Throws an error if
--   the vector is not the right shape.
fromVectorInto_ :: (Shape f, Storable a) => Layout f -> Vector a -> SArray f a
fromVectorInto_ = G.fromVectorInto_
{-# INLINE fromVectorInto_ #-}

-- | The empty 'SArray' with a 'zero' shape.
empty :: (Storable a, Additive f) => SArray f a
empty = G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: F.Foldable f => SArray f a -> Bool
null = G.null
{-# INLINE null #-}

-- Indexing ------------------------------------------------------------

-- | Index an element of an array. Throws 'IndexOutOfBounds' if the
--   index is out of bounds.
(!) :: (Shape f, Storable a) => SArray f a -> f Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

-- | Safe index of an element.
(!?) :: (Shape f, Storable a) => SArray f a -> f Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

-- | Index an element of an array without bounds checking.
unsafeIndex :: (Shape f, Storable a) => SArray f a -> f Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

-- | Index an element of an array while ignoring its shape.
linearIndex :: Storable a => SArray f a -> Int -> a
linearIndex = G.linearIndex
{-# INLINE linearIndex #-}

-- | Index an element of an array while ignoring its shape, without
--   bounds checking.
unsafeLinearIndex :: Storable a => SArray f a -> Int -> a
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
indexM :: (Shape f, Storable a, Monad m) => SArray f a -> f Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeIndexM :: (Shape f, Storable a, Monad m) => SArray f a -> f Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ Indexing in a monad. Throws an error if the index is out of
--   range.
linearIndexM :: (Shape f, Storable a, Monad m) => SArray f a -> Int -> m a
linearIndexM = G.linearIndexM
{-# INLINE linearIndexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeLinearIndexM :: (Storable a, Monad m) => SArray f a -> Int -> m a
unsafeLinearIndexM = G.unsafeLinearIndexM
{-# INLINE unsafeLinearIndexM #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: (Storable a, Shape f)
       => (forall s. ST s (SMArray f s a)) -> SArray f a
create m = m `seq` runST (m >>= G.unsafeFreeze)
{-# INLINE create #-}

-- | O(n) SArray of the given shape with the same value in each position.
replicate :: (Shape f, Storable a) => f Int -> a -> SArray f a
replicate = G.replicate
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
linearGenerate :: (Shape f, Storable a) => Layout f -> (Int -> a) -> SArray f a
linearGenerate = G.linearGenerate
{-# INLINE linearGenerate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: (Shape f, Storable a) => Layout f -> (f Int -> a) -> SArray f a
generate = G.generate
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape f, Storable a) => Layout f -> m a -> m (SArray f a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape f, Storable a) => Layout f -> (f Int -> m a) -> m (SArray f a)
generateM = G.generateM
{-# INLINE generateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
linearGenerateM :: (Monad m, Shape f, Storable a) => Layout f -> (Int -> m a) -> m (SArray f a)
linearGenerateM = G.linearGenerateM
{-# INLINE linearGenerateM #-}

-- Modifying -----------------------------------------------------------

-- | /O(n)/ Map a function over an array
map :: (Storable a, Storable b) => (a -> b) -> SArray f a -> SArray f b
map = G.map
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Shape f, Storable a, Storable b) => (f Int -> a -> b) -> SArray f a -> SArray f b
imap = G.imap
{-# INLINE imap #-}

-- Bulk updates --------------------------------------------------------


-- | For each pair (i,a) from the list, replace the array element at
--   position i by a.
(//) :: (Storable a, Shape f) => SArray f a -> [(f Int, a)] -> SArray f a
(//) = (G.//)
{-# INLINE (//) #-}

-- Accumilation --------------------------------------------------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the array element
--   @a@ at position @i@ by @f a b@.
--
accum :: (Shape f, Storable a)
      => (a -> b -> a) -- ^ accumulating function @f@
      -> SArray f a     -- ^ initial array
      -> [(f Int, b)]  -- ^ list of index/value pairs (of length @n@)
      -> SArray f a
accum = G.accum
{-# INLINE accum #-}

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- Zip with function ---------------------------------------------------

-- | Zip two arrays using the given function. If the array's don't have
--   the same shape, the new array with be the intersection of the two
--   shapes.
zipWith :: (Shape f, Storable a, Storable b, Storable c)
        => (a -> b -> c)
        -> SArray f a
        -> SArray f b
        -> SArray f c
zipWith = G.zipWith
{-# INLINE zipWith #-}

-- | Zip three arrays using the given function. If the array's don't
--   have the same shape, the new array with be the intersection of the
--   two shapes.
zipWith3 :: (Shape f, Storable a, Storable b, Storable c, Storable d)
         => (a -> b -> c -> d)
         -> SArray f a
         -> SArray f b
         -> SArray f c
         -> SArray f d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

-- Indexed zipping -----------------------------------------------------

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith :: (Shape f, Storable a, Storable b, Storable c)
         => (f Int -> a -> b -> c)
         -> SArray f a
         -> SArray f b
         -> SArray f c
izipWith = G.izipWith
{-# INLINE izipWith #-}

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith3 :: (Shape f, Storable a, Storable b, Storable c, Storable d)
          => (f Int -> a -> b -> c -> d)
          -> SArray f a
          -> SArray f b
          -> SArray f c
          -> SArray f d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

------------------------------------------------------------------------
-- Slices
------------------------------------------------------------------------

-- $setup
-- >>> import qualified Data.Vector.Storable as V
-- >>> let m = fromListInto_ (V2 2 3) [1..] :: SArray V2 Int

-- | Indexed traversal over the rows of a matrix. Each row is an
--   efficient 'Data.Vector.Generic.slice' of the original vector.
--
-- >>> traverseOf_ rows print m
-- [1,2,3]
-- [4,5,6]
rows :: (Storable a, Storable b)
     => IndexedTraversal Int (SArray V2 a) (SArray V2 b) (Vector a) (Vector b)
rows = G.rows
{-# INLINE rows #-}

-- | Affine traversal over a single row in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixRow 1 . each +~ 2
-- [1,2,3]
-- [6,7,8]
--
--   The row vector should remain the same size to satisfy traversal
--   laws but give reasonable behaviour if the size differs:
--
-- >>> traverseOf_ rows print $ m & ixRow 1 .~ V.fromList [0,1]
-- [1,2,3]
-- [0,1,6]
--
-- >>> traverseOf_ rows print $ m & ixRow 1 .~ V.fromList [0..100]
-- [1,2,3]
-- [0,1,2]
ixRow :: Storable a => Int -> IndexedTraversal' Int (SArray V2 a) (Vector a)
ixRow = G.ixRow
{-# INLINE ixRow #-}

-- | Indexed traversal over the columns of a matrix. Unlike 'rows', each
--   column is a new separate vector.
--
-- >>> traverseOf_ columns print m
-- [1,4]
-- [2,5]
-- [3,6]
--
-- >>> traverseOf_ rows print $ m & columns . indices odd . each .~ 0
-- [1,0,3]
-- [4,0,6]
--
--   The vectors should be the same size to be a valid traversal. If the
--   vectors are different sizes, the number of rows in the new array
--   will be the length of the smallest vector.
columns :: (Storable a, Storable b)
        => IndexedTraversal Int (SArray V2 a) (SArray V2 b) (Vector a) (Vector b)
columns = G.columns
{-# INLINE columns #-}

-- | Affine traversal over a single column in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixColumn 2 . each *~ 10
-- [1,2,30]
-- [4,5,60]
ixColumn :: Storable a => Int -> IndexedTraversal' Int (SArray V2 a) (Vector a)
ixColumn = G.ixColumn
{-# INLINE ixColumn #-}

-- | Traversal over a single plane of a 3D array given a lens onto that
--   plane (like '_xy', '_yz', '_zx').
ixPlane :: Storable a
        => ALens' (V3 Int) (V2 Int)
        -> Int
        -> IndexedTraversal' Int (SArray V3 a) (SArray V2 a)
ixPlane = G.ixPlane
{-# INLINE ixPlane #-}

-- | Traversal over all planes of 3D array given a lens onto that plane
--   (like '_xy', '_yz', '_zx').
planes :: (Storable a, Storable b)
       => ALens' (V3 Int) (V2 Int)
       -> IndexedTraversal Int (SArray V3 a) (SArray V3 b) (SArray V2 a) (SArray V2 b)
planes = G.planes
{-# INLINE planes #-}

-- | Flatten a plane by reducing a vector in the third dimension to a
--   single value.
flattenPlane :: (Storable a, Storable b)
             => ALens' (V3 Int) (V2 Int)
             -> (Vector a -> b)
             -> SArray V3 a
             -> SArray V2 b
flattenPlane = G.flattenPlane
{-# INLINE flattenPlane #-}

-- Ordinals ------------------------------------------------------------

-- | This 'Traversal' should not have any duplicates in the list of
--   indices.
unsafeOrdinals :: (Storable a, Shape f) => [f Int] -> IndexedTraversal' (f Int) (SArray f a) a
unsafeOrdinals = G.unsafeOrdinals
{-# INLINE [0] unsafeOrdinals #-}

-- Mutable -------------------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape f, Storable a)
       => SMArray f (PrimState m) a -> m (SArray f a)
freeze = G.freeze
{-# INLINE freeze #-}

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape f, Storable a)
     => SArray f a -> m (SMArray f (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Shape f, Storable a)
             => SMArray f (PrimState m) a -> m (SArray f a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Shape f, Storable a)
           => SArray f a -> m (SMArray f (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | Isomorphism between an array and its delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Storable a, Storable b, Shape f, Shape k)
        => Iso (SArray f a) (SArray k b) (G.Delayed f a) (G.Delayed k b)
delayed = G.delayed
{-# INLINE delayed #-}

-- | Isomorphism between an array and its delayed representation.
--   Conversion to the array is done in sequence.
seqDelayed :: (Storable a, Storable b, Shape f, Shape k)
        => Iso (SArray f a) (SArray k b) (G.Delayed f a) (G.Delayed k b)
seqDelayed = G.seqDelayed
{-# INLINE seqDelayed #-}

-- | Turn a material array into a delayed one with the same shape.
delay :: (Storable a, Shape f) => SArray f a -> G.Delayed f a
delay = G.delay
{-# INLINE delay #-}

-- | Parallel manifestation of a delayed array into a material one.
manifest :: (Storable a, Shape f) => G.Delayed f a -> SArray f a
manifest = G.manifest
{-# INLINE manifest #-}

-- | Sequential manifestation of a delayed array.
seqManifest :: (Storable a, Shape f) => G.Delayed f a -> SArray f a
seqManifest = G.seqManifest
{-# INLINE seqManifest #-}

-- | 'manifest' an array to a 'SArray' and delay again.
affirm :: (Shape f, Storable a) => G.Delayed f a -> G.Delayed f a
affirm = delay . manifest
{-# INLINE affirm #-}

-- | 'seqManifest' an array to a 'SArray' and delay again.
seqAffirm :: (Shape f, Storable a) => G.Delayed f a -> G.Delayed f a
seqAffirm = delay . seqManifest
{-# INLINE seqAffirm #-}

-- Pointer operations --------------------------------------------------

-- | Pass a pointer to the array's data to the IO action. Modifying
--   data through the 'Ptr' is unsafe.
unsafeWithPtr :: Storable a => SArray f a -> (Ptr a -> IO b) -> IO b
unsafeWithPtr (Array _ v) = S.unsafeWith v
{-# INLINE unsafeWithPtr #-}

-- | Yield the underlying ForeignPtr. Modifying the data through the
--   'ForeignPtr' is unsafe.
unsafeToForeignPtr :: (Shape f, Storable a) => SArray f a -> ForeignPtr a
unsafeToForeignPtr (Array _ v) = fp
  where (fp, _, _) = S.unsafeToForeignPtr v
{-# INLINE unsafeToForeignPtr #-}

-- | O(1) Create an array from a layout and 'ForeignPtr'. It is
--   assumed the pointer points directly to the data (no offset).
--   Modifying data through the 'ForeignPtr' afterwards is unsafe.
unsafeFromForeignPtr
  :: (Shape f, Storable a) => Layout f -> ForeignPtr a -> SArray f a
unsafeFromForeignPtr l fp = Array l (S.unsafeFromForeignPtr0 fp (shapeSize l))
{-# INLINE unsafeFromForeignPtr #-}

