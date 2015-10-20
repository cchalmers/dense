{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Generic
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides generic functions over shaped vectors.
-----------------------------------------------------------------------------
module Data.Shaped.Generic
  (
    -- * Array types
    Array
  , Shape (..)
  , BArray
  , UArray
  , SArray
  , PArray

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
  , Data.Shaped.Generic.zip
  , Data.Shaped.Generic.zip3

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
  , MArray
  , M.BMArray
  , M.UMArray
  , M.SMArray
  , M.PMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , Delayed

  -- ** Generating delayed

  , delayed
  , delay
  , manifest
  , seqManifest
  , genDelayed
  , indexDelayed
  , affirm
  , seqAffirm

  -- * Focused

  , Focused

  -- ** Generating focused

  , focusOn
  , unfocus
  , unfocused
  , extendFocus

  -- ** Focus location
  , locale
  , shiftFocus

  -- * Fusion
  -- ** Streams
  , streamGenerate
  , streamGenerateM
  , streamIndexes

  -- ** Bundles
  , bundleGenerate
  , bundleGenerateM
  , bundleIndexes

  ) where


#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative               (Applicative, pure, (<*>))
import           Data.Foldable                     (Foldable)
#endif

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens                      hiding (imap)
import           Control.Monad                     (liftM)
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Foldable                     as F
import           Data.Functor.Classes
import qualified Data.List                         as L
import           Data.Maybe                        (fromMaybe)
import qualified Data.Vector                       as B
import           Data.Vector.Fusion.Bundle         (MBundle)
import qualified Data.Vector.Fusion.Bundle         as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import           Data.Vector.Generic               (Vector)
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Storable              as S
import qualified Data.Vector.Unboxed               as U
import           Linear                            hiding (vector)

import           Data.Shaped.Base
import           Data.Shaped.Index
import           Data.Shaped.Mutable               (MArray (..))
import qualified Data.Shaped.Mutable               as M

import           Prelude                           hiding (map, null, replicate,
                                                    zipWith, zipWith3)

-- Aliases -------------------------------------------------------------

-- | 'Boxed' array.
type BArray = Array B.Vector

-- | 'Data.Vector.Unboxed.Unbox'ed array.
type UArray = Array U.Vector

-- | 'Foreign.Storable.Storeable' array.
type SArray = Array S.Vector

-- | 'Data.Primitive.Types.Prim' array.
type PArray = Array P.Vector

-- Lenses --------------------------------------------------------------

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape f, Vector v a, Vector v b)
       => IndexedTraversal (f Int) (Array v f a) (Array v f b) a b
values' = values
{-# INLINE values' #-}

-- | Traverse over the 'values' between two indexes.
valuesBetween :: (Shape f, Vector v a) => f Int -> f Int -> IndexedTraversal' (f Int) (Array v f a) a
valuesBetween a b = unsafeOrdinals (toListOf (shapeIndexesFrom a) b)
{-# INLINE valuesBetween #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in "Data.Vector.Generic" on them without
--   needing to convert.
flat :: (Vector v a, Vector w b) => Iso (Array v V1 a) (Array w V1 b) (v a) (w b)
flat = iso (\(Array _ v) -> v) (\v -> Array (V1 $ G.length v) v)
{-# INLINE flat #-}

-- Constructing vectors ------------------------------------------------

-- | Contruct a flat array from a list. (This is just 'G.fromList' from
--   'Data.Vector.Generic'.)
fromList :: Vector v a => [a] -> Array v V1 a
fromList = G.fromList
{-# INLINE fromList #-}

-- | O(n) Convert the first @n@ elements of a list to an Array with the
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto :: (Shape f, Vector v a) => Layout f -> [a] -> Maybe (Array v f a)
fromListInto l as
  | G.length v == n = Just $ Array l v
  | otherwise       = Nothing
  where v = G.fromListN n as
        n = shapeSize l
{-# INLINE fromListInto #-}

-- | O(n) Convert the first @n@ elements of a list to an Array with the
--   given shape. Throw an error if the list is not long enough.
fromListInto_ :: (Shape f, Vector v a) => Layout f -> [a] -> Array v f a
fromListInto_ l as = fromMaybe err $ fromListInto l as
  where
    err = error $ "fromListInto_: shape " ++ showShape l ++ " is too large for list"
{-# INLINE fromListInto_ #-}

-- | Create an array from a 'vector' and a 'layout'. Return 'Nothing' if
--   the vector is not the right shape.
fromVectorInto :: (Shape f, Vector v a) => Layout f -> v a -> Maybe (Array v f a)
fromVectorInto l v
  | shapeSize l == G.length v = Just $! Array l v
  | otherwise                 = Nothing
{-# INLINE fromVectorInto #-}

-- | Create an array from a 'vector' and a 'layout'. Throws an error if
--   the vector is not the right shape.
fromVectorInto_ :: (Shape f, Vector v a) => Layout f -> v a -> Array v f a
fromVectorInto_ l as = fromMaybe err $ fromVectorInto l as
  where
    err = error $ "fromVectorInto_: shape " ++ showShape l ++ " is too large for the vector"
{-# INLINE fromVectorInto_ #-}

-- | The empty 'Array' with a 'zero' shape.
empty :: (Vector v a, Additive f) => Array v f a
empty = Array zero G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: Foldable f => Array v f a -> Bool
null (Array l _) = F.all (==0) l
{-# INLINE null #-}

-- Indexing ------------------------------------------------------------

-- | Index an element of an array. Throws 'IndexOutOfBounds' if the
--   index is out of bounds.
(!) :: (Shape f, Vector v a) => Array v f a -> f Int -> a
Array l v ! i = boundsCheck l i $ G.unsafeIndex v (shapeToIndex l i)
{-# INLINE (!) #-}

-- | Safe index of an element.
(!?) :: (Shape f, Vector v a) => Array v f a -> f Int -> Maybe a
Array l v !? i
  | shapeInRange l i = Just $! G.unsafeIndex v (shapeToIndex l i)
  | otherwise   = Nothing
{-# INLINE (!?) #-}

-- | Index an element of an array without bounds checking.
unsafeIndex :: (Shape f, Vector v a) => Array v f a -> f Int -> a
unsafeIndex (Array l v) i = G.unsafeIndex v (shapeToIndex l i)
{-# INLINE unsafeIndex #-}

-- | Index an element of an array while ignoring its shape.
linearIndex :: Vector v a => Array v f a -> Int -> a
linearIndex (Array _ v) i = v G.! i
{-# INLINE linearIndex #-}

-- | Index an element of an array while ignoring its shape, without
--   bounds checking.
unsafeLinearIndex :: Vector v a => Array v f a -> Int -> a
unsafeLinearIndex (Array _ v) i = G.unsafeIndex v i
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
indexM :: (Shape f, Vector v a, Monad m) => Array v f a -> f Int -> m a
indexM (Array l v) i = boundsCheck l i $ G.unsafeIndexM v (shapeToIndex l i)
{-# INLINE indexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeIndexM :: (Shape f, Vector v a, Monad m) => Array v f a -> f Int -> m a
unsafeIndexM (Array l v) i = G.unsafeIndexM v (shapeToIndex l i)
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ Indexing in a monad. Throws an error if the index is out of
--   range.
linearIndexM :: (Shape f, Vector v a, Monad m) => Array v f a -> Int -> m a
linearIndexM (Array l v) i = boundsCheck l (shapeFromIndex l i) $ G.unsafeIndexM v i
{-# INLINE linearIndexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeLinearIndexM :: (Vector v a, Monad m) => Array v f a -> Int -> m a
unsafeLinearIndexM (Array _ v) = G.unsafeIndexM v
{-# INLINE unsafeLinearIndexM #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: (Vector v a, Shape f)
       => (forall s. ST s (MArray (G.Mutable v) f s a)) -> Array v f a
create m = m `seq` runST (m >>= unsafeFreeze)
{-# INLINE create #-}

-- | O(n) Array of the given shape with the same value in each position.
replicate :: (Shape f, Vector v a) => f Int -> a -> Array v f a
replicate l a
  | n > 0     = Array l $ G.replicate n a
  | otherwise = empty
  where n = shapeSize l
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
linearGenerate :: (Shape f, Vector v a) => Layout f -> (Int -> a) -> Array v f a
linearGenerate l f
  | n > 0     = Array l $ G.generate n f
  | otherwise = empty
  where n = shapeSize l
{-# INLINE linearGenerate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: (Shape f, Vector v a) => Layout f -> (f Int -> a) -> Array v f a
generate l f = Array l $ G.unstream (bundleGenerate l f)
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape f, Vector v a) => Layout f -> m a -> m (Array v f a)
replicateM l a
  | n > 0     = Array l `liftM` G.replicateM n a
  | otherwise = return empty
  where n = shapeSize l
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape f, Vector v a) => Layout f -> (f Int -> m a) -> m (Array v f a)
generateM l f = Array l `liftM` unstreamM (bundleGenerateM l f)
{-# INLINE generateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
linearGenerateM :: (Monad m, Shape f, Vector v a) => Layout f -> (Int -> m a) -> m (Array v f a)
linearGenerateM l f
  | n > 0     = Array l `liftM` G.generateM n f
  | otherwise = return empty
  where n = shapeSize l
{-# INLINE linearGenerateM #-}

-- Modifying -----------------------------------------------------------

-- | /O(n)/ Map a function over an array
map :: (Vector v a, Vector v b) => (a -> b) -> Array v f a -> Array v f b
map f (Array l a) = Array l (G.map f a)
{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Shape f, Vector v a, Vector v b) => (f Int -> a -> b) -> Array v f a -> Array v f b
imap f (Array l v) =
  Array l $ (G.unstream . Bundle.inplace (Stream.zipWith f (streamIndexes l)) id . G.stream) v
{-# INLINE imap #-}

-- Bulk updates --------------------------------------------------------

-- | For each pair (i,a) from the list, replace the array element at
--   position i by a.
(//) :: (G.Vector v a, Shape f) => Array v f a -> [(f Int, a)] -> Array v f a
Array l v // xs = Array l $ v G.// over (each . _1) (shapeToIndex l) xs

-- Accumilation --------------------------------------------------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the array element
--   @a@ at position @i@ by @f a b@.
--
accum :: (Shape f, Vector v a)
      => (a -> b -> a)  -- ^ accumulating function @f@
      -> Array v f a    -- ^ initial array
      -> [(f Int, b)]   -- ^ list of index/value pairs (of length @n@)
      -> Array v f a
accum f (Array l v) us = Array l $ G.accum f v (over (mapped . _1) (shapeToIndex l) us)
{-# INLINE accum #-}

------------------------------------------------------------------------
-- Streams
------------------------------------------------------------------------

-- Copied from Data.Vector.Generic because it isn't exported from there.

unstreamM :: (Monad m, Vector v a) => Bundle.MBundle m u a -> m (v a)
{-# INLINE [1] unstreamM #-}
unstreamM s = do
  xs <- MBundle.toList s
  return $ G.unstream $ Bundle.unsafeFromList (MBundle.size s) xs

unstreamPrimM :: (PrimMonad m, Vector v a) => Bundle.MBundle m u a -> m (v a)
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM s = GM.munstream s >>= G.unsafeFreeze

-- FIXME: the next two functions are only necessary for the specialisations
unstreamPrimM_IO :: Vector v a => Bundle.MBundle IO u a -> IO (v a)
{-# INLINE unstreamPrimM_IO #-}
unstreamPrimM_IO = unstreamPrimM

unstreamPrimM_ST :: Vector v a => Bundle.MBundle (ST s) u a -> ST s (v a)
{-# INLINE unstreamPrimM_ST #-}
unstreamPrimM_ST = unstreamPrimM

{-# RULES

"unstreamM[IO]" unstreamM = unstreamPrimM_IO
"unstreamM[ST]" unstreamM = unstreamPrimM_ST  #-}

-- | Generate a stream from a 'Layout''s indices.
streamGenerate :: (Monad m, Shape f) => Layout f -> (f Int -> a) -> Stream m a
streamGenerate l f = streamGenerateM l (return . f)
{-# INLINE streamGenerate #-}

-- | Generate a stream from a 'Layout''s indices.
streamGenerateM :: (Monad m, Shape f) => Layout f -> (f Int -> m a) -> Stream m a
streamGenerateM l f = l `seq` Stream step (if eq1 l zero then Nothing else Just zero)
  where
    {-# INLINE [0] step #-}
    step (Just i) = do
      x <- f i
      return $ Yield x (shapeStep l i)
    step Nothing  = return Done
{-# INLINE [1] streamGenerateM #-}

-- | Stream a sub-layout of an 'Array'. The layout should be shapeInRange of
--   the array's layout, this is not checked.
unsafeStreamSub :: (Monad m, Shape f, G.Vector v a) => Layout f -> Array v f a -> Stream m a
unsafeStreamSub l2 (Array l1 v) = streamGenerateM l2 $ \x -> G.basicUnsafeIndexM v (shapeToIndex l1 x)
{-# INLINE unsafeStreamSub #-}

-- | Stream a sub-layout of an 'Array'.
streamSub :: (Monad m, Shape f, G.Vector v a) => Layout f -> Array v f a -> Stream m a
streamSub l2 arr@(Array l1 _) = unsafeStreamSub (shapeIntersect l1 l2) arr
{-# INLINE streamSub #-}

-- | Make a stream of the indexes of a 'Layout'.
streamIndexes :: (Monad m, Shape f) => Layout f -> Stream m (f Int)
streamIndexes l = Stream step (if eq1 l zero then Nothing else Just zero)
  where
    {-# INLINE [0] step #-}
    step (Just i) = return $ Yield i (shapeStep l i)
    step Nothing  = return Done
{-# INLINE [1] streamIndexes #-}

------------------------------------------------------------------------
-- Bundles
------------------------------------------------------------------------

-- | Generate a bundle from 'Layout' indices.
bundleGenerate :: (Monad m, Shape f) => Layout f -> (f Int -> a) -> MBundle m v a
bundleGenerate l f = bundleGenerateM l (return . f)
{-# INLINE bundleGenerate #-}

-- | Generate a bundle from 'Layout' indices.
bundleGenerateM :: (Monad m, Shape f) => Layout f -> (f Int -> m a) -> MBundle m v a
bundleGenerateM l f = MBundle.fromStream (streamGenerateM l f) (Exact (shapeSize l))
{-# INLINE [1] bundleGenerateM #-}

-- | Generate a bundle of indexes for the given 'Layout'.
bundleIndexes :: (Monad m, Shape f) => Layout f -> MBundle m v (f Int)
bundleIndexes l = MBundle.fromStream (streamIndexes l) (Exact (shapeSize l))
{-# INLINE [1] bundleIndexes #-}

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- Tuple zip -----------------------------------------------------------

-- | Zip two arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip :: (Shape f, Vector v a, Vector v b, Vector v (a,b))
    => Array v f a
    -> Array v f b
    -> Array v f (a,b)
zip = zipWith (,)

-- | Zip three arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip3 :: (Shape f, Vector v a, Vector v b, Vector v c, Vector v (a,b,c))
     => Array v f a
     -> Array v f b
     -> Array v f c
     -> Array v f (a,b,c)
zip3 = zipWith3 (,,)

-- Zip with function ---------------------------------------------------

-- | Zip two arrays using the given function. If the array's don't have
--   the same shape, the new array with be the intersection of the two
--   shapes.
zipWith :: (Shape f, Vector v a, Vector v b, Vector v c)
        => (a -> b -> c)
        -> Array v f a
        -> Array v f b
        -> Array v f c
zipWith f a1@(Array l1 v1) a2@(Array l2 v2)
  | eq1 l1 l1 = Array l1 $ G.zipWith f v1 v2
  | otherwise = Array l' $ G.unstream $
      MBundle.fromStream (Stream.zipWith f (streamSub l' a1) (streamSub l' a2)) (Exact (shapeSize l'))
  where l' = shapeIntersect l1 l2
{-# INLINE zipWith #-}

-- | Zip three arrays using the given function. If the array's don't
--   have the same shape, the new array with be the intersection of the
--   two shapes.
zipWith3 :: (Shape f, Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d)
         -> Array v f a
         -> Array v f b
         -> Array v f c
         -> Array v f d
zipWith3 f a1@(Array l1 v1) a2@(Array l2 v2) a3@(Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.zipWith3 f v1 v2 v3
  | otherwise = Array l' $ G.unstream $
      MBundle.fromStream (Stream.zipWith3 f (streamSub l' a1) (streamSub l' a2) (streamSub l' a3)) (Exact (shapeSize l'))
  where l' = shapeIntersect (shapeIntersect l1 l2) l3
{-# INLINE zipWith3 #-}

-- Indexed zipping -----------------------------------------------------

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith :: (Shape f, Vector v a, Vector v b, Vector v c)
         => (f Int -> a -> b -> c)
         -> Array v f a
         -> Array v f b
         -> Array v f c
izipWith f a1@(Array l1 v1) a2@(Array l2 v2)
  | eq1 l1 l2 = Array l1 $ G.unstream $ Bundle.zipWith3 f (bundleIndexes l1) (G.stream v1) (G.stream v2)
  | otherwise = Array l' $ G.unstream $
      MBundle.fromStream (Stream.zipWith3 f (streamIndexes l') (streamSub l' a1) (streamSub l' a2)) (Exact (shapeSize l'))
  where l' = shapeIntersect l1 l2
{-# INLINE izipWith #-}

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith3 :: (Shape f, Vector v a, Vector v b, Vector v c, Vector v d)
          => (f Int -> a -> b -> c -> d)
          -> Array v f a
          -> Array v f b
          -> Array v f c
          -> Array v f d
izipWith3 f a1@(Array l1 v1) a2@(Array l2 v2) a3@(Array l3 v3)
  | eq1 l1 l2 = Array l1 $ G.unstream $ Bundle.zipWith4 f (bundleIndexes l1) (G.stream v1) (G.stream v2) (G.stream v3)
  | otherwise =
      Array l' $ G.unstream $ MBundle.fromStream
        (Stream.zipWith4 f (streamIndexes l') (streamSub l' a1) (streamSub l' a2) (streamSub l' a3)) (Exact (shapeSize l'))
  where l' = shapeIntersect (shapeIntersect l1 l2) l3
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
rows :: (Vector v a, Vector w b)
     => IndexedTraversal Int (Array v V2 a) (Array w V2 b) (v a) (w b)
rows f (Array l@(V2 x y) v) = Array l . G.concat <$> go 0 0 where
  go i a | i >= x    = pure []
         | otherwise = (:) <$> indexed f i (G.slice a y v) <*> go (i+1) (a+y)
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
ixRow :: Vector v a => Int -> IndexedTraversal' Int (Array v V2 a) (v a)
ixRow i f m@(Array (l@(V2 x y)) v)
  | y >= 0 && i < x = Array l . G.unsafeUpd v . L.zip [a..] . G.toList . G.take y <$> indexed f i (G.slice a y v)
  | otherwise       = pure m
  where a  = i * y
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
columns :: (Vector v a, Vector w b)
        => IndexedTraversal Int (Array v V2 a) (Array w V2 b) (v a) (w b)
columns f m@(Array l@(V2 _ y) _) = transposeConcat l <$> go 0 where
  go j | j >= y    = pure []
       | otherwise = (:) <$> indexed f j (getColumn m j) <*> go (j+1)
{-# INLINE columns #-}

-- | Affine traversal over a single column in a matrix.
--
-- >>> traverseOf_ rows print $ m & ixColumn 2 . each +~ 1
-- [a,b,c + 1,d]
-- [e,f,g + 1,h]
-- [i,j,k + 1,l]
ixColumn :: Vector v a => Int -> IndexedTraversal' Int (Array v V2 a) (v a)
ixColumn j f m@(Array (l@(V2 _ y)) v)
  | j >= 0 && j < y = Array l . G.unsafeUpd v . L.zip js . G.toList . G.take y <$> indexed f j (getColumn m j)
  | otherwise       = pure m
  where js = [j, j + y .. ]
{-# INLINE ixColumn #-}

getColumn :: Vector v a => Array v V2 a -> Int -> v a
getColumn (Array (V2 x y) v) j = G.generate x $ \i -> G.unsafeIndex v (i * y + j)
{-# INLINE getColumn #-}

transposeConcat :: Vector v a => V2 Int -> [v a] -> Array v V2 a
transposeConcat (V2 _ y) vs = Array (V2 x' y) $ G.create $ do
  mv <- GM.new (x'*y)
  iforM_ vs $ \j v ->
    F.for_ [0..x'-1] $ \i ->
      GM.write mv (i*y + j) (v G.! i)
  return mv
  where x' = minimum $ fmap G.length vs
{-# INLINE transposeConcat #-}

-- | Traversal over a single plane of a 3D array given a lens onto that
--   plane (like '_xy', '_yz', '_zx').
ixPlane :: Vector v a
        => ALens' (V3 Int) (V2 Int)
        -> Int
        -> IndexedTraversal' Int (Array v V3 a) (Array v V2 a)
ixPlane l32 i f a@(Array l v)
  | i < 0 || i >= k = pure a
  | otherwise       = Array l . (v G.//) . L.zip is . toListOf values
                        <$> indexed f i (getPlane l32 i a)
  where
    is = toListOf (cloneLens l32 . shapeIndexes . to (\x -> shapeToIndex l $ pure i & l32 #~ x)) l
    k  = F.sum $ l & l32 #~ 0

-- | Traversal over all planes of 3D array given a lens onto that plane
--   (like '_xy', '_yz', '_zx').
planes :: (Vector v a, Vector w b)
       => ALens' (V3 Int) (V2 Int)
       -> IndexedTraversal Int (Array v V3 a) (Array w V3 b) (Array v V2 a) (Array w V2 b)
planes l32 f a@(Array l _) = concatPlanes l l32 <$> go 0 where
  go i | i >= k    = pure []
       | otherwise = (:) <$> indexed f i (getPlane l32 i a) <*> go (i+1)
  k = F.sum $ l & l32 #~ 0
{-# INLINE planes #-}

concatPlanes :: Vector v a => V3 Int -> ALens' (V3 Int) (V2 Int) -> [Array v V2 a] -> Array v V3 a
concatPlanes l l32 as = create $ do
  arr <- M.new l
  iforM_ as $ \i m ->
    iforMOf_ values m $ \x a -> do
      let w = pure i & l32 #~ x
      M.write arr w a
  return arr

getPlane :: Vector v a => ALens' (V3 Int) (V2 Int) -> Int -> Array v V3 a -> Array v V2 a
getPlane l32 i a = generate (a ^# layout . l32) $ \x -> a ! (pure i & l32 #~ x)

-- | Flatten a plane by reducing a vector in the third dimension to a
--   single value.
flattenPlane :: (Vector v a, Vector w b)
             => ALens' (V3 Int) (V2 Int)
             -> (v a -> b)
             -> Array v V3 a
             -> Array w V2 b
flattenPlane l32 f a@(Array l _) = generate l' $ \x -> f (getVector x)
  where
    getVector x = G.generate n $ \i -> a ! (pure i & l32 #~ x)
    n  = F.sum $ l & l32 #~ 0
    l' = l ^# l32
{-# INLINE flattenPlane #-}

-- Ordinals ------------------------------------------------------------

-- | This 'Traversal' should not have any duplicates in the list of
--   indices.
unsafeOrdinals :: (Vector v a, Shape f) => [f Int] -> IndexedTraversal' (f Int) (Array v f a) a
unsafeOrdinals is f (Array l v) = Array l . (v G.//) <$> traverse g is
  where g x = let i = shapeToIndex l x in (,) i <$> indexed f x (G.unsafeIndex v i)
{-# INLINE [0] unsafeOrdinals #-}

setOrdinals :: (Indexable (f Int) p, Vector v a, Shape f) => [f Int] -> p a a -> Array v f a -> Array v f a
setOrdinals is f (Array l v) = Array l $ G.unsafeUpd v (fmap g is)
  where g x = let i = shapeToIndex l x in (,) i $ indexed f x (G.unsafeIndex v i)
{-# INLINE setOrdinals #-}

{-# RULES
"unsafeOrdinals/setOrdinals" forall (is :: [f Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape f) => ASetter' (Array v f a) a;
"unsafeOrdinalts/isetOrdintals" forall (is :: [f Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape f) => AnIndexedSetter' (f Int) (Array v f a) a
 #-}

-- Mutable -------------------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape f, Vector v a)
       => MArray (G.Mutable v) f (PrimState m) a -> m (Array v f a)
freeze (MArray l mv) = Array l `liftM` G.freeze mv
{-# INLINE freeze #-}

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape f, Vector v a)
     => Array v f a -> m (MArray (G.Mutable v) f (PrimState m) a)
thaw (Array l v) = MArray l `liftM` G.thaw v
{-# INLINE thaw #-}

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | Isomorphism between an array and it's delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Vector v a, Vector w b, Shape f, Shape g)
        => Iso (Array v f a) (Array w g b) (Delayed f a) (Delayed g b)
delayed = iso delay manifest
{-# INLINE delayed #-}

-- | Sequential manifestation of a delayed array.
seqManifest :: (Vector v a, Shape f) => Delayed f a -> Array v f a
seqManifest (Delayed l f) = generate l f
{-# INLINE seqManifest #-}

-- | 'manifest' an array to a 'UArray' and delay again. See
--   "Data.Shaped.Boxed" or "Data.Shaped.Storable" to 'affirm' for other
--   types of arrays.
affirm :: (Shape f, U.Unbox a) => Delayed f a -> Delayed f a
affirm = delay . (manifest :: (U.Unbox a, Shape f) => Delayed f a -> UArray f a)
{-# INLINE affirm #-}

-- | 'seqManifest' an array to a 'UArray' and delay again. See
--   "Data.Shaped.Boxed" or "Data.Shaped.Storable" to 'affirm' for other
--   types of arrays.
seqAffirm :: (Shape f, U.Unbox a) => Delayed f a -> Delayed f a
seqAffirm = delay . (seqManifest :: (U.Unbox a, Shape f) => Delayed f a -> UArray f a)
{-# INLINE seqAffirm #-}

------------------------------------------------------------------------
-- Focused
------------------------------------------------------------------------

-- | Focus on a particular element of a delayed array.
focusOn :: f Int -> Delayed f a -> Focused f a
focusOn = Focused -- XXX do range checking
{-# INLINE focusOn #-}

-- | Discard the focus to retrieve the delayed array.
unfocus :: Focused f a -> Delayed f a
unfocus (Focused _ d) = d
{-# INLINE unfocus #-}

-- | Indexed lens onto the delayed array, indexed at the focus.
unfocused :: IndexedLens (f Int) (Focused f a) (Focused f b) (Delayed f a) (Delayed f b)
unfocused f (Focused x d) = Focused x <$> indexed f x d
{-# INLINE unfocused #-}

-- | Modify a 'Delayed' array by extracting a value from a 'Focused'
--   each point.
extendFocus :: Shape f => (Focused f a -> b) -> Delayed f a -> Delayed f b
extendFocus f = unfocus . extend f . focusOn zero
{-# INLINE extendFocus #-}

-- | Lens onto the position of a 'ComonadStore'.
--
-- @
-- 'locale' :: 'Lens'' ('Focused' l a) (l 'Int')
-- @
locale :: ComonadStore s w => Lens' (w a) s
locale f w = (`seek` w) <$> f (pos w)
{-# INLINE locale #-}

-- | Focus on a neighbouring element, relative to the current focus.
shiftFocus :: Applicative f => f Int -> Focused f a -> Focused f a
shiftFocus dx (Focused x d@(Delayed l _)) = Focused x' d
  where
    x' = f <$> l <*> x <*> dx
    f k i di
      | i' < 0    = k + i'
      | i' >= k   = i' - k
      | otherwise = i'
      where i' = i + di
{-# INLINE shiftFocus #-}

