{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides generic functions over shaped vectors. This is
-- equivalent to "vector"'s 'Data.Vector.Generic' module.
-----------------------------------------------------------------------------
module Data.Shaped
  (
    -- * Array types
    Array
  , Shape (..)
  , BArray
  , UArray
  , SArray
  , PArray

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
  , Data.Shaped.zip
  , Data.Shaped.zip3

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
  , manifestS
  , genDelayed
  , indexDelayed

  -- * Focused

  , Focused

  -- ** Generating focused

  , focusOn
  , unfocus
  , unfocused

  -- ** Focus location
  , locale
  , shiftFocus

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


#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative            (Applicative, (<*>), pure)
import           Data.Foldable                  (Foldable)
#endif

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad                  (liftM)
import           Control.Monad.ST
import qualified Data.Foldable                  as F
import           Data.Functor.Classes
import qualified Data.List                   as L
import           Data.Maybe                     (fromMaybe)
import qualified Data.Vector                    as B
import           Data.Vector.Fusion.Bundle      (Bundle)
import qualified Data.Vector.Fusion.Bundle      as Bundle
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Util
import           Data.Vector.Generic            (Vector)
import qualified Data.Vector.Generic            as G
import           Data.Vector.Generic.Lens       (toVectorOf)
import qualified Data.Vector.Generic.Mutable    as GM
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Storable           as S
import qualified Data.Vector.Unboxed            as U
import           Linear                         hiding (vector)

import           Data.Shaped.Base
import           Data.Shaped.Index
import           Data.Shaped.Mutable            (MArray (..))
import qualified Data.Shaped.Mutable            as M

import           Prelude                        hiding (null, replicate,
                                                 zipWith, zipWith3)

-- Aliases -------------------------------------------------------------

-- | Boxed array.
type BArray = Array B.Vector

-- | 'Unboxed' array.
type UArray = Array U.Vector

-- | 'Storeable' array.
type SArray = Array S.Vector

-- | 'Primitive' array.
type PArray = Array P.Vector

-- Lenses --------------------------------------------------------------

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape l, Vector v a, Vector v b)
       => IndexedTraversal (l Int) (Array v l a) (Array v l b) a b
values' = values
{-# INLINE values' #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in 'Data.Vector.Generic' on them without
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
fromListInto :: (Shape l, Vector v a) => l Int -> [a] -> Maybe (Array v l a)
fromListInto l as
  | G.length v == n = Just $ Array l v
  | otherwise       = Nothing
  where v = G.fromListN n as
        n = F.product l
{-# INLINE fromListInto #-}

-- | O(n) Convert the first @n@ elements of a list to an Array with the
--   given shape. Throw an error if the list is not long enough.
fromListInto_ :: (Shape l, Vector v a) => l Int -> [a] -> Array v l a
fromListInto_ l as = fromMaybe err $ fromListInto l as
  where
    err = error $ "fromListInto_: shape " ++ showShape l ++ " is too large for list"
{-# INLINE fromListInto_ #-}

-- | The empty 'Array' with a 'zero' shape.
empty :: (Vector v a, Additive l) => Array v l a
empty = Array zero G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: Foldable l => Array v l a -> Bool
null (Array l _) = F.all (==0) l
{-# INLINE null #-}

-- Indexing ------------------------------------------------------------

-- | Index an element of an array. Throws 'IndexOutOfBounds' if the
--   index is out of bounds.
(!) :: (Shape l, Vector v a) => Array v l a -> l Int -> a
Array l v ! i = boundsCheck l i $ G.unsafeIndex v (toIndex l i)
{-# INLINE (!) #-}

-- | Safe index of an element.
(!?) :: (Shape l, Vector v a) => Array v l a -> l Int -> Maybe a
Array l v !? i
  | inRange l i = Just $! G.unsafeIndex v (toIndex l i)
  | otherwise   = Nothing
{-# INLINE (!?) #-}

-- | Index an element of an array without bounds checking.
unsafeIndex :: (Shape l, Vector v a) => l Int -> Array v l a -> a
unsafeIndex i (Array l v) = G.unsafeIndex v (toIndex l i)
{-# INLINE unsafeIndex #-}

-- | Index an element of an array while ignoring its shape.
linearIndex :: Vector v a => Int -> Array v l a -> a
linearIndex i (Array _ v) = v G.! i
{-# INLINE linearIndex #-}

-- | Index an element of an array while ignoring its shape, without
--   bounds checking.
unsafeLinearIndex :: Vector v a => Int -> Array v l a -> a
unsafeLinearIndex i (Array _ v) = G.unsafeIndex v i
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
indexM :: (Shape l, Vector v a, Monad m) => Array v l a -> l Int -> m a
indexM (Array l v) i = boundsCheck l i $ G.unsafeIndexM v (toIndex l i)
{-# INLINE indexM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeIndexM :: (Shape l, Vector v a, Monad m) => Array v l a -> l Int -> m a
unsafeIndexM (Array l v) i = G.unsafeIndexM v (toIndex l i)
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ Indexing in a monad. Throws an error if the index is out of
--   range.
linearIndexM :: (Shape l, Vector v a, Monad m) => Array v l a -> Int -> m a
linearIndexM (Array l v) i = boundsCheck l (fromIndex l i) $ G.unsafeIndexM v i

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
--   explanation of why this is useful.
unsafeLinearIndexM :: (Vector v a, Monad m) => Array v l a -> Int -> m a
unsafeLinearIndexM (Array _ v) = G.unsafeIndexM v
{-# INLINE unsafeLinearIndexM #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: (Vector v a, Shape l)
       => (forall s. ST s (MArray (G.Mutable v) l s a)) -> Array v l a
create m = m `seq` runST (m >>= unsafeFreeze)
{-# INLINE create #-}

-- | O(n) Array of the given shape with the same value in each position.
replicate :: (Shape l, Vector v a) => l Int -> a -> Array v l a
replicate l a
  | n > 0     = Array l $ G.replicate n a
  | otherwise = empty
  where n = F.product l
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
linearGenerate :: (Shape l, Vector v a) => l Int -> (Int -> a) -> Array v l a
linearGenerate l f
  | n > 0     = Array l $ G.generate n f
  | otherwise = empty
  where n = F.product l
{-# INLINE linearGenerate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: (Shape l, Vector v a) => l Int -> (l Int -> a) -> Array v l a
generate l f
  | n > 0     = Array l $ G.generate n (f . fromIndex l)
  | otherwise = empty
  where n = F.product l
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape l, Vector v a) => l Int -> m a -> m (Array v l a)
replicateM l a
  | n > 0     = Array l `liftM` G.replicateM n a
  | otherwise = return empty
  where n = F.product l
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape l, Vector v a) => l Int -> (l Int -> m a) -> m (Array v l a)
generateM l f
  | n > 0     = Array l `liftM` G.generateM n (f . fromIndex l)
  | otherwise = return empty
  where n = F.product l
{-# INLINE generateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
linearGenerateM :: (Monad m, Shape l, Vector v a) => l Int -> (Int -> m a) -> m (Array v l a)
linearGenerateM l f
  | n > 0     = Array l `liftM` G.generateM n f
  | otherwise = return empty
  where n = F.product l
{-# INLINE linearGenerateM #-}

-- | For each pair (i,a) from the list, replace the array element at
--   position i by a.
(//) :: (G.Vector v a, Shape l) => Array v l a -> [(l Int, a)] -> Array v l a
Array l v // xs = Array l $ v G.// over (each . _1) (toIndex l) xs

------------------------------------------------------------------------
-- Streams
------------------------------------------------------------------------

streamSub :: (Shape l, Vector v a) => l Int -> Array v l a -> Bundle v a
streamSub l2 (Array l1 v) | eq1 l1 l2 = G.stream v
streamSub l2 (Array l1 v)             = Bundle.unfoldr get 0 `Bundle.sized` Exact n
  where
    n = F.product l2

    get i | i >= n    = Nothing
          | otherwise = case G.basicUnsafeIndexM v (toIndex l1 j) of Box x -> Just (x, i+1)
      where j = fromIndex l2 i

streamShape :: Shape l => l Int -> Bundle v (l Int)
streamShape l = Bundle.fromListN (F.product l) $ toListOf enumShape l
{-# INLINE streamShape #-}

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- Tuple zip -----------------------------------------------------------

-- | Zip two arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip :: (Shape l, Vector v a, Vector v b, Vector v (a,b))
    => Array v l a
    -> Array v l b
    -> Array v l (a,b)
zip = zipWith (,)

-- | Zip three arrays element wise. If the array's don't have the same
--   shape, the new array with be the intersection of the two shapes.
zip3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v (a,b,c))
     => Array v l a
     -> Array v l b
     -> Array v l c
     -> Array v l (a,b,c)
zip3 = zipWith3 (,,)

-- Zip with function ---------------------------------------------------

-- | Zip two arrays using the given function. If the array's don't have
--   the same shape, the new array with be the intersection of the two
--   shapes.
zipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
        => (a -> b -> c)
        -> Array v l a
        -> Array v l b
        -> Array v l c
zipWith f a1@(Array l1 v1) a2@(Array l2 v2)
  | eq1 l1 l1 = Array l1 $ G.zipWith f v1 v2
  | otherwise = Array l' $ G.unstream $ Bundle.zipWith f (streamSub l' a1) (streamSub l' a2)
  where l' = intersectShape l1 l2

-- | Zip three arrays using the given function. If the array's don't
--   have the same shape, the new array with be the intersection of the
--   two shapes.
zipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d)
         -> Array v l a
         -> Array v l b
         -> Array v l c
         -> Array v l d
zipWith3 f a1@(Array l1 v1) a2@(Array l2 v2) a3@(Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.zipWith3 f v1 v2 v3
  | otherwise = Array l' $ G.unstream $ Bundle.zipWith3 f (streamSub l' a1) (streamSub l' a2) (streamSub l' a3)
  where l' = intersectShape (intersectShape l1 l2) l3

-- Indexed zipping -----------------------------------------------------

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
         => (l Int -> a -> b -> c)
         -> Array v l a
         -> Array v l b
         -> Array v l c
izipWith f a1@(Array l1 v1) a2@(Array l2 v2)
  | eq1 l1 l2 = Array l1 $ G.unstream $ Bundle.zipWith3 f (streamShape l1) (G.stream v1) (G.stream v2)
  | otherwise = Array l' $ G.unstream $ Bundle.zipWith3 f (streamShape l') (streamSub l' a1) (streamSub l' a2)
  where l' = intersectShape l1 l2
{-# INLINE izipWith #-}

-- | Zip two arrays using the given function with access to the index.
--   If the array's don't have the same shape, the new array with be the
--   intersection of the two shapes.
izipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
          => (l Int -> a -> b -> c -> d)
          -> Array v l a
          -> Array v l b
          -> Array v l c
          -> Array v l d
izipWith3 f a1@(Array l1 v1) a2@(Array l2 v2) a3@(Array l3 v3)
  | eq1 l1 l2 = Array l1 $ G.unstream $ Bundle.zipWith4 f (streamShape l1) (G.stream v1) (G.stream v2) (G.stream v3)
  | otherwise = l' `seq`
      Array l' (G.unstream $ Bundle.zipWith4 f (streamShape l') (streamSub l' a1) (streamSub l' a2) (streamSub l' a3))
  where l' = intersectShape (intersectShape l1 l2) l3
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
  where x' = minimum $ map G.length vs
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
    is = toListOf (cloneLens l32 . enumShape . to (\x -> toIndex l $ pure i & l32 #~ x)) l
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

-- Ordinals ------------------------------------------------------------

-- | This 'Traversal' should not have any duplicates in the list of
--   indices.
unsafeOrdinals :: (Vector v a, Shape l) => [l Int] -> IndexedTraversal' (l Int) (Array v l a) a
unsafeOrdinals is f (Array l v) = Array l . (v G.//) <$> traverse g is
  where g x = let i = toIndex l x in (,) i <$> indexed f x (G.unsafeIndex v i)
{-# INLINE [0] unsafeOrdinals #-}

setOrdinals :: (Indexable (l Int) p, Vector v a, Shape l) => [l Int] -> p a a -> Array v l a -> Array v l a
setOrdinals is f (Array l v) = Array l $ G.unsafeUpd v (map g is)
  where g x = let i = toIndex l x in (,) i $ indexed f x (G.unsafeIndex v i)
{-# INLINE setOrdinals #-}

{-# RULES
"unsafeOrdinals/setOrdinals" forall (is :: [l Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape l) => ASetter' (Array v l a) a;
"unsafeOrdinalts/isetOrdintals" forall (is :: [l Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape l) => AnIndexedSetter' (l Int) (Array v l a) a
 #-}

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | Isomorphism between an array and it's delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Vector v a, Vector w b, Shape l, Shape k)
        => Iso (Array v l a) (Array w k b) (Delayed l a) (Delayed k b)
delayed = iso delay manifest
{-# INLINE delayed #-}

-- | Sequential manifestation of a delayed array.
manifestS :: (Vector v a , Shape l) => Delayed l a -> Array v l a
manifestS arr@(Delayed l _) = Array l (toVectorOf folded arr)
{-# INLINE manifestS #-}

------------------------------------------------------------------------
-- Focused
------------------------------------------------------------------------

-- | Focus on a particular element of a delayed array.
focusOn :: l Int -> Delayed l a -> Focused l a
focusOn = Focused -- XXX do range checking
{-# INLINE focusOn #-}

-- | Discard the focus to retrieve the delayed array.
unfocus :: Focused l a -> Delayed l a
unfocus (Focused _ d) = d
{-# INLINE unfocus #-}

-- | Indexed lens onto the delayed array, indexed with the focus.
unfocused :: IndexedLens (l Int) (Focused l a) (Focused l b) (Delayed l a) (Delayed l b)
unfocused f (Focused x d) = Focused x <$> indexed f x d
{-# INLINE unfocused #-}

-- | Lens onto the position of store.
--
-- @
-- 'locale' :: 'Lens'' ('Focused' l a) (l 'Int')
-- @
locale :: ComonadStore s w => Lens' (w a) s
locale f w = (`seek` w) <$> f (pos w)
{-# INLINE locale #-}

-- | Focus on a neighbouring element, relative to the current focus.
shiftFocus :: Applicative l => l Int -> Focused l a -> Focused l a
shiftFocus dx (Focused x d@(Delayed l _)) = Focused x' d
  where
    x' = f <$> l <*> x <*> dx
    f k i di
      | i' < 0    = k + i'
      | i' >= k   = i' - k
      | otherwise = i'
      where i' = i + di
{-# INLINE shiftFocus #-}

