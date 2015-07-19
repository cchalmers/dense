{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
-- equivilant to "vector"'s 'Data.Vector.Generic' module.
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

    -- * Lenses
  , layout
  , linear
  , values
  , values'

  -- * Construction

  -- ** Flat arrays
  , _Flat
  , fromList

  -- ** Shaped lists
  , fromListInto
  , fromListInto_

  -- ** Initialisation
  , replicate
  , generate

  -- ** Monadic initialisation
  , create
  , replicateM
  , generateM

  -- * Functions on arrays

  -- ** Empty arrays
  , empty
  , null

  -- ** Modifying arrays
  , (//)

  -- ** Zipping
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3

  -- ** Slices

  -- , slice
  , sliced
  , line
  , unsafeOrdinals
  , plane

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
  ) where


#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative      (Applicative, (<*>))
import           Data.Foldable            (Foldable)
#endif

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad            (liftM)
import           Control.Monad.ST
import qualified Data.Foldable            as F
import           Data.Functor.Classes
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector              as B
import           Data.Vector.Generic      (Vector)
import qualified Data.Vector.Generic      as G
import           Data.Vector.Generic.Lens (toVectorOf)
import qualified Data.Vector.Primitive    as P
import qualified Data.Vector.Storable     as S
import qualified Data.Vector.Unboxed      as U
import           Linear

import           Data.Shaped.Base
import           Data.Shaped.Index
import           Data.Shaped.Mutable      (MArray (..))
import qualified Data.Shaped.Mutable      as M

import           Prelude                  hiding (null, replicate, zipWith,
                                           zipWith3)

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
_Flat :: (Vector v a, Vector w b) => Iso (Array v V1 a) (Array w V1 b) (v a) (w b)
_Flat = iso (\(Array _ v) -> v) (\v -> Array (V1 $ G.length v) v)
{-# INLINE _Flat #-}

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
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto_ :: (Shape l, Vector v a) => l Int -> [a] -> Array v l a
fromListInto_ l as = fromMaybe err $ fromListInto l as
  where
    err = error $ "fromListInto_: shape " ++ showShape l ++ " is too large for list"
{-# INLINE fromListInto_ #-}

-- | The empty 'Array' with a 'zeroDim' shape.
empty :: (Vector v a, Additive l) => Array v l a
empty = Array zero G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: Foldable l => Array v l a -> Bool
null (Array l _) = F.all (==0) l
{-# INLINE null #-}

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

-- | For each pair (i,a) from the list, replace the vector element at
--   position i by a.
(//) :: (G.Vector v a, Shape l) => Array v l a -> [(l Int, a)] -> Array v l a
Array l v // xs = Array l $ v G.// over (each . _1) (toIndex l) xs

-- Zipping -------------------------------------------------------------

zipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
        => (a -> b -> c)
        -> Array v l a
        -> Array v l b
        -> Array v l c
zipWith f (Array l1 v1) (Array l2 v2)
  | eq1 l1 l1 = Array l1 $ G.zipWith f v1 v2
  | otherwise = Array l' $ error "intersect zipWith not yet implimented"
  where l' = intersectShape l1 l2

zipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d)
         -> Array v l a
         -> Array v l b
         -> Array v l c
         -> Array v l d
zipWith3 f (Array l1 v1) (Array l2 v2) (Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.zipWith3 f v1 v2 v3
  | otherwise = Array l' $ error "intersect zipWith not yet implimented"
  where l' = intersectShape l1 l2 `intersectShape` l3


izipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
         => (l Int -> a -> b -> c)
         -> Array v l a
         -> Array v l b
         -> Array v l c
izipWith f (Array l1 v1) (Array l2 v2)
  | eq1 l1 l2 = Array l1 $ G.izipWith (f . fromIndex l1) v1 v2
  | otherwise = error "izipWith not yet implimented for different shaped arrays"
{-# INLINE izipWith #-}

izipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
          => (l Int -> a -> b -> c -> d)
          -> Array v l a
          -> Array v l b
          -> Array v l c
          -> Array v l d
izipWith3 f (Array l1 v1) (Array l2 v2) (Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.izipWith3 (f . fromIndex l1) v1 v2 v3
  | otherwise = error "izipWith3 not yet implimented for different shaped arrays"
{-# INLINE izipWith3 #-}

-- Slices --------------------------------------------------------------

-- -- XXX Needs to be checked

-- Index starts at l1.
-- slicedValues :: l Int -> l Int -> IndexedTraversal (l Int) (Array v l a) a
-- sliced

-- Index starts at 0
-- dicedValues :: l Int -> l Int -> IndexedTraversal (l Int) (Array v l a) a

sliced :: (Vector v a, Shape l1, Shape l2)
      => Getting (l2 Int) (l1 Int) (l2 Int)
      -> (l2 Int -> l1 Int)
      -> Lens' (Array v l1 a) (Array v l2 a)
sliced = undefined

sliced'
  :: (Vector v a, Shape l1, Shape l2)
  => (l1 Int -> l2 Int)
  -> (l2 Int -> l1 Int)
  -> Lens' (Array v l1 a) (Array v l2 a)
sliced' f12 f21 f arr@(Array l1 _) =
  f a <&> \arr' -> arr // (arr' ^@.. reindexed f21 values)
  where a  = generate (f12 l1) $ \x -> arr ^?! ix (f21 x)

slice :: (Vector v a, Vector w a, Shape l1, Shape l2)
      => Getting (l2 Int) (l1 Int) (l2 Int)
      -> (l2 Int -> l1 Int)
      -> Array v l1 a
      -> Array w l2 a
slice l f arr@(Array l1 _) = generate l2 $ \x -> arr ^?! ix (f x) where l2 = l1 ^. l

-- column :: Vector v a => Int -> Array v V2 a -> Array v V1 a
-- column y = slice (_x . to V1) (\(V1 x) -> V2 x y)

-- | 'row' :: Lens' (l Int) Int -> l Int -> Array v l a -> Array v V1 a
--
--
-- row :: Vector v a => Getting (l Int) Int (l Int) -> l Int -> Lens' (Array v l a) (Array v V1 a)
-- row g l0 f arr@(Array l _) = f v1 <&> \v1' -> --
--   where
--     v1 = generate (l ^. g) $ \x -> arr ^?! ix (x ^.)
line :: (Vector v a, Shape l)
     => Lens' (l Int) Int -- ^ Lens onto target line
     -> l Int             -- ^ Some point on the line
     -> Lens' (Array v l a) (Array v V1 a) -- Lens onto the line
line l = line' l l
{-# INLINE line #-}

line' :: (Vector v a, Shape l)
      => Getting (V1 Int) (l Int) Int
      -> ASetter' (l Int) Int
      -> l Int
      -> Lens' (Array v l a) (Array v V1 a)
line' g s l = sliced' (view (g . to V1)) (\(V1 x) -> l & s .~ x)
{-# INLINE line' #-}

-- line :: (Vector v a, Shape l) => Lens' (l Int) Int -> l Int -> IndexedTraversal' (l Int) (Array v l a) a

-- planes :: (Vector v a, Shape l) => Lens' (l Int) Int -> l Int -> IndexedTraversal' Int (Array v V3 a) (Array v V2 a)

-- row :: (Vector v a, Shape l) => ALens' (l Int) Int -> Int -> Lens' (Array v l a) (Array v V1 a)
-- row l x = sliced (cloneLens l . to V1) (\(V1 x) -> xx & l #~ x)
--   where xx = x <$ zero

plane :: (Vector v a, Vector w a)
      => ALens' (V3 Int) (V2 Int) -- Lens onto plane
      -> Int -- number of plane
      -> Array v V3 a
      -> Array w V2 a -- Lens' (V3 Int) (V2 Int) =>
plane l n = slice getter (\xx -> x & l #~ xx)
  where x      = n <$ (zero :: Additive f => f Int)
        getter = cloneLens l

-- plane :: (Vector v a, Vector w a)
--       => Lens' (V3 Int) (V2 Int) -- Lens onto plane
--       -> Int -- number of plane
--       -> Array v V3 a
--       -> Array w V2 a -- Lens' (V3 Int) (V2 Int) =>
-- plane l n = slice getter (\xx -> x & l #~ xx)
--   where x      = n <$ (zero :: Additive f => f Int)
--         getter = cloneLens l


-- | This 'Traversal' will ignore any duplicates in the supplied list
--   of indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
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

