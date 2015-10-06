-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Slice
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides functions for working with slices of arrays.
-----------------------------------------------------------------------------
module Data.Shaped.Slice where


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

import Data.Shaped.Base
import Control.Lens

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


