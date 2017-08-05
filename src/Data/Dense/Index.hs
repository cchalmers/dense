{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Dense.Index
  ( Shape(..)
  , Peano(..)
  , Countable(..)
  , ix1
  , ix2
  , ix3
  , ix4
  , ix5
  -- , shapeToAddress
  -- , shapeFromAddress
  -- , revVV
  -- , shapeV3

  , Shaped (..)
  , Strided (..)
  , Layout (..)
  , HasLayout (..)
  , size
  , extent
  , indexes
  , layoutIndexes
  , boundsCheck
  , sizeMissmatch

  , StridedLayout (..)
  , showShape
  ) where

import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens.Internal.Getter (noEffect)


import Data.Data
import Linear
import Control.Lens hiding (Index)
import Data.Functor.Classes
import qualified Data.Foldable as F
import qualified Control.Applicative as A
import qualified Data.Traversable as T
import qualified Data.Monoid as Monoid
import Data.Constraint
import qualified Foreign.Storable  as Store
import qualified Foreign.Ptr as Ptr

import Data.Dense.Nat
import Data.Dense.Shape

-- import Data.Dense.Index

guardPure :: A.Alternative f => (a -> Bool) -> a -> f a
guardPure p a = if p a then pure a else A.empty
{-# INLINE guardPure #-}



data GDSlice (from :: Nat) (to :: Nat) :: * where
  GDNil   :: GDSlice 'Z 'Z

  GDPick  :: !Int -- index to take in this dimension
          -> !(GDSlice from to)
          -> GDSlice ('S from) to

  GDRange :: (Int -- start
             ,Int -- step
             ,Int) -- num elements
          -> (GDSlice from to)
          -> GDSlice ('S from) ('S to)

  GDAll   :: (GDSlice from to)
          -> GDSlice ('S from) ('S to)

instance Show (GDSlice n n') where
  showsPrec p s = case s of
    GDNil       -> showString "GDNil"
    GDPick i s' -> showParen (p > 10) $
      showString "GDPick " . showsPrec 11 i . showChar ' ' . showsPrec 11 s'
    GDRange i s' -> showParen (p > 10) $
      showString "GDRange " . showsPrec 11 i . showChar ' ' . showsPrec 11 s'
    GDAll s' -> showParen (p > 10) $
      showString "GDAll " . showsPrec 11 s'


gdSlice :: GDSlice n n' -> Strided (Shape n) Int -> (Strided (Shape n') Int, Int)
gdSlice GDNil (Strided Nil Nil Nil) = (Strided Nil Nil Nil, 0)
gdSlice sl (Strided (m:*ms) (e:*es) (s:*ss)) =
  case sl of

    GDPick x sl' ->
      let (sl'', off) = gdSlice sl' ls
      in  (sl'', off + s*(x - m))

    GDRange (m',dx,n) sl' ->
      let (Strided ms' es' ss', off) = gdSlice sl' ls
      in  (Strided (m' :* ms') (n :* es') (s*dx :* ss'), off + s * (m' - m))

    GDAll sl' ->
      let (Strided ms' es' ss', off) = gdSlice sl' ls
      in  (Strided (m :* ms') (e :* es') (s :* ss'), off)

  where
    ls = Strided ms es ss

-- | Size of the array that will be be enough to accomidate the extent
--   of the layout while still being a multiple of the largest stride.
--
--   (I don't know if this is actually useful)
-- shSizeAligned :: Strided (Shape n) Int -> Int
-- shSizeAligned (Strided (m:*ms) (e:*es) (s:*ss)) = max (e*s) (shSizeAligned (Strided ms es ss))
-- shSizeAligned _ = 0

--computeSlicePlan:: GDSlice from to -> Shape from Int -> Shape from (Either Int (AffineRange Int))
--computeSlicePlan GDNil  Nil = Nil
--computeSlicePlan  ( ix `GDPick` gdRest )
--                  (bd:* shpRest)| ix < bd   && ix >= 0 = Left ix :* computeSlicePlan gdRest shpRest
--                      | otherwise = error
--                          $ "bad indices for computeSlicePlan " ++ show (ix,bd)
--computeSlicePlan ( (strt,step,end) `GDRange` grest) (bd:* shprest)



-- class StridedLayout f where
--   strided

------------------------------------------------------------------------
-- Strided layout
------------------------------------------------------------------------

-- | A strided layout.
data Strided f a = Strided
  { sliceMin    :: !(f a)
  , sliceExtent :: !(f a)
  , sliceStride :: !(f a)
  } deriving Show

instance Eq1 f => Eq1 (Strided f) where
  liftEq = \f (Strided m1 e1 s1) (Strided m2 e2 s2)
    -> liftEq f m1 m2 && liftEq f e1 e2 && liftEq f s1 s2
  {-# INLINE liftEq #-}

class (Shaped f, Eq1 f, Additive f, Traversable f) => StridedLayout f where
  stridedIndexToOffset   :: Strided f Int -> f Int -> Int
  stridedIndexFromOffset :: Strided f Int -> Int -> f Int
  stridedLayoutIntersect :: Strided f Int -> Strided f Int -> Strided f Int
  stridedUnsafeStepIndex :: Strided f Int -> f Int -> f Int
  stridedStepIndex       :: Strided f Int -> f Int -> Maybe (f Int)
  stridedIndexInRange    :: Strided f Int -> f Int -> Bool
  stridedLayoutLength    :: Strided f Int -> Int
  stridedLayoutSize      :: Strided f Int -> Int
  stridedEmptyLayout     :: Strided f Int

class StridedSh (n :: Nat) where
  stridedShToOffset   :: Strided (Shape n) Int -> Shape f Int -> Int
  stridedShFromOffset :: Strided (Shape n) Int -> Int -> (Int, Shape n Int)
  stridedShIntersect  :: Strided (Shape n) Int -> Strided (Shape n) Int -> Strided (Shape n) Int
  stridedUnsafeStepSh :: Strided (Shape n) Int -> Shape n Int -> Shape n Int
  stridedStepSh       :: Strided (Shape n) Int -> Shape n Int -> Maybe (Shape n Int)
  stridedShInRange    :: Strided (Shape n) Int -> Shape n Int -> Bool
  stridedShLength     :: Strided (Shape n) Int -> Int
  stridedShSize       :: Strided (Shape n) Int -> Int

instance StridedSh 'Z where
  stridedShToOffset _ _   = 0
  stridedShFromOffset _ o = (o, Nil)
  stridedShIntersect _ _ = Strided Nil Nil Nil
  stridedUnsafeStepSh _ _ = Nil
  stridedStepSh _ _       = Nothing
  stridedShInRange _ _    = True
  stridedShLength _       = 0
  stridedShSize _         = 0

instance StridedSh n => StridedSh ('S n) where
  stridedShToOffset (Strided (m:*ms) (e:*es) (s:*ss)) (i :* is) =
    s*(i - m) + stridedShToOffset (Strided ms es ss) is
  stridedShFromOffset (Strided (m:*ms) (_:*es) (s:*ss)) o =
    let (o',sh) = stridedShFromOffset (Strided ms es ss) o
        (i,q)   = o' `quotRem` s
    in  (q, m+i :* sh)
  stridedShIntersect
    (Strided (m1:*ms1) (e1:*es1) (s1:*ss1))
    (Strided (m2:*ms2) (e2:*es2) (s2:*ss2))
    =
      let Strided ms' es' ss' =
            stridedShIntersect
              (Strided ms1 es1 ss1)
              (Strided ms2 es2 ss2)

          -- s' = lcm s1 s2
          -- m' = h

      in  Strided
            (max m1 2 :* ms')
            -- Not quite right, lower shape stride may not touch min of
            -- other.
            (max 0 (min e1 e2) :* es')
            (lcm s1 s2 :* ss')

  stridedUnsafeStepSh (Strided (m:*ms) (e:*es) (s:*ss)) (a:*as)
    | a - m < e = a + 1 :* as
    | otherwise = m :* stridedUnsafeStepSh (Strided ms es ss) as

  stridedStepSh (Strided (m:*ms) (e:*es) (s:*ss)) (a:*as)
    | a - m < e = Just (a + 1 :* as)
    | otherwise = case stridedStepSh (Strided ms es ss) as of
                    Just sh' -> Just (m :* sh')
                    Nothing  -> Nothing

  stridedShInRange (Strided (m:*ms) (e:*es) (s:*ss)) (a:*as) =
    a >= m && a < m + e && stridedShInRange (Strided ms es ss) as
  -- stridedShLength (Strided _ (e:es) _) = F.product es
  stridedShSize (Strided (m:*ms) (e:*es) (s:*ss)) = s*(e-1) + stridedShSize (Strided ms es ss)
  stridedShLength (Strided (m:*ms) (e:*es) (s:*ss)) = e * stridedShLength (Strided ms es ss)

instance Countable (NatDict StridedSh) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict

instance Peano n => StridedLayout (Shape n) where
  stridedIndexToOffset   = stridedShToOffset \\ countDict @StridedSh @n
  stridedIndexFromOffset = \l o -> snd $ stridedShFromOffset l o \\ countDict @StridedSh @n
  stridedLayoutIntersect = stridedShIntersect \\ countDict @StridedSh @n
  stridedUnsafeStepIndex = stridedUnsafeStepSh \\ countDict @StridedSh @n
  stridedStepIndex       = stridedStepSh \\ countDict @StridedSh @n
  stridedIndexInRange    = stridedShInRange \\ countDict @StridedSh @n
  stridedLayoutLength    = \(Strided _ es _) -> F.product es
  stridedLayoutSize      = stridedShSize \\ countDict @StridedSh @n
  stridedEmptyLayout     = Strided zero zero zero

-- | Find the first intersection of two arithmetic sequences, if it
--   exists.
firstIntersection :: (Int,Int) -> (Int,Int) -> Maybe Int
firstIntersection (a1, da) (b1, db)
  | ((a1 - b1) `mod` g) /= 0 = Nothing
  -- _ | not (gcd da db `divides` (a1 - b1)) = Nothing
  | otherwise              = Just (a1 + (n-1)*da)
  -- _ | otherwise = Just (b1 + (m-1)*db) -- (a1 + n*da)
  where
    (g,u,v) = egcd (-da) db
    c = a1 - b1 + db - da
    cdivg = c `div` g
    -- t = max (- (c `div` db) * u + 1) (- (c `div` da)*v + 1)
    -- should it be this?
    t = max (floor (- (r c / r db) * r u) + 1)
            (floor (- (r c / r da) * r v) + 1)
    r = toRational
    a = -da
    b = db
    a' = a `div` g
    b' = b `div` g
    (n,m) = (cdivg * u + t*b', cdivg * v - t*a')

    -- taken from:
    -- https://math.stackexchange.com/questions/1656120
    -- formula-to-find-the-first-intersection-of-two-arithmetic-progressions

-- If a and b are integers (with a not zero), we say a divides b if
-- there is an integer c such that b = ac.
a `divides` b = b `mod` a == 0

-- eGCD :: Int -> Int -> (Int,Int,Int)
-- eGCD 0 b = (b, 0, 1)
-- eGCD a b = (g, t - (b `div` a) * s, s)
--   where
    -- (g, s, t) = eGCD (b `mod` a) a

egcd :: Integral a => a -> a -> (a, a, a)
egcd a b = (d, u, v)
  where
    (d, x, y) = eGCD 0 1 1 0 (abs a) (abs b)
    u | a < 0     = negate x
      | otherwise = x
    v | b < 0     = negate y
      | otherwise = y
    eGCD !n1 o1 !n2 o2 r s
      | s == 0    = (r, o1, o2)
      | otherwise = case r `quotRem` s of
                      (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t

------------------------------------------------------------------------
-- Layout
------------------------------------------------------------------------

-- | Class for types that can be converted to and from linear indexes.
class (Eq1 f, Eq1 (Index f), Additive (Index f), Traversable (Index f), Shaped (Index f)) => Layout f where
  type Index f :: * -> *

  -- | Convert a shape to its linear index using the 'Layout'.
  indexToOffset :: f Int -> Index f Int -> Int
  -- shapeToOffset l x = F.foldr (\(e, a) k -> a + e*k) 0 (liftI2 (,) l x)
  -- {-# INLINE shapeToIndex #-}

  -- | Convert a linear index to a shape the 'Layout'.
  indexFromOffset :: f Int -> Int -> Index f Int
  -- shapeFromOffset l i = snd $ mapAccumL quotRem i l
  -- {-# INLINE shapeFromIndex #-}

  -- | Calculate the intersection of two shapes.
  layoutIntersect :: f Int -> f Int -> f Int
  -- layoutIntersect = liftU2 min
  -- {-# INLINE shapeIntersect #-}

  layoutStride :: f Int -> Index f Int

  -- | Increment a shape by one. It is assumed that the provided index
  --   is 'inRange'.
  unsafeStepIndex :: f Int -> Index f Int -> Index f Int
  -- unsafeShapeStep l
  --   = shapeFromIndex l
  --   . (+1)
  --   . shapeToIndex l
  -- {-# INLINE unsafeShapeStep #-}

  -- | Increment a shape by one. It is assumed that the provided index
  --   is 'indexInRange'.
  stepIndex :: f Int -> Index f Int -> Maybe (Index f Int)
  -- shapeStep l = fmap (shapeFromIndex l)
  --             . guardPure (< shapeSize l)
  --             . (+1)
  --             . shapeToIndex l
  -- {-# INLINE shapeStep #-}

  -- | Increment a shape by one between the two bounds
  -- shapeStepBetween :: Index f Int -> f -> Index f Int -> Maybe (Index f Int)
  -- shapeStepBetween a l = fmap (^+^ a) . shapeStep l . (^-^ a)
  -- {-# INLINE shapeStepBetween #-}

  -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
  indexInRange :: f Int -> Index f Int -> Bool
  -- shapeInRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l
  -- {-# INLINE shapeInRange #-}

  -- | The extent of the layout. The number of elements in each
  --   dimension.
  layoutExtent :: f Int -> Index f Int

  -- | The number of accessible elements in a layout.
  layoutLength :: f Int -> Int

  -- | The smallest size of the array needed for the last element to be in
  --   the array. This may be larger than the 'layoutLength' if the
  --   layout is strided.
  layoutSize :: f Int -> Int
  -- shapeSize = F.product
  -- {-# INLINE shapeSize #-}

  emptyLayout :: f Int

instance StridedLayout f => Layout (Strided f) where
  type Index (Strided f) = f
  indexToOffset = stridedIndexToOffset
  indexFromOffset = stridedIndexFromOffset
  layoutIntersect = stridedLayoutIntersect
  layoutStride = \(Strided _ _ s) -> s
  unsafeStepIndex = stridedUnsafeStepIndex
  stepIndex = stridedStepIndex
  indexInRange = stridedIndexInRange
  layoutExtent = \(Strided _ e _) -> e
  layoutLength = stridedLayoutLength
  layoutSize = stridedLayoutSize
  emptyLayout = stridedEmptyLayout

instance Layout V1 where
  type Index V1 = V1
  indexToOffset = \_ (V1 i) -> i
  indexFromOffset = \_ i -> V1 i
  layoutIntersect = min
  stepIndex = \l -> guardPure (indexInRange l) . (+1)
  -- shapeStepBetween _a b i = guardPure (> b) i'
  --   where i' = i + 1
  indexInRange = \m i -> i >= 0 && i < m
  layoutExtent = id
  layoutStride = \_ -> 1
  unsafeStepIndex = \_ (V1 a) -> V1 (a+1)
  layoutLength = \(V1 n) -> n
  layoutSize = \(V1 n) -> n
  emptyLayout = V1 0
  -- stridedShLength = \_

instance Layout V2 where
  type Index V2 = V2
  indexToOffset (V2 x _y) (V2 i j) = i + x*j

  indexFromOffset (V2 x _y) n = V2 i j
    where (j, i) = n `quotRem` x

  stepIndex (V2 x y) (V2 i j)
    | i + 1 < x  = Just (V2 (i + 1)  j     )
    | j + 1 < y  = Just (V2      0  (j + 1))
    | otherwise  = Nothing

  unsafeStepIndex (V2 x _y) (V2 i j)
    | i + 1 < x  = V2 (i + 1)  j
    | otherwise  = V2      0  (j + 1)

  -- shapeStepBetween (V2 ia _ja) (V2 ib jb) (V2 i j)
  --   | i + 1 < ib = Just (V2 (i + 1)   j     )
  --   | j + 1 < jb = Just (V2      ia  (j + 1))
  --   | otherwise  = Nothing
  -- {-# INLINE shapeStepBetween #-}

  layoutExtent = id
  layoutStride (V2 x _) = V2 1 x

  layoutIntersect = \(V2 a1 b1) (V2 a2 b2) -> V2 (min a1 a2) (min a2 b2)
  indexInRange = \(V2 a b) (V2 i j) -> i < a && j < b
  layoutLength = \(V2 a b) -> a*b
  layoutSize = \(V2 a b) -> a*b
  emptyLayout = V2 0 0


-- instance Layout V3 where
--   type Index V3 = V3
--   indexToOffset (V3 x y _z) (V3 i j k) = i + x*(j + y*k)

--   stepIndex (V3 x y z) (V3 i j k)
--     | k + 1 < z  = Just (V3 (i + 1)      j       k )
--     | j + 1 < y  = Just (V3      0  (j + 1)      k )
--     | i + 1 < x  = Just (V3      0       0  (k + 1))
--     | otherwise  = Nothing

--   layoutStride = \(V3 x y _) -> V3 1 x y
--   layoutExtent = id

--   -- shapeStepBetween (V3 ia ja _ka) (V3 ib jb kb) (V3 i j k)
--   --   | k < kb  = Just (V3 (i + 1)      j       k )
--   --   | j < jb  = Just (V3     ia  (j + 1)      k )
--   --   | i < ib  = Just (V3     ia      ja  (k + 1))
--   --   | otherwise  = Nothing
--   -- {-# INLINE shapeStepBetween #-}

-- instance Layout V4 where
--   type Index V4 = V4
--   stepIndex (V4 x y z w) (V4 i j k l)
--     | l + 1 < w  = Just (V4 (i + 1)      j       k       l )
--     | k + 1 < z  = Just (V4      0  (j + 1)      k       l )
--     | j + 1 < y  = Just (V4      0       0  (k + 1)      l )
--     | i + 1 < x  = Just (V4      0       0       0  (l + 1))
--     | otherwise  = Nothing

--   -- shapeStepBetween (V4 ia ja ka _la) (V4 ib jb kb lb) (V4 i j k l)
--   --   | l < lb  = Just (V4 (i + 1)      j       k       l )
--   --   | k < kb  = Just (V4     ia  (j + 1)      k       l )
--   --   | j < jb  = Just (V4     ia      ja  (k + 1)      l )
--   --   | i < ib  = Just (V4     ia      ia      ka  (l + 1))
--   --   | otherwise  = Nothing

--   layoutExtent = id
--   layoutStride = \(V4 x y z _) -> V4 1 x y z












-- {-# LANGUAGE DefaultSignatures      #-}
-- {-# LANGUAGE DeriveDataTypeable     #-}
-- {-# LANGUAGE FlexibleContexts       #-}
-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE LambdaCase             #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE Rank2Types             #-}
-- {-# LANGUAGE TypeFamilies           #-}
-- {-# LANGUAGE UndecidableInstances   #-}
-- -----------------------------------------------------------------------------
-- -- |
-- -- Module      :  Data.Dense.Mutable
-- -- Copyright   :  (c) Christopher Chalmers
-- -- License     :  BSD3
-- --
-- -- Maintainer  :  Christopher Chalmers
-- -- Stability   :  provisional
-- -- Portability :  non-portable
-- --
-- -- This module provides a class for types that can be converted to and
-- -- from linear indexes.
-- --
-- -- The default instances are defined in row-major order.
-- -----------------------------------------------------------------------------
-- module Data.Dense.Index
--   ( -- * Shape class
--     Layout
--   , Shape (..)
--   , indexIso
--   , shapeIndexes
--   , shapeIndexesFrom
--   , shapeIndexesBetween

--     -- * HasLayout
--   , HasLayout (..)
--   , extent
--   , size
--   , indexes
--   , indexesBetween
--   , indexesFrom

--     -- * Exceptions

--     -- (* Bounds checking
--   , ArrayException (IndexOutOfBounds)
--   , _IndexOutOfBounds
--   , boundsCheck

--     -- (* Size missmatch
--   , SizeMissmatch (..)
--   , AsSizeMissmatch (..)
--   , sizeMissmatch

--     -- * Utilities
--   , showShape
--   ) where

-- import           Control.Applicative
-- import           Control.Lens
-- import           Control.Lens.Internal.Getter
-- import           Data.Foldable                as F
-- import           Data.Typeable

-- import           Data.Functor.Classes
-- import           Data.Traversable
-- import           Linear

-- -- | A 'Layout' is the full size of an array. This alias is used to help
-- --   distinguish between the layout of an array and an index (usually
-- --   just @l Int@) in a type signature.
-- type Layout f = f Int

-- ------------------------------------------------------------------------
-- -- Shape class
-- ------------------------------------------------------------------------

-- -- | Class for types that can be converted to and from linear indexes.
-- class (Eq1 f, Additive f, Traversable f) => Shape f where
--   -- | Convert a shape to its linear index using the 'Layout'.
--   shapeToIndex :: Layout f -> f Int -> Int
--   shapeToIndex l x = F.foldr (\(e, a) k -> a + e*k) 0 (liftI2 (,) l x)
--   {-# INLINE shapeToIndex #-}

--   -- | Convert a linear index to a shape the 'Layout'.
--   shapeFromIndex :: Layout f -> Int -> f Int
--   shapeFromIndex l i = snd $ mapAccumL quotRem i l
--   {-# INLINE shapeFromIndex #-}

--   -- | Calculate the intersection of two shapes.
--   shapeIntersect :: Layout f -> Layout f -> Layout f
--   shapeIntersect = liftU2 min
--   {-# INLINE shapeIntersect #-}

--   -- | Increment a shape by one. It is assumed that the provided index
--   --   is 'inRange'.
--   unsafeShapeStep :: Layout f -> f Int -> f Int
--   unsafeShapeStep l
--     = shapeFromIndex l
--     . (+1)
--     . shapeToIndex l
--   {-# INLINE unsafeShapeStep #-}

--   -- | Increment a shape by one. It is assumed that the provided index
--   --   is 'inRange'.
--   shapeStep :: Layout f -> f Int -> Maybe (f Int)
--   shapeStep l = fmap (shapeFromIndex l)
--               . guardPure (< shapeSize l)
--               . (+1)
--               . shapeToIndex l
--   {-# INLINE shapeStep #-}

--   -- | Increment a shape by one between the two bounds
--   shapeStepBetween :: f Int -> Layout f -> f Int -> Maybe (f Int)
--   shapeStepBetween a l = fmap (^+^ a) . shapeStep l . (^-^ a)
--   {-# INLINE shapeStepBetween #-}

--   -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
--   shapeInRange :: Layout f -> f Int -> Bool
--   shapeInRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l
--   {-# INLINE shapeInRange #-}

--   -- | The number of elements in a shape.
--   shapeSize :: Layout f -> Int
--   shapeSize = F.product
--   {-# INLINE shapeSize #-}

-- guardPure :: Alternative f => (a -> Bool) -> a -> f a
-- guardPure p a = if p a then pure a else empty
-- {-# INLINE guardPure #-}

-- instance Shape V0

-- instance Shape V1 where
--   {-# INLINE shapeToIndex #-}
--   {-# INLINE shapeFromIndex #-}
--   {-# INLINE shapeIntersect #-}
--   {-# INLINE shapeStep #-}
--   {-# INLINE shapeInRange #-}
--   shapeToIndex _ (V1 i) = i
--   shapeFromIndex _ i = V1 i
--   shapeIntersect = min
--   shapeStep l = guardPure (shapeInRange l) . (+1)
--   shapeStepBetween _a b i = guardPure (> b) i'
--     where i' = i + 1
--   shapeInRange m i = i >= 0 && i < m

-- instance Shape V2 where
--   shapeToIndex (V2 x _y) (V2 i j) = i + x*j
--   {-# INLINE shapeToIndex #-}

--   shapeFromIndex (V2 x _y) n = V2 i j
--     where (j, i) = n `quotRem` x
--   {-# INLINE shapeFromIndex #-}

--   shapeStep (V2 x y) (V2 i j)
--     | i + 1 < x  = Just (V2 (i + 1)  j     )
--     | j + 1 < y  = Just (V2      0  (j + 1))
--     | otherwise  = Nothing
--   {-# INLINE shapeStep #-}

--   unsafeShapeStep (V2 x _y) (V2 i j)
--     | i + 1 < x  = V2 (i + 1)  j
--     | otherwise  = V2      0  (j + 1)
--   {-# INLINE unsafeShapeStep #-}

--   shapeStepBetween (V2 ia _ja) (V2 ib jb) (V2 i j)
--     | i + 1 < ib = Just (V2 (i + 1)   j     )
--     | j + 1 < jb = Just (V2      ia  (j + 1))
--     | otherwise  = Nothing
--   {-# INLINE shapeStepBetween #-}

instance Layout V3 where
  type Index V3 = V3
  -- shapeToIndex (V3 x y _z) (V3 i j k) = i + x*(j + y*k)
  -- {-# INLINE shapeToIndex #-}

  -- shapeStep (V3 x y z) (V3 i j k)
  --   | k + 1 < z  = Just (V3 (i + 1)      j       k )
  --   | j + 1 < y  = Just (V3      0  (j + 1)      k )
  --   | i + 1 < x  = Just (V3      0       0  (k + 1))
  --   | otherwise  = Nothing
  -- {-# INLINE shapeStep #-}

  -- shapeStepBetween (V3 ia ja _ka) (V3 ib jb kb) (V3 i j k)
  --   | k < kb  = Just (V3 (i + 1)      j       k )
  --   | j < jb  = Just (V3     ia  (j + 1)      k )
  --   | i < ib  = Just (V3     ia      ja  (k + 1))
  --   | otherwise  = Nothing
  -- {-# INLINE shapeStepBetween #-}

instance Layout V4 where
  type Index V4 = V4
--   shapeStep (V4 x y z w) (V4 i j k l)
--     | l + 1 < w  = Just (V4 (i + 1)      j       k       l )
--     | k + 1 < z  = Just (V4      0  (j + 1)      k       l )
--     | j + 1 < y  = Just (V4      0       0  (k + 1)      l )
--     | i + 1 < x  = Just (V4      0       0       0  (l + 1))
--     | otherwise  = Nothing
--   {-# INLINE shapeStep #-}

--   shapeStepBetween (V4 ia ja ka _la) (V4 ib jb kb lb) (V4 i j k l)
--     | l < lb  = Just (V4 (i + 1)      j       k       l )
--     | k < kb  = Just (V4     ia  (j + 1)      k       l )
--     | j < jb  = Just (V4     ia      ja  (k + 1)      l )
--     | i < ib  = Just (V4     ia      ia      ka  (l + 1))
--     | otherwise  = Nothing
--   {-# INLINE shapeStepBetween #-}

-- -- instance Dim n => Shape (V n)

-- -- | @'toIndex' l@ and @'fromIndex' l@ form two halfs of an isomorphism.
-- indexIso :: Shape f => Layout f -> Iso' (f Int) Int
-- indexIso l = iso (shapeToIndex l) (shapeFromIndex l)
-- {-# INLINE indexIso #-}

------------------------------------------------------------------------
-- HasLayout
------------------------------------------------------------------------

-- | Class of things that have a 'Layout'. This means we can use the
--   same functions for the various different arrays in the library.
class Layout f => HasLayout f a | a -> f where
  -- | Lens onto the  'Layout' of something.
  layout :: Lens' a (f Int)
  -- default layout :: (a ~ f Int) => (f -> g a) -> a -> g a
  -- layout = id
  -- {-# INLINE layout #-}

-- instance i ~ Int => HasLayout V0 (V0 i) where layout = id
instance i ~ Int => HasLayout V1 (V1 i) where layout = id
instance i ~ Int => HasLayout V2 (V2 i) where layout = id
-- instance i ~ Int => HasLayout V3 (V3 i) where layout = id
-- instance i ~ Int => HasLayout V4 (V4 i) where layout = id

-- | Get the extent of an array.
--
-- @
-- 'extent' :: 'Data.Dense.Base.Array' v f a    -> f 'Int'
-- 'extent' :: 'Data.Dense.Mutable.MArray' v f s a -> f 'Int'
-- 'extent' :: 'Data.Dense.Base.Delayed' f a    -> f 'Int'
-- 'extent' :: 'Data.Dense.Base.Focused' f a    -> f 'Int'
-- @
extent :: HasLayout f a => a -> Index f Int
extent = layoutExtent . view layout
{-# INLINE extent #-}

-- | Get the total number of elements in an array.
--
-- @
-- 'size' :: 'Data.Dense.Base.Array' v f a    -> 'Int'
-- 'size' :: 'Data.Dense.Mutable.MArray' v f s a -> 'Int'
-- 'size' :: 'Data.Dense.Base.Delayed' f a    -> 'Int'
-- 'size' :: 'Data.Dense.Base.Focused' f a    -> 'Int'
-- @
size :: HasLayout f a => a -> Int
size = layoutSize . view layout
{-# INLINE size #-}

-- NB: lens already uses indices so we settle for indexes

-- | Indexed fold for all the indexes in the layout.
indexes :: HasLayout f a => IndexedFold Int a (Index f Int)
indexes = layout . layoutIndexes
{-# INLINE indexes #-}

-- | 'indexes' for a 'Shape'.
layoutIndexes :: Layout f => IndexedFold Int (f Int) (Index f Int)
layoutIndexes g l = go (0::Int) (if layoutLength l == 0 then Nothing else Just zero) where
  go i (Just x) = indexed g i x *> go (i + 1) (stepIndex l x)
  go _ Nothing  = noEffect
{-# INLINE layoutIndexes #-}

-- | Indexed fold starting starting from some point, where the index is
--   the linear index for the original layout.
-- indexesFrom :: HasLayout f a => Index f Int -> IndexedFold Int a (Index f Int)
-- indexesFrom a = layout . shapeIndexesFrom a
-- {-# INLINE indexesFrom #-}

-- | 'indexesFrom' for a 'Shape'.
-- shapeIndexesFrom :: Layout f => Index f Int -> IndexedFold Int f (Index f Int)
-- shapeIndexesFrom a f l = shapeIndexesBetween a l f l
-- {-# INLINE shapeIndexesFrom #-}

-- | Indexed fold between the two indexes where the index is the linear
--   index for the original layout.
-- indexesBetween :: HasLayout f a => f Int -> f Int -> IndexedFold Int a (f Int)
-- indexesBetween a b = layout . shapeIndexesBetween a b
-- {-# INLINE indexesBetween #-}

-- | 'indexesBetween' for a 'Shape'.
-- shapeIndexesBetween :: Layout f => f Int -> f Int -> IndexedFold Int (Layout f) (f Int)
-- shapeIndexesBetween a b f l =
--   go (if eq1 l a || not (shapeInRange l b) then Nothing else Just a) where
--     go (Just x) = indexed f (shapeToIndex l x) x *> go (shapeStepBetween a b x)
--     go Nothing  = noEffect
-- {-# INLINE shapeIndexesBetween #-}

------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

-- Bounds check --------------------------------------------------------

-- | @boundsCheck l i@ performs a bounds check for index @i@ and layout
--   @l@. Throws an 'IndexOutOfBounds' exception when out of range in
--   the form @(i, l)@. This can be caught with the '_IndexOutOfBounds'
--   prism.
--
-- >>> boundsCheck (V2 3 5) (V2 1 4) "in range"
-- "in range"
--
-- >>> boundsCheck (V2 10 20) (V2 10 5) "in bounds"
-- "*** Exception: array index out of range: (V2 10 5, V2 10 20)
--
-- >>> catching _IndexOutOfBounds (boundsCheck (V1 2) (V1 2) (putStrLn "in range")) print
-- "(V1 2, V1 2)"
--
-- The output format is suitable to be read using the '_Show' prism:
--
-- >>> trying (_IndexOutOfBounds . _Show) (boundsCheck (V1 2) (V1 20) (putStrLn "in range")) :: IO (Either (V1 Int, V1 Int) ())
-- Left (V1 20,V1 2)
boundsCheck :: Layout f => f Int -> Index f Int -> a -> a
boundsCheck l i
  | indexInRange l i = id
  | otherwise        = throwing _IndexOutOfBounds $
      "(" ++ showShape i ++ ", " ++ "LAYOUT GOES HERE" {- showShape l -} ++ ")"
  -- XXXX FIX: can't use showShape here
{-# INLINE boundsCheck #-}

-- Size missmatch ------------------------------------------------------

-- | Thrown when two sizes that should match, don't.
data SizeMissmatch = SizeMissmatch String
  deriving Typeable

instance Exception SizeMissmatch
instance Show SizeMissmatch where
  showsPrec _ (SizeMissmatch s)
    = showString "size missmatch"
    . (if not (null s) then showString ": " . showString s
                       else id)

-- | Exception thown from missmatching sizes.
class AsSizeMissmatch t where
  -- | Extract information about an 'SizeMissmatch'.
  --
  -- @
  -- '_SizeMissmatch' :: 'Prism'' 'SizeMissmatch' 'String'
  -- '_SizeMissmatch' :: 'Prism'' 'SomeException' 'String'
  -- @
  _SizeMissmatch :: Prism' t String

instance AsSizeMissmatch SizeMissmatch where
  _SizeMissmatch = prism' SizeMissmatch $ (\(SizeMissmatch s) -> Just s)
  {-# INLINE _SizeMissmatch #-}

instance AsSizeMissmatch SomeException where
  _SizeMissmatch = exception . (_SizeMissmatch :: Prism' SizeMissmatch String)
  {-# INLINE _SizeMissmatch #-}

-- | Check the sizes are equal. If not, throw 'SizeMissmatch'.
sizeMissmatch :: Int -> Int -> String -> a -> a
sizeMissmatch i j err
  | i == j    = id
  | otherwise = throwing _SizeMissmatch err
{-# INLINE sizeMissmatch #-}

-- Utilities -----------------------------------------------------------

-- | Show a shape in the form @VN i1 i2 .. iN@ where @N@ is the 'length'
--   of the shape.
showShape :: Foldable f => f Int -> String
showShape l = "V" ++ show (lengthOf folded l) ++ " " ++ unwords (show <$> F.toList l)

