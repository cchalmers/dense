{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Mutable
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides a class for types that can be converted to and
-- from linear indexes.
--
-- The default instances are defined in row-major order.
-----------------------------------------------------------------------------
module Data.Shaped.Index
  ( -- * Shape class
    Layout
  , Shape (..)
  , indexIso
  , shapeIndexes
  , shapeIndexesFrom
  , shapeIndexesBetween

    -- * HasLayout
  , HasLayout (..)
  , extent
  , size
  , indexes
  , indexesBetween
  , indexesFrom

    -- * Exceptions

    -- (* Bounds checking
  , ArrayException (IndexOutOfBounds)
  , _IndexOutOfBounds
  , boundsCheck

    -- (* Size missmatch
  , SizeMissmatch (..)
  , AsSizeMissmatch
  , _SizeMissmatch
  , sizeMissmatch

    -- * Utilities
  , showShape
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Lens.Internal.Getter
import           Data.Foldable                as F
import           Data.Typeable

import           Data.Functor.Classes
import           Data.Traversable
import           Linear

-- | A 'Layout' is the full size of an array. This alias is used to help
--   distinguish between the layout of an array and an index (usually
--   just @l Int@) in a type signature.
type Layout f = f Int

------------------------------------------------------------------------
-- Shape class
------------------------------------------------------------------------

-- | Class for types that can be converted to and from linear indexes.
class (Eq1 f, Additive f, Traversable f) => Shape f where
  -- | Convert a shape to its linear index using the 'Layout'.
  shapeToIndex :: Layout f -> f Int -> Int
  shapeToIndex l x = F.foldl (\k (e, a) -> k * e + a) 0 (liftI2 (,) l x)
  {-# INLINE shapeToIndex #-}

  -- | Convert a linear index to a shape the 'Layout'.
  shapeFromIndex :: Layout f -> Int -> f Int
  shapeFromIndex l i = snd $ mapAccumR quotRem i l
  {-# INLINE shapeFromIndex #-}

  -- | Calculate the intersection of two shapes.
  shapeIntersect :: Layout f -> Layout f -> Layout f
  shapeIntersect = liftU2 min
  {-# INLINE shapeIntersect #-}

  -- | Increment a shape by one. It is assumed that the provided index
  --   is 'inRange'.
  shapeStep :: Layout f -> f Int -> Maybe (f Int)
  shapeStep l = fmap (shapeFromIndex l)
              . guardPure (< shapeSize l)
              . (+1)
              . shapeToIndex l
  {-# INLINE shapeStep #-}

  -- | Increment a shape by one between the two bounds
  shapeStepBetween :: f Int -> Layout f -> f Int -> Maybe (f Int)
  shapeStepBetween a l = fmap (^+^ a) . shapeStep l . (^-^ a)
  {-# INLINE shapeStepBetween #-}

  -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
  shapeInRange :: Layout f -> f Int -> Bool
  shapeInRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l
  {-# INLINE shapeInRange #-}

  -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
  shapeSize :: Layout f -> Int
  shapeSize = F.product
  {-# INLINE shapeSize #-}

guardPure :: Alternative f => (a -> Bool) -> a -> f a
guardPure p a = if p a then pure a else empty
{-# INLINE guardPure #-}

instance Shape V0

instance Shape V1 where
  {-# INLINE shapeToIndex #-}
  {-# INLINE shapeFromIndex #-}
  {-# INLINE shapeIntersect #-}
  {-# INLINE shapeStep #-}
  {-# INLINE shapeInRange #-}
  shapeToIndex _ (V1 i) = i
  shapeFromIndex _ i = V1 i
  shapeIntersect = min
  shapeStep l = guardPure (shapeInRange l) . (+1)
  shapeStepBetween _a b i = guardPure (> b) i'
    where i' = i + 1
  shapeInRange m i = i >= 0 && i < m

instance Shape V2 where
  shapeStep (V2 x y) (V2 i j)
    | j + 1 < y  = Just (V2      i  (j + 1))
    | i + 1 < x  = Just (V2 (i + 1)      0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

  shapeStepBetween (V2 _ia ja) (V2 ib jb) (V2 i j)
    | j + 1 < jb = Just (V2      i  (j + 1))
    | i + 1 < ib = Just (V2 (i + 1)     ja )
    | otherwise  = Nothing
  {-# INLINE shapeStepBetween #-}

instance Shape V3 where
  shapeStep (V3 x y z) (V3 i j k)
    | k + 1 < z  = Just (V3      i       j  (k + 1))
    | j + 1 < y  = Just (V3      i  (j + 1)      0 )
    | i + 1 < x  = Just (V3 (i + 1)      0       0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

  shapeStepBetween (V3 _ia ja ka) (V3 ib jb kb) (V3 i j k)
    | k < kb  = Just (V3      i       j  (k + 1))
    | j < jb  = Just (V3      i  (j + 1)     ka )
    | i < ib  = Just (V3 (i + 1)     ja      ka )
    | otherwise  = Nothing
  {-# INLINE shapeStepBetween #-}

instance Shape V4 where
  shapeStep (V4 x y z w) (V4 i j k l)
    | l + 1 < w  = Just (V4      i       j       k  (l + 1))
    | k + 1 < z  = Just (V4      i       j  (k + 1)      0 )
    | j + 1 < y  = Just (V4      i  (j + 1)      0       0 )
    | i + 1 < x  = Just (V4 (i + 1)      0       0       0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

  shapeStepBetween (V4 _ia ja ka la) (V4 ib jb kb lb) (V4 i j k l)
    | l < lb  = Just (V4      i       j       k  (l + 1))
    | k < kb  = Just (V4      i       j  (k + 1)     la )
    | j < jb  = Just (V4      i  (j + 1)     ka      la )
    | i < ib  = Just (V4 (i + 1)     ja      ka      la )
    | otherwise  = Nothing
  {-# INLINE shapeStepBetween #-}

-- instance Dim n => Shape (V n)

-- | @'toIndex' l@ and @'fromIndex' l@ form two halfs of an isomorphism.
indexIso :: Shape f => Layout f -> Iso' (f Int) Int
indexIso l = iso (shapeToIndex l) (shapeFromIndex l)
{-# INLINE indexIso #-}

------------------------------------------------------------------------
-- HasLayout
------------------------------------------------------------------------

-- | Class of things that have a 'Layout'. This means we can use the
--   same functions for the various different arrays in the library.
class Shape f => HasLayout f a | a -> f where
  -- | Lens onto the  'Layout' of something.
  layout :: Lens' a (Layout f)
  default layout :: (a ~ f Int) => Lens' a (Layout f)
  layout = id
  {-# INLINE layout #-}
  -- layout :: Shape f' => Lens' a a' (Layout f) (Layout f')

instance i ~ Int => HasLayout V0 (V0 i)
instance i ~ Int => HasLayout V1 (V1 i)
instance i ~ Int => HasLayout V2 (V2 i)
instance i ~ Int => HasLayout V3 (V3 i)
instance i ~ Int => HasLayout V4 (V4 i)

-- | Get the extent of an array.
--
-- @
-- 'extent' :: 'Data.Shaped.Base.Array' v f a    -> f 'Int'
-- 'extent' :: 'Data.Shaped.Mutable.MArray' v f s a -> f 'Int'
-- 'extent' :: 'Data.Shaped.Base.Delayed' f a    -> f 'Int'
-- 'extent' :: 'Data.Shaped.Base.Focused' f a    -> f 'Int'
-- @
extent :: HasLayout f a => a -> f Int
extent = view layout
{-# INLINE extent #-}

-- | Get the total number of elements in an array.
--
-- @
-- 'size' :: 'Data.Shaped.Base.Array' v f a    -> 'Int'
-- 'size' :: 'Data.Shaped.Mutable.MArray' v f s a -> 'Int'
-- 'size' :: 'Data.Shaped.Base.Delayed' f a    -> 'Int'
-- 'size' :: 'Data.Shaped.Base.Focused' f a    -> 'Int'
-- @
size :: HasLayout f a => a -> Int
size = shapeSize . view layout
{-# INLINE size #-}

-- NB: lens already uses indices so we settle for indexes

-- | Indexed fold for all the indexes in the layout.
indexes :: HasLayout f a => IndexedFold Int a (f Int)
indexes = layout . shapeIndexes
{-# INLINE indexes #-}

-- | 'indexes' for a 'Shape'.
shapeIndexes :: Shape f => IndexedFold Int (Layout f) (f Int)
shapeIndexes g l = go (0::Int) (if eq1 l zero then Nothing else Just zero) where
  go i (Just x) = indexed g i x *> go (i + 1) (shapeStep l x)
  go _ Nothing  = noEffect
{-# INLINE shapeIndexes #-}

-- | Indexed fold starting starting from some point, where the index is
--   the linear index for the original layout.
indexesFrom :: HasLayout f a => f Int -> IndexedFold Int a (f Int)
indexesFrom a = layout . shapeIndexesFrom a
{-# INLINE indexesFrom #-}

-- | 'indexesFrom' for a 'Shape'.
shapeIndexesFrom :: Shape f => f Int -> IndexedFold Int (Layout f) (f Int)
shapeIndexesFrom a f l = shapeIndexesBetween a l f l
{-# INLINE shapeIndexesFrom #-}

-- | Indexed fold between the two indexes where the index is the linear
--   index for the original layout.
indexesBetween :: HasLayout f a => f Int -> f Int -> IndexedFold Int a (f Int)
indexesBetween a b = layout . shapeIndexesBetween a b
{-# INLINE indexesBetween #-}

-- | 'indexesBetween' for a 'Shape'.
shapeIndexesBetween :: Shape f => f Int -> f Int -> IndexedFold Int (Layout f) (f Int)
shapeIndexesBetween a b f l =
  go (if eq1 l a || not (shapeInRange l b) then Nothing else Just a) where
    go (Just x) = indexed f (shapeToIndex l x) x *> go (shapeStepBetween a b x)
    go Nothing  = noEffect
{-# INLINE shapeIndexesBetween #-}

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
boundsCheck :: Shape l => Layout l-> l Int -> a -> a
boundsCheck l i
  | shapeInRange l i = id
  | otherwise        = throwing _IndexOutOfBounds $ "(" ++ showShape i ++ ", " ++ showShape l ++ ")"
{-# INLINE boundsCheck #-}

-- Size missmatch ------------------------------------------------------

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
showShape :: Shape f => f Int -> String
showShape l = "V" ++ show (lengthOf folded l) ++ " " ++ unwords (show <$> F.toList l)

