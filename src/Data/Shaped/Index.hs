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
    Shape (..)
  , indexIso

  , Windowed
  , mkW

  , shapeIndexes
  -- , shapeIndexesFrom
  -- , shapeIndexesBetween

    -- -- * HasLayout
  , HasLayout (..)
  , extent
  , size
  , indexes
  -- , indexesBetween
  -- , indexesFrom

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

------------------------------------------------------------------------
-- Shape class
------------------------------------------------------------------------

-- | A 'Shape' whose layout it itself.
class Shape (f Int) f => SelfLayout f
instance Shape (f Int) f => SelfLayout f

class (Eq1 f, Additive f, Traversable f) => Shape l f | l -> f where
  -- | Convert a shape to its linear index using the 'Layout'.
  shapeToIndex :: l -> f Int -> Int
  default shapeToIndex :: l ~ f Int => l -> f Int -> Int
  shapeToIndex l x = F.foldl (\k (e, a) -> k * e + a) 0 (liftI2 (,) l x)
  {-# INLINE shapeToIndex #-}

  -- | Convert a linear index to a shape the 'Layout'.
  shapeFromIndex :: l -> Int -> f Int
  default shapeFromIndex :: l ~ f Int => f Int -> Int -> f Int
  shapeFromIndex l i = snd $ mapAccumR quotRem i l
  {-# INLINE shapeFromIndex #-}

  -- | Calculate the intersection of two shapes.
  shapeIntersect :: l -> l -> l
  default shapeIntersect :: l ~ f Int => l -> l -> l
  shapeIntersect = liftU2 min
  {-# INLINE shapeIntersect #-}

  -- | Increment a shape by one. It is assumed that the provided index
  --   satisfies 'shapeInRange'.
  shapeStep :: l -> f Int -> Maybe (f Int)
  default shapeStep :: l ~ f Int => l -> f Int -> Maybe (f Int)
  shapeStep l = fmap (shapeFromIndex l)
              . guardPure (< F.product l)
              . (+1)
              . shapeToIndex l
  {-# INLINE shapeStep #-}

  -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
  shapeInRange :: l -> f Int -> Bool
  default shapeInRange :: l ~ f Int => l -> f Int -> Bool
  shapeInRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l
  {-# INLINE shapeInRange #-}

  -- | @inRange ex i@ checks @i < ex@ for every coordinate of @f@.
  shapeSize :: l -> Int
  default shapeSize :: l ~ f Int => l -> Int
  shapeSize = F.product
  {-# INLINE shapeSize #-}

  emptyLayout :: l -> Bool
  default emptyLayout :: l ~ f Int => l -> Bool
  emptyLayout = eq1 zero
  {-# INLINE emptyLayout #-}

  initialIndex :: l -> f Int
  default initialIndex :: l ~ f Int => l -> f Int
  initialIndex _ = zero
  {-# INLINE initialIndex #-}


guardPure :: Alternative f => (a -> Bool) -> a -> f a
guardPure p a = if p a then pure a else empty
{-# INLINE guardPure #-}

-- instance Shape V0

instance Shape (V1 Int) V1 where
  {-# INLINE shapeToIndex #-}
  {-# INLINE shapeFromIndex #-}
  {-# INLINE shapeIntersect #-}
  {-# INLINE shapeStep #-}
  {-# INLINE shapeInRange #-}
  shapeToIndex _ (V1 i) = i
  shapeFromIndex _ i = V1 i
  shapeIntersect = min
  shapeStep l = guardPure (shapeInRange l) . (+1)
  shapeInRange m i = i >= 0 && i < m

instance Shape (V2 Int) V2 where
  shapeStep (V2 x y) (V2 i j)
    | j + 1 < y  = Just (V2      i  (j + 1))
    | i + 1 < x  = Just (V2 (i + 1)      0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

instance Shape (V3 Int) V3 where
  shapeStep (V3 x y z) (V3 i j k)
    | k + 1 < z  = Just (V3      i       j  (k + 1))
    | j + 1 < y  = Just (V3      i  (j + 1)      0 )
    | i + 1 < x  = Just (V3 (i + 1)      0       0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

instance Shape (V4 Int) V4 where
  shapeStep (V4 x y z w) (V4 i j k l)
    | l + 1 < w  = Just (V4      i       j       k  (l + 1))
    | k + 1 < z  = Just (V4      i       j  (k + 1)      0 )
    | j + 1 < y  = Just (V4      i  (j + 1)      0       0 )
    | i + 1 < x  = Just (V4 (i + 1)      0       0       0 )
    | otherwise  = Nothing
  {-# INLINE shapeStep #-}

-- instance Dim n => Shape (V n)

-- Windowed layouts ----------------------------------------------------

-- | A layout that does not nesses
data Windowed f = Windowed !(f Int) !(f Int)
  -- Windowed startIndex extent

-- | Make a windowed layouted from a first index and upper bound.
mkW :: SelfLayout f => f Int -> f Int -> Windowed f
mkW a b = Windowed a (b ^-^ a)

instance SelfLayout f => Shape (Windowed f) f where
  shapeToIndex (Windowed a l) x = shapeToIndex l (x ^-^ a)
  {-# INLINE shapeToIndex #-}

  shapeFromIndex (Windowed a l) i = shapeFromIndex l i ^+^ a
  {-# INLINE shapeFromIndex #-}

  shapeIntersect (Windowed a1 l1) (Windowed a2 l2) = Windowed (liftU2 max a1 a2) (shapeIntersect l1 l2)
  {-# INLINE shapeIntersect #-}

  shapeStep (Windowed a l) x = (^+^ a) <$> shapeStep l (x ^-^ a)
  {-# INLINE shapeStep #-}

  shapeInRange (Windowed a l) x = shapeInRange l (x ^-^ a)
  {-# INLINE shapeInRange #-}

  shapeSize (Windowed _ l) = shapeSize l
  {-# INLINE shapeSize #-}

  emptyLayout (Windowed _ l) = emptyLayout l
  {-# INLINE emptyLayout #-}

  initialIndex (Windowed a _) = a
  {-# INLINE initialIndex #-}

-- | @'toIndex' l@ and @'fromIndex' l@ form two halfs of an isomorphism.
indexIso :: Shape l f => l -> Iso' (f Int) Int
indexIso l = iso (shapeToIndex l) (shapeFromIndex l)
{-# INLINE indexIso #-}

  -- -- | Increment a shape by one between the two bounds
  -- shapeStepBetween :: f Int -> l -> f Int -> Maybe (f Int)
  -- default shapeStepBetween :: l~f Int => f Int -> f Int -> f Int -> Maybe (f Int)
  -- shapeStepBetween a l = fmap (^+^ a) . shapeStep l . (^-^ a)
  -- {-# INLINE shapeStepBetween #-}

------------------------------------------------------------------------
-- HasLayout
------------------------------------------------------------------------

-- | Class of things that have a 'Layout'. This means we can use the
--   same functions for the various different arrays in the library.
class Shape l f => HasLayout l f a | a -> l where
  -- | Lens onto the  'Layout' of something.
  layout :: Lens' a l
  default layout :: (a ~ l) => Lens' a l
  layout = id
  {-# INLINE layout #-}
  -- layout :: Shape f' => Lens' a a' (Layout f) (Layout f')

-- instance (i ~ Int, i ~ j) => HasLayout (V0 i) V0 (V0 j)
instance (i ~ Int, i ~ j) => HasLayout (V1 i) V1 (V1 j)
instance (i ~ Int, i ~ j) => HasLayout (V2 i) V2 (V2 j)
instance (i ~ Int, i ~ j) => HasLayout (V3 i) V3 (V3 j)
instance (i ~ Int, i ~ j) => HasLayout (V4 i) V4 (V4 j)

-- | Get the extent of an array.
--
-- @
-- 'extent' :: 'Data.Shaped.Base.Array' v f a    -> f 'Int'
-- 'extent' :: 'Data.Shaped.Mutable.MArray' v f s a -> f 'Int'
-- 'extent' :: 'Data.Shaped.Base.Delayed' f a    -> f 'Int'
-- 'extent' :: 'Data.Shaped.Base.Focused' f a    -> f 'Int'
-- @
extent :: HasLayout l f a => a -> l
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
size :: HasLayout l f a => a -> Int
size = shapeSize . view layout
{-# INLINE size #-}

---- NB: lens already uses indices so we settle for indexes

-- | Indexed fold for all the indexes in the layout.
indexes :: HasLayout l f a => IndexedFold Int a (f Int)
indexes = layout . shapeIndexes
{-# INLINE indexes #-}

-- | 'indexes' for a 'Shape'.
shapeIndexes :: Shape l f => IndexedFold Int l (f Int)
shapeIndexes g l = go (0::Int) (if emptyLayout l then Nothing else Just (initialIndex l)) where
  go i (Just x) = indexed g i x *> go (i + 1) (shapeStep l x)
  go _ Nothing  = noEffect
{-# INLINE shapeIndexes #-}

-- -- | Indexed fold starting starting from some point, where the index is
-- --   the linear index for the original layout.
-- indexesFrom :: HasLayout l f a => f Int -> IndexedFold Int a (f Int)
-- indexesFrom a = layout . shapeIndexesFrom a
-- {-# INLINE indexesFrom #-}

-- -- | 'indexesFrom' for a 'Shape'.
-- shapeIndexesFrom :: Shape l f => f Int -> IndexedFold Int (Layout f) (f Int)
-- shapeIndexesFrom a f l = shapeIndexesBetween a l f l
-- {-# INLINE shapeIndexesFrom #-}

-- -- | Indexed fold between the two indexes where the index is the linear
-- --   index for the original layout.
-- indexesBetween :: HasLayout l f a => f Int -> f Int -> IndexedFold Int a (f Int)
-- indexesBetween a b = layout . shapeIndexesBetween a b
-- {-# INLINE indexesBetween #-}

-- -- | 'indexesBetween' for a 'Shape'.
-- shapeIndexesBetween :: Shape l f => f Int -> f Int -> IndexedFold Int (Layout f) (f Int)
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
boundsCheck :: Shape l f => l -> f Int -> a -> a
boundsCheck l i
  | shapeInRange l i = id
  -- | otherwise        = throwing _IndexOutOfBounds $ "(" ++ showShape i ++ ", " ++ showShape l ++ ")"
  | otherwise        = throwing _IndexOutOfBounds $ "(" ++ showShape i ++ ", " ++ "l needs show instance!" ++ ")"
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
showShape :: Foldable f => f Int -> String
showShape l = "V" ++ show (lengthOf folded l) ++ " " ++ unwords (show <$> F.toList l)

