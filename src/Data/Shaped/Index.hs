{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}
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

    -- * Bounds checking
  , ArrayException (IndexOutOfBounds)
  , _IndexOutOfBounds
  , boundsCheck

    -- * Size missmatch
  , SizeMissmatch (..)
  , AsSizeMissmatch
  , _SizeMissmatch
  , sizeMissmatch

    -- * Utilities
  , showShape
  ) where

#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative
#endif
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Lens.Internal.Getter
import           Data.Foldable                as F
import           Data.Typeable

import           Data.Functor.Classes
import           Data.Traversable
import           Linear
import           Linear.V

-- | A 'Layout' is the full size of an array. This alias is used to help
--   distinguish between the layout of an array and an index (usually
--   just @l Int@) in a type signature.
type Layout l = l Int

-- | Class for types that can be converted to and from linear indexes.
class (Eq1 f, Additive f, Traversable f) => Shape f where
  -- | @toIndex l x@ returns the linear index @i@ of the shaped index @x@
  --   for array layout @l@.
  toIndex :: Layout f -> f Int -> Int
  toIndex l x = F.foldl (\k (e, a) -> k * e + a) 0 (liftI2 (,) l x)
  {-# INLINE toIndex #-}

  -- | @toIndex l i@ returns the shaped index @x@ of the linear index @i@
  --   for array layout @l@.
  fromIndex :: Layout f -> Int -> f Int
  fromIndex l i = snd $ mapAccumR quotRem i l
  {-# INLINE fromIndex #-}

  -- | Calculate the intersection of two shapes.
  intersectShape :: f Int -> f Int -> f Int
  intersectShape = liftU2 min
  {-# INLINE intersectShape #-}

  -- | @'toIndex' l@ and @'fromIndex' l@ form two halfs of an isomorphism.
  indexFor :: Layout f -> Iso' (f Int) Int
  indexFor l = iso (toIndex l) (fromIndex l)

  -- | @inRange ex i@ checks @i < ex@ for every coodinate of @f@.
  inRange :: Layout f -> f Int -> Bool
  inRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l

  -- | Indexed fold for the range between two shapes.
  rangeBetween :: f Int -> f Int -> IndexedFold Int (Layout f) (f Int)
  rangeBetween x1 x2 = l -- conjoined l (indexing l)
   -- horribly inefficient
    where f x = F.and (liftI2 (<=) x1 x) && F.and (liftI2 (>) x x2)
          l = enumShape . filtered f
  {-# INLINE rangeBetween #-}

  -- | Indexed fold to the shape.
  enumShape :: IndexedFold Int (Layout f) (f Int)
  enumShape f l = go 0 where
    -- What about negative indices?
    n = F.product l
    go i | i == n    = noEffect
         | otherwise = indexed f i (fromIndex l i) *> go (i + 1)
  {-# INLINE enumShape #-}

instance Shape Identity
instance Shape V0
instance Shape V1
instance Shape V2 where
  enumShape = enumV2
  {-# INLINE enumShape #-}
  rangeBetween = rangeBetweenV2
  {-# INLINE rangeBetween #-}
instance Shape V3 where
  enumShape = enumV3
  {-# INLINE enumShape #-}
instance Shape V4 where
  enumShape = enumV4
  {-# INLINE enumShape #-}
instance Dim n => Shape (V n)

enumV2 :: IndexedFold Int (Layout V2) (V2 Int)
enumV2 f l@(V2 x y) = go zero where
  go q@(V2 i j)
    | i >= x    = noEffect
    | j >= y    = go $ V2 (i+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V2 i (j+1))
{-# INLINE enumV2 #-}

rangeBetweenV2 :: V2 Int -> V2 Int -> IndexedFold Int (Layout V2) (V2 Int)
rangeBetweenV2 v0@(V2 _ y0) (V2 x2 y2) f l = go v0 where
  go q@(V2 i j)
    | j >= x2   = noEffect
    | i >= y2   = go $ V2 (i+1) y0
    | otherwise = indexed f (toIndex l q) q *> go (V2 i (j+1))
{-# INLINE rangeBetweenV2 #-}

enumV3 :: IndexedFold Int (Layout V3) (V3 Int)
enumV3 f l@(V3 x y z) = go zero where
  go q@(V3 i j k)
    | i >= x    = noEffect
    | j >= y    = go $ V3 (i+1)    0  0
    | k >= z    = go $ V3    i  (j+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V3 i j (k+1))
{-# INLINE enumV3 #-}

enumV4 :: IndexedFold Int (Layout V4) (V4 Int)
enumV4 f l@(V4 x y z w) = go zero where
  go q@(V4 i j k m)
    | i >= x    = noEffect
    | j >= y    = go $ V4 (i+1)    0     0  0
    | k >= z    = go $ V4    i  (j+1)    0  0
    | m >= w    = go $ V4    i  (j+1) (k+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V4 i j k (m+1))
{-# INLINE enumV4 #-}

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
  | inRange l i = id
  | otherwise   = throwing _IndexOutOfBounds $ "(" ++ showShape i ++ ", " ++ showShape l ++ ")"
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

-- Utilities -----------------------------------------------------------

-- | Show a shape in the form @VN i1 i2 .. iN@ where @N@ is the 'length'
--   of the shape.
showShape :: Shape l => l Int -> String
showShape l = "V" ++ show (lengthOf folded l) ++ " " ++ unwords (show <$> F.toList l)

