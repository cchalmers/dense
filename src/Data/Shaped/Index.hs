{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Shaped.Index where

#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative
#endif
import           Control.Exception.Lens
import           Control.Lens
import           Control.Lens.Internal.Getter
import           Data.Foldable                as F

import           Data.Functor.Classes
import           Data.Traversable
import           Linear
import           Linear.V

-- I'm not happy with this class. Additive and Traversable is enough for
-- everything we need but not everything that satisfies this is makes a
-- valid shape. This class will likely change in the future. Suggestions
-- are welcome.

class (Eq1 f, Additive f, Traversable f) => Shape f where
  -- | @toIndex l x@ returns the linear index @i@ of the shaped index @x@
  --   for array layout @l@.
  toIndex :: f Int -> f Int -> Int
  toIndex l x = F.foldl (\k (e, a) -> k * e + a) 0 (liftI2 (,) l x)
  {-# INLINE toIndex #-}

  -- | @toIndex l i@ returns the shaped index @x@ of the linear index @i@
  --   for array layout @l@.
  fromIndex :: f Int -> Int -> f Int
  fromIndex l i = snd $ mapAccumR quotRem i l
  {-# INLINE fromIndex #-}

  intersectShape :: Ord a => f a -> f a -> f a
  intersectShape = liftU2 min
  {-# INLINE intersectShape #-}

  -- | @'toIndex' l@ and @'fromIndex' l@ form two halfs of an isomorphism.
  indexFor :: f Int -> Iso' (f Int) Int
  indexFor l = iso (toIndex l) (fromIndex l)

  -- | @inRange ex i@ checks @i < ex@ for every coodinate of @f@.
  inRange :: (Num a, Ord a) => f a -> f a -> Bool
  inRange l i = F.and $ liftI2 (\ii li -> ii >= 0 && ii < li) i l

  -- slicing :: Lens' l1 l2 -> (l2 -> l2) -> l1 -> Array v l1 a -> Array v l2 a
  -- slicing ls f l arr =

  rangeBetween :: f Int -> f Int -> IndexedFold Int (f Int) (f Int)
  rangeBetween x1 x2 = l -- conjoined l (indexing l)
   -- horribly inefficient
    where f x = F.and (liftI2 (<=) x1 x) && F.and (liftI2 (>) x x2)
          l = enumShape . filtered f
  {-# INLINE rangeBetween #-}

  enumShape :: IndexedFold Int (f Int) (f Int)
  enumShape f l = go 0 where
    -- What about negative indices?
    n = F.product l
    go i | i == n    = noEffect
         | otherwise = indexed f i (fromIndex l i) *> go (i + 1)
  {-# INLINE enumShape #-}

instance Shape V0
instance Shape V1
instance Shape V2
instance Shape V3
instance Shape V4
instance Dim n => Shape (V n)

enumV2 :: IndexedFold Int (V2 Int) (V2 Int)
enumV2 f l@(V2 x y) = go zero where
  go q@(V2 i j)
    | i >= x    = noEffect
    | j >= y    = go $ V2 (i+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V2 i (j+1))

rangeBetweenV2 :: V2 Int -> V2 Int -> IndexedFold Int (V2 Int) (V2 Int)
rangeBetweenV2 v0@(V2 _ y0) (V2 x2 y2) f l = go v0 where
  go q@(V2 i j)
    | j >= x2   = noEffect
    | i >= y2   = go $ V2 (i+1) y0
    | otherwise = indexed f (toIndex l q) q *> go (V2 i (j+1))

enumV3 :: IndexedFold Int (V3 Int) (V3 Int)
enumV3 f l@(V3 x y z) = go zero where
  go q@(V3 i j k)
    | i >= x    = noEffect
    | j >= y    = go $ V3 (i+1)    0  0
    | k >= z    = go $ V3    i  (j+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V3 i j (k+1))

enumV4 :: IndexedFold Int (V4 Int) (V4 Int)
enumV4 f l@(V4 x y z w) = go zero where
  go q@(V4 i j k m)
    | i >= x    = noEffect
    | j >= y    = go $ V4 (i+1)    0     0  0
    | k >= z    = go $ V4    i  (j+1)    0  0
    | m >= w    = go $ V4    i  (j+1) (k+1) 0
    | otherwise = indexed f (toIndex l q) q *> go (V4 i j k (m+1))

-- | Perform a bounds check for index @i@ and layout @l@. Throws an
--   'IndexOutOfBounds' exception when out of range. This can be caught
--   with the '_IndexOutOfBounds' prism.
--
-- >>> boundsCheck (V2 3 5) (V2 1 4) "in range"
-- "in range"
--
-- >>> boundsCheck (V3 10 20) (V2 10 5) "in bounds"
-- *** Exception: array index out of range: (V2 10 20, V2 10 5)
--
-- >>> catching _IndexOutOfBounds (boundsCheck (V1 2) (V1 2) (putStrLn "in range")) print
-- "(V1 2, V1 2)"
--
-- The output format is suitable to be read using the '_Show' prism:
--
-- >>> trying (_IndexOutOfBounds . _Show) (boundsCheck (V1 2) (V1 20) (putStrLn "in range")) :: IO (Either (V1 Int, V1 Int) ())
-- Left (V1 2,V1 2)
boundsCheck :: Shape l => l Int -> l Int -> a -> a
boundsCheck i l
  | inRange i l = id
  | otherwise   = throwing _IndexOutOfBounds $ "(" ++ showShape i ++ ", " ++ showShape l ++ ")"
{-# INLINE boundsCheck #-}

-- _Show1 :: (Read1 f, Show1 f, Read a, Show a) => Prism' String (f a)
-- _Show1 = prism (\a -> showsPrec1 0 a "") $ \s -> case readsPrec1 0 s of
--   [(a,"")] -> Right a
--   _        -> Left s
-- {-# INLINE _Show1 #-}

showShape :: Shape l => l Int -> String
showShape l = "V" ++ show (length l) ++ " " ++ unwords (show <$> F.toList l)

