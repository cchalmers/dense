{-# LANGUAGE BangPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Fusion
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Fusion combinators for arrays.
-----------------------------------------------------------------------------
module Data.Shaped.Fusion
  ( -- * Streams
    streamGenerate
  , streamGenerateM
  , unsafeStreamSub
  , streamSub
  , streamIndexes

    -- * Bundles
  , bundleGenerate
  , bundleGenerateM
  , bundleIndexes
  ) where


import qualified Data.Foldable                     as F
import           Data.Functor.Classes
import           Data.Vector.Fusion.Bundle.Size

import           Data.Vector.Fusion.Bundle.Monadic (Bundle (..))
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import           Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))

import qualified Data.Vector.Generic               as G
import           Linear


import           Data.Shaped.Base
import           Data.Shaped.Index

------------------------------------------------------------------------
-- Streams
------------------------------------------------------------------------

-- | Generate a stream from a 'Layout''s indices.
streamGenerate :: (Monad m, Shape l) => Layout l -> (l Int -> a) -> Stream m a
streamGenerate l f = streamGenerateM l (return . f)
{-# INLINE streamGenerate #-}

-- | Generate a stream from a 'Layout''s indices.
streamGenerateM :: (Monad m, Shape l) => Layout l -> (l Int -> m a) -> Stream m a
streamGenerateM l f = l `seq` Stream step (if eq1 l zero then Nothing else Just zero)
  where
    {-# INLINE [0] step #-}
    step (Just i) = do
      x <- f i
      return $ Yield x (stepShape l i)
    step Nothing  = return Done
{-# INLINE [1] streamGenerateM #-}

-- | Stream a sub-layout of an 'Array'. The layout should be inRange of
--   the array's layout, this is not checked.
unsafeStreamSub :: (Monad m, Shape l, G.Vector v a) => Layout l -> Array v l a -> Stream m a
unsafeStreamSub l2 (Array l1 v) = streamGenerateM l2 $ \x -> G.basicUnsafeIndexM v (toIndex l1 x)
{-# INLINE unsafeStreamSub #-}

-- | Stream a sub-layout of an 'Array'.
streamSub :: (Monad m, Shape l, G.Vector v a) => Layout l -> Array v l a -> Stream m a
streamSub l2 arr@(Array l1 _) = unsafeStreamSub (intersectShape l1 l2) arr
{-# INLINE streamSub #-}

-- | Make a stream of the indexes of a 'Layout'.
streamIndexes :: (Monad m, Shape l) => Layout l -> Stream m (l Int)
streamIndexes !l = Stream step (if eq1 l zero then Nothing else Just zero)
  where
    {-# INLINE [0] step #-}
    step (Just i) = return $ Yield i (stepShape l i)
    step Nothing  = return Done
{-# INLINE [1] streamIndexes #-}

------------------------------------------------------------------------
-- Bundles
------------------------------------------------------------------------

-- | Generate a bundle from 'Layout' indices.
bundleGenerate :: (Monad m, Shape l) => Layout l -> (l Int -> a) -> Bundle m v a
bundleGenerate l f = bundleGenerateM l (return . f)
{-# INLINE bundleGenerate #-}

-- | Generate a bundle from 'Layout' indices.
bundleGenerateM :: (Monad m, Shape l) => Layout l -> (l Int -> m a) -> Bundle m v a
bundleGenerateM l f = B.fromStream (streamGenerateM l f) (Exact (F.product l))
{-# INLINE [1] bundleGenerateM #-}

bundleIndexes :: (Monad m, Shape l) => Layout l -> Bundle m v (l Int)
bundleIndexes l = B.fromStream (streamIndexes l) (Exact (F.product l))
{-# INLINE [1] bundleIndexes #-}

