{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dense.Mutable
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides generic functions over mutable multidimensional
-- arrays.
-----------------------------------------------------------------------------
module Data.Dense.Mutable
  (
    -- * Mutable array
    MArray (..)
  , UMArray
  , SMArray
  , BMArray
  , PMArray

  -- * Lenses
  , mlayout
  , mvector

    -- * Creation
  , new
  , replicate
  , replicateM
  , clone

    -- * Standard operations
    -- ** Indexing
  , read
  , linearRead
  , unsafeRead
  , unsafeLinearRead

    -- ** Writing
  , write
  , linearWrite
  , unsafeWrite
  , unsafeLinearWrite

    -- ** Modifying
  , modify
  , linearModify
  , unsafeModify
  , unsafeLinearModify

    -- ** Swap
  , swap
  , linearSwap
  , unsafeSwap
  , unsafeLinearSwap

    -- ** Exchange
  , exchange
  , linearExchange
  , unsafeExchange
  , unsafeLinearExchange

    -- * Misc
  , set
  , clear
  , copy

  ) where

import           Control.Monad                 (liftM)
import           Control.Monad.Primitive
import           Control.Lens                  (IndexedLens, indexed, Lens, (<&>))
import           Data.Foldable                 as F
import           Data.Typeable
import qualified Data.Vector                   as B
import           Data.Vector.Generic.Mutable   (MVector)
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Primitive.Mutable as P
import qualified Data.Vector.Storable.Mutable  as S
import qualified Data.Vector.Unboxed.Mutable   as U
import           Linear.V1

import           Data.Dense.Index

import           Prelude                       hiding (read, replicate)

-- | A mutable array with a shape.
data MArray v f s a = MArray !(f Int) !(v s a)
  deriving Typeable

-- | Boxed mutable array.
type BMArray = MArray B.MVector

-- | Unboxed mutable array.
type UMArray = MArray U.MVector

-- | Storable mutable array.
type SMArray = MArray S.MVector

-- | Primitive mutable array.
type PMArray = MArray P.MVector

-- | Lens onto the shape of the vector. The total size of the layout
--   _must_ remain the same or an error is thrown.
mlayout :: (Layout f, Layout f') => Lens (MArray v f s a) (MArray v f' s a) (f Int) (f' Int)
mlayout f (MArray l v) = f l <&> \l' ->
  sizeMissmatch (layoutSize l) (layoutSize l')
    -- ("mlayout: trying to replace shape " ++ showLayout l ++ ", with " ++ showLayout l')
    "mlayout: trying to replace shape LAYOUT1, with LAYOUT2"
    $ MArray l' v
{-# INLINE mlayout #-}

instance Layout f => HasLayout f (MArray v f s a) where
  layout = mlayout
  {-# INLINE layout #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must __not__ change the length of
--   the vector, otherwise an error will be thrown.
mvector :: (MVector v a, MVector w b) => IndexedLens (f Int) (MArray v f s a) (MArray w f t b) (v s a) (w t b)
mvector f (MArray l v) =
  indexed f l v <&> \w ->
  sizeMissmatch (GM.length v) (GM.length w)
     ("mvector: trying to replace vector of length " ++ show (GM.length v) ++ ", with one of length " ++ show (GM.length w))
     $ MArray l w
{-# INLINE mvector #-}

-- | New mutable array with shape @l@.
new :: (PrimMonad m, Layout f, MVector v a) => f Int -> m (MArray v f (PrimState m) a)
new l = MArray l `liftM` GM.new (layoutSize l)
{-# INLINE new #-}

-- | New mutable array with shape @l@ filled with element @a@.
replicate :: (PrimMonad m, Layout f, MVector v a) => f Int -> a -> m (MArray v f (PrimState m) a)
replicate l a = MArray l `liftM` GM.replicate (layoutSize l) a
{-# INLINE replicate #-}

-- | New mutable array with shape @l@ filled with result of monadic
--   action @a@.
replicateM :: (PrimMonad m, Layout f, MVector v a) => f Int -> m a -> m (MArray v f (PrimState m) a)
replicateM l a = MArray l `liftM` GM.replicateM (layoutSize l) a
{-# INLINE replicateM #-}

-- | Clone a mutable array, making a new, separate mutable array.
clone :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> m (MArray v f (PrimState m) a)
clone (MArray l v) = MArray l `liftM` GM.clone v
{-# INLINE clone #-}

-- Individual elements -------------------------------------------------

-- | Clear the elements of a mutable array. This is usually a no-op for
--   unboxed arrays.
clear :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> m ()
clear (MArray _ v) = GM.clear v
{-# INLINE clear #-}

-- | Read a mutable array at element @l@.
read :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> m a
read (MArray l v) s = boundsCheck l s $ GM.unsafeRead v (indexToOffset l s)
{-# INLINE read #-}

-- | Write a mutable array at element @l@.
write :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> a -> m ()
write (MArray l v) s a = boundsCheck l s $ GM.unsafeWrite v (indexToOffset l s) a
{-# INLINE write #-}

-- | Modify a mutable array at element @l@ by applying a function.
modify :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> (a -> a) -> m ()
modify (MArray l v) s f = boundsCheck l s $ GM.unsafeRead v i >>= GM.unsafeWrite v i . f
  where i = indexToOffset l s
{-# INLINE modify #-}

-- | Swap two elements in a mutable array.
swap :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> Index f Int -> m ()
swap (MArray l v) i j = boundsCheck l i boundsCheck l j $ GM.unsafeSwap v (indexToOffset l i) (indexToOffset l j)
{-# INLINE swap #-}

-- | Replace the element at the give position and return the old
--   element.
exchange :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> a -> m a
exchange (MArray l v) i a = boundsCheck l i $ GM.unsafeExchange v (indexToOffset l i) a
{-# INLINE exchange #-}

-- | Read a mutable array at element @i@ by indexing the internal
--   vector.
linearRead :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> m a
linearRead (MArray _ v) = GM.read v
{-# INLINE linearRead #-}

-- | Write a mutable array at element @i@ by indexing the internal
--   vector.
linearWrite :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> a -> m ()
linearWrite (MArray _ v) = GM.write v
{-# INLINE linearWrite #-}

-- | Swap two elements in a mutable array by indexing the internal
--   vector.
linearSwap :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> Int -> m ()
linearSwap (MArray _ v) = GM.swap v
{-# INLINE linearSwap #-}

-- | Modify a mutable array at element @i@ by applying a function.
linearModify :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> (a -> a) -> m ()
linearModify (MArray _ v) i f = GM.read v i >>= GM.unsafeWrite v i . f
{-# INLINE linearModify #-}

-- | Replace the element at the give position and return the old
--   element.
linearExchange :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> a -> m a
linearExchange (MArray _ v) i a = GM.exchange v i a
{-# INLINE linearExchange #-}

-- Unsafe varients

-- | 'read' without bounds checking.
unsafeRead :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> m a
unsafeRead (MArray l v) s = GM.unsafeRead v (indexToOffset l s)
{-# INLINE unsafeRead #-}

-- | 'write' without bounds checking.
unsafeWrite :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> a -> m ()
unsafeWrite (MArray l v) s = GM.unsafeWrite v (indexToOffset l s)
{-# INLINE unsafeWrite #-}

-- | 'swap' without bounds checking.
unsafeSwap :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> Index f Int -> m ()
unsafeSwap (MArray l v) s j = GM.unsafeSwap v (indexToOffset l s) (indexToOffset l j)
{-# INLINE unsafeSwap #-}

-- | 'modify' without bounds checking.
unsafeModify :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> (a -> a) -> m ()
unsafeModify (MArray l v) s f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
  where i = indexToOffset l s
{-# INLINE unsafeModify #-}

-- | Replace the element at the give position and return the old
--   element.
unsafeExchange :: (PrimMonad m, Layout f, MVector v a) => MArray v f (PrimState m) a -> Index f Int -> a -> m a
unsafeExchange (MArray l v) i a = GM.unsafeExchange v (indexToOffset l i) a
{-# INLINE unsafeExchange #-}

-- | 'linearRead' without bounds checking.
unsafeLinearRead :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> m a
unsafeLinearRead (MArray _ v) = GM.unsafeRead v
{-# INLINE unsafeLinearRead #-}

-- | 'linearWrite' without bounds checking.
unsafeLinearWrite :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> a -> m ()
unsafeLinearWrite (MArray _ v) = GM.unsafeWrite v
{-# INLINE unsafeLinearWrite #-}

-- | 'linearSwap' without bounds checking.
unsafeLinearSwap :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> Int -> m ()
unsafeLinearSwap (MArray _ v) = GM.unsafeSwap v
{-# INLINE unsafeLinearSwap #-}

-- | 'linearModify' without bounds checking.
unsafeLinearModify :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeLinearModify (MArray _ v) i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
{-# INLINE unsafeLinearModify #-}

-- | Replace the element at the give position and return the old
--   element.
unsafeLinearExchange :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> Int -> a -> m a
unsafeLinearExchange (MArray _ v) i a = GM.unsafeExchange v i a
{-# INLINE unsafeLinearExchange #-}

-- Filling and copying -------------------------------------------------

-- | Set all elements in a mutable array to a constant value.
set :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> a -> m ()
set (MArray _ v) = GM.set v
{-# INLINE set #-}

-- | Copy all elements from one array into another.
copy :: (PrimMonad m, MVector v a) => MArray v f (PrimState m) a -> MArray v f (PrimState m) a -> m ()
copy (MArray _ v) (MArray _ u) = GM.copy v u
{-# INLINE copy #-}

-- V1 instances --------------------------------------------------------

-- Array v V1 a is essentially v a with a wrapper. Instance is provided
-- for convience.

instance (MVector v a, f ~ V1) => MVector (MArray v f) a where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicInitialize #-}
  basicLength (MArray (V1 n) _) = n
  basicUnsafeSlice i n (MArray _ v) = MArray (V1 n) $ GM.basicUnsafeSlice i n v
  basicOverlaps (MArray _ v) (MArray _ w) = GM.basicOverlaps v w
  basicUnsafeNew n = MArray (V1 n) `liftM` GM.basicUnsafeNew n
  basicUnsafeRead (MArray _ v) = GM.basicUnsafeRead v
  basicUnsafeWrite (MArray _ v) = GM.basicUnsafeWrite v
  basicInitialize (MArray _ v) = GM.basicInitialize v

