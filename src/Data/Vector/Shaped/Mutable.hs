{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Vector.Shaped.Mutable
  (
    -- * Mutable array
    MArray (..)
  , UMArray
  , SMArray
  , BMArray
  , PMArray

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

    -- * Misc
  , set
  , clear
  , copy

  ) where

import           Control.Monad.Primitive
import           Data.Typeable
import qualified Data.Vector                   as B
import           Data.Vector.Generic.Mutable   (MVector)
import qualified Data.Vector.Generic.Mutable   as GM
import qualified Data.Vector.Primitive.Mutable as P
import qualified Data.Vector.Storable.Mutable  as S
import qualified Data.Vector.Unboxed.Mutable   as U
import           Linear.V1

import           Data.Vector.Shaped.Index

import           Prelude                       hiding (read, replicate)

-- | A mutable array with a shape.
data MArray v l s a = MArray !(l Int) !(v s a)
  deriving Typeable

type UMArray = MArray U.MVector
type SMArray = MArray S.MVector
type BMArray = MArray B.MVector
type PMArray = MArray P.MVector

-- | New mutable array with shape @l@.
new :: (PrimMonad m, Shape l, MVector v a) => l Int -> m (MArray v l (PrimState m) a)
new l = MArray l <$> GM.new (product l)

-- | New mutable array with shape @l@ filled with element @a@.
replicate :: (PrimMonad m, Shape l, MVector v a) => l Int -> a -> m (MArray v l (PrimState m) a)
replicate l a = MArray l <$> GM.replicate (product l) a

-- | New mutable array with shape @l@ filled with result of monadic
--   action @a@.
replicateM :: (PrimMonad m, Shape l, MVector v a) => l Int -> m a -> m (MArray v l (PrimState m) a)
replicateM l a = MArray l <$> GM.replicateM (product l) a

-- | Clone a mutable array, making a new, separate mutable array.
clone :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> m (MArray v l (PrimState m) a)
clone (MArray l v) = MArray l <$> GM.clone v

-- Individual elements -------------------------------------------------

-- | Clear the elements of a mutable array. This is usually a no-op for
--   unboxed arrays.
clear :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> m ()
clear (MArray _ v) = GM.clear v

-- | Read a mutable array at element @l@.
read :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> m a
read (MArray l v) s = GM.read v (toIndex l s)

-- | Write a mutable array at element @l@.
write :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> a -> m ()
write (MArray l v) s a = GM.write v (toIndex l s) a

-- | Modify a mutable array at element @l@ by applying a function.
modify :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> (a -> a) -> m ()
modify (MArray l v) s f = GM.read v i >>= GM.unsafeWrite v i . f
  where i = toIndex l s

-- | Swap two elements in a mutable array.
swap :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> l Int -> m ()
swap (MArray l v) i j = GM.swap v (toIndex l i) (toIndex l j)

-- | Read a mutable array at element @i@ by indexing the internal
--   vector.
linearRead :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> m a
linearRead (MArray _ v) i = GM.read v i

-- | Write a mutable array at element @i@ by indexing the internal
--   vector.
linearWrite :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> a -> m ()
linearWrite (MArray _ v) i a = GM.write v i a

-- | Swap two elements in a mutable array by indexing the internal
--   vector.
linearSwap :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> Int -> m ()
linearSwap (MArray _ v) i j = GM.swap v i j

linearModify :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> (a -> a) -> m ()
linearModify (MArray _ v) i f = GM.read v i >>= GM.unsafeWrite v i . f

-- Unsafe varients

-- | 'read' without bounds checking.
unsafeRead :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> m a
unsafeRead (MArray l v) s = GM.unsafeRead v (toIndex l s)

-- | 'write' without bounds checking.
unsafeWrite :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> a -> m ()
unsafeWrite (MArray l v) s a = GM.unsafeWrite v (toIndex l s) a

-- | 'swap' without bounds checking.
unsafeSwap :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> l Int -> m ()
unsafeSwap (MArray l v) s j = GM.unsafeSwap v (toIndex l s) (toIndex j s)

-- | 'modify' without bounds checking.
unsafeModify :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> l Int -> (a -> a) -> m ()
unsafeModify (MArray l v) s f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f
  where i = toIndex l s

-- | 'linearRead' without bounds checking.
unsafeLinearRead :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> m a
unsafeLinearRead (MArray _ v) i = GM.unsafeRead v i

-- | 'linearWrite' without bounds checking.
unsafeLinearWrite :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> a -> m ()
unsafeLinearWrite (MArray _ v) i a = GM.unsafeWrite v i a

-- | 'linearSwap' without bounds checking.
unsafeLinearSwap :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> Int -> Int -> m ()
unsafeLinearSwap (MArray _ v) i j = GM.unsafeSwap v i j

-- | 'linearModify' without bounds checking.
unsafeLinearModify :: (PrimMonad m, Shape l, MVector v a) => MArray v l (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeLinearModify (MArray _ v) i f = GM.unsafeRead v i >>= GM.unsafeWrite v i . f

-- Filling and copying -------------------------------------------------

-- | Set all elements in a mutable array to a constant value.
set :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> a -> m ()
set (MArray _ v) a = GM.set v a

-- | Copy all elements from one array into another.
copy :: (PrimMonad m, MVector v a) => MArray v l (PrimState m) a -> MArray v l (PrimState m) a -> m ()
copy (MArray _ v) (MArray _ u) = GM.copy v u

-- V1 instances --------------------------------------------------------

-- Array v V1 a is essentially v a with a wrapper. Instance are provided
-- for convience.

instance (MVector v a, l ~ V1) => MVector (MArray v l) a where
  basicLength (MArray (V1 n) _) = n
  basicUnsafeSlice i n (MArray _ v) = MArray (V1 n) $ GM.basicUnsafeSlice i n v
  basicOverlaps (MArray _ v) (MArray _ w) = GM.basicOverlaps v w
  basicUnsafeNew n = MArray (V1 n) <$> GM.basicUnsafeNew n
  basicUnsafeRead (MArray _ v) i = GM.basicUnsafeRead v i
  basicUnsafeWrite (MArray _ v) i a = GM.basicUnsafeWrite v i a
