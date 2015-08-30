{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Shaped
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Base module for shaped vectors. This module exports the constructors
-- for the 'Shaped' data type.
-----------------------------------------------------------------------------
module Data.Shaped.Base
  (
    -- * Array types
    Array (..)
  , Boxed

    -- * Lenses
  , layout
  , vector
  , values

  -- * Functions on arrays

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , Delayed (..)
  , delay
  , manifest
  , genDelayed
  , indexDelayed

  -- * Focused

  , Focused (..)

  ) where


#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative             (pure)
import           Data.Foldable                   (Foldable)
import           Data.Monoid                     (mappend, mempty)
#endif

import           Control.Applicative             (liftA2)
import           Control.Comonad
import           Control.Comonad.Store
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                   (liftM)
import           Control.Monad.Primitive
import           Data.Binary                     as Binary
import           Data.Bytes.Serial
import           Data.Data
import qualified Data.Foldable                   as F
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Serialize                  as Cereal
import           Data.Traversable                (for)
import qualified Data.Vector                     as B
import           Data.Vector.Generic             (Vector)
import qualified Data.Vector.Generic             as G
import           Data.Vector.Generic.Lens        (vectorTraverse)
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Generic.New         as New
import           GHC.Generics                    (Generic, Generic1)
import           Linear                          hiding (vector)
import           Text.ParserCombinators.ReadPrec (readS_to_Prec)
import qualified Text.Read                       as Read

import           Data.Shaped.Index
import           Data.Shaped.Mutable             (MArray (..))

import           Control.Concurrent              (forkOn, getNumCapabilities,
                                                  newEmptyMVar, putMVar,
                                                  takeMVar)
import           System.IO.Unsafe                (unsafePerformIO)

import           Prelude                         hiding (null, replicate,
                                                  zipWith, zipWith3)

-- | An 'Array' is a generic vector with a shape.
data Array v l a = Array !(l Int) !(v a)
  deriving Typeable

-- | Get the shape of an array.
extent :: Array v f a -> f Int
extent (Array l _) = l
{-# INLINE extent #-}

-- Lenses --------------------------------------------------------------

-- | Indexed traversal over the elements of an array. The index is the
--   current position in the array.
values :: (Shape l, Vector v a, Vector w b)
       => IndexedTraversal (l Int) (Array v l a) (Array w l b) a b
values = \f arr -> reindexed (fromIndex $ extent arr) (vector . vectorTraverse) f arr
{-# INLINE values #-}

-- | Lens onto the shape of the vector. The total size of the layout
--   _must_ remain the same (this is not checked).
layout :: Lens (Array v l a) (Array v t a) (l Int) (t Int)
layout f (Array l v) = f l <&> \l' -> Array l' v
{-# INLINE layout #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must _not_ change the length of the
--   vector (even for 'V1' layouts, use '_Flat' for 'V1').
vector :: IndexedLens (l Int) (Array v l a) (Array w l b) (v a) (w b)
vector f (Array l v) = indexed f l v <&> \w -> Array l w
{-# INLINE vector #-}

-- Mutable conversion --------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape l, Vector v a)
       => MArray (G.Mutable v) l (PrimState m) a -> m (Array v l a)
freeze (MArray l mv) = Array l `liftM` G.freeze mv
{-# INLINE freeze #-}

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape l, Vector v a)
     => Array v l a -> m (MArray (G.Mutable v) l (PrimState m) a)
thaw (Array l v) = MArray l `liftM` G.thaw v
{-# INLINE thaw #-}

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Shape l, Vector v a)
             => MArray (G.Mutable v) l (PrimState m) a -> m (Array v l a)
unsafeFreeze (MArray l mv) = Array l `liftM` G.unsafeFreeze mv
{-# INLINE unsafeFreeze #-}

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Shape l, Vector v a)
           => Array v l a -> m (MArray (G.Mutable v) l (PrimState m) a)
unsafeThaw (Array l v) = MArray l `liftM` G.unsafeThaw v
{-# INLINE unsafeThaw #-}

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (Vector v a, Eq1 l, Eq a) => Eq (Array v l a) where
  Array l1 v1 == Array l2 v2 = eq1 l1 l2 && G.eq v1 v2
  {-# INLINE (==) #-}

instance (Vector v a, Show1 l, Show a) => Show (Array v l a) where
  showsPrec p (Array l v2) = showParen (p > 10) $
    showString "Array " . showsPrec1 11 l . showChar ' ' . G.showsPrec 11 v2

type instance Index (Array v l a) = l Int
type instance IxValue (Array v l a) = a

instance (Shape l, Vector v a) => Ixed (Array v l a) where
  ix x f (Array l v)
    | inRange l x = f (G.unsafeIndex v i) <&>
        \a -> Array l (G.modify (\mv -> GM.unsafeWrite mv i a) v)
      where i = toIndex l x
  ix _ _ arr = pure arr
  {-# INLINE ix #-}

instance (Vector v a, Vector v b) => Each (Array v l a) (Array v l b) a b where
  each = vector . vectorTraverse
  {-# INLINE each #-}

instance (Shape l, Vector v a) => AsEmpty (Array v l a) where
  _Empty = nearly (Array zero G.empty) (F.all (==0) . extent)
  {-# INLINE _Empty #-}

instance (Vector v a, Read1 l, Read a) => Read (Array v l a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "Array" <- Read.lexP
    l <- readS_to_Prec readsPrec1
    v <- G.readPrec
    return $ Array l v

instance (NFData (l Int), NFData (v a)) => NFData (Array v l a) where
  rnf (Array l v) = rnf l `seq` rnf v
  {-# INLINE rnf #-}

-- Boxed instances -----------------------------------------------------

-- | A the vector is the boxed vector.
type Boxed v = v ~ B.Vector

instance Boxed v => Functor (Array v l) where
  fmap = over vector . fmap
  {-# INLINE fmap #-}

instance Boxed v => F.Foldable (Array v l) where
  foldMap f = F.foldMap f . view vector
  {-# INLINE foldMap #-}

instance Boxed v => Traversable (Array v l) where
  traverse = each
  {-# INLINE traverse #-}

instance (Boxed v, Eq1 l) => Eq1 (Array v l) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Boxed v, Read1 l) => Read1 (Array v l) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Boxed v, Shape l) => FunctorWithIndex (l Int) (Array v l)
instance (Boxed v, Shape l) => FoldableWithIndex (l Int) (Array v l)
instance (Boxed v, Shape l) => TraversableWithIndex (l Int) (Array v l) where
  itraverse = itraverseOf values
  {-# INLINE itraverse #-}
  itraversed = values
  {-# INLINE itraversed #-}

instance (Boxed v, Foldable l, Serial1 l) => Serial1 (Array v l) where
  serializeWith putF (Array l v) = do
    serializeWith serialize l
    traverseOf_ vectorTraverse putF v
  deserializeWith = genGet (deserializeWith deserialize)

deriving instance (Generic1 v, Generic1 l) => Generic1 (Array v l)

-- instance (v ~ B.Vector, Shape l) => Apply (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Bind (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Additive (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Metric (Array v l) where

-- V1 instances --------------------------------------------------------

-- Array v V1 a is essentially v a with a wrapper.

type instance G.Mutable (Array v l) = MArray (G.Mutable v) l

-- | 1D Arrays can be used as a generic 'Vector'.
instance (Vector v a, l ~ V1) => Vector (Array v l) a where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze                = unsafeFreeze
  basicUnsafeThaw                  = unsafeThaw
  basicLength (Array (V1 n) _)     = n
  basicUnsafeSlice i n (Array _ v) = Array (V1 n) $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (Array _ v)    = G.basicUnsafeIndexM v

-- Serialise instances -------------------------------------------------

instance (Vector v a, Foldable l, Serial1 l, Serial a) => Serial (Array v l a) where
  serialize (Array l v) = do
    serializeWith serialize l
    traverseOf_ vectorTraverse serialize v
  {-# INLINE serialize #-}
  deserialize = genGet (deserializeWith deserialize) deserialize
  {-# INLINE deserialize #-}

instance (Vector v a, Foldable l, Binary (l Int), Binary a) => Binary (Array v l a) where
  put (Array l v) = do
    Binary.put l
    traverseOf_ vectorTraverse Binary.put v
  {-# INLINE put #-}
  get = genGet Binary.get Binary.get
  {-# INLINE get #-}

instance (Vector v a, Foldable l, Serialize (l Int), Serialize a) => Serialize (Array v l a) where
  put (Array l v) = do
    Cereal.put l
    traverseOf_ vectorTraverse Cereal.put v
  {-# INLINE put #-}
  get = genGet Cereal.get Cereal.get
  {-# INLINE get #-}

genGet :: Monad m => (Vector v a, Foldable l) => m (l Int) -> m a -> m (Array v l a)
genGet getL getA = do
  l <- getL
  let n       = F.product l
      nv0     = New.create (GM.new n)
      f acc i = (\a -> New.modify (\mv -> GM.write mv i a) acc) `liftM` getA
  nv <- F.foldlM f nv0 [0 .. n - 1]
  return $! Array l (G.new nv)
{-# INLINE genGet #-}

instance (Vector v a, Foldable l, Hashable a) => Hashable (Array v l a) where
  hashWithSalt s (Array l v) = G.foldl' hashWithSalt s' v
    where s' = F.foldl' hashWithSalt s l
  {-# INLINE hashWithSalt #-}

deriving instance (Generic (v a), Generic1 l) => Generic (Array v l a)
deriving instance (Typeable l, Typeable v, Typeable a, Data (l Int), Data (v a)) => Data (Array v l a)


-- instance (Vector v a, Typeable v, Typeable l, Shape l, Data a) => Data (Array v l a) where
--   gfoldl f z (Array l a) =
--     z (\l' a' -> Array (l & partsOf traverse .~ l') (G.fromList a')) `f` F.toList l `f` G.toList a
--   gunfold k z _ = k (k (z (\l a -> Array (zero & partsOf traverse .~ l) (G.fromList a))))
--   toConstr _ = con
--   dataTypeOf _ = ty
--   dataCast1 = gcast1

-- ty :: DataType
-- ty = mkDataType "Array" [con]

-- con :: Constr
-- con = mkConstr ty "Array" [] Prefix

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | A delayed representation of an array. This useful for mapping over
--   an array in parallel.
data Delayed l a = Delayed !(l Int) (Int -> a)
  deriving (Typeable, Functor)

-- | Turn a material array into a delayed one with the same shape.
delay :: (Vector v a, Shape l) => Array v l a -> Delayed l a
delay (Array l v) = Delayed l (G.unsafeIndex v)
{-# INLINE delay #-}

-- | 'foldMap' in parallel.
instance Shape l => Foldable (Delayed l) where
  foldr f b (Delayed l ixF) = go 0 where
    go i
      | i >= n     = b
      | otherwise = f (ixF i) (go (i+1))
    n = F.product l
  {-# INLINE foldr #-}

  foldMap f (Delayed l ixF) = unsafePerformIO $ do
    childs <- for [0 .. threads - 1] $ \c -> do
      child <- newEmptyMVar
      _ <- forkOn c $ do
        let k | c == threads - 1 = q + r
              | otherwise        = q
            x = c * q
            m = x + k
            go i !acc
              | i >= m    = acc
              | otherwise = go (i+1) (acc `mappend` f (ixF i))
        putMVar child $! go x mempty
      return child
    F.fold <$> for childs takeMVar
    where
    !n       = F.product l
    !(q, r)  = n `quotRem` threads
    !threads = unsafePerformIO getNumCapabilities
  {-# INLINE foldMap #-}

instance (Shape l, Show1 l, Show a) => Show (Delayed l a) where
  showsPrec p arr@(Delayed l _) = showParen (p > 10) $
    showString "Delayed " . showsPrec1 11 l . showChar ' ' . showsPrec 11 (F.toList arr)

instance (Shape l, Show1 l) => Show1 (Delayed l) where
  showsPrec1 = showsPrec

instance Shape l => Traversable (Delayed l) where
  traverse f arr = delay <$> traversed f (manifest arr)

instance Shape l => Additive (Delayed l) where
  zero = _Empty # ()
  {-# INLINE zero #-}

  liftU2 f (Delayed l ixF) (Delayed k ixG)
    | l `eq1` k = Delayed l (liftA2 f ixF ixG)
    | otherwise = Delayed (liftU2 max l k) $ \i ->
        let x = fromIndex q i
        in if | inRange q x -> liftA2 f ixF ixG i
              | inRange l x -> ixF i
              | otherwise   -> ixG i
    where q = intersectShape l k
  {-# INLINE liftU2 #-}

  liftI2 f (Delayed l ixF) (Delayed k ixG) = Delayed (intersectShape l k) $ liftA2 f ixF ixG
  {-# INLINE liftI2 #-}

instance Shape l => FunctorWithIndex (l Int) (Delayed l) where
  imap f (Delayed l ixF) = Delayed l $ \i -> f (fromIndex l i) (ixF i)
  {-# INLINE imap #-}

instance Shape l => FoldableWithIndex (l Int) (Delayed l) where
  ifoldr f b (Delayed l ixF) = ifoldrOf enumShape (\i x -> f x (ixF i)) b l
  {-# INLINE ifoldr #-}

  ifolded = ifoldring ifoldr
  {-# INLINE ifolded #-}

  ifoldMap = ifoldMapOf ifolded
  {-# INLINE ifoldMap #-}

instance Shape l => TraversableWithIndex (l Int) (Delayed l) where
  itraverse f arr = delay <$> itraverse f (manifest arr)
  {-# INLINE itraverse #-}

instance Shape l => Each (Delayed l a) (Delayed l b) a b where
  each = traversed
  {-# INLINE each #-}

instance Shape l => AsEmpty (Delayed l a) where
  _Empty = nearly (Delayed zero (error "empty delayed array"))
                  (\(Delayed l _) -> F.all (==0) l)
  {-# INLINE _Empty #-}

type instance Index (Delayed l a) = l Int
type instance IxValue (Delayed l a) = a
instance Shape l => Ixed (Delayed l a) where
  ix x f arr@(Delayed l ixF)
    | inRange l x = f (ixF i) <&> \a ->
      let g j | j == i    = a
              | otherwise = ixF j
      in  Delayed l g
    | otherwise   = pure arr
    where i = toIndex l x
  {-# INLINE ix #-}

-- | Index a delayed array, returning a 'IndexOutOfBounds' exception if
--   the index is out of range.
indexDelayed :: Shape l => l Int -> Delayed l a -> a
indexDelayed x (Delayed l ixF) =
  boundsCheck l x $ ixF (toIndex l x)
{-# INLINE indexDelayed #-}

-- | Parallel manifestation of a delayed array into a material one.
manifest :: (Vector v a, Shape l) => Delayed l a -> Array v l a
manifest (Delayed l ixF) = Array l v
  where
    !v = unsafePerformIO $! do
      mv <- GM.new n
      childs <- for [0 .. threads - 1] $ \c -> do
        child <- newEmptyMVar
        _ <- forkOn c $ do
          let k | c == 0    = q + r
                | otherwise = q
              x = c * q
          F.for_ [x .. x + k - 1] $ \i -> GM.unsafeWrite mv i $! ixF i
          putMVar child ()
        return child
      F.for_ childs takeMVar
      G.unsafeFreeze mv
    !n       = F.product l
    !(q, r)  = n `quotRem` threads
    !threads = unsafePerformIO getNumCapabilities
{-# INLINE manifest #-}

genDelayed :: Shape l => l Int -> (l Int -> a) -> Delayed l a
genDelayed l f = Delayed l (f . fromIndex l)
{-# INLINE genDelayed #-}

------------------------------------------------------------------------
-- Focused
------------------------------------------------------------------------

-- | A delayed representation of an array with a focus on a single
--   element. This element is the target of 'extract'.
data Focused l a = Focused !(l Int) !(Delayed l a)
  deriving (Typeable, Functor)

instance Shape l => Comonad (Focused l) where
  {-# INLINE extract #-}
  {-# INLINE extend  #-}
  extract (Focused x d) = indexDelayed x d
  extend f (Focused x d@(Delayed l _)) =
    Focused x (genDelayed l $ \i -> f (Focused i d))

instance Shape l => ComonadStore (l Int) (Focused l) where
  {-# INLINE pos   #-}
  {-# INLINE peek  #-}
  {-# INLINE peeks #-}
  {-# INLINE seek  #-}
  {-# INLINE seeks #-}
  pos     (Focused x _) = x
  peek  x (Focused _ d) = indexDelayed x d
  peeks f (Focused x d) = indexDelayed (f x) d
  seek  x (Focused _ d) = Focused x d
  seeks f (Focused x d) = Focused (f x) d

instance (Shape l, Show1 l, Show a) => Show (Focused l a) where
  showsPrec p (Focused l d) = showParen (p > 10) $
    showString "Focused " . showsPrec1 11 l . showsPrec 11 d

instance (Shape l, Show1 l) => Show1 (Focused l) where
  showsPrec1 = showsPrec

type instance Index (Focused l a) = l Int
type instance IxValue (Focused l a) = a

instance Shape l => Foldable (Focused l) where
  foldr f b (Focused _ d) = F.foldr f b d
  {-# INLINE foldr #-}

instance Shape l => Traversable (Focused l) where
  traverse f (Focused u d) = Focused u <$> traverse f d
  {-# INLINE traverse #-}

-- | Relative to focus.
instance Shape l => Ixed (Focused l a) where
  ix i f (Focused u d) = Focused u <$> ix (i ^+^ u) f d
  {-# INLINE ix #-}

