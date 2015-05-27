{-# LANGUAGE DeriveDataTypeable    #-}
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
-- Simple wrapper over a generic vector, giving the vector a 'Shape'.
-- Currently the api is a bit sparse but many standard functions that
-- appear missing can be used with the 'values' traversal with the
-- functions in the lens library.
-----------------------------------------------------------------------------
module Data.Vector.Shaped
  (
    -- * Array types
    Array (..)
  , BArray
  , UArray
  , SArray
  , PArray

    -- * Lenses
  , layout
  , linear
  , values
  , values'
  , _Flat

  -- * Construction
  -- ** Lists
  , fromList
  , fromListInto

  -- ** Initialisation
  , replicate
  , generate

  -- ** Monadic initialisation
  , create
  , replicateM
  , generateM

  -- ** Mutable
  , MArray (..)
  , M.BMArray
  , M.UMArray
  , M.SMArray
  , M.PMArray
  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- ** Zipping
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3

  -- * Common layouts
  , V1 (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)

  ) where


import           Control.Applicative             (pure)
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                   (liftM)
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Binary                     as Binary
import           Data.Bytes.Serial
import           Data.Data
import           Data.Foldable                   (Foldable)
import qualified Data.Foldable                   as F
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Serialize                  as Cereal
import qualified Data.Vector                     as B
import           Data.Vector.Generic             (Vector)
import qualified Data.Vector.Generic             as G
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Generic.New         as New
import qualified Data.Vector.Primitive           as P
import qualified Data.Vector.Storable            as S
import qualified Data.Vector.Unboxed             as U
import           GHC.Generics                    (Generic, Generic1)
import           Linear
import           Text.ParserCombinators.ReadPrec (readS_to_Prec)
import qualified Text.Read                       as Read

import           Data.Vector.Shaped.Index
import           Data.Vector.Shaped.Mutable      (MArray (..))
import qualified Data.Vector.Shaped.Mutable      as M

import           Prelude                         hiding (null, replicate,
                                                  zipWith, zipWith3)

-- | Generic vector with a shape. The constructor is exported but it is
--   not safe use. Use 'fromListInto' or 'vector'.
data Array v l a = Array !(l Int) !(v a)
  deriving Typeable

-- | Boxed array.
type BArray = Array B.Vector

-- | 'Unboxed' array.
type UArray = Array U.Vector

-- | 'Storeable' array.
type SArray = Array S.Vector

-- | 'Primitive' array.
type PArray = Array P.Vector

-- | Get the shape of an array.
extent :: Array v f a -> f Int
extent (Array l _) = l

-- Lenses --------------------------------------------------------------

-- | Indexed traversal over the elements of an array. The index is the
--   current position in the array.
values :: (Shape l, Vector v a, Vector w b)
       => IndexedTraversal (l Int) (Array v l a) (Array w l b) a b
values f arr = reindexed (fromIndex $ extent arr) (linear . vectorValues) f arr
{-# INLINE values #-}

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape l, Vector v a, Vector v b)
       => IndexedTraversal (l Int) (Array v l a) (Array v l b) a b
values' = values
{-# INLINE values' #-}

vectorValues :: (Vector v a, Vector w b) => IndexedTraversal Int (v a) (w b) a b
vectorValues = conjoined l (indexing l)
  where l f v = traverse f (G.toList v) <&> G.fromListN (G.length v)
{-# INLINE vectorValues #-}

-- | Lens onto the shape of the vector. The total size of the layout
--   _must_ remain the same (this is not checked).
layout :: Lens (Array v l a) (Array v t a) (l Int) (t Int)
layout f (Array l v) = f l <&> \l' -> Array l' v
{-# INLINE layout #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must _not_ change the length of the
--   vector (even for 'V1' layouts, use '_Flat' for 'V1').
linear :: IndexedLens (l Int) (Array v l a) (Array w l b) (v a) (w b)
linear f (Array l v) = indexed f l v <&> \w -> Array l w
{-# INLINE linear #-}

-- | 1D arrays are just vectors. You are free to change the length of
--   the vector when going 'over' this 'Iso' (unlike 'linear').
--
--   Note that 'V1' arrays are an instance of 'Vector' so you can use
--   any of the functions in 'Data.Vector.Generic' on them without
--   needing to convert.
_Flat :: (Vector v a, Vector w b) => Iso (Array v V1 a) (Array w V1 b) (v a) (w b)
_Flat = iso (\(Array _ v) -> v) (\v -> Array (V1 $ G.length v) v)
{-# INLINE _Flat #-}

-- Constructing vectors ------------------------------------------------

-- | Contruct a flat array from a list. (This is just 'G.fromList' from
--   'Data.Vector.Generic'.)
fromList :: Vector v a => [a] -> Array v V1 a
fromList = G.fromList
{-# INLINE fromList #-}

-- | O(n) Convert the first @n@ elements of a list to an Array with the
--   given shape. Returns 'Nothing' if there are not enough elements in
--   the list.
fromListInto :: (Shape l, Vector v a) => l Int -> [a] -> Maybe (Array v l a)
fromListInto l as
  | G.length v == n = Just $ Array l v
  | otherwise       = Nothing
  where v = G.fromListN n as
        n = F.product l
{-# INLINE fromListInto #-}

-- | The empty 'Array' with a 'zeroDim' shape.
empty :: (Vector v a, Additive l) => Array v l a
empty = Array zero G.empty
{-# INLINE empty #-}

-- | Test is if the array is 'empty'.
null :: Foldable l => Array v l a -> Bool
null (Array l _) = F.all (==0) l
{-# INLINE null #-}

-- Initialisation ------------------------------------------------------

-- | Execute the monadic action and freeze the resulting array.
create :: (Vector v a, Shape l)
       => (forall s. ST s (MArray (G.Mutable v) l s a)) -> Array v l a
create m = m `seq` runST (m >>= unsafeFreeze)
{-# INLINE create #-}

-- | O(n) Array of the given shape with the same value in each position.
replicate :: (Shape l, Vector v a) => l Int -> a -> Array v l a
replicate l a
  | n > 0     = Array l $ G.replicate n a
  | otherwise = empty
  where n = F.product l
{-# INLINE replicate #-}

-- | O(n) Construct an array of the given shape by applying the
--   function to each index.
generate :: (Shape l, Vector v a) => l Int -> (l Int -> a) -> Array v l a
generate l f
  | n > 0     = Array l $ G.generate n (f . fromIndex l)
  | otherwise = empty
  where n = F.product l
{-# INLINE generate #-}

-- Monadic initialisation ----------------------------------------------

-- | O(n) Construct an array of the given shape by filling each position
--   with the monadic value.
replicateM :: (Monad m, Shape l, Vector v a) => l Int -> m a -> m (Array v l a)
replicateM l a
  | n > 0     = Array l `liftM` G.replicateM n a
  | otherwise = return empty
  where n = F.product l
{-# INLINE replicateM #-}

-- | O(n) Construct an array of the given shape by applying the monadic
--   function to each index.
generateM :: (Monad m, Shape l, Vector v a) => l Int -> (l Int -> m a) -> m (Array v l a)
generateM l f
  | n > 0     = Array l `liftM` G.generateM n (f . fromIndex l)
  | otherwise = return empty
  where n = F.product l
{-# INLINE generateM #-}

-- Mutable conversion --------------------------------------------------

-- | O(n) Yield a mutable copy of the immutable vector.
freeze :: (PrimMonad m, Shape l, Vector v a)
       => MArray (G.Mutable v) l (PrimState m) a -> m (Array v l a)
freeze (MArray l mv) = Array l `liftM` G.freeze mv

-- | O(n) Yield an immutable copy of the mutable array.
thaw :: (PrimMonad m, Shape l, Vector v a)
     => Array v l a -> m (MArray (G.Mutable v) l (PrimState m) a)
thaw (Array l v) = MArray l `liftM` G.thaw v

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Shape l, Vector v a)
             => MArray (G.Mutable v) l (PrimState m) a -> m (Array v l a)
unsafeFreeze (MArray l mv) = Array l `liftM` G.unsafeFreeze mv

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Shape l, Vector v a)
           => Array v l a -> m (MArray (G.Mutable v) l (PrimState m) a)
unsafeThaw (Array l v) = MArray l `liftM` G.unsafeThaw v

-- Zipping -------------------------------------------------------------

zipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
        => (a -> b -> c)
        -> Array v l a
        -> Array v l b
        -> Array v l c
zipWith f (Array l1 v1) (Array l2 v2)
  | eq1 l1 l1 = Array l1 $ G.zipWith f v1 v2
  | otherwise = Array l' $ error "intersect zipWith not yet implimented"
  where l' = intersectShape l1 l2

zipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d)
         -> Array v l a
         -> Array v l b
         -> Array v l c
         -> Array v l d
zipWith3 f (Array l1 v1) (Array l2 v2) (Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.zipWith3 f v1 v2 v3
  | otherwise = Array l' $ error "intersect zipWith not yet implimented"
  where l' = intersectShape l1 l2 `intersectShape` l3


izipWith :: (Shape l, Vector v a, Vector v b, Vector v c)
         => (l Int -> a -> b -> c)
         -> Array v l a
         -> Array v l b
         -> Array v l c
izipWith f (Array l1 v1) (Array l2 v2)
  | eq1 l1 l2 = Array l1 $ G.izipWith (f . fromIndex l1) v1 v2
  | otherwise = error "izipWith not yet implimented for different shaped arrays"
{-# INLINE izipWith #-}

izipWith3 :: (Shape l, Vector v a, Vector v b, Vector v c, Vector v d)
          => (l Int -> a -> b -> c -> d)
          -> Array v l a
          -> Array v l b
          -> Array v l c
          -> Array v l d
izipWith3 f (Array l1 v1) (Array l2 v2) (Array l3 v3)
  | eq1 l1 l2 &&
    eq1 l2 l3 = Array l1 $ G.izipWith3 (f . fromIndex l1) v1 v2 v3
  | otherwise = error "izipWith3 not yet implimented for different shaped arrays"
{-# INLINE izipWith3 #-}

-- Slices --------------------------------------------------------------

-- -- XXX Needs to be checked

-- Index starts at l1.
-- slicedValues :: l Int -> l Int -> IndexedTraversal (l Int) (Array v l a) a
-- sliced

-- Index starts at 0
-- dicedValues :: l Int -> l Int -> IndexedTraversal (l Int) (Array v l a) a

-- slice :: (Vector v a, Vector w a, Shape l1, Shape l2)
--       => Getting (l2 Int) (l1 Int) (l2 Int)
--       -> (l2 Int -> l1 Int)
--       -> Array v l1 a
--       -> Array w l2 a
-- slice l f arr@(Array l1 _) = generate l2 $ \x -> arr ^?! ix (f x)
--   where l2 = l1 ^. l

-- column :: Vector v a => Int -> Array v V2 a -> Array v V1 a
-- column y = slice (_x . to V1) (\(V1 x) -> V2 x y)

-- row :: Vector v a => Int -> Array v V2 a -> Array v V1 a
-- row x = slice (_y . to V1) (\(V1 y) -> V2 x y)

-- plane :: (Vector v a, Vector w a)
--       => ALens' (V3 Int) (V2 Int)
--       -> Int
--       -> Array v V3 a
--       -> Array w V2 a -- Lens' (V3 Int) (V2 Int) =>
-- plane l n = slice getter (\xx -> x & l #~ xx)
--   where x      = n <$ (zero :: Additive f => f Int)
--         getter = cloneLens l

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (Vector v a, Eq1 l, Eq a) => Eq (Array v l a) where
  Array l1 v1 == Array l2 v2 = eq1 l1 l2 && G.eq v1 v2

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
  each = linear . vectorValues
  {-# INLINE each #-}

instance (Shape l, Vector v a) => AsEmpty (Array v l a) where
  _Empty = nearly empty null
  {-# INLINE _Empty #-}

instance (Vector v a, Read1 l, Read a) => Read (Array v l a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "Array" <- Read.lexP
    l <- readS_to_Prec readsPrec1
    v <- G.readPrec
    return $ Array l v

instance (NFData (l Int), NFData (v a)) => NFData (Array v l a) where
  rnf (Array l v) = rnf l `seq` rnf v

-- Boxed instances -----------------------------------------------------

instance v ~ B.Vector => Functor (Array v l) where
  fmap = over linear . fmap

instance v ~ B.Vector => F.Foldable (Array v l) where
  foldMap f = F.foldMap f . view linear
  -- foldr f = foldr f . view linear

instance v ~ B.Vector => Traversable (Array v l) where
  traverse = each

instance (v ~ B.Vector, Eq1 l) => Eq1 (Array v l) where
  eq1 = (==)

instance (v ~ B.Vector, Read1 l) => Read1 (Array v l) where
  readsPrec1 = readsPrec

instance (v ~ B.Vector, Shape l) => FunctorWithIndex (l Int) (Array v l)
instance (v ~ B.Vector, Shape l) => FoldableWithIndex (l Int) (Array v l)
instance (v ~ B.Vector, Shape l) => TraversableWithIndex (l Int) (Array v l) where
  itraverse = itraverseOf values
  {-# INLINE itraverse #-}
  itraversed = values
  {-# INLINE itraversed #-}

instance (v ~ B.Vector, Foldable l, Serial1 l) => Serial1 (Array v l) where
  serializeWith putF (Array l v) = do
    serializeWith serialize l
    traverseOf_ vectorValues putF v
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
  basicUnsafeFreeze                = unsafeFreeze
  basicUnsafeThaw                  = unsafeThaw
  basicLength (Array (V1 n) _)     = n
  basicUnsafeSlice i n (Array _ v) = Array (V1 n) $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (Array _ v)    = G.basicUnsafeIndexM v

-- Serialise instances -------------------------------------------------

instance (Vector v a, Foldable l, Serial1 l, Serial a) => Serial (Array v l a) where
  serialize (Array l v) = do
    serializeWith serialize l
    traverseOf_ vectorValues serialize v
  {-# INLINE serialize #-}
  deserialize = genGet (deserializeWith deserialize) deserialize
  {-# INLINE deserialize #-}

instance (Vector v a, Foldable l, Binary (l Int), Binary a) => Binary (Array v l a) where
  put (Array l v) = do
    Binary.put l
    traverseOf_ vectorValues Binary.put v
  {-# INLINE put #-}
  get = genGet Binary.get Binary.get
  {-# INLINE get #-}

instance (Vector v a, Foldable l, Serialize (l Int), Serialize a) => Serialize (Array v l a) where
  put (Array l v) = do
    Cereal.put l
    traverseOf_ vectorValues Cereal.put v
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

-- instance (Vector v a, Shape l, Unboxed a) => Unboxed (Array v l a) where
-- instance (Vector v a, Shape l, Storable a) => Storable (Array v l a) where

instance (Vector v a, Foldable l, Hashable a) => Hashable (Array v l a) where
  hashWithSalt s (Array l v) = G.foldl' hashWithSalt s' v
    where s' = F.foldl' hashWithSalt s l
  {-# INLINE hashWithSalt #-}

deriving instance (Generic (v a), Generic1 l) => Generic (Array v l a)
deriving instance (Typeable l, Typeable v, Typeable a, Data (l Int), Data (v a)) => Data (Array v l a)

