{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dense.Base
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Base module for multidimensional arrays. This module exports the
-- constructors for the 'Array' data type.
--
-- Also, to prevent this module becomming too large, only the data types
-- and the functions nessesary for the instances are defined here. All
-- other functions are defined in "Data.Dense.Generic".
-----------------------------------------------------------------------------
module Data.Dense.Base
  (
    -- * Array types
    Array (..)
  , Boxed

    -- ** Lenses
  , vector
  , values

  -- ** Conversion to/from mutable arrays

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
import           Control.Applicative             (pure, (*>))
import           Data.Foldable                   (Foldable)
import           Data.Monoid                     (Monoid, mappend, mempty)
#endif

import           Control.Applicative             (liftA2)
import           Control.Comonad
import           Control.Comonad.Store
import           Control.DeepSeq
import           Control.Lens
import           Control.Lens.Internal           (noEffect)
import           Control.Monad                   (guard, liftM)
import           Control.Monad.Primitive
import           Data.Binary                     as Binary
import           Data.Bytes.Serial
import           Data.Data
import qualified Data.Foldable                   as F
import           Data.Functor.Apply
import           Data.Functor.Classes
import           Data.Functor.Extend
import           Data.Hashable
import           Data.Serialize                  as Cereal
import           Data.Traversable                (for)
import qualified Data.Vector                     as B
import           Data.Vector.Generic             (Vector)
import qualified Data.Vector.Generic             as G
import           Data.Vector.Generic.Lens        (vectorTraverse)
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Generic.New         as New
-- import           GHC.Generics                    (Generic, Generic1)
import           Linear                          hiding (vector)
import           Text.ParserCombinators.ReadPrec (readS_to_Prec)
import qualified Text.Read                       as Read

import           Data.Dense.Index
import           Data.Dense.Mutable              (MArray (..))

import           Control.Concurrent              (forkOn, getNumCapabilities,
                                                  newEmptyMVar, putMVar,
                                                  takeMVar)
import           System.IO.Unsafe                (unsafePerformIO)

import           Prelude                         hiding (null, replicate,
                                                  zipWith, zipWith3)

import           GHC.Types                       (SPEC (..))

-- | An 'Array' is a vector with a shape.
data Array v f a = Array !(Layout f) !(v a)
  deriving Typeable

-- Lenses --------------------------------------------------------------

-- | Indexed traversal over the elements of an array. The index is the
--   current position in the array.
values :: (Shape f, Vector v a, Vector w b)
       => IndexedTraversal (f Int) (Array v f a) (Array w f b) a b
values = \f arr -> reindexed (shapeFromIndex $ extent arr) (vector . vectorTraverse) f arr
{-# INLINE values #-}

-- | Indexed lens over the underlying vector of an array. The index is
--   the 'extent' of the array. You must _not_ change the length of the
--   vector, otherwise an error will be thrown (even for 'V1' layouts,
--   use 'flat' for 'V1').
vector :: (Vector v a, Vector w b) => IndexedLens (Layout f) (Array v f a) (Array w f b) (v a) (w b)
vector f (Array l v) =
  indexed f l v <&> \w ->
  sizeMissmatch (G.length v) (G.length w)
     ("vector: trying to replace vector of length " ++ show (G.length v) ++ " with one of length " ++ show (G.length w))
     $ Array l w
{-# INLINE vector #-}

-- Mutable conversion --------------------------------------------------

-- | O(1) Unsafe convert a mutable array to an immutable one without
-- copying. The mutable array may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Vector v a)
             => MArray (G.Mutable v) f (PrimState m) a -> m (Array v f a)
unsafeFreeze (MArray l mv) = Array l `liftM` G.unsafeFreeze mv
{-# INLINE unsafeFreeze #-}

-- | O(1) Unsafely convert an immutable array to a mutable one without
--   copying. The immutable array may not be used after this operation.
unsafeThaw :: (PrimMonad m, Vector v a)
           => Array v f a -> m (MArray (G.Mutable v) f (PrimState m) a)
unsafeThaw (Array l v) = MArray l `liftM` G.unsafeThaw v
{-# INLINE unsafeThaw #-}

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- | The 'size' of the 'layout' __must__ remain the same or an error is thrown.
instance Shape f => HasLayout f (Array v f a) where
  layout f (Array l v) = f l <&> \l' ->
    sizeMissmatch (shapeSize l) (shapeSize l')
      ("layout (Array): trying to replace shape " ++ showShape l ++ " with " ++ showShape l')
      $ Array l' v
  {-# INLINE layout #-}

-- layout :: (Shape l, Shape t) => Lens (Array v l a) (Array v t a) (Layout l) (Layout t)

instance (Vector v a, Eq1 f, Eq a) => Eq (Array v f a) where
  Array l1 v1 == Array l2 v2 = eq1 l1 l2 && G.eq v1 v2
  {-# INLINE (==) #-}

instance (Vector v a, Show1 f, Show a) => Show (Array v f a) where
  showsPrec p (Array l v2) = showParen (p > 10) $
    showString "Array " . showsPrec1 11 l . showChar ' ' . G.showsPrec 11 v2

type instance Index (Array v f a) = f Int
type instance IxValue (Array v f a) = a

instance (Shape f, Vector v a) => Ixed (Array v f a) where
  ix x f (Array l v)
    | shapeInRange l x = f (G.unsafeIndex v i) <&>
        \a -> Array l (G.modify (\mv -> GM.unsafeWrite mv i a) v)
      where i = shapeToIndex l x
  ix _ _ arr = pure arr
  {-# INLINE ix #-}

instance (Vector v a, Vector v b) => Each (Array v f a) (Array v f b) a b where
  each = vector . vectorTraverse
  {-# INLINE each #-}

instance (Shape f, Vector v a) => AsEmpty (Array v f a) where
  _Empty = nearly (Array zero G.empty) (F.all (==0) . extent)
  {-# INLINE _Empty #-}

instance (Vector v a, Read1 f, Read a) => Read (Array v f a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "Array" <- Read.lexP
    l <- readS_to_Prec readsPrec1
    v <- G.readPrec
    return $ Array l v

instance (NFData (f Int), NFData (v a)) => NFData (Array v f a) where
  rnf (Array l v) = rnf l `seq` rnf v
  {-# INLINE rnf #-}

-- Boxed instances -----------------------------------------------------

-- | The vector is the boxed vector.
type Boxed v = v ~ B.Vector

instance Boxed v => Functor (Array v f) where
  fmap = over vector . fmap
  {-# INLINE fmap #-}

instance Boxed v => F.Foldable (Array v f) where
  foldMap f = F.foldMap f . view vector
  {-# INLINE foldMap #-}

instance Boxed v => Traversable (Array v f) where
  traverse = each
  {-# INLINE traverse #-}

#if (MIN_VERSION_transformers(0,5,0)) || !(MIN_VERSION_transformers(0,4,0))
instance (Boxed v, Eq1 f) => Eq1 (Array v f) where
  liftEq f (Array l1 v1) (Array l2 v2) = eq1 l1 l2 && G.and (G.zipWith f v1 v2)
  {-# INLINE liftEq #-}

instance (Boxed v, Read1 f) => Read1 (Array v f) where
  liftReadsPrec _ f = readsData $ readsBinaryWith readsPrec1 (const f) "Array" (\c l -> Array c (G.fromList l))
  {-# INLINE liftReadsPrec #-}
#else
instance (Boxed v, Eq1 f) => Eq1 (Array v f) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Boxed v, Read1 f) => Read1 (Array v f) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}
#endif

instance (Boxed v, Shape f) => FunctorWithIndex (f Int) (Array v f)
instance (Boxed v, Shape f) => FoldableWithIndex (f Int) (Array v f)
instance (Boxed v, Shape f) => TraversableWithIndex (f Int) (Array v f) where
  itraverse = itraverseOf values
  {-# INLINE itraverse #-}
  itraversed = values
  {-# INLINE itraversed #-}

instance (Boxed v, Shape f, Serial1 f) => Serial1 (Array v f) where
  serializeWith putF (Array l v) = do
    serializeWith serialize l
    F.traverse_ putF v
  deserializeWith = genGet (deserializeWith deserialize)

-- deriving instance (Generic1 v, Generic1 f) => Generic1 (Array v f)

-- instance (v ~ B.Vector, Shape l) => Apply (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Bind (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Additive (Array v l) where
-- instance (v ~ B.Vector, Shape l) => Metric (Array v l) where

-- V1 instances --------------------------------------------------------

-- Array v V1 a is essentially v a with a wrapper.

type instance G.Mutable (Array v f) = MArray (G.Mutable v) f

-- | 1D Arrays can be used as a generic 'Vector'.
instance (Vector v a, f ~ V1) => Vector (Array v f) a where
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

instance (Vector v a, Shape f, Serial1 f, Serial a) => Serial (Array v f a) where
  serialize (Array l v) = do
    serializeWith serialize l
    traverseOf_ vectorTraverse serialize v
  {-# INLINE serialize #-}
  deserialize = genGet (deserializeWith deserialize) deserialize
  {-# INLINE deserialize #-}

instance (Vector v a, Shape f, Binary (f Int), Binary a) => Binary (Array v f a) where
  put (Array l v) = do
    Binary.put l
    traverseOf_ vectorTraverse Binary.put v
  {-# INLINE put #-}
  get = genGet Binary.get Binary.get
  {-# INLINE get #-}

instance (Vector v a, Shape f, Serialize (f Int), Serialize a) => Serialize (Array v f a) where
  put (Array l v) = do
    Cereal.put l
    traverseOf_ vectorTraverse Cereal.put v
  {-# INLINE put #-}
  get = genGet Cereal.get Cereal.get
  {-# INLINE get #-}

genGet :: Monad m => (Vector v a, Shape f) => m (f Int) -> m a -> m (Array v f a)
genGet getL getA = do
  l <- getL
  let n       = shapeSize l
      nv0     = New.create (GM.new n)
      f acc i = (\a -> New.modify (\mv -> GM.write mv i a) acc) `liftM` getA
  nv <- F.foldlM f nv0 [0 .. n - 1]
  return $! Array l (G.new nv)
{-# INLINE genGet #-}

instance (Vector v a, Foldable f, Hashable a) => Hashable (Array v f a) where
  hashWithSalt s (Array l v) = G.foldl' hashWithSalt s' v
    where s' = F.foldl' hashWithSalt s l
  {-# INLINE hashWithSalt #-}

-- deriving instance (Generic (v a), Generic1 f) => Generic (Array v f a)
deriving instance (Typeable f, Typeable v, Typeable a, Data (f Int), Data (v a)) => Data (Array v f a)


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
data Delayed f a = Delayed !(Layout f) (f Int -> a)
  deriving (Typeable, Functor)

-- | Turn a material array into a delayed one with the same shape.
delay :: (Vector v a, Shape f) => Array v f a -> Delayed f a
delay (Array l v) = Delayed l (G.unsafeIndex v . shapeToIndex l)
{-# INLINE delay #-}

-- | The 'size' of the 'layout' __must__ remain the same or an error is thrown.
instance Shape f => HasLayout f (Delayed f a) where
  layout f (Delayed l ixF) = f l <&> \l' ->
    sizeMissmatch (shapeSize l) (shapeSize l')
      ("layout (Delayed): trying to replace shape " ++ showShape l ++ " with " ++ showShape l')
      $ Delayed l' ixF
  {-# INLINE layout #-}

-- | 'foldMap' in parallel.
instance Shape f => Foldable (Delayed f) where
  foldr f b (Delayed l ixF) = foldrOf shapeIndexes (\x -> f (ixF x)) b l
  {-# INLINE foldr #-}

  foldMap = foldDelayed . const

#if __GLASGOW_HASKELL__ >= 710
  length = size
  {-# INLINE length #-}
#endif

instance (Shape f, Show1 f, Show a) => Show (Delayed f a) where
  showsPrec p arr@(Delayed l _) = showParen (p > 10) $
    showString "Delayed " . showsPrec1 11 l . showChar ' ' . showsPrec 11 (F.toList arr)

-- instance (Shape f, Show1 f) => Show1 (Delayed f) where
--   showsPrec1 = showsPrec

instance Shape f => Traversable (Delayed f) where
  traverse f arr = delay <$> traversed f (manifest arr)

instance Shape f => Apply (Delayed f) where
  {-# INLINE (<.>) #-}
  {-# INLINE (<. ) #-}
  {-# INLINE ( .>) #-}
  (<.>) = liftI2 id
  (<. ) = liftI2 const
  ( .>) = liftI2 (const id)

instance Shape f => Additive (Delayed f) where
  zero = _Empty # ()
  {-# INLINE zero #-}

  -- This can only be satisfied on if one array is larger than the other
  -- in all dimensions, otherwise there will be gaps in the array
  liftU2 f (Delayed l ixF) (Delayed k ixG)
    | l `eq1` k       = Delayed l (liftA2 f ixF ixG)

    -- l > k
    | F.all (>= EQ) cmp = Delayed l $ \x ->
        if | shapeInRange l x -> liftA2 f ixF ixG x
           | otherwise        -> ixF x

    -- k > l
    | F.all (<= EQ) cmp = Delayed k $ \x ->
        if | shapeInRange k x -> liftA2 f ixF ixG x
           | otherwise        -> ixG x

    -- not possible to union array sizes because there would be gaps,
    -- just intersect them instead
    | otherwise       = Delayed (shapeIntersect l k) $ liftA2 f ixF ixG
    where cmp = liftI2 compare l k

  liftI2 f (Delayed l ixF) (Delayed k ixG) = Delayed (shapeIntersect l k) $ liftA2 f ixF ixG
  {-# INLINE liftI2 #-}

instance Shape f => Metric (Delayed f)

instance FunctorWithIndex (f Int) (Delayed f) where
  imap f (Delayed l ixF) = Delayed l $ \x -> f x (ixF x)
  {-# INLINE imap #-}

-- | 'ifoldMap' in parallel.
instance Shape f => FoldableWithIndex (f Int) (Delayed f) where
  ifoldr f b (Delayed l ixF) = foldrOf shapeIndexes (\x -> f x (ixF x)) b l
  {-# INLINE ifoldr #-}

  ifolded = ifoldring ifoldr
  {-# INLINE ifolded #-}

  ifoldMap = foldDelayed
  {-# INLINE ifoldMap #-}

instance Shape f => TraversableWithIndex (f Int) (Delayed f) where
  itraverse f arr = delay <$> itraverse f (manifest arr)
  {-# INLINE itraverse #-}

instance Shape f => Each (Delayed f a) (Delayed f b) a b where
  each = traversed
  {-# INLINE each #-}

instance Shape f => AsEmpty (Delayed f a) where
  _Empty = nearly (Delayed zero (error "empty delayed array"))
                  (\(Delayed l _) -> F.all (==0) l)
  {-# INLINE _Empty #-}

type instance Index (Delayed f a) = f Int
type instance IxValue (Delayed f a) = a
instance Shape f => Ixed (Delayed f a) where
  ix x f arr@(Delayed l ixF)
    | shapeInRange l x = f (ixF x) <&> \a ->
      let g y | eq1 x y   = a
              | otherwise = ixF x
      in  Delayed l g
    | otherwise        = pure arr
  {-# INLINE ix #-}

-- | Index a delayed array, returning a 'IndexOutOfBounds' exception if
--   the index is out of range.
indexDelayed :: Shape f => Delayed f a -> f Int -> a
indexDelayed (Delayed l ixF) x =
  boundsCheck l x $ ixF x
{-# INLINE indexDelayed #-}

foldDelayed :: (Shape f, Monoid m) => (f Int -> a -> m) -> (Delayed f a) -> m
foldDelayed f (Delayed l ixF) = unsafePerformIO $ do
  childs <- for [0 .. threads - 1] $ \c -> do
    child <- newEmptyMVar
    _ <- forkOn c $ do
      let k | c == threads - 1 = q + r
            | otherwise        = q
          x = c * q
          m = x + k
          go i (Just s) acc
            | i >= m       = acc
            | otherwise    = let !acc' = acc `mappend` f s (ixF s)
                             in  go (i+1) (shapeStep l s) acc'
          go _ Nothing acc = acc
      putMVar child $! go x (Just $ shapeFromIndex l x) mempty
    return child
  F.fold <$> for childs takeMVar
  where
  !n       = shapeSize l
  !(q, r)  = n `quotRem` threads
  !threads = unsafePerformIO getNumCapabilities
{-# INLINE foldDelayed #-}

-- | Parallel manifestation of a delayed array into a material one.
manifest :: (Vector v a, Shape f) => Delayed f a -> Array v f a
manifest (Delayed l ixF) = Array l v
  where
    !v = unsafePerformIO $! do
      mv <- GM.new n
      childs <- for [0 .. threads - 1] $ \c -> do
        child <- newEmptyMVar
        _ <- forkOn c $ do
          let k | c == threads - 1 = q + r
                | otherwise        = q
              x = c * q
          iforOf_ (linearIndexesBetween x (x+k)) l $ \i s ->
            GM.unsafeWrite mv i (ixF s)
          putMVar child ()
        return child
      F.for_ childs takeMVar
      G.unsafeFreeze mv
    !n       = shapeSize l
    !(q, r)  = n `quotRem` threads
    !threads = unsafePerformIO getNumCapabilities
{-# INLINE manifest #-}

linearIndexesBetween :: Shape f => Int -> Int -> IndexedFold Int (Layout f) (f Int)
linearIndexesBetween i0 k g l = go SPEC i0 (Just $ shapeFromIndex l i0)
  where
  go !_ i (Just x) = indexed g i x *> go SPEC (i+1) (guard (i+1 < k) *> shapeStep l x)
  go !_ _ _        = noEffect
{-# INLINE linearIndexesBetween #-}

-- | Generate a 'Delayed' array using the given 'Layout' and
--   construction function.
genDelayed :: Layout f -> (f Int -> a) -> Delayed f a
genDelayed = Delayed
{-# INLINE genDelayed #-}

------------------------------------------------------------------------
-- Focused
------------------------------------------------------------------------

-- | A delayed representation of an array with a focus on a single
--   element. This element is the target of 'extract'.
data Focused f a = Focused !(f Int) !(Delayed f a)
  deriving (Typeable, Functor)

-- | The 'size' of the 'layout' __must__ remain the same or an error is thrown.
instance Shape f => HasLayout f (Focused f a) where
  layout f (Focused x (Delayed l ixF)) = f l <&> \l' ->
    sizeMissmatch (shapeSize l) (shapeSize l')
      ("layout (Focused): trying to replace shape " ++ showShape l ++ " with " ++ showShape l')
      $ Focused x (Delayed l' ixF)
  {-# INLINE layout #-}

instance Shape f => Comonad (Focused f) where
  {-# INLINE extract #-}
  {-# INLINE extend  #-}
  extract (Focused x d) = indexDelayed d x
  extend f (Focused x d@(Delayed l _)) =
    Focused x (genDelayed l $ \i -> f (Focused i d))

instance Shape f => Extend (Focused f) where
  {-# INLINE extended #-}
  extended = extend

instance Shape f => ComonadStore (f Int) (Focused f) where
  {-# INLINE pos   #-}
  {-# INLINE peek  #-}
  {-# INLINE peeks #-}
  {-# INLINE seek  #-}
  {-# INLINE seeks #-}
  pos     (Focused x _) = x
  peek  x (Focused _ d) = indexDelayed d x
  peeks f (Focused x d) = indexDelayed d (f x)
  seek  x (Focused _ d) = Focused x d
  seeks f (Focused x d) = Focused (f x) d

instance (Shape f, Show1 f, Show a) => Show (Focused f a) where
  showsPrec p (Focused l d) = showParen (p > 10) $
    showString "Focused " . showsPrec1 11 l . showChar ' ' . showsPrec 11 d

-- instance (Shape f, Show1 f) => Show1 (Focused f) where
--   showsPrec1 = showsPrec

type instance Index (Focused f a) = f Int
type instance IxValue (Focused f a) = a

instance Shape f => Foldable (Focused f) where
  foldr f b (Focused _ d) = F.foldr f b d
  {-# INLINE foldr #-}

  foldMap f (Focused _ d) = F.foldMap f d
  {-# INLINE foldMap #-}

#if __GLASGOW_HASKELL__ >= 710
  length = size
  {-# INLINE length #-}
#endif

instance Shape f => Traversable (Focused f) where
  traverse f (Focused u d) = Focused u <$> traverse f d
  {-# INLINE traverse #-}

-- | Index relative to focus.
instance Shape f => FunctorWithIndex (f Int) (Focused f) where
  imap f (Focused u d) = Focused u (imap (f . (^-^ u)) d)
  {-# INLINE imap #-}

-- | Index relative to focus.
instance Shape f => FoldableWithIndex (f Int) (Focused f) where
  ifoldr f b (Focused u d) = ifoldr (f . (^-^ u)) b d
  {-# INLINE ifoldr #-}

  ifolded = ifoldring ifoldr
  {-# INLINE ifolded #-}

  ifoldMap f (Focused u d) = ifoldMap (f . (^-^) u) d
  {-# INLINE ifoldMap #-}

-- | Index relative to focus.
instance Shape f => TraversableWithIndex (f Int) (Focused f) where
  itraverse f (Focused u d) = Focused u <$> itraverse (f . (^-^ u)) d
  {-# INLINE itraverse #-}

-- | Index relative to focus.
instance Shape f => Ixed (Focused f a) where
  ix i f (Focused u d) = Focused u <$> ix (i ^-^ u) f d
  {-# INLINE ix #-}

