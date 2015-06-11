{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiWayIf            #-}
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
-- Simple wrapper over a generic vector, giving the vector a 'Shape'.
-- Currently the api is a bit sparse but many standard functions that
-- appear missing can be used with the 'values' traversal with the
-- functions in the lens library.
-----------------------------------------------------------------------------
module Data.Shaped
  (
    -- * Array types
    Array (..)
  , Shape (..)
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

  -- * Functions on arrays

  -- ** Modifying arrays
  , (//)

  -- ** Zipping
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3

  -- ** Slices

  -- , slice
  , line
  , unsafeOrdinals
  , plane

  -- * Mutable
  , MArray (..)
  , M.BMArray
  , M.UMArray
  , M.SMArray
  , M.PMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Common layouts
  , V1 (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)
  , _x, _y, _z

  -- * Delayed

  , Delayed (..)
  , delayed
  , delay
  , manifest
  , manifestS
  , genDelayed
  ) where


#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative             (pure)
import           Data.Foldable                   (Foldable)
#endif

import           Control.Applicative             (liftA2)
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                   (liftM)
import           Control.Monad.Primitive
import           Control.Monad.ST
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
import           Data.Vector.Generic.Lens        (toVectorOf, vectorTraverse)
import qualified Data.Vector.Generic.Mutable     as GM
import qualified Data.Vector.Generic.New         as New
import qualified Data.Vector.Primitive           as P
import qualified Data.Vector.Storable            as S
import qualified Data.Vector.Unboxed             as U
import           GHC.Generics                    (Generic, Generic1)
import           Linear
import           Text.ParserCombinators.ReadPrec (readS_to_Prec)
import qualified Text.Read                       as Read

import           Data.Shaped.Index
import           Data.Shaped.Mutable             (MArray (..))
import qualified Data.Shaped.Mutable             as M

import           Control.Concurrent              (forkOn, getNumCapabilities,
                                                  newEmptyMVar, putMVar,
                                                  takeMVar)
import           System.IO.Unsafe                (unsafePerformIO)

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
values f arr = reindexed (fromIndex $ extent arr) (linear . vectorTraverse) f arr
{-# INLINE values #-}

-- | Same as 'values' but restrictive in the vector type.
values' :: (Shape l, Vector v a, Vector v b)
       => IndexedTraversal (l Int) (Array v l a) (Array v l b) a b
values' = values
{-# INLINE values' #-}

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

-- | For each pair (i,a) from the list, replace the vector element at
--   position i by a.
(//) :: (G.Vector v a, Shape l) => Array v l a -> [(l Int, a)] -> Array v l a
Array l v // xs = Array l $ v G.// over (each . _1) (toIndex l) xs

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

sliced :: (Vector v a, Shape l1, Shape l2)
      => Getting (l2 Int) (l1 Int) (l2 Int)
      -> (l2 Int -> l1 Int)
      -> Lens' (Array v l1 a) (Array v l2 a)
sliced = undefined

sliced'
  :: (Vector v a, Shape l1, Shape l2)
  => (l1 Int -> l2 Int)
  -> (l2 Int -> l1 Int)
  -> Lens' (Array v l1 a) (Array v l2 a)
sliced' f12 f21 f arr@(Array l1 _) =
  f a <&> \arr' -> arr // (arr' ^@.. reindexed f21 values)
  where a  = generate (f12 l1) $ \x -> arr ^?! ix (f21 x)

slice :: (Vector v a, Vector w a, Shape l1, Shape l2)
      => Getting (l2 Int) (l1 Int) (l2 Int)
      -> (l2 Int -> l1 Int)
      -> Array v l1 a
      -> Array w l2 a
slice l f arr@(Array l1 _) = generate l2 $ \x -> arr ^?! ix (f x) where l2 = l1 ^. l

-- column :: Vector v a => Int -> Array v V2 a -> Array v V1 a
-- column y = slice (_x . to V1) (\(V1 x) -> V2 x y)

-- | 'row' :: Lens' (l Int) Int -> l Int -> Array v l a -> Array v V1 a
--
--
-- row :: Vector v a => Getting (l Int) Int (l Int) -> l Int -> Lens' (Array v l a) (Array v V1 a)
-- row g l0 f arr@(Array l _) = f v1 <&> \v1' -> --
--   where
--     v1 = generate (l ^. g) $ \x -> arr ^?! ix (x ^.)
line :: (Vector v a, Shape l)
     => Lens' (l Int) Int -- ^ Lens onto target line
     -> l Int             -- ^ Some point on the line
     -> Lens' (Array v l a) (Array v V1 a) -- Lens onto the line
line l = line' l l
{-# INLINE line #-}

line' :: (Vector v a, Shape l)
      => Getting (V1 Int) (l Int) Int
      -> ASetter' (l Int) Int
      -> l Int
      -> Lens' (Array v l a) (Array v V1 a)
line' g s l = sliced' (view (g . to V1)) (\(V1 x) -> l & s .~ x)
{-# INLINE line' #-}

-- line :: (Vector v a, Shape l) => Lens' (l Int) Int -> l Int -> IndexedTraversal' (l Int) (Array v l a) a

-- planes :: (Vector v a, Shape l) => Lens' (l Int) Int -> l Int -> IndexedTraversal' Int (Array v V3 a) (Array v V2 a)

-- row :: (Vector v a, Shape l) => ALens' (l Int) Int -> Int -> Lens' (Array v l a) (Array v V1 a)
-- row l x = sliced (cloneLens l . to V1) (\(V1 x) -> xx & l #~ x)
--   where xx = x <$ zero

plane :: (Vector v a, Vector w a)
      => ALens' (V3 Int) (V2 Int) -- Lens onto plane
      -> Int -- number of plane
      -> Array v V3 a
      -> Array w V2 a -- Lens' (V3 Int) (V2 Int) =>
plane l n = slice getter (\xx -> x & l #~ xx)
  where x      = n <$ (zero :: Additive f => f Int)
        getter = cloneLens l

-- plane :: (Vector v a, Vector w a)
--       => Lens' (V3 Int) (V2 Int) -- Lens onto plane
--       -> Int -- number of plane
--       -> Array v V3 a
--       -> Array w V2 a -- Lens' (V3 Int) (V2 Int) =>
-- plane l n = slice getter (\xx -> x & l #~ xx)
--   where x      = n <$ (zero :: Additive f => f Int)
--         getter = cloneLens l


-- | This 'Traversal' will ignore any duplicates in the supplied list
-- of indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
unsafeOrdinals :: (Vector v a, Shape l) => [l Int] -> IndexedTraversal' (l Int) (Array v l a) a
unsafeOrdinals is f (Array l v) = Array l . (v G.//) <$> traverse g is
  where g x = let i = toIndex l x in (,) i <$> indexed f x (G.unsafeIndex v i)
{-# INLINE [0] unsafeOrdinals #-}

setOrdinals :: (Indexable (l Int) p, Vector v a, Shape l) => [l Int] -> p a a -> Array v l a -> Array v l a
setOrdinals is f (Array l v) = Array l $ G.unsafeUpd v (map g is)
  where g x = let i = toIndex l x in (,) i $ indexed f x (G.unsafeIndex v i)
{-# INLINE setOrdinals #-}

{-# RULES
"unsafeOrdinals/setOrdinals" forall (is :: [l Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape l) => ASetter' (Array v l a) a;
"unsafeOrdinalts/isetOrdintals" forall (is :: [l Int]).
  unsafeOrdinals is = sets (setOrdinals is)
    :: (Vector v a, Shape l) => AnIndexedSetter' (l Int) (Array v l a) a
 #-}

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
  each = linear . vectorTraverse
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
  {-# INLINE rnf #-}

-- Boxed instances -----------------------------------------------------

instance v ~ B.Vector => Functor (Array v l) where
  fmap = over linear . fmap

instance v ~ B.Vector => F.Foldable (Array v l) where
  foldMap f = F.foldMap f . view linear

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

-- instance (Vector v a, Shape l, Unboxed a) => Unboxed (Array v l a) where
-- instance (Vector v a, Shape l, Storable a) => Storable (Array v l a) where

instance (Vector v a, Foldable l, Hashable a) => Hashable (Array v l a) where
  hashWithSalt s (Array l v) = G.foldl' hashWithSalt s' v
    where s' = F.foldl' hashWithSalt s l
  {-# INLINE hashWithSalt #-}

deriving instance (Generic (v a), Generic1 l) => Generic (Array v l a)
deriving instance (Typeable l, Typeable v, Typeable a, Data (l Int), Data (v a)) => Data (Array v l a)

------------------------------------------------------------------------
-- Delayed
------------------------------------------------------------------------

-- | A delayed representation of an array. This is primarily used for
--   mapping over an array in parallel.
data Delayed l a = Delayed !(l Int) (Int -> a)
  deriving (Functor)

-- | 'foldMap' in parallel.
instance Shape l => Foldable (Delayed l) where
  foldr f b (Delayed l ixF) = go 0 where
    go i
      | i >= n     = b
      | otherwise = f (ixF i) (go (i+1))
    n = product l
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
    !n       = product l
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
                  (\(Delayed l _) -> all (==0) l)
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

-- | Isomorphism between an array and it's delayed representation.
--   Conversion to the array is done in parallel.
delayed :: (Vector v a, Vector w b, Shape l, Shape k)
        => Iso (Array v l a) (Array w k b) (Delayed l a) (Delayed k b)
delayed = iso delay manifest
{-# INLINE delayed #-}

-- | Turn a material array into a delayed one with the same shape.
delay :: (Vector v a, Shape l) => Array v l a -> Delayed l a
delay (Array l v) = Delayed l (G.unsafeIndex v)
{-# INLINE delay #-}

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
    !n       = product l
    !(q, r)  = n `quotRem` threads
    !threads = unsafePerformIO getNumCapabilities
{-# INLINE manifest #-}

-- | Sequential manifestation of a delayed array.
manifestS :: (Vector v a , Shape l) => Delayed l a -> Array v l a
manifestS arr@(Delayed l _) = Array l (toVectorOf folded arr)
{-# INLINE manifestS #-}

genDelayed :: Shape l => l Int -> (l Int -> a) -> Delayed l a
genDelayed l f = Delayed l (f . fromIndex l)
{-# INLINE genDelayed #-}

