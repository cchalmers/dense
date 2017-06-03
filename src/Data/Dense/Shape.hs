{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Dense.Shape
  ( Shape(..)
  , ix1
  , ix2
  , ix3
  , ix4
  , ix5

  , reverseShape

  , Sh0, Sh1, Sh2
  , Sh3, Sh4, Sh5
  , Sh6, Sh7, Sh8
  , Sh9, Sh10

  , Shaped (..)
  , shaped
  ) where

import qualified Control.Applicative  as A
import           Control.Lens         hiding (Index)
import           Data.Constraint
import qualified Data.Foldable        as F
import           Data.Functor.Classes
import qualified Data.Traversable     as T
import           Linear

import           Data.Dense.Nat

------------------------------------------------------------------------
-- The Shape type
------------------------------------------------------------------------

infixr 3 :*

-- | Shape with type level rank.
data Shape (n :: Nat) a where
  Nil  :: Shape 'Z a
  (:*) :: !a -> !(Shape r a) -> Shape ('S r) a

-- | An 'Index' with a TypeLit natural.
--
-- @
-- >>> set -XTypeLiterals
-- >>> ix3 1 2 3 :: V 3 Int
-- 1 :* 2 :* 3 :* Nil
-- @
-- type V n = Shape (U n)

-- | Create a 1 dimensional shape.
ix1 :: a -> Sh1 a
ix1 a = a :* Nil

-- | Create a 2 dimensional shape.
ix2 :: a -> a -> Sh2 a
ix2 a b = a :* b :* Nil

-- | Create a 3 dimensional shape.
ix3 :: a -> a -> a -> Sh3 a
ix3 a b c = a :* b :* c :* Nil

-- | Create a 4 dimensional shape.
ix4 :: a -> a -> a -> a -> Sh4 a
ix4 a b c d = a :* b :* c :* d :* Nil

-- | Create a 5 dimensional shape.
ix5 :: a -> a -> a -> a -> a -> Sh5 a
ix5 a b c d e = a :* b :* c :* d :* e :* Nil

-- nilShConstrRep :: Constr
-- nilShConstrRep    = mkConstr shapeDataTypeRep "Nil" [] Prefix
-- consShConstrRep :: Constr
-- consShConstrRep   = mkConstr shapeDataTypeRep ":*" [] Infix

-- shapeDataTypeRep :: DataType
-- shapeDataTypeRep = mkDataType "Shape" [nilShConstrRep,consShConstrRep]

newtype NatDict (c :: Nat -> Constraint) (s :: Nat) = NatDict { und :: Dict (c s) }

countDict :: Countable (NatDict c) => Peano r :- c r
countDict = Sub (und count)

class EqSh (s :: Nat) where
  eqSh :: (a -> b -> Bool) -> Shape s a -> Shape s b -> Bool

instance EqSh 'Z where
  eqSh = \_ _ _ -> True
  {-# INLINE eqSh #-}

instance EqSh s => EqSh ('S s) where
  eqSh = \f (a:*as) (b:*bs) -> f a b && eqSh f as bs
  {-# INLINE eqSh #-}

instance Countable (NatDict EqSh) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
  {-# INLINE czero #-}
  {-# INLINE csucc #-}

instance Peano r => Eq1 (Shape r) where
  liftEq = eqSh \\ (countDict :: Peano r :- EqSh r)
  {-# INLINE liftEq #-}

instance (Peano r, Eq a) => Eq (Shape r a) where
  (==) = eq1
  {-# INLINE (==) #-}

instance Show a => Show (Shape r a) where
  showsPrec _ Nil = showString "Nil"
  showsPrec p as0  = showParen (p >= 3) (showsSh as0)
    where
    -- this is too avoid too unnecessary brackets
    showsSh :: Shape s a -> ShowS
    showsSh Nil     = showString "Nil"
    showsSh (a:*as) = showsPrec 3 a . showString " :* " . showsSh as

-- Functor
class ShMap (s::Nat) where
  shapeMap :: (a -> b) -> Shape s a -> Shape s b
instance ShMap 'Z where shapeMap _ _ = Nil
instance ShMap s => ShMap ('S s) where
  shapeMap f (a :* as) = f a :* shapeMap f as
instance Countable (NatDict ShMap) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
instance Peano r => Functor (Shape r) where
  fmap = shapeMap \\ (countDict :: Peano r :- ShMap r)

-- Foldable
class ShFold (s::Nat) where
  foldrSh :: (a -> b -> b) -> b -> Shape s a -> b
  foldMapSh :: Monoid m => (a -> m) -> Shape s a -> m

instance ShFold 'Z where
  foldrSh _ b _ = b
  foldMapSh _ _ = mempty
  {-# INLINE foldrSh #-}
  {-# INLINE foldMapSh #-}

instance ShFold s => ShFold ('S s) where
  foldrSh f b (a :* as) = f a (foldrSh f b as)
  foldMapSh f (a :* as) = f a `mappend` foldMapSh f as
  {-# INLINE foldrSh #-}
  {-# INLINE foldMapSh #-}

instance Countable (NatDict ShFold) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
  {-# INLINE czero #-}
  {-# INLINE csucc #-}

instance Peano r => F.Foldable (Shape r) where
  foldr = foldrSh \\ (countDict :: Peano r :- ShFold r)
  foldMap = foldMapSh \\ (countDict :: Peano r :- ShFold r)
  {-# INLINE foldr #-}
  {-# INLINE foldMap #-}

-- Traversable
class ShTraverse (n::Nat) where
  traverseSh :: Applicative f => (a -> f b) -> Shape n a -> f (Shape n b)
instance ShTraverse 'Z where
  traverseSh = \_ _ -> pure Nil
instance ShTraverse n => ShTraverse ('S n) where
  traverseSh = \f (a :* as) -> (:*) <$> f a <*> traverseSh f as
instance Countable (NatDict ShTraverse) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
instance Peano r => T.Traversable (Shape r) where
  traverse = traverseSh \\ (countDict :: Peano r :- ShTraverse r)

-- Applicative
class ShAp (s::Nat) where
  pureSh :: a -> Shape s a
  apSh :: Shape s (a -> b) -> Shape s a -> Shape s b

instance ShAp 'Z where
  pureSh = \_ -> Nil
  apSh = \_ _ -> Nil
  {-# INLINE pureSh #-}
  {-# INLINE apSh #-}

instance ShAp s => ShAp ('S s) where
  pureSh = \a -> a :* pureSh a
  apSh = \(f :* fs) (a :* as) -> f a :* apSh fs as
  {-# INLINE pureSh #-}
  {-# INLINE apSh #-}

instance Countable (NatDict ShAp) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
  {-# INLINE czero #-}
  {-# INLINE csucc #-}

instance Peano r => Applicative (Shape r) where
  pure = pureSh \\ (countDict :: Peano r :- ShAp r)
  (<*>) = apSh \\ (countDict :: Peano r :- ShAp r)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Peano n => Additive (Shape n) where
  liftI2 = A.liftA2
  {-# INLINE liftI2 #-}
  liftU2 = A.liftA2
  {-# INLINE liftU2 #-}
  (^+^) = A.liftA2 (+)
  {-# INLINE (^+^) #-}
  zero = pure 0
  {-# INLINE zero #-}

-- shapeToAddress :: Peano r => Shape r Int -> Shape r Int -> Int
-- shapeToAddress l x = F.foldl (\k (e, a) -> k * e + a) 0 (A.liftA2 (,) l x)
-- {-# INLINE shapeToAddress #-}

-- -- | Convert a linear index to a shape the 'Layout'.
-- shapeFromAddress :: Peano r => Shape r Int -> Int -> Shape r Int
-- shapeFromAddress l i = snd $ T.mapAccumR quotRem i l
-- {-# INLINE shapeFromAddress #-}

class Rev (s::Nat) where
  rev :: Shape s a -> Shape s a
  unsnocS :: Shape ('S s) a -> (Shape s a, a)

instance Rev 'Z where
  rev = \_ -> Nil
  {-# INLINE rev #-}
  unsnocS (a :* Nil) = (Nil, a)
  {-# INLINE unsnocS #-}

instance Rev s => Rev ('S s) where
  unsnocS (a :* as) =
    let (as', l) = unsnocS as
    in  (a :* as', l)
  {-# INLINE unsnocS #-}
  rev = \as ->
    let (as', l) = unsnocS as
        as'rev   = rev as'
    in  l :* as'rev
  {-# INLINE rev #-}

instance Countable (NatDict Rev) where
  czero = NatDict Dict
  csucc = \(NatDict Dict) -> NatDict Dict
  {-# INLINE czero #-}
  {-# INLINE csucc #-}

-- | Reverse a shape.
reverseShape :: Peano r => Shape r a -> Shape r a
reverseShape = f where
  f :: forall r a. Peano r => Shape r a -> Shape r a
  f = rev \\ (countDict :: Peano r :- Rev r)
{-# INLINE reverseShape #-}

-- | The shape with 0 dimensions.
type Sh0 = Shape N0
-- | The shape with 1 dimension.
type Sh1 = Shape N1
-- | The shape with 2 dimensions.
type Sh2 = Shape N2
-- | The shape with 3 dimensions.
type Sh3 = Shape N3
-- | The shape with 4 dimensions.
type Sh4 = Shape N4
-- | The shape with 5 dimensions.
type Sh5 = Shape N5
-- | The shape with 6 dimensions.
type Sh6 = Shape N6
-- | The shape with 7 dimensions.
type Sh7 = Shape N7
-- | The shape with 8 dimensions.
type Sh8 = Shape N8
-- | The shape with 9 dimensions.
type Sh9 = Shape N9
-- | The shape with 10 dimensions.
type Sh10 = Shape N10

instance R1 (Shape (S1 r)) where
  _x = \f (x:*as) -> f x <&> \x' -> x' :* as

instance R2 (Shape (S2 r)) where
  _y = \f (x:*y:*as) -> f y <&> \y' -> x :* y' :* as
  _xy = \f (x:*y:*as) -> f (V2 x y) <&> \(V2 x' y') -> x' :* y' :* as

instance R3 (Shape (S3 r)) where
  _z = \f (x:*y:*z:*as) -> f z <&> \z' -> x :* y :* z' :* as
  _xyz = \f (x:*y:*z:*as) -> f (V3 x y z) <&> \(V3 x' y' z') -> x' :* y' :* z' :* as

instance R4 (Shape (S4 r)) where
  _w = \f (x:*y:*z:*w:*as) -> f w <&> \w' -> x :* y :* z :* w' :* as
  _xyzw = \f (x:*y:*z:*w:*as) -> f (V4 x y z w) <&> \(V4 x' y' z' w') -> x' :* y' :* z' :* w' :* as

------------------------------------------------------------------------
-- Shaped
------------------------------------------------------------------------

class Shaped f where
  -- | Number of elements in the shape.
  type Size f :: Nat

  -- | Convert to a 'Shape' of the same 'Size'.
  toShape :: f a -> Shape (Size f) a

  -- | Convert from a 'Shape' of the same 'Size'.
  fromShape :: Shape (Size f) a -> f a

-- | Isomorphism between an index and 'Shape' of the same size.
shaped :: Shaped f => Iso' (f a) (Shape (Size f) a)
shaped = iso toShape fromShape

instance Shaped (Shape n) where
  type Size (Shape n) = n
  toShape = id
  fromShape = id
  {-# INLINE toShape #-}
  {-# INLINE fromShape #-}

instance Shaped V1 where
  type Size V1 = N1
  toShape = \(V1 a) -> a :* Nil
  fromShape = \(a :* Nil) -> V1 a
  {-# INLINE toShape #-}
  {-# INLINE fromShape #-}

instance Shaped V2 where
  type Size V2 = N2
  toShape = \(V2 a b) -> a :* b :* Nil
  fromShape = \(a :* b :* Nil) -> V2 a b
  {-# INLINE toShape #-}
  {-# INLINE fromShape #-}

instance Shaped V3 where
  type Size V3 = N3
  toShape = \(V3 x y z) -> x :* y :* z :* Nil
  fromShape = \(x :* y :* z :* Nil) -> V3 x y z
  {-# INLINE toShape #-}
  {-# INLINE fromShape #-}

instance Shaped V4 where
  type Size V4 = N4
  toShape = \(V4 x y z w) -> x :* y :* z :* w :* Nil
  fromShape = \(x :* y :* z :* w :* Nil) -> V4 x y z w
  {-# INLINE toShape #-}
  {-# INLINE fromShape #-}

