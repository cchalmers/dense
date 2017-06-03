{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Dense.Nat
  ( Nat(..)
  , N0 , N1 , N2
  , N3 , N4 , N5
  , N6 , N7 , N8
  , N9 , N10

  , S1 , S2 , S3
  , S4 , S5 , S6
  , S7 , S8 , S9
  , S10

  , type (+)
  , U

  , Peano (..)
  , Countable (..)
  , natVal
  ) where

import           Data.Data
import           Data.Profunctor.Unsafe
import           Data.Semigroup
import qualified GHC.TypeLits           as TL
import           Numeric.Natural

-- | Strict Peano naturals.
data Nat = Z | S !Nat
  deriving (Eq,Show,Read,Typeable,Data)

instance Semigroup Nat where
  Z   <> a = a
  S a <> b = S (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Nat where
  mempty  = Z
  mappend = (<>)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

type family U (n :: TL.Nat) :: Nat where
  U 0 = 'Z
  U n = 'S (U (((TL.-)) n 1))

type family a + b where
  'Z   + b = b
  'S a + b = 'S (a + b)

-- | The 0 'Nat'.
type N0 = 'Z
-- | The 1 'Nat'.
type N1 = 'S N0
-- | The 2 'Nat'.
type N2 = 'S N1
-- | The 3 'Nat'.
type N3 = 'S N2
-- | The 4 'Nat'.
type N4 = 'S N3
-- | The 5 'Nat'.
type N5 = 'S N4
-- | The 6 'Nat'.
type N6 = 'S N5
-- | The 7 'Nat'.
type N7 = 'S N6
-- | The 8 'Nat'.
type N8 = 'S N7
-- | The 9 'Nat'.
type N9 = 'S N8
-- | The 10 'Nat'.
type N10 = 'S N9

-- | The r+1 'Nat'.
type S1 r  = 'S r
-- | The r+2 'Nat'.
type S2 r  = 'S (S1 r)
-- | The r+3 'Nat'.
type S3 r  = 'S (S2 r)
-- | The r+4 'Nat'.
type S4 r  = 'S (S3 r)
-- | The r+5 'Nat'.
type S5 r  = 'S (S4 r)
-- | The r+6 'Nat'.
type S6 r  = 'S (S5 r)
-- | The r+7 'Nat'.
type S7 r  = 'S (S6 r)
-- | The r+8 'Nat'.
type S8 r  = 'S (S7 r)
-- | The r+9 'Nat'.
type S9 r  = 'S (S8 r)
-- | The r+10 'Nat'.
type S10 r = 'S (S9 r)

-- | Counting on the type level.
--
--   This class is used to perform compile time finite recursion.
class Countable f where
  czero :: f 'Z
  csucc :: f s -> f ('S s)

-- | The mechanics for counting natural numbers.
class Peano (a :: Nat) where
  count :: Countable f => f a

instance Peano 'Z where
  count = czero
  {-# INLINE count #-}

instance Peano a => Peano ('S a) where
  count = csucc count
  {-# INLINE count #-}

newtype CountNat (r::Nat) = CountNat { getCount :: Natural }
instance Countable CountNat where
  czero              = CountNat 0
  csucc (CountNat n) = CountNat (succ n)

-- | Reflect the value of a peano natural number.
natVal :: Peano r => proxy r -> Natural
natVal = getCount #. f where
  f :: Peano r => proxy r -> CountNat r
  f _ = count
{-# INLINE natVal #-}


