{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dense.Stencil
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Stencils can be used to sum (or any fold) over neighbouring sites to
-- the current position on a 'Focused'.
-----------------------------------------------------------------------------
module Data.Dense.Stencil
  ( -- * The Stencil type
    Stencil (..)
  , mkStencil
  , mkStencilUnboxed

    -- ** Using stencils
  , stencilSum

  ) where

import           Control.Lens
import           Data.Dense.Base
import           Data.Dense.Generic   (Boundary (..), peekRelativeB)
import           Data.Dense.Index
import qualified Data.Foldable        as F
import           Data.Functor.Classes
import qualified Data.Vector.Unboxed  as U
import           Text.Show

-- Types ---------------------------------------------------------------

-- | Stencils are used to fold over neighbouring array sites. To
--   construct a stencil use 'mkStencil', 'mkStencilUnboxed'. For
--   static sized stencils you can use the quasiquoter
--   'Data.Dense.TH.stencil'.
--
--   To use a stencil you can use 'stencilSum' or use the 'Foldable' and
--   'FoldableWithIndex' instances.
newtype Stencil f a = Stencil (forall b. (f Int -> a -> b -> b) -> b -> b)

instance (Show1 f, Show a) => Show (Stencil f a) where
  showsPrec _ s = showListWith g (itoList s) where
    g (i,x) = showChar '(' . showsPrec1 0 i . showChar ',' . showsPrec 0 x . showChar ')'

instance F.Foldable (Stencil f) where
  foldr f z (Stencil s) = s (\_ a b -> f a b) z
  {-# INLINE foldr #-}

instance FoldableWithIndex (f Int) (Stencil f) where
  ifoldr f b (Stencil s) = s f b
  {-# INLINE ifoldr #-}
  ifoldMap = ifoldMapOf (ifoldring ifoldr)
  {-# INLINE ifoldMap #-}

instance Functor (Stencil f) where
  fmap f (Stencil s) = Stencil $ \g z -> s (\x a b -> g x (f a) b) z
  {-# INLINE [0] fmap #-}

-- | Make a stencil folding over a list.
--
--   If the list is staticlly known this should expand at compile time
--   via rewrite rules, similar to 'Data.Dense.TH.makeStencilTH' but less reliable. If
--   that does not happen the resulting could be slow. If the list is
--   not know at compile time, 'mkStencilUnboxed' can be signifcantly
--   faster (but isn't subject expending via rewrite rules).
mkStencil :: [(f Int, a)] -> Stencil f a
mkStencil l = Stencil $ \g z -> myfoldr (\(i,a) b -> g i a b) z l
{-# INLINE mkStencil #-}

-- Version of foldr that recursivly expands the list via rewrite rules.
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f b = go where
  go []     = b
  go (a:as) = f a (go as)
{-# INLINE [0] myfoldr #-}

{-# RULES
"mkStencil/cons" forall f b a as.
 myfoldr f b (a:as) = f a (myfoldr f b as)
 #-}

-- | Make a stencil folding over an unboxed vector from the list.
mkStencilUnboxed :: (U.Unbox (f Int), U.Unbox a) => [(f Int, a)] -> Stencil f a
mkStencilUnboxed l = Stencil $ \g z -> U.foldr (\(i,a) b -> g i a b) z v
  where !v = U.fromList l
{-# INLINE mkStencilUnboxed #-}

-- | Sum the elements around a 'Focused' using a 'Boundary' condition
--   and a 'Stencil'.
--
--   This is often used in conjunction with 'Data.Dense.extendFocus'.
stencilSum :: (Shape f, Num a) => Boundary -> Stencil f a -> Focused f a -> a
stencilSum bnd s = \w ->
  let f i b a = b + a * peekRelativeB bnd i w
      {-# INLINE [0] f #-}
  in  ifoldl' f 0 s
{-# INLINE stencilSum #-}

