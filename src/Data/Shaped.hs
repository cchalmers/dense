{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module provides a large subset of the full functionality of
-- "shaped" without exporting names that conflict with names in prelude,
-- so it can often be imported unqualified. It also includes reexported
-- classes and data types from other modules. However it does not
-- contain much functions necessary to construct arrays, for that see
-- "Data.Shaped.Generic" or one of the type specific modules intended to
-- be imported qualified. Typical imports for shaped will look like this:
--
-- @
-- import           "Data.Shaped"
-- import qualified "Data.Shaped.Unboxed" as U
-- @
--
-- For boxed-specific arrays (a la "Data.Vector") see 'Data.Shaped.Boxed'.
-----------------------------------------------------------------------------
module Data.Shaped
  (
    -- * Array types
    Array
  , BArray
  , UArray
  , SArray
  , PArray

    -- * Indexing
  , Layout
  , HasLayout (..)
  , Shape
  , extent
  , size

    -- ** Folds over indexes
  , indexes
  , indexesBetween
  , indexesFrom

    -- ** Lenses
  , vector

    -- ** Traversals
  , values
  , values'
  , valuesBetween

  -- * Construction

  -- ** Flat arrays
  , flat

  -- ** Shaped from lists
  , fromListInto
  , fromListInto_

  -- ** Shaped from vectors
  , fromVectorInto
  , fromVectorInto_

  -- ** Generating
  -- | See "Data.Shaped.Generic".

  -- * Functions on arrays

  -- ** Empty arrays
  -- | See 'Control.Lens.Empty.AsEmpty' class or "Data.Shaped.Generic".

  -- ** Indexing
  -- | See 'Control.Lens.At.Ixed' class.

  -- ** Modifying arrays
  -- | See "Data.Shaped.Generic".

  -- ** Slices

  -- *** Matrix
  , ixRow
  , rows
  , ixColumn
  , columns

  -- *** 3D
  , ixPlane
  , planes
  , flattenPlane

  -- * Mutable
  , MArray
  , BMArray
  , UMArray
  , SMArray
  , PMArray

  -- * Delayed

  , Delayed

  -- ** Generating delayed

  , delayed
  , delay
  , manifest
  , seqManifest
  , genDelayed
  , indexDelayed
  , affirm
  , seqAffirm

    -- ** Helpful reexports
  , (*^)
  , (^*)
  , (^/)
  , Additive (..)
  , Metric (..)

  -- * Focused

  , Focused

  -- ** Generating focused

  , focusOn
  , unfocus
  , unfocused
  , extendFocus

  -- ** Focus location
  , locale
  , shiftFocus

    -- ** Helpful reexports
  , Comonad (..)
  , ComonadStore (..)

  -- * Common shapes
  , V1 (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)
  , R1 (..)
  , R2 (..)
  , R3 (..)
  , R4 (..)

  -- ** Extra planes
  , _xz
  , _yz
  , _yx
  , _zy
  , _zx
  ) where

import           Data.Shaped.Generic
import           Control.Comonad.Store
import           Linear                hiding (vector)

