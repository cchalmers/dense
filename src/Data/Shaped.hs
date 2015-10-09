{-# LANGUAGE CPP                   #-}
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
-- so it can often be imported unqualified.
--
-- For boxed-specific arrays (a la 'Data.Vector') see 'Data.Shaped.Boxed'.
-----------------------------------------------------------------------------
module Data.Shaped
  (
    -- * Array types
    Array
  , Shape (..)
  , BArray
  , UArray
  , SArray
  , PArray

    -- ** Extracting size
  , extent
  , size

    -- ** Lenses
  , layout
  , vector

    -- ** Traversals
  , values
  , values'

  -- * Construction

  -- ** Flat arrays
  , flat
  -- , fromList

  -- ** Shaped from lists
  -- , fromListInto
  -- , fromListInto_

  -- ** Shaped from vectors
  -- , fromVectorInto
  -- , fromVectorInto_

  -- ** Initialisation
  -- , replicate
  -- , generate
  -- , linearGenerate

  -- ** Monadic initialisation
  -- , create
  -- , replicateM
  -- , generateM
  -- , linearGenerateM

  -- * Functions on arrays

  -- ** Empty arrays
  -- | See 'Control.Lens.Empty.AsEmpty' class.

  -- ** Indexing
  -- | See 'Control.Lens.At.Ixed' class.

  , unsafeIndex
  , linearIndex
  , unsafeLinearIndex

  -- *** Monadic indexing
  , indexM
  , unsafeIndexM
  , linearIndexM
  , unsafeLinearIndexM

  -- -- ** Modifying arrays
  -- , (//)


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

  -- *** Ordinals
  , unsafeOrdinals

  -- * Mutable
  , MArray
  , BMArray
  , UMArray
  , SMArray
  , PMArray

  , thaw
  , freeze
  , unsafeThaw
  , unsafeFreeze

  -- * Delayed

  , Delayed

  -- ** Generating delayed

  , delayed
  , delay
  , manifest
  , manifestS
  , genDelayed
  , indexDelayed

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

  -- * Common layouts
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
import           Linear              hiding (vector)

