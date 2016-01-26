{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, NoImplicitPrelude,
             PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Bool
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Basic operations on type-level Booleans.
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------

module Data.Type.Bool (
  If, type (&&), type (||), Not
  ) where

import Data.Bool

-- This needs to be in base because (&&) is used in Data.Type.Equality.
-- The other functions do not need to be in base, but seemed to be appropriate
-- here.

-- | Type-level "If". @If True a b@ ==> @a@; @If False a b@ ==> @b@
type family If cond tru fls where
  If 'True  tru  fls = tru
  If 'False tru  fls = fls

-- | Type-level "and"
type family a && b where
  'False && a      = 'False
  'True  && a      = a
  a      && 'False = 'False
  a      && 'True  = a
  a      && a      = a
infixr 3 &&

-- | Type-level "or"
type family a || b where
  'False || a      = a
  'True  || a      = 'True
  a      || 'False = a
  a      || 'True  = 'True
  a      || a      = a
infixr 2 ||

-- | Type-level "not"
type family Not a where
  Not 'False = 'True
  Not 'True  = 'False
