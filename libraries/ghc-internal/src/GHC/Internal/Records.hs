{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Records
-- Copyright   :  (c) Adam Gundry 2015-2024
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'HasField' class used by the
-- @OverloadedRecordDot@ extension.  See the
-- <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/hasfield.html
-- user's guide> for more details.
--
-----------------------------------------------------------------------------

module GHC.Internal.Records
       ( HasField(..)
       ) where

import GHC.Internal.Types (TYPE, Constraint)

-- | Constraint representing the fact that the field @x@ belongs to
-- the record type @r@ and has field type @a@.  This will be solved
-- automatically, but manual instances may be provided as well.

--   HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
--   getField :: forall {k} {r_rep} {a_rep} (x::k) (r :: TYPE r_rep) (a :: TYPE a_rep) . HasField x r a => r -> a
-- NB: The {k} means that k is an 'inferred' type variable, and
--     hence not provided in visible type applications.  Thus you
--     say     getField @"foo"
--     not     getField @Symbol @"foo"
type HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
class HasField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  getField :: r -> a
