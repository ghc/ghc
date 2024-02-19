{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Records
-- Copyright   :  (c) Adam Gundry 2015-2016
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'HasField' class used by the
-- @OverloadedRecordFields@ extension.  See the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields
-- wiki page> for more details.
--
-----------------------------------------------------------------------------

module GHC.Internal.Records
       ( HasField(..)
       ) where

-- The import of GHC.Internal.Base is for build ordering; see Notes in GHC.Internal.Base for
-- more info.
import GHC.Internal.Base ()

-- | Constraint representing the fact that the field @x@ belongs to
-- the record type @r@ and has field type @a@.  This will be solved
-- automatically, but manual instances may be provided as well.

--   HasField :: forall {k}. k -> * -> * -> Constraint
--   getField :: forall {k} (x::k) r a. HasField x r a => r -> a
-- NB: The {k} means that k is an 'inferred' type variable, and
--     hence not provided in visible type applications.  Thus you
--     say     getField @"foo"
--     not     getField @Symbol @"foo"
class HasField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  getField :: r -> a
