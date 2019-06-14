{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Records
-- Copyright   :  (c) Adam Gundry 2015-2020
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'HasField' class used to provide polymorphism over
-- record fields.  See the
-- [wiki page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields)
-- for more details.
--
-----------------------------------------------------------------------------

module GHC.Records
       ( HasField(..)
       , getField
       , setField
       ) where

-- | Constraint representing the fact that the field @x@ can be get and set on
--   the record type @r@ and has field type @a@.  This constraint will be solved
--   automatically, but manual instances may be provided as well.
--
-- @since 4.10.0.0

--   HasField :: forall {k}. k -> * -> * -> Constraint
--   getField :: forall {k} (x::k) r a. HasField x r a => r -> a
-- NB: The {k} means that k is an 'inferred' type variable, and
--     hence not provided in visible type applications.  Thus you
--     say     getField @"foo"
--     not     getField @Symbol @"foo"
class HasField x r a | x r -> a where

  -- | Function to get and set a field in a record.
  --
  --   The function should satisfy the invariant:
  --
  -- > uncurry ($) (hasField @x r) == r
  --
  -- @since 4.15.0.0
  hasField :: r -> (a -> r, a)


-- | Selector function to extract the field from the record.
--
-- @since 4.10.0.0
--
getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x


-- | Update function to set a new value of the field on the record.
--
-- @since 4.15.0.0
--
setField :: forall x r a . HasField x r a => r -> a -> r
setField = fst . hasField @x
