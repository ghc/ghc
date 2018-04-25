{-# LANGUAGE AllowAmbiguousTypes
           , FunctionalDependencies
           , KindSignatures
           , MultiParamTypeClasses
           , PolyKinds
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Records
-- Copyright   :  (c) Adam Gundry 2015-2016
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'HasField' class used by the
-- @OverloadedRecordFields@ extension.  See the
-- <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields
-- wiki page> for more details.
--
-----------------------------------------------------------------------------

module GHC.Records
       ( HasField(..)
       ) where

-- | Constraint representing the fact that the field @x@ belongs to
-- the record type @r@ and has field type @a@.  This will be solved
-- automatically, but manual instances may be provided as well.
class HasField (x :: k) r a | x r -> a where
  -- | Selector function to extract the field from the record.
  getField :: r -> a
