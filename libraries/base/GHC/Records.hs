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
-- When @OverloadedRecordFields@ is enabled (but neither
-- @OverloadedLabels@ nor @RebindableSyntax@ are enabled), if GHC sees
-- an occurrence of the overloaded label syntax @#foo@, it is replaced
-- with
--
-- > fromLabel @"foo" :: alpha -> beta
--
-- plus a wanted constraint @HasField "foo" alpha beta@.  Type
-- inference will then be used to determine the record type @alpha@
-- and field type @beta@.
--
-- See "GHC.OverloadedLabels" for more details on what happens when
-- @OverloadedLabels@ is enabled.  Note that if @RebindableSyntax@ is
-- enabled, the desugaring of overloaded label syntax will make use of
-- whatever @fromLabel@ is in scope.
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
  fromLabel :: r -> a
