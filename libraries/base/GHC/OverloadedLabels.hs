{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , NoImplicitPrelude
           , ScopedTypeVariables
           , TypeApplications
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.OverloadedLabels
-- Copyright   :  (c) Adam Gundry 2015-2016
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'IsLabel' class is used by the
-- @OverloadedLabels@ extension.  See the
-- <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/OverloadedLabels wiki page>
-- for more details.
--
-- When @OverloadedLabels@ is enabled (but @RebindableSyntax@ is not
-- enabled), if GHC sees an occurrence of the overloaded label syntax
-- @#foo@, it is replaced with
--
-- > fromLabel @"foo" :: alpha
--
-- plus a wanted constraint @IsLabel "foo" alpha@.
--
-- This generalises the @OverloadedRecordFields@ extension described
-- in "GHC.Records".
--
-- Note that if @RebindableSyntax@ is enabled, the desugaring of
-- overloaded label syntax will make use of whatever @fromLabel@ is in
-- scope.
--
-----------------------------------------------------------------------------

-- Note [Overloaded labels]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- An overloaded label is represented by the 'HsOverLabel' constructor
-- of 'HsExpr', which stores a 'FastString'.  It is passed through
-- unchanged by the renamer, and the type-checker transforms it into a
-- call to 'fromLabel'.  See Note [Type-checking overloaded labels] in
-- TcExpr for more details in how type-checking works.

module GHC.OverloadedLabels
       ( IsLabel(..)
       ) where

import GHC.Base ( Symbol )
import qualified GHC.Records

class IsLabel (x :: Symbol) a where
  fromLabel :: a

-- | If an overloaded label is used at function type, it will be
-- treated as an overloaded record field selector using
-- 'GHC.Records.HasField'.
instance GHC.Records.HasField x r a => IsLabel x (r -> a) where
  fromLabel = GHC.Records.fromLabel @x
