{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
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
-- When @OverloadedLabels@ is enabled, if GHC sees an occurrence of
-- the overloaded label syntax @#foo@, it is replaced with
--
-- > fromLabel @"foo" :: alpha
--
-- plus a wanted constraint @IsLabel "foo" alpha@.
--
-- Note that if @RebindableSyntax@ is enabled, the desugaring of
-- overloaded label syntax will make use of whatever @fromLabel@ is in
-- scope.
--
-----------------------------------------------------------------------------

-- Note [Overloaded labels]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- An overloaded label is represented by the 'HsOverLabel' constructor
-- of 'HsExpr', which stores the 'FastString' text of the label and an
-- optional id for the 'fromLabel' function to use (if
-- RebindableSyntax is enabled) .  The type-checker transforms it into
-- a call to 'fromLabel'.  See Note [Type-checking overloaded labels]
-- in TcExpr for more details in how type-checking works.

module GHC.OverloadedLabels
       ( IsLabel(..)
       ) where

import GHC.Base ( Symbol )

class IsLabel (x :: Symbol) a where
  fromLabel :: a
