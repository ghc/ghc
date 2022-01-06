{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- This module defines the 'IsLabel' class used by the
-- @OverloadedLabels@ extension.  See the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/overloaded-labels wiki page>
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
-- RebindableSyntax is enabled) .  The renamer transforms it into
-- a call to 'fromLabel'.
-- See Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr.

module GHC.OverloadedLabels
       ( IsLabel(..)
       ) where

import GHC.Base ( Symbol )

class IsLabel (x :: Symbol) a where
  fromLabel :: a
