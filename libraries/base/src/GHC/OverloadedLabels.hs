-- |
--
-- Module      :  GHC.OverloadedLabels
-- Copyright   :  (c) Adam Gundry 2015-2016
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
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

module GHC.OverloadedLabels
    (IsLabel(..)
     ) where

import GHC.Internal.OverloadedLabels