{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.Records
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

module GHC.Records
    (HasField(..)
     ) where

import GHC.Internal.Records