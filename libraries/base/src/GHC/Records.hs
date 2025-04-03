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
-- @OverloadedRecordDot@ extension.  See the
-- [wiki page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields)
-- for more details.
--
-- @since 4.10.0.0

module GHC.Records
    (HasField(..)
     ) where

import GHC.Internal.Records