{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.ByteOrder
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Target byte ordering.
--
-- @since 4.11.0.0

module GHC.ByteOrder
    (ByteOrder(..),
     targetByteOrder
     ) where

import GHC.Internal.ByteOrder