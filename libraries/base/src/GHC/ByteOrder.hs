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
-- Host byte ordering.
--
-- @since 4.11.0.0

module GHC.ByteOrder
    (ByteOrder(..),
     hostByteOrder,
     targetByteOrder
     ) where

import GHC.Internal.ByteOrder

-- | Deprecated (wrongly-named) alias of 'hostByteOrder'.
--
-- Although this function was technically introduced in
-- @base-4.11.0.0@ (shipped with GHC 8.4), it was BROKEN prior to
-- @base-4.14.0.0@ (shipped with GHC 8.10) and would always return
-- 'LittleEndian' regardless of what the actual relevant machine's
-- byte order is.
-- (See also GHC tickets
-- [#17337](https://gitlab.haskell.org/ghc/ghc/-/issues/17337),
-- [#18845](https://gitlab.haskell.org/ghc/ghc/-/issues/18445), and
-- [#20338](https://gitlab.haskell.org/ghc/ghc/-/issues/20338).)
--
-- ('hostByteOrder' does not have a corresponding warning
-- because it was introduced after this bug was fixed.)
targetByteOrder :: ByteOrder
{-# DEPRECATED targetByteOrder
  [ "Use 'hostByteOrder' instead.",
    "(The base library is not a compiler or code generation tool,",
    "so the notion of a target machine is not applicable!)"]
  #-}
targetByteOrder = hostByteOrder
