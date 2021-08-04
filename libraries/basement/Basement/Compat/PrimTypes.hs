-- |
-- Module      : Basement.Compat.PrimTypes
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
module Basement.Compat.PrimTypes
    ( FileSize#
    , Offset#
    , CountOf#
    , Bool#
    , Pinned#
    ) where

import GHC.Prim

-- | File size in bytes
type FileSize# = Word64#

-- | Offset in a bytearray, string, type alias
--
-- for code documentation purpose only, just a simple type alias on Int#
type Offset# = Int#

-- | CountOf in bytes type alias
--
-- for code documentation purpose only, just a simple type alias on Int#
type CountOf# = Int#

-- | Lowlevel Boolean
type Bool# = Int#

-- | Pinning status
type Pinned# = Bool#
