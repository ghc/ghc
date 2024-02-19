{-# LANGUAGE Safe #-}

{-# LANGUAGE MagicHash #-}

-- |
--
-- Module      :  GHC.ArrayArray
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Legacy interface for arrays of arrays.
-- Deprecated, because the 'Array#' type can now store arrays directly.
-- Consider simply using 'Array#' instead of 'ArrayArray#'.
--
-- Use GHC.Exts instead of importing this module directly.
--

module GHC.ArrayArray
    (ArrayArray#(..),
     MutableArrayArray#(..),
     newArrayArray#,
     unsafeFreezeArrayArray#,
     sizeofArrayArray#,
     sizeofMutableArrayArray#,
     indexByteArrayArray#,
     indexArrayArrayArray#,
     readByteArrayArray#,
     readMutableByteArrayArray#,
     readArrayArrayArray#,
     readMutableArrayArrayArray#,
     writeByteArrayArray#,
     writeMutableByteArrayArray#,
     writeArrayArrayArray#,
     writeMutableArrayArrayArray#,
     copyArrayArray#,
     copyMutableArrayArray#,
     sameArrayArray#,
     sameMutableArrayArray#
     ) where

import GHC.Internal.ArrayArray
