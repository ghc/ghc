{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

{-# LANGUAGE MagicHash #-}

-- |
--
-- Module      :  GHC.ArrayArray
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable (GHC Extensions)
--
-- Legacy interface for arrays of arrays.
-- Deprecated, because the 'Array#' type can now store arrays directly.
-- Consider simply using 'Array#' instead of 'ArrayArray#'.
--
-- Use GHC.Exts instead of importing this module directly.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.ArrayArray should be removed in GHC 10.02."
#endif

module GHC.ArrayArray
  {-# DEPRECATED ["GHC.ArrayArray is deprecated and will be removed in GHC 10.02. Please use GHC.Exts."] #-}
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
