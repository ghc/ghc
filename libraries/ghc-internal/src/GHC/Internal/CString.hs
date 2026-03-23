{-# LANGUAGE MagicHash, NoImplicitPrelude, BangPatterns, UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.CString
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC C strings definitions (re-exported from GHC.Internal.Types).
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.CString (
        -- * Ascii variants
        unpackCString#, unpackAppendCString#, unpackFoldrCString#,
        cstringLength#,

        -- * Utf variants
        unpackCStringUtf8#, unpackAppendCStringUtf8#, unpackFoldrCStringUtf8#,

        -- * Other
        unpackNBytes#,
    ) where

import GHC.Internal.Types
