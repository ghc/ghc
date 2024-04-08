{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Pack
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- ⚠ Warning: Starting @base-4.18@, this module is being deprecated.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/21461 for more information.
--
--
--
-- This module provides a small set of low-level functions for packing
-- and unpacking a chunk of bytes. Used by code emitted by the compiler
-- plus the prelude libraries.
--
-- The programmer level view of packed strings is provided by a GHC
-- system library PackedString.
--

module GHC.Pack
    {-# DEPRECATED "The exports of this module should be instead imported from GHC.Exts" #-}
    (packCString#,
     unpackCString,
     unpackCString#,
     unpackNBytes#,
     unpackFoldrCString#,
     unpackAppendCString#
     ) where

import GHC.Internal.Pack
