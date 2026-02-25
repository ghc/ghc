{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Pack
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This function is just used by `rts_mkString`
--
-----------------------------------------------------------------------------

module GHC.Internal.Pack
       (
        unpackCString,
       )
        where

import GHC.Internal.Base (otherwise)
import GHC.Internal.Classes (Eq(..))
import GHC.Internal.CString (unpackCString#)
import GHC.Internal.Ptr
import GHC.Internal.Types (Char)

unpackCString :: Ptr a -> [Char]
unpackCString a@(Ptr addr)
  | a == nullPtr  = []
  | otherwise      = unpackCString# addr
