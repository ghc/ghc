{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Stable
-- Copyright   :  (c) The University of Glasgow, 1992-2004
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Stable pointers.
--

module GHC.Stable (
        StablePtr(..),
        newStablePtr,
        deRefStablePtr,
        freeStablePtr,
        castStablePtrToPtr,
        castPtrToStablePtr
    ) where

import GHC.Internal.Stable
