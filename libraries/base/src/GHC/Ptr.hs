{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Ptr
-- Copyright   :  (c) The FFI Task Force, 2000-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Ptr' and 'FunPtr' types and operations.
--

module GHC.Ptr (
        Ptr(..), FunPtr(..),
        nullPtr, castPtr, plusPtr, alignPtr, minusPtr,
        nullFunPtr, castFunPtr,

        -- * Unsafe functions
        castFunPtrToPtr, castPtrToFunPtr,
    ) where

import GHC.Internal.Ptr
