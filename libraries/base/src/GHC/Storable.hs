{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Storable
-- Copyright   :  (c) The FFI task force, 2000-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Helper functions for "Foreign.Storable"
--

module GHC.Storable
    (readWideCharOffPtr,
     readIntOffPtr,
     readWordOffPtr,
     readPtrOffPtr,
     readFunPtrOffPtr,
     readFloatOffPtr,
     readDoubleOffPtr,
     readStablePtrOffPtr,
     readInt8OffPtr,
     readInt16OffPtr,
     readInt32OffPtr,
     readInt64OffPtr,
     readWord8OffPtr,
     readWord16OffPtr,
     readWord32OffPtr,
     readWord64OffPtr,
     writeWideCharOffPtr,
     writeIntOffPtr,
     writeWordOffPtr,
     writePtrOffPtr,
     writeFunPtrOffPtr,
     writeFloatOffPtr,
     writeDoubleOffPtr,
     writeStablePtrOffPtr,
     writeInt8OffPtr,
     writeInt16OffPtr,
     writeInt32OffPtr,
     writeInt64OffPtr,
     writeWord8OffPtr,
     writeWord16OffPtr,
     writeWord32OffPtr,
     writeWord64OffPtr
     ) where

import GHC.Internal.Storable
