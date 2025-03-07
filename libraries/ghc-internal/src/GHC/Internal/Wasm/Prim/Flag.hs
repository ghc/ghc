{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Wasm.Prim.Flag
  ( isJSFFIUsed,
  )
where

import GHC.Internal.Base

-- | If the current wasm module has any JSFFI functionality linked in,
-- this would be 'True' at runtime and 'False' otherwise. If this is
-- 'False', the wasm module would be a self-contained wasm32-wasi
-- module that can be run by non-web runtimes as well.
foreign import ccall unsafe "rts_JSFFI_used" isJSFFIUsed :: Bool
