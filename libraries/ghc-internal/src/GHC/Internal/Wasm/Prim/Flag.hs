{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Wasm.Prim.Flag
  ( isJSFFIUsed,
  )
where

import GHC.Internal.Base

foreign import ccall unsafe "rts_JSFFI_used" isJSFFIUsed :: Bool
