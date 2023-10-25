{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Wasm.Prim.Flag
  ( isJSFFIUsed,
  )
where

import GHC.Base

foreign import ccall unsafe "rts_JSFFI_used" isJSFFIUsed :: Bool
