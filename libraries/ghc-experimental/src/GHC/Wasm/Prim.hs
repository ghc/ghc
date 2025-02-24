{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Wasm.Prim (
  -- User-facing JSVal type and freeJSVal
  JSVal,
  freeJSVal,

  -- The JSString type and conversion from/to Haskell String
  JSString (..),
  fromJSString,
  toJSString,

  -- Exception types related to JSFFI
  JSException (..),
  WouldBlockException (..),
  PromisePendingException (..),

  -- Is JSFFI used in the current wasm module?
  isJSFFIUsed
) where

import GHC.Internal.Wasm.Prim
