{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Wasm.Prim (
  -- * User-facing 'JSVal' and related utilities
  JSVal,
  freeJSVal,
  mkWeakJSVal,

  -- * 'JSString' and conversion from/to Haskell 'String'
  JSString (..),
  fromJSString,
  toJSString,

  -- * Exception types related to JSFFI
  JSException (..),
  WouldBlockException (..),

  -- * Is JSFFI used in the current wasm module?
  isJSFFIUsed
) where

import GHC.Internal.Wasm.Prim
