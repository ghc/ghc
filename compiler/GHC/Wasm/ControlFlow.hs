{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Wasm.ControlFlow
  ( WasmControl(..), (<>), pattern WasmIf
  , BrTableInterval(..), inclusiveInterval

  , WasmType, WasmTypeTag(..)
  , TypeList(..)
  , WasmFunctionType(..)
  )
where

import GHC.Prelude

import GHC.CmmToAsm.Wasm.Types
import GHC.Utils.Panic

{-|
Module      : GHC.Wasm.ControlFlow
Description : Representation of control-flow portion of the WebAssembly instruction set
-}

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = let count = hi - lo + 1
                 in  BrTableInterval lo count
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

(<>) :: forall s e pre mid post
      . WasmControl s e pre mid
     -> WasmControl s e mid post
     -> WasmControl s e pre post
(<>) = WasmSeq
-- N.B. Fallthrough can't be optimized away because of type checking.


-- Syntactic sugar.
pattern WasmIf :: WasmFunctionType pre post
               -> e
               -> WasmControl s e pre post
               -> WasmControl s e pre post
               -> WasmControl s e pre post

pattern WasmIf ty e t f =
    WasmPush TagI32 e `WasmSeq` WasmIfTop ty t f
