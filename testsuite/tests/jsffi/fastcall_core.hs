module FastcallCore (
  js_exported,
  fast_exported,
  fast_private
) where

import GHC.Wasm.Prim

foreign import javascript unsafe "$1 + 1"
  js_exported :: Int -> Int

foreign import javascript unsafe "$1 + 2"
  js_private :: Int -> Int

foreign import javascript unsafe "$1 + 3"
  js_unused :: Int -> Int

fast_exported :: Int -> Int
fast_exported = fastcall js_exported

fast_private :: Int -> Int
fast_private = fastcall js_private
