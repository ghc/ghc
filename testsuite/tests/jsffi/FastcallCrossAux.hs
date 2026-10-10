module FastcallCrossAux (
  js_exported,
  fast_exported_here,
  fast_private_here
) where

import GHC.Wasm.Prim

foreign import javascript unsafe "$1 + 1"
  js_exported :: Int -> Int

foreign import javascript unsafe "$1 + 2"
  js_private :: Int -> Int

foreign import javascript unsafe "$1 + 3"
  js_unused :: Int -> Int

fast_exported_here :: Int -> Int
fast_exported_here = fastcall js_exported

fast_private_here :: Int -> Int
fast_private_here = fastcall js_private
