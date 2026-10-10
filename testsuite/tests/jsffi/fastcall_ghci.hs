module Main where

import GHC.Wasm.Prim

foreign import javascript unsafe "$1 + 1"
  js_inc :: Int -> Int

main :: IO ()
main = print (fastcall js_inc 41)
