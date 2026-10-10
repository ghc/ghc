module Main where

import GHC.Wasm.Prim

foreign import javascript unsafe "$1 + 1"
  js_inc :: Int -> Int

foreign export javascript "main"
  main :: IO ()

main :: IO ()
main = print (js_inc 41, fastcall js_inc 41)
