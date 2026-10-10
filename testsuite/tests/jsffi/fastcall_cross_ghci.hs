module Main where

import FastcallCrossAux
import GHC.Wasm.Prim

main :: IO ()
main =
  print
    ( fast_exported_here 40,
      fast_private_here 40,
      fastcall js_exported 40
    )
