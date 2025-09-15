module T25473A where

import GHC.Wasm.Prim

type BinOp a = a -> a -> a

foreign import javascript "wrapper"
  mkJSBinOp :: BinOp Int -> IO JSVal
