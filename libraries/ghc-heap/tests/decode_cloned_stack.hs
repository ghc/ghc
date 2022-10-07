module Main where

import GHC.Stack.CloneStack
import GHC.Exts.DecodeStack
import GHC.Float (minExpt)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  stack <- cloneMyStack
  res <- decodeStack stack
  hPutStrLn stderr $ "result: " ++ show res
  return ()
