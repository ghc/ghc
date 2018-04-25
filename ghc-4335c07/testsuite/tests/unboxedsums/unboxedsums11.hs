{-# LANGUAGE UnboxedSums, MagicHash #-}

module Main where

type Ty = (# () | () #)

{-# NOINLINE showTy #-}
showTy :: Ty -> String
showTy (# _ | #)  = "(# _ | #)"
showTy (# | () #) = "(# | () #)"

main :: IO ()
main = do
  print (showTy (# undefined | #))
  print (showTy (# | () #))
